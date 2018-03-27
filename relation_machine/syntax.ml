type relvar =
  { ident : string
  ; arity : int
  }

module RelVar = struct
  type t = relvar
  let compare = Pervasives.compare
end

type attr = string

module Attr = struct
  type t = attr
  let compare = Pervasives.compare
end

type scalar =
  | Attr of attr
  | Lit  of int32

type expr =
  | Return of
      { guard_relation : relvar option
      ; values         : scalar list
      }
  | Select of
      { relation    : relvar
      ; conditions  : (int * scalar) list
      ; projections : (int * attr) list
      ; cont        : expr
      }

type comm =
  | WhileNotEmpty of relvar list * comms
  | Insert of relvar * expr
  | Move of { tgt : relvar; src : relvar }
  | Declare of relvar list * comms

and comms = comm list

type program =
  { edb_relvars : relvar list
  ; idb_relvars : relvar list
  ; commands    : comms
  }

(**********************************************************************)
let pp_scalar fmt = function
  | Attr attr -> Format.pp_print_string fmt attr
  | Lit i     -> Format.fprintf fmt "%ld" i

let merge_projections_conditions arity conditions projections =
  let conditions = List.sort (fun (i,_) (j,_) -> compare i j) conditions in
  let projections = List.sort (fun (i,_) (j,_) -> compare i j) projections in
  let rec loop i conds projs rev_merged =
    if i = arity then
      List.rev rev_merged
    else
      match conds, projs with
        | _, (j, x)::projs when i = j ->
           loop (i+1) conds projs (`Bind x::rev_merged)
        | (j, x)::conds, _ when i = j -> 
           loop (i+1) conds projs (`Match x::rev_merged)
        | _ ->
           loop (i+1) conds projs (`Ignore::rev_merged)
  in
  loop 0 conditions projections []

let pp_matcher fmt = function
  | `Match x -> Format.fprintf fmt "=%a" pp_scalar x
  | `Bind x  -> Format.fprintf fmt "?%s" x
  | `Ignore  -> Format.fprintf fmt "?_"

let pp_matching_spec =
  Fmt.(parens (list ~sep:(always ", ") pp_matcher))

let pp_relvar fmt { ident; arity } =
  Format.fprintf fmt "%s/%d" ident arity

let rec pp_expr fmt = function
  | Return {guard_relation=None; values} ->
     Format.fprintf fmt "(@[<h>%a@])"
       Fmt.(list ~sep:(always ",@ ") pp_scalar) values
  | Return {guard_relation=Some rel; values} ->
     Format.fprintf fmt "where (@[<h>%a@]) not in %a;@ (@[<h>%a@])"
       Fmt.(list ~sep:(always ",@ ") pp_scalar) values
       pp_relvar                                rel
       Fmt.(list ~sep:(always ",@ ") pp_scalar) values
  | Select { relation; conditions; projections; cont } ->
     Format.fprintf fmt
       "@[<hv 0>select @[<hv 0>%a from %a@];@]@ %a"
       pp_matching_spec (merge_projections_conditions
                           relation.arity
                           conditions
                           projections)
       pp_relvar        relation
       pp_expr          cont

let rec pp_comm fmt = function
  | WhileNotEmpty (rels, body) ->
     Format.fprintf fmt "while_not_empty (@[<hov>%a@])@ {@[<v 4>@,%a@]@,}"
       Fmt.(list ~sep:(always ",@ ") pp_relvar) rels
       pp_comms body
  | Insert (rel, expr) ->
     Format.fprintf fmt "@[<hv 4>%a +=@ { @[<v>%a@] };@]"
       pp_relvar rel
       pp_expr   expr
  | Move {tgt; src} ->
     Format.fprintf fmt "%a <- %a;"
       pp_relvar tgt
       pp_relvar src
  | Declare (initialisers, body) ->
     Format.fprintf fmt "declare (@[<hv>%a@])@ {@[<v 4>@,%a@]@,}"
       Fmt.(list ~sep:(always ",@ ") pp_relvar) initialisers
       pp_comms body

and pp_comms fmt = function
  | [] -> ()
  | [c] -> pp_comm fmt c
  | c::cs ->
     pp_comm fmt c;
     Format.pp_print_cut fmt ();
     Format.pp_print_cut fmt ();
     pp_comms fmt cs

let pp_program fmt {edb_relvars; idb_relvars; commands} =
  let pp_relvar_decl typ fmt nm =
    Format.fprintf fmt "%s %a@," typ pp_relvar nm
  in
  Format.fprintf fmt "@[<v>%a%a@,%a@]"
    Fmt.(list ~sep:(always "") (pp_relvar_decl "ext")) edb_relvars
    Fmt.(list ~sep:(always "") (pp_relvar_decl "int")) idb_relvars
    pp_comms                                           commands
