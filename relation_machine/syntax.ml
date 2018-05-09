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
      { values      : scalar array
      }
  | Guard_NotIn of
      { relation    : relvar
      ; values      : scalar array
      ; cont        : expr
      }
  | Select of
      { relation    : relvar
      ; conditions  : (int * scalar) list
      ; projections : (int * attr) list
      ; cont        : expr
      }

type comm =
  | ReadRelation   of relvar * string
  | WriteRelation  of relvar * string
  | WhileNotEmpty  of relvar list * comms
  | Insert         of relvar * expr
  | Swap           of relvar
  | DeclareBuffers of relvar list * comms

and comms = comm list

type program =
  { relvars  : relvar list
  ; commands : comms
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
  | Return {values} ->
     Format.fprintf fmt "(@[<h>%a@])"
       Fmt.(array ~sep:(always ",@ ") pp_scalar) values
  | Guard_NotIn { relation; values; cont } ->
     Format.fprintf fmt "where (@[<h>%a@]) not in %a;@ %a"
       Fmt.(array ~sep:(always ",@ ") pp_scalar) values
       pp_relvar                                 relation
       pp_expr                                   cont
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
  | ReadRelation (relvar, filename) ->
     Format.fprintf fmt "load %S into %a;"
       filename
       pp_relvar relvar
  | WriteRelation (relvar, filename) ->
     Format.fprintf fmt "save %a to %S;"
       pp_relvar relvar
       filename
  | WhileNotEmpty (rels, body) ->
     Format.fprintf fmt "while_not_empty (@[<hov>%a@])@ {@[<v 4>@,%a@]@,}"
       Fmt.(list ~sep:(always ",@ ") pp_relvar) rels
       pp_comms body
  | Insert (rel, expr) ->
     Format.fprintf fmt "@[<hv 4>%a +=@ { @[<v>%a@] };@]"
       pp_relvar rel
       pp_expr   expr
  | Swap relvar ->
     Format.fprintf fmt "swap %a;"
       pp_relvar relvar
  | DeclareBuffers (initialisers, body) ->
     Format.fprintf fmt "with_buffers (@[<hv>%a@])@ {@[<v 4>@,%a@]@,}"
       Fmt.(list ~sep:(always ",@ ") pp_relvar) initialisers
       pp_comms body

and pp_comms fmt =
  Fmt.(list ~sep:(always "@,@,") pp_comm) fmt

let pp_program fmt {relvars; commands} =
  let pp_relvar_decl fmt nm =
    Format.fprintf fmt "var %a;@," pp_relvar nm
  in
  Format.fprintf fmt "@[<v>%a@,%a@]"
    Fmt.(list ~sep:(always "") pp_relvar_decl) relvars
    pp_comms                                   commands
