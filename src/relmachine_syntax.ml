type relvar = string

type attr = string

type scalar =
  | Attr of attr
  | Lit  of int32

type expr =
  | Return of { guard_relation : relvar option
              ; values         : scalar list
              }
  | Select of { relation    : relvar
              ; conditions  : (int * scalar) list
              ; projections : (int * attr) list
              ; body        : expr
              }

type comm =
  | WhileNotEmpty of relvar list * comms
  (** Loop until all the relations in the named variables are
      empty. *)

  | Insert of relvar * expr
  (** Insert the results of the expression into the named
      variable. *)

  | Merge of { tgt : relvar; src : relvar }

  | Move of { tgt : relvar; src : relvar }
  (** Move the contents of 'src' into 'tgt', leaving 'src' empty. *)

  | Declare of (relvar * relvar option) list * comms

and comms = comm list

type program =
  { edb_relvars : (relvar * int) list
  ; idb_relvars : (relvar * int) list
  ; commands    : comms
  }

let arity_of_relvar relvar {edb_relvars;idb_relvars} =
  try List.assoc relvar edb_relvars
  with Not_found -> List.assoc relvar idb_relvars

(**********************************************************************)

let pp_scalar fmt = function
  | Attr attr -> Format.pp_print_string fmt attr
  | Lit i     -> Format.fprintf fmt "%ld" i

let pp_condition fmt (idx, scalar) =
  Format.fprintf fmt "%d = %a" idx pp_scalar scalar

let pp_projection fmt (idx, attr) =
  Format.fprintf fmt "%d -> %s" idx attr

let rec pp_expr fmt = function
  | Return {guard_relation=None; values} ->
     Format.fprintf fmt "(@[<h>%a@])"
       Fmt.(list ~sep:(always ",@ ") pp_scalar) values
  | Return {guard_relation=Some rel; values} ->
     Format.fprintf fmt "(@[<h>%a@]) unless in %s"
       Fmt.(list ~sep:(always ",@ ") pp_scalar) values
       rel
  | Select { relation; conditions=[]; projections; body } ->
     Format.fprintf fmt
       "@[<hv 0>select @[<hv 0>from %s@ with@ @[%a@]@]@ in@]@ %a"
       relation
       Fmt.(list ~sep:(always ",@ ") pp_projection) projections
       pp_expr body
  | Select { relation; conditions; projections; body } ->
     Format.fprintf fmt
       "@[<hv 0>select @[<hv 0>from %s@ when @[%a@]@ with @[%a@]@]@ in@]@ %a"
       relation
       Fmt.(list ~sep:(always ",@ ") pp_condition) conditions
       Fmt.(list ~sep:(always ",@ ") pp_projection) projections
       pp_expr body

let pp_initialiser fmt = function
  | vnm, None -> Format.fprintf fmt "%s = { }" vnm
  | vnm, Some vnm' -> Format.fprintf fmt "%s = %s" vnm vnm'

let rec pp_comm fmt = function
  | WhileNotEmpty (rels, body) ->
     Format.fprintf fmt "@[<v 4>whileNotEmpty (@[<hv>%a@]) do@,%a@]@,end"
       Fmt.(list ~sep:(always ",@ ") string) rels
       pp_comms body
  | Insert (vars, expr) ->
     Format.fprintf fmt "@[<hv 4>insert into %s value@ %a@]"
       vars
       pp_expr expr
  | Merge {tgt; src} ->
     Format.fprintf fmt "merge %s into %s" src tgt
  | Move {tgt; src} ->
     Format.fprintf fmt "move %s into %s" src tgt
  | Declare (initialisers, body) ->
     Format.fprintf fmt "@[<v 4>declare (@[<v 0>%a@]) in@,%a@]@,end"
       Fmt.(list ~sep:(always ",@ ") pp_initialiser) initialisers
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
  let pp_relvar_decl typ fmt (nm, arity) =
    Format.fprintf fmt "%s %s : %d" typ nm arity
  in
  Format.fprintf fmt "@[<v>%a@,@,%a@,@,%a@]"
    Fmt.(list (pp_relvar_decl "ext")) edb_relvars
    Fmt.(list (pp_relvar_decl "int")) idb_relvars
    pp_comms                          commands
