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

val pp_program : Format.formatter -> program -> unit
