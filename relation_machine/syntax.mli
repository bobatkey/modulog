type relvar =
  { ident : string
  ; arity : int
  }

module RelVar : sig
  type t = relvar
  val compare : t -> t -> int
end

type attr = string

module Attr : sig
  type t = attr
  val compare : t -> t -> int
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
  (** Loop until all the relations in the named variables are
      empty. *)

  | Insert of relvar * expr
  (** Insert the results of the expression into the named
      variable. *)

  | Move of { tgt : relvar; src : relvar }
  (** Move the contents of 'src' into 'tgt', leaving 'src' empty. *)

  | Declare of relvar list * comms

and comms =
  comm list

type program =
  { edb_relvars : relvar list
  ; idb_relvars : relvar list
  ; commands    : comms
  }

val pp_relvar : Format.formatter -> relvar -> unit

val pp_program : Format.formatter -> program -> unit
