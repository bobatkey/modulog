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
  (** Read a csv file and insert all the tuples into the named
      relation variable. *)

  | WriteRelation  of relvar * string
  (** Write all the tuples in the named relation to the file in CSV
      format, overwriting what was there before. *)

  | WhileNotEmpty of relvar list * comms
  (** Loop until all the relations in the named variables are
      empty. *)

  | Insert of relvar * expr
  (** Insert the results of the expression into the named
      variable. *)

  | Swap of relvar
  (** Swap the named read/write buffer. *)

  | DeclareBuffers of relvar list * comms

and comms =
  comm list

type program =
  { relvars  : relvar list
  ; commands : comms
  }

val pp_relvar : Format.formatter -> relvar -> unit

val pp_program : Format.formatter -> program -> unit
