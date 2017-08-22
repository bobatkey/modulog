(** Stamped identifiers *)

(** The type of stamped identifiers. *)
type t

(** [create name] creates a new stamped identifier with name [name]
   that is distinct from all other identifiers. *)
val create : string -> t

(** [name ident] returns the name of the identifier [ident]. This will
   be the same name as the one passed to {!create} when creating this
   identifier. *)
val name : t -> string

(** [full_name ident] returns a string representation of the
   identifier [ident] that uniquely identifies this identifier amongst
   all others created during this run of the program. *)
val full_name : t -> string

(** [pp fmt ident] pretty prints the {!full_name} of [ident] on the
   formatter [fmt]. *)
val pp : Format.formatter -> t -> unit

(** [equal ident1 ident2] returns [true] when [ident1] and [ident2]
   are the same identifier, and [false] otherwise. Note that two
   identifiers can have the same name (as returned by {!name}) but not
   be equal. *)
val equal : t -> t -> bool

(** Immutable tables that bind identifiers to arbitrary data. *)
module Table : sig
  type key = t

  (** Tables binding identifiers to data. *)
  type 'a t

  (** An empty identifier indexed table. *)
  val empty : 'a t

  (** [add ident data t] returns a the table [t] extended with a
     binding of [ident] to [data]. *)
  val add : key -> 'a -> 'a t -> 'a t

  (** [find ident t] returns [Some data] if [data] is bound to [ident]
     in [t], and [None] otherwise. *)
  val find : key -> 'a t -> 'a option
end

