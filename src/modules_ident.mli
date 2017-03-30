type t

val create : string -> t

val name : t -> string

val equal : t -> t -> bool

module Table : sig
  type key = t
  type 'a t
  val empty : 'a t
  val add : key -> 'a -> 'a t -> 'a t
  val find : key -> 'a t -> 'a option
end

val pp : Format.formatter -> t -> unit
