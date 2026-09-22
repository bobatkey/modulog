type t =
  | Pident of Ident.t
  | Pdot   of t * string

val equal : t -> t -> bool

val to_string : t -> string
