type t =
  | Pident of Ident.t
  | Pdot   of t * string

val equal : t -> t -> bool
