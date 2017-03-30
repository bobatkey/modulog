type t =
  | Pident of Modules_ident.t
  | Pdot   of t * string

val equal : t -> t -> bool
