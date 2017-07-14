type t

val identity : t

val add : Ident.t -> Path.t -> t -> t

val path : t -> Path.t -> Path.t
