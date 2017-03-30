type t

val identity : t

val add : Modules_ident.t -> Modules_path.t -> t -> t

val path : t -> Modules_path.t -> Modules_path.t
