type t

val mk : Lexing.position -> Lexing.position -> t

val of_lexbuf : Lexing.lexbuf -> t

val generated : t

val pp : Format.formatter -> t -> unit

val pp_without_filename : Format.formatter -> t -> unit

