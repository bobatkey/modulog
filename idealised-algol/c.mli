(** Generation of C code. *)

val output : 'a Syntax.program -> 'a -> Format.formatter -> unit

val compile : string -> 'a Syntax.program -> 'a -> unit
