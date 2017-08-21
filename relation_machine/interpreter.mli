module Env : sig
  type t

  val pp : Format.formatter -> t -> unit
end

val eval : Syntax.program -> Env.t
