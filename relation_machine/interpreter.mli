module RelvarEnv : sig
  type t
end

val eval : Syntax.program -> RelvarEnv.t

val pp_relvarenv : Format.formatter -> RelvarEnv.t -> unit
