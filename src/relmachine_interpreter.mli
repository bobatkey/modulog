module RelvarEnv : sig
  type t
end

val eval : Relmachine_syntax.program -> RelvarEnv.t

val pp_relvarenv : Format.formatter -> RelvarEnv.t -> unit
