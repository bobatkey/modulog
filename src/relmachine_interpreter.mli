module RelvarEnv : sig
  type t
end

val eval : Relmachine_syntax.comms -> RelvarEnv.t

val pp_relvarenv : Format.formatter -> RelvarEnv.t -> unit
