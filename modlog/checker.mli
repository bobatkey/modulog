type error

val type_structure :
  Syntax.Mod.structure ->
  (Checked_syntax.Mod.structure * Checked_syntax.Mod.signature, error) result

val pp_error : Format.formatter -> error -> unit
