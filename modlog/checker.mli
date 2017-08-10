module Core : sig
  include module type of (Core_syntax.Make (Modules.Syntax.Bound_names))

  include Modules.Syntax.CORE_SYNTAX
    with type term       := term
     and type val_type   := val_type
     and type def_type   := def_type
     and type kind       := kind
     and module Location := Location
     and module Names    := Names
end

module Mod : Modules.Syntax.MOD_SYNTAX
  with type Core.Location.t = Location.t
   and type Core.term       = Core.term
   and type Core.val_type   = Core.val_type
   and type Core.def_type   = Core.def_type
   and type Core.kind       = Core.kind

type error

val type_structure :
  Syntax.Mod.structure ->
  (Mod.structure * Mod.signature, error) result

val pp_error : Format.formatter -> error -> unit
