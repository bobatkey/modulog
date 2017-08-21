module Core : module type of Core_syntax.Make (Modules.Syntax.String_names)

module Mod : Modules.Syntax.MOD_SYNTAX_RAW
  with type Core.Location.t      = Core.Location.t
   and type Core.Names.ident     = Core.Names.ident
   and type Core.Names.longident = Core.Names.longident
   and type Core.term            = Core.term
   and type Core.val_type        = Core.val_type
   and type Core.def_type        = Core.def_type
   and type Core.kind            = Core.kind
