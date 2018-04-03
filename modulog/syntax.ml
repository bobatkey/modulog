module Core = Core_syntax.Make (Modules.Syntax.String_names)
module Mod  = Modules.Syntax.Mod_Syntax_Raw (Core)
