module Ident  = Modules_ident
module Subst  = Modules_subst
module Path   = Modules_path
module Syntax = Modules_syntax

module type CORE_NORM = sig
  module Core : Syntax.CORE_SYNTAX

  val subst_term : Subst.t -> Core.term -> Core.term
end

module type MOD_NORMALISATION = sig
  module Mod : Syntax.MOD_SYNTAX

  module Env : sig
    type t
    val empty : t
    val add_module : Ident.t -> Mod.mod_term -> t -> t
    val add_type : Ident.t -> Mod.Core.kind -> Mod.Core.def_type -> t -> t
    val find_module : Path.t -> t -> Mod.mod_term option
    val find_type : Path.t -> t -> (Mod.Core.kind * Mod.Core.def_type)
  end

  val norm_modterm : Env.t -> Mod.mod_term -> Mod.mod_term
  val norm_structure : Env.t -> Mod.structure -> Mod.structure
end

module Mod_normalise
    (Mod : Syntax.MOD_SYNTAX)
    (CN  : CORE_NORM with module Core = Mod.Core)
  : MOD_NORMALISATION with module Mod = Mod
