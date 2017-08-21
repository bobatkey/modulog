open Syntax

module type CORE_TYPING = sig
  module Src : CORE_SYNTAX_RAW
    with type Names.ident = string
     and type Names.longident = String_names.longident

  module Core : CORE_SYNTAX

  module Env : Typing_environment.S with module Mod.Core = Core

  type core_error

  val pp_error : Format.formatter -> core_error -> unit


  val type_term : Env.t -> Src.term -> (Core.term * (Ident.t * Core.val_type) list, core_error) result

  val check_deftype : Env.t -> Core.kind -> Src.def_type -> (Core.def_type, core_error) result

  val check_valtype : Env.t -> Src.val_type -> (Core.val_type, core_error) result

  val check_kind : Env.t -> Src.kind -> (Core.kind, core_error) result

  val valtype_match : Env.t -> Core.val_type -> Core.val_type -> bool

  val deftype_equiv : Env.t -> Core.kind -> Core.def_type -> Core.def_type -> bool

  val kind_match : Env.t -> Core.kind -> Core.kind -> bool

  val deftype_of_path : Path.t -> Core.kind -> Core.def_type
end

module type MOD_TYPING = sig
  module Src : MOD_SYNTAX_RAW
    with type Core.Names.ident     = String_names.ident
     and type Core.Names.longident = String_names.longident

  module Tgt : MOD_SYNTAX
    with type Core.Location.t = Src.Core.Location.t

  module Env : Typing_environment.S with module Mod = Tgt

  type error

  val pp_error : Format.formatter -> error -> unit

  val type_modterm :
    Env.t -> Src.mod_term -> (Tgt.mod_term * Tgt.mod_type, error) result

  val type_structure :
    Env.t -> Src.structure -> (Tgt.structure * Tgt.signature, error) result
end

module Mod_typing
    (Src : MOD_SYNTAX_RAW
     with type Core.Names.ident = string
      and type Core.Names.longident = String_names.longident)
    (Tgt : MOD_SYNTAX
     with type Core.Location.t = Src.Core.Location.t)
    (CT  : CORE_TYPING
     with module Src     = Src.Core
      and module Core    = Tgt.Core
      and module Env.Mod = Tgt)
  : MOD_TYPING with module Src = Src
                and module Tgt = Tgt
                and module Env = CT.Env
