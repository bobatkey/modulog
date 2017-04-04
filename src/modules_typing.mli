
open Modules_syntax

module Ident = Modules_ident
module Path  = Modules_path

type env_lookup_error =
  { path    : String_names.longident
  ; subpath : String_names.longident
  ; reason  : [ `not_found | `not_a_structure | `not_a_module
              | `not_a_value | `not_a_type | `not_a_module_type]
  }

val pp_lookup_error : Format.formatter -> env_lookup_error -> unit

module type ENV = sig
  module Mod : MOD_SYNTAX

  type t

  val empty : t


  val add_value : String_names.ident -> Mod.Core.val_type -> t -> Ident.t * t

  val add_type : String_names.ident -> Mod.type_decl -> t -> Ident.t * t

  val add_module : String_names.ident -> Mod.mod_type -> t -> Ident.t * t

  val add_modty : String_names.ident -> Mod.mod_type -> t -> Ident.t * t

  
  val add_signature : Mod.signature -> t -> t

  val add_module_by_ident : Ident.t -> Mod.mod_type -> t -> t


  val find_value : String_names.longident -> t -> (Path.t * Mod.Core.val_type, env_lookup_error) result

  val find_type : String_names.longident -> t -> (Path.t * Mod.type_decl, env_lookup_error) result

  val find_module : String_names.longident -> t -> (Path.t * Mod.mod_type, env_lookup_error) result

  val find_modtype : String_names.longident -> t -> (Path.t * Mod.mod_type, env_lookup_error) result


  val lookup_modtype : Path.t -> t -> Mod.mod_type

  val lookup_type : Path.t -> t -> Mod.type_decl
end

module Env (Mod_syntax : MOD_SYNTAX) : ENV with module Mod = Mod_syntax

module type CORE_TYPING = sig
  module Src : CORE_SYNTAX_RAW with type Names.ident = string
                                and type Names.longident = String_names.longident
  module Core : CORE_SYNTAX

  module Env : ENV with module Mod.Core = Core

  type core_error

  val pp_error : Format.formatter -> core_error -> unit


  val type_term : Env.t -> Src.term -> (Core.term * (string * Core.val_type) list, core_error) result

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

  module Env : ENV with module Mod = Tgt

  type error

  val pp_error : Format.formatter -> error -> unit

  val type_modterm :
    Env.t -> Src.mod_term -> (Tgt.mod_term * Tgt.mod_type, error) result

  val type_structure :
    Env.t -> Src.structure -> (Tgt.structure * Tgt.signature, error) result
end

module Mod_typing
    (Src : MOD_SYNTAX_RAW with type Core.Names.ident = string
                           and type Core.Names.longident = String_names.longident)
    (Tgt : MOD_SYNTAX)
    (CT  : CORE_TYPING
     with module Src     = Src.Core
      and module Core    = Tgt.Core
      and module Env.Mod = Tgt)
  : MOD_TYPING with module Src = Src
                and module Tgt = Tgt
                and module Env = CT.Env
