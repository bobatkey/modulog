open Syntax

type lookup_error =
  { path    : String_names.longident
  ; subpath : String_names.longident
  ; reason  : [ `not_found | `not_a_structure | `not_a_module
              | `not_a_value | `not_a_type | `not_a_module_type]
  }

val pp_lookup_error : Format.formatter -> lookup_error -> unit

module type S = sig
  module Mod : MOD_SYNTAX

  type t

  val empty : t


  val add_value : String_names.ident -> Mod.Core.val_type -> t -> Ident.t * t

  val add_type : String_names.ident -> Mod.type_decl -> t -> Ident.t * t

  val add_module : String_names.ident -> Mod.mod_type -> t -> Ident.t * t

  val add_modty : String_names.ident -> Mod.mod_type -> t -> Ident.t * t


  val add_signature : Mod.signature -> t -> t

  val add_module_by_ident : Ident.t -> Mod.mod_type -> t -> t

  val bind_value : Ident.t -> Mod.Core.val_type -> t -> t


  val find_value : String_names.longident -> t -> (Path.t * Mod.Core.val_type, lookup_error) result

  val find_type : String_names.longident -> t -> (Path.t * Mod.type_decl, lookup_error) result

  val find_module : String_names.longident -> t -> (Path.t * Mod.mod_type, lookup_error) result

  val find_modtype : String_names.longident -> t -> (Path.t * Mod.mod_type, lookup_error) result


  val lookup_modtype : Path.t -> t -> Mod.mod_type

  val lookup_type : Path.t -> t -> Mod.type_decl
end

module Make (Mod_syntax : MOD_SYNTAX) : S with module Mod = Mod_syntax
