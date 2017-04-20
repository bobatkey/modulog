module type SOURCE_LOCATION = sig
  type t

  val generated : t

  val pp : Format.formatter -> t -> unit
end

module type NAMES = sig
  type ident
  type longident

  val pp_ident : Format.formatter -> ident -> unit
  val pp_longident : Format.formatter -> longident -> unit
end

module String_names : sig
  type ident = string
  type longident =
    | Lid_ident of ident
    | Lid_dot   of longident * string

  include NAMES with type ident := ident
                 and type longident := longident
end

module Bound_names : sig
  type ident = Modules_ident.t
  type longident = Modules_path.t

  include NAMES with type ident := ident
                 and type longident := longident
end

module type CORE_SYNTAX_RAW = sig
  module Location : SOURCE_LOCATION
  module Names : NAMES

  type term
  type val_type
  type def_type
  type kind

  val pp_term : Format.formatter -> term -> unit

  val pp_val_decl : Format.formatter -> Names.ident * val_type -> unit

  val pp_def_decl : Format.formatter -> Names.ident * kind * def_type option -> unit
end

module type MOD_SYNTAX_RAW = sig
  module Core : CORE_SYNTAX_RAW

  type type_decl =
    { kind     : Core.kind
    ; manifest : Core.def_type option
    }

  val pp_type_decl : Format.formatter -> Core.Names.ident * type_decl -> unit

  type mod_type =
    { modtype_loc  : Core.Location.t
    ; modtype_data : modtype_data
    }

  and modtype_data =
    | Modtype_longident of Core.Names.longident
    | Modtype_signature of signature
    | Modtype_functor   of Core.Names.ident * mod_type * mod_type

  and signature =
    sig_item list

  and sig_item =
    { sigitem_loc  : Core.Location.t
    ; sigitem_data : sigitem_data
    }

  and sigitem_data =
    | Sig_value  of Core.Names.ident * Core.val_type
    | Sig_type   of Core.Names.ident * type_decl
    | Sig_module of Core.Names.ident * mod_type
    | Sig_modty  of Core.Names.ident * mod_type

  val pp_modtype : Format.formatter -> mod_type -> unit
  val pp_signature : Format.formatter -> signature -> unit
  val pp_sig_item : Format.formatter -> sig_item -> unit

  type mod_term =
    { modterm_loc  : Core.Location.t
    ; modterm_data : modterm_data
    }

  and modterm_data =
    | Mod_longident  of Core.Names.longident
    | Mod_structure  of structure
    | Mod_functor    of Core.Names.ident * mod_type * mod_term
    | Mod_apply      of mod_term * mod_term
    | Mod_constraint of mod_term * mod_type

  and structure =
    str_item list

  and str_item =
    { stritem_loc  : Core.Location.t
    ; stritem_data : stritem_data
    }

  and stritem_data =
    | Str_value  of Core.term
    | Str_type   of Core.Names.ident * Core.kind * Core.def_type
    | Str_module of Core.Names.ident * mod_term
    | Str_modty  of Core.Names.ident * mod_type

  val pp_modterm : Format.formatter -> mod_term -> unit
  val pp_structure : Format.formatter -> structure -> unit
end

module Mod_Syntax_Raw (Core : CORE_SYNTAX_RAW)
  : MOD_SYNTAX_RAW with module Core = Core

module type CORE_SYNTAX = sig
  include CORE_SYNTAX_RAW
    with type Names.ident     = Modules_ident.t
     and type Names.longident = Modules_path.t

  val subst_valtype : Modules_subst.t -> val_type -> val_type
  val subst_deftype : Modules_subst.t -> def_type -> def_type
  val subst_kind : Modules_subst.t -> kind -> kind
end

module type MOD_SYNTAX = sig
  module Core : CORE_SYNTAX

  include MOD_SYNTAX_RAW with module Core := Core

  val subst_typedecl : Modules_subst.t -> type_decl -> type_decl

  val subst_modtype : Modules_subst.t -> mod_type -> mod_type
end

module Mod_Syntax (Core_syntax : CORE_SYNTAX)
  : MOD_SYNTAX with module Core = Core_syntax
