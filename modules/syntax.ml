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

module String_names = struct
  type ident = string
  type longident =
    | Lid_ident of ident
    | Lid_dot   of longident * string

  let pp_ident = Format.pp_print_string

  let rec pp_longident pp = function
    | Lid_ident id ->
       Format.pp_print_string pp id
    | Lid_dot (lid, f) ->
       pp_longident pp lid;
       Format.pp_print_string pp ".";
       Format.pp_print_string pp f
end

let rec pp_path fmt = function
  | [] ->
     failwith "internal error: empty path"
  | [nm] ->
     Format.pp_print_string fmt nm
  | nm::nms ->
     Format.fprintf fmt "%s.%a" nm pp_path nms

module Bound_names = struct
  type ident = Ident.t
  type longident = Path.t

  let pp_ident pp id =
    Format.pp_print_string pp (Ident.name id)

  let rec pp_longident pp = function
    | Path.Pident id -> pp_ident pp id
    | Path.Pdot (lid, f) ->
       pp_longident pp lid;
       Format.pp_print_string pp ".";
       Format.pp_print_string pp f
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

  val pp_def_decl :
    Format.formatter -> Names.ident * kind * def_type option -> unit

  val pp_type_constraint :
    Format.formatter -> string list * kind * def_type -> unit
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
    | Modtype_withtype  of mod_type * string list * Core.kind * Core.def_type

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
  : MOD_SYNTAX_RAW with module Core = Core =
struct
  module Core = Core

  type type_decl =
    { kind     : Core.kind
    ; manifest : Core.def_type option
    }

  type mod_type =
    { modtype_loc  : Core.Location.t
    ; modtype_data : modtype_data
    }

  and modtype_data =
    | Modtype_longident of Core.Names.longident
    | Modtype_signature of signature
    | Modtype_functor   of Core.Names.ident * mod_type * mod_type
    | Modtype_withtype  of mod_type * string list * Core.kind * Core.def_type

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

  let pp_type_decl fmt (id, { kind; manifest }) =
    Core.pp_def_decl fmt (id, kind, manifest)



  let rec pp_modtype fmt = function
    | {modtype_data = Modtype_functor (id, mty1, mty2)} ->
       Format.fprintf fmt "functor(%a : %a) ->@ %a"
         Core.Names.pp_ident id
         pp_modtype mty1
         pp_modtype mty2
    | modty ->
       pp_modtype2 fmt modty

  and pp_modtype2 fmt = function
    | {modtype_data = Modtype_longident lid} ->
       Core.Names.pp_longident fmt lid
    | {modtype_data = Modtype_signature sg} ->
       Format.fprintf fmt "@[<v 2>sig@ %a@]@ end"
         pp_signature sg
    | {modtype_data = Modtype_withtype (mty, path, kind, deftype)} ->
       Format.fprintf fmt "%a with type %a = %a"
         pp_modtype2 mty
         pp_path path
         Core.pp_type_constraint (path, kind, deftype)
    | modty ->
       Format.fprintf fmt "(%a)" pp_modtype modty

  and pp_signature fmt = function
    | [] -> ()
    | [item] ->
       pp_sig_item fmt item
    | item :: items ->
       pp_sig_item fmt item;
       Format.pp_print_space fmt ();
       pp_signature fmt items

  and pp_sig_item fmt = function
    | { sigitem_data = Sig_value (id, vty) } ->
       Core.pp_val_decl fmt (id, vty)
    | { sigitem_data = Sig_type (id, decl) } ->
       pp_type_decl fmt (id, decl)
    | { sigitem_data = Sig_module (id, mty) } ->
       Format.fprintf fmt "@[<hv 2>module %a :@ %a@]"
         Core.Names.pp_ident id
         pp_modtype mty
    | { sigitem_data = Sig_modty (id, mty) } ->
       Format.fprintf fmt "@[<v 2>module type %a =@ %a@]"
         Core.Names.pp_ident id
         pp_modtype mty

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

  let rec pp_modterm pp = function
    | {modterm_data = Mod_functor (id, mty, modl)} ->
       Format.fprintf pp "functor(%a : %a) ->@ %a"
         Core.Names.pp_ident id
         pp_modtype mty
         pp_modterm modl
    | modterm ->
       pp_modterm2 pp modterm

  and pp_modterm2 fmt = function
    | {modterm_data=Mod_longident lid} ->
       Core.Names.pp_longident fmt lid
    | {modterm_data=Mod_structure items} ->
       Format.fprintf fmt "@[<v 2>struct@ %a@]@ end"
         pp_structure items
    | {modterm_data = Mod_apply (modl1, modl2)} ->
       Format.fprintf fmt "%a (%a)"
         pp_modterm2 modl1
         pp_modterm modl2
    | {modterm_data = Mod_constraint (modl, mty)} ->
       Format.fprintf fmt "(%a :@ %a)"
         pp_modterm modl
         pp_modtype mty
    | modterm ->
       Format.fprintf fmt "(%a)" pp_modterm modterm

  and pp_structure pp = function
    | [] -> ()
    | [item] ->
       pp_str_item pp item
    | item :: items ->
       pp_str_item pp item;
       Format.pp_print_space pp ();
       Format.pp_print_cut pp ();
       pp_structure pp items

  and pp_str_item fmt = function
    | {stritem_data = Str_value term} ->
       Core.pp_term fmt term
    | {stritem_data = Str_type (id, kind, def_type)} ->
       Core.pp_def_decl fmt (id, kind, Some def_type)
    | {stritem_data = Str_module (id, modl)} ->
       Format.fprintf fmt "@[<v 2>module %a =@ %a@]"
         Core.Names.pp_ident id
         pp_modterm modl
    | {stritem_data = Str_modty (id, mty)} ->
       Format.fprintf fmt "@[<v 2>module type %a =@ %a@]"
         Core.Names.pp_ident id
         pp_modtype mty
end

module type CORE_SYNTAX_CONCRETE =
  CORE_SYNTAX_RAW
  with type Names.ident = string
   and type Names.longident = String_names.longident

module type CORE_SYNTAX = sig
  include CORE_SYNTAX_RAW
    with type Names.ident     = Ident.t
     and type Names.longident = Path.t

  val subst_valtype : Subst.t -> val_type -> val_type
  val subst_deftype : Subst.t -> def_type -> def_type
  val subst_kind : Subst.t -> kind -> kind
end

module type MOD_SYNTAX = sig
  module Core : CORE_SYNTAX

  include MOD_SYNTAX_RAW with module Core := Core

  val subst_typedecl : Subst.t -> type_decl -> type_decl

  val subst_modtype : Subst.t -> mod_type -> mod_type
end

module type MOD_SYNTAX_CONCRETE =
  MOD_SYNTAX_RAW
  with type Core.Names.ident     = String_names.ident
   and type Core.Names.longident = String_names.longident


module Mod_Syntax (Core_syntax : CORE_SYNTAX)
  : MOD_SYNTAX with module Core = Core_syntax =
struct
  module Core = Core_syntax

  include (Mod_Syntax_Raw (Core_syntax)
           : MOD_SYNTAX_RAW with module Core := Core_syntax)

  let subst_typedecl sub decl =
    { kind =
        Core.subst_kind sub decl.kind
    ; manifest =
        match decl.manifest with
          | None     -> None
          | Some dty -> Some (Core.subst_deftype sub dty)
    }

  let rec subst_modtype sub modtype =
    {modtype with
       modtype_data =
         match modtype.modtype_data with
           | Modtype_longident p ->
              Modtype_longident (Subst.path sub p)
           | Modtype_signature sg ->
              Modtype_signature (List.map (subst_sig_item sub) sg)
           | Modtype_functor (id, mty1, mty2) ->
              Modtype_functor (id, subst_modtype sub mty1, subst_modtype sub mty2)
           | Modtype_withtype (mty, path, kind, deftype) ->
              Modtype_withtype (subst_modtype sub mty,
                                path,
                                Core.subst_kind sub kind,
                                Core.subst_deftype sub deftype)
    }

  and subst_sig_item sub sigitem =
    {sigitem with
       sigitem_data =
         match sigitem.sigitem_data with
           | Sig_value (id, vty)  -> Sig_value (id, Core.subst_valtype sub vty)
           | Sig_type (id, decl)  -> Sig_type (id, subst_typedecl sub decl)
           | Sig_module (id, mty) -> Sig_module (id, subst_modtype sub mty)
           | Sig_modty (id, mty)  -> Sig_modty (id, subst_modtype sub mty)
    }
end
