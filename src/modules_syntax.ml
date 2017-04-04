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

module Ident = Modules_ident

module Path = Modules_path

module Subst = Modules_subst

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
  module Names : NAMES

  type term
  type val_type
  type def_type
  type kind

  val pp_term : Format.formatter -> term -> unit
  val pp_val_type : Format.formatter -> val_type -> unit
  val pp_def_type : Format.formatter -> def_type -> unit
  val pp_kind : (Format.formatter -> kind -> unit) option
end

module type MOD_SYNTAX_RAW = sig
  module Core : CORE_SYNTAX_RAW

  type type_decl =
    { kind     : Core.kind
    ; manifest : Core.def_type option
    }

  val pp_type_decl : Format.formatter -> Core.Names.ident * type_decl -> unit

  type mod_type =
    { modtype_loc  : Location.t
    ; modtype_data : modtype_data
    }

  and modtype_data =
    | Modtype_longident of Core.Names.longident
    | Modtype_signature of signature
    | Modtype_functor   of Core.Names.ident * mod_type * mod_type

  and signature =
    sig_item list

  and sig_item =
    { sigitem_loc  : Location.t
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
    { modterm_loc  : Location.t
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
    { stritem_loc  : Location.t
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
    { modtype_loc  : Location.t
    ; modtype_data : modtype_data
    }

  and modtype_data =
    | Modtype_longident of Core.Names.longident
    | Modtype_signature of signature
    | Modtype_functor   of Core.Names.ident * mod_type * mod_type

  and signature =
    sig_item list

  and sig_item =
    { sigitem_loc  : Location.t
    ; sigitem_data : sigitem_data
    }

  and sigitem_data =
    | Sig_value  of Core.Names.ident * Core.val_type
    | Sig_type   of Core.Names.ident * type_decl
    | Sig_module of Core.Names.ident * mod_type
    | Sig_modty  of Core.Names.ident * mod_type

  let pp_type_decl pp = function
    | (id, { kind; manifest = None }) ->
       (match Core.pp_kind with
         | None ->
            Format.fprintf pp "type %a" Core.Names.pp_ident id
         | Some pp_kind ->
            Format.fprintf pp "type %a :: %a"
              Core.Names.pp_ident id
              pp_kind kind)
    | (id, { kind; manifest = Some ty }) ->
       (match Core.pp_kind with
         | None ->
            Format.fprintf pp "type %a = %a"
              Core.Names.pp_ident id
              Core.pp_def_type ty
         | Some pp_kind ->
            Format.fprintf pp "@[<hv 2>type %a :: %a =@ %a@]"
              Core.Names.pp_ident id
              pp_kind kind
              Core.pp_def_type ty)


  let rec pp_modtype pp = function
    | {modtype_data = Modtype_longident lid} ->
       Core.Names.pp_longident pp lid
    | {modtype_data = Modtype_signature sg} ->
       Format.fprintf pp "@[<v 2>sig@ %a@]@ end"
         pp_signature sg
    | {modtype_data = Modtype_functor (id, mty1, mty2)} ->
       Format.fprintf pp "functor(%a : %a) ->@ %a"
         Core.Names.pp_ident id
         pp_modtype mty1
         pp_modtype mty2

  and pp_signature pp = function
    | [] -> ()
    | [item] ->
       pp_sig_item pp item
    | item :: items ->
       pp_sig_item pp item;
       Format.pp_print_space pp ();
       pp_signature pp items

  and pp_sig_item pp = function
    | { sigitem_data = Sig_value (id, vty) } ->
       Format.fprintf pp "%a : %a"
         Core.Names.pp_ident id
         Core.pp_val_type vty
    | { sigitem_data = Sig_type (id, decl) } ->
       pp_type_decl pp (id, decl)
    | { sigitem_data = Sig_module (id, mty) } ->
       Format.fprintf pp "@[<hv 2>module %a :@ %a@]"
         Core.Names.pp_ident id
         pp_modtype mty
    | { sigitem_data = Sig_modty (id, mty) } ->
       Format.fprintf pp "@[<v 2>module type %a =@ %a@]"
         Core.Names.pp_ident id
         pp_modtype mty

  type mod_term =
    { modterm_loc  : Location.t
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
    { stritem_loc  : Location.t
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

  and pp_str_item pp = function
    | {stritem_data = Str_value term} ->
       Core.pp_term pp term
    | {stritem_data = Str_type (id, kind, def_type)} ->
       Format.fprintf pp "type %a = %a"
         Core.Names.pp_ident id
         Core.pp_def_type def_type
    | {stritem_data = Str_module (id, modl)} ->
       Format.fprintf pp "@[<v 2>module %a =@ %a@]"
         Core.Names.pp_ident id
         pp_modterm modl
    | {stritem_data = Str_modty (id, mty)} ->
       Format.fprintf pp "@[<v 2>module type %a =@ %a@]"
         Core.Names.pp_ident id
         pp_modtype mty
end

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

