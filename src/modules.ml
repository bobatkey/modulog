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
    | Lid_ident id -> Format.pp_print_string pp id
    | Lid_dot (lid, f) ->
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
end

module Mod_Syntax_Raw (Core : CORE_SYNTAX_RAW) : MOD_SYNTAX_RAW with module Core = Core = struct
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
    | { sigitem_data = Sig_type (id, { kind; manifest = None }) } ->
       (match Core.pp_kind with
         | None ->
            Format.fprintf pp "type %a" Core.Names.pp_ident id
         | Some pp_kind ->
            Format.fprintf pp "type %a :: %a"
              Core.Names.pp_ident id
              pp_kind kind)
    | { sigitem_data = Sig_type (id, { kind; manifest = Some ty })} ->
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
    | { sigitem_data = Sig_module (id, mty) } ->
       Format.fprintf pp "@[<hv 2>module %a :@ %a@]"
         Core.Names.pp_ident id
         pp_modtype mty
    | { sigitem_data = Sig_modty (id, mty) } ->
       Format.fprintf pp "@[<hv 2>module type %a =@ %a@]"
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
end

module Ident = Modules_ident

module Path = Modules_path

module Subst = Modules_subst

module Bound_names = struct
  type ident = Ident.t
  type longident = Path.t

  let pp_ident pp id = Format.pp_print_string pp (Ident.name id)
  let rec pp_longident pp = function
    | Path.Pident id -> pp_ident pp id
    | Path.Pdot (lid, f) ->
       pp_longident pp lid;
       Format.pp_print_string pp ".";
       Format.pp_print_string pp f
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

  include (Mod_Syntax_Raw (Core_syntax) : MOD_SYNTAX_RAW with module Core := Core_syntax)

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

  
  val find_value : String_names.longident -> t -> Path.t * Mod.Core.val_type

  val find_type : String_names.longident -> t -> Path.t * Mod.type_decl

  val find_module : String_names.longident -> t -> Path.t * Mod.mod_type

  val find_modtype : String_names.longident -> t -> Path.t * Mod.mod_type

  val lookup_modtype : Path.t -> t -> Mod.mod_type
  val lookup_type : Path.t -> t -> Mod.type_decl
end

module Env (Mod_syntax : MOD_SYNTAX) : ENV with module Mod = Mod_syntax =
struct
  module Mod = Mod_syntax

  module NameMap = Map.Make (String)

  type binding =
    | Value  of Mod.Core.val_type
    | Type   of Mod.type_decl
    | Module of Mod.mod_type
    | Modty  of Mod.mod_type

  
  type t =
    { bindings : binding Ident.Table.t
    ; names    : Ident.t NameMap.t
    }

  let empty =
    { bindings = Ident.Table.empty
    ; names    = NameMap.empty
    }

  let add id binding {bindings; names} =
    let ident = Ident.create id in
    ident,
    { bindings = Ident.Table.add ident binding bindings
    ; names    = NameMap.add id ident names
    }

  let add_value id vty = add id (Value vty)

  let add_type id decl = add id (Type decl)

  let add_module id mty = add id (Module mty)

  let add_modty id mty = add id (Modty mty)

  let add_by_ident ident binding env =
    {env with bindings = Ident.Table.add ident binding env.bindings}

  let add_module_by_ident ident mty env =
    add_by_ident ident (Module mty) env
  
  let add_sigitem item env =
    match item.Mod.sigitem_data with
      | Mod.Sig_value (id, vty)  -> add_by_ident id (Value vty) env
      | Mod.Sig_type (id, decl)  -> add_by_ident id (Type decl) env
      | Mod.Sig_module (id, mty) -> add_by_ident id (Module mty) env
      | Mod.Sig_modty (id, mty)  -> add_by_ident id (Modty mty) env

  let add_signature =
    List.fold_right add_sigitem

  let rec transl_path names = function
    | String_names.Lid_ident ident -> Path.Pident (NameMap.find ident names)
    | String_names.Lid_dot (p, f)  -> Path.Pdot (transl_path names p, f)

  let rec find path env = match path with
    | Path.Pident id ->
       (match Ident.Table.find id env with
         | None ->
            Error "identifier not found"
         | Some p ->
            Ok p)
    | Path.Pdot (root, field) ->
       let rec resolve = function
         | Mod.{modtype_data=Modtype_signature sg} ->
            find_field root field Subst.identity sg
         | Mod.{modtype_data=Modtype_longident lid} ->
            resolve (find_modty lid env)
         | _ ->
            Error "structure expected in dot access"
       in
       resolve (find_module root env)

  
  and find_field p field sub = function
    | [] ->
       Error (Printf.sprintf "no such field '%s' in structure" field)
    | Mod.{sigitem_data=Sig_value (id, vty)} :: rem ->
       if Ident.name id = field
       then Ok (Value (Mod.Core.subst_valtype sub vty))
       else find_field p field sub rem
    | Mod.{sigitem_data=Sig_type (id, decl)} :: rem ->
       if Ident.name id = field
       then Ok (Type (Mod.subst_typedecl sub decl))
       else
         find_field p field (Subst.add id (Path.Pdot (p, Ident.name id)) sub) rem
    | Mod.{sigitem_data=Sig_module (id, mty)} :: rem ->
       if Ident.name id = field
       then Ok (Module (Mod.subst_modtype sub mty))
       else
         find_field p field (Subst.add id (Path.Pdot (p, Ident.name id)) sub) rem
    | Mod.{sigitem_data=Sig_modty (id, mty)} :: rem ->
       if Ident.name id = field
       then Ok (Modty (Mod.subst_modtype sub mty))
       else
         find_field p field (Subst.add id (Path.Pdot (p, Ident.name id)) sub) rem

  and find_module path env =
    match find path env with
      | Ok (Module mty) -> mty
      | Ok _ -> failwith "module field expected"
      | Error msg -> failwith msg

  and find_modty path env =
    match find path env with
      | Ok (Modty mty) -> mty
      | Ok _ -> failwith "module type expected"
      | Error msg -> failwith msg
    
  
  let find_value lid {bindings;names} =
    let path = transl_path names lid in
    match find path bindings with
      | Ok (Value vty) ->
         path, vty
      | Ok _ ->
         failwith "value field expected"
      | Error msg ->
         failwith msg

  let find_type lid {bindings;names} =
    let path = transl_path names lid in
    match find path bindings with
      | Ok (Type decl) ->
         path, decl
      | Ok _ -> failwith "type field expected"
      | Error msg -> failwith msg

  let find_module lid {bindings;names} =
    let path = transl_path names lid in
    match find path bindings with
      | Ok (Module mty) ->
         path, mty
      | Ok _ -> failwith "module field expected"
      | Error msg -> failwith msg

  let find_modtype lid {bindings;names} =
    let path = transl_path names lid in
    match find path bindings with
      | Ok (Modty mty) ->
         path, mty
      | Ok _ -> failwith "module type field expected"
      | Error msg -> failwith msg

  let lookup_modtype path {bindings} =
    match find path bindings with
      | Ok (Modty mty) ->
         mty
      | Ok _ -> failwith "module type field expected"
      | Error msg -> failwith msg

  let lookup_type path {bindings} =
    match find path bindings with
      | Ok (Type ty) ->
         ty
      | Ok _ -> failwith "module type field expected"
      | Error msg -> failwith msg
end

module type CORE_TYPING = sig
  module Src : CORE_SYNTAX_RAW with type Names.ident = string
                                and type Names.longident = String_names.longident
  module Core : CORE_SYNTAX

  module Env : ENV with module Mod.Core = Core

  val type_term : Env.t -> Src.term -> Core.term * (string * Core.val_type) list

  val kind_deftype : Env.t -> Src.def_type -> Core.def_type * Core.kind

  val check_valtype : Env.t -> Src.val_type -> Core.val_type

  val check_kind : Env.t -> Src.kind -> Core.kind

  val valtype_match : Env.t -> Core.val_type -> Core.val_type -> bool

  val deftype_equiv : Env.t -> Core.kind -> Core.def_type -> Core.def_type -> bool

  val kind_match : Env.t -> Core.kind -> Core.kind -> bool

  val deftype_of_path : Path.t -> Core.kind -> Core.def_type
end

module type MOD_TYPING = sig
  module Src : MOD_SYNTAX_RAW with type Core.Names.ident = String_names.ident
                               and type Core.Names.longident = String_names.longident
  module Tgt : MOD_SYNTAX

  module Env : ENV with module Mod = Tgt

  val type_modterm : Env.t -> Src.mod_term -> Tgt.mod_term * Tgt.mod_type

  val type_structure : Env.t -> Src.structure -> Tgt.structure * Tgt.signature
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
=
struct
  module Src = Src
  module Tgt = Tgt
  module Env = CT.Env

  let rec modtype_match env mty1 mty2 =
    let open Tgt in
    match mty1, mty2 with
      (* Expand out module type names *)
      | {modtype_data=Modtype_longident id1}, mty2 ->
         modtype_match env (Env.lookup_modtype id1 env) mty2
      | mty1, {modtype_data=Tgt.Modtype_longident id2} ->
         modtype_match env mty1 (Env.lookup_modtype id2 env)

      (* Check signatures via subsumption *)
      | {modtype_data=Modtype_signature sig1},
        {modtype_data=Modtype_signature sig2} ->
         let paired_components, sub = pair_signature_components sig1 sig2 in
         let ext_env = Env.add_signature sig1 env in
         List.iter (specification_match ext_env sub) paired_components

      (* *)
      | {modtype_data=Modtype_functor (param1, arg1, res1)},
        {modtype_data=Modtype_functor (param2, arg2, res2)} ->
         let sub   = Subst.add param1 (Path.Pident param2) Subst.identity in
         let res1' = Tgt.subst_modtype sub res1 in
         modtype_match env arg2 arg1;
         modtype_match (Env.add_module_by_ident param2 arg2 env) res1' res2

      | _, _ ->
         failwith "module type mismatch"

  and pair_signature_components sig1 sig2 =
    let open Tgt in
    match sig2 with
      | [] ->
         [], Subst.identity

      | {sigitem_data=item2} :: rem2 ->
         let rec find_matching_component = function
           | [] ->
              failwith "unmatched signature component"
           | {sigitem_data=item1} :: rem1 ->
              match item1, item2 with
                | Sig_value (id1, _),  Sig_value (id2, _)
                | Sig_type (id1, _),   Sig_type (id2, _)
                | Sig_module (id1, _), Sig_module (id2, _)
                | Sig_modty (id1, _),  Sig_modty (id2, _)
                  when Ident.name id1 = Ident.name id2 ->
                   (id1, id2, item1)
                | _ ->
                   find_matching_component rem1
         in
         let id1, id2, item1 = find_matching_component sig1 in
         let pairs, sub = pair_signature_components sig1 rem2 in
         ((item1, item2) :: pairs, Subst.add id2 (Path.Pident id1) sub)

  and specification_match env sub = function
    | Tgt.Sig_value (_, vty1), Tgt.Sig_value (_, vty2) ->
       if not (CT.valtype_match env vty1 (Tgt.Core.subst_valtype sub vty2))
       then failwith "value components do not match"

    | Tgt.Sig_type (id, decl1), Tgt.Sig_type (_, decl2) ->
       if not (typedecl_match env id decl1 (Tgt.subst_typedecl sub decl2))
       then failwith "type components do not match"

    | Tgt.Sig_module (_, mty1), Tgt.Sig_module (_, mty2) ->
       modtype_match env mty1 (Tgt.subst_modtype sub mty2)

    | Tgt.Sig_modty (_, mty1), Tgt.Sig_modty (_, mty2) ->
       (* FIXME: not sure this is right? matching both ways =>
          equality up to permutation? *)
       modtype_match env mty1 (Tgt.subst_modtype sub mty2);
       modtype_match env (Tgt.subst_modtype sub mty2) mty1

    | _, _ ->
       assert false

  and typedecl_match env id decl1 decl2 =
    let open Tgt in
    CT.kind_match env decl1.kind decl2.kind &&
    (match decl1.manifest, decl2.manifest with
      | _, None ->
         true
      | Some typ1, Some typ2 ->
         CT.deftype_equiv env decl2.kind typ1 typ2
      | None, Some typ2 ->
         CT.deftype_equiv env decl2.kind
           (CT.deftype_of_path (Path.Pident id) decl1.kind)
           typ2)


  (* Strengthening: FIXME: switch to Generated locations everywhere *)
  let rec strengthen_modtype env path = function
    | Tgt.{modtype_data = Modtype_longident p} ->
       strengthen_modtype env path (Env.lookup_modtype p env)

    | Tgt.{modtype_data = Modtype_signature sg} as modtype ->
       Tgt.{modtype with
              modtype_data=
                Modtype_signature (List.map (strengthen_sigitem env path) sg)}

    | Tgt.{modtype_data = Modtype_functor _} as mty ->
       mty

  and strengthen_sigitem env path = function
    | Tgt.{sigitem_data=Sig_value _} as item ->
       item

    | Tgt.{sigitem_data=Sig_type (id, decl)} as item ->
       let m =
         match decl.Tgt.manifest with
           | None ->
              Some (CT.deftype_of_path
                      (Path.Pdot (path, Ident.name id))
                      decl.Tgt.kind)
           | Some ty ->
              Some ty
       in
       Tgt.{item with sigitem_data=Sig_type (id, {decl with manifest = m})}

    | Tgt.{sigitem_data=Sig_module (id, mty)} as item ->
       Tgt.{item with
              sigitem_data =
                Sig_module
                  (id,
                   strengthen_modtype env (Path.Pdot (path, Ident.name id)) mty)}

    | Tgt.{sigitem_data=Sig_modty (id, mty)} as item ->
       Tgt.{item with sigitem_data=Sig_modty (id, mty)}

  
  module SeenSet = Set.Make (String)

  let rec check_modtype env = function
    | Src.{modtype_loc;modtype_data=Modtype_longident path} ->
       let path, _ = Env.find_modtype path env in
       Tgt.{ modtype_loc
           ; modtype_data = Modtype_longident path
           }

    | Src.{modtype_loc;modtype_data=Modtype_signature items} ->
       Tgt.{ modtype_loc
           ; modtype_data = Modtype_signature (check_signature env [] SeenSet.empty items)
           }

    | Src.{modtype_loc;modtype_data=Modtype_functor (param, arg, res)} ->
       let arg        = check_modtype env arg in
       let param, env = Env.add_module param arg env in
       let res        = check_modtype env res in
       Tgt.{ modtype_loc
           ; modtype_data = Modtype_functor (param, arg, res)
           }

  and check_signature env rev_sig seen = function
    | [] ->
       List.rev rev_sig

    | Src.{sigitem_loc; sigitem_data=Sig_value (id, vty)} :: rem ->
       if SeenSet.mem id seen
       then failwith "repeated value name";
       let seen = SeenSet.add id seen in
       let vty = CT.check_valtype env vty in
       let id, env = Env.add_value id vty env in
       check_signature
         env
         (Tgt.{sigitem_loc; sigitem_data=Sig_value (id, vty)} :: rev_sig)
         seen
         rem

    | Src.{sigitem_loc; sigitem_data=Sig_type (id, {Src.kind; manifest})} :: rem ->
       if SeenSet.mem id seen
       then failwith "repeated type name";
       let kind = CT.check_kind env kind in
       let manifest = 
         match manifest with
           | None -> None
           | Some typ ->
              match CT.kind_deftype env typ with
                | typ, kind' when CT.kind_match env kind' kind ->
                   Some typ
                | _ ->
                   failwith "kind mismatch in manifest type specification"
       in
       let seen = SeenSet.add id seen in
       let decl = { Tgt.kind; manifest } in
       let id, env = Env.add_type id decl env in
       let item =
         Tgt.{ sigitem_loc
             ; sigitem_data = Sig_type (id, decl)
             }
       in
       check_signature env (item :: rev_sig) seen rem

    | Src.{sigitem_loc;sigitem_data=Sig_module (id, mty)} :: rem ->
       if SeenSet.mem id seen
       then failwith "repeated module name";
       let mty = check_modtype env mty in
       let seen = SeenSet.add id seen in
       let id, env = Env.add_module id mty env in
       let item =
         Tgt.{ sigitem_loc
             ; sigitem_data = Sig_module (id, mty)
             }
       in
       check_signature env (item :: rev_sig) seen rem

    | Src.{sigitem_loc; sigitem_data=Sig_modty (id, mty)} :: rem ->
       let mty = check_modtype env mty in
       let seen = SeenSet.add id seen in
       let id, env = Env.add_modty id mty env in
       let item =
         Tgt.{ sigitem_loc
             ; sigitem_data = Sig_modty (id, mty)
             }
       in
       check_signature env (item :: rev_sig) seen rem


  
  let rec type_modterm env = function
    | Src.{modterm_loc; modterm_data=Mod_longident path} ->
       let path, mty = Env.find_module path env in
       Tgt.{modterm_loc; modterm_data=Mod_longident path},
       strengthen_modtype env path mty

    | Src.{modterm_loc; modterm_data=Mod_structure str} ->
       let structure, signature = type_structure env [] [] SeenSet.empty str in
       Tgt.{ modterm_loc; modterm_data=Mod_structure structure},
       Tgt.{ modtype_loc  = Location.Generated
           ; modtype_data = Modtype_signature signature
           }

    | Src.{modterm_loc; modterm_data=Mod_functor (param, mty, body)} ->
       let mty = check_modtype env mty in
       let param, env = Env.add_module param mty env in
       let body, body_ty = type_modterm env body in
       Tgt.{modterm_loc; modterm_data=Mod_functor (param, mty, body)},
       Tgt.{ modtype_loc  = Location.Generated
           ; modtype_data = Modtype_functor (param, mty, body_ty)
           }

    | Src.{modterm_loc; modterm_data=Mod_apply (funct, arg)} ->
       (match arg with
         | Src.{modterm_loc=arg_loc;modterm_data=Mod_longident path} ->
            (match type_modterm env funct with
              | funct,
                Tgt.{modtype_data=Modtype_functor (param, mty_param, mty_res)} ->
                 let path, mty_arg = Env.find_module path env in
                 modtype_match env mty_arg mty_param;
                 let arg = Tgt.{ modterm_loc = arg_loc; modterm_data = Mod_longident path } in
                 Tgt.{ modterm_loc; modterm_data = Mod_apply (funct, arg) },
                 Tgt.subst_modtype (Subst.add param path Subst.identity) mty_res
              | _ ->
                 failwith "application of a non-functor")
         | Src.{modterm_loc=arg_loc} ->
            failwith "application of a functor to a non-path")

    | Src.{modterm_loc; modterm_data=Mod_constraint (modl, mty)} ->
       let mty = check_modtype env mty in
       let modl, mty' = type_modterm env modl in
       modtype_match env mty' mty;
       Tgt.{ modterm_loc; modterm_data = Mod_constraint (modl, mty) },
       mty

  and type_structure env rev_sig rev_str seen = function
    | [] ->
       List.rev rev_str,
       List.rev rev_sig

    | Src.{stritem_loc; stritem_data=Str_value term} :: items ->
       let term, val_items = CT.type_term env term in
       (* FIXME: also do 'seen' *)
       let rec collect env rev_sigitems = function
         | [] -> env, rev_sigitems
         | (id, ty) :: items ->
            let ident, env = Env.add_value id ty env in
            let item = Tgt.{ sigitem_loc  = Location.Generated
                           ; sigitem_data = Tgt.Sig_value (ident, ty)
                           }
            in
            collect env (item :: rev_sigitems) items
       in
       let env, rev_sigitems = collect env [] val_items in
       let stritem  =
         Tgt.{ stritem_loc
             ; stritem_data = Str_value term
             }
       in
       type_structure
         env
         (rev_sigitems @ rev_sig)
         (stritem :: rev_str)
         seen
         items

    | Src.{stritem_loc; stritem_data=Str_module (id, modl)} :: items ->
       if SeenSet.mem id seen
       then failwith "repeated module name";
       let seen = SeenSet.add id seen in
       let modl, modty = type_modterm env modl in
       let id, env = Env.add_module id modty env in
       let sigitem =
         Tgt.{ sigitem_loc  = Location.Generated
             ; sigitem_data = Sig_module (id, modty)
             }
       and stritem =
         Tgt.{ stritem_loc
             ; stritem_data = Str_module (id, modl)
             }
       in
       type_structure env (sigitem :: rev_sig) (stritem :: rev_str) seen items

    | Src.{stritem_loc; stritem_data=Str_type (id, kind, typ)} :: items ->
       if SeenSet.mem id seen
       then failwith "repeated type name";
       let seen = SeenSet.add id seen in
       let kind = CT.check_kind env kind in
       let typ, kind' = CT.kind_deftype env typ in
       if not (CT.kind_match env kind' kind)
       then failwith "kind mismatch in type definition";
       let tydecl = {Tgt.kind = kind; manifest = Some typ} in
       let id, env = Env.add_type id tydecl env in
       let sigitem =
         Tgt.{ sigitem_loc = Location.Generated
             ; sigitem_data = Sig_type (id, tydecl)
             }
       and stritem =
         Tgt.{ stritem_loc
             ; stritem_data = Str_type (id, kind, typ)
             }
       in
       type_structure env (sigitem :: rev_sig) (stritem :: rev_str) seen items

    | Src.{stritem_loc; stritem_data=Str_modty (id, mty)} :: items ->
       let mty = check_modtype env mty in
       let id, env = Env.add_modty id mty env in
       let sigitem =
         Tgt.{ sigitem_loc  = Location.Generated
             ; sigitem_data = Sig_modty (id, mty)
             }
       and stritem =
         Tgt.{ stritem_loc
             ; stritem_data = Str_modty (id, mty)
             }
       in
       type_structure env (sigitem :: rev_sig) (stritem :: rev_str) seen items

  let type_structure env str =
    type_structure env [] [] SeenSet.empty str
  
end

(*
module type CORE_NORM = sig
  module Core : CORE_SYNTAX

  val subst_term : Core.term -> Subst.t -> Core.term
end

module Mod_normalise
    (Mod : MOD_SYNTAX)
    (CN  : CORE_NORM with module Core = Mod.Core) =
struct
  (* normalise module expressions by replacing functors with the
     substituted versions. 

     FIXME: could also substitute out type declarations in terms?
  *)

  open Mod

  let rec subst_modterm sub = function
    | Mod_longident lid ->
       Mod_longident (Subst.path sub lid)
    | Mod_structure str ->
       Mod_structure (List.map (subst_def sub) str)
    | Mod_functor (id, mty, modl) ->
       Mod_functor (id, subst_modtype sub mty, subst_modterm sub modl)
    | Mod_apply (modl1, modl2) ->
       Mod_apply (subst_modterm sub modl1, subst_modterm sub modl2)
    | Mod_constraint (modl, mty) ->
       Mod_constraint (subst_modterm sub modl, subst_modtype sub mty)

  and subst_def sub = function
    | Str_value tm -> Str_value (CN.subst_term tm sub)
    | Str_type (id, kind, dty) ->
       Str_type (id, Core.subst_kind sub kind, Core.subst_deftype sub dty)
    | Str_module (id, modl) ->
       Str_module (id, subst_modterm sub modl)
    | Str_modty (id, mty) ->
       Str_modty (id, subst_modtype sub mty)

  module Env : sig
    type t
    val empty : t
    val find : Path.t -> t -> mod_term option
    val add : Ident.t -> mod_term -> t -> t
  end = struct
    type t = mod_term Ident.Table.t

    let empty = Ident.Table.empty

    let add id mtm env = Ident.Table.add id mtm env

    let add_item item env =
      match item with
        | Str_value _ | Str_type _ | Str_modty _ -> env
        | Str_module (id, modl) -> add id modl env

    let add_structure = List.fold_right add_item

    let rec find path env =
      match path with
        | Path.Pident id ->
           Ident.Table.find id env
        | Path.Pdot (root, field) ->
           match find root env with
             | Some (Mod_structure str) ->
                Some (find_field root field Subst.identity str)
             | None ->
                None
             | _ ->
                failwith "structure expected in dot access"

    and find_field p field sub = function
      | [] ->
         failwith "no such field in structure"
      | Str_value _ :: rem ->
         find_field p field sub rem
      | Str_type (id, _, _) :: rem
      | Str_modty (id, _) :: rem ->
         if Ident.name id = field
         then failwith "expecting to find a module"
         else find_field p field (Subst.add id (Path.Pdot (p, Ident.name id)) sub) rem
      | Str_module (id, modl) :: rem ->
         if Ident.name id = field
         then subst_modterm sub modl
         else find_field p field (Subst.add id (Path.Pdot (p, Ident.name id)) sub) rem
  end

  let rec norm_modterm env = function
    | Mod_longident lid ->
       (match Env.find lid env with
         | None ->
            (* If not found, must be abstract *)
            Mod_longident lid
         | Some modl ->
            modl)
    | Mod_structure items ->
       let _, defs = List.fold_left norm_def (env,[]) items in
       Mod_structure (List.rev defs)
    | Mod_functor (id, mty, modl) ->
       Mod_functor (id, mty, norm_modterm env modl)
    | Mod_apply (modl, (Mod_longident lid as arg)) ->
       (match norm_modterm env modl with
         | Mod_longident _ as modl ->
            Mod_apply (modl, arg)
         | Mod_functor (id, _, modl) ->
            norm_modterm env (subst_modterm Subst.(add id lid identity) modl)
         | _ ->
            failwith "internal error: type error in module normalisation")
    | Mod_apply (_, _) ->
       failwith "Application to non path"
    | Mod_constraint (modl, _) ->
       modl

  and norm_def (env, defs) = function
    | (Str_value _ | Str_type _ | Str_modty _) as def ->
       (env, def :: defs)
    | Str_module (id, modl) ->
       let modl = norm_modterm env modl in
       let env  = Env.add id modl env in
       (env, Str_module (id, modl) :: defs)
end
*)

(*
module Test_syn = struct
  type term = Path.t
  type val_type = Path.t
  type def_type = Path.t
  type kind = unit
  let subst_valtype = Subst.path
  let subst_deftype = Subst.path
  let subst_kind () _ = ()
end

module Test_syn_norm = struct
  module Core = Test_syn
  let subst_term = Subst.path
end

module Test_mod = Mod_Syntax (Test_syn)
module Test_norm = Mod_normalise (Test_mod) (Test_syn_norm)

let test =
  let open Test_mod in
  let id = Ident.create in
  let a = id"a" in
  Structure begin
    let f = id"F" in
    let y = id"Y" in
    let z = id"z" in
    [ Str_module (f,
                  let t = id"t" in
                  let x = id"X" in
                  Functor (x,
                           Signature [Sig_type (t, {kind=();manifest=None})],
                           Structure [Str_type (id"u", (), Path.Pdot (Path.Pident x, "t"))]))
    ; Str_module (y, Structure [Str_type (id"t", (), Path.Pident a)])
    ; Str_module (z, Apply (Longident (Path.Pident f), Longident (Path.Pident y)))
    ]
  end
*)
