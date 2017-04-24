open Modules_syntax

module Ident  = Modules_ident
module Path   = Modules_path
module Subst  = Modules_subst

type lookup_error =
  { path    : String_names.longident
  ; subpath : String_names.longident
  ; reason  : [ `not_found | `not_a_structure | `not_a_module
              | `not_a_value | `not_a_type | `not_a_module_type]
  }

let pp_lookup_error pp { path; subpath; reason } =
  let reason =
    match reason with
      | `not_found         -> "not found"
      | `not_a_structure   -> "not a structure"
      | `not_a_module      -> "not a module"
      | `not_a_value       -> "not a value" (* FIXME: 'value' doesn't always make sense *)
      | `not_a_type        -> "not a type"
      | `not_a_module_type -> "not a module type"
  in
  if path = subpath then
    Format.fprintf pp "The name %a was %s"
      String_names.pp_longident path
      reason
  else
    Format.fprintf pp "While looking up %a, the sub path %a was %s"
      String_names.pp_longident path
      String_names.pp_longident subpath
      reason

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

  val bind_value : Ident.t -> Mod.Core.val_type -> t -> t


  val find_value : String_names.longident -> t -> (Path.t * Mod.Core.val_type, lookup_error) result

  val find_type : String_names.longident -> t -> (Path.t * Mod.type_decl, lookup_error) result

  val find_module : String_names.longident -> t -> (Path.t * Mod.mod_type, lookup_error) result

  val find_modtype : String_names.longident -> t -> (Path.t * Mod.mod_type, lookup_error) result


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

  let bind_value id vty {bindings; names} =
    { bindings = Ident.Table.add id (Value vty) bindings
    ; names    = NameMap.add (Ident.name id) id names
    }

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


  let rec lid_of_path = function
    | Path.Pident id   -> String_names.Lid_ident (Ident.name id)
    | Path.Pdot (p, f) -> String_names.Lid_dot (lid_of_path p, f)


  let rec transl_path names = function
    | String_names.Lid_ident ident as path ->
       (match NameMap.find ident names with
         | exception Not_found -> Error (path, `not_found)
         | ident -> Ok (Path.Pident ident))
    | String_names.Lid_dot (p, f) ->
       (match transl_path names p with
         | Error _ as err -> err
         | Ok p -> Ok (Path.Pdot (p, f)))

  let rec find path env = match path with
    | Path.Pident id ->
       (match Ident.Table.find id env with
         | None ->
            failwith "internal error: path unresolvable"
         | Some binding ->
            Ok binding)
    | Path.Pdot (root, field) ->
       match find root env with
         | Ok (Module mty) ->
            let rec resolve = function
              | Mod.{modtype_data=Modtype_signature sg} ->
                 find_field root field Subst.identity sg
              | Mod.{modtype_data=Modtype_longident lid} ->
                 resolve (lookup_modtype lid env)
              | _ ->
                 Error (lid_of_path root, `not_a_structure)
            in
            resolve mty
         | Ok _ ->
            Error (lid_of_path root, `not_a_module)
         | Error err ->
            Error err

  and find_field p field sub = function
    | [] ->
       Error (lid_of_path (Path.Pdot (p, field)), `not_found)
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

  and lookup_modtype path env =
    match find path env with
      | Ok (Modty mty) -> mty
      | _ -> failwith "internal: module type lookup failed"

  let lookup_modtype path {bindings} =
    lookup_modtype path bindings

  let lookup_type path {bindings} =
    match find path bindings with
      | Ok (Type ty) -> ty
      | _ -> failwith "internal: type lookup failed"

  let (>>=) c f = match c with Ok a -> f a | Error e -> Error e

  let reword_lookup_error ~target_path result =
    match result with
      | Ok value ->
         Ok value
      | Error (subpath, reason) ->
         Error { path = target_path; subpath; reason }

  let find_value lid {bindings;names} =
    reword_lookup_error ~target_path:lid begin
      transl_path names lid >>= fun path ->
      find path bindings >>= function
      | Value vty -> Ok (path, vty)
      | _         -> Error (lid, `not_a_value)
    end

  let find_type lid {bindings;names} =
    reword_lookup_error ~target_path:lid begin
      transl_path names lid >>= fun path ->
      find path bindings >>= function
      | Type decl -> Ok (path, decl)
      | _         -> Error (lid, `not_a_type)
    end

  let find_module lid {bindings;names} =
    reword_lookup_error ~target_path:lid begin
      transl_path names lid >>= fun path ->
      find path bindings >>= function
      | Module mty -> Ok (path, mty)
      | _          -> Error (lid, `not_a_module)
    end

  let find_modtype lid {bindings;names} =
    reword_lookup_error ~target_path:lid begin
      transl_path names lid >>= fun path ->
      find path bindings >>= function
      | Modty mty -> Ok (path, mty)
      | _         -> Error (lid, `not_a_module_type)
    end
end

module type CORE_TYPING = sig
  module Src : CORE_SYNTAX_RAW with type Names.ident = string
                                and type Names.longident = String_names.longident
  module Core : CORE_SYNTAX

  module Env : ENV with module Mod.Core = Core

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
    (Tgt : MOD_SYNTAX
     with type Core.Location.t = Src.Core.Location.t)
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
  module Location = Src.Core.Location

  let (>>=) c f = match c with Ok a -> f a | Error e -> Error e
  let (>>|) c f = match c with Ok a -> Ok (f a) | Error e -> Error e

  let rec check_list f = function
    | [] -> Ok ()
    | x :: xs ->
       match f x with
         | Error _ as err -> err
         | Ok () -> check_list f xs

  type match_error_detail =
    | Signature_vs_functor
    | Unmatched       of Tgt.sig_item
    | Value_mismatch  of Ident.t * Tgt.Core.val_type * Tgt.Core.val_type
    | Type_mismatch   of Ident.t * Tgt.type_decl * Tgt.type_decl
    | Module_mismatch of Ident.t * match_error
    | Module_eq_mismatch of Ident.t * match_error

  and match_error =
    { mty1   : Tgt.mod_type
    ; mty2   : Tgt.mod_type
    ; detail : match_error_detail
    }

  let rec pp_match_error pp {mty1; mty2; detail} =
    Format.fprintf pp
      "the module type@ @[<hv 2>@,%a@,@]@ does not match the module type@ @[<hv 2>@,%a@,@]@ because %a"
      Tgt.pp_modtype mty1
      Tgt.pp_modtype mty2
      pp_match_error_detail detail

  and pp_match_error_detail pp = function
    | Signature_vs_functor ->
       Format.fprintf pp "one is signature and one is a functor."
    | Unmatched item ->
       Format.fprintf pp "the item@ @[<v 2>@,%a@,@]@ is unmatched."
         Tgt.pp_sig_item item
    | Value_mismatch (id, vty1, vty2) ->
       Format.fprintf pp "the entry %S has type@ @[<v 2>@,%a@,@]@ in the former, and@ @[<v 2>@,%a@,@]@ in the latter, and these are not equal."
         (Ident.name id)
         Tgt.Core.pp_val_decl (id, vty1)
         Tgt.Core.pp_val_decl (id, vty2)
    | Type_mismatch (id, decl1, decl2) ->
       Format.fprintf pp "the type %a is declared as@ %a@ in the former, and @ %a@ in the latter, and these do not match."
         Ident.pp id
         Tgt.pp_type_decl (id, decl1)
         Tgt.pp_type_decl (id, decl2)
    | Module_mismatch (id, err) ->
       Format.fprintf pp "the modules named %a do not match. Specifically, %a."
         Ident.pp id
         pp_match_error err
    | Module_eq_mismatch (id, err) ->
       Format.fprintf pp "the modules named %a are not equal. Specifically, %a/"
         Ident.pp id
         pp_match_error err

  let rec modtype_match env mty1 mty2 : (unit, match_error) result =
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
         (match signature_match env sig1 sig2 with
           | Ok () ->
              Ok ()
           | Error detail ->
              Error {mty1;mty2;detail})

      (* Check that functor types match: contravariantly in their
         arguments, and covariantly in their results. *)
      | {modtype_data=Modtype_functor (param1, arg1, res1)},
        {modtype_data=Modtype_functor (param2, arg2, res2)} ->
         let sub   = Subst.add param1 (Path.Pident param2) Subst.identity in
         let res1' = Tgt.subst_modtype sub res1 in
         modtype_match env arg2 arg1 >>= fun () ->
         modtype_match (Env.add_module_by_ident param2 arg2 env) res1' res2

      | _, _ ->
         Error {mty1;mty2;detail=Signature_vs_functor}

  and signature_match env sig1 sig2 =
    pair_signature_components sig1 sig2 >>= fun (paired_components, sub) ->
    let ext_env = Env.add_signature sig1 env in
    check_list (specification_match ext_env sub) paired_components

  and pair_signature_components sig1 sig2 =
    let open Tgt in
    match sig2 with
      | [] ->
         Ok ([], Subst.identity)

      | {sigitem_data=item2} as item2' :: rem2 ->
         let rec find_matching_component = function
           | [] ->
              Error (Unmatched item2')
           | {sigitem_data=item1} :: rem1 ->
              match item1, item2 with
                | Sig_value (id1, _),  Sig_value (id2, _)
                | Sig_type (id1, _),   Sig_type (id2, _)
                | Sig_module (id1, _), Sig_module (id2, _)
                | Sig_modty (id1, _),  Sig_modty (id2, _)
                  when Ident.name id1 = Ident.name id2 ->
                   Ok (id1, id2, item1)
                | _ ->
                   find_matching_component rem1
         in
         find_matching_component sig1 >>= fun (id1, id2, item1) ->
         pair_signature_components sig1 rem2 >>= fun (pairs, sub) ->
         Ok ((item1, item2) :: pairs, Subst.add id2 (Path.Pident id1) sub)

  and specification_match env sub = function
    | Tgt.Sig_value (id, vty1), Tgt.Sig_value (_, vty2) ->
       if CT.valtype_match env vty1 (Tgt.Core.subst_valtype sub vty2) then
         Ok ()
       else
         Error (Value_mismatch (id, vty1, vty2))

    | Tgt.Sig_type (id, decl1), Tgt.Sig_type (_, decl2) ->
       if typedecl_match env id decl1 (Tgt.subst_typedecl sub decl2) then
         Ok ()
       else
         Error (Type_mismatch (id, decl1, decl2))

    | Tgt.Sig_module (id, mty1), Tgt.Sig_module (_, mty2) ->
       (match modtype_match env mty1 (Tgt.subst_modtype sub mty2) with
         | Ok () ->
            Ok ()
         | Error e ->
            Error (Module_mismatch (id, e)))

    | Tgt.Sig_modty (id, mty1), Tgt.Sig_modty (_, mty2) ->
       (* FIXME: not sure this is right? matching both ways =>
          equality up to permutation? *)
       (match
          modtype_match env mty1 (Tgt.subst_modtype sub mty2) >>= fun () ->
          modtype_match env (Tgt.subst_modtype sub mty2) mty1
        with
          | Ok () ->
             Ok ()
          | Error e ->
             Error (Module_eq_mismatch (id, e)))

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


  let rec strengthen_modtype env path = function
    | Tgt.{modtype_data = Modtype_longident p} ->
       strengthen_modtype env path (Env.lookup_modtype p env)

    | Tgt.{modtype_data = Modtype_signature sg} as modtype ->
       Tgt.{modtype with
              modtype_data=
                Modtype_signature (List.map (strengthen_sigitem env path) sg)}

    | Tgt.{modtype_data = Modtype_functor _} as mty ->
       mty

    | Tgt.{modtype_data = Modtype_withtype _} ->
       failwith "internal error: unexpanded 'with type' in checked module type"

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

  type core_error = CT.core_error

  type error_detail =
    | Application_of_nonfunctor
    | Application_to_non_path
    | Core_error of CT.core_error
    | Lookup_error of lookup_error
    | Match_error of string * match_error
    | Repeated_name of Src.Core.Names.ident
    | Not_a_signature
    | Type_already_manifest of string list
    | Kind_mismatch_in_with of Tgt.Core.Names.ident * Tgt.Core.kind * string list * Tgt.Core.kind * Tgt.Core.def_type
    | Path_not_found of string list

  type error = Location.t * error_detail

  let pp_error pp (location, detail) =
    match detail with
      | Application_of_nonfunctor ->
         Format.fprintf pp "In %a@,Error: Application of non-functor"
           Location.pp location
      | Application_to_non_path ->
         Format.fprintf pp "In %a@,Error: Application of functor to non-path"
           Location.pp location
      | Core_error core_error ->
         Format.fprintf pp "In the declaration at %a@,Error @[<v>%a@]"
           Location.pp location
           CT.pp_error core_error
      | Lookup_error lookup_error ->
         Format.fprintf pp "In %a@,%a"
           Location.pp location
           pp_lookup_error lookup_error
      | Match_error (reason, err) ->
         Format.fprintf pp "In %a@,In the %s, %a"
           Location.pp location
           reason
           pp_match_error err
      | Repeated_name id ->
         Format.fprintf pp "In declaration at %a@, repeated name: %S"
           Location.pp location
           id
      | Not_a_signature ->
         Format.fprintf pp "The module type at %a is not a signature"
           Location.pp location
      | Type_already_manifest path ->
         Format.fprintf pp "At %a, the type identified by the path@ @;<0 4>%a@,@ is already manifest@]"
           Location.pp location
           pp_path path
      | Kind_mismatch_in_with (ident, kind1, path, kind2, dty) ->
         Format.fprintf pp
           "At %a,@, Error:@;<0 2>Cannot replace@,@;<0 4>%a@,@,with@,@;<0 4>%a@,@,because the kinds do not match"
           Location.pp location
           Tgt.Core.pp_def_decl (ident, kind1, None)
           Tgt.Core.pp_type_constraint (path, kind2, dty)
      | Path_not_found path ->
         Format.fprintf pp
           "At %a, the path %a does not refer to a type in this signature"
           Location.pp location
           pp_path path

  let lift_lookup_error loc = function
    | Ok value  -> Ok value
    | Error err -> Error (loc, Lookup_error err)

  let lift_core_error loc = function
    | Ok value  -> Ok value
    | Error err -> Error (loc, Core_error err)

  let lift_match_error loc reason = function
    | Ok value  -> Ok value
    | Error err -> Error (loc, Match_error (reason, err))

  let check_manifest env loc kind = function
    | None -> Ok None
    | Some dty ->
       lift_core_error loc @@ CT.check_deftype env kind dty
       >>= fun dty ->
       Ok (Some dty)

  let rec subst_type_in_items env typename kind dty rev_items = function
    | [] ->
       Error `path_not_found
    | Tgt.({sigitem_data=Sig_type (ident, decl)} as item)::items
      when Ident.name ident = typename ->
       let open Tgt in
       if CT.kind_match env kind decl.kind then
         (match decl.manifest with
           | None ->
              let sigitem_data = Sig_type (ident, { kind; manifest=Some dty }) in
              Ok (List.rev_append rev_items ({item with sigitem_data} :: items))
           | Some _ ->
              Error `already_manifest)
       else
         Error (`kinds_dont_match (ident, decl.kind))
    | item::items ->
       subst_type_in_items env typename kind dty (item::rev_items) items

  let rec subst_type env modty path kind dty =
    match modty with
      | Tgt.{modtype_data = Modtype_longident lid} ->
         subst_type env (Env.lookup_modtype lid env) path kind dty
      | Tgt.{modtype_data = Modtype_signature items} ->
         (match path with
           | [] ->
              failwith "internal error: empty path"
           | [typename] ->
              subst_type_in_items env typename kind dty [] items >>= fun items ->
              Ok (Tgt.{ modtype_loc  = Location.generated
                      ; modtype_data = Modtype_signature items
                      })
           | modname::path ->
              subst_type_in_submodule env modname path kind dty [] items
              >>= fun items ->
              Ok (Tgt.{ modtype_loc  = Location.generated
                      ; modtype_data = Modtype_signature items
                      }))
      | Tgt.{modtype_data = Modtype_functor _} ->
         Error `Not_a_signature
      | Tgt.{modtype_data = Modtype_withtype _} ->
         failwith "internal error: unexpanded 'with type' in checked module type"

  and subst_type_in_submodule env nm path kind dty rev_items = function
    | [] ->
       Error `path_not_found
    | Tgt.{sigitem_data=Sig_module (ident, modty)}::items
      when Ident.name ident = nm ->
       subst_type env modty path kind dty >>= fun modty ->
       let item = Tgt.{ sigitem_loc  = Location.generated
                      ; sigitem_data = Sig_module (ident, modty)
                      }
       in
       Ok (List.rev_append rev_items (item :: items))
    | item::items ->
       subst_type_in_submodule env nm path kind dty (item::rev_items) items

  let rec check_modtype env = function
    | Src.{modtype_loc;modtype_data=Modtype_longident path} ->
       lift_lookup_error modtype_loc @@ Env.find_modtype path env
       >>| fun (path, _) ->
       Tgt.{ modtype_loc
           ; modtype_data = Modtype_longident path
           }

    | Src.{modtype_loc;modtype_data=Modtype_signature items} ->
       check_signature env [] SeenSet.empty items >>| fun sg ->
       Tgt.{ modtype_loc
           ; modtype_data = Modtype_signature sg
           }

    | Src.{modtype_loc;modtype_data=Modtype_functor (param, arg, res)} ->
       check_modtype env arg >>= fun arg ->
       let param, env = Env.add_module param arg env in
       check_modtype env res >>| fun res ->
       Tgt.{ modtype_loc
           ; modtype_data = Modtype_functor (param, arg, res)
           }
    | Src.{modtype_loc;modtype_data=Modtype_withtype (mty, path, kind, dty)} ->
       let sig_loc = mty.Src.modtype_loc in
       check_modtype env mty >>= fun mty ->
       lift_core_error modtype_loc (CT.check_kind env kind) >>= fun kind ->
       lift_core_error modtype_loc (CT.check_deftype env kind dty) >>= fun dty ->
       (match subst_type env mty path kind dty with
         | Ok mty -> Ok mty
         | Error `Not_a_signature  ->
            Error (sig_loc, Not_a_signature)
         | Error `already_manifest ->
            Error (modtype_loc, Type_already_manifest path)
         | Error (`kinds_dont_match (ident, expected_kind)) ->
            Error (modtype_loc, Kind_mismatch_in_with (ident, expected_kind, path, kind, dty))
         | Error `path_not_found ->
            Error (modtype_loc, Path_not_found path))

  and check_signature env rev_sig seen = function
    | [] ->
       Ok (List.rev rev_sig)

    | Src.{sigitem_loc; sigitem_data=Sig_value (id, vty)} :: rem ->
       if SeenSet.mem id seen then
         Error (sigitem_loc, Repeated_name id)
       else
         let seen = SeenSet.add id seen in
         lift_core_error sigitem_loc @@ CT.check_valtype env vty >>= fun vty ->
         let id, env = Env.add_value id vty env in
         check_signature
           env
           (Tgt.{sigitem_loc; sigitem_data=Sig_value (id, vty)} :: rev_sig)
           seen
           rem

    | Src.{sigitem_loc; sigitem_data=Sig_type (id, {Src.kind; manifest})} :: rem ->
       if SeenSet.mem id seen then
         Error (sigitem_loc, Repeated_name id)
       else
         let seen = SeenSet.add id seen in
         lift_core_error sigitem_loc @@ CT.check_kind env kind >>= fun kind ->
         check_manifest env sigitem_loc kind manifest >>= fun manifest ->
         let decl    = Tgt.{ kind; manifest } in
         let id, env = Env.add_type id decl env in
         let item =
           Tgt.{ sigitem_loc
               ; sigitem_data = Sig_type (id, decl)
               }
         in
         check_signature env (item :: rev_sig) seen rem

    | Src.{sigitem_loc;sigitem_data=Sig_module (id, mty)} :: rem ->
       if SeenSet.mem id seen then
         Error (sigitem_loc, Repeated_name id)
       else
         let seen = SeenSet.add id seen in
         check_modtype env mty >>= fun mty ->
         let id, env = Env.add_module id mty env in
         let item =
           Tgt.{ sigitem_loc
               ; sigitem_data = Sig_module (id, mty)
               }
         in
         check_signature env (item :: rev_sig) seen rem

    | Src.{sigitem_loc; sigitem_data=Sig_modty (id, mty)} :: rem ->
       if SeenSet.mem id seen then
         Error (sigitem_loc, Repeated_name id)
       else
         let seen = SeenSet.add id seen in
         check_modtype env mty >>= fun mty ->
         let id, env = Env.add_modty id mty env in
         let item =
           Tgt.{ sigitem_loc
               ; sigitem_data = Sig_modty (id, mty)
               }
         in
         check_signature env (item :: rev_sig) seen rem



  let rec type_modterm env = function
    | Src.{modterm_loc; modterm_data=Mod_longident path} ->
       lift_lookup_error modterm_loc (Env.find_module path env)
       >>| fun (path, mty) ->
       Tgt.{modterm_loc; modterm_data=Mod_longident path},
       strengthen_modtype env path mty

    | Src.{modterm_loc; modterm_data=Mod_structure str} ->
       type_structure env [] [] SeenSet.empty str
       >>| fun (structure, signature) ->
       Tgt.{ modterm_loc; modterm_data=Mod_structure structure},
       Tgt.{ modtype_loc  = Location.generated
           ; modtype_data = Modtype_signature signature
           }

    | Src.{modterm_loc; modterm_data=Mod_functor (param, mty, body)} ->
       check_modtype env mty >>= fun mty ->
       let param, env = Env.add_module param mty env in
       type_modterm env body >>| fun (body, body_ty) ->
       Tgt.{modterm_loc; modterm_data=Mod_functor (param, mty, body)},
       Tgt.{ modtype_loc  = Location.generated
           ; modtype_data = Modtype_functor (param, mty, body_ty)
           }

    | Src.{modterm_loc; modterm_data=Mod_apply (funct, arg)} ->
       (match arg with
         | Src.{modterm_loc=arg_loc;modterm_data=Mod_longident path} ->
            (type_modterm env funct >>= function
              | funct,
                Tgt.{modtype_data=Modtype_functor (param, mty_param, mty_res)} ->
                 lift_lookup_error arg_loc (Env.find_module path env)
                 >>= fun (path, mty_arg) ->
                 lift_match_error arg_loc "functor application"
                   (modtype_match env mty_arg mty_param)
                 >>| fun () ->
                 let arg = Tgt.{ modterm_loc = arg_loc
                               ; modterm_data = Mod_longident path }
                 in
                 ( Tgt.{ modterm_loc
                       ; modterm_data = Mod_apply (funct, arg) }
                 , Tgt.subst_modtype (Subst.add param path Subst.identity) mty_res
                 )
              | _ ->
                 Error (funct.Src.modterm_loc, Application_of_nonfunctor))
         | Src.{modterm_loc=arg_loc} ->
            Error (arg.Src.modterm_loc, Application_to_non_path))

    | Src.{modterm_loc; modterm_data=Mod_constraint (modl, mty)} ->
       check_modtype env mty >>= fun mty ->
       type_modterm env modl >>= fun (modl, mty') ->
       lift_match_error modterm_loc "module type constraint"
         (modtype_match env mty' mty) >>| fun () ->
       ( Tgt.{ modterm_loc; modterm_data = Mod_constraint (modl, mty) }
       , mty
       )

  and type_structure env rev_sig rev_str seen = function
    | [] ->
       Ok ( List.rev rev_str
          , List.rev rev_sig
          )

    | Src.{stritem_loc; stritem_data=Str_value term} :: items ->
       lift_core_error stritem_loc (CT.type_term env term)
       >>= fun (term, val_items) ->
       (* FIXME: also do 'seen' *)
       let rec collect env rev_sigitems = function
         | [] -> env, rev_sigitems
         | (ident, ty) :: items ->
            let env = Env.bind_value ident ty env in
            let item = Tgt.{ sigitem_loc  = Location.generated
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
       if SeenSet.mem id seen then
         Error (stritem_loc, Repeated_name id)
       else
         let seen = SeenSet.add id seen in
         type_modterm env modl >>= fun (modl, modty) ->
         let id, env = Env.add_module id modty env in
         let sigitem =
           Tgt.{ sigitem_loc  = Location.generated
               ; sigitem_data = Sig_module (id, modty)
               }
         and stritem =
           Tgt.{ stritem_loc
               ; stritem_data = Str_module (id, modl)
               }
         in
         type_structure env (sigitem :: rev_sig) (stritem :: rev_str) seen items

    | Src.{stritem_loc; stritem_data=Str_type (id, kind, typ)} :: items ->
       if SeenSet.mem id seen then
         Error (stritem_loc, Repeated_name id)
       else
         let seen = SeenSet.add id seen in
         lift_core_error stritem_loc (CT.check_kind env kind) >>= fun kind ->
         lift_core_error stritem_loc (CT.check_deftype env kind typ)
         >>= fun typ ->
         let tydecl = {Tgt.kind = kind; manifest = Some typ} in
         let id, env = Env.add_type id tydecl env in
         let sigitem =
           Tgt.{ sigitem_loc = Location.generated
               ; sigitem_data = Sig_type (id, tydecl)
               }
         and stritem =
           Tgt.{ stritem_loc
               ; stritem_data = Str_type (id, kind, typ)
               }
         in
         type_structure env (sigitem :: rev_sig) (stritem :: rev_str) seen items

    | Src.{stritem_loc; stritem_data=Str_modty (id, mty)} :: items ->
       check_modtype env mty >>= fun mty ->
       let id, env = Env.add_modty id mty env in
       let sigitem =
         Tgt.{ sigitem_loc  = Location.generated
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
