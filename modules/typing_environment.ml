open Syntax

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

module Make (Mod_syntax : MOD_SYNTAX) : S with module Mod = Mod_syntax =
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
