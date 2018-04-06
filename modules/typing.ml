open Syntax

module type TYPING_ENV = sig
  type val_type
  type def_type
  type kind

  type t

  (* FIXME: this is needed to handle mutually recursive values. Could
     there be another way? *)
  val add_value : string -> val_type -> t -> Ident.t * t

  val find_value :
    String_names.longident ->
    t -> (Path.t * val_type, Typing_environment.lookup_error) result

  val find_type :
    String_names.longident ->
    t -> (Path.t * kind, Typing_environment.lookup_error) result

  (* FIXME: this is used to get definitions of types during type
     equality checking. *)
  val lookup_type : Path.t -> t -> kind * def_type option
end

module type CORE_TYPING = sig
  module Src  : CORE_SYNTAX_CONCRETE
  module Core : CORE_SYNTAX

  type error

  val pp_error : Format.formatter -> error -> unit

  module Checker
      (Env : TYPING_ENV
       with type val_type = Core.val_type
        and type def_type = Core.def_type
        and type kind     = Core.kind) :
  sig
    val type_term :
      Env.t -> Src.term ->
      (Core.term * (Ident.t * Core.val_type) list, error) result

    val check_deftype :
      Env.t -> Core.kind -> Src.def_type -> (Core.def_type, error) result

    val check_valtype :
      Env.t -> Src.val_type -> (Core.val_type, error) result

    val check_kind : Env.t -> Src.kind -> (Core.kind, error) result

    val valtype_match : Env.t -> Core.val_type -> Core.val_type -> bool

    val rec_safe_valtype : Env.t -> Core.val_type -> bool

    val deftype_equiv :
      Env.t -> Core.kind -> Core.def_type -> Core.def_type -> bool

    val kind_match : Env.t -> Core.kind -> Core.kind -> bool

    val deftype_of_path : Path.t -> Core.kind -> Core.def_type
  end
end

module type MOD_TYPING = sig
  module Src : MOD_SYNTAX_CONCRETE
    with type Core.Names.ident     = String_names.ident
     and type Core.Names.longident = String_names.longident

  module Tgt : MOD_SYNTAX

  module Env : Typing_environment.S with module Mod = Tgt

  type error

  val pp_error : Format.formatter -> error -> unit

  val type_modterm :
    Env.t -> Src.mod_term -> (Tgt.mod_term * Tgt.mod_type, error) result

  val type_structure :
    Env.t -> Src.structure -> (Tgt.structure * Tgt.signature, error) result
end

module Mod_typing
    (Src : MOD_SYNTAX_CONCRETE)
    (Tgt : MOD_SYNTAX
     with type Core.Location.t = Src.Core.Location.t)
    (CT  : CORE_TYPING
     with module Src  = Src.Core
      and module Core = Tgt.Core)
  : MOD_TYPING with module Src = Src
                and module Tgt = Tgt
=
struct
  module Src = Src
  module Tgt = Tgt
  module Env = Typing_environment.Make (Tgt)
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

  module TypingEnv = struct
    type def_type = Tgt.Core.def_type
    type val_type = Tgt.Core.val_type
    type kind     = Tgt.Core.kind
    include Env
  end

  module CTC = CT.Checker (TypingEnv)

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
       if CTC.valtype_match env vty1 (Tgt.Core.subst_valtype sub vty2) then
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
    CTC.kind_match env decl1.kind decl2.kind &&
    (match decl1.manifest, decl2.manifest with
      | _, None ->
         true
      | Some typ1, Some typ2 ->
         CTC.deftype_equiv env decl2.kind typ1 typ2
      | None, Some typ2 ->
         CTC.deftype_equiv env decl2.kind
           (CTC.deftype_of_path (Path.Pident id) decl1.kind)
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
              Some (CTC.deftype_of_path
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

  type error_detail =
    | Application_of_nonfunctor
    | Application_to_non_path
    | Core_error of CT.error
    | Lookup_error of Typing_environment.lookup_error
    | Match_error of string * match_error
    | Repeated_name of Src.Core.Names.ident
    | Not_a_signature
    | Type_already_manifest of string list
    | Kind_mismatch_in_with of Tgt.Core.Names.ident * Tgt.Core.kind * string list * Tgt.Core.kind * Tgt.Core.def_type
    | Path_not_found of string list
    | Unsafe_recursion

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
           Typing_environment.pp_lookup_error lookup_error
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
      | Unsafe_recursion ->
         (* FIXME: more detail -- why is the type unsafe? *)
         Format.fprintf pp
           "At %a, recursive module has unsafe type"
           Location.pp location

  let lift_lookup_error loc = function
    | Ok value  -> Ok value
    | Error err -> Error (loc, Lookup_error err)

  let lift_core_error loc = function
    | Ok value  -> Ok value
    | Error err -> Error (loc, Core_error err)

  let lift_match_error loc reason = function
    | Ok value  -> Ok value
    | Error err -> Error (loc, Match_error (reason, err))

  let lift_safety_error loc = function
    | Ok value -> Ok value
    | Error () -> Error (loc, Unsafe_recursion)

  let check_manifest env loc kind = function
    | None -> Ok None
    | Some dty ->
       lift_core_error loc @@ CTC.check_deftype env kind dty
       >>= fun dty ->
       Ok (Some dty)

  let rec subst_type_in_items env typename kind dty rev_items = function
    | [] ->
       Error `path_not_found
    | Tgt.({sigitem_data=Sig_type (ident, decl)} as item)::items
      when Ident.name ident = typename ->
       let open Tgt in
       if CTC.kind_match env kind decl.kind then
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
       lift_core_error modtype_loc (CTC.check_kind env kind) >>= fun kind ->
       lift_core_error modtype_loc (CTC.check_deftype env kind dty) >>= fun dty ->
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
  (** TODO: destructive update of a type. Removes the type declaration
      from the signature and substitutes the new definition for that
      declared name in the rest of the signature. *)

  and check_signature env rev_sig seen = function
    | [] ->
       Ok (List.rev rev_sig)

    | Src.{sigitem_loc; sigitem_data=Sig_value (id, vty)} :: rem ->
       if SeenSet.mem id seen then
         Error (sigitem_loc, Repeated_name id)
       else
         let seen = SeenSet.add id seen in
         lift_core_error sigitem_loc @@ CTC.check_valtype env vty >>= fun vty ->
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
         lift_core_error sigitem_loc @@ CTC.check_kind env kind >>= fun kind ->
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

  (* Checks to see if the module type is 'safe' for recursion: it is a
     signature, all its value declarations have safe type, and all of
     its submodules have signature type. *)
  let rec modtype_is_safe env = function
    | Tgt.{modtype_data=Modtype_longident id} ->
       modtype_is_safe env (Env.lookup_modtype id env)
    | Tgt.{modtype_data=Modtype_signature sg; modtype_loc} ->
       signature_is_safe env [] sg >>= fun sg ->
       Ok (Tgt.{modtype_loc; modtype_data=Modtype_signature sg})
    | Tgt.{modtype_data=Modtype_functor _} ->
       Error ()
    | Tgt.{modtype_data=Modtype_withtype _} ->
       failwith "internal error: unexpanded 'with type' in checked module type"

  and signature_is_safe env rev_sig = function
    | [] ->
       Ok (List.rev rev_sig)
    | (Tgt.{sigitem_data=Sig_value (ident, val_type)} as item) :: items ->
       if CTC.rec_safe_valtype env val_type then
         signature_is_safe env (item :: rev_sig) items
       else
         Error ()
    | (Tgt.{sigitem_data=Sig_type _} as item) :: items ->
       (* FIXME: not sure what to do here -- should probably reject? *)
       signature_is_safe env (item :: rev_sig) items
    | Tgt.{sigitem_data=Sig_module (ident, modty); sigitem_loc} :: items ->
       modtype_is_safe env modty >>= fun modty ->
       let item = Tgt.{sigitem_data=Sig_module (ident, modty); sigitem_loc} in
       signature_is_safe env (item :: rev_sig) items
    | (Tgt.{sigitem_data=Sig_modty _} as item) :: items ->
       signature_is_safe env (item :: rev_sig) items


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
       lift_core_error stritem_loc (CTC.type_term env term)
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
         lift_core_error stritem_loc (CTC.check_kind env kind) >>= fun kind ->
         lift_core_error stritem_loc (CTC.check_deftype env kind typ)
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

    | Src.{stritem_loc; stritem_data=Str_modrec bindings} :: items ->
       (* Plan:
          1. Check all the module types in the current environment
             (no recursive module types yet)
             (additionally, all the module types must be signatures, 
              which only contain structures and values. This is because
              we can't evaluate recursively defined functors, which would cause
              looping in the evaluator. This is a slightly different safety
              condition to the OCaml one)
          2. Add the module types to the environment (tagged as recursive?)
          3. Check the module bodies in turn in the extended environment
          4. Add the module types to the environment again, this time not tagged as
             recursive.
       *)
       check_rec_module_types env env [] bindings
       >>= fun (rec_env, bindings) ->
       check_rec_module_terms rec_env env [] rev_sig bindings
       >>= fun (new_env, bindings, rev_sig) ->
       let stritem = Tgt.{ stritem_loc; stritem_data = Str_modrec bindings } in
       type_structure new_env rev_sig (stritem :: rev_str) seen items

  (* FIXME: thread the seen set through to check for repeated names *)
  and check_rec_module_types env new_env rev_acc = function
    | [] ->
       Ok (new_env, List.rev rev_acc)
    | (ident, modty, modl) :: bindings ->
       check_modtype env modty >>= fun modty ->
       lift_safety_error modty.modtype_loc
         (modtype_is_safe env modty) >>= fun modty ->
       (* FIXME: mark these bindings as recursive, information that
          can be used by the core type checker. *)
       let ident, new_env = Env.add_module ident modty new_env in
       check_rec_module_types env new_env
         ((ident, modty, modl) :: rev_acc)
         bindings

  and check_rec_module_terms rec_env new_env rev_acc rev_sig = function
    | [] ->
       Ok (new_env, List.rev rev_acc, rev_sig)
    | (ident, modty, modl) :: bindings ->
       type_modterm rec_env modl >>= fun (modl, modty') ->
       (* FIXME: better location information, and error message *)
       lift_match_error modty.modtype_loc "definition of a recursive module"
         (modtype_match rec_env modty' modty) >>= fun () ->
       let new_env = Env.bind_module ident modty new_env in
       let sigitem = Tgt.{ sigitem_loc  = Location.generated
                         ;     sigitem_data = Sig_module (ident, modty) }
       in
       check_rec_module_terms rec_env new_env
         ((ident, modty, modl) :: rev_acc)
         (sigitem :: rev_sig)
         bindings

  let type_structure env str =
    type_structure env [] [] SeenSet.empty str

end
