(* normalise module expressions by replacing functors with the
   substituted versions. *)

open Modules_syntax

module type CORE_NORM = sig
  module Core : Modules_syntax.CORE_SYNTAX

  val subst_term : Subst.t -> Core.term -> Core.term
end

module Mod_normalise
    (Mod : Modules_syntax.MOD_SYNTAX)
    (CN  : CORE_NORM with module Core = Mod.Core) =
struct
  open Mod

  let rec subst_modterm sub = function
    | {modterm_loc; modterm_data=Mod_longident lid} ->
       { modterm_loc
       ; modterm_data = Mod_longident (Subst.path sub lid)
       }
    | {modterm_loc; modterm_data=Mod_structure str} ->
       { modterm_loc
       ; modterm_data = Mod_structure (List.map (subst_def sub) str)
       }
    | {modterm_loc; modterm_data=Mod_functor (id, mty, modl)} ->
       { modterm_loc
       ; modterm_data =
           Mod_functor (id, subst_modtype sub mty, subst_modterm sub modl)
       }
    | {modterm_loc; modterm_data=Mod_apply (modl1, modl2)} ->
       { modterm_loc
       ; modterm_data =
           Mod_apply (subst_modterm sub modl1, subst_modterm sub modl2)
       }
    | {modterm_loc; modterm_data=Mod_constraint (modl, mty)} ->
       { modterm_loc
       ; modterm_data =
           Mod_constraint (subst_modterm sub modl, subst_modtype sub mty)
       }

  and subst_def sub = function
    | {stritem_loc; stritem_data=Str_value tm} ->
       { stritem_loc
       ; stritem_data = Str_value (CN.subst_term sub tm)
       }
    | {stritem_loc; stritem_data = Str_type (id, kind, dty)} ->
       { stritem_loc
       ; stritem_data =
           Str_type (id, Core.subst_kind sub kind, Core.subst_deftype sub dty)
       }
    | {stritem_loc; stritem_data=Str_module (id, modl)} ->
       { stritem_loc
       ; stritem_data = Str_module (id, subst_modterm sub modl)
       }
    | {stritem_loc; stritem_data=Str_modty (id, mty)} ->
       { stritem_loc
       ; stritem_data = Str_modty (id, subst_modtype sub mty)
       }

  module Env : sig
    type t
    val empty : t
    val find_module : Path.t -> t -> mod_term option
    val add_module : Ident.t -> mod_term -> t -> t
    val add_type : Ident.t -> Core.kind -> Core.def_type -> t -> t
    val find_type : Path.t -> t -> (Core.kind * Core.def_type)
  end = struct
    type binding =
      | Module of mod_term
      | Type   of Core.kind * Core.def_type

    type t = binding Ident.Table.t

    let empty =
      Ident.Table.empty

    let add id binding env =
      Ident.Table.add id binding env

    let add_module id modl env =
      add id (Module modl) env

    let add_type id kind defty env =
      add id (Type (kind, defty)) env

(*    let add_item item env =
      match item.stritem_data with
        | Str_value _ | Str_type _ | Str_modty _ -> env
        | Str_module (id, modl) -> add id (Module modl) env

    let add_structure = List.fold_right add_item
*)
    let rec find path env =
      match path with
        | Path.Pident id ->
           Ident.Table.find id env
        | Path.Pdot (root, field) ->
           match find root env with
             | Some (Module {modterm_data=Mod_structure str}) ->
                Some (find_field root field Subst.identity str)
             | None ->
                None
             | _ ->
                failwith "structure expected in dot access"

    and find_field p field sub = function
      | [] ->
         failwith "no such field in structure"
      | {stritem_data=Str_value _} :: rem ->
         find_field p field sub rem
      | {stritem_data=Str_modty (id, _)} :: rem ->
         if Ident.name id = field
         then failwith "expecting to find a module"
         else
           find_field p field (Subst.add id (Path.Pdot (p, Ident.name id)) sub) rem
      | {stritem_data=Str_type (id, kind, dty)} :: rem ->
         if Ident.name id = field
         then Type (Core.subst_kind sub kind,
                    Core.subst_deftype sub dty)
         else
           find_field p field (Subst.add id (Path.Pdot (p, Ident.name id)) sub) rem
      | {stritem_data=Str_module (id, modl)} :: rem ->
         if Ident.name id = field
         then Module (subst_modterm sub modl)
         else
           find_field p field (Subst.add id (Path.Pdot (p, Ident.name id)) sub) rem

    let find_module path env =
      match find path env with
        | None               -> None
        | Some (Module modl) -> Some modl
        | Some _             -> failwith "expecting a module"

    let find_type path env =
      match find path env with
        | Some (Type (kind, defty)) -> (kind, defty)
        | _             -> failwith "expecting a type"
  end

  let rec norm_modterm env = function
    | {modterm_loc; modterm_data=Mod_longident lid} as modl ->
       (match Env.find_module lid env with
         | None ->
            (* If not found, must be abstract *)
            modl
         | Some modl ->
            modl)
    | {modterm_loc; modterm_data=Mod_structure items} ->
       let _, defs = List.fold_left norm_def (env,[]) items in
       { modterm_loc
       ; modterm_data = Mod_structure (List.rev defs)
       }
    | {modterm_loc; modterm_data=Mod_functor (id, mty, modl)} ->
       { modterm_loc
       ; modterm_data =
           Mod_functor (id, mty, norm_modterm env modl)
       }
    | {modterm_loc; modterm_data=Mod_apply (modl, arg)} ->
       (match norm_modterm env modl, arg with
         | {modterm_data=Mod_longident _} as modl, arg ->
            {modterm_loc; modterm_data=Mod_apply (modl, arg)}
         | {modterm_data=Mod_functor (id, _, modl)},
           {modterm_data=Mod_longident lid} ->
            norm_modterm env (subst_modterm Subst.(add id lid identity) modl)
         | modl, arg ->
            Format.fprintf
              Format.err_formatter
              "Failed application: %a and %a@;"
              Mod.pp_modterm modl
              Mod.pp_modterm arg;
            failwith "internal error: type error in module normalisation")
    | {modterm_data=Mod_constraint (modl, _)} ->
       modl

  and norm_def (env, defs) = function
    | {stritem_data=(Str_value _ | Str_type _ | Str_modty _)} as def ->
       (env, def :: defs)
    | {stritem_loc; stritem_data=Str_module (id, modl)} ->
       let modl = norm_modterm env modl in
       let env  = Env.add_module id modl env in
       (env, {stritem_loc; stritem_data=Str_module (id, modl)} :: defs)

  let norm_structure env items =
    let _, defs = List.fold_left norm_def (env,[]) items in
    List.rev defs
end
