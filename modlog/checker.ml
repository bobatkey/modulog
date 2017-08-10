module Core = struct
  include Core_syntax.Make (Modules.Syntax.Bound_names)

  let rec subst_deftype sub domtype =
    { domtype with
        domtype_data =
          match domtype.domtype_data with
            | Type_int          -> Type_int
            | Type_typename lid -> Type_typename (Modules.Subst.path sub lid)
            | Type_tuple tys    -> Type_tuple (List.map (subst_deftype sub) tys)
            | Type_enum syms    -> Type_enum syms
    }


  let subst_valtype sub = function
    | Predicate predty ->
       Predicate
         { predty with
             predty_data = List.map (subst_deftype sub) predty.predty_data
         }

    | Value domty ->
       Value (subst_deftype sub domty)

  let subst_kind sub () = ()
end

module Mod = Modules.Syntax.Mod_Syntax (Core)

module Env = Modules.Typing.Env (Mod)

module Core_typing = struct
  module Src  = Syntax.Core
  module Core = Core

  module Env = Env

  open Rresult

  type core_error_detail =
    | Lookup_error of Modules.Typing.lookup_error
    | Type_mismatch of { expected : Core.def_type
                       ; actual   : Core.def_type }
    | Expr_is_tuple of { expected : Core.def_type }
    | Tuple_too_short
    | Tuple_too_long
    | Unsafe of [`Unbound_var | `Catch_all]
    | Name_mismatch of { expected : string
                       ; actual   : string
                       }
    | Enum_sym_not_found of string * string list
    | Expr_is_enum of { expected : Core.def_type }
    | Predicate_reference_in_expr of Src.Names.longident
    | Constant_reference_in_atom of Src.Names.longident

  type core_error = Location.t * core_error_detail

  let pp_error pp (loc, detail) =
    Format.fprintf pp "at %a:@ "
      Location.pp_without_filename loc;
    match detail with
      | Lookup_error lookup_error ->
         Modules.Typing.pp_lookup_error pp lookup_error
      | Type_mismatch { expected; actual } ->
         Format.fprintf pp
           "@[<hv 0>This expression has type@[<4>@ %a@]@ but was expected to have type@[<4>@ %a@]@]@;"
           Core.pp_domaintype actual
           Core.pp_domaintype expected
      | Expr_is_tuple { expected } ->
         Format.fprintf pp
           "@[<v 4>This expression is a tuple, but was expected to have type@,%a@]@;"
           Core.pp_domaintype expected
      | Tuple_too_short ->
         Format.fprintf pp "Not enough elements in tuple expression"
      | Tuple_too_long ->
         Format.fprintf pp "Too many elements in tuple expression"
      | Unsafe `Unbound_var ->
         Format.fprintf pp "Rule is unsafe: unbound variable in head"
      | Unsafe `Catch_all ->
         Format.fprintf pp "Rule is unsafe: catch all expression in head"
      | Name_mismatch { expected; actual } ->
         Format.fprintf pp "Predicate declaration has name %S, but this rule is given name %S"
           expected
           actual
      | Enum_sym_not_found (sym, syms) ->
         Format.fprintf pp
           "Symbol %a is not a member of the enumerated type @[<1>[%a]@]"
           Src.pp_enum_sym sym
           (Fmt.list ~sep:(Fmt.always "|@ ") Src.pp_enum_sym) syms
      | Expr_is_enum { expected } ->
         Format.fprintf pp
           "@[<v 4>Expression is a symbol, but the expected type is@,%a@]@;"
           Core.pp_domaintype expected
      | Predicate_reference_in_expr lid ->
         Format.fprintf pp
           "@[<hv>Reference@ to@ predicate@ %a,@ where@ a@ reference@ to@ a@ constant@ value@ was@ expected@]"
           Src.Names.pp_longident lid
      | Constant_reference_in_atom lid ->
         Format.fprintf pp
           "@[<hv>Reference@ to@ constant@ %a,@ where@ a@ reference@ to@ a@ predicate@ was@ expected@]"
           Src.Names.pp_longident lid

  let lift_lookup_error loc r =
    R.reword_error (fun e -> (loc, Lookup_error e)) r

  let rec check_deftype env () = function
    | Src.{ domtype_loc; domtype_data=Type_int} ->
       Ok Core.{ domtype_loc
               ; domtype_data = Type_int
               }
    | Src.{domtype_loc; domtype_data=Type_typename path} ->
       lift_lookup_error domtype_loc (Env.find_type path env)
       >>| fun (path, _) ->
       Core.{ domtype_loc
            ; domtype_data = Type_typename path
            }
    | Src.{domtype_loc; domtype_data=Type_tuple tys} ->
       check_deftypes env [] tys >>| fun tys ->
       Core.{ domtype_loc
            ; domtype_data = Type_tuple tys
            }
    | Src.{domtype_loc; domtype_data=Type_enum syms} ->
       (* FIXME: check for duplicates *)
       Ok Core.{ domtype_loc
               ; domtype_data = Type_enum syms
               }

  and check_deftypes env rev_tys = function
    | [] -> Ok (List.rev rev_tys)
    | ty :: tys ->
       check_deftype env () ty >>= fun ty ->
       check_deftypes env (ty :: rev_tys) tys

  let check_predtype env Src.{predty_loc; predty_data} =
    check_deftypes env [] predty_data >>| fun tys ->
    Core.{ predty_loc; predty_data = tys }


  let check_valtype env = function
    | Src.Predicate predty ->
       check_predtype env predty >>| fun predty ->
       Core.Predicate predty
    | Src.Value domty ->
       check_deftype env () domty >>| fun domty ->
       Core.Value domty


  let rec deftype_equiv env () domty1 domty2 =
    let open Core in
    match domty1, domty2 with
      | {domtype_data=Type_typename path1},
        {domtype_data=Type_typename path2}
        when Modules.Path.equal path1 path2 ->
         true
      | {domtype_data=Type_typename path}, domty
      | domty, {domtype_data=Type_typename path} ->
         (match Env.lookup_type path env with
           | {Mod.manifest=None} ->
              false
           | {Mod.manifest=Some domty'} ->
              deftype_equiv env () domty' domty)
      | {domtype_data=Type_int},
        {domtype_data=Type_int} ->
         true
      | {domtype_data=Type_tuple tys1},
        {domtype_data=Type_tuple tys2} ->
         List.length tys1 = List.length tys2 &&
         List.for_all2 (deftype_equiv env ()) tys1 tys2
      | {domtype_data=Type_enum syms1},
        {domtype_data=Type_enum syms2} ->
         syms1 = syms2
      | _, _ ->
         false

  module LocalEnv = Map.Make (String)

  let ty_int =
    Core.{ domtype_loc  = Location.generated
         ; domtype_data = Type_int
         }

  let rec domtype_is_tuple env = function
    | Core.{domtype_data=Type_typename path} ->
       (match Env.lookup_type path env with
         | Mod.{manifest=None} -> None
         | Mod.{manifest=Some ty} -> domtype_is_tuple env ty)
    | Core.{domtype_data=Type_tuple tys} ->
       Some tys
    | _ ->
       None

  let rec domtype_is_enum env = function
    | Core.{domtype_data=Type_typename path} ->
       (match Env.lookup_type path env with
         | Mod.{manifest=None} -> None
         | Mod.{manifest=Some ty} -> domtype_is_enum env ty)
    | Core.{domtype_data=Type_enum syms} ->
       Some syms
    | _ ->
       None

  (* check each expression against some expected type, using an
     environment of already bound vars. Return the translated
     expression and complete list of bound vars. *)
  let rec type_expr env local_env expected_ty = function
    | Src.{ expr_loc; expr_data = Expr_var vnm } ->
       (match LocalEnv.find vnm local_env with
         | exception Not_found ->
            Ok (Core.{ expr_loc; expr_data = Expr_var vnm },
                LocalEnv.add vnm expected_ty local_env)
         | ty ->
            if deftype_equiv env () expected_ty ty then
              Ok (Core.{ expr_loc; expr_data = Expr_var vnm },
                  local_env)
            else
              Error (expr_loc,
                     Type_mismatch { expected = expected_ty ; actual = ty }))

    | Src.{ expr_loc; expr_data = Expr_lid lid } ->
       (match Env.find_value lid env with
         | Ok (lid, Core.Value ty) ->
            if deftype_equiv env () expected_ty ty then
              Ok (Core.{ expr_loc; expr_data = Expr_lid lid }, local_env)
            else
              Error (expr_loc,
                     Type_mismatch { expected = expected_ty; actual = ty })
         | Ok (_, Core.Predicate _) ->
            Error (expr_loc, Predicate_reference_in_expr lid)
         | Error err ->
            Error (expr_loc, Lookup_error err))

    | Src.{ expr_loc; expr_data = Expr_literal i } ->
       if deftype_equiv env () expected_ty ty_int then
         Ok (Core.{ expr_loc; expr_data = Expr_literal i }, local_env)
       else
         Error (expr_loc,
                Type_mismatch { expected = expected_ty; actual = ty_int })

    | Src.{ expr_loc; expr_data = Expr_underscore } ->
       Ok (Core.{ expr_loc; expr_data = Expr_underscore }, local_env)

    | Src.{ expr_loc; expr_data = Expr_tuple exprs } ->
       (match domtype_is_tuple env expected_ty with
         | Some tys ->
            type_exprs env local_env expr_loc tys exprs >>= fun (e, local_env) ->
            Ok (Core.{ expr_loc; expr_data = Expr_tuple e }, local_env)
         | None ->
            Error (expr_loc, Expr_is_tuple { expected = expected_ty }))

    | Src.{ expr_loc; expr_data = Expr_enum sym } ->
       (match domtype_is_enum env expected_ty with
         | Some syms ->
            if List.mem sym syms then
              Ok (Core.{ expr_loc; expr_data = Expr_enum sym }, local_env)
            else
              Error (expr_loc, Enum_sym_not_found (sym, syms))
         | None ->
            Error (expr_loc, Expr_is_enum { expected = expected_ty }))

  and type_exprs env local_env loc tys exprs =
    let rec check local_env rev_exprs exprs tys =
      match exprs, tys with
        | [], [] ->
           Ok (List.rev rev_exprs, local_env)
        | [], _ ->
           Error (loc, Tuple_too_short)
        | _,  [] ->
           Error (loc, Tuple_too_long)
        | e::exprs, t::tys ->
           type_expr env local_env t e >>= fun (e, local_env) ->
           check local_env (e::rev_exprs) exprs tys
    in
    check local_env [] exprs tys


  let type_atom env local_env = function
    | Src.{atom_loc; atom_data = Atom_predicate { pred; args }} ->
       lift_lookup_error atom_loc (Env.find_value pred env)
       >>= function
       | (pred, Core.Predicate predty) ->
          type_exprs env local_env atom_loc predty.Core.predty_data args
          >>| fun (args, local_env) ->
          (Core.{ atom_loc; atom_data = Atom_predicate {pred;args}},
           local_env)
       | (_, Core.Value _) ->
          Error (atom_loc, Constant_reference_in_atom pred)


  let rec type_atoms env local_env rev_atoms = function
    | [] ->
       Ok (List.rev rev_atoms, local_env)
    | atom :: atoms ->
       type_atom env local_env atom >>= fun (atom, local_env) ->
       type_atoms env local_env (atom :: rev_atoms) atoms


  let rec bind_decls env rev_decls = function
    | [] ->
       Ok (List.rev rev_decls, env)
    | Src.{decl_name; decl_type} :: decls ->
       check_predtype env decl_type >>= fun decl_type ->
       let ident, env = Env.add_value decl_name (Core.Predicate decl_type) env in
       bind_decls env ((ident, decl_type) :: rev_decls) decls


  let rec check_safe local_env = function
    | [] -> Ok ()
    | expr :: exprs ->
       safe_expr local_env expr >>= fun () ->
       check_safe local_env exprs
  and safe_expr local_env = function
    | Src.{ expr_loc; expr_data = Expr_var vnm } ->
       if LocalEnv.mem vnm local_env then Ok ()
       else Error (expr_loc, Unsafe `Unbound_var)
    | Src.{ expr_data = Expr_literal _ | Expr_enum _ | Expr_lid _ } ->
       Ok ()
    | Src.{ expr_loc; expr_data = Expr_underscore } ->
       Error (expr_loc, Unsafe `Catch_all)
    | Src.{ expr_data = Expr_tuple exprs } ->
       check_safe local_env exprs


  let type_rule env expected_name ident tys
      Src.{rule_loc;rule_pred;rule_args;rule_rhs} =
    if rule_pred <> expected_name then
      Error (rule_loc,
             Name_mismatch { expected = expected_name; actual = rule_pred })
    else
      let local_env = LocalEnv.empty in
      type_atoms env local_env [] rule_rhs >>= fun (rule_rhs, local_env) ->
      check_safe local_env rule_args >>= fun () ->
      type_exprs env local_env rule_loc tys rule_args >>| fun (rule_args, _) ->
      Core.{ rule_loc
           ; rule_pred = ident
           ; rule_args
           ; rule_rhs }

  let rec type_rules env decl_name ident tys rev_rules = function
    | [] -> Ok (List.rev rev_rules)
    | rule :: rules ->
       type_rule env decl_name ident tys rule >>= fun rule ->
       type_rules env decl_name ident tys (rule :: rev_rules) rules

  let type_decl env (ident, predty) Src.{decl_name; decl_loc; decl_rules} =
    type_rules env decl_name ident predty.Core.predty_data [] decl_rules
    >>| fun decl_rules ->
    Core.{ decl_loc
         ; decl_name  = ident
         ; decl_type  = predty
         ; decl_rules
         }

  let rec type_decls env rev_decls idents decls =
    match idents, decls with
      | [], [] -> Ok (List.rev rev_decls)
      | ident :: idents, decl :: decls ->
         type_decl env ident decl >>= fun decl ->
         type_decls env (decl :: rev_decls) idents decls
      | _ ->
         assert false

  let type_term env = function
    | Src.PredicateDefs decls ->
       bind_decls env [] decls >>= fun (idents, env) ->
       type_decls env [] idents decls >>= fun decls ->
       Ok ( Core.PredicateDefs decls
          , List.map (fun (id, ty) -> (id, Core.Predicate ty)) idents
          )

    | Src.(ConstantDef { const_loc; const_name; const_type; const_expr }) ->
       check_deftype env () const_type >>= fun typ ->
       type_expr env LocalEnv.empty typ const_expr >>= fun (expr, _) ->
       let name = Modules.Ident.create const_name in
       Ok ( Core.(ConstantDef { const_loc
                              ; const_name = name
                              ; const_type = typ
                              ; const_expr = expr
                              })
          , [ Modules.Ident.create const_name, Core.Value typ ]
          )

  let check_kind env () =
    Ok ()


  let valtype_match env ty1 ty2 =
    match ty1, ty2 with
      | Core.Predicate predty1, Core.Predicate predty2 ->
         let open Core in
         List.length predty1.predty_data = List.length predty2.predty_data &&
         List.for_all2 (deftype_equiv env ())
           predty1.predty_data
           predty2.predty_data
      | Core.Value domty1, Core.Value domty2 ->
         deftype_equiv env () domty1 domty2
      | _ ->
         false


  let kind_match env () () =
    true


  let deftype_of_path path () =
    Core.{ domtype_loc  = Location.generated
         ; domtype_data = Type_typename path
         }
end

module Typing = Modules.Typing.Mod_typing (Syntax.Mod) (Mod) (Core_typing)

let type_structure structure =
  Typing.type_structure Env.empty structure

type error = Typing.error

let pp_error = Typing.pp_error
