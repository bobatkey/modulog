module Core_syntax = struct
  include Datalog_syntax.Make_syntax (Modules.Bound_names)

  let rec subst_deftype sub domtype =
    { domtype with
        domtype_data =
          match domtype.domtype_data with
            | Type_int          -> Type_int
            | Type_typename lid -> Type_typename (Modules.Subst.path sub lid)
            | Type_tuple tys    -> Type_tuple (List.map (subst_deftype sub) tys)
    }

  let subst_valtype sub predty =
    { predty with predty_data = List.map (subst_deftype sub) predty.predty_data
    }

  let subst_kind sub () = ()
end

module Mod = Modules.Mod_Syntax (Core_syntax)

module Env = Modules.Env (Mod)

module Core_typing = struct
  module Src  = Datalog_syntax.Syntax
  module Core = Core_syntax

  module Env = Env

  let rec kind_deftype env = function
    | Src.{ domtype_loc; domtype_data=Type_int} ->
       Core.{ domtype_loc
            ; domtype_data = Type_int
            },
       ()
    | Src.{domtype_loc; domtype_data=Type_typename path} ->
       let path, _ = Env.find_type path env in
       Core.{ domtype_loc
            ; domtype_data = Type_typename path
            },
       ()
    | Src.{domtype_loc; domtype_data=Type_tuple tys} ->
       let tys = List.map (fun ty -> fst (kind_deftype env ty)) tys in
       Core.{ domtype_loc
            ; domtype_data = Type_tuple tys
            },
       ()

  let check_valtype env Src.{predty_loc; predty_data} =
    Core.{ predty_loc
         ; predty_data =
             List.map (fun dty -> fst (kind_deftype env dty)) predty_data
         }

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
      | _, _ ->
         false

  module LocalEnv = Map.Make (String)

  let ty_int =
    Core.{ domtype_loc  = Location.Generated
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
  
  (* check each expression against some expected type, using an
     environment of already bound vars. Return the translated
     expression and complete list of bound vars. *)
  let rec type_expr env local_env expected_ty = function
    | Src.{ expr_loc; expr_data = Expr_var vnm } ->
       (match LocalEnv.find vnm local_env with
         | exception Not_found ->
            (Core.{ expr_loc; expr_data = Expr_var vnm },
             LocalEnv.add vnm expected_ty local_env)
         | ty ->
            if deftype_equiv env () expected_ty ty then
              (Core.{ expr_loc; expr_data = Expr_var vnm },
               local_env)
            else
              failwith "type mismatch in expression")
    | Src.{ expr_loc; expr_data = Expr_literal i } ->
       if deftype_equiv env () expected_ty ty_int then
         (Core.{ expr_loc; expr_data = Expr_literal i }, local_env)
       else
         (Format.fprintf Format.err_formatter
            "Type mismatch on integer literal %ld, expected type is " i;
          Core_syntax.pp_domaintype Format.err_formatter expected_ty;
          Format.pp_flush_formatter Format.err_formatter;
          failwith "checker error")
    | Src.{ expr_loc; expr_data = Expr_underscore } ->
       (Core.{ expr_loc; expr_data = Expr_underscore }, local_env)
    | Src.{ expr_loc; expr_data = Expr_tuple exprs } ->
       (match domtype_is_tuple env expected_ty with
         | Some tys ->
            let e, local_env = type_exprs env local_env tys exprs in
            (Core.{ expr_loc; expr_data = Expr_tuple e }, local_env)
         | None ->
            failwith "tuple expression expected to have tuple type")

  and type_exprs env local_env tys exprs =
    let rec check local_env rev_exprs exprs tys =
      match exprs, tys with
        | [], [] ->
           List.rev rev_exprs, local_env
        | [], _
        | _,  [] ->
           failwith "length mismatch"
        | e::exprs, t::tys ->
           let e, local_env = type_expr env local_env t e in
           check local_env (e::rev_exprs) exprs tys
    in
    check local_env [] exprs tys


  let type_atom env local_env = function
    | Src.{atom_loc; atom_data = Atom_predicate { pred; args }} ->
       let pred, predty    = Env.find_value pred env in
       let args, local_env =
         type_exprs env local_env predty.Core.predty_data args
       in
       (Core.{ atom_loc
             ; atom_data = Atom_predicate {pred;args}},
        local_env)


  let rec type_atoms env local_env rev_atoms = function
    | [] ->
       List.rev rev_atoms, local_env
    | atom :: atoms ->
       let atom, local_env = type_atom env local_env atom in
       type_atoms env local_env (atom :: rev_atoms) atoms


  let rec bind_decls env rev_decls = function
    | [] -> List.rev rev_decls, env
    | Src.{decl_name; decl_type} :: decls ->
       let decl_type  = check_valtype env decl_type in
       let ident, env = Env.add_value decl_name decl_type env in
       bind_decls env ((decl_name, ident, decl_type) :: rev_decls) decls


  let rec check_safe local_env = List.iter (safe_expr local_env)
  and safe_expr local_env = function
    | Src.{ expr_data = Expr_var vnm } ->
       if not (LocalEnv.mem vnm local_env) then
         failwith "variable in head unbound in body"
    | Src.{ expr_data = Expr_literal _ } ->
       ()
    | Src.{ expr_data = Expr_underscore } ->
       failwith "rule is unsafe: catch-all pattern in rule head"
    | Src.{ expr_data = Expr_tuple exprs } ->
       check_safe local_env exprs
  

  let type_rule env expected_name ident tys
      Src.{rule_loc;rule_pred;rule_args;rule_rhs} =
    if rule_pred <> expected_name then
      failwith "all rules should have the same name";
    let local_env = LocalEnv.empty in
    let rule_rhs, local_env = type_atoms env local_env [] rule_rhs in
    check_safe local_env rule_args;
    let rule_args, _ = type_exprs env local_env tys rule_args in
    Core.{ rule_loc
         ; rule_pred = ident
         ; rule_args
         ; rule_rhs }


  let type_decl env (id, ident, predty) Src.{decl_name; decl_loc; decl_rules} =
    Core.{ decl_loc
         ; decl_name  = ident
         ; decl_type  = predty
         ; decl_rules =
             List.map (type_rule env decl_name ident predty.predty_data) decl_rules
         }

  let type_term env decls =
    let idents, env = bind_decls env [] decls in
    ( List.map2 (type_decl env) idents decls
    , List.map (fun (id, _, ty) -> (id, ty)) idents
    )


  let check_kind env () =
    ()


  let valtype_match env predty1 predty2 =
    let open Core in
    List.length predty1.predty_data = List.length predty2.predty_data &&
    List.for_all2 (deftype_equiv env ()) predty1.predty_data predty2.predty_data

  
  let kind_match env () () =
    true

  
  let deftype_of_path path () =
    Core.{ domtype_loc  = Location.Generated
         ; domtype_data = Type_typename path
         }
end

module Typing = Modules.Mod_typing (Datalog_syntax.Mod) (Mod) (Core_typing)
