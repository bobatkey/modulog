module RS = Datalog.Ruleset

module Eval = struct
  module Core = Checked_syntax.Core

  open Core

  type 'a eval =
    RS.Builder.t -> 'a * RS.Builder.t

  let return x rules = (x, rules)
  let (>>=) c f rules =
    let a, rules = c rules in
    f a rules

  let run c =
    let _, rules = c RS.Builder.empty in
    RS.Builder.finish rules

  type eval_type =
    | Itype_int
    | Itype_tuple of eval_type list
    | Itype_enum  of string list

  let arity_of_eval_type typ =
    let rec count = function
      | Itype_int | Itype_enum _ -> fun i -> i+1
      | Itype_tuple typs -> List.fold_right count typs
    in
    count typ 0

  let arity_of_decl_type typs =
    List.fold_left (fun a typ -> arity_of_eval_type typ + a) 0 typs

  type eval_value =
    | Val_predicate of RS.predicate_name * eval_type list
    | Val_const     of expr

  module Eval (Env : Modules.Evaluator.EVAL_ENV
               with type eval_value = eval_value
                and type eval_type = eval_type) =
  struct

    let rec eval_type env () = function
      | {domtype_data=Type_int} ->
         Itype_int
      | {domtype_data=Type_typename lid} ->
         (match Env.find lid env with
           | Some (`Type ty) -> ty
           | _ -> failwith "internal: expecting a type")
      | {domtype_data=Type_tuple tys} ->
         Itype_tuple (List.map (eval_type env ()) tys)
      | {domtype_data=Type_enum syms} ->
         Itype_enum syms

    let rec eta_expand_var vnm suffix ty flexprs =
      match ty with
        | Itype_int | Itype_enum _ ->
           let vnm =
             vnm ^ (String.concat "/" (List.map string_of_int (List.rev suffix)))
           in
           RS.Var vnm :: flexprs
        | Itype_tuple tys ->
           snd (List.fold_right
                  (fun ty (i, l) ->
                     (i+1, eta_expand_var vnm (i::suffix) ty l))
                  tys
                  (0,flexprs))

    let rec eta_expand_underscore ty flexprs =
      match ty with
        | Itype_int | Itype_enum _ ->
           RS.Underscore :: flexprs
        | Itype_tuple tys ->
           List.fold_right eta_expand_underscore tys flexprs

    let rec flatten_expr env expr ty flexprs =
      match expr, ty with
        | {expr_data = Expr_var vnm}, ty ->
           eta_expand_var vnm [] ty flexprs
        | {expr_data = Expr_literal i}, Itype_int ->
           RS.Lit i :: flexprs
        | {expr_data = Expr_underscore}, ty ->
           eta_expand_underscore ty flexprs
        | {expr_data = Expr_tuple exprs}, Itype_tuple tys ->
           flatten_exprs env exprs tys flexprs
        | {expr_data = Expr_enum sym}, Itype_enum syms ->
           let rec find i = function
             | [] -> failwith "internal error: dodgy enum symbol"
             | s :: _ when s = sym -> Int32.of_int i
             | _ :: syms -> find (i+1) syms
           in
           RS.Lit (find 0 syms) :: flexprs
        | {expr_data = Expr_lid lid}, ty ->
           (match Env.find lid env with
             | Some (`Value (Val_const expr)) ->
                flatten_expr env expr ty flexprs
             | Some _ | None ->
                failwith "internal error: failure looking up constant")
        | _ ->
           failwith "internal error: type mismatch in flatten_expr"

    and flatten_exprs env exprs tys =
      List.fold_right2 (flatten_expr env) exprs tys

    let flatten_args env exprs tys =
      flatten_exprs env exprs tys []

    let eval_atom env = function
      | {atom_data=Atom_predicate { pred; args }} ->
         (match Env.find pred env with
           | Some (`Value (Val_predicate (pred, typ))) ->
              let args = flatten_args env args typ in
              RS.Atom {pred; args}
           | _ ->
              failwith "internal error: type error in eval_atom")

    let eval_rule env {rule_pred; rule_args; rule_rhs} =
      match Env.find (Modules.Path.Pident rule_pred) env with
        | Some (`Value (Val_predicate (pred, typ))) ->
           let args = flatten_args env rule_args typ in
           let rhs  = List.map (eval_atom env) rule_rhs in
           RS.{ pred; args; rhs }
        | _ ->
           failwith "internal error: type error in eval_rule"

    let ignore_builder_error = function
      | Ok x -> x
      | Error _ -> failwith "internal error: builder error"

    let eval_predicate env defs rules =
      let bindings, rules =
        List.fold_right
          (fun {decl_name; decl_type} (bindings, rules) ->
             let ident     = Modules.Ident.name decl_name in
             let decl_type = List.map (eval_type env ()) decl_type.predty_data in
             let arity     = arity_of_decl_type decl_type in
             let name      = RS.Builder.freshen_name RS.{ident;arity} rules in
             let rules =
               ignore_builder_error (RS.Builder.add_idb_predicate name rules)
             in
             ((decl_name, Val_predicate (name, decl_type)) :: bindings, rules))
          defs
          ([], rules)
      in
      let env = Env.add_values bindings env in
      let rules =
        List.fold_right
          (fun {decl_rules} ->
             List.fold_right
               (fun rule rules ->
                  ignore_builder_error
                    (RS.Builder.add_rule (eval_rule env rule) rules))
               decl_rules)
          defs
          rules
      in
      bindings, rules

    let eval_external env { external_name; external_type } rules =
      let ident     = Modules.Ident.name external_name in
      let decl_type = List.map (eval_type env ()) external_type.predty_data in
      let arity     = arity_of_decl_type decl_type in
      let name      = RS.Builder.freshen_name RS.{ident;arity} rules in
      [ (external_name, Val_predicate (name, decl_type)) ],
      ignore_builder_error (RS.Builder.add_edb_predicate name rules)

    let eval_term env term rules =
      match term with
        | PredicateDefs defs ->
           eval_predicate env defs rules
        | External ext ->
           eval_external env ext rules
        | ConstantDef {const_name;const_expr} ->
           [ (const_name, Val_const const_expr) ],
           rules

  end
end

module Evaluator =
  Modules.Evaluator.Make (Checked_syntax.Mod) (Eval)

let from_structure structure =
  Eval.run (Evaluator.eval_structure structure)
