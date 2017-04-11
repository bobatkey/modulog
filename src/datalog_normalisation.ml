module RS = Datalog_ruleset

module Eval = struct
  module Core = Datalog_checker.Core_syntax

  open Core

  type 'a eval =
    RS.rule list -> 'a * RS.rule list

  let return x rules = (x, rules)
  let (>>=) c f rules =
    let a, rules = c rules in
    f a rules

  type eval_type =
    | Itype_int
    | Itype_tuple of eval_type list

  type eval_value = string * eval_type list

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

    let rec eta_expand_var vnm suffix ty flexprs =
      match ty with
        | Itype_int ->
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
        | Itype_int ->
           RS.Underscore :: flexprs
        | Itype_tuple tys ->
           List.fold_right eta_expand_underscore tys flexprs

    let rec flatten_expr expr ty flexprs =
      match expr, ty with
        | {expr_data = Expr_var vnm}, ty ->
           eta_expand_var vnm [] ty flexprs
        | {expr_data = Expr_literal i}, Itype_int ->
           RS.Lit i :: flexprs
        | {expr_data = Expr_underscore}, ty ->
           eta_expand_underscore ty flexprs
        | {expr_data = Expr_tuple exprs}, Itype_tuple tys ->
           flatten_exprs exprs tys flexprs
        | _ ->
           failwith "internal error: type mismatch in flatten_expr"

    and flatten_exprs exprs tys =
      List.fold_right2 flatten_expr exprs tys

    let flatten_args exprs tys =
      flatten_exprs exprs tys []

    let eval_atom env = function
      | {atom_data=Atom_predicate { pred; args }} ->
         (match Env.find pred env with
           | Some (`Value (pred, typ)) ->
              let args = flatten_args args typ in
              RS.Atom {pred; args}
           | _ ->
              failwith "internal error: type error in eval_atom")

    let eval_rule env {rule_pred; rule_args; rule_rhs} =
      match Env.find (Modules.Path.Pident rule_pred) env with
        | Some (`Value (pred, typ)) ->
           let args = flatten_args rule_args typ in
           let rhs  = List.map (eval_atom env) rule_rhs in
           RS.{ pred; args; rhs }
        | _ ->
           failwith "internal error: type error in eval_rule"

    let eval_term env term rules =
      let bindings =
        List.map
          (fun {decl_name; decl_type} ->
             let decl_type = List.map (eval_type env ()) decl_type.predty_data in
             (decl_name, (Modules.Ident.(full_name @@ create (name decl_name)), decl_type)))
          term
      in
      let env = Env.add_values bindings env in
      let rules =
        List.fold_right
          (fun {decl_rules} ->
             List.fold_right
               (fun rule -> List.cons (eval_rule env rule))
               decl_rules)
          term
          rules
      in
      bindings, rules
  end
end

module ModularDatalogEvaluator =
  Modules.Evaluator.Make (Datalog_checker.Mod) (Eval)

let rules_of_structure structure =
  RS.of_rules (snd (ModularDatalogEvaluator.norm_structure structure []))
