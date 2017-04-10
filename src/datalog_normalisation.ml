let rec pp_list pp fmt = function
  | [] -> ()
  | [x] -> pp fmt x
  | x::xs -> Format.fprintf fmt "%a,@ %a" pp x (pp_list pp) xs


type predicate_name = Modules.Ident.t

type flat_expr =
  | Var of string * int list
  | Lit of int32
  | Underscore

type flat_atom =
  | Atom of { pred : predicate_name; args : flat_expr list }

type flat_rule =
  { pred : predicate_name
  ; args : flat_expr list
  ; rhs  : flat_atom list
  }

let pp_expr fmt = function
  | Var (h, suffix) ->
     Format.fprintf fmt "%s%s"
       h
       (String.concat "/" (List.map string_of_int suffix))
  | Lit i ->
     Format.fprintf fmt "%ld" i
  | Underscore ->
     Format.fprintf fmt "_"

let pp_exprs = pp_list pp_expr

let pp_atom fmt = function
  | Atom { pred; args } ->
     Format.fprintf fmt "%a(@[<h>%a@])"
       Modules.Ident.pp pred
       pp_exprs args

let pp_rhs = pp_list pp_atom

let pp_rule fmt {pred; args; rhs} =
  match rhs with
    | [] ->
       Format.fprintf fmt
         "%a(@[<h>%a@])@,"
         Modules.Ident.pp pred
         pp_exprs args
    | rhs ->
       Format.fprintf fmt
         "%a(@[<h>%a@]) :- @[<hv>%a@]@,"
         Modules.Ident.pp pred
         pp_exprs args
         pp_rhs rhs

module Eval = struct
  module Core = Datalog_checker.Core_syntax

  open Core

  type 'a eval =
    flat_rule list -> 'a * flat_rule list

  let return x rules = (x, rules)
  let (>>=) c f rules =
    let a, rules = c rules in
    f a rules

  type eval_type =
    | Itype_int
    | Itype_tuple of eval_type list

  type eval_value = Modules.Ident.t * eval_type list

  module Eval (Env : Modules.Normalisation.ENV with type eval_value = eval_value and type eval_type = eval_type) =
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
           Var (vnm, List.rev suffix) :: flexprs
        | Itype_tuple tys ->
           snd (List.fold_right
                  (fun ty (i, l) ->
                     (i+1, eta_expand_var vnm (i::suffix) ty l))
                  tys
                  (0,flexprs))

    let rec eta_expand_underscore ty flexprs =
      match ty with
        | Itype_int ->
           Underscore :: flexprs
        | Itype_tuple tys ->
           List.fold_right eta_expand_underscore tys flexprs

    let rec flatten_expr expr ty flexprs =
      match expr, ty with
        | {expr_data = Expr_var vnm}, ty ->
           eta_expand_var vnm [] ty flexprs
        | {expr_data = Expr_literal i}, Itype_int ->
           Lit i :: flexprs
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
              Atom {pred; args}
           | _ ->
              failwith "internal error: type error in eval_atom")

    let eval_rule env {rule_pred; rule_args; rule_rhs} =
      match Env.find (Modules.Path.Pident rule_pred) env with
        | Some (`Value (pred, typ)) ->
           let args = flatten_args rule_args typ in
           let rhs  = List.map (eval_atom env) rule_rhs in
           { pred; args; rhs }
        | _ ->
           failwith "internal error: type error in eval_rule"

    let eval_term env term rules =
      let bindings =
        List.map
          (fun {decl_name; decl_type} ->
             let decl_type = List.map (eval_type env ()) decl_type.predty_data in
             (decl_name, (Modules.Ident.(create (name decl_name)), decl_type)))
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
  Modules.Normalisation.Evaluator (Datalog_checker.Mod) (Eval)

let rules_of_structure structure =
  List.rev (snd (ModularDatalogEvaluator.norm_structure structure []))
