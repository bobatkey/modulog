module Ruleset = Datalog.Ruleset

module List = struct
  include List

  let fold_righti f l a =
    snd (List.fold_right (fun x (i, a) -> (i+1, f i x a)) l (0,a))
end

module Eval = struct
  module Core = Checked_syntax.Core

  type 'a eval =
    Ruleset.builder -> 'a * Ruleset.builder

  let return x rules =
    (x, rules)

  let (>>=) c f rules =
    let a, rules = c rules in
    f a rules

  let run c =
    let (), rules = c Ruleset.Builder.empty in
    Ruleset.Builder.finish rules

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
    | Val_predicate of Ruleset.predicate_name * eval_type list
    | Val_const     of Core.expr

  module Eval (Env : Modules.Evaluator.EVAL_ENV
               with type eval_value = eval_value
                and type eval_type  = eval_type) =
  struct

    let rec eval_type env () = function
      | {Core.domtype_data=Type_int} ->
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
           let vnm = vnm ^ (String.concat "/" (List.rev_map string_of_int suffix))
           in
           Ruleset.Var vnm :: flexprs
        | Itype_tuple tys ->
           List.fold_righti
             (fun i ty l -> eta_expand_var vnm (i::suffix) ty l)
             tys
             flexprs

    let rec eta_expand_underscore ty flexprs =
      match ty with
        | Itype_int | Itype_enum _ ->
           Ruleset.Underscore :: flexprs
        | Itype_tuple tys ->
           List.fold_right eta_expand_underscore tys flexprs

    let num_of_symbol sym syms =
      let rec find i = function
        | [] -> failwith "internal error: dodgy enum symbol"
        | s :: _ when String.equal s sym -> Int32.of_int i
        | _ :: syms -> find (i+1) syms
      in
      find 0 syms

    let rec flatten_expr env expr ty flexprs =
      match expr, ty with
        | {Core.expr_data = Expr_var vnm}, ty ->
           eta_expand_var vnm [] ty flexprs
        | {expr_data = Expr_literal i}, Itype_int ->
           Ruleset.Lit i :: flexprs
        | {expr_data = Expr_underscore}, ty ->
           eta_expand_underscore ty flexprs
        | {expr_data = Expr_tuple exprs}, Itype_tuple tys ->
           flatten_exprs env exprs tys flexprs
        | {expr_data = Expr_enum sym}, Itype_enum syms ->
           Ruleset.Lit (num_of_symbol sym syms) :: flexprs
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
      | Core.{atom_data=Atom_predicate { pred; args }} ->
         (match Env.find pred env with
           | Some (`Value (Val_predicate (pred, typ))) ->
              let args = flatten_args env args typ in
              Ruleset.Atom {pred; args}
           | _ ->
              failwith "internal error: type error in eval_atom")

    let eval_rule env Core.{rule_pred; rule_args; rule_rhs} =
      match Env.find (Modules.Path.Pident rule_pred) env with
        | Some (`Value (Val_predicate (pred, typ))) ->
           let args = flatten_args env rule_args typ in
           let rhs  = List.map (eval_atom env) rule_rhs in
           Ruleset.{ pred; args; rhs }
        | _ ->
           failwith "internal error: type error in eval_rule"

    let builder_map f l s =
      let rec loop acc s = function
        | []   -> List.rev acc, s
        | x::l -> let y, s = f x s in loop (y::acc) s l
      in
      loop [] s l

    let builder_iter f l s =
      let rec loop s = function
        | []   -> (), s
        | x::l -> let (), s = f x s in loop s l
      in
      loop s l

    let builder f rules =
      match f rules with
        | Ok rules -> (), rules
        | Error _ -> failwith "internal error: builder error"

    let make_ident path ident =
      (* FIXME: better way of communicating structured names *)
      String.concat "_" (List.rev (Modules.Ident.name ident :: path))

    let declare_pred env path decl_name int decl_type =
      let ident     = make_ident path decl_name in
      let decl_type = List.map (eval_type env ()) decl_type.Core.predty_data in
      let arity     = arity_of_decl_type decl_type in
      let name      = Ruleset.{ident;arity} in
      builder (Ruleset.Builder.add_predicate name int) >>= fun () ->
      return (decl_name, Val_predicate (name, decl_type))

    let eval_predicate env path defs =
      builder_map
        Core.(fun decl ->
            declare_pred env path decl.decl_name `Intensional decl.decl_type)
        defs
      >>= fun bindings ->
      let env = Env.add_values bindings env in
      builder_iter
        (fun Core.{decl_rules} ->
           builder_iter
             (fun rule -> builder (Ruleset.Builder.add_rule (eval_rule env rule)))
             decl_rules)
        defs
      >>= fun () ->
      return bindings

    let eval_external env path Core.{ external_name; external_type } =
      declare_pred env path external_name `Extensional external_type
      >>= fun binding ->
      return [binding]

    let eval_term env path term =
      match term with
        | Core.PredicateDefs defs ->
           eval_predicate env path defs
        | Core.External ext ->
           eval_external env path ext
        | Core.ConstantDef {const_name;const_expr} ->
           return [ (const_name, Val_const const_expr) ]

    let eval_decl env path ident val_type =
      match val_type with
        | Core.Predicate predty ->
           declare_pred env path ident `Intensional predty >>= fun (_, typ) ->
           return typ
        | Core.Value _ ->
           failwith "internal error: unsafe recursive constant defn"
  end

end

module ModEval =
  Modules.Evaluator.Make (Checked_syntax.Mod) (Eval)

let from_structure structure =
  Eval.run (ModEval.eval_structure structure)
