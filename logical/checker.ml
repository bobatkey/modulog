open Core_syntax
open Result_ext

let generate_names sorts =
  let rec make idx = function
    | [] -> []
    | _::xs -> ("x" ^ string_of_int idx) :: make (idx+1) xs
  in
  make 0 sorts

module Enum = struct
  type 'a t = 'a list option
  let return x = Some [x]
  let fail = None
  let of_list xs = Some xs
  let ( let* ) x f = match x with
  | None -> None
  | Some xs ->
    let rec loop collected = function
      | [] -> Some collected
      | x::xs -> (match f x with None -> None
      | Some ys -> loop (ys @ collected) xs)
    in
    loop [] xs
end

module InnerTyping = struct
  module Src = SurfaceInnerSyntax
  module Core = CheckedInnerSyntax

  type error =
    [ `Msg of string
    | `Lookup_error of Modules.Typing_environment.lookup_error ]

  let pp_error fmt = function
    | `Msg string ->
      Format.pp_print_string fmt string
    | `Lookup_error err ->
      Modules.Typing_environment.pp_lookup_error fmt err

  module type ENV = Modules.Typing.TYPING_ENV
                        with type val_type = Core.val_type
                         and type def_type = Core.def_type
                         and type kind = Core.kind

  module Evaluation (Env : ENV) = struct
    open Core

    (* Expand a sort expression down to the next constructor *)
    let rec expand_sort env = function
      | Sum _ | Prod _ as sort -> sort
      | SVar ident as sort ->
	(match Env.lookup_type ident env with
	| _, None -> sort
	| _, Some (SortDefn defn) -> expand_sort env defn
	| _, _ -> assert false)

    type value =
      | VTruth of bool
      | VTuple of value list
      | VVariant of symbol * value

    let rec expr_of_value = function
      | VTruth true -> Core.True
      | VTruth false -> Core.False
      | VTuple values -> Core.Tuple (List.map expr_of_value values)
      | VVariant (lbl, value) -> Core.Variant (lbl, expr_of_value value)

    (* Enumerate all the values of a sort, returning [None] if this sort is
       not entirely ground. *)
    let rec enumerate_sort env =
      let open Enum in
      function
	| SVar ident ->
	  (match Env.lookup_type ident env with
	  | _, None -> fail
	  | _, Some (SortDefn defn) -> enumerate_sort env defn
	  | _, _ -> assert false)
	| Sum variants ->
	  let* lbl, sort = of_list variants in
	  let* value = enumerate_sort env sort in
	  return (VVariant (lbl, value))
	| Prod sorts ->
	  let rec loop values = function
	    | [] ->
	      return (VTuple (List.rev values))
	    | sort::sorts ->
	      let* value = enumerate_sort env sort in
	      loop (value::values) sorts
	  in
	  loop [] sorts

    module LocalEnv = Map.Make (String)

    (* Expression evaluation. This assumes that all sorts and subexpressions
       are fully defined, so it can always evaluate to a ground value *)

    exception Evaluation_error of string

    let truth_value = function
      | VTruth v -> v
      | _ -> failwith "internal error: type error in expression evaluation"

    let rec eval_expr env ctxt = function
      | Core.Var var ->
	LocalEnv.find var ctxt
      | Core.Variant (lbl, expr) ->
	VVariant (lbl, eval_expr env ctxt expr)
      | Core.Tuple exprs ->
	VTuple (List.map (eval_expr env ctxt) exprs)
      | Core.Case (expr, cases) ->
	(match eval_expr env ctxt expr with
	| VVariant (lbl, value) ->
	  let var, body = List.assoc lbl cases in
	  let ctxt = LocalEnv.add var value ctxt in
	  eval_expr env ctxt body
	| _ ->
	  failwith "internal error: type error in expression evaluation")
      | Core.Project (expr, idx) ->
	(match eval_expr env ctxt expr with
	| VTuple values ->
	  List.nth values idx
	| _ ->
	  failwith "internal error: type error in expression evaluation")
      | Core.App (nm, exprs) ->
	(match Env.lookup_type nm env with
	| (Predicate _ | Function _), Some (ExprDefn { args; body }) ->
    	  let ctxt' =
	    List.fold_left2
	      (fun ctxt' v e -> LocalEnv.add v (eval_expr env ctxt e) ctxt')
	      LocalEnv.empty
	      args exprs
	  in
	  eval_expr env ctxt' body
	| (Predicate _ | Function _), None ->
	  raise (Evaluation_error "abstract symbol found in evaluation")
	| _ ->
	  failwith "internal error: type error in expression evaluation")
      | Core.True ->
	VTruth true
      | Core.False ->
	VTruth false
      | Core.Eq (e1, e2) ->
	let v1 = eval_expr env ctxt e1 in
	let v2 = eval_expr env ctxt e2 in
	VTruth (v1 = v2) (* FIXME: write a better equality check *)
      | Core.Conj (p, q) ->
	let p_value = eval_expr env ctxt p in
	let q_value = eval_expr env ctxt q in
	VTruth (truth_value p_value && truth_value q_value)
      | Core.Disj (p, q) ->
	let p_value = eval_expr env ctxt p in
	let q_value = eval_expr env ctxt q in
	VTruth (truth_value p_value || truth_value q_value)
      | Core.Impl (p, q) ->
	let p_value = eval_expr env ctxt p in
	let q_value = eval_expr env ctxt q in
	VTruth (not (truth_value p_value) || truth_value q_value)
      | Core.Iff (p, q) ->
	let p_value = eval_expr env ctxt p in
	let q_value = eval_expr env ctxt q in
	VTruth (truth_value p_value = truth_value q_value)
      | Core.Not p ->
	let p_value = eval_expr env ctxt p in
	VTruth (not (truth_value p_value))
      | Core.Forall (x, sort, p) ->
	(match enumerate_sort env sort with
	| Some values ->
	  let rec check_all = function
	    | [] -> VTruth true
	    | symb::symbols ->
	      let result = eval_expr env (LocalEnv.add x symb ctxt) p in
	      if truth_value result then
		check_all symbols
	      else
		VTruth false
	  in
	  check_all values
	| None ->
	  raise (Evaluation_error "abstract sort cannot be checked"))
      | Core.Exists (x, sort, p) ->
	(match enumerate_sort env sort with
	| Some values ->
	  let rec check_all = function
	    | [] -> VTruth false
	    | symb::symbols ->
	      let result = eval_expr env (LocalEnv.add x symb ctxt) p in
	      if truth_value result then
		VTruth true
	      else
		check_all symbols
	  in
	  check_all values
	| None ->
	  raise (Evaluation_error "abstract sort cannot be checked"))
  end

  module Checker (Env : ENV) =
  struct

    include Evaluation (Env)

    let rec wf_sort env = function
      | Src.SVar ident ->
	(match Env.find_type ident env with
	| Error err ->
	  Error (`Lookup_error err)
	| Ok (ident, Sort) ->
	  Ok (Core.SVar ident)
	| Ok (_ident, (Predicate _ | Function _)) ->
	  Error (`Msg "expecting a sort"))
      | Src.Sum variants ->
	(* FIXME: check for duplicate labels, and normalise to a better internal
           representations. *)
	let variants = List.stable_sort (fun (l1,_) (l2,_) -> String.compare l1 l2) variants in
	let* variants =
	  map_result (fun (lbl, sort) ->
	    let* sort = wf_sort env sort in
	    Ok (lbl, sort))
	    variants
	in
	Ok (Core.Sum variants)
      | Src.Prod sorts ->
	let* sorts = map_result (wf_sort env) sorts in
	Ok (Core.Prod sorts)

    let rec wf_sorts env = function
      | [] ->
	Ok []
      | sort::sorts ->
	let* sort = wf_sort env sort in
	let* sorts = wf_sorts env sorts in
	Ok (sort::sorts)

  let rec eqsort env sort1 sort2 =
    match expand_sort env sort1, expand_sort env sort2 with
    | Core.SVar ident1, Core.SVar ident2 ->
      Modules.Path.equal ident1 ident2
    | Core.Sum variants1, Core.Sum variants2 ->
      (* FIXME: deal with reordering *)
      List.length variants1 = List.length variants2
        && List.for_all2 (fun (lbl1, s1) (lbl2, s2) -> String.equal lbl1 lbl2 && eqsort env s1 s2) variants1 variants2
    | Core.Prod sorts1, Core.Prod sorts2 ->
      List.length sorts1 = List.length sorts2
	&& List.for_all2 (eqsort env) sorts1 sorts2
    | _ ->
      false

  module Ctxt = Map.Make (String)

    (* In the source language, var might refer to a local variable, or it
       might refer to a previously defined constant. The sort checker
       disambiguates these. *)

  type extended_sort =
    | Prop
    | Sort of Core.sort_expr

  let eq_extended_sort env s1 s2 =
    match s1, s2 with
    | Prop, Prop ->
      true
    | Sort s1, Sort s2 ->
      eqsort env s1 s2
    | Prop, Sort _ -> false
    | Sort _, Prop -> false

  let rec all_sorts = function
    | [] -> Ok []
    | Prop::_ -> Error (`Msg "not allowed a Prop here")
    | Sort s::sorts ->
      let* sorts = all_sorts sorts in
      Ok (s::sorts)

    let rec infer_sort env ctxt = function
      | Src.Var var ->
	(match Ctxt.find_opt var ctxt with
	| Some var_sort ->
	  Ok (Core.Var var, Sort var_sort)
	| None ->
	  infer_application env ctxt (Modules.Syntax.String_names.Lid_ident var) [])
      | Src.Variant _ ->
	Error (`Msg "cannot infer type of variant expression")
      | Src.Case _ ->
	Error (`Msg "cannot infer type of case expression")
      | Src.Project (expr, idx) ->
	(let* expr, sort = infer_sort env ctxt expr in
	match sort with
	| Sort sort ->
	  (match expand_sort env sort with
	  | Core.Prod sorts ->
	    if 0 <= idx && idx < List.length sorts then
	      Ok (Core.Project (expr, idx), Sort (List.nth sorts idx))
	    else
	      Error (`Msg "index out of range")
	  | _ ->
	    Error (`Msg "attempt to project out of non tuple"))
	| Prop ->
	  Error (`Msg "attempt to project out of Prop"))
      | Src.Tuple exprs ->
	let* exprs, sorts = infer_sorts env ctxt exprs in
	let* sorts = all_sorts sorts in
	Ok (Core.Tuple exprs, Sort (Core.Prod sorts))
      | Src.App (relname, exprs) ->
	infer_application env ctxt relname exprs
      | Src.True ->
	Ok (Core.True, Prop)
      | Src.False ->
	Ok (Core.False, Prop)
      | Src.Eq (expr1, expr2) ->
	(match infer_sort env ctxt expr1, infer_sort env ctxt expr2 with
	| Ok (expr1, sort1), Ok (expr2, sort2) ->
	  if eq_extended_sort env sort1 sort2 then
	    Ok (Core.Eq (expr1, expr2), Prop)
	  else
	    Error (`Msg "sort mismatch in equality")
	| Ok (expr1, sort), Error _ ->
	  let* expr2 = check_sort env ctxt expr2 sort in
	  Ok (Core.Eq (expr1, expr2), Prop)
	| Error _, Ok (expr2, sort) ->
	  let* expr1 = check_sort env ctxt expr1 sort in
	  Ok (Core.Eq (expr1, expr2), Prop)
	| Error _ as e, _ ->
	  e)
      | Src.Conj (e1, e2) ->
	let* e1 = check_sort env ctxt e1 Prop in
	let* e2 = check_sort env ctxt e2 Prop in
	Ok (Core.Conj (e1, e2), Prop)
      | Src.Disj (e1, e2) ->
	let* e1 = check_sort env ctxt e1 Prop in
	let* e2 = check_sort env ctxt e2 Prop in
	Ok (Core.Disj (e1, e2), Prop)
      | Src.Impl (e1, e2) ->
	let* e1 = check_sort env ctxt e1 Prop in
	let* e2 = check_sort env ctxt e2 Prop in
	Ok (Core.Impl (e1, e2), Prop)
      | Src.Iff (e1, e2) ->
	let* e1 = check_sort env ctxt e1 Prop in
	let* e2 = check_sort env ctxt e2 Prop in
	Ok (Core.Iff (e1, e2), Prop)
      | Src.Not e ->
	let* e = check_sort env ctxt e Prop in
	Ok (Core.Not e, Prop)
      | Src.Forall (var, sort, e) ->
	let* sort = wf_sort env sort in
	let ctxt = Ctxt.add var sort ctxt in
	let* e = check_sort env ctxt e Prop in
	Ok (Core.Forall (var, sort, e), Prop)
      | Src.Exists (var, sort, e) ->
	let* sort = wf_sort env sort in
	let ctxt = Ctxt.add var sort ctxt in
	let* e = check_sort env ctxt e Prop in
	Ok (Core.Exists (var, sort, e), Prop)

  and infer_sorts env ctxt = function
    | [] -> Ok ([], [])
    | expr::exprs ->
      let* expr, sort = infer_sort env ctxt expr in
      let* exprs, sorts = infer_sorts env ctxt exprs in
      Ok (expr::exprs, sort::sorts)

  and infer_application env ctxt nm exprs =
    match Env.find_type nm env with
    | Error lookup_error ->
      Error (`Lookup_error lookup_error)
    | Ok (_path, Sort) ->
      Error (`Msg "Cannot use a sort here (FIXME: why? where?)")
    | Ok (path, Predicate sorts) ->
      let* exprs = check_sorts env ctxt exprs sorts in
      Ok (Core.App (path, exprs), Prop)
    | Ok (path, Function (sorts, rsort)) ->
      let* exprs = check_sorts env ctxt exprs sorts in
      Ok (Core.App (path, exprs), Sort rsort)

  and check_sort env ctxt expr sort =
    match expr with
    | Src.Variant (lbl, expr) ->
      (match sort with
      | Prop -> Error (`Msg "expecting a Prop here")
      | Sort sort ->
	(match expand_sort env sort with
	| Sum variants ->
	  (match List.assoc_opt lbl variants with
	  | None -> Error (`Msg "this symbol is not a member of this type")
	  | Some sort ->
	    let* expr = check_sort env ctxt expr (Sort sort) in
	    Ok (Core.Variant (lbl, expr)))
	| _ ->
	  Error (`Msg "not expecting a value of sum sort")))
    | Src.Tuple exprs ->
      (match sort with
      | Sort sort ->
	(match expand_sort env sort with
	| Prod sorts ->
	  let* exprs = check_sorts env ctxt exprs sorts in
	  Ok (Core.Tuple exprs)
	| _ ->
	  Error (`Msg "given term is tuple, but not expecting a term of product type"))
      | Prop ->
	Error (`Msg "given term is tuple, but not expecting a term of product type"))
    | Src.Case (expr, cases) ->
      (let* expr, csort = infer_sort env ctxt expr in
      match csort with
      | Prop ->
	(* But maybe we should be able to? *)
	Error (`Msg "cannot analyse a Prop by cases")
      | Sort csort ->
	(match expand_sort env csort with
	| Sum variants ->
	  let cases = List.stable_sort (fun (l1,_) (l2,_) -> String.compare l1 l2) cases in
	  let rec match_cases variants cases =
	    match variants, cases with
	    | [], [] ->
	      Ok []
	    | (vlbl, vsort)::variants, (clbl, (v, expr))::cases ->
	      if String.equal vlbl clbl then
		let ctxt = Ctxt.add v vsort ctxt in
		let* expr = check_sort env ctxt expr sort in
		let* cases = match_cases variants cases in
		Ok ((clbl, (v, expr))::cases)
	      else
		Error (`Msg "incorrect cases")
	    | _ ->
	      Error (`Msg "incorrect cases")
	  in
	  let* cases = match_cases variants cases in
	  Ok (Core.Case (expr, cases))
	| _ ->
	  Error (`Msg "")))
    | expr ->
      let* expr, sort' = infer_sort env ctxt expr in
      if eq_extended_sort env sort sort' then
	Ok expr
      else
	Error (`Msg "sort mismatch")

  and check_sorts env ctxt exprs sorts =
    match exprs, sorts with
    | [], [] ->
      Ok []
    | expr::exprs, sort::sorts ->
      let* expr = check_sort env ctxt expr (Sort sort) in
      let* exprs = check_sorts env ctxt exprs sorts in
      Ok (expr::exprs)
    | _ ->
      Error (`Msg "incorrect number of expressions")

    (**********************************************************************)
    (* The actual core typing interface *)
    let type_term env (Src.Check { name; property }) =
      let name      = Modules.Ident.create name in
      let* property = check_sort env Ctxt.empty property Prop in
      let check_result = eval_expr env LocalEnv.empty property in
      if truth_value check_result then
	Ok (Core.Check { name; property }, [ name, Core.Property property ])
      else
	Error (`Msg "property not true in this structure")

    let check_deftype env kind defn =
      match kind, defn with
      | Core.Sort, Src.SortDefn sort ->
	let* sort = wf_sort env sort in
	Ok (Core.SortDefn sort)
      | Core.Sort, Src.ExprDefn _ ->
	Error (`Msg "Expecting a sort, got a definition")
      | Core.Predicate sorts, Src.ExprDefn { args; body } ->
	if List.length sorts = List.length args then
	  let ctxt =
	    List.fold_left2
	      (fun ctxt var sort -> Ctxt.add var sort ctxt)
	      Ctxt.empty
	      args sorts
	  in
	  let* body = check_sort env ctxt body Prop in
	  Ok (Core.ExprDefn { args; body })
	else
	  Error (`Msg "incorrect number of arguments")
      | Core.Function (sorts, rsort), Src.ExprDefn { args; body } ->
	if List.length sorts = List.length args then
	  let ctxt =
	    List.fold_left2
	      (fun ctxt var sort -> Ctxt.add var sort ctxt)
	      Ctxt.empty
	      args sorts
	  in
	  let* body = check_sort env ctxt body (Sort rsort) in
	  Ok (Core.ExprDefn { args; body })
	else
	  Error (`Msg "incorrect number of arguments")
      | (Core.Predicate _ | Core.Function _), Src.SortDefn _ ->
	Error (`Msg "Expecting a definition, got a sort")

    let check_valtype env (Src.Property formula) =
      let* formula = check_sort env Ctxt.empty formula Prop in
      Ok (Core.Property formula)

    let check_kind env = function
      | Src.Sort ->
	Ok Core.Sort
      | Src.Predicate sorts ->
	let* sorts = wf_sorts env sorts in
	Ok (Core.Predicate sorts)
      | Src.Function (sorts, rsort) ->
	let* sorts = wf_sorts env sorts in
	let* rsort = wf_sort env rsort in
	Ok (Core.Function (sorts, rsort))

    (**********************************************************************)
    (* FIXME: this ought to be in the Evaluation module, and it would be
       better to be using de Bruijn indicies. *)
    let rec vars_eq x1 x2 = function
      | [] -> x1 = x2
      | (y1, y2) :: pairs ->
	(x1 = y1 && x2 = y2) || (x1 <> y1 && x2 <> y2 && vars_eq x1 x2 pairs)

    (* FIXME: should use de Bruijn indicies instead *)
    let rec alpha_equal env pairs e1 e2 =
      match e1, e2 with
      | Core.Var x, Core.Var y ->
	vars_eq x y pairs
      | Core.Variant (lbl1, expr1), Core.Variant (lbl2, expr2) ->
	String.equal lbl1 lbl2 && alpha_equal env pairs expr1 expr2
      | Core.Tuple exprs1, Core.Tuple exprs2 ->
	List.length exprs1 = List.length exprs2
	&& List.for_all2 (alpha_equal env pairs) exprs1 exprs2
      | Core.True, Core.True -> true
      | Core.False, Core.False -> true
      | Core.App (r1, exprs1), Core.App (r2, exprs2) ->
	Modules.Path.equal r1 r2 &&
	(* FIXME: what if they are defined? Should expand both sides and continue
           to check. *)
	List.for_all2 (alpha_equal env pairs) exprs1 exprs2
      | Core.Conj (p1, q1), Core.Conj (p2, q2)
      | Core.Disj (p1, q1), Core.Disj (p2, q2)
      | Core.Impl (p1, q1), Core.Impl (p2, q2)
      | Core.Eq (p1, q1), Core.Eq (p2, q2) ->
	alpha_equal env pairs p1 p2 && alpha_equal env pairs q1 q2
      | Core.Forall (x1, s1, p1), Core.Forall (x2, s2, p2)
      | Core.Exists (x1, s1, p1), Core.Exists (x2, s2, p2) ->
	eqsort env s1 s2 && alpha_equal env ((x1, x2)::pairs) p1 p2
      | Core.Not p1, Core.Not p2 ->
	alpha_equal env pairs p1 p2
      | _ ->
	false

    (**********************************************************************)

    let valtype_match env (Core.Property formula1) (Core.Property formula2) =
      alpha_equal env [] formula1 formula2

    (* FIXME: ought to be able to disable this at the modules level. *)
    let rec_safe_valtype _env _formula =
      false

    let deftype_equiv env kind defn1 defn2 =
      match kind, defn1, defn2 with
      | Core.Sort, Core.SortDefn sort1, Core.SortDefn sort2 ->
	eqsort env sort1 sort2
      | (Core.Predicate _ | Core.Function _),
        Core.ExprDefn { args = args1; body = predicate1 },
        Core.ExprDefn { args = args2; body = predicate2 } ->
	alpha_equal env (List.combine args1 args2) predicate1 predicate2
      | _ -> false

    let kind_match env kind1 kind2 =
      match kind1, kind2 with
      | Core.Sort, Core.Sort ->
	true
      | Core.Predicate sorts1, Core.Predicate sorts2 ->
	List.for_all2 (eqsort env) sorts1 sorts2
      | Core.Function (sorts1, rsort1), Core.Function (sorts2, rsort2) ->
	List.for_all2 (eqsort env) sorts1 sorts2 && eqsort env rsort1 rsort2
      | _ ->
	false

    let deftype_of_path path = function
      | Core.Sort ->
	Core.SortDefn (Core.SVar path)
      | Core.Predicate sorts | Core.Function (sorts, _) ->
	let args = generate_names sorts in
	let body =
	  Core.App (path, List.map (fun x -> Core.Var x) args)
	in
	Core.ExprDefn { args; body }

  end
end

module TypeChecker = Modules.Typing.Mod_typing (SurfaceSyntax) (CheckedSyntax) (InnerTyping)
