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

  type error = [`Msg of string | `Lookup_error of Modules.Typing_environment.lookup_error ]

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

    module LocalEnv = Map.Make (String)

    let rec eval_expr env ctxt = function
      | Core.LocalVar var -> LocalEnv.find var ctxt
      | Core.Variant (lbl, expr) -> Core.Variant (lbl, eval_expr env ctxt expr)
      | Core.Tuple exprs -> Core.Tuple (List.map (eval_expr env ctxt) exprs)

    open Enum

    (* Enumerate all the values of a sort, returning [None] if this sort is
    not entirely ground. *)
    let rec enumerate_sort env = function
      | SVar ident ->
	(match Env.lookup_type ident env with
	| _, None -> fail
	| _, Some (SortDefn defn) -> enumerate_sort env defn
	| _, _ -> assert false)
      | Sum variants ->
	let* lbl, sort = of_list variants in
	let* value = enumerate_sort env sort in
	return (Variant (lbl, value))
      | Prod sorts ->
	let rec loop values = function
	  | [] ->
	    return (Tuple (List.rev values))
	  | sort::sorts ->
	    let* value = enumerate_sort env sort in
	    loop (value::values) sorts
	in
	loop [] sorts

  end

  module Checker (Env : ENV) = struct

    include Evaluation (Env)

    let rec check_sort env = function
      | Src.SVar ident ->
	  (match Env.find_type ident env with
	  | Error err ->
	    Error (`Lookup_error err)
	  | Ok (ident, Sort) ->
	    Ok (Core.SVar ident)
	  | Ok (_ident, Predicate _) ->
	    Error (`Msg "expecting a sort"))
	| Src.Sum variants ->
	  let* variants =
	    map_result (fun (lbl, sort) ->
	      let* sort = check_sort env sort in
	      Ok (lbl, sort))
	      variants
	  in
	  Ok (Core.Sum variants)
	| Src.Prod sorts ->
	  let* sorts = map_result (check_sort env) sorts in
	  Ok (Core.Prod sorts)

    let rec check_sorts env = function
      | [] ->
	Ok []
      | sort::sorts ->
	let* sort = check_sort env sort in
	let* sorts = check_sorts env sorts in
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

    let rec check_expr env ctxt sort = function
      | Src.LocalVar var ->
	(match Ctxt.find_opt var ctxt with
	| None -> Error (`Msg "variable not in scope")
	| Some var_sort ->
	  if eqsort env sort var_sort then
	    Ok (Core.LocalVar var)
	  else
	    Error (`Msg "sorts not equal"))
      | Src.Variant (lbl, expr) ->
	(match expand_sort env sort with
	| Sum variants ->
	  (match List.assoc_opt lbl variants with
	  | None ->
	    Error (`Msg ("variant " ^ lbl ^ " not an element of this sort"))
	  | Some sort ->
	    let* expr = check_expr env ctxt sort expr in
	    Ok (Core.Variant (lbl, expr)))
	| SVar _ | Prod _ ->
	  Error (`Msg "not a sum sort"))
      | Src.Tuple exprs ->
	(match expand_sort env sort with
	| Core.Prod sorts ->
	  let* exprs = check_exprs env ctxt sorts exprs in
	  Ok (Core.Tuple exprs)
	| SVar _ | Sum _ ->
	  Error (`Msg "not a tuple sort"))

    and check_exprs env ctxt sorts exprs =
      match sorts, exprs with
      | [], [] -> Ok []
      | [], _::_ | _::_, [] ->
	Error (`Msg "argument length mismatch")
      | sort::sorts, expr::exprs ->
	let* expr  = check_expr env ctxt sort expr in
	let* exprs = check_exprs env ctxt sorts exprs in
	Ok (expr::exprs)

    let infer_sort_of_expr _env ctxt = function
      | Src.LocalVar var ->
	(match Ctxt.find_opt var ctxt with
	| None -> Error (`Msg "variable not in scope")
	| Some sort -> Ok (Some (Core.LocalVar var, sort)))
      | Src.Variant _ | Src.Tuple _ -> Ok None

    (* Checks that a formula is well sorted *)
    let rec check_formula env ctxt = function
      | Src.True -> Ok Core.True
      | Src.False -> Ok Core.False
      | Src.Atom (relname, exprs) ->
	(match Env.find_type relname env with
	| Error err -> Error (`Lookup_error err)
	| Ok (_relname, Sort) ->
	  Error (`Msg "Expected a predicate, got a sort")
	| Ok (relname, Predicate sorts) ->
	  let* exprs = check_exprs env ctxt sorts exprs in
	  Ok (Core.Atom (relname, exprs)))
      | Src.Eq (expr1, expr2) ->
	let* result1 = infer_sort_of_expr env ctxt expr1 in
	let* result2 = infer_sort_of_expr env ctxt expr2 in
	(match result1, result2 with
	| None, None -> Error (`Msg "could not infer type of equality")
	| Some (expr1, sort1), Some (expr2, sort2) ->
	  if eqsort env sort1 sort2 then
	    Ok (Core.Eq (expr1, expr2))
	  else
	    Error (`Msg "sort mismatch in equality")
	| Some (expr1, sort), None ->
	  let* expr2 = check_expr env ctxt sort expr2 in
	  Ok (Core.Eq (expr1, expr2))
	| None, Some (expr2, sort) ->
	  let* expr1 = check_expr env ctxt sort expr1 in
	  Ok (Core.Eq (expr1, expr2)))
      | Src.Conj (p, q) ->
	let* p = check_formula env ctxt p in
	let* q = check_formula env ctxt q in
	Ok (Core.Conj (p, q))
      | Src.Disj (p, q) ->
	let* p = check_formula env ctxt p in
	let* q = check_formula env ctxt q in
	Ok (Core.Disj (p, q))
      | Src.Impl (p, q) ->
	let* p = check_formula env ctxt p in
	let* q = check_formula env ctxt q in
	Ok (Core.Impl (p, q))
      | Src.Not p ->
	let* p = check_formula env ctxt p in
	Ok (Core.Not p)
      | Src.Forall (var, sort, p) ->
	let* sort = check_sort env sort in
	let ctxt = Ctxt.add var sort ctxt in
	let* p = check_formula env ctxt p in
	Ok (Core.Forall (var, sort, p))
      | Src.Exists (var, sort, p) ->
	let* sort = check_sort env sort in
	let ctxt = Ctxt.add var sort ctxt in
	let* p = check_formula env ctxt p in
	Ok (Core.Exists (var, sort, p))

    (* To check that formulas are true, we assume that all sorts are fully
       defined and so are all the relation symbols. *)

    let rec model_check env local_env = function
      | Core.True -> Ok true
      | Core.False -> Ok false
      | Core.Atom (r, exprs) ->
	(match Env.lookup_type r env with
	| Predicate _, Some (PredDefn { args; predicate }) ->
	  let ctxt' =
	    List.fold_left2
	      (fun ctxt' v e -> Ctxt.add v (eval_expr env local_env e) ctxt')
	      Ctxt.empty
	      args exprs
	  in
	  model_check env ctxt' predicate
	| Predicate _, None -> Error (`Msg "abstract predicate cannot be checked")
	| _ -> failwith "internal: type error in model checker")
      | Core.Eq (e1, e2) ->
	let v1 = eval_expr env local_env e1 in
	let v2 = eval_expr env local_env e2 in
	Ok (v1 = v2) (* FIXME: write a better equality check *)
      | Core.Conj (p, q) ->
	let* p_value = model_check env local_env p in
	let* q_value = model_check env local_env q in
	Ok (p_value && q_value)
      | Core.Disj (p, q) ->
	let* p_value = model_check env local_env p in
	let* q_value = model_check env local_env q in
	Ok (p_value || q_value)
      | Core.Impl (p, q) ->
	let* p_value = model_check env local_env p in
	let* q_value = model_check env local_env q in
	Ok (not p_value || q_value)
      | Core.Not p ->
	let* p_value = model_check env local_env p in
	Ok (not p_value)
      | Core.Forall (x, sort, p) ->
	(match enumerate_sort env sort with
	| Some values ->
	  let rec check_all = function
	    | [] -> Ok true
	    | symb::symbols ->
	      let* result = model_check env (LocalEnv.add x symb local_env) p in
	      if result then
		check_all symbols
	      else
		Ok false
	  in
	  check_all values
	| None ->
	  Error (`Msg "abstract sort cannot be checked"))
      | Core.Exists (x, sort, p) ->
	(match enumerate_sort env sort with
	| Some values ->
	  let rec check_all = function
	    | [] -> Ok false
	    | symb::symbols ->
	      let* result = model_check env (LocalEnv.add x symb local_env) p in
	      if result then
		Ok true
	      else
		check_all symbols
	  in
	  check_all values
	| None ->
	  Error (`Msg "abstract sort cannot be checked"))

    (**********************************************************************)
    (* The actual core typing interface *)
    let type_term env (Src.Check { name; property }) =
      let name      = Modules.Ident.create name in
      let* property = check_formula env Ctxt.empty property in
      let* check_result = model_check env Ctxt.empty property in
      if check_result then
	Ok (Core.Check { name; property }, [ name, property ])
      else
	Error (`Msg "property not true in this structure")

    let check_deftype env kind defn =
      match kind, defn with
      | Core.Sort, Src.SortDefn sort ->
	let* sort = check_sort env sort in
	Ok (Core.SortDefn sort)
      | Core.Sort, Src.PredDefn _ ->
	Error (`Msg "Expecting a sort, got a predicate definition")
      | Core.Predicate sorts, Src.PredDefn { args; predicate } ->
	if List.length sorts = List.length args then
	  let ctxt =
	    List.fold_left2
	      (fun ctxt var sort -> Ctxt.add var sort ctxt)
	      Ctxt.empty
	      args sorts
	  in
	  let* predicate = check_formula env ctxt predicate in
	  Ok (Core.PredDefn { args; predicate })
	else
	  Error (`Msg "incorrect number of arguments")
      | Core.Predicate _, Src.SortDefn _ ->
	Error (`Msg "Expecting a predicate definition, got a sort")

    let check_valtype env formula =
      check_formula env Ctxt.empty formula

    let check_kind env = function
      | Src.Sort ->
	Ok Core.Sort
      | Src.Predicate sorts ->
	let* sorts = check_sorts env sorts in
	Ok (Core.Predicate sorts)

    let rec vars_eq x1 x2 = function
      | [] -> x1 = x2
      | (y1, y2) :: pairs ->
	(x1 = y1 && x2 = y2) || (x1 <> y1 && x2 <> y2 && vars_eq x1 x2 pairs)

    let expr_alpha_equal pairs expr1 expr2 =
      match expr1, expr2 with
      | Core.LocalVar x, Core.LocalVar y ->
	vars_eq x y pairs
	(* FIXME: sums and products too *)
      | _ -> false

    (* FIXME: should use de Bruijn indicies instead *)
    let rec alpha_equal env pairs = function
      | Core.True, Core.True -> true
      | Core.False, Core.False -> true
      | Core.Atom (r1, exprs1), Core.Atom (r2, exprs2) ->
	Modules.Path.equal r1 r2 && (* FIXME: what if they are defined? Should probably expand both sides and
                                       continue to check. *)
	List.for_all2 (expr_alpha_equal pairs) exprs1 exprs2
      | Core.Conj (p1, q1), Core.Conj (p2, q2)
      | Core.Disj (p1, q1), Core.Disj (p2, q2)
      | Core.Impl (p1, q1), Core.Impl (p2, q2) ->
	alpha_equal env pairs (p1, p2) && alpha_equal env pairs (q1, q2)
      | Core.Forall (x1, s1, p1), Core.Forall (x2, s2, p2)
      | Core.Exists (x1, s1, p1), Core.Exists (x2, s2, p2) ->
	eqsort env s1 s2 && alpha_equal env ((x1, x2)::pairs) (p1, p2)
      | Core.Not p1, Core.Not p2 ->
	alpha_equal env pairs (p1, p2)
      | _ ->
	false

    let valtype_match env formula1 formula2 =
      alpha_equal env [] (formula1, formula2)

    let rec_safe_valtype _env _formula =
      false

    let deftype_equiv env kind defn1 defn2 =
      match kind, defn1, defn2 with
      | Core.Sort, Core.SortDefn sort1, Core.SortDefn sort2 ->
	eqsort env sort1 sort2
      | Core.Predicate _sorts, Core.PredDefn { args = args1; predicate = predicate1 }, Core.PredDefn { args = args2; predicate = predicate2 } ->
	alpha_equal env (List.combine args1 args2) (predicate1, predicate2)
      | _ -> false

    let kind_match env kind1 kind2 =
      match kind1, kind2 with
      | Core.Sort, Core.Sort ->
	true
      | Core.Predicate sorts1, Core.Predicate sorts2 ->
	List.for_all2 (eqsort env) sorts1 sorts2
      | _ ->
	false

    let deftype_of_path path = function
      | Core.Sort ->
	Core.SortDefn (Core.SVar path)
      | Core.Predicate sorts ->
	let args = generate_names sorts in
	let predicate =
	  Core.Atom (path, List.map (fun x -> Core.LocalVar x) args)
	in
	Core.PredDefn { args; predicate }

  end
end

module TypeChecker = Modules.Typing.Mod_typing (SurfaceSyntax) (CheckedSyntax) (InnerTyping)
