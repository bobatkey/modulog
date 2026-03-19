module Make (Names : Modules.Syntax.NAMES) = struct

  module Location = Location
  module Names = Names

  type sort_expr =
    | SVar of Names.longident
    | Enumeration of string list (* FIXME: 'string' standing in for 'symbol' *)

  type value_expr =
    | LocalVar of string
    | Symbol   of string (* i.e., 'symbol' *)
  (* FIXME: and function symbols, when we have them. And other primitive
     type formers. *)

  type formula =
    | True
    | False
    | Atom of Names.longident * value_expr list
    | Eq   of value_expr * value_expr
    | Conj of formula * formula
    | Disj of formula * formula
    | Impl of formula * formula
    | Not of formula
    | Forall of string * sort_expr * formula
    | Exists of string * sort_expr * formula

  let rec conjunction = function
    | [] -> True
    | [x] -> x
    | x::xs -> Conj (x, conjunction xs)

  let rec disjunction = function
    | [] -> False
    | [x] -> x
    | x::xs -> Disj (x, disjunction xs)

  type kind =
    | Sort
    | Predicate of sort_expr list
  (* Or a function *)

  type term =
    | Check of { name : Names.ident; property : formula }
  (* FIXME: also proofs *)

  type val_type = formula

  type def_type =
    | SortDefn of sort_expr
    | PredDefn of { args : string list; predicate : formula  }
  (* Or function, defined by pattern matching on the input sort *)

  let pp_comma fmt () =
    Format.fprintf fmt ",@ "

  let pp_star fmt () =
    Format.fprintf fmt " *@ "

  let pp_symbol fmt =
    Format.fprintf fmt "%s"

  let pp_sort fmt = function
    | SVar name ->
      Names.pp_longident fmt name
    | Enumeration symbols ->
      Format.fprintf fmt "@[<hv2>{%a}@]"
	(Format.pp_print_list ~pp_sep:pp_comma pp_symbol) symbols

  let pp_expr fmt = function
    | LocalVar varname -> Format.fprintf fmt "%s" varname
    | Symbol symbol    -> pp_symbol fmt symbol

  let pp_formula =
    let rec formula fmt = function
      | Forall _ | Exists _ as f ->
	Format.fprintf fmt "@[<hov2>%a@]" quantifiers f
      | f ->
	propositional fmt f
    and quantifiers fmt = function
      | Forall (x, sort, p) ->
	Format.fprintf fmt "forall %s : %a -> %a"
	  x
	  pp_sort sort
	  quantifiers p
      | Exists (x, sort, p) ->
	Format.fprintf fmt "exists %s : %a -> %a"
	  x
	  pp_sort sort
	  quantifiers p
      | p ->
	Format.fprintf fmt "@,%a" propositional p
    and propositional fmt = function
      | Impl _ as f -> Format.fprintf fmt "@[<hov>%a@]" imps f
      | Conj _ as f -> Format.fprintf fmt "@[<hov>%a@]" conj f
      | Disj _ as f -> Format.fprintf fmt "@[<hov>%a@]" disj f
      | f -> base fmt f
    and imps fmt = function
      | Impl (p, q) -> Format.fprintf fmt "%a ->@ %a" base p imps q
      | p          -> base fmt p
    and conj fmt = function
      | Conj (p, q) -> Format.fprintf fmt "%a &@ %a" base p conj q
      | p -> base fmt p
    and disj fmt = function
      | Disj (p, q) -> Format.fprintf fmt "%a |@ %a" base p disj q
      | p -> base fmt p
    and base fmt = function
      | True -> Format.fprintf fmt "true"
      | False -> Format.fprintf fmt "false"
      | Atom (relname, exprs) ->
	Format.fprintf fmt "%a@[<hv2>(%a)@]"
	  Names.pp_longident relname
	  (Format.pp_print_list ~pp_sep:pp_comma pp_expr) exprs
      | Eq (e1, e2) ->
	Format.fprintf fmt "%a = %a"
	  pp_expr e1
	  pp_expr e2
      | Not p ->
	Format.fprintf fmt "¬%a"
	  base p
      | (Forall _ | Exists _ | Impl _ | Conj _ | Disj _) as f ->
	Format.fprintf fmt "(%a)" formula f
    in
    formula

  (* Terms are proofs of additional formulas *)
  let pp_term fmt (Check { name; property }) =
    Format.fprintf fmt "@[<hov2>check %a :@ %a@]"
      Names.pp_ident name
      pp_formula property

  (* Value declarations: which are declarations that certain properties
     hold. *)
  let pp_val_decl fmt (ident, formula) =
    Format.fprintf fmt "@[<hov2>axiom %a :@ %a@]"
      Names.pp_ident ident
      pp_formula     formula

  let pp_var fmt =
    Format.pp_print_string fmt

  let pp_def fmt = function
    | SortDefn sort_expr -> pp_sort fmt sort_expr
    | PredDefn { args; predicate } ->
      Format.fprintf fmt "@[<hov2>(%a).@ %a@]"
        (Format.pp_print_list ~pp_sep:pp_comma pp_var) args
        pp_formula predicate

  let pp_def_decl fmt = function
    | ident, Sort, None ->
      Format.fprintf fmt "sort %a" Names.pp_ident ident
    | ident, Sort, Some def ->
      Format.fprintf fmt "@[<hov2>sort %a =@ %a@]"
        Names.pp_ident ident
        pp_def def
    | ident, Predicate sorts, None ->
      Format.fprintf fmt "pred %a : (%a)"
        Names.pp_ident ident
        (Format.pp_print_list ~pp_sep:pp_comma pp_sort) sorts
    | ident, Predicate sorts, Some def ->
      Format.fprintf fmt "@[<hov2>pred %a : (%a) =@ %a@]"
        Names.pp_ident ident
        (Format.pp_print_list ~pp_sep:pp_star pp_sort) sorts
        pp_def def

  let pp_type_constraint fmt (path, kind, def) =
    match kind with
    | Sort ->
      Format.fprintf fmt "sort %s = %a"
        (String.concat "." path)
        pp_def def
    | Predicate sorts ->
      Format.fprintf fmt "@[<hov2>pred %s : (%a) =@ %a@]"
        (String.concat "." path)
        (Format.pp_print_list ~pp_sep:pp_comma pp_sort) sorts
        pp_def def
end

module SurfaceInnerSyntax = struct
  include Make (Modules.Syntax.String_names)
end

module CheckedInnerSyntax  = struct
  include Make (Modules.Syntax.Bound_names)

  module Subst = Modules.Subst

  let subst_expr _subst e = e

  let subst_sort subst = function
    | SVar ident -> SVar (Subst.path subst ident)
    | Enumeration symbols -> Enumeration symbols

  let rec subst_formula subst = function
    | True | False as f -> f
    | Atom (relname, tms) ->
      Atom (Subst.path subst relname, List.map (subst_expr subst) tms)
    | Eq (e1, e2) ->
      Eq (subst_expr subst e1, subst_expr subst e2)
    | Conj (p, q) ->
      Conj (subst_formula subst p, subst_formula subst q)
    | Disj (p, q) ->
      Disj (subst_formula subst p, subst_formula subst q)
    | Impl (p, q) ->
      Impl (subst_formula subst p, subst_formula subst q)
    | Not p ->
      Not (subst_formula subst p)
    | Forall (v, sort, p) ->
      Forall (v, subst_sort subst sort, subst_formula subst p)
    | Exists (v, sort, p) ->
      Exists (v, subst_sort subst sort, subst_formula subst p)

  let subst_valtype = subst_formula

  let subst_deftype subst = function
    | SortDefn sort -> SortDefn (subst_sort subst sort)
    | PredDefn { args; predicate } ->
      PredDefn { args; predicate = subst_formula subst predicate }

  let subst_kind subst = function
    | Sort -> Sort
    | Predicate sorts -> Predicate (List.map (subst_sort subst) sorts)
end

module SurfaceSyntax = Modules.Syntax.Mod_Syntax_Raw (SurfaceInnerSyntax)

module CheckedSyntax = Modules.Syntax.Mod_Syntax (CheckedInnerSyntax)

let ( let* ) = Result.bind

let generate_names sorts =
  let rec make idx = function
    | [] -> []
    | _::xs -> ("x" ^ string_of_int idx) :: make (idx+1) xs
  in
  make 0 sorts

module InnerTyping = struct
  module Src = SurfaceInnerSyntax
  module Core = CheckedInnerSyntax

  type error = [`Msg of string | `Lookup_error of Modules.Typing_environment.lookup_error ]

  let pp_error fmt = function
    | `Msg string ->
      Format.pp_print_string fmt string
    | `Lookup_error err ->
      Modules.Typing_environment.pp_lookup_error fmt err

  module Checker (Env : Modules.Typing.TYPING_ENV with type val_type = Core.val_type and type def_type = Core.def_type and type kind = Core.kind) = struct

    module Ctxt = Map.Make (String)

    let rec expand_sort env = function
      | Core.Enumeration _ as sort -> sort
      | Core.SVar ident as sort ->
	(match Env.lookup_type ident env with
	| _, None -> sort
	| _, Some (SortDefn defn) -> expand_sort env defn
	| _, _ -> assert false)

    let check_sort env = function
      | Src.SVar ident ->
	(match Env.find_type ident env with
	| Error err ->
	  Error (`Lookup_error err)
	| Ok (ident, Sort) ->
	  Ok (Core.SVar ident)
	| Ok (_ident, Predicate _) ->
	  Error (`Msg "expecting a sort"))
      | Src.Enumeration symbols ->
	Ok (Core.Enumeration symbols)

    let rec check_sorts env = function
      | [] ->
	Ok []
      | sort::sorts ->
	let* sort = check_sort env sort in
	let* sorts = check_sorts env sorts in
	Ok (sort::sorts)

    let eqsort env sort1 sort2 =
      match expand_sort env sort1, expand_sort env sort2 with
      | Core.SVar ident1, Core.SVar ident2 ->
	Modules.Path.equal ident1 ident2
      | Core.Enumeration symbols1, Core.Enumeration symbols2 ->
	List.sort String.compare symbols1 = List.sort String.compare symbols2
      | _ ->
	false

    let check_expr env ctxt sort expr =
      match expr with
      | Src.LocalVar var ->
	(match Ctxt.find_opt var ctxt with
	| None -> Error (`Msg "variable not in scope")
	| Some var_sort ->
	  if eqsort env sort var_sort then
	    Ok (Core.LocalVar var)
	  else
	    Error (`Msg "sorts not equal"))
      | Src.Symbol symbol ->
	(match expand_sort env sort with
	| SVar _ ->
	  Error (`Msg "symbol not known to be inhabitant of abstract sort")
	| Enumeration symbols ->
	  if List.mem symbol symbols then
	    Ok (Core.Symbol symbol)
	  else
	    Error (`Msg "symbol not an element of this type"))

    let infer_sort_of_expr _env ctxt = function
      | Src.LocalVar var ->
	(match Ctxt.find_opt var ctxt with
	| None -> Error (`Msg "variable not in scope")
	| Some sort -> Ok (Some (Core.LocalVar var, sort)))
      | Src.Symbol _ -> Ok None

    let rec check_exprs env ctxt sorts exprs =
      match sorts, exprs with
      | [], [] -> Ok []
      | [], _::_ | _::_, [] -> Error (`Msg "argument length mismatch")
      | sort::sorts, expr::exprs ->
	let* expr = check_expr env ctxt sort expr in
	let* exprs = check_exprs env ctxt sorts exprs in
	Ok (expr::exprs)

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

    let eval_expr _env ctxt = function
      | Core.LocalVar var -> Ctxt.find var ctxt
      | Core.Symbol symb  -> symb

    let rec model_check env ctxt = function
      | Core.True -> Ok true
      | Core.False -> Ok false
      | Core.Atom (r, exprs) ->
	(match Env.lookup_type r env with
	| Predicate _, Some (PredDefn { args; predicate }) ->
	  let ctxt' =
	    List.fold_left2
	      (fun ctxt' v e -> Ctxt.add v (eval_expr env ctxt e) ctxt')
	      Ctxt.empty
	      args exprs
	  in
	  model_check env ctxt' predicate
	| Predicate _, None -> Error (`Msg "abstract predicate cannot be checked")
	| _ -> failwith "internal: type error in model checker")
      | Core.Eq (e1, e2) ->
	let v1 = eval_expr env ctxt e1 in
	let v2 = eval_expr env ctxt e2 in
	Ok (String.equal v1 v2)
      | Core.Conj (p, q) ->
	let* p_value = model_check env ctxt p in
	let* q_value = model_check env ctxt q in
	Ok (p_value && q_value)
      | Core.Disj (p, q) ->
	let* p_value = model_check env ctxt p in
	let* q_value = model_check env ctxt q in
	Ok (p_value || q_value)
      | Core.Impl (p, q) ->
	let* p_value = model_check env ctxt p in
	let* q_value = model_check env ctxt q in
	Ok (not p_value || q_value)
      | Core.Not p ->
	let* p_value = model_check env ctxt p in
	Ok (not p_value)
      | Core.Forall (x, sort, p) ->
	(match expand_sort env sort with
	| Core.Enumeration symbols ->
	  let rec check_all = function
	    | [] -> Ok true
	    | symb::symbols ->
	      let* result = model_check env (Ctxt.add x symb ctxt) p in
	      if result then
		check_all symbols
	      else
		Ok false
	  in
	  check_all symbols
	| Core.SVar _ ->
	  Error (`Msg "abstract sort cannot be checked"))
      | Core.Exists (x, sort, p) ->
	(match expand_sort env sort with
	| Core.Enumeration symbols ->
	  let rec check_all = function
	    | [] -> Ok false
	    | symb::symbols ->
	      let* result = model_check env (Ctxt.add x symb ctxt) p in
	      if result then
		Ok true
	      else
		check_all symbols
	  in
	  check_all symbols
	| Core.SVar _ ->
	  Error (`Msg "abstract sort cannot be checked"))

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
	  let ctxt = List.fold_left2 (fun ctxt var sort -> Ctxt.add var sort ctxt) Ctxt.empty args sorts in
	  let* predicate = check_formula env ctxt predicate in
	  Ok (Core.PredDefn { args; predicate })
	else
	  Error (`Msg "incorrect number of arguments")
      | Core.Predicate _, Src.SortDefn _ ->
	Error (`Msg "Expecting a predicate definition, got a sort")

    let check_valtype env formula =
      check_formula env Ctxt.empty formula

    let check_kind env = function
      | Src.Sort -> Ok Core.Sort
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
      | Core.Symbol sym1, Core.Symbol sym2 ->
	String.equal sym1 sym2
      | _ -> false

    (* FIXME: should use de Bruijn indicies instead *)
    let rec alpha_equal env pairs = function
      | Core.True, Core.True -> true
      | Core.False, Core.False -> true
      | Core.Atom (r1, exprs1), Core.Atom (r2, exprs2) ->
	Modules.Path.equal r1 r2 && (* FIXME: what if they are defined? *)
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

type command =
  | Synth of Modules.Syntax.String_names.ident * SurfaceSyntax.mod_type
  | Display of Modules.Syntax.String_names.longident

type toplevel_item =
  | Declaration of SurfaceSyntax.str_item
  | Command     of command

type script = toplevel_item list

module Env = TypeChecker.Env

let display env longident =
  match Env.find_module longident env with
  | Ok (path, mod_ty) ->
    Format.print_flush ();
    Format.printf "@[<v2>module %a :@ %a@]\n"
      Modules.Syntax.Bound_names.pp_longident path
      CheckedSyntax.pp_modtype mod_ty
  | Error lookup_error ->
    Format.eprintf "Lookup error %a\n"
      Modules.Typing_environment.pp_lookup_error lookup_error

module Synthesis = struct

  open CheckedInnerSyntax

  module LocalEnv = Map.Make (String)

  module RelTable = Hashtbl.Make (Modules.Ident)

  (* FIXME: this is duplicated from the type checker above *)
  let rec expand_sort env = function
    | Enumeration _ as sort -> sort
    | SVar ident as sort ->
      (match Env.lookup_type ident env with
      | _, None -> sort
      | _, Some (SortDefn defn) -> expand_sort env defn
      | _, _ -> assert false)

  let eval_expr local_env = function
    | LocalVar v -> LocalEnv.find v local_env
    | Symbol b   -> b

  let rec translate_formula clauses table env local_env = function
    | True ->
      Solver.add_conj clauses []
    | False ->
      Solver.add_disj clauses []
    | Atom (rel, exprs) ->
      (match Env.lookup_type rel env with
      | Sort, _ -> failwith "internal error: sort where a relation should be"
      | Predicate _, None ->
	(match rel with
	| Modules.Path.Pident ident ->
	  (* FIXME: how to check that 'rel' is local bound locally? In what cases
          would it not be if this is a top-level module signature? *)
	  let tuple = List.map (eval_expr local_env) exprs in
	  let reltable =
	    match RelTable.find table ident with
	    | exception Not_found ->
	      let t = Hashtbl.create 128 in RelTable.add table ident t; t
	    | t -> t
	  in
	  let v =
	    match Hashtbl.find reltable tuple with
	    | exception Not_found ->
	      let v = Solver.gen clauses in
	      Hashtbl.add reltable tuple v; v
	    | v -> v
	  in v
	| Modules.Path.Pdot _ ->
	  failwith "FIXME: implement synthesis for nested modules")

      (* For a defined predicate, evaluate it in the current environment *)
      | Predicate _, Some (PredDefn { args; predicate }) ->
	let local_env =
	  List.fold_left2
	    (fun le x e -> LocalEnv.add x (eval_expr local_env e) le)
	    LocalEnv.empty
	    args exprs
	in
	translate_formula clauses table env local_env predicate
      | Predicate _, Some (SortDefn _) ->
	failwith "internal error: predicate with a sort definition")
    | Eq (e1, e2) ->
      let x1 = eval_expr local_env e1 in
      let x2 = eval_expr local_env e2 in
      if String.equal x1 x2 then
	Solver.add_conj clauses []
      else
	Solver.add_disj clauses []
    | Conj (p, q) ->
      let v1 = translate_formula clauses table env local_env p in
      let v2 = translate_formula clauses table env local_env q in
      Solver.add_conj clauses [ v1; v2 ]
    | Disj (p, q) ->
      let v1 = translate_formula clauses table env local_env p in
      let v2 = translate_formula clauses table env local_env q in
      Solver.add_disj clauses [ v1; v2 ]
    | Impl (p, q) ->
      let v1 = translate_formula clauses table env local_env p in
      let v2 = translate_formula clauses table env local_env q in
      Solver.add_implies clauses v1 v2
    | Not p ->
      let v = translate_formula clauses table env local_env p in
      Solver.add_not clauses v
    | Forall (x, sort, p) ->
      (match expand_sort env sort with
      | Enumeration symbols ->
	symbols
	|> List.map (fun v ->
	  translate_formula clauses table env (LocalEnv.add x v local_env) p)
	  |> Solver.add_conj clauses
      | SVar _ ->
	failwith "internal error: undefined sort in synthesis")
    | Exists (x, sort, p) ->
      (match expand_sort env sort with
      | Enumeration symbols ->
	symbols
	|> List.map (fun v ->
	  translate_formula clauses table env (LocalEnv.add x v local_env) p)
	  |> Solver.add_disj clauses
      | SVar _ ->
	failwith "internal error: undefined sort in synthesis")

  open CheckedSyntax

  let synthesise env _ident mod_ty =
    let* mod_ty =
      Result.map_error (fun err -> `TypeChecker err)
	(TypeChecker.check_modtype env mod_ty)
    in
    let rec to_signature = function
      | {modtype_loc; modtype_data=Modtype_functor _} ->
	Error (`Synth_error (modtype_loc, "Cannot synthesise module functors"))
      | {modtype_loc=_; modtype_data=Modtype_longident path} ->
	to_signature (Env.lookup_modtype path env)
      | {modtype_loc=_; modtype_data=Modtype_withtype _} ->
	failwith "internal error: 'withtype' found in elaborated module tyoe"
      | {modtype_loc=_; modtype_data=Modtype_signature signature} ->
	Ok signature
    in
    let* signature = to_signature mod_ty in
    let env = Env.add_signature signature env in
    let table = RelTable.create 128 in
    let solver = Solver.create () in
    let rec assert_axioms = function
      | [] ->
	()
      | {sigitem_loc=_; sigitem_data=Sig_value (_, formula)} :: items ->
	let v = translate_formula solver table env LocalEnv.empty formula in
	Solver.add_assert solver v;
	assert_axioms items
      | {sigitem_loc=_; sigitem_data=Sig_type (ident, {kind=Predicate _; manifest=None})} :: items ->
	(* Create an empty table for this predicate *)
	let tbl = Hashtbl.create 128 in
	RelTable.add table ident tbl;
	assert_axioms items
      | {sigitem_loc=_; sigitem_data=Sig_type (_ident, _decl)} :: items ->
	assert_axioms items
      | {sigitem_loc=_; sigitem_data=Sig_module _} :: _items ->
	failwith "FIXME: cannot synthesis nested modules yet"
      | {sigitem_loc=_; sigitem_data=Sig_modty _} :: items ->
	(* FIXME: just skip these? *)
	assert_axioms items
    in
    assert_axioms signature;
    match Solver.solve solver with
    | None -> Error (`Synth_error (Location.generated, "Unable to synthesise"))
    | Some eval ->
      let rec build_struct rev_items = function
	| [] ->
	  List.rev rev_items
	| {sigitem_loc=_; sigitem_data=Sig_value (name, property)} :: items ->
	  (* FIXME: is it okay to reuse the identifiers? *)
	  let item = {
	    stritem_loc=Location.generated;
	    stritem_data=Str_value (Check {name; property})
	  }
	  in
	  build_struct (item :: rev_items) items
	| {sigitem_loc=_; sigitem_data=Sig_type (ident, {kind; manifest=Some defn})} :: items ->
	  let item = {
	    stritem_loc=Location.generated;
	    stritem_data=Str_type (ident, kind, defn)
	  }
	  in
	  build_struct (item :: rev_items) items
	| {sigitem_loc=_;
           sigitem_data=Sig_type (ident, {kind=Predicate sorts; manifest=None})
	  } :: items ->
	  let args = generate_names sorts in
	  let predicate =
	    RelTable.find table ident |>
            Hashtbl.to_seq |>
            Seq.filter (fun (_,v) -> eval v) |>
            Seq.map (fun (values, _) -> conjunction (List.map2 (fun x value -> Eq (LocalVar x, Symbol value)) args values)) |>
            List.of_seq |>
            disjunction
	  in
	  let item = {
	    stritem_loc = Location.generated;
	    stritem_data = Str_type (ident, Predicate sorts, PredDefn { args; predicate })
	  }
	  in
	  build_struct (item :: rev_items) items
	| _ ->
	  failwith "unexpected item in signature"
      in
      let modterm = {
	modterm_loc=Location.generated;
	modterm_data=Mod_structure (build_struct [] signature)
      } in
      Format.print_flush ();
      Format.printf "@[<v2>Synthesised:@ %a@]\n"
	pp_modterm modterm;
      (* FIXME: add this to the environment as a definition *)
      Ok ()
end

let execute_script script =
  let rec loop env = function
    | [] ->
      Ok ()
    | Declaration str_item :: items ->
      (match TypeChecker.type_str_item env str_item with
      | Ok env ->
	loop env items
      | Error err ->
	Format.eprintf "%a" TypeChecker.pp_error err; Error "err")
    | Command (Display longident) :: items ->
      display env longident;
      loop env items
    | Command (Synth (ident, modty)) :: items ->
      (match Synthesis.synthesise env ident modty with
      | Ok () ->
	loop env items
      | Error (`Synth_error (loc, string)) ->
	Format.eprintf "at %a; %s\n" Location.pp loc string;
	Error "script failed"
      | Error (`TypeChecker err) ->
	Format.eprintf "%a\n" TypeChecker.pp_error err; Error "err")
  in
  loop Env.empty script
