open Result_ext
open Core_syntax.CheckedInnerSyntax
open Core_syntax.CheckedSyntax
open Checker

module Env = TypeChecker.Env

module LocalEnv = Map.Make (String)

module RelTable = Hashtbl.Make (Modules.Ident)

(* FIXME: this is duplicated from the type checker in Checker.ml *)
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

let rec constraints_of_formula clauses table env local_env = function
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
      constraints_of_formula clauses table env local_env predicate
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
    let v1 = constraints_of_formula clauses table env local_env p in
    let v2 = constraints_of_formula clauses table env local_env q in
    Solver.add_conj clauses [ v1; v2 ]
  | Disj (p, q) ->
    let v1 = constraints_of_formula clauses table env local_env p in
    let v2 = constraints_of_formula clauses table env local_env q in
    Solver.add_disj clauses [ v1; v2 ]
  | Impl (p, q) ->
    let v1 = constraints_of_formula clauses table env local_env p in
    let v2 = constraints_of_formula clauses table env local_env q in
    Solver.add_implies clauses v1 v2
  | Not p ->
    let v = constraints_of_formula clauses table env local_env p in
    Solver.add_not clauses v
  | Forall (x, sort, p) ->
    (match expand_sort env sort with
    | Enumeration symbols ->
      symbols
	|> List.map (fun v ->
	  constraints_of_formula clauses table env (LocalEnv.add x v local_env) p)
	  |> Solver.add_conj clauses
    | SVar _ ->
      failwith "internal error: undefined sort in synthesis")
  | Exists (x, sort, p) ->
    (match expand_sort env sort with
    | Enumeration symbols ->
      symbols
	|> List.map (fun v ->
	  constraints_of_formula clauses table env (LocalEnv.add x v local_env) p)
	  |> Solver.add_disj clauses
    | SVar _ ->
      failwith "internal error: undefined sort in synthesis")

let rec to_signature env = function
  | {modtype_loc; modtype_data=Modtype_functor _} ->
    Error (`Synth_error (modtype_loc, "Cannot synthesise module functors"))
  | {modtype_loc=_; modtype_data=Modtype_longident path} ->
    to_signature env (Env.lookup_modtype path env)
  | {modtype_loc=_; modtype_data=Modtype_withtype _} ->
    failwith "internal error: 'withtype' found in elaborated module tyoe"
  | {modtype_loc=_; modtype_data=Modtype_signature signature} ->
    Ok signature

let synthesise env _ident mod_ty =
  let* mod_ty =
    Result.map_error (fun err -> `TypeChecker err)
      (TypeChecker.check_modtype env mod_ty)
  in
  let* signature = to_signature env mod_ty in
  let env = Env.add_signature signature env in
  let table = RelTable.create 128 in
  let solver = Solver.create () in
  let rec assert_axioms = function
    | [] ->
      ()
    | {sigitem_loc=_; sigitem_data=Sig_value (_, formula)} :: items ->
      let v = constraints_of_formula solver table env LocalEnv.empty formula in
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
	  (* FIXME: I think this ought to be a fresh identifier with the same name *)
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
