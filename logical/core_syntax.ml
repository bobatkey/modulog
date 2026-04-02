module Make (Names : Modules.Syntax.NAMES) = struct

  module Location = Location
  module Names = Names

  type sort_expr =
    | SVar of Names.longident
    | Sum of (string * sort_expr) list
    | Prod of sort_expr list (* invariant: 1-tuples do not exist *)

  type symbol = string

  type expr =
    | Var      of string
    | Variant  of symbol * expr
    | Tuple    of expr list
    | App      of Names.longident * expr list
    | Case     of expr * (symbol * (string * expr)) list
    (* TODO: case trees etc. *)
    | True
    | False
    | Eq       of expr * expr
    | Conj     of expr * expr
    | Disj     of expr * expr
    | Impl     of expr * expr
    | Not      of expr
    | Forall   of string * sort_expr * expr
    | Exists   of string * sort_expr * expr

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
    | Function of sort_expr list * sort_expr

  type term =
    | Check of { name : Names.ident; property : expr }
  (* FIXME: also proofs *)

  type val_type =
    | Property of expr

  type def_type =
    | SortDefn of sort_expr
    | ExprDefn of { args : string list; body : expr }

  (**********************************************************************)
  (* Pretty printing *)
  let pp_comma fmt () =
    Format.fprintf fmt ",@ "

  let pp_star fmt () =
    Format.fprintf fmt " *@ "

  let pp_symbol fmt =
    Format.fprintf fmt "%s"

  let rec pp_sort fmt = function
    | SVar name ->
      Names.pp_longident fmt name
    | Sum variants ->
      let pp_variant fmt = function
	| lbl, Prod [] ->
	  Format.fprintf fmt "%a" pp_symbol lbl
	| lbl, sort_expr ->
	  Format.fprintf fmt "@[<hv2>%a :@ %a@]" pp_symbol lbl pp_sort sort_expr
      in
      Format.fprintf fmt "@[<hv2>[%a]@]"
	(Format.pp_print_list ~pp_sep:pp_comma pp_variant) variants
    | Prod sorts ->
      Format.fprintf fmt "@[<hov2>%a@]"
	(Format.pp_print_list ~pp_sep:pp_star pp_sort) sorts

  let pp_expr =
    let rec formula fmt = function
      | Forall _ | Exists _ | Impl _ as f ->
	Format.fprintf fmt "@[<hv2>%a@]" quantifiers f
      | f ->
	propositional fmt f
    and quantifiers fmt = function
      | Forall (x, sort, p) ->
	Format.fprintf fmt "@[<hv2>forall %s : %a ->@ %a@]"
	  x
	  pp_sort sort
	  quantifiers p
      | Exists (x, sort, p) ->
	Format.fprintf fmt "@[<hv2>exists %s : %a ->@ %a@]"
	  x
	  pp_sort sort
	  quantifiers p
      | Impl (p, q) ->
	Format.fprintf fmt "@[<hv2>%a ->@ %a@]" equality p quantifiers q
      | p ->
	Format.fprintf fmt "%a" propositional p
    and propositional fmt = function
      (* | Impl _ as f -> Format.fprintf fmt "@[<hov>%a@]" imps f *)
      | Conj _ as f -> Format.fprintf fmt "@[<hov>%a@]" conj f
      | Disj _ as f -> Format.fprintf fmt "@[<hov>%a@]" disj f
      | f -> equality fmt f
       (* and imps fmt = function *)
	 (*   | Impl (p, q) -> Format.fprintf fmt "%a ->@ %a" equality p imps q *)
	 (*   | p          -> equality fmt p *)
    and conj fmt = function
      | Conj (p, q) -> Format.fprintf fmt "%a &@ %a" equality p conj q
      | p -> equality fmt p
    and disj fmt = function
      | Disj (p, q) -> Format.fprintf fmt "%a |@ %a" equality p disj q
      | p -> equality fmt p
    and equality fmt = function
      | Eq (e1, e2) ->
	Format.fprintf fmt "%a == %a"
	  application e1
	  application e2
      | e ->
	application fmt e
    and application fmt = function
      | (Variant (_, Tuple []) | App (_, [])) as e ->
	base fmt e
      | App (relname, exprs) ->
	Format.fprintf fmt "@[<hv2>%a(%a)@]"
	  Names.pp_longident relname
	  (Format.pp_print_list ~pp_sep:pp_comma formula) exprs
      | Variant (lbl, expr) ->
	Format.fprintf fmt "%s %a" lbl base expr
      | Not p ->
	(* FIXME: lower the precedence of this? *)
	Format.fprintf fmt "¬%a"
	  base p
      | e -> base fmt e
    and base fmt = function
      | Case _ -> failwith "FIXME: pretty print case expressions"
      | True -> Format.fprintf fmt "true"
      | False -> Format.fprintf fmt "false"
      | Var nm -> Format.fprintf fmt "%s" nm
      | App (relname, []) -> Format.fprintf fmt "%a" Names.pp_longident relname
      | Variant (lbl, Tuple []) -> Format.fprintf fmt "%s" lbl
      | Tuple exprs ->
	Format.fprintf fmt "(%a)"
	  (Format.pp_print_list ~pp_sep:pp_comma formula) exprs
      | ( Forall _ | Exists _ | Impl _ | Conj _ | Disj _
        | Variant (_, _) | App (_, _::_) | Eq _ | Not _) as f ->
	Format.fprintf fmt "(%a)" formula f
    in
    formula

  (* Terms are proofs of additional formulas *)
  let pp_term fmt (Check { name; property }) =
    Format.fprintf fmt "@[<hov2>check %a :@ %a@]"
      Names.pp_ident name
      pp_expr property

  (* Value declarations: which are declarations that certain properties
     hold. *)
  let pp_val_decl fmt (ident, Property formula) =
    Format.fprintf fmt "@[<hov2>axiom %a :@ %a@]"
      Names.pp_ident ident
      pp_expr        formula

  let pp_var fmt =
    Format.pp_print_string fmt

  let pp_def fmt = function
    | SortDefn sort_expr ->
      pp_sort fmt sort_expr
    | ExprDefn { args; body } ->
      Format.fprintf fmt "@[<hov2>(%a).@ %a@]"
        (Format.pp_print_list ~pp_sep:pp_comma pp_var) args
        pp_expr body

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
    | ident, Function (sorts, rsort), None ->
      Format.fprintf fmt "func %a : (%a) -> %a"
        Names.pp_ident ident
        (Format.pp_print_list ~pp_sep:pp_comma pp_sort) sorts
	pp_sort rsort
    | ident, Function (sorts, rsort), Some def ->
      Format.fprintf fmt "@[<hov2>func %a : (%a) -> %a =@ %a@]"
        Names.pp_ident ident
        (Format.pp_print_list ~pp_sep:pp_star pp_sort) sorts
	pp_sort rsort
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
    | Function (sorts, rsort) ->
      Format.fprintf fmt "@[<hov2>func %s : (%a) -> %a =@ %a@]"
        (String.concat "." path)
        (Format.pp_print_list ~pp_sep:pp_comma pp_sort) sorts
	pp_sort rsort
        pp_def def
end

module SurfaceInnerSyntax = struct
  include Make (Modules.Syntax.String_names)
end

module CheckedInnerSyntax  = struct
  include Make (Modules.Syntax.Bound_names)

  module Subst = Modules.Subst

  let rec subst_sort subst = function
    | SVar ident ->
      SVar (Subst.path subst ident)
    | Sum variants ->
      Sum (List.map (fun (lbl, sort) -> lbl, subst_sort subst sort) variants)
    | Prod sorts ->
      Prod (List.map (subst_sort subst) sorts)

  let rec subst_expr subst = function
    | Var _ as e -> e
    | Variant (lbl, expr) ->
      Variant (lbl, subst_expr subst expr)
    | Tuple exprs ->
      Tuple (List.map (subst_expr subst) exprs)
    | Case (e, cases) ->
      Case (subst_expr subst e,
            List.map (fun (symb, (var, e)) -> (symb, (var, subst_expr subst e))) cases)
    | True | False as f -> f
    | App (relname, tms) ->
      App (Subst.path subst relname, List.map (subst_expr subst) tms)
    | Eq (e1, e2) ->
      Eq (subst_expr subst e1, subst_expr subst e2)
    | Conj (p, q) ->
      Conj (subst_expr subst p, subst_expr subst q)
    | Disj (p, q) ->
      Disj (subst_expr subst p, subst_expr subst q)
    | Impl (p, q) ->
      Impl (subst_expr subst p, subst_expr subst q)
    | Not p ->
      Not (subst_expr subst p)
    | Forall (v, sort, p) ->
      Forall (v, subst_sort subst sort, subst_expr subst p)
    | Exists (v, sort, p) ->
      Exists (v, subst_sort subst sort, subst_expr subst p)

  let subst_valtype subst (Property fmla) =
    Property (subst_expr subst fmla)

  let subst_deftype subst = function
    | SortDefn sort -> SortDefn (subst_sort subst sort)
    | ExprDefn { args; body } ->
      ExprDefn { args; body = subst_expr subst body }

  let subst_kind subst = function
    | Sort ->
      Sort
    | Predicate sorts ->
      Predicate (List.map (subst_sort subst) sorts)
    | Function (sorts, rsort) ->
      Function (List.map (subst_sort subst) sorts, subst_sort subst rsort)
end

module SurfaceSyntax = Modules.Syntax.Mod_Syntax_Raw (SurfaceInnerSyntax)

module CheckedSyntax = Modules.Syntax.Mod_Syntax (CheckedInnerSyntax)

(**********************************************************************)
(* The toplevel operations *)

type command =
  | Synth of Modules.Syntax.String_names.ident * SurfaceSyntax.mod_type
  | Display of Modules.Syntax.String_names.longident

type toplevel_item =
  | Declaration of SurfaceSyntax.str_item
  | Command     of command

type script = toplevel_item list
