module Make (Names : Modules.Syntax.NAMES) = struct

  module Location = Location
  module Names = Names

  type sort_expr =
    | SVar of Names.longident
    | Sum of (string * sort_expr) list
    | Prod of sort_expr list
  (* FIXME: remove 'Enumeration' in favour of sums with no arguments *)

  type value_expr =
    | LocalVar of string
    | Variant  of string * value_expr
    | Tuple    of value_expr list
  (* FIXME: and function symbols, when we have them. *)

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

  let rec pp_expr fmt = function
    | LocalVar varname ->
      Format.fprintf fmt "%s" varname
    | Variant (symbol, Tuple []) ->
      Format.fprintf fmt "%a" pp_symbol symbol
    | Variant (symbol, expr) ->
      Format.fprintf fmt "%a %a" pp_symbol symbol pp_expr expr
    | Tuple exprs ->
      Format.fprintf fmt "(%a)"
	(Format.pp_print_list ~pp_sep:pp_comma pp_expr) exprs

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

  let rec subst_sort subst = function
    | SVar ident ->
      SVar (Subst.path subst ident)
    | Sum variants ->
      Sum (List.map (fun (lbl, sort) -> lbl, subst_sort subst sort) variants)
    | Prod sorts ->
      Prod (List.map (subst_sort subst) sorts)

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

(**********************************************************************)
(* The toplevel operations *)

type command =
  | Synth of Modules.Syntax.String_names.ident * SurfaceSyntax.mod_type
  | Display of Modules.Syntax.String_names.longident

type toplevel_item =
  | Declaration of SurfaceSyntax.str_item
  | Command     of command

type script = toplevel_item list
