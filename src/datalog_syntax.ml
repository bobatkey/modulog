module Make_syntax (Names : Modules.Syntax.NAMES) = struct

  module Location = Location

  module Names = Names

  open Names
  
  type expr =
    { expr_loc : Location.t
    ; expr_data : expr_data
    }

  and expr_data =
    | Expr_var of string
    | Expr_lid of longident
    | Expr_literal of int32
    | Expr_underscore
    | Expr_tuple of expr list
    | Expr_enum of string

  type domaintype =
    { domtype_loc  : Location.t
    ; domtype_data : domaintype_data
    }

  and domaintype_data =
    | Type_int
    | Type_typename of longident
    | Type_tuple    of domaintype list
    | Type_enum     of string list

  let pp_enum_sym fmt sym =
    Format.fprintf fmt "`%s" sym

  let rec pp_domaintype pp = function
    | { domtype_data = Type_int } ->
       Format.pp_print_string pp "int"
    | { domtype_data = Type_typename lid } ->
       Names.pp_longident pp lid
    | { domtype_data = Type_tuple tys } ->
       Format.fprintf pp "(@[<hov>%a@])" pp_domaintypes tys
    | { domtype_data = Type_enum syms } ->
       Format.fprintf pp "@[<2>[ %a ]@]"
         (Fmt.list ~sep:(Fmt.always " |@ ") pp_enum_sym) syms

  and pp_domaintypes pp tys =
    let rec lp = function
      | [] ->
         ()
      | [ty] ->
         pp_domaintype pp ty
      | ty::tys ->
         Format.fprintf pp "%a *@ " pp_domaintype ty;
         lp tys
    in
    lp tys

  type predicate_type =
    { predty_loc  : Location.t
    ; predty_data : domaintype list
    }

  type atom =
    { atom_loc  : Location.t
    ; atom_data : atom_data
    }

  and atom_data =
    | Atom_predicate of { pred : longident
                        ; args : expr list
                        }
  (* FIXME: equality, inequality, negation *)

  type rule =
    { rule_loc  : Location.t
    ; rule_pred : ident
    ; rule_args : expr list
    ; rule_rhs  : atom list
    }

  type declaration =
    { decl_loc   : Location.t
    ; decl_name  : ident
    ; decl_type  : predicate_type
    ; decl_rules : rule list
    }

  type kind = unit

  type val_type =
    | Predicate of predicate_type
    | Value     of domaintype

  type def_type = domaintype

  type constant_def =
    { const_loc  : Location.t
    ; const_name : Names.ident
    ; const_type : domaintype
    ; const_expr : expr
    }

  type term =
    | PredicateDefs of declaration list
    | ConstantDef   of constant_def

  let pp_kind =
    None

  let pp_def_decl fmt = function
    | (id, (), Some domty) ->
      Format.fprintf fmt "type %a = %a"
        Names.pp_ident id
        pp_domaintype domty
    | (id, (), None) ->
      Format.fprintf fmt "type %a"
        Names.pp_ident id

  let pp_val_decl fmt = function
    | (id, Predicate {predty_data=tys}) ->
       Format.fprintf fmt "%a : @[<hv 1>%a@]"
         Names.pp_ident id
         pp_domaintypes tys
    | (id, Value domty) ->
       Format.fprintf fmt "constant %a : %a"
         Names.pp_ident id
         pp_domaintype  domty

  let rec pp_expr fmt = function
    | {expr_data = Expr_var nm}     -> Format.fprintf fmt "?%s" nm
    | {expr_data = Expr_literal i}  -> Format.fprintf fmt "%ld" i
    | {expr_data = Expr_underscore} -> Format.pp_print_string fmt "_"
    | {expr_data = Expr_tuple es}   -> Format.fprintf fmt "(%a)" pp_exprs es
    | {expr_data = Expr_enum sym}   -> pp_enum_sym fmt sym
    | {expr_data = Expr_lid lid}    -> Names.pp_longident fmt lid

  and pp_exprs pp = function
    | [] -> ()
    | expr::exprs ->
       pp_expr pp expr;
       List.iter (Format.fprintf pp ", %a" pp_expr) exprs

  let pp_atom pp = function
    | {atom_data = Atom_predicate {pred; args}} ->
       Format.fprintf pp "%a(%a)"
         Names.pp_longident pred
         pp_exprs args

  let pp_rule pp {rule_pred; rule_args; rule_rhs} =
    Format.pp_open_hovbox pp 4;
    Format.fprintf pp "%a(%a)"
      Names.pp_ident rule_pred
      pp_exprs rule_args;
    (match rule_rhs with
      | [] -> ()
      | atom::atoms ->
         Format.fprintf pp " :- %a" pp_atom atom;
         List.iter (Format.fprintf pp ",@ %a" pp_atom) atoms);
    Format.pp_close_box pp ()

  let pp_decl pp {decl_name; decl_type; decl_rules} =
    Format.pp_open_vbox pp 0;
    Format.fprintf pp "%a : @[<h>%a@]@,"
      Names.pp_ident decl_name
      pp_domaintypes decl_type.predty_data;
    let rec pp_rules = function
      | []          -> ()
      | [rule]      -> pp_rule pp rule
      | rule::rules -> pp_rule pp rule; Format.pp_print_space pp (); pp_rules rules
    in
    pp_rules decl_rules;
    Format.pp_close_box pp ()

  let pp_pred_defs pp = function
    | [] -> ()
    | decl :: decls ->
       Format.pp_open_vbox pp 0;
       Format.fprintf pp "def %a" pp_decl decl;
       List.iter (Format.fprintf pp "@ @,and %a" pp_decl) decls;
       Format.pp_close_box pp ()

  let pp_constant_def fmt { const_name; const_type; const_expr } =
    Format.fprintf fmt
      "constant %a : %a = %a"
      Names.pp_ident const_name
      pp_domaintype  const_type
      pp_expr        const_expr

  let pp_term fmt = function
    | PredicateDefs defs -> pp_pred_defs fmt defs
    | ConstantDef c      -> pp_constant_def fmt c
end

module Syntax = Make_syntax (Modules.Syntax.String_names)
module Mod    = Modules.Syntax.Mod_Syntax_Raw (Syntax)

include Modules.Syntax.String_names
include Syntax
include Mod
