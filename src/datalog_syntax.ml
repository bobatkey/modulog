type ident =
  string

type longident =
  | Lid_ident of ident
  | Lid_dot   of longident * string


type expr =
  { expr_loc : Location.t
  ; expr_data : expr_data
  }

and expr_data =
  | Expr_var of string
  | Expr_literal of int32
  | Expr_underscore
  | Expr_tuple of expr list

type domaintype =
  { domtype_loc  : Location.t
  ; domtype_data : domaintype_data
  }

and domaintype_data =
  | Type_int
  | Type_typename of longident
  | Type_tuple    of domaintype list

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

type term = declaration list

(***********************************************************************)
(* Module language *)

type modtype =
  { modtype_loc  : Location.t
  ; modtype_data : modtype_data
  }

and modtype_data =
  | Modtype_longident of longident
  | Modtype_signature of signature
  | Modtype_functor   of ident * modtype * modtype

and signature = sig_item list

and sig_item =
  { sigitem_loc  : Location.t
  ; sigitem_data : sigitem_data
  }

and sigitem_data =
  | Sig_value  of ident * predicate_type
  | Sig_type   of ident * domaintype option
  | Sig_module of ident * modtype
  | Sig_modty  of ident * modtype


type modterm =
  { modterm_loc  : Location.t
  ; modterm_data : modterm_data
  }

and modterm_data =
  | Mod_longident  of longident
  | Mod_functor    of ident * modtype * modterm
  | Mod_apply      of modterm * modterm
  | Mod_structure  of structure
  | Mod_constraint of modterm * modtype

and structure = str_item list

and str_item =
  { stritem_loc  : Location.t
  ; stritem_data : stritem_data
  }

and stritem_data =
  | Str_value  of declaration list
  | Str_type   of ident * domaintype
  | Str_module of ident * modterm
  | Str_modty  of ident * modtype
