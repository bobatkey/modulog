type predicate_name = string

type expr =
  | Var of string
  | Lit of int32
  | Underscore

type atom =
  | Atom of { pred : predicate_name; args : expr list }

type rule =
  { pred : predicate_name
  ; args : expr list
  ; rhs  : atom list
  }

type ruleset

val of_rules : rule list -> ruleset


type rule_id

val rule : rule_id -> ruleset -> rule

val rule_is_self_recursive : ruleset -> rule_id -> bool

val scc_list : ruleset -> rule_id list list

