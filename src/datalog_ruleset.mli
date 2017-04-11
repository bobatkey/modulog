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

val pp_rule : Format.formatter -> rule -> unit

type ruleset

val of_rules : rule list -> ruleset

type rule_id

val rule_id : rule_id -> int

val rule : rule_id -> ruleset -> rule

val rule_is_self_recursive : ruleset -> rule_id -> bool

val scc_list : ruleset -> rule_id list list

module G : sig
  type t = ruleset

  module V : Graph.Sig.COMPARABLE with type t = ruleset * rule_id

  module E : sig
    type t
    val src : t -> V.t
    val dst : t -> V.t
  end

  val iter_vertex : (V.t -> unit) -> t -> unit
  val iter_succ : (V.t -> unit) -> t -> V.t -> unit
  val iter_edges_e : (E.t -> unit) -> t -> unit
end
