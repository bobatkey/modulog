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

val pp : Format.formatter -> ruleset -> unit

val of_rules : rule list -> ruleset

type rule_id

val rule_id : rule_id -> int

val rule : rule_id -> ruleset -> rule

val rule_is_self_recursive : ruleset -> rule_id -> bool

val components : ruleset -> [> `Direct of rule | `Recursive of rule list ] list

(** Graph representation of a set of datalog rules. Each vertex is a
    rule. There exists an edge [r1 -> r2] if the head of [r2] is
    mentioned on th right-hand side of [r1]. *)
module G : sig
  type t = ruleset

  module V : sig
    type t = rule_id
    val equal : t -> t -> bool
    val hash : t -> int
    val compare : t -> t -> int
  end

  module E : sig
    type t
    val src : t -> V.t
    val dst : t -> V.t
  end

  val iter_vertex : (V.t -> unit) -> t -> unit

  val iter_succ : (V.t -> unit) -> t -> V.t -> unit

  val iter_edges_e : (E.t -> unit) -> t -> unit
end
