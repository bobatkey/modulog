(** Datalog programs as sets of rules. *)

type predicate_name =
  { ident : string
  ; arity : int
  }

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

type rule_id

val rule_id : rule_id -> int

val rule : rule_id -> ruleset -> rule

val rule_is_self_recursive : ruleset -> rule_id -> bool

type predicate_info =
  { intensional : bool
  }

val predicates : ruleset -> (predicate_name * predicate_info) list

module Builder : sig
  type t

  type error =
    | Undeclared_predicate of predicate_name
    | Arity_mismatch of
        { pred       : predicate_name
        ; used_arity : int
        }
    | Definition_of_extensional_predicate of predicate_name
    | Predicate_already_declared of predicate_name

  val empty : t
  val add_rule : rule -> t -> (t, error) result
  val add_idb_predicate : predicate_name -> t -> (t, error) result
  val add_edb_predicate : predicate_name -> t -> (t, error) result

  val finish : t -> ruleset
end

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
