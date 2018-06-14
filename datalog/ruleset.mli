(** Datalog programs as sets of rules. *)

(** {1 Datalog programs} *)

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

type ruleset

type rule_id

val rule_id : rule_id -> int

val rule_of_id : rule_id -> ruleset -> rule

type predicate_info =
  { kind   : [`Intensional | `Extensional of string]
  ; output : string list
  }

val predicates : ruleset -> (predicate_name * predicate_info) list

val components : ruleset -> [> `Direct of rule | `Recursive of rule list ] list

(** {2 Pretty printing} *)

val pp_rule : rule Fmt.t

val pp : ruleset Fmt.t

(** {2 Construction of rulesets} *)

type builder

module Builder : sig
  type t = builder

  type error =
    | Undeclared_predicate of predicate_name
    | Arity_mismatch of
        { pred       : predicate_name
        ; used_arity : int
        }
    | Definition_of_extensional_predicate of predicate_name
    | Predicate_already_declared of predicate_name

  val empty : t

  val add_predicate : predicate_name -> predicate_info -> t -> (t, error) result

  val add_rule : rule -> t -> (t, error) result

  val add_output : predicate_name -> string -> t -> t

  val finish : t -> ruleset
end

(** {2 Graph representation} *)

(** Graph representation of a set of datalog rules. Each vertex is a
    rule. There exists an edge [r1 -> r2] if the head of [r2] is
    mentioned on th right-hand side of [r1]. *)
module As_graph : sig
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
