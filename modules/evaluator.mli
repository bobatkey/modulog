(** Evaluation of the module language. *)

module type EVAL_ENV = sig
  type eval_value

  type eval_type

  type value = private [> `Value of eval_value | `Type of eval_type ]

  type t

  val empty : t

  val add_values : (Ident.t * eval_value) list -> t -> t

  val find : Path.t -> t -> value option
end

module type CORE_EVAL = sig
  module Core : Syntax.CORE_SYNTAX

  type 'a eval

  val return : 'a -> 'a eval

  val (>>=)  : 'a eval -> ('a -> 'b eval) -> 'b eval

  type eval_value

  type eval_type

  module Eval (Env : EVAL_ENV
               with type eval_value = eval_value
                and type eval_type  = eval_type) :
  sig

    val eval_type : Env.t -> Core.kind -> Core.def_type -> eval_type

    val eval_decl : Env.t -> string list -> Ident.t -> Core.val_type -> eval_value eval

    val eval_term : Env.t -> string list -> Core.term -> (Ident.t * eval_value) list eval

  end
end

module Make
    (Mod       : Syntax.MOD_SYNTAX)
    (Core_eval : CORE_EVAL with module Core = Mod.Core) :
sig
  val eval_structure : Mod.structure -> unit Core_eval.eval
end
