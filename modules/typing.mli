open Syntax

module type TYPING_ENV = sig
  type val_type
  type def_type
  type kind

  type t

  (* FIXME: this is needed to handle mutually recursive values. Could
     there be another way? *)
  val add_value : string -> val_type -> t -> Ident.t * t

  val find_value :
    String_names.longident ->
    t -> (Path.t * val_type, Typing_environment.lookup_error) result

  val find_type :
    String_names.longident ->
    t -> (Path.t * kind, Typing_environment.lookup_error) result

  (* FIXME: this is used to get definitions of types during type
     equality checking. *)
  val lookup_type : Path.t -> t -> kind * def_type option
end

module type CORE_TYPING = sig
  module Src  : CORE_SYNTAX_CONCRETE
  module Core : CORE_SYNTAX

  type error

  val pp_error : Format.formatter -> error -> unit

  module Checker (Env : TYPING_ENV
                  with type val_type = Core.val_type
                   and type def_type = Core.def_type
                   and type kind     = Core.kind) :
  sig
    val type_term : Env.t -> Src.term -> (Core.term * (Ident.t * Core.val_type) list, error) result

    val check_deftype : Env.t -> Core.kind -> Src.def_type -> (Core.def_type, error) result

    val check_valtype : Env.t -> Src.val_type -> (Core.val_type, error) result

    val check_kind : Env.t -> Src.kind -> (Core.kind, error) result

    val valtype_match : Env.t -> Core.val_type -> Core.val_type -> bool

    val deftype_equiv : Env.t -> Core.kind -> Core.def_type -> Core.def_type -> bool

    val kind_match : Env.t -> Core.kind -> Core.kind -> bool

    val deftype_of_path : Path.t -> Core.kind -> Core.def_type
  end

end

module type MOD_TYPING = sig
  module Src : MOD_SYNTAX_CONCRETE

  module Tgt : MOD_SYNTAX

  module Env : Typing_environment.S with module Mod = Tgt

  type error

  val pp_error : Format.formatter -> error -> unit

  val type_modterm :
    Env.t -> Src.mod_term -> (Tgt.mod_term * Tgt.mod_type, error) result

  val type_structure :
    Env.t -> Src.structure -> (Tgt.structure * Tgt.signature, error) result
end

module Mod_typing
    (Src : MOD_SYNTAX_CONCRETE)
    (Tgt : MOD_SYNTAX with type Core.Location.t = Src.Core.Location.t)
    (CT  : CORE_TYPING
     with module Src  = Src.Core
      and module Core = Tgt.Core)
  : MOD_TYPING
    with module Src = Src
     and module Tgt = Tgt
