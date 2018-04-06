module type EVAL_ENV = sig
  type eval_value

  type eval_type

  type t

  type value = private [> `Value of eval_value | `Type of eval_type ]

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
                and type eval_type = eval_type) :
  sig

    val eval_type : Env.t -> Core.kind -> Core.def_type -> eval_type

    val eval_decl : Env.t -> string list -> Ident.t -> Core.val_type -> eval_value eval

    val eval_term : Env.t -> string list -> Core.term -> (Ident.t * eval_value) list eval

  end
end

module Make
    (Mod       : Syntax.MOD_SYNTAX)
    (Core_eval : CORE_EVAL with module Core = Mod.Core) =
struct

  open Mod

  module Env = struct
    type eval_value = Core_eval.eval_value
    type eval_type = Core_eval.eval_type

    type t = value Ident.Table.t

    and value =
      [ `Value     of eval_value
      | `Type      of eval_type
      | `Structure of (string * value) list
      | `Functor   of t * Ident.t * Mod.mod_term
      ]

    let empty =
      Ident.Table.empty

    let add = Ident.Table.add

    let add_values =
      List.fold_right
        (fun (id, value) -> add id (`Value value))

    let rec find lident t =
      match lident with
        | Path.Pident ident ->
           Ident.Table.find ident t
        | Path.Pdot (root, field) ->
           match find root t with
             | None ->
                None
             | Some (`Structure bindings) ->
                (try Some (List.assoc field bindings)
                 with Not_found -> None)
             | Some _ ->
                failwith "identifier not found"
  end

  let return = Core_eval.return
  and (>>=) = Core_eval.(>>=)

  module Eval = Core_eval.Eval (Env)

  let rec declare_modtype env path = function
    | {modtype_data=Modtype_signature sigitems} ->
       declare_signature env path [] sigitems >>= fun bindings ->
       return (`Structure bindings)

    | _ ->
       failwith "internal error: expecting a signature type"

  and declare_signature env path rev_bindings = function
    | [] ->
       return (List.rev rev_bindings)

    | {sigitem_data=Sig_value (ident, val_type)} :: items ->
       Eval.eval_decl env path ident val_type >>= fun value ->
       declare_signature env path ((Ident.name ident, `Value value) :: rev_bindings) items

    | {sigitem_data=Sig_type _} :: items ->
       (* FIXME: probably shouldn't ignore this -- the typecheck ought
          to reject uses of abstract types in signatures of recursive
          modules. *)
       declare_signature env path rev_bindings items

    | {sigitem_data=Sig_module (ident, modty)} :: items ->
       let sub_path = Ident.name ident :: path in
       declare_modtype env sub_path modty >>= fun value ->
       let env = Env.add ident value env in
       let rev_bindings = (Ident.name ident, value) :: rev_bindings in
       declare_signature env path rev_bindings items

    | {sigitem_data=Sig_modty _} :: items ->
       declare_signature env path rev_bindings items

  let rec declare_recbindings env path new_env rev_bindings = function
    | [] ->
       return (new_env, rev_bindings)

    | (ident, modty, _) :: rec_bindings ->
       let sub_path = Ident.name ident :: path in
       declare_modtype env sub_path modty >>= fun value ->
       let rev_bindings = (Ident.name ident, value) :: rev_bindings in
       let new_env = Env.add ident value new_env in
       declare_recbindings env path new_env rev_bindings rec_bindings

  let rec eval_structure env path rev_bindings = function
    | [] ->
       return (List.rev rev_bindings)

    | {stritem_data=Str_value term} :: items ->
       Eval.eval_term env path term >>= fun bindings ->
       let env = Env.add_values bindings env in
       let rev_bindings =
         List.fold_right
           (fun (ident, value) ->
              List.cons (Ident.name ident, `Value value))
           bindings
           rev_bindings
       in
       eval_structure env path rev_bindings items

    | {stritem_data=Str_type (ident, kind, typ)} :: items ->
       let typ = Eval.eval_type env kind typ in
       let value = `Type typ in
       let env = Env.add ident value env in
       let rev_bindings = (Ident.name ident, value) :: rev_bindings in
       eval_structure env path rev_bindings items

    | {stritem_data=Str_module (ident, modl)} :: items ->
       let sub_path = Ident.name ident :: path in
       eval_modterm env sub_path modl >>= fun value ->
       let env = Env.add ident value env in
       let rev_bindings = (Ident.name ident, value) :: rev_bindings in
       eval_structure env path rev_bindings items

    | {stritem_data=Str_modty _} :: items ->
       eval_structure env path rev_bindings items

    | {stritem_data=Str_modrec bindings} :: items ->
       declare_recbindings env path env rev_bindings bindings
       >>= fun (env, rev_bindings) ->
       eval_recbindings env path bindings
       >>= fun () ->
       eval_structure env path rev_bindings items

  and eval_recbindings env path = function
    | [] ->
       return ()
    | (ident, modty, modl) :: rec_bindings ->
       let sub_path = Ident.name ident :: path in
       eval_modterm env sub_path modl >>= fun _ ->
       eval_recbindings env path rec_bindings

  and eval_modterm env path = function
    | {modterm_data=Mod_longident lid} ->
       (match Env.find lid env with
         | Some value ->
            return value
         | None ->
            failwith
              "internal error: module identifier not found during evaluation")

    | {modterm_data=Mod_structure items} ->
       eval_structure env path [] items >>= fun bindings ->
       return (`Structure bindings)

    | {modterm_data=Mod_functor (arg_name, _, body)} ->
       return (`Functor (env, arg_name, body))

    | {modterm_data=Mod_apply (funct, arg)} ->
       (eval_modterm env path funct >>= function
         | `Functor (clo_env, arg_name, body) ->
            eval_modterm env path arg >>= fun value ->
            let env = Env.add arg_name value clo_env in
            eval_modterm env path body
         | _ ->
            failwith "internal error: not a functor in application")

    | {modterm_data=Mod_constraint (modl, _)} ->
       eval_modterm env path modl

  let eval_structure str =
    eval_structure Env.empty [] [] str >>= fun _ -> return ()

end
