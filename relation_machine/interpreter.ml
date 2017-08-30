module MakeEnv (K : Map.OrderedType) (T : sig type t end) : sig
  type t
  type value = T.t
  exception No_binding of K.t
  val empty : t
  val find : K.t -> t -> value
  val add : K.t -> value -> t -> t
  val iter : (K.t -> value -> unit) -> t -> unit
end = struct
  module M = Map.Make (K)
  type t = T.t M.t
  type value = T.t
  exception No_binding of K.t
  let empty = M.empty
  let find k env =
    try M.find k env with Not_found -> raise (No_binding k)
  let add k v env = M.add k v env
  let iter = M.iter
end

module Relation : sig
  type t
  val create : int -> t
  val add : t -> int32 array -> unit
  val mem : t -> int32 array -> bool
  val iter : (int32 array -> unit) -> t -> unit
  val is_empty : t -> bool
  val clear : t -> unit
  val copy : t -> t
end = struct
  module H = Hashtbl.Make
      (struct
        type t = int32 array
        let equal = (=)
        let hash = Hashtbl.hash
      end)
  type t = unit H.t
  let create n = H.create n
  let add rel tuple = H.add rel tuple ()
  let mem rel tuple = H.mem rel tuple
  let iter f rel = H.iter (fun tuple () -> f tuple) rel
  let is_empty r = H.length r = 0
  let clear = H.clear
  let copy = H.copy
end

let pp_rel =
  Fmt.(braces
         (iter ~sep:(always ",@ ") Relation.iter
            (brackets (array ~sep:(always ",") int32))))

module Env = struct
  include MakeEnv (struct type t = Syntax.relvar let compare = compare end) (Relation)

  let pp =
    Fmt.iter_bindings ~sep:(Fmt.always ",@ ") iter
      (Fmt.pair ~sep:(Fmt.always " = ") Syntax.pp_relvar pp_rel)  
end

module AttrEnv = MakeEnv (String) (struct type t = int32 end)

open Syntax

let eval_scalar attr_env = function
  | Attr attr -> AttrEnv.find attr attr_env
  | Lit v     -> v

let check_condition attr_env values (i, scalar) =
  values.(i) = eval_scalar attr_env scalar

let rec eval_expr rel_env attr_env f = function
  | Return { guard_relation = Some rel; values } ->
     let values = Array.of_list (List.map (eval_scalar attr_env) values) in
     let guard_relation = Env.find rel rel_env in
     if not (Relation.mem guard_relation values) then
       f values
  | Return { guard_relation = None; values } ->
     let values = Array.of_list (List.map (eval_scalar attr_env) values) in
     f values
  | Select { relation; conditions; projections; cont } ->
     let relation = Env.find relation rel_env in
     relation |> Relation.iter begin fun values ->
       if List.for_all (check_condition attr_env values) conditions then begin
         let attr_env =
           List.fold_left
             (fun attr_env (i, attr) -> AttrEnv.add attr values.(i) attr_env)
             attr_env
             projections
         in
         eval_expr rel_env attr_env f cont
       end
     end

let rec eval_comm rel_env = function
  | WhileNotEmpty (rels, body) ->
     let rels = List.map (fun rel -> Env.find rel rel_env) rels in
     let rec loop () =
       if not (List.for_all Relation.is_empty rels) then
         (eval_comms rel_env body; loop ())
     in
     loop ()

  | Insert (rel, expr) ->
     let rel = Env.find rel rel_env in
     eval_expr rel_env AttrEnv.empty (Relation.add rel) expr

  | Move { tgt; src } ->
     let src = Env.find src rel_env
     and tgt = Env.find tgt rel_env
     in
     Relation.clear tgt;
     Relation.iter (Relation.add tgt) src;
     Relation.clear src

  | Declare (inits, comms) ->
     let initialise rel_env rel =
       Env.add rel (Relation.create 128) rel_env
     in
     let rel_env = List.fold_left initialise rel_env inits in
     eval_comms rel_env comms

and eval_comms rel_env comms =
  List.iter (eval_comm rel_env) comms


let eval {edb_relvars; idb_relvars; commands} =
  let rel_env =
    List.fold_left
      (fun rel_env nm -> Env.add nm (Relation.create 128) rel_env)
      Env.empty
      idb_relvars
  in
  eval_comms rel_env commands;
  rel_env
