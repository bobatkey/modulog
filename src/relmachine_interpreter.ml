exception Variable_not_found of string

module MakeEnv (T : sig type t end) : sig
  type t
  type value = T.t
  val empty : t
  val find : string -> t -> value
  val add : string -> value -> t -> t
  val iter : (string -> value -> unit) -> t -> unit
end = struct
  module M = Map.Make (String)
  type t = T.t M.t
  type value = T.t
  let empty = M.empty
  let find k env =
    try M.find k env with Not_found -> raise (Variable_not_found k)
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

module RelvarEnv = MakeEnv (Relation)

module AttrEnv   = MakeEnv (struct type t = int32 end)

open Relmachine_syntax

let eval_scalar attr_env = function
  | Attr attr -> AttrEnv.find attr attr_env
  | Lit v     -> v

let check_condition attr_env values (i, scalar) =
  values.(i) = eval_scalar attr_env scalar

let rec eval_expr rel_env attr_env f = function
  | Return { guard_relation = Some rel; values } ->
     let values = Array.of_list (List.map (eval_scalar attr_env) values) in
     let guard_relation = RelvarEnv.find rel rel_env in
     if not (Relation.mem guard_relation values) then
       f values
  | Return { guard_relation = None; values } ->
     let values = Array.of_list (List.map (eval_scalar attr_env) values) in
     f values
  | Select { relation; conditions; projections; body } ->
     let relation = RelvarEnv.find relation rel_env in
     relation |> Relation.iter begin fun values ->
       if List.for_all (check_condition attr_env values) conditions then begin
         let attr_env =
           List.fold_left
             (fun attr_env (i, attr) -> AttrEnv.add attr values.(i) attr_env)
             attr_env
             projections
         in
         eval_expr rel_env attr_env f body
       end
     end

let rec eval_comm rel_env = function
  | WhileNotEmpty (rels, body) ->
     let rels = List.map (fun rel -> RelvarEnv.find rel rel_env) rels in
     let rec loop () =
       if not (List.for_all Relation.is_empty rels) then
         (eval_comms rel_env body; loop ())
     in
     loop ()

  | Insert (rel, expr) ->
     let rel = RelvarEnv.find rel rel_env in
     eval_expr rel_env AttrEnv.empty (Relation.add rel) expr

  | Merge { tgt; src } ->
     let src = RelvarEnv.find src rel_env
     and tgt = RelvarEnv.find tgt rel_env
     in
     Relation.iter (Relation.add tgt) src

  | Move { tgt; src } ->
     let src = RelvarEnv.find src rel_env
     and tgt = RelvarEnv.find tgt rel_env
     in
     Relation.clear tgt;
     Relation.iter (Relation.add tgt) src;
     Relation.clear src

  | Declare (inits, comms) ->
     let initialise rel_env = function
       | rel, None ->
          RelvarEnv.add rel (Relation.create 128) rel_env
       | rel, Some init ->
          let init = RelvarEnv.find init rel_env in
          RelvarEnv.add rel (Relation.copy init) rel_env
     in
     let rel_env = List.fold_left initialise rel_env inits in
     eval_comms rel_env comms

and eval_comms rel_env comms =
  List.iter (eval_comm rel_env) comms


let eval {edb_relvars; idb_relvars; commands} =
  let rel_env =
    List.fold_left
      (fun rel_env (nm, _) -> RelvarEnv.add nm (Relation.create 128) rel_env)
      RelvarEnv.empty
      idb_relvars
  in
  eval_comms rel_env commands;
  rel_env

let pp_rel =
  Fmt.(braces
         (iter ~sep:(always ",@ ") Relation.iter
            (brackets (array ~sep:(always ",") int32))))

let pp_relvarenv =
  Fmt.(iter_bindings ~sep:(always ",@ ") RelvarEnv.iter
         (pair ~sep:(always " = ") string pp_rel))
            
