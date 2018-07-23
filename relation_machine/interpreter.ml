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
  (* val clear : t -> unit *)
  (* val copy : t -> t *)
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
  (* let clear = H.clear *)
  (* let copy = H.copy *)
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
  | Return { values } ->
     let values = Array.map (eval_scalar attr_env) values in
     f values
  | Guard_NotIn { relation; values; cont } ->
     let values = Array.map (eval_scalar attr_env) values in
     let guard_relation = Env.find relation rel_env in
     if not (Relation.mem guard_relation values) then
       eval_expr rel_env attr_env f cont
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
  | ReadRelation (_relvar, _filename) ->
     failwith "interpreter: FIXME: implement LoadRelation"

  | WriteRelation (_relvar, _filename) ->
     failwith "interpreter: FIXME: implement WriteRelation"

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

  | Swap _rel ->
     failwith "FIXME: impement buffers in the interpreter"
(*
  | Move { tgt; src } ->
     let src = Env.find src rel_env
     and tgt = Env.find tgt rel_env
     in
     Relation.clear tgt;
     Relation.iter (Relation.add tgt) src;
     Relation.clear src
*)
  | DeclareBuffers (inits, comms) ->
     let initialise rel_env rel =
       Env.add rel (Relation.create 128) rel_env
     in
     let rel_env = List.fold_left initialise rel_env inits in
     eval_comms rel_env comms

and eval_comms rel_env comms =
  List.iter (eval_comm rel_env) comms

(*
let read_line arity line =
  let attrs = Array.make arity 0l in
  let rec read field_idx acc i =
    if field_idx >= arity then
      failwith "Too many fields";
    if i = String.length line then
      (attrs.(field_idx) <- acc;
       attrs)
    else match line.[i] with
      | ',' ->
         attrs.(field_idx) <- acc;
         read (field_idx+1) 0l (i+1)
      | '0' .. '9' as c ->
         let d = Int32.of_int (Char.code c - Char.code '0') in
         read field_idx Int32.(add (mul acc 10l) d) (i+1)
      | _ ->
         failwith "Unrecognised character"
  in
  read 0 0l 0

let load_csv_file filename arity =
  let ch  = open_in filename in
  let rel = Relation.create 128 in
  let rec loop () =
    match input_line ch with
      | exception End_of_file -> ()
      | line ->
         let tuple = read_line arity line in
         Relation.add rel tuple;
         loop ()
  in
  try loop (); close_in ch; rel
  with e -> close_in ch; raise e
*)

let eval {relvars; commands} =
  let rel_env = Env.empty in
(*
  let rel_env =
    List.fold_left
      (fun rel_env nm ->
         let rel = load_csv_file (nm.ident ^ ".csv") nm.arity in
         Env.add nm rel rel_env)
      rel_env
      edb_relvars
  in
*)
  let rel_env =
    List.fold_left
      (fun rel_env nm -> Env.add nm (Relation.create 128) rel_env)
      rel_env
      relvars
  in
  eval_comms rel_env commands;
  rel_env
