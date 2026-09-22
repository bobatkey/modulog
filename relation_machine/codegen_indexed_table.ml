module Array = struct
  include Array
  let fold_lefti f init arr =
    let rec loop ~idx ~accum =
      if idx = Array.length arr then
        accum
      else
        let accum = f accum idx arr.(idx) in
        loop ~idx:(idx+1) ~accum
    in
    loop ~idx:0 ~accum:init
end

module type S = sig
  module S : Idealised_algol.Syntax.S

  type handle

  val arity : int

  val declare : name:string -> (handle -> S.comm) -> S.comm

  val iterate :
    handle ->
    pat:[`Wild | `Fixed of int32 S.exp] array ->
    do_:(int32 S.exp array -> S.comm) ->
    S.comm

  val iterate_all :
    handle ->
    do_:(int32 S.exp array -> S.comm) ->
    S.comm

  val insert : handle -> int32 S.exp array -> S.comm

  val ifmember_pat :
    handle ->
    pat:[`Wild | `Fixed of int32 S.exp] array ->
    then_:S.comm ->
    else_:S.comm ->
    S.comm

  val ifmember :
    handle ->
    int32 S.exp array ->
    then_:S.comm ->
    else_:S.comm ->
    S.comm
end

module type CONFIG = sig
  val name : string
  val arity : int
  val indexes : int array array  
end

module Make (S : Idealised_algol.Syntax.S) (A : CONFIG) () : S with module S = S =
struct

  module S = S

  let _ =
    (* FIXME: raise invalid_arg? *)
    assert (A.arity > 0);
    assert (Array.length A.indexes > 0);
    assert (Array.for_all (fun a -> Array.length a = A.arity) A.indexes)

  let arity = A.arity

  module Key = Codegen_inttuple.Make (S) (A) ()

  module BT =
    Idealised_algol.Btree.Make (S)
      (struct let min_children = 16l end)
      (Key)
      ()

  type handle = BT.handle array

  let declare ~name:_ k =
    let rec loop i l =
      if i = Array.length A.indexes then
        k (Array.of_list (List.rev l))
      else
        BT.declare (fun v -> loop (i+1) (v::l))
    in
    loop 0 []

  module IntSet = Set.Make (Int)

  (** Convert a search pattern to a set of fields that are the keys for the
      search. *)
  let conv_pattern =
    Array.fold_lefti
      (fun l i -> function `Fixed _ -> IntSet.add i l | `Wild -> l)
      IntSet.empty

  (** Match a set of search key fields against as storage order to find if
      the key fields are a permutation of the prefix. *)
  let match_prefix search_fields index_order =
    let rec loop required i =
      if IntSet.is_empty required then
	true
      else if i = Array.length index_order then
	false (* FIXME: this oughtn't happen because all of the field names ought to
                 appear in each index_order *)
      else if IntSet.mem index_order.(i) required then
	loop (IntSet.remove index_order.(i) required) (i+1)
      else
	false
    in
    loop search_fields 0

  let find_handle prefix handles =
    let rec loop i =
      if i = Array.length handles then
        failwith (Printf.sprintf "Invalid search pattern: [%s] not in [%s] for %s"
          (String.concat "," (List.map string_of_int (IntSet.to_list prefix)))
          (String.concat ";" (List.map (fun index -> String.concat "," (List.map string_of_int (Array.to_list index))) (Array.to_list A.indexes)))
	  A.name)
      else if match_prefix prefix A.indexes.(i) then
	handles.(i), A.indexes.(i)
      else
	loop (i+1)
    in
    loop 0

  let invert perm =
    let inv = Array.make (Array.length perm) 0 in
    Array.iteri (fun i j -> inv.(j) <- i) perm;
    inv

  let for_pattern h pat k =
    let search_fields = conv_pattern pat in
    let handle, perm  = find_handle search_fields h in
    let perm_inv      = invert perm in
    let minimum =
      Key.create
        (Array.init A.arity
          (fun i ->
	    match pat.(perm.(i)) with
	    | `Fixed e -> e
	    | `Wild -> S.Int32.const 0l))
    in
    let maximum =
      Key.create
        (Array.init A.arity
          (fun i ->
	    match pat.(perm.(i)) with
	    | `Fixed e -> e
	    | `Wild -> S.Int32.maximum))
    in
    k handle minimum maximum perm_inv

  let iterate h ~pat ~do_:k =
    for_pattern h pat begin fun tree minimum maximum perm_inv ->
      BT.iterate_range
        minimum maximum tree
        begin fun key ->
          k (Array.init A.arity (fun i -> Key.get key perm_inv.(i)))
        end
    end

  let ifmember_pat h ~pat ~then_ ~else_ =
    for_pattern h pat begin fun tree minimum maximum _ ->
      BT.ifmember_range
        minimum maximum tree
        then_
        else_
    end

  let ifmember h key ~then_ ~else_ =
    let perm   = A.indexes.(0) in
    let handle = h.(0) in
    let key    = Key.create (Array.init A.arity (fun i -> key.(perm.(i)))) in
    BT.ifmember key handle then_ else_

  let iterate_all h ~do_:k =
    let perm_inv = invert A.indexes.(0) in
    let handle = h.(0) in
    BT.iterate_all handle (fun key -> k (Array.init A.arity (fun i -> Key.get key perm_inv.(i))))

  let insert h exps =
    h
    |> Array.mapi (fun i ->
        BT.insert (Key.create (Array.map (fun j -> exps.(j)) A.indexes.(i))))
    |> let open! S in Array.fold_left (^^) empty

end
