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

  let declare ~name k =
    let rec loop i l =
      if i = Array.length A.indexes then
        k (Array.of_list (List.rev l))
      else
        BT.declare (fun v -> loop (i+1) (v::l))
    in
    loop 0 []

  let conv_pattern pat =
    pat
    |> Array.fold_lefti (fun l i -> function `Fixed _ -> i::l | `Wild -> l) []
    |> List.rev
    |> Array.of_list

  let is_prefix ar1 ar2 =
    if Array.length ar1 > Array.length ar2 then false
    else
      let rec loop i =
        if i = Array.length ar1 then true
        else if ar1.(i) <> ar2.(i) then false
        else loop (i+1)
      in
      loop 0

  let find_handle prefix handles =
    let rec loop i =
      if i = Array.length handles then
        failwith "Invalid search pattern"
      else if is_prefix prefix A.indexes.(i) then
        handles.(i), A.indexes.(i)
      else
        loop (i+1)
    in
    loop 0

  let invert perm =
    let inv = Array.make (Array.length perm) 0 in
    Array.iteri (fun i j -> inv.(j) <- i) perm;
    inv

  let get_fixed = function
    | `Fixed e -> e
    | `Wild    -> assert false

  let for_pattern h pat k =
    let prefix_pat   = conv_pattern pat in
    let handle, perm = find_handle prefix_pat h in
    let perm_inv     = invert perm in
    let minimum =
      Key.create
        (Array.init A.arity
           (fun i ->
              if i < Array.length prefix_pat then
                get_fixed (pat.(prefix_pat.(i)))
              else
                S.Int32.const 0l))
    in
    let maximum =
      Key.create
        (Array.init A.arity
           (fun i ->
              if i < Array.length prefix_pat then
                get_fixed (pat.(prefix_pat.(i)))
              else
                S.Int32.maximum))
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
