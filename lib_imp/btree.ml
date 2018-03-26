module type PARAMETERS = sig
  val min_children : int32
end

module type KEY = sig
  module S : Syntax.S

  type t

  val t : t S.typ

  val lt : (t,[>`exp]) S.expr -> (t,[>`exp]) S.expr -> bool S.exp
  val le : (t,[>`exp]) S.expr -> (t,[>`exp]) S.expr -> bool S.exp
  val eq : (t,[>`exp]) S.expr -> (t,[>`exp]) S.expr -> bool S.exp
end

module type S = sig
  module S : Syntax.S

  type key

  type handle

  val with_tree : (handle -> S.comm) -> S.comm

  val insert    : (key,[>`exp]) S.expr -> handle -> S.comm

  val ifmember  : (key,[>`exp]) S.expr -> handle -> S.comm -> S.comm -> S.comm

  val ifmember_range :
    (key,[>`exp]) S.expr ->
    (key,[>`exp]) S.expr ->
    handle ->
    S.comm ->
    S.comm ->
    S.comm

  val iterate_range :
    (key,[>`exp]) S.expr ->
    (key,[>`exp]) S.expr ->
    handle ->
    (key S.exp -> S.comm) ->
    S.comm

  val iterate_all :
    handle ->
    (key S.exp -> S.comm) ->
    S.comm
end

module Make
    (S : Syntax.S)
    (P : PARAMETERS)
    (K : KEY with module S = S)
    ()
  : S with module S = S
       and type   key = K.t
= struct
  module S = S

  open! S

  let min_keys = Int32.sub P.min_children 1l
  let max_keys = Int32.(sub (mul P.min_children 2l) 1l)
  let child_slots = Int32.mul P.min_children 2l

  type node
  let node : node structure typ = structure "node"
  let leaf     = field node "leaf" bool
  let nkeys    = field node "nkeys" int
  let keys     = field node "keys" (array K.t max_keys)
  let children = field node "children" (array (ptr node) child_slots)
  let ()       = seal node

  type handle = node structure ptr var

  type key = K.t

  let incr (i : _ var) =
    i := i + const 1l

  let decr (i : _ var) =
    i := i - const 1l

  let find_key i x key =
    begin%monoid
      i := const 0l;
      while_ (i < x#->nkeys && K.lt x#->keys#@i key)
        (incr i)
    end

  let with_nodeptr init body =
    declare ~name:"node" (ptr node) @@ fun x ->
    begin%monoid
      x := init; body x
    end

  let alloc_node body =
    declare ~name:"node" (ptr node) @@ fun x ->
    begin%monoid
      malloc x node;
      body x
    end

  let with_int body =
    declare ~name:"i" int body

  let loop body =
    while_ true_ body


  let with_tree body =
    alloc_node @@ fun x ->
    begin%monoid
      x#->leaf := true_;
      x#->nkeys := const 0l;
      body x
      (* FIXME: free the tree afterwards? *)
    end

  (************************************************************)
  let ifmember key t yes no =
    with_nodeptr t @@ fun x ->
    with_int @@ fun i ->
    loop begin%monoid
      find_key i x key;
      ifthen (i < x#->nkeys && K.eq x#->keys#@i key)
        ~then_:begin%monoid yes; break end;
      ifthen x#->leaf
        ~then_:begin%monoid no; break end;
      x := x#->children#@i
    end

  (************************************************************)
  let ifmember_range from upto t yes no =
    with_nodeptr t @@ fun x ->
    with_int @@ fun i ->
    loop begin%monoid
      find_key i x from;
      ifthen (i < x#->nkeys && K.le x#->keys#@i upto)
        begin%monoid yes; break end;
      ifthen x#->leaf ~then_:begin%monoid no; break end;
      x := x#->children#@i
    end

  (************************************************************)
  module Stk = Stack.Make (S)

  (* FIXME: compute this from the min_children and a reasonable
     estimate of the maximum size of any tree. *)
  let max_stack_depth = 40l

  let iterate_range from upto (tree : handle) body =
    with_nodeptr tree @@ fun x ->
    Stk.with_stack max_stack_depth (ptr node) int @@
    fun Stk.{push;pop;top;is_empty} ->
    with_int @@ fun i ->
    begin%monoid
      (* recurse down the tree *)
      loop begin%monoid
        find_key i x from;
        ifthen x#->leaf ~then_:break;
        ifthen (i < x#->nkeys) ~then_:(push x i);
        x := x#->children#@i
      end;
      loop begin%monoid
        while_ (i < x#->nkeys && K.le x#->keys#@i upto)
          ~do_:begin%monoid
            body x#->keys#@i;
            incr i
          end;

        ifthen (i != x#->nkeys || is_empty)
          ~then_:break;

        x := fst top;
        i := snd top;

        ifthen (not (K.le x#->keys#@i upto))
          ~then_:break;

        body x#->keys#@i;

        if_ (i == x#->nkeys - const 1l)
          ~then_:pop
          ~else_:(incr (snd top));

        x := x#->children#@(i + const 1l);

        while_ (not x#->leaf)
          ~do_:begin%monoid
            push x (const 0l);
            x := x#-> children#@(const 0l)
          end;

        i := const 0l;
      end
    end

  (************************************************************)
  let iterate_all tree body =
    with_nodeptr tree @@ fun x ->
    Stk.with_stack max_stack_depth (ptr node) int @@
    fun Stk.{push;pop;top;is_empty} ->
    with_int @@ fun i ->
    begin%monoid
      while_ (not x#->leaf)
        ~do_:begin%monoid
          push x (const 0l);
          x := x#->children#@(const 0l)
        end;
      loop begin%monoid
        while_ (i < x#->nkeys)
          ~do_:begin%monoid
            body x#->keys#@i;
            incr i
          end;

        ifthen is_empty
          ~then_:break;

        x := fst top;
        i := snd top;

        body x#->keys#@i;

        if_ (i == x#->nkeys - const 1l)
          ~then_:pop
          ~else_:(incr (snd top));

        x := x#->children#@(i + const 1l);

        while_ (not x#->leaf)
          ~do_:begin%monoid
            push x (const 0l);
            x := x#->children#@(const 0l)
          end;

        i := const 0l;
      end
    end

  (************************************************************)
  (* Insertion *)
  let move_keys_up x i =
    with_int @@ fun j ->
    begin%monoid
      j := x#->nkeys - const 1l;
      while_ (j >= i) ~do_:begin%monoid
        x#->keys#@(j + const 1l) := x#->keys#@j;
        decr j
      end
    end

  let copy ~n ~src ~dst =
    with_int @@ fun j ->
    begin%monoid
      j := const 0l;
      while_ (j < n) begin%monoid
        dst j := src j;
        incr j
      end
    end

  let split_child x i =
    with_nodeptr x#->children#@i @@ fun y ->
    alloc_node @@ fun z ->
    begin%monoid
      z#->leaf := y#->leaf;
      z#->nkeys := const min_keys;

      (* copy the keys over *)
      copy
        ~n:(const min_keys)
        ~src:(fun j -> y#->keys#@(j + const P.min_children))
        ~dst:(fun j -> z#->keys#@j);

      (* copy the children over (if not a leaf node) *)
      ifthen (not y#->leaf) begin
        copy ~n:(const P.min_children)
          ~src:(fun j -> y#->children#@(j + const P.min_children))
          ~dst:(fun j -> z#->children#@j)
      end;

      (* truncate y *)
      y #-> nkeys := const min_keys;

      (* shunt x's children up *)
      begin
        with_int @@ fun j ->
        begin%monoid
          j := x#->nkeys;
          while_ (j > i) ~do_:begin%monoid
            x#->children#@(j + const 1l) := x#->children#@j;
            decr j
          end
        end
      end;

      move_keys_up x i;

      x#->children#@(i + const 1l) := z;
      x#->keys#@i := y#->keys#@(const min_keys);
      incr (x #-> nkeys)
    end

  let node_is_full x =
    x#->nkeys == const max_keys

  let insert_nonfull x key =
    with_int @@ fun i ->
    begin%monoid
      while_ (not x#->leaf) begin%monoid
        find_key i x key;
        ifthen (node_is_full x#->children#@i)
          ~then_:begin%monoid
            split_child x i;
            ifthen (K.lt x#->keys#@i key)
              ~then_:(incr i)
          end;
        x:= x#->children#@i
      end;

      find_key i x key;
      move_keys_up x i;
      x#->keys#@i := key;
      incr (x #-> nkeys)
    end

  let insert key root = begin%monoid
    (* if the root is full, then split it by making a new root node
       with a single child, and using split_child *)
    ifthen (node_is_full root) begin
      alloc_node @@ fun s ->
      begin%monoid
        s #-> leaf := false_;
        s #-> nkeys := const 0l;
        s#->children#@(const 0l) := root;
        split_child s (const 0l);
        root := s
      end
    end;
    (* Once the root is not full, insert the key into it. *)
    with_nodeptr root @@ fun x ->
    insert_nonfull x key
  end
end
