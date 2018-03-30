module type PARAMETERS = sig
  val min_children : int32
end

module type KEY = sig
  module S : Syntax.S

  type t

  val t : t S.typ

  val lt : t S.exp -> t S.exp -> bool S.exp
  val le : t S.exp -> t S.exp -> bool S.exp
  val eq : t S.exp -> t S.exp -> bool S.exp
end

module type S = sig
  module S : Syntax.S

  type key

  type handle

  val declare : (handle -> S.comm) -> S.comm

  val insert : key S.exp -> handle -> S.comm

  val ifmember : key S.exp -> handle -> S.comm -> S.comm -> S.comm

  val ifmember_range :
    key S.exp ->
    key S.exp ->
    handle ->
    S.comm ->
    S.comm ->
    S.comm

  val iterate_range :
    key S.exp ->
    key S.exp ->
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
  let nkeys    = field node "nkeys" int32
  let keys     = field node "keys" (array K.t max_keys)
  let children = field node "children" (array (ptr node) child_slots)
  let ()       = seal node

  type handle = node structure ptr var

  type key = K.t

  let incr (i : _ var) =
    i := i + const 1l

  let decr (i : _ var) =
    i := i - const 1l

  let find_key =
    declare_func
      ~name:"find_key"
      ~typ:(("i", int32) @&-> ("x",ptr node) @-> ("key",K.t) @-> return_void)
      ~body:begin fun i x key ->
        begin%monoid
          i := const 0l;
          while_ (i < x#->nkeys && K.lt x#->keys#@i key)
            ~do_:(incr i)
        end
      end

  let with_nodeptr ~name init body =
    declare_init ~name (ptr node) init body

  let alloc_node body =
    declare ~name:"node" (ptr node) @@ fun x -> begin%monoid
      malloc x node;
      body x
    end

  let with_int body =
    declare ~name:"i" int32 body

  let loop body =
    while_ true_ ~do_:body


  let declare body =
    alloc_node @@ fun x -> begin%monoid
      x#->leaf := true_;
      x#->nkeys := const 0l;
      body x
      (* FIXME: free the tree afterwards? *)
    end

  (************************************************************)
  let ifmember key t yes no =
    with_nodeptr ~name:"cursor" t @@ fun x ->
    with_int @@ fun i ->
    loop begin%monoid
      find_key i (to_exp x) key;
      ifthen (i < x#->nkeys && K.eq x#->keys#@i key)
        ~then_:begin%monoid yes; break end;
      ifthen x#->leaf
        ~then_:begin%monoid no; break end;
      x := x#->children#@i
    end

  (************************************************************)
  let ifmember_range from upto t yes no =
    with_nodeptr ~name:"cursor" t @@ fun x ->
    with_int @@ fun i ->
    loop begin%monoid
      find_key i (to_exp x) from;
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
    with_nodeptr ~name:"cursor" tree @@ fun x ->
    Stk.with_stack max_stack_depth (ptr node) int32 @@
    fun Stk.{push;pop;top;is_empty} ->
    with_int @@ fun i ->
    begin%monoid
      (* recurse down the tree *)
      loop begin%monoid
        find_key i (to_exp x) from;
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
    with_nodeptr ~name:"cursor" tree @@ fun x ->
    Stk.with_stack max_stack_depth (ptr node) int32 @@
    fun Stk.{push;pop;top;is_empty} -> begin%monoid
      while_ (not x#->leaf)
        ~do_:begin%monoid
          push x (const 0l);
          x := x#->children#@(const 0l)
        end;
      loop begin%monoid
        declare_init ~name:"i" int32 (const 0l) @@ fun i -> begin%monoid
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
        end
      end
    end

  (************************************************************)
  (* Insertion *)
  let move_keys_up x i =
    declare_init ~name:"j" int32 (x#->nkeys - const 1l) @@ fun j ->
    while_ (j >= i) ~do_:begin%monoid
      x#->keys#@(j + const 1l) := x#->keys#@j;
      decr j
    end

  let copy ~n ~src ~dst =
    declare_init ~name:"j" int32 (const 0l) @@ fun j ->
    while_ (j < n) ~do_:begin%monoid
      dst j := src j;
      incr j
    end

  let split_child =
    declare_func
      ~name:"split_child"
      ~typ:(("x", ptr node) @-> ("i", int32) @-> return_void)
      ~body:begin fun x i ->
        with_nodeptr ~name:"child" x#->children#@i @@ fun y ->
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
            declare_init ~name:"i" int32 (x#->nkeys) @@ fun j ->
            begin%monoid
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
      end

  let node_is_full x =
    x#->nkeys == const max_keys

  let insert_nonfull =
    declare_func ~name:"insert_nonfull"
      ~typ:(("x", ptr node) @-> ("key", K.t) @-> return_void)
      ~body:begin fun x' key ->
        with_nodeptr ~name:"insert_cursor" x' @@ fun x ->
        with_int @@ fun i ->
        begin%monoid
          while_ (not x#->leaf) begin%monoid
            find_key i (to_exp x) key;
            ifthen (node_is_full x#->children#@i)
              ~then_:begin%monoid
                split_child (to_exp x) (to_exp i);
                ifthen (K.lt x#->keys#@i key)
                  ~then_:(incr i)
              end;
            x:= x#->children#@i
          end;

          find_key i (to_exp x) key;
          move_keys_up x i;
          x#->keys#@i := key;
          incr (x #-> nkeys)
        end
      end

  let insert =
    declare_func
      ~name:"insert"
      ~typ:(("key", K.t) @-> ("root", ptr node) @&-> return_void)
      ~body:begin fun key root ->
        begin%monoid
          (* if the root is full, then split it by making a new root node
                   with a single child, and using split_child *)
          ifthen (node_is_full root) begin
            alloc_node @@ fun s ->
            begin%monoid
              s #-> leaf := false_;
              s #-> nkeys := const 0l;
              s#->children#@(const 0l) := root;
              split_child (to_exp s) (const 0l);
              root := s
            end
          end;
          (* Once the root is not full, insert the key into it. *)
          insert_nonfull (to_exp root) key
        end
      end
end
