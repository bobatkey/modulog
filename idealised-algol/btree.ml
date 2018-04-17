(* FIXME: add 'next' pointers to avoid the use of a stack for
   iteration? I think this requires putting all keys at the leaves as
   well as in the internal nodes. As it stands, this implementation is
   good for membership queries, but requires an auxillary stack for
   iteration.

   An intermediate solution would be to maintain parent pointers so
   the stack needn't store the parent pointers. *)


module type PARAMETERS = sig
  val min_children : int32
end

module type KEY = sig
  module Syn : Syntax.S

  type t

  val t : t Syn.typ

  val lt : t Syn.exp -> t Syn.exp -> bool Syn.exp
  val le : t Syn.exp -> t Syn.exp -> bool Syn.exp
  val eq : t Syn.exp -> t Syn.exp -> bool Syn.exp
end

module type S = sig
  module Syn : Syntax.S

  type key

  type handle

  val declare : (handle -> Syn.comm) -> Syn.comm

  val insert : key Syn.exp -> handle -> Syn.comm

  val ifmember : key Syn.exp -> handle -> Syn.comm -> Syn.comm -> Syn.comm

  val ifmember_range :
    key Syn.exp ->
    key Syn.exp ->
    handle ->
    Syn.comm ->
    Syn.comm ->
    Syn.comm

  val iterate_range :
    key Syn.exp ->
    key Syn.exp ->
    handle ->
    (key Syn.exp -> Syn.comm) ->
    Syn.comm

  val iterate_all :
    handle ->
    (key Syn.exp -> Syn.comm) ->
    Syn.comm

  val move : src:handle -> tgt:handle -> Syn.comm

  val is_empty : handle -> bool Syn.exp
end

module Make
    (Syn : Syntax.S)
    (P   : PARAMETERS)
    (K   : KEY with module Syn = Syn)
    ()
  : S with module Syn = Syn
       and type   key = K.t =
struct
  module Syn = Syn

  let min_keys = Int32.sub P.min_children 1l
  let max_keys = Int32.(sub (mul P.min_children 2l) 1l)
  let child_slots = Int32.mul P.min_children 2l

  open! Syn
  open Syn.RawPtr
  open Syn.RawArray

  type node
  let node : node Struct.t typ = Struct.make "node"
  let leaf     = Struct.field node "leaf" Syn.Bool.t
  let nkeys    = Struct.field node "nkeys" Syn.Int32.t
  let keys     = Struct.field node "keys" (array K.t max_keys)
  let children = Struct.field node "children" (array (ptr node) child_slots)
  let ()       = Struct.seal node

  type handle = node Struct.t ptr var

  type key = K.t

  let incr (i : _ var) =
    let open! Syn.Int32 in
    i := i + const 1l

  let decr (i : _ var) =
    let open! Syn.Int32 in
    i := i - const 1l

  let int32 = Syn.Int32.const

  let find_key =
    declare_func
      ~name:"find_key"
      ~typ:(("i", Syn.Int32.t) @&-> ("x",ptr node) @-> ("key",K.t) @-> return_void)
      ~body:begin fun i x key ->
        let open! Syn.Bool in
        let open! Syn.Int32 in
        begin%monoid
          i := int32 0l;
          while_ (i < x#->nkeys && K.lt x#->keys#@i key)
            ~do_:(incr i)
        end
      end

  let with_nodeptr ~name init body =
    Syn.declare ~name (ptr node) ~init body

  let alloc_node body =
    declare ~name:"node" (ptr node) @@ fun x -> begin%monoid
      malloc x node;
      body x
    end

  let with_int body =
    declare ~name:"i" Syn.Int32.t ~init:(int32 0l) body

  let loop body =
    while_ Bool.true_ ~do_:body


  let declare body =
    alloc_node @@ fun x -> begin%monoid
      x#->leaf := Bool.true_;
      x#->nkeys := int32 0l;
      body x
      (* FIXME: free the tree afterwards? *)
    end

  (************************************************************)
  let ifmember key t yes no =
    with_nodeptr ~name:"cursor" t @@ fun x ->
    with_int @@ fun i ->
    loop begin%monoid
      let open! Syn.Bool in
      let open! Syn.Int32 in
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
      let open! Syn.Bool in
      let open! Syn.Int32 in
      find_key i (to_exp x) from;
      ifthen (i < x#->nkeys && K.le x#->keys#@i upto)
        begin%monoid yes; break end;
      ifthen x#->leaf ~then_:begin%monoid no; break end;
      x := x#->children#@i
    end

  (************************************************************)
  module Stk = Stack.Make (Syn)

  (* FIXME: compute this from the min_children and a reasonable
     estimate of the maximum size of any tree. *)
  let max_stack_depth = 40l

  let iterate_range from upto (tree : handle) body =
    with_nodeptr ~name:"cursor" tree @@ fun x ->
    Stk.with_stack max_stack_depth (ptr node) Syn.Int32.t @@
    fun Stk.{push;pop;top;is_empty} ->
    with_int @@ fun i ->
    begin%monoid
      let open! Syn.Int32 in
      (* recurse down the tree *)
      loop begin%monoid
        find_key i (to_exp x) from;
        ifthen x#->leaf ~then_:break;
        ifthen (i < x#->nkeys) ~then_:(push x i);
        x := x#->children#@i
      end;
      loop begin%monoid
        let open! Bool in
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

        if_ (i == x#->nkeys - int32 1l)
          ~then_:pop
          ~else_:(incr (snd top));

        x := x#->children#@(i + int32 1l);

        while_ (not x#->leaf)
          ~do_:begin%monoid
            push x (int32 0l);
            x := x#-> children#@(int32 0l)
          end;

        i := int32 0l;
      end
    end

  (************************************************************)
  let iterate_all tree body =
    with_nodeptr ~name:"cursor" tree @@ fun x ->
    Stk.with_stack max_stack_depth (ptr node) Syn.Int32.t @@
    fun Stk.{push;pop;top;is_empty} -> begin%monoid
      while_ (Bool.not x#->leaf)
        ~do_:begin%monoid
          push x (int32 0l);
          x := x#->children#@(int32 0l)
        end;
      loop begin%monoid
        Syn.declare ~name:"i" Syn.Int32.t ~init:(int32 0l) @@ fun i -> begin%monoid
          let open! Syn.Int32 in
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

          if_ (i == x#->nkeys - int32 1l)
            ~then_:pop
            ~else_:(incr (snd top));

          x := x#->children#@(i + const 1l);

          while_ (Syn.Bool.not x#->leaf)
            ~do_:begin%monoid
              push x (int32 0l);
              x := x#->children#@(int32 0l)
            end;
        end
      end
    end

  (************************************************************)
  (* Insertion *)

  let move_keys_up x i =
    let open! Syn.Int32 in
    Syn.declare ~name:"j" Syn.Int32.t ~init:(x#->nkeys - const 1l) @@ fun j ->
    while_ (j >= i) ~do_:begin%monoid
      x#->keys#@(j + const 1l) := x#->keys#@j;
      decr j
    end

  let copy ~n ~src ~dst =
    let open! Syn.Int32 in
    Syn.declare ~name:"j" Syn.Int32.t ~init:(int32 0l) @@ fun j ->
    while_ (j < n) ~do_:begin%monoid
      dst j := src j;
      incr j
    end

  let split_child =
    declare_func
      ~name:"split_child"
      ~typ:(("x", ptr node) @-> ("i", Syn.Int32.t) @-> return_void)
      ~body:begin fun x i ->
        with_nodeptr ~name:"child" x#->children#@i @@ fun y ->
        alloc_node @@ fun z ->
        begin%monoid
          let open Syn.Int32 in
          z#->leaf := y#->leaf;
          z#->nkeys := const min_keys;

          (* copy the keys over *)
          copy
            ~n:(const min_keys)
            ~src:(fun j -> y#->keys#@(j + const P.min_children))
            ~dst:(fun j -> z#->keys#@j);

          (* copy the children over (if not a leaf node) *)
          ifthen (Bool.not y#->leaf) begin
            copy ~n:(const P.min_children)
              ~src:(fun j -> y#->children#@(j + const P.min_children))
              ~dst:(fun j -> z#->children#@j)
          end;

          (* truncate y *)
          y#->nkeys := const min_keys;

          (* shunt x's children up *)
          begin
            Syn.declare ~name:"i" Syn.Int32.t ~init:(x#->nkeys) @@ fun j ->
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
    let open! Syn.Int32 in
    x#->nkeys == const max_keys

  let insert_nonfull =
    declare_func ~name:"insert_nonfull"
      ~typ:(("x", ptr node) @-> ("key", K.t) @-> return_void)
      ~body:begin fun x' key ->
        with_nodeptr ~name:"insert_cursor" x' @@ fun x ->
        with_int @@ fun i ->
        begin%monoid
          while_ (Bool.not x#->leaf) begin%monoid
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
          (* if the root is full, then split it by making a new root
             node with a single child, and using split_child *)
          ifthen (node_is_full root) begin
            alloc_node @@ fun s ->
            begin%monoid
              s#->leaf := Bool.false_;
              s#->nkeys := int32 0l;
              s#->children#@(int32 0l) := root;
              split_child (to_exp s) (int32 0l);
              root := s
            end
          end;
          (* Once the root is not full, insert the key into it. *)
          insert_nonfull (to_exp root) key
        end
      end

  let free tree =
    with_nodeptr ~name:"cursor" tree @@ fun cursor ->
    Stk.with_stack max_stack_depth (ptr node) Syn.Int32.t @@ fun stk ->
    begin%monoid
      while_ (Bool.not cursor#->leaf)
        ~do_:begin%monoid
          stk.push cursor (int32 0l);
          cursor := cursor#->children#@(int32 0l)
        end;

      loop begin%monoid
        free cursor;

        ifthen stk.is_empty ~then_:break;

        cursor := fst stk.top;

        let open! Syn.Int32 in

        if_ (snd stk.top == cursor#->nkeys - int32 1l)
          ~then_:begin%monoid
            stk.pop;
            with_nodeptr ~name:"old_cursor" cursor @@ fun old_cursor ->
            begin%monoid
              cursor := cursor#->children#@(cursor#->nkeys);
              free old_cursor
            end
          end
          ~else_:begin%monoid
            incr (snd stk.top);
            cursor := cursor#->children#@(snd stk.top)
          end;

        while_ (Syn.Bool.not cursor#->leaf)
          ~do_:begin%monoid
            stk.push cursor (int32 0l);
            cursor := cursor#->children#@(int32 0l)
          end
      end
    end

  let move ~src ~tgt =
    begin%monoid
      free tgt;
      tgt := src;
      malloc src node;
      src#->leaf := Bool.true_;
      src#->nkeys := int32 0l;
    end

  let is_empty tree =
    let open! Syn.Bool in
    let open! Syn.Int32 in
    tree#->leaf && tree#->nkeys == const 0l
end
