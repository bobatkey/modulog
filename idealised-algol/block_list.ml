module type S = sig
  module S : Syntax.S

  type handle

  val declare : name:string -> arity:int -> (handle -> S.comm) -> S.comm

  (** the two handles ought to have the same arity *)
  val move : src:handle -> tgt:handle -> S.comm

  (** the length of the list and arity must match *)
  val insert : handle -> int32 S.exp array -> S.comm

  val is_empty : handle -> bool S.exp

  val iterate : handle -> (int32 S.exp array -> S.comm) -> S.comm
end

module Make (S : Syntax.S) () : S with module S = S = struct
  module S = S

  let int32_of_int = Int32.of_int
  let int32_to_int = Int32.to_int
  let int32_mul    = Int32.mul

  open! S
  open S.RawPtr
  open S.RawArray

  let block_size = 16l

  type list_node
  let list_node : list_node Struct.t typ = Struct.make "list_node"
  let occupied = Struct.field list_node "occupied" S.Int32.t
  let next     = Struct.field list_node "next" (ptr list_node)
  let values   = Struct.field list_node "values" (array S.Int32.t 1l)
  let ()       = Struct.seal list_node

  type handle =
    { arity : int32
    ; name  : string
    ; var   : list_node Struct.t ptr var
    }

  let is_empty {var} =
    var =*= null

  let free_list p =
    while_ (p =!*= null) ~do_:begin%monoid
      declare ~name:"ahead" (ptr list_node) ~init:p#->next @@ fun ahead ->
      begin%monoid
        free p;
        p := ahead
      end
    end

  let move ~src:{var=src;arity=a1} ~tgt:{var=tgt;arity=a2} =
    if a1 <> a2 then
      invalid_arg "Block_list.Make.move: mismatched arities";
    begin%monoid
      declare ~name:"cursor" (ptr list_node) ~init:tgt free_list;
      tgt := src;
      src := null;
    end

  let write val_array offset vals =
    let open! S.Int32 in
    vals |>
    Array.mapi begin fun i v ->
      let i = int32_of_int i in
      val_array#@(offset + const i) := v
    end |>
    Array.fold_left (^^) empty

  let read val_array offset arity =
    let open! Int32 in
    Array.init (int32_to_int arity)
      (fun i -> val_array#@(offset + const (int32_of_int i)))

  let new_block v ~arity ~vals ~next:nxt =
    let length = int32_mul arity block_size in
    let open! S.Int32 in
    begin%monoid
      malloc_ext v list_node (const length) S.Int32.t;
      v#->occupied := const 1l;
      v#->next := nxt;
      write (v#->values) (const 0l) vals
    end

  let insert {var=head; arity} vals =
    if Array.length vals <> int32_to_int arity then
      invalid_arg "Block_list.Make.insert: arity mismatch";
    if_ (head =*= null)
      ~then_:begin%monoid
        new_block head ~arity ~vals ~next:null
      end
      ~else_:begin%monoid
        let open! S.Int32 in
        if_ (head#->occupied == const block_size)
          ~then_:begin%monoid
            declare (ptr list_node) @@ fun new_head -> begin%monoid
              new_block new_head ~arity ~vals ~next:head;
              head := new_head
            end
          end
          ~else_:begin%monoid
            write head#->values (head#->occupied * const arity) vals;
            head#->occupied := head#->occupied + const 1l
          end
      end

  let iterate {var=head; arity} body =
    declare ~name:"cursor" (ptr list_node) ~init:head @@ fun node ->
    begin%monoid
      while_ (node =!*= null)
        ~do_:begin%monoid
          let open! S.Int32 in
          declare ~name:"i" S.Int32.t ~init:(const 0l) @@ fun i ->
          begin%monoid
            while_ (i < (node#->occupied * const arity))
              ~do_:begin%monoid
                body (read node#->values i arity);
                i := i + const arity
              end;
            node := node#->next
          end
        end
    end

  let declare ~name ~arity k =
    let arity = int32_of_int arity in
    declare ~name (ptr list_node) ~init:null @@ fun var ->
    begin%monoid
      k { arity; name; var };
      free_list var
    end
end
