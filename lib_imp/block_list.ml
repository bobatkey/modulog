module type S = sig
  module S : Syntax.S

  type handle

  val declare : name:string -> arity:int -> (handle -> S.comm) -> S.comm

  (** the two handles ought to have the same arity *)
  val move : src:handle -> tgt:handle -> S.comm

  (** the length of the list and arity must match *)
  val insert : handle -> int S.exp array -> S.comm

  val is_empty : handle -> bool S.exp

  val iterate : handle -> (int S.exp array -> S.comm) -> S.comm
end

module Make (S : Syntax.S) () : S with module S = S = struct
  module S = S

  open! S

  let block_size = 16l

  type list_node
  let list_node : list_node structure typ = structure "list_node"
  let occupied = field list_node "occupied" int
  let next     = field list_node "next" (ptr list_node)
  let values   = field list_node "values" (array int 1l)
  let ()       = seal list_node

  type handle =
    { arity : int
    ; var   : list_node structure ptr var
    }

  let arity handle =
    handle.arity

  let is_empty {var} =
    var =*= null

  let move ~src:{var=src;arity=a1} ~tgt:{var=tgt;arity=a2} =
    if a1 <> a2 then
      invalid_arg "Block_list.Make.move: mismatched arities";
    begin%monoid
      tgt := src;
      src := null;
    end

  let write val_array offset vals =
    vals
    |> Array.mapi begin fun i v ->
      let i = Int32.of_int i in
      val_array#@(offset + const i) := v
    end
    |> Array.fold_left (^^) empty

  let read val_array offset arity =
    Array.init arity
      (fun i -> val_array#@(offset + const (Int32.of_int i)))

  let new_block v ~arity ~vals ~next:nxt =
    begin%monoid
      let length = Int32.mul (Int32.of_int arity) block_size in
      malloc_ext v list_node (const length) int;
      v#->occupied := const 1l;
      v#->next := nxt;
      write (v#->values) (const 0l) vals
    end

  let insert {var=head; arity} vals =
    if Array.length vals <> arity then
      invalid_arg "Block_list.Make.insert: arity mismatch";
    if_ (head =*= null)
      ~then_:begin%monoid
        new_block head ~arity ~vals ~next:null
      end
      ~else_:begin
        if_ (head#->occupied == const block_size)
          ~then_:begin%monoid
            declare (ptr list_node) @@ fun new_head ->
            begin%monoid
              new_block new_head ~arity ~vals ~next:head;
              head := new_head
            end
          end
          ~else_:begin%monoid
            write (head#->values)
              (head#->occupied * const (Int32.of_int arity)) vals;
            head#->occupied := head#->occupied + const 1l
          end
      end

  let iterate {var=head; arity} body =
    declare (ptr list_node) @@ fun node ->
    declare int @@ fun i ->
    begin%monoid
      node := head;

      while_ (node =!*= null) ~do_:begin%monoid
        i := const 0l;
        while_ (i < (node#->occupied * const (Int32.of_int arity))) ~do_:begin%monoid
          body (read (node#->values) i arity);
          i := i + const (Int32.of_int arity)
        end;

        node := node#->next
      end
    end

  let declare ~name ~arity k =
    declare ~name (ptr list_node) @@ fun var ->
    begin%monoid
      var := null;
      k { arity; var };
      declare ~name:"ahead" (ptr list_node) @@ fun ahead ->
      while_ (var =!*= null) ~do_:begin%monoid
        ahead := var#->next;
        free var;
        var := ahead
      end
    end
end
