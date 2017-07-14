module Make (S : Syntax.S) () : sig
  type handle

  val is_empty : handle -> bool S.exp
  val insert : handle -> int S.exp -> S.comm
  val dispose : handle -> S.comm
  val iterate : handle -> (int S.exp -> S.comm) -> S.comm
end = struct
  open S

  let block_size = 16l

  type list_node
  let list_node : list_node structure typ = structure "list_node"
  let occupied = field list_node "occupied" int
  let values   = field list_node "values" (array int block_size)
  let next     = field list_node "next" (ptr list_node)
  let ()       = seal list_node

  type handle = list_node S.structure S.ptr S.var

  let is_empty head =
    head =*= null

  let insert head value =
    if_ (head =*= null)
      ~then_:begin%monoid
        malloc head list_node;

        head#->occupied := const 1l;
        head#->values#@(const 0l) := value;
        head#->next := null
      end
      ~else_:begin
        if_ (head#->occupied == const block_size)
          ~then_:begin%monoid
            declare (ptr list_node) @@ fun new_head ->
            begin%monoid
              malloc new_head list_node;
              new_head#->occupied := const 1l;
              new_head#->values#@(const 0l) := value;
              new_head#->next := head;

              head := new_head
            end
          end
          ~else_:begin%monoid
            head#->values#@(head#->occupied) := value;
            head#->occupied := head#->occupied + const 1l
          end
      end

  let dispose head =
    declare (ptr list_node) @@ fun ahead ->
    while_ (head =!*= null) ~do_:begin%monoid
      ahead := head#->next;
      free head;
      head := ahead
    end

  let iterate head body =
    declare (ptr list_node) @@ fun node ->
    declare int @@ fun i ->
    begin%monoid
      node := head;

      while_ (node =!*= null) ~do_:begin%monoid
        i := const 0l;
        while_ (i < node#->occupied) ~do_:begin%monoid
          body (node#->values#@i);
          i := i + const 1l
        end;

        node := node#->next
      end
    end
end
