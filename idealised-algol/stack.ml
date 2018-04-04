module Make (S : Syntax.S) : sig
  open S

  type ('a, 'b) stack_ops =
    { push     : 'c 'd. ('a,[>`exp] as 'c) expr -> ('b, [>`exp] as 'd) expr -> comm
    ; pop      : comm
    ; top      : 'a var * 'b var
    ; is_empty : bool exp
    }

  val with_stack :
    int32 -> 'a typ -> 'b typ -> (('a,'b) stack_ops -> comm) -> comm
end = struct
  open! S
  open S.RawArray

  let incr i =
    i := i + const 1l

  let decr i =
    i := i - const 1l

  type ('a, 'b) stack_ops =
    { push     : 'c 'd. ('a,[>`exp] as 'c) expr -> ('b, [>`exp] as 'd) expr -> comm
    ; pop      : comm
    ; top      : 'a var * 'b var
    ; is_empty : bool exp
    }

  let with_stack max_depth typ1 typ2 body =
    declare ~name:"stack1_" (array typ1 max_depth) @@ fun stack1 ->
    declare ~name:"stack2_" (array typ2 max_depth) @@ fun stack2 ->
    declare ~name:"stackptr" int32 ~init:(const 0l) @@ fun stackptr ->
    let push x1 x2 =
      begin%monoid
        stack1#@stackptr := x1;
        stack2#@stackptr := x2;
        incr stackptr
      end
    and pop =
      decr stackptr
    and top =
      (stack1#@(stackptr - const 1l),
       stack2#@(stackptr - const 1l))
    and is_empty =
      stackptr == const 0l
    in
    body {push; pop; top; is_empty}
end
