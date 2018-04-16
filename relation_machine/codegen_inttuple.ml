module type S = sig
  include Idealised_algol.Btree.KEY

  val create : int32 S.exp array -> t S.exp

  val get : t S.exp -> int -> int32 S.exp
end

module Make (S : Idealised_algol.Syntax.S) (A : sig val arity : int end) ()
  : S with module S = S =
struct
  module S = S

  type key
  type t = key S.Struct.t
  let t : t S.typ = S.Struct.make "key"
  let val_fields =
    Array.init A.arity
      (fun i -> S.Struct.field t (Printf.sprintf "x%d" i) S.Int32.t)
  let () = S.Struct.seal t

  let create exps =
    S.Struct.const t (Array.to_list (Array.map (fun e -> S.Struct.Exp e) exps))

  let get x i =
    let open! S.Struct in
    x#.val_fields.(i)

  let eq =
    S.declare_func
      ~name:"eq"
      ~typ:S.(("x", t) @-> ("y", t) @-> return S.Bool.t)
      ~body:begin fun x y ->
        let rec loop i acc =
          if i = A.arity then
            acc
          else
            loop (i+1)
              (let open! S.Bool in let open! S.Int32 in
               let open! S.Struct in
               S.(acc && x#.val_fields.(i) == y#.val_fields.(i)))
        in
        loop 1
          (let open! S in let open! S.Bool in let open! S.Int32 in
           let open! S.Struct in
           x#.val_fields.(0) == y#.val_fields.(0))
      end

  let le =
    S.declare_func
      ~name:"le"
      ~typ:S.(("x", t) @-> ("y", t) @-> return S.Bool.t)
      ~body:begin fun x y ->
        let rec loop i =
          if i = A.arity - 1 then
            let open! S in
            let open! S.Int32 in
            let open! S.Struct in
            x#.val_fields.(i) <= y#.val_fields.(i)
          else
            let e = loop (i+1) in
            let open! S in
            let open! Bool in
            let open! Int32 in
            let open! S.Struct in
            x#.val_fields.(i) < y#.val_fields.(i)
            || (x#.val_fields.(i) == y#.val_fields.(i) && e)
        in
        loop 0
      end

  let lt =
    S.declare_func
      ~name:"lt"
      ~typ:S.(("x", t) @-> ("y", t) @-> return S.Bool.t)
      ~body:begin fun x y ->
        let rec loop i =
          if i = A.arity - 1 then
            let open! S in
            let open! S.Int32 in
            let open! S.Struct in
            x#.val_fields.(i) < y#.val_fields.(i)
          else
            let e = loop (i+1) in
            let open! S in
            let open! S.Bool in
            let open! S.Int32 in
            let open! S.Struct in
            x#.val_fields.(i) < y#.val_fields.(i)
            || (x#.val_fields.(i) == y#.val_fields.(i) && e)
        in
        loop 0
      end
end
