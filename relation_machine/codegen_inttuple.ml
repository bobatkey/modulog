module type S = sig
  include Idealised_algol.Btree.KEY

  val create : int32 Syn.exp array -> t Syn.exp

  val get : t Syn.exp -> int -> int32 Syn.exp
end

module Make (Syn : Idealised_algol.Syntax.S) (A : sig val arity : int end) ()
  : S with module Syn = Syn =
struct
  module Syn = Syn

  type key
  type t = key Syn.Struct.t
  let t : t Syn.typ = Syn.Struct.make "key"
  let val_fields =
    Array.init A.arity
      (fun i -> Syn.Struct.field t (Printf.sprintf "x%d" i) Syn.Int32.t)
  let () = Syn.Struct.seal t

  let create exps =
    Syn.Struct.const t (Array.map (fun e -> Syn.Struct.Exp e) exps)

  let get x i =
    let open! Syn.Struct in
    x#.val_fields.(i)

  let eq =
    Syn.declare_func
      ~name:"eq"
      ~typ:Syn.(("x", t) @-> ("y", t) @-> return Syn.Bool.t)
      ~body:begin fun x y ->
        let rec loop i acc =
          if i = A.arity then
            acc
          else
            loop (i+1)
              (let open! Syn.Bool in let open! Syn.Int32 in
               let open! Syn.Struct in
               acc && x#.val_fields.(i) == y#.val_fields.(i))
        in
        loop 1
          (let open! Syn in let open! Syn.Bool in let open! Syn.Int32 in
           let open! Syn.Struct in
           x#.val_fields.(0) == y#.val_fields.(0))
      end

  let le =
    Syn.declare_func
      ~name:"le"
      ~typ:Syn.(("x", t) @-> ("y", t) @-> return Syn.Bool.t)
      ~body:begin fun x y ->
        let rec loop i =
          if i = A.arity - 1 then
            let open! Syn in
            let open! Syn.Int32 in
            let open! Syn.Struct in
            x#.val_fields.(i) <= y#.val_fields.(i)
          else
            let e = loop (i+1) in
            let open! Syn in
            let open! Bool in
            let open! Int32 in
            let open! Syn.Struct in
            x#.val_fields.(i) < y#.val_fields.(i)
            || (x#.val_fields.(i) == y#.val_fields.(i) && e)
        in
        loop 0
      end

  let lt =
    Syn.declare_func
      ~name:"lt"
      ~typ:Syn.(("x", t) @-> ("y", t) @-> return Syn.Bool.t)
      ~body:begin fun x y ->
        let rec loop i =
          if i = A.arity - 1 then
            let open! Syn in
            let open! Syn.Int32 in
            let open! Syn.Struct in
            x#.val_fields.(i) < y#.val_fields.(i)
          else
            let e = loop (i+1) in
            let open! Syn in
            let open! Syn.Bool in
            let open! Syn.Int32 in
            let open! Syn.Struct in
            x#.val_fields.(i) < y#.val_fields.(i)
            || (x#.val_fields.(i) == y#.val_fields.(i) && e)
        in
        loop 0
      end
end
