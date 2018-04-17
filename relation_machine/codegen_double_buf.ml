module type CONFIG = sig
  val arity : int
end

module type S = sig
  module Syn : Idealised_algol.Syntax.S

  type handle

  val declare : (handle -> Syn.comm) -> Syn.comm

  val insert : handle -> int32 Syn.exp array -> Syn.comm

  val iterate_all : handle -> (int32 Syn.exp array -> Syn.comm) -> Syn.comm

  val swap : handle -> Syn.comm

  val is_empty : handle -> bool Syn.exp
end

module Make (Syn : Idealised_algol.Syntax.S) (C : CONFIG) ()
  : S with module Syn = Syn
=
struct

  module Syn = Syn
  
  module Key = Codegen_inttuple.Make (Syn) (C) ()
  module BT  =
    Idealised_algol.Btree.Make (Syn) (struct let min_children = 16l end) 
      (Key)
      ()

  type handle =
    { write : BT.handle
    ; read  : BT.handle
    }

  (* declare_buffer (path_buf/2) {
         path_buf/2 += path;

         swap path_buf;

         while_not_empty (path_buf) {
            path_buf <-
                [ (x,z) | (x,y) <- path_buf, (y,z) <- edge, (x,z) \nin path ]

            swap path_buf;

            path <- path_buf;
     }
  *)
  
  let declare k =
    BT.declare @@ fun read ->
    BT.declare @@ fun write ->
    k {read; write}

  let insert h vals =
    let key = Key.create vals in
    BT.ifmember key h.write
      Syn.empty
      (BT.insert key h.write)

  let iterate_all h k =
    BT.iterate_all h.read (fun key -> k (Array.init C.arity (Key.get key)))

  let swap h =
    BT.move ~src:h.write ~tgt:h.read

  let is_empty h =
    BT.is_empty h.read
  
end
