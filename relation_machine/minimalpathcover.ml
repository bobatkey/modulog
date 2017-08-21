(** Compute minimal path coverings of directed simple graphs. *)

module type G = sig
  type t

  module V : Graph.Sig.HASHABLE

  val iter_vertex : (V.t -> unit) -> t -> unit
  val iter_succ : (V.t -> unit) -> t -> V.t -> unit
  val iter_pred : (V.t -> unit) -> t -> V.t -> unit
end

module Make (G : G) : sig
  val minimal_path_cover : G.t -> G.V.t list list
end = struct

  module Bipartite = struct
    type t = G.t

    module V = struct
      type t =
        | Src
        | Snk
        | Left of G.V.t
        | Right of G.V.t
      let equal x y = match x, y with
        | Src, Src -> true
        | Snk, Snk -> true
        | Left s,  Left t  -> G.V.equal s t
        | Right s, Right t -> G.V.equal s t
        | _ -> false
      let hash = function
        | Src     -> 0
        | Snk     -> 1
        | Left s  -> 2*G.V.hash s+2
        | Right s -> 2*G.V.hash s+3
    end

    module E = struct
      type t = V.t * V.t
      type label = unit
      let label _ = ()
      let src = fst
      let dst = snd
    end

    let iter_succ_e f g = function
      | V.Src ->
         G.iter_vertex (fun a -> f (V.Src, V.Left a)) g
      | V.Snk ->
         ()
      | V.Left a as v ->
         G.iter_succ (fun b -> f (v, V.Right b)) g a
      | V.Right _ as v ->
         f (v, V.Snk)

    let iter_pred_e f g = function
      | V.Src ->
         ()
      | V.Snk ->
         G.iter_vertex (fun b -> f (V.Right b, V.Snk)) g
      | V.Left _ as v ->
         f (V.Src, v)
      | V.Right b as v ->
         G.iter_pred (fun a -> f (V.Left a, v)) g b

  end

  module Flow = struct
    type t = int
    type label = unit
    let max_capacity _ = 1
    let min_capacity _ = 0
    let flow () = 0
    let add = (+)
    let sub = (-)
    let zero = 0
    let compare = Pervasives.compare
  end

  module F = Graph.Flow.Ford_Fulkerson (Bipartite) (Flow)

  module PathStore : sig
    type t
    val empty : G.t -> t
    val add_edge : t -> G.V.t * G.V.t -> unit
    val paths : t -> G.V.t list list
  end = struct
    module Table = Hashtbl.Make (G.V)

    type t = (G.V.t list * G.V.t) Table.t

    let concat (p1, _) (p2, c) =
      (p1 @ p2, c)

    let empty g =
      let ends  = Table.create 12 in
      g |> G.iter_vertex begin fun v ->
        Table.add ends v ([v], v)
      end;
      ends

    let head_of_path (_, v) = v
    let tail_of_path (vs, _) = List.hd vs

    let add_edge table (v1, v2) =
      let p1 = Table.find table v1 in
      let p2 = Table.find table v2 in
      Table.remove table v1;
      Table.remove table v2;
      let p = concat p1 p2 in
      Table.replace table (head_of_path p) p;
      Table.replace table (tail_of_path p) p

    let paths t =
      Table.fold
        (fun v (p,_) l -> if G.V.equal (List.hd p) v then p::l else l)
        t
        []
  end

  let minimal_path_cover g =
    let flow_f, _ = F.maxflow g Bipartite.V.Src Bipartite.V.Snk in
    let paths = PathStore.empty g in
    (* would be nice if there were a way to iterate only over edges
       with non-zero flow. *)
    g |> G.iter_vertex begin fun v ->
      G.iter_succ
        (fun v' ->
           if flow_f (Bipartite.V.Left v, Bipartite.V.Right v') = 1 then
             PathStore.add_edge paths (v,v'))
        g v
    end;
    PathStore.paths paths

end
