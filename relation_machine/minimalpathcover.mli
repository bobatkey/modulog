module type G = sig
  type t

  module V : Graph.Sig.HASHABLE

  val iter_vertex : (V.t -> unit) -> t -> unit
  val iter_succ : (V.t -> unit) -> t -> V.t -> unit
  val iter_pred : (V.t -> unit) -> t -> V.t -> unit
end

module Make (G : G) : sig
  val minimal_path_cover : G.t -> G.V.t list list
end
