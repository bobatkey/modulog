(*
module SetOfSets = struct
  module Bdd =
    Tdk.Bdd.Make
      (struct
        type t = int
        let hash (i : int) = Hashtbl.hash i
        let compare (x : int) (y : int) = Pervasives.compare x y
        let to_string i = string_of_int i
      end)

  type t =
    { width   : int
    ; the_set : Bdd.t
    }

  let empty ~width =
    { width; the_set = Bdd.const false }


(*
  let complete n l =
    let rec fill k j l =
      if k = j then (j, true) :: loop (j+1) l
      else (k, false) :: fill (k+1) j l
    and loop i = function
      | [] ->
         fill 
         []
      | j :: l ->
         
         fill i
    in
    loop 0 l
*)

  let singleton l =
    let open Bdd in
    List.fold_right
      (fun (i,b) -> Bdd.prod (if b then var i else neg (var i)))
      l
      (Bdd.const true)

  let add l t =
    Bdd.sum t (singleton l)

  let members {width;the_set=t} =
    let rec fill_in l v k vs vss =
      if l >= v then k vs vss
      else
        fill_in (l+1) v k (l::vs) (fill_in (l+1) v k vs vss)
    in
    Bdd.fold
      (fun b l ->
         fill_in l width (fun vs vss -> if b then List.rev vs :: vss else vss))
      (fun (v,_) tt ff l ->
         fill_in l v (fun vs vss -> tt (v+1) (v::vs) (ff (v+1) vs vss)))
      t
      0
      []
      []

  (* return the bdd of all subsets of the given node *)
  let successors l t =
    let open Bdd in
    let prod_var (i,_) = Bdd.prod (Bdd.neg (Bdd.var i)) in
    let sum_var (i,_) = Bdd.sum (Bdd.var i) in
    let t = Bdd.(prod t
                   (prod
                      (List.fold_right prod_var (List.filter (fun (i,b) -> not b) l) (Bdd.const true))
                      (List.fold_right sum_var  (List.filter (fun (i,b) -> b) l) (Bdd.const false))))
    in
    (* ensure strict subsets *)
    let t = Bdd.(prod (neg (singleton l)) t) in
    members t

  (*  let predecessors l t =*)
  (* the bdd of all super sets:
     - 
     -  *)
  
end

let s =
  SetOfSets.(empty ~width:4
             |> add [0,true ;1,true ;2,false;3,true]
             |> add [0,true ;1,true ;2,true ;3,false]
             |> add [0,true ;1,true ;2,false;3,false]
             |> add [0,true ;1,false;2,true ;3,false]
             |> add [0,true ;1,false;2,false;3,false]
             |> add [0,false;1,true; 2,false;3,false])
*)


module S = Set.Make (struct type t = int let compare = compare end)
module SS = Set.Make (S)

module G = struct
  type t = SS.t

  module V = struct
    type t = S.t
    let equal = S.equal
    let hash s = Hashtbl.hash (S.elements s)
  end

  let is_strict_subset s1 s2 =
    S.subset s1 s2 && not (S.equal s1 s2)

  let iter_vertex = SS.iter
  let iter_succ f g s =
    SS.iter (fun s' -> if is_strict_subset s' s then f s') g
  let iter_pred f g s =
    SS.iter (fun s' -> if is_strict_subset s s' then f s') g
end

let g =
  SS.of_list
    [ S.of_list [0;1;2]
    ; S.of_list [0;1;3]
    ; S.of_list [0;1]
    ; S.of_list [0;2]
    ; S.of_list [0]
    ; S.of_list [1]
    ; S.of_list [0;1;2;3]
    ]

module MPC = Minimalpathcover.Make (G)

let paths =
  MPC.minimal_path_cover g |> List.map (List.map S.elements)
