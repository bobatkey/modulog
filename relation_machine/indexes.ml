open Syntax

(* Indexes:
     - work out which indexes are needed:
       - for each select, we know the inputs and the outputs required.
       - therefore, we know what sub-indexes to maintain for each relation

     Optimisations:
     - if a selection has no projections, then it can just be a membership
       test, which avoids the necessity to loop.
  *)

module PatternSet = struct
  (* This could be more efficiently implemented with using BDDs, but
     it is probably not worth it for the sizes of sets we will be
     dealing with. *)

  module Pattern = struct
    include Set.Make (struct type t = int let compare = compare end)

    let complete n =
      let rec loop pat i = if i = n then pat else loop (add i pat) (i+1) in
      loop empty 0

    let pp =
      Fmt.braces (Fmt.iter ~sep:(Fmt.always ",@ ") iter Fmt.int)
  end

  type pattern = Pattern.t

  include Set.Make (Pattern)

  let pp = Fmt.braces (Fmt.iter ~sep:(Fmt.always ",@ ") iter Pattern.pp)

  module V = struct
    type t = Pattern.t
    let equal = Pattern.equal
    let hash s = Hashtbl.hash (Pattern.elements s)
  end

  let is_strict_subset s1 s2 =
    Pattern.subset s1 s2 && not (Pattern.equal s1 s2)

  let iter_vertex =
    iter

  let iter_succ f g s =
    iter (fun s' -> if is_strict_subset s' s then f s') g

  let iter_pred f g s =
    iter (fun s' -> if is_strict_subset s s' then f s') g
end

module MPC = Minimalpathcover.Make (PatternSet)

module PredicatePats : sig
  type t

  val empty : t

  val pats : relvar -> t -> PatternSet.t

  val add : relvar -> PatternSet.pattern -> t -> t

  val fold : (relvar -> PatternSet.t -> 'a -> 'a) -> t -> 'a -> 'a

  val map_to_list : (relvar -> PatternSet.t -> 'a) -> t -> 'a list

  val pp : Format.formatter -> t -> unit
end = struct
  module VarMap = Map.Make (struct type t = relvar let compare = compare end)

  type t = PatternSet.t VarMap.t

  let empty = VarMap.empty

  let pats pred t =
    match VarMap.find pred t with
      | exception Not_found -> PatternSet.empty
      | set -> set

  let add pred pat t =
    if PatternSet.Pattern.is_empty pat then t
    else
      let set = pats pred t in
      VarMap.add pred (PatternSet.add pat set) t

  let fold = VarMap.fold
  let map_to_list f t = VarMap.fold (fun p pats -> List.cons (f p pats)) t []

  let pp =
    Fmt.iter_bindings VarMap.iter
      (Fmt.pair ~sep:(Fmt.always " => ")
         Syntax.pp_relvar
         PatternSet.pp)
end

let rec search_patterns_of_expr pats = function
  | Select { relation; conditions; projections; cont } ->
     let pat  = PatternSet.Pattern.of_list (List.map fst conditions) in
     let pats = PredicatePats.add relation pat pats in
     search_patterns_of_expr pats cont
  | Return { guard_relation = None } ->
     pats
  | Return { guard_relation = Some relation; values } ->
     let pat = PatternSet.Pattern.complete (List.length values) in
     PredicatePats.add relation pat pats

let rec search_patterns_of_command pats = function
  | WhileNotEmpty (_, comms) | Declare (_, comms) ->
     search_patterns_of_commands pats comms
  | Insert (_, expr) ->
     search_patterns_of_expr pats expr
  | Move _ ->
     pats

and search_patterns_of_commands pats commands =
  List.fold_left search_patterns_of_command pats commands

let search_patterns {commands} =
  search_patterns_of_commands PredicatePats.empty commands

let ordering_of_pattern_path arity pats =
  let included = Array.make arity false in
  let ordering = Array.make arity (-1) in
  let pos      = ref 0 in
  pats |> List.iter begin fun pat ->
    let elems = PatternSet.Pattern.elements pat in
    elems |> List.iter begin fun idx ->
      if not included.(idx) then
        (ordering.(!pos) <- idx; included.(idx) <- true; incr pos)
    end
  end;
  included |> Array.iteri begin fun i visited ->
    if not visited then (ordering.(!pos) <- i; incr pos)
  end;
  assert (!pos = arity);
  ordering

let orderings_of_patterns program pred pats =
  let arity = pred.arity in
  let pattern_paths =
    MPC.minimal_path_cover pats
    |> List.map List.rev
    |> List.map (ordering_of_pattern_path arity)
  in
  (pred, pattern_paths)

let indexes program : (relvar * int array list) list =
  program
  |> search_patterns
  |> PredicatePats.map_to_list (orderings_of_patterns program)

(* Now:
   - generate a variable ordering for each index
   - and a back mapping to the search patterns, in some form
*)

let pp_indexes =
  Fmt.(brackets
         (list ~sep:(always ";@ ")
            (brackets
               (list ~sep:(always ";@ ")
                  PatternSet.Pattern.pp))))

let pp_all_indexes =
  Fmt.(braces
         (list ~sep:(always ";@ ")
            (pair ~sep:(always " =>@ ") Syntax.pp_relvar pp_indexes)))

let pp_orderings =
  Fmt.(brackets
         (list ~sep:(always ";@ ")
            (brackets
              (array ~sep:(always ";@ ") int))))

let pp_all_orderings =
  Fmt.(braces
         (list ~sep:(always ";@ ")
            (pair ~sep:(always " =>@ ") Syntax.pp_relvar pp_orderings)))

(* Now to work out the indexes required for each relation:
   - run "search_patterns program"
   - for each predicate, get the minimal path cover - each path is an index.
   - Two types of 'relation'
     - sequential only: typically new_* and delta_*
       - these are only used to (a) add things to; and (b) iterate over. Just use a linked list of blocks.
     - lookup oriented ones
       - will have 1 or more indexes
       - insertion updates the indexes (all data is stored in the indexes anyway)
*)
