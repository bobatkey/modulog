
let test =
  let open Program in
  of_rules [ { head={atm_pred="a"; atm_args=[Var"X"]}
             ; rhs=[ {atm_pred="b"; atm_args=[Var"X"]}
                   ; {atm_pred="c"; atm_args=[Var"X"]}
                   ]
             }
           ; { head={atm_pred="b"; atm_args=[Lit 1] }
             ; rhs = [] }
           ; { head={atm_pred="b"; atm_args=[Var"X"] }
             ; rhs=[ {atm_pred="c"; atm_args=[Var"X"]}
                   ; {atm_pred="d"; atm_args=[Var"X"]}
                   ]
             }
           ; { head={atm_pred="c"; atm_args=[Lit 2]}
             ; rhs=[] }
           ; { head={atm_pred="c"; atm_args=[Var"X"]}
             ; rhs=[ {atm_pred="b"; atm_args=[Var"X"]}
                   ; {atm_pred="d"; atm_args=[Var"X"]}
                   ]
             }
           ; { head={atm_pred="d"; atm_args=[Lit 3]}
             ; rhs=[] }
           ]

let test2 =
  let open Program in
  { rules = [| { head={atm_pred="edge"; atm_args=[Lit 1; Lit 2]}; rhs=[] }
             ; { head={atm_pred="edge"; atm_args=[Lit 2; Lit 3]}; rhs=[] }
             ; { head={atm_pred="path"; atm_args=[Var"X"; Var"Y"]}
               ; rhs=[ {atm_pred="edge"; atm_args=[Var"X";Var"Y"]} ] }
             ; { head={atm_pred="path"; atm_args=[Var"X"; Var"Z"]}
               ; rhs=[ {atm_pred="path"; atm_args=[Var"X";Var"Y"]}
                     ; {atm_pred="edge"; atm_args=[Var"Y";Var"Z"]}
                     ] }
            |]
  ; rules_of_pred =
      PredicateNameMap.(empty
                        |> add "edge" [0;1]
                        |> add "path" [2;3])
  }


  (* For each relation variable:
     - Get the arity
     - Get the access patterns (for each position: Fixed, Ignored, Desired)
     - Generate:
       - initialisation code
       - search code (which requires the various indexes)
       - 
     - Basically, for each search pattern have a B-tree that has as keys the 
       inputs; and as values arrays of the associated values to be looked up.
     - Insertion puts the right things into each tree.

     - If we have a b-tree with attribute ordering x<y<z and we need to look
       up based on x<y, then we can just start at the first x<y key and carry on,
       doing a range scan. This would cut down on the number of indexes needed.

       Idea: 1. find a minimal set of 

     - If a relation is only used for insertion, we just maintain a list?
       Probably quicker if it is an ordered set, so it can be inserted quickly.
       Insertion-only relations (other than 'dead' ones) are used for the new_*
       relations for gathering the next iteration's delta set, and so will
       ultimately be (a) merged into the main relation, and (b) indexed to
       serve as the next iteration's delta_*.

     - Possible optimisations:
       - delayed recomputation of indexes?
       - key comparisons
       - can the delta relation be stored as part of the bigger relation by means
         of a linked list threaded through the nodes?
  *)


(*
type spec =
  | Fixed of int
  | Desired
  | Ignored

module type Relation = sig
  type t

  val make_empty : unit -> t
  val make_copy  : t -> t

  val move     : t -> t -> unit
  
  val iter     : spec array -> (int array -> unit) -> t -> unit

  val insert   : int array -> t -> unit

  val mem      : int array -> t -> bool
  val is_empty : t -> bool
end

module Relation : Relation = struct
  type trie = Leaf | Node of { mutable children : trie CCIntMap.t }

  type t = trie

  let make_empty () =
    Node { children = CCIntMap.empty }

  let rec make_copy = function
    | Leaf            -> Leaf
    | Node {children} -> Node { children = CCIntMap.map make_copy children }

  let is_empty = function
    | Leaf -> true
    | Node {children} -> CCIntMap.cardinal children = 0 (* FIXME: no is_empty? *)

  let move x y = match x, y with
    | Leaf, Leaf -> ()
    | Node x, Node y ->
       x.children <- y.children;
       y.children <- CCIntMap.empty
    | _ -> invalid_arg "move: mismatched tries"
  
  let iter spec f trie =
    let output_slots =
      Array.fold_left (fun i spec -> if spec = Desired then i+1 else i) 0 spec in
    let output = Array.make output_slots 0 in
    let rec search level olevel = function
      | Leaf -> f output
      | Node {children} ->
         match spec.(level) with
           | Fixed i ->
              (match CCIntMap.find_exn i children with
                | trie -> search (level+1) olevel trie
                | exception Not_found -> ())
           | Ignored ->
              CCIntMap.iter (fun _ -> search (level+1) olevel) children
           | Desired ->
              CCIntMap.iter (fun i trie -> output.(olevel) <- i; search (level+1) (olevel+1) trie) children
    in
    search 0 0 trie

  let insert tuple trie =
    let l = Array.length tuple in
    let rec build level target t =
      if level = target then t
      else
        let t = Node { children = CCIntMap.singleton tuple.(level) t } in
        build (level-1) target t
    in
    let rec insert level = function
      | Leaf ->
         assert (level = l)
      | Node n ->
         let i = tuple.(level) in
         match CCIntMap.find_exn i n.children with
           | trie -> insert (level+1) trie
           | exception Not_found ->
              let t = build (l-1) level Leaf in
              n.children <- CCIntMap.add i t n.children
    in
    insert 0 trie

  let mem tuple trie =
    let rec search level = function
      | Leaf ->
         assert (level = Array.length tuple);
         true
      | Node {children} ->
         match CCIntMap.find_exn tuple.(level) children with
           | trie -> search (level+1) trie
           | exception Not_found -> false
    in
    search 0 trie


end
*)
