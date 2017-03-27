(* - Translation from Datalog down to 'Relational Machine'

   1. Start with a collection of rules.
   2. Find the strongly connected components of the rules.
   3. Turn each one into a loop using the 'semi-naive' strategy
*)

let list_init l f =
  let rec init i =
    if i = l then []
    else f i :: init (i+1)
  in
  init 0

module PredicateNameMap = Map.Make (String)

module Program = struct
  type predicate_name = string
  type variable       = string

  type expr =
    | Var of variable
    | Lit of int

  type atom =
    { atm_pred : predicate_name
    ; atm_args : expr list
    }

  type rule =
    { head : atom
    ; rhs  : atom list
    }

  type ruleset =
    { rules         : rule array
    ; rules_of_pred : int list PredicateNameMap.t
    }

  module G = struct
    type t = ruleset

    module V = struct
      type t = int
      let compare (x : t) (y : t) = Pervasives.compare x y
      let hash (x : t) = x
      let equal (x : t) y = x = y
    end

    let iter_vertex f ruleset =
      for i = 0 to Array.length ruleset.rules - 1 do
        f i
      done

    let iter_succ f ruleset rule_id =
      let rule = ruleset.rules.(rule_id) in
      rule.rhs |> List.iter begin fun {atm_pred} ->
        PredicateNameMap.find atm_pred ruleset.rules_of_pred |> List.iter f
      end
  end

  module SCC = Graph.Components.Make (G)

  let scc_list = SCC.scc_list

  (** A rule is self recursive if it mentions the head predicate in
      the right hand side. *)
  let rule_is_self_recursive ruleset rule_id =
    let rule = ruleset.rules.(rule_id) in
    List.exists (fun {atm_pred} -> atm_pred = rule.head.atm_pred) rule.rhs

  let of_rules rules =
    let update pred f map =
      let existing =
        match PredicateNameMap.find pred map with
          | exception Not_found -> []
          | rule_ids            -> rule_ids
      in
      PredicateNameMap.add pred (f existing) map
    in
    let rules = Array.of_list rules in
    let _, rules_of_pred =
      Array.fold_left
        (fun (i,map) rule -> (i+1, update rule.head.atm_pred (List.cons i) map))
        (0, PredicateNameMap.empty)
        rules
    in
    { rules; rules_of_pred }
end

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

module RAM = struct

  type var = string

  type attr = Program.variable

  type scalar =
    | Attr of attr
    | Lit  of int

  type expr =
    | Return of { guard_relation : var option
                ; values         : scalar list
                }
    | Select of { relation    : var
                ; conditions  : (int * scalar) list
                ; projections : (int * attr) list
                ; body        : expr
                }

  type comm =
    | WhileNotEmpty of var list * comms
    (** Loop until all the relations in the named variables are
        empty. *)

    | Insert of var list * expr

    | Move of var * var

    | Declare of (var * var option) list * comms

  and comms = comm list

  module VarSet  = (Set.Make (String) : Set.S with type elt = var)
  module AttrSet = (Set.Make (String) : Set.S with type elt = attr)

  let scalar_of_expr = function
    | Program.Var x -> Attr x
    | Program.Lit i -> Lit i

  let expr_of_rule varname_map guard_relation head atoms =
    let rec transl_rhs env = function
      | [] ->
         (* FIXME: assert that all the variables are in scope. *)
         let values = List.map scalar_of_expr head in
         let reqd   =
           List.fold_left
             (fun set -> function Attr a -> AttrSet.add a set | Lit _ -> set)
             AttrSet.empty
             values
         in
         assert (AttrSet.subset reqd env);
         Return { guard_relation; values }, reqd
      | {Program.atm_pred; atm_args} :: atoms ->
         let relation = varname_map atm_pred in
         let _, projections, conditions =
           List.fold_left
             (fun (i, projections, conditions) expr -> match expr with
                | Program.Var x when AttrSet.mem x env ->
                   (i+1, projections, (i,Attr x)::conditions)
                | Program.Var x ->
                   (i+1, (i, x)::projections, conditions)
                | Program.Lit v ->
                   (i+1, projections, (i,Lit v)::conditions))
             (0, [], [])
             atm_args
         in
         let env = List.fold_right (fun (_,x) -> AttrSet.add x) projections env in
         let body, reqd = transl_rhs env atoms in
         (* remove any attrs not in reqd from projections. *)
         let projections =
           List.filter (fun (_,x) -> AttrSet.mem x reqd) projections
         in
         let reqd =
           List.fold_left
             (fun reqd (_,x) -> AttrSet.remove x reqd)
             reqd
             projections
         in
         Select {relation; projections; conditions; body}, reqd
    in
    let expr, reqd = transl_rhs AttrSet.empty atoms in
    assert (AttrSet.is_empty reqd);
    expr

  let translate_rule ruleset rule_id =
    let open Program in
    let {head={atm_pred=predicate;atm_args=head}; rhs} =
      ruleset.Program.rules.(rule_id)
    in
    let expr = expr_of_rule (fun x -> x) None head rhs in
    Insert ([predicate], expr)

  (* To translate a non-recursive rule:
     - convert the rhs of the rule to an expr and emit an assignment

     To translate a recursive rule(s):
     - initialise the delta_p_i to the current values for p_i
     - while the union of the deltas is not empty:
       - compute new_p_i using delta_p_i for any predicates inside the loop
         - joins inside rules; unions across rules for same predicate
       - set delta_p_i = new_p_i 
       - set p_i       = p_i \cup new_p_i
  *)

  let translate_recursive ruleset rule_ids =
    let rules =
      List.map (fun id -> ruleset.Program.rules.(id)) rule_ids
    in
    (* get the set of predicates involved *)
    let predicates =
      List.fold_right
        (fun rule -> VarSet.add rule.Program.head.Program.atm_pred)
        rules
        VarSet.empty
    in
    let declarations =
      []
      |> VarSet.fold
        (fun pred_nm ->
           List.cons ([ "new_"^pred_nm, None
                      ; "delta_"^pred_nm, Some pred_nm ]))
        predicates
      |> List.concat
    in
    let var_transl nm =
      if VarSet.mem nm predicates then
        "delta_"^nm else nm
    in
    Declare
      (declarations,
       [ WhileNotEmpty
           (VarSet.fold (fun pred_nm -> List.cons ("delta_"^pred_nm)) predicates [],
            begin
              (rules |> List.map begin fun rule ->
                  let open Program in
                  let {head={atm_pred=predicate; atm_args=head}; rhs} = rule in
                  let expr = expr_of_rule var_transl (Some predicate) head rhs in
                  Insert ( [predicate; "new_"^predicate]
                         , expr 
                         )
                end)
              @
              VarSet.fold (fun nm -> List.cons (Move ("delta_"^nm, "new_"^nm))) predicates []
            end)
       ])

  let translate_component ruleset = function
    | [] ->
       invalid_arg "translate_component: empty component"
    | [rule_id] as rules ->
       if Program.rule_is_self_recursive ruleset rule_id then
         translate_recursive ruleset rules
       else
         translate_rule ruleset rule_id
    | rules ->
       translate_recursive ruleset rules

  let translate ruleset =
    let components = Program.scc_list ruleset in
    List.map (translate_component ruleset) components

  (* Indexes:
     - work out which indexes are needed:
       - for each select, we know the inputs and the outputs required.
       - therefore, we know what sub-indexes to maintain for each relation

     Optimisations:
     - if a selection has no projections, then it can just be a membership
       test, which avoids the necessity to loop.
  *)

  module Pattern = Set.Make (struct type t = int let compare = compare end)
  module PatternSet = Set.Make (Pattern)

  module PredicatePats = struct
    type t = PatternSet.t PredicateNameMap.t
    let empty = PredicateNameMap.empty
    let pats pred t =
      match PredicateNameMap.find pred t with
        | exception Not_found -> PatternSet.empty
        | set -> set
    let add pred pat t =
      let set = pats pred t in
      PredicateNameMap.add pred (PatternSet.add pat set) t
  end
  
  let rec search_patterns_of_expr = function
    | Select { relation; conditions; projections; body } ->
       let pat = Pattern.of_list (List.map fst conditions) in
       fun set -> PredicatePats.add relation pat (search_patterns_of_expr body set)
    | Return { guard_relation = None } ->
       fun set -> set
    | Return { guard_relation = Some relation; values } ->
       let pat = Pattern.of_list (list_init (List.length values) (fun i -> i)) in
       PredicatePats.add relation pat

  let rec search_patterns_of_comm = function
    | WhileNotEmpty (_, comms) | Declare (_, comms) ->
       search_patterns_of_comms comms
    | Insert (_, expr) ->
       search_patterns_of_expr expr
    | Move _ ->
       fun set -> set

  and search_patterns_of_comms comms =
    List.fold_right search_patterns_of_comm comms

  let search_patterns comms =
    search_patterns_of_comms comms PredicatePats.empty
end

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
