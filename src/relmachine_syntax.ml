module RS = Datalog_ruleset

type relvar = string

type attr = string

type scalar =
  | Attr of attr
  | Lit  of int32

type expr =
  | Return of { guard_relation : relvar option
              ; values         : scalar list
              }
  | Select of { relation    : relvar
              ; conditions  : (int * scalar) list
              ; projections : (int * attr) list
              ; body        : expr
              }

type comm =
  | WhileNotEmpty of relvar list * comms
  (** Loop until all the relations in the named variables are
      empty. *)

  | Insert of relvar * expr
  (** Insert the results of the expression into the named
      variable. *)

  | Merge of { tgt : relvar; src : relvar }

  | Move of { tgt : relvar; src : relvar }
  (** Move the contents of 'src' into 'tgt', leaving 'src' empty. *)

  | Declare of (relvar * relvar option) list * comms

and comms = comm list

let pp_scalar fmt = function
  | Attr attr -> Format.pp_print_string fmt attr
  | Lit i     -> Format.fprintf fmt "%ld" i

let pp_condition fmt (idx, scalar) =
  Format.fprintf fmt "%d = %a" idx pp_scalar scalar

let pp_projection fmt (idx, attr) =
  Format.fprintf fmt "%d -> %s" idx attr

let rec pp_expr fmt = function
  | Return {guard_relation=None; values} ->
     Format.fprintf fmt "(@[<h>%a@])"
       (Format_util.pp_list pp_scalar) values
  | Return {guard_relation=Some rel; values} ->
     Format.fprintf fmt "(@[<h>%a@]) unless in %s"
       (Format_util.pp_list pp_scalar) values
       rel
  | Select { relation; conditions=[]; projections; body } ->
     Format.fprintf fmt
       "@[<hv 0>select @[<hv 0>from %s@ with@ @[%a@]@]@ in@]@ %a"
       relation
       (Format_util.pp_list pp_projection) projections
       pp_expr body
  | Select { relation; conditions; projections; body } ->
     Format.fprintf fmt
       "@[<hv 0>select @[<hv 0>from %s@ when @[%a@]@ with @[%a@]@]@ in@]@ %a"
       relation
       (Format_util.pp_list pp_condition) conditions
       (Format_util.pp_list pp_projection) projections
       pp_expr body

let pp_initialiser fmt = function
  | vnm, None -> Format.fprintf fmt "%s = { }" vnm
  | vnm, Some vnm' -> Format.fprintf fmt "%s = %s" vnm vnm'

let rec pp_comm fmt = function
  | WhileNotEmpty (rels, body) ->
     Format.fprintf fmt "@[<v 4>whileNotEmpty (@[<hv>%a@]) do@,%a@]@,end"
       Format_util.(pp_list Format.pp_print_string) rels
       pp_comms body
  | Insert (vars, expr) ->
     Format.fprintf fmt "@[<hv 4>insert into %s value@ %a@]"
       vars
       pp_expr expr
  | Merge {tgt; src} ->
     Format.fprintf fmt "merge %s into %s" src tgt
  | Move {tgt; src} ->
     Format.fprintf fmt "move %s into %s" src tgt
  | Declare (initialisers, body) ->
     Format.fprintf fmt "@[<v 4>declare (@[<v 0>%a@]) in@,%a@]@,end"
       (Format_util.pp_list pp_initialiser) initialisers
       pp_comms body

and pp_comms fmt = function
  | [] -> ()
  | [c] -> pp_comm fmt c
  | c::cs ->
     pp_comm fmt c;
     Format.pp_print_cut fmt ();
     Format.pp_print_cut fmt ();
     pp_comms fmt cs


module RelvarSet  = struct
  include (Set.Make (String) : Set.S with type elt = relvar)
  let map_to_list f set =
    fold (fun x -> List.cons (f x)) set []
  let concat_map_to_list f set =
    List.concat (map_to_list f set)
end

module AttrSet = (Set.Make (String) : Set.S with type elt = attr)

let scalar_of_expr = function
  | RS.Var x      -> Attr x
  | RS.Lit i      -> Lit i
  | RS.Underscore -> failwith "internal error: underscore found in head"

let expr_of_rule guard_relation head atoms =
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

    | RS.Atom {pred=relation; args} :: atoms ->
       let _, projections, conditions =
         List.fold_left
           (fun (i, projections, conditions) expr -> match expr with
              | RS.Var x when AttrSet.mem x env ->
                 (i+1, projections, (i,Attr x)::conditions)
              | RS.Var x ->
                 (i+1, (i, x)::projections, conditions)
              | RS.Lit v ->
                 (* FIXME: assert that 'v' is in scope *)
                 (i+1, projections, (i,Lit v)::conditions)
              | RS.Underscore ->
                 (i+1, projections, conditions))
           (0, [], [])
           args
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
       let reqd =
         List.fold_left
           (fun reqd (_, s) -> match s with
              | Attr vnm -> AttrSet.add vnm reqd
              | Lit _    -> reqd)
           reqd
           conditions
       in
       Select {relation; projections; conditions; body}, reqd
  in
  let expr, reqd = transl_rhs AttrSet.empty atoms in
  assert (AttrSet.is_empty reqd);
  expr

let translate_rule ruleset RS.{pred; args; rhs} =
  let expr = expr_of_rule None args rhs in
  Insert (pred, expr)

let predicates_of_rules =
  List.fold_left
    (fun set rule -> RelvarSet.add rule.RS.pred set)
    RelvarSet.empty

let delta_ nm = "delta_"^nm
let new_ nm = "new_"^nm

let extract_predicate dpred rhs =
  let rec loop before = function
    | [] ->
       []
    | (RS.Atom { pred; args } as atom) :: after ->
       let rest = loop (atom :: before) after in
       if pred = dpred then
         let hatom = RS.Atom { pred = delta_ dpred; args} in
         (hatom :: List.rev_append before after) :: rest
       else
         rest
  in
  loop [] rhs

let translate_recursive ruleset rules =
  let predicates = predicates_of_rules rules in
  let delta_predicates = RelvarSet.map_to_list delta_ predicates in
  let declarations =
    RelvarSet.concat_map_to_list
      (fun pred_nm -> [new_ pred_nm, None; delta_ pred_nm, Some pred_nm])
      predicates
  in
  let updates =
    List.concat @@
    List.map
      begin fun RS.{pred; args; rhs} ->
        RelvarSet.concat_map_to_list
          (fun delta'd_predicate ->
             List.map
               (fun rhs ->
                  Insert (new_ pred, expr_of_rule (Some pred) args rhs))
               (extract_predicate delta'd_predicate rhs))
          predicates
      end
      rules
  and merges =
    RelvarSet.map_to_list
      (fun nm -> Merge { src = new_ nm; tgt = nm })
      predicates
  and moves =
    RelvarSet.map_to_list
      (fun nm -> Move {tgt="delta_"^nm; src="new_"^nm})
      predicates
  in
  Declare
    (declarations, [ WhileNotEmpty (delta_predicates, updates @ merges @ moves) ])

let translate_component ruleset = function
  | `Direct rule ->
     translate_rule ruleset rule
  | `Recursive rules ->
     translate_recursive ruleset rules

let translate ruleset =
  List.map (translate_component ruleset) (RS.scc_list ruleset)

(**********************************************************************)
let rec free_written_relvars_comm comm =
  match comm with
    | WhileNotEmpty (_, comms) ->
       free_written_relvars comms
    | Insert (relvar, _) ->
       RelvarSet.add relvar
    | Merge { tgt } | Move { tgt } ->
       RelvarSet.add tgt
    | Declare (vars, comms) ->
       fun set ->
         List.fold_right (fun (rv, _) -> RelvarSet.remove rv) vars @@
         free_written_relvars comms @@
         set

and free_written_relvars comms =
  List.fold_right free_written_relvars_comm comms

let free_written_relvars comms =
  free_written_relvars comms RelvarSet.empty

(* Indexes:
     - work out which indexes are needed:
       - for each select, we know the inputs and the outputs required.
       - therefore, we know what sub-indexes to maintain for each relation

     Optimisations:
     - if a selection has no projections, then it can just be a membership
       test, which avoids the necessity to loop.
  *)

module PatternSet = struct
  (* FIXME: this could be more efficiently implemented with using
     BDDs, but it is probably not worth it for the sizes of sets we
     will be dealing with. *)

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

  val pats : string -> t -> PatternSet.t

  val add : string -> PatternSet.pattern -> t -> t

  val fold : (string -> PatternSet.t -> 'a -> 'a) -> t -> 'a -> 'a

  val map_to_list : (string -> PatternSet.t -> 'a) -> t -> 'a list

  val pp : Format.formatter -> t -> unit
end = struct
  module VarMap = Map.Make (String)

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
         Fmt.string
         PatternSet.pp)
end

let rec search_patterns_of_expr = function
  | Select { relation; conditions; projections; body } ->
     let pat = PatternSet.Pattern.of_list (List.map fst conditions) in
     fun set -> PredicatePats.add relation pat (search_patterns_of_expr body set)
  | Return { guard_relation = None } ->
     fun set -> set
  | Return { guard_relation = Some relation; values } ->
     let pat = PatternSet.Pattern.complete (List.length values) in
     PredicatePats.add relation pat

let rec search_patterns_of_comm = function
  | WhileNotEmpty (_, comms) | Declare (_, comms) ->
     search_patterns_of_comms comms
  | Insert (_, expr) ->
     search_patterns_of_expr expr
  | Move _ | Merge _ ->
     fun set -> set

and search_patterns_of_comms comms =
  List.fold_right search_patterns_of_comm comms

let search_patterns comms =
  search_patterns_of_comms comms PredicatePats.empty

let indexes comms : (string * PatternSet.Pattern.t list list) list =
  comms
  |> search_patterns
  |> PredicatePats.map_to_list
    (fun pred pats -> (pred, List.rev (MPC.minimal_path_cover pats)))

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
            (pair ~sep:(always " =>@ ") string pp_indexes)))

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
