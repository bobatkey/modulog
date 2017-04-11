module RS = Datalog_ruleset

type var = string

type attr = string

type scalar =
  | Attr of attr
  | Lit  of int32

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
  (** Insert the results of the expression into the named
      variables. *)

  | Move of { tgt : var; src : var }
  (** Move the contents of 'src' into 'tgt', leaving 'src' empty. *)

  | Declare of (var * var option) list * comms

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
     Format.fprintf fmt "@[<hv 4>insert into @[<hv>%a@] value@ %a@]"
       Format_util.(pp_list Format.pp_print_string) vars
       pp_expr expr
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


module VarSet  = struct
  include (Set.Make (String) : Set.S with type elt = var)
  let map_to_list f set =
    fold (fun x -> List.cons (f x)) set []
  let concat_map_to_list f set =
    List.concat (map_to_list f set)
end
module AttrSet = (Set.Make (String) : Set.S with type elt = attr)

let scalar_of_expr = function
  | RS.Var x      -> Attr x
  | RS.Lit i      -> Lit i
  | RS.Underscore -> failwith "FIXME: do underscores"

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

let translate_rule ruleset rule_id =
  let RS.{pred; args; rhs} = ruleset.RS.rules.(rule_id) in
  let expr = expr_of_rule None args rhs in
  Insert ([pred], expr)

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

let predicates_of_rules =
  List.fold_left
    (fun set rule -> VarSet.add rule.RS.pred set)
    VarSet.empty

let extract_predicate dpred rhs =
  let rec loop before = function
    | [] ->
       []
    | (RS.Atom { pred; args } as atom) :: after ->
       let rest = loop (atom :: before) after in
       if pred = dpred then
         let hatom = RS.Atom { pred = "delta_"^dpred; args} in
         (hatom :: List.rev_append before after) :: rest
       else
         rest
  in
  loop [] rhs

let translate_recursive ruleset rule_ids =
  let rules = List.map (fun id -> ruleset.RS.rules.(id)) rule_ids in
  let predicates = predicates_of_rules rules in
  let delta_predicates =
    VarSet.map_to_list (fun pred_nm -> "delta_"^pred_nm) predicates
  in
  let declarations =
    VarSet.concat_map_to_list
      (fun pred_nm -> ["new_"^pred_nm, None; "delta_"^pred_nm, Some pred_nm])
      predicates
  in
  let moves =
    VarSet.map_to_list
      (fun nm -> Move {tgt="delta_"^nm; src="new_"^nm})
      predicates
  in
  Declare
    (declarations,
     [ WhileNotEmpty
         (delta_predicates,
          begin
            (rules
             |> List.map
               begin fun RS.{pred; args; rhs} ->
                 VarSet.concat_map_to_list
                   (fun delta'd_predicate ->
                      List.map
                        (fun rhs ->
                           Insert ([pred; "new_"^pred], expr_of_rule (Some pred) args rhs))
                        (extract_predicate delta'd_predicate rhs))
                   predicates
               end
             |> List.concat)
            @
            moves
          end)
     ])

let translate_component ruleset = function
  | [] ->
     invalid_arg "translate_component: empty component"
  | [rule_id] as rules ->
     if RS.rule_is_self_recursive ruleset rule_id then
       translate_recursive ruleset rules
     else
       translate_rule ruleset rule_id
  | rules ->
     translate_recursive ruleset rules

let translate ruleset =
  let components = RS.scc_list ruleset in
  List.map (translate_component ruleset) components

  (* Indexes:
     - work out which indexes are needed:
       - for each select, we know the inputs and the outputs required.
       - therefore, we know what sub-indexes to maintain for each relation

     Optimisations:
     - if a selection has no projections, then it can just be a membership
       test, which avoids the necessity to loop.
  *)

module VarMap = Map.Make (String)
module Pattern = Set.Make (struct type t = int let compare = compare end)
module PatternSet = Set.Make (Pattern)

module PredicatePats = struct
  type t = PatternSet.t VarMap.t
  let empty = VarMap.empty
  let pats pred t =
    match VarMap.find pred t with
      | exception Not_found -> PatternSet.empty
      | set -> set
  let add pred pat t =
    let set = pats pred t in
    VarMap.add pred (PatternSet.add pat set) t
end

let list_init l f =
  let rec init i =
    if i = l then []
    else f i :: init (i+1)
  in
  init 0

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
