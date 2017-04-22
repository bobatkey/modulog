(* FIXME: keep track of the arities of predicates too *)

type predicate_name = string

type expr =
  | Var of string
  | Lit of int32
  | Underscore

type atom =
  | Atom of { pred : predicate_name; args : expr list }

type rule =
  { pred : predicate_name
  ; args : expr list
  ; rhs  : atom list
  }

(**********************************************************************)

let pp_expr fmt = function
  | Var vnm ->
     Format.fprintf fmt "%s" vnm
  | Lit i ->
     Format.fprintf fmt "%ld" i
  | Underscore ->
     Format.fprintf fmt "_"

let pp_exprs = Format_util.pp_list pp_expr

let pp_atom fmt = function
  | Atom { pred; args } ->
     Format.fprintf fmt "%s(@[<h>%a@])"
       pred
       pp_exprs args

let pp_rhs = Format_util.pp_list pp_atom

let pp_rule fmt {pred; args; rhs} =
  match rhs with
    | [] ->
       Format.fprintf fmt
         "%s(@[<h>%a@])"
         pred
         pp_exprs args
    | rhs ->
       Format.fprintf fmt
         "%s(@[<h>%a@]) :- @[<hv>%a@]"
         pred
         pp_exprs args
         pp_rhs rhs

(**********************************************************************)
module PredicateNameMap = Map.Make (String)

type ruleset =
  { rules         : rule array
  ; rules_of_pred : int list PredicateNameMap.t
  }

type rule_id = int

let rule_id i = i

let pp fmt set =
  Format.pp_open_vbox fmt 0;
  for i = 0 to Array.length set.rules - 1 do
    pp_rule fmt set.rules.(i);
    if i < Array.length set.rules - 1 then
      Format.pp_print_cut fmt ()
  done;
  Format.pp_close_box fmt ()

let rule i set = set.rules.(i)

module G = struct
  type t = ruleset

  module V = struct
    type t = rule_id
    let compare (x : t) (y : t) = Pervasives.compare x y
    let hash (x : t) = x
    let equal (x : t) y = x = y
  end

  type vertex = V.t

  let graph_of_vertex (ruleset, _) = ruleset
  
  module E = struct
    type t = V.t * V.t
    let src = fst
    let dst = snd
  end

  type edge = E.t

  let iter_vertex f ruleset =
    for i = 0 to Array.length ruleset.rules - 1 do
      f i
    done

  let iter_succ f ruleset rule_id =
    let rule = ruleset.rules.(rule_id) in
    rule.rhs |> List.iter begin fun (Atom {pred}) ->
      match PredicateNameMap.find pred ruleset.rules_of_pred with
        | exception Not_found -> ()
        | rule_ids -> List.iter f rule_ids
    end

  let iter_edges_e f ruleset =
    iter_vertex (fun src -> iter_succ (fun tgt -> f (src,tgt)) ruleset src) ruleset
end

(** A rule is self recursive if it mentions the head predicate in the
    right hand side. *)
let rule_is_self_recursive ruleset rule_id =
  let rule = ruleset.rules.(rule_id) in
  List.exists (fun (Atom {pred}) -> pred = rule.pred) rule.rhs

module SCC = Graph.Components.Make (G)

let form_of_component ruleset = function
  | [] -> assert false
  | [rule_id] ->
     if rule_is_self_recursive ruleset rule_id then
       `Recursive [rule rule_id ruleset]
     else
       `Direct (rule rule_id ruleset)
  | rules ->
     `Recursive (List.map (fun id -> rule id ruleset) rules)

let scc_list ruleset =
  List.map (form_of_component ruleset) (SCC.scc_list ruleset)


let of_rules rules =
  let update pred f map =
    let existing =
      match PredicateNameMap.find pred map with
        | exception Not_found -> []
        | rule_ids -> rule_ids
    in
    PredicateNameMap.add pred (f existing) map
  in
  let rules = Array.of_list rules in
  let _, rules_of_pred =
    Array.fold_left
      (fun (i,map) rule -> (i+1, update rule.pred (List.cons i) map))
      (0, PredicateNameMap.empty)
      rules
  in
  { rules; rules_of_pred }
