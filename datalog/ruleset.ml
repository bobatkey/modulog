type predicate_name =
  { ident : string
  ; arity : int
  }

module PredicateName = struct
  type t = predicate_name

  let compare x y =
    match Pervasives.compare x.ident y.ident with
      | 0 -> Pervasives.compare x.arity y.arity
      | c -> c
end

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
     Format.fprintf fmt "?%s" vnm
  | Lit i ->
     Format.fprintf fmt "%ld" i
  | Underscore ->
     Format.fprintf fmt "_"

let pp_exprs =
  Fmt.list ~sep:(Fmt.always ", ") pp_expr

let pp_atom fmt = function
  | Atom { pred; args } ->
     Format.fprintf fmt "%s(%a)"
       pred.ident
       pp_exprs args

let pp_rhs =
  Fmt.list ~sep:(Fmt.always ",@ ") pp_atom

let pp_rule fmt = function
  | { pred; args; rhs=[] } ->
     Format.fprintf fmt
       "%s(@[<h>%a@])."
       pred.ident
       pp_exprs args
  | { pred; args; rhs } ->
     Format.fprintf fmt
       "@[<v4>%s(@[<h>%a@]) :-@ %a.@]"
       pred.ident
       pp_exprs args
       pp_rhs rhs

(**********************************************************************)
module PredicateNameMap = Map.Make (PredicateName)

type predicate_info =
  { intensional : bool
  }

type ruleset =
  { rules         : rule array
  ; rules_of_pred : int list PredicateNameMap.t
  ; pred_info     : predicate_info PredicateNameMap.t 
  }

let pp_pred_info fmt (name, {intensional}) =
  Format.fprintf fmt
    "%s %s/%d"
    (if intensional then "int" else "ext")
    name.ident
    name.arity

let pp fmt set =
  Format.fprintf fmt "@[<v>%a@]"
    Fmt.(iter_bindings PredicateNameMap.iter pp_pred_info) set.pred_info;
  Format.pp_print_cut fmt ();
  Format.pp_print_cut fmt ();
  Format.pp_open_vbox fmt 0;
  for i = 0 to Array.length set.rules - 1 do
    pp_rule fmt set.rules.(i);
    if i < Array.length set.rules - 1 then
      Format.pp_print_cut fmt ()
  done;
  Format.pp_close_box fmt ()

type rule_id = int

let rule_id i = i

let rule_of_id i set = set.rules.(i)

let predicates {pred_info} =
  PredicateNameMap.bindings pred_info

(**********************************************************************)
module Builder = struct
  type t =
    { rules_so_far      : rule list
    ; next_rule_id      : int
    ; index_so_far      : int list PredicateNameMap.t
    ; predicates_so_far : predicate_info PredicateNameMap.t
    }

  type error =
    | Undeclared_predicate of predicate_name
    | Arity_mismatch of
        { pred       : predicate_name
        ; used_arity : int
        }
    | Definition_of_extensional_predicate of predicate_name
    | Predicate_already_declared of predicate_name

  let empty =
    { rules_so_far      = []
    ; next_rule_id      = 0
    ; index_so_far      = PredicateNameMap.empty
    ; predicates_so_far = PredicateNameMap.empty
    }

  let update_index pred f map =
    let existing =
      match PredicateNameMap.find pred map with
        | exception Not_found -> []
        | rule_ids -> rule_ids
    in
    PredicateNameMap.add pred (f existing) map

  let rec check_atoms pred_info = function
    | [] ->
       Ok ()
    | Atom {pred;args} :: atoms ->
       let used_arity = List.length args in
       if pred.arity <> used_arity then
         Error (Arity_mismatch { pred; used_arity })
       else
         check_atoms pred_info atoms

  let add_rule ({pred;args;rhs} as rule) t =
    match check_atoms t.predicates_so_far rhs with
      | Error e -> Error e
      | Ok () ->
         match PredicateNameMap.find pred t.predicates_so_far with
           | exception Not_found ->
              Error (Undeclared_predicate pred)
           | {intensional=false} ->
              Error (Definition_of_extensional_predicate pred)
           | _ ->
              let used_arity = List.length args in
              if pred.arity <> used_arity then
                Error (Arity_mismatch {pred; used_arity})
              else
                let id = t.next_rule_id in
                Ok { t with rules_so_far = rule :: t.rules_so_far
                          ; next_rule_id = id + 1
                          ; index_so_far =
                              update_index pred (List.cons id) t.index_so_far
                   }

  let add_predicate name intensional t =
    match PredicateNameMap.find name t.predicates_so_far with
      | exception Not_found ->
         let info = { intensional } in
         Ok { t with predicates_so_far =
                       PredicateNameMap.add name info t.predicates_so_far
            }
      | { intensional = previous_intensionality } ->
         if previous_intensionality = intensional then
           Ok t
         else
           Error (Predicate_already_declared name)

  let add_predicate name int t =
    match int with
      | `Extensional -> add_predicate name false t
      | `Intensional -> add_predicate name true t

  let finish { rules_so_far; next_rule_id; index_so_far; predicates_so_far } =
    let rules_of_pred = index_so_far
    and pred_info     = predicates_so_far in
    if next_rule_id = 0 then
      { rules = [||]; rules_of_pred; pred_info }
    else
      let rules = Array.make next_rule_id (List.hd rules_so_far) in
      (* Do this backwards to maintain the numbering *)
      let rec insert_all i = function
        | []    -> ()
        | r::rs -> rules.(i) <- r; insert_all (i-1) rs
      in
      insert_all (next_rule_id - 1) rules_so_far;
      { rules; rules_of_pred; pred_info }
end

type builder = Builder.t

(**********************************************************************)
module As_graph = struct
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
let rule_is_self_recursive rule_id ruleset =
  let rule = ruleset.rules.(rule_id) in
  List.exists (fun (Atom {pred}) -> pred = rule.pred) rule.rhs

module SCC = Graph.Components.Make (As_graph)

let form_of_component ruleset = function
  | [] ->
     assert false
  | [id] ->
     if rule_is_self_recursive id ruleset then
       `Recursive [rule_of_id id ruleset]
     else
       `Direct (rule_of_id id ruleset)
  | rules ->
     `Recursive (List.map (fun id -> rule_of_id id ruleset) rules)

let components ruleset =
  List.map (form_of_component ruleset) (SCC.scc_list ruleset)
