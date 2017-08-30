module Ruleset = Datalog.Ruleset

open Syntax

module RelvarSet  = struct
  module Relvar = struct
    type t = relvar
    let compare = compare
  end
  include (Set.Make (Relvar) : Set.S with type elt = relvar)

  let map_to_list f set =
    fold (fun x -> List.cons (f x)) set []

  let concat_map_to_list f set =
    List.concat (map_to_list f set)
end

module AttrSet =
  (Set.Make (String) : Set.S with type elt = attr)


let scalar_of_expr = function
  | Ruleset.Var x      -> Attr x
  | Ruleset.Lit i      -> Lit i
  | Ruleset.Underscore -> failwith "internal error: underscore found in head"

let relvar_of_predname Ruleset.{ ident; arity } =
  { ident; arity }

let predname_of_relvar { ident; arity } =
  Ruleset.{ ident; arity }

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

    | Ruleset.Atom {pred=relation; args} :: atoms ->
       let _, projections, conditions =
         List.fold_left
           (fun (i, projections, conditions) expr -> match expr with
              | Ruleset.Var x when AttrSet.mem x env ->
                 (i+1, projections, (i,Attr x)::conditions)
              | Ruleset.Var x ->
                 (i+1, (i, x)::projections, conditions)
              | Ruleset.Lit v ->
                 (* FIXME: assert that 'v' is in scope *)
                 (i+1, projections, (i,Lit v)::conditions)
              | Ruleset.Underscore ->
                 (i+1, projections, conditions))
           (0, [], [])
           args
       in
       let env = List.fold_right (fun (_,x) -> AttrSet.add x) projections env in
       let cont, reqd = transl_rhs env atoms in
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
       let relation = relvar_of_predname relation in
       Select {relation; projections; conditions; cont}, reqd
  in
  let expr, reqd = transl_rhs AttrSet.empty atoms in
  assert (AttrSet.is_empty reqd);
  expr

let relvar_of_rule { Ruleset.pred } =
  relvar_of_predname pred

let translate_rule ruleset (Ruleset.{pred; args; rhs} as rule) =
  let expr = expr_of_rule None args rhs in
  let rel  = relvar_of_rule rule in
  Insert (rel, expr)

let predicates_of_rules =
  List.fold_left
    (fun set rule -> RelvarSet.add (relvar_of_rule rule) set)
    RelvarSet.empty

let delta_ relvar =
  { relvar with ident = "delta:" ^ relvar.ident }

let new_ relvar =
  { relvar with ident = "new:" ^ relvar.ident }

let select_all src =
  let projections =
    Array.to_list (Array.init src.arity (fun i -> (i, Printf.sprintf "X%d" i)))
  in
  let values =
    List.map (fun (_, nm) -> Attr nm) projections
  in
  Select { relation = src
         ; conditions = []
         ; projections
         ; cont = Return { guard_relation = None; values }
         }

let mk_merge src tgt =
  Insert (tgt, select_all src)

let extract_predicate dpred rhs =
  let rec loop before = function
    | [] ->
       []
    | (Ruleset.Atom { pred; args } as atom) :: after ->
       let rest = loop (atom :: before) after in
       if relvar_of_predname pred = dpred then
         let hatom = Ruleset.Atom { pred = predname_of_relvar (delta_ dpred)
                                  ; args } in
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
      (fun pred_nm -> [new_ pred_nm; delta_ pred_nm])
      predicates
  and initialisations =
    RelvarSet.map_to_list
      (fun pred_nm -> mk_merge pred_nm (delta_ pred_nm))
      predicates
  in
  let updates =
    List.concat @@
    List.map
      begin fun Ruleset.{pred; args; rhs} ->
        let pred = relvar_of_predname pred in
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
      (fun nm -> mk_merge (new_ nm) nm)
      predicates
  and moves =
    RelvarSet.map_to_list
      (fun nm -> Move {tgt=delta_ nm; src=new_ nm})
      predicates
  in
  Declare
    (declarations, initialisations @ [ WhileNotEmpty (delta_predicates, updates @ merges @ moves) ])

let translate_component ruleset = function
  | `Direct rule ->
     translate_rule ruleset rule
  | `Recursive rules ->
     translate_recursive ruleset rules

let translate_rules ruleset =
  List.map (translate_component ruleset) (Ruleset.components ruleset)


let translate ruleset =
  let edb_relvars, idb_relvars =
    List.fold_left
      (fun (edb, idb) (pred_name, Ruleset.{intensional}) ->
         let decl = relvar_of_predname pred_name in
         if intensional then
           (edb, decl :: idb)
         else
           (decl :: edb, idb))
      ([], [])
      (Ruleset.predicates ruleset)
  in
  let commands = translate_rules ruleset in
  { edb_relvars; idb_relvars; commands }
