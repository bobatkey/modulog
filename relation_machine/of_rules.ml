module Ruleset = Datalog.Ruleset

open Syntax

module RelvarSet  = struct
  include (Set.Make (RelVar) : Set.S with type elt = relvar)

  let map_to_list f set =
    fold (fun x -> List.cons (f x)) set []

  let concat_map_to_list f set =
    List.concat (map_to_list f set)
end

module AttrSet =
  (Set.Make (Attr) : Set.S with type elt = attr)


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
       (* FIXME: move to arrays earlier *)
       let values = List.map scalar_of_expr head in
       let reqd   =
         List.fold_left
           (fun set -> function Attr a -> AttrSet.add a set | Lit _ -> set)
           AttrSet.empty
           values
       in
       assert (AttrSet.subset reqd env);
       let values = Array.of_list values in
       let expr = Return { values } in
       let expr = match guard_relation with
         | Some relation -> Guard_NotIn { relation; values; cont = expr }
         | None          -> expr
       in
       expr, reqd

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

let select_all src =
  let projections = Array.init src.arity (fun i -> (i, Printf.sprintf "X%d" i)) in
  let values      = Array.map (fun (_, nm) -> Attr nm) projections in
  Select { relation = src
         ; conditions = []
         ; projections = Array.to_list projections
         ; cont = Return { values }
         }

let mk_merge src tgt =
  (* FIXME: these insertions don't need to check for duplicates. Could
     have a flag that says 'dispense with the membership test'. *)
  Insert (tgt, select_all src)

let buf_ relvar =
  { relvar with ident = "buf:" ^ relvar.ident }

let extract_predicate dpred rhs =
  let rec loop before = function
    | [] ->
       []
    | (Ruleset.Atom { pred; args } as atom) :: after ->
       let rest = loop (atom :: before) after in
       if relvar_of_predname pred = dpred then
         let hatom = Ruleset.Atom { pred = predname_of_relvar (buf_ dpred)
                                  ; args } in
         (hatom :: List.rev_append before after) :: rest
       else
         rest
  in
  loop [] rhs

let translate_recursive ruleset rules =
  let predicates = predicates_of_rules rules in
  let buf_predicates = RelvarSet.map_to_list buf_ predicates in
  let initialisations =
    RelvarSet.map_to_list (fun pred_nm -> mk_merge pred_nm (buf_ pred_nm))
      predicates in
  let swaps = List.map (fun nm -> Swap nm) buf_predicates in
  let updates =
    List.concat @@
    List.map
      begin fun Ruleset.{pred; args; rhs} ->
        let pred = relvar_of_predname pred in
        RelvarSet.concat_map_to_list
          (fun delta'd_predicate ->
             List.map
               (fun rhs -> Insert (buf_ pred, expr_of_rule (Some pred) args rhs))
               (extract_predicate delta'd_predicate rhs))
          predicates
      end
      rules
  and merges =
    RelvarSet.map_to_list
      (fun nm -> mk_merge (buf_ nm) nm)
      predicates
  in
  DeclareBuffers
    (buf_predicates,
     initialisations @
     swaps @
     [ WhileNotEmpty (buf_predicates, updates @ swaps @ merges) ])

let translate_component ruleset = function
  | `Direct rule ->
     translate_rule ruleset rule
  | `Recursive rules ->
     translate_recursive ruleset rules

let translate_rules ruleset =
  List.map (translate_component ruleset) (Ruleset.components ruleset)


let translate ruleset =
  (* FIXME: restrict the scope of each predicate to only the rule
     evaluations it is needed for. *)
  List.fold_right
    (fun (pred_name, info) {relvars; commands} ->
       let relvar = relvar_of_predname pred_name in
       let prefix =
         match info.Ruleset.kind with
           | `Intensional ->
              []
           | `Extensional filename ->
              [ReadRelation (relvar, filename)]
       in
       let suffix =
         List.map
           (fun filename -> WriteRelation (relvar, filename))
           info.Ruleset.output
       in
       { relvars  = relvar :: relvars
       ; commands = prefix @ commands @ suffix
       })
    (Ruleset.predicates ruleset)
    { relvars = []
    ; commands = translate_rules ruleset }
