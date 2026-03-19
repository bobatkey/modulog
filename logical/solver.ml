type t = Msat_sat.solver * int ref
type v = Msat_sat.Int_lit.t

let neg = Msat_sat.Int_lit.neg

(* FIXME: memoisation:
     - have a hash table that stores operations -> atom names
     - sort the conjunctions and disjunctions before use, and remove duplicates
     - also store ones that are definitely true or definitely false, and use this in simplification of ANDs and ORs
     *)

let create () = (Msat_sat.create (), ref 1)

let gen (_, r) =
  let v = !r in
  incr r;
  Msat_sat.Int_lit.make v

let add_conj ((solver, _) as t) vs =
  let x = gen t in
  Msat_sat.assume solver
    ((x :: List.map neg vs) :: List.map (fun v -> [ neg x; v ]) vs)
    ();
  x

let add_disj ((solver, _) as t) vs =
  let x = gen t in
  Msat_sat.assume solver
    ((neg x :: vs) :: List.map (fun v -> [ x; neg v ]) vs)
    ();
  x

let add_not ((solver, _) as t) v =
  let x = gen t in
  Msat_sat.assume solver [ [ neg x; neg v ]; [ x; v ] ] ();
  x

let add_implies ((solver, _) as t) v1 v2 =
  let x = gen t in
  Msat_sat.assume solver
    [ [ neg v1; v2; neg x ]; [ v1; x ]; [ neg v2; x ] ]
    ();
  x

let add_assert (solver, _) v = Msat_sat.assume solver [ [ v ] ] ()

let solve (solver, _) =
  match Msat_sat.solve solver with
  | Msat_sat.Unsat _ -> None
  | Msat_sat.Sat { eval; _} -> Some eval
