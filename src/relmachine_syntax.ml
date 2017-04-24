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

type program =
  { edb_relvars : (relvar * int) list
  ; idb_relvars : (relvar * int) list
  ; commands    : comms
  }

(**********************************************************************)

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
       Fmt.(list ~sep:(always ",@ ") pp_scalar) values
  | Return {guard_relation=Some rel; values} ->
     Format.fprintf fmt "(@[<h>%a@]) unless in %s"
       Fmt.(list ~sep:(always ",@ ") pp_scalar) values
       rel
  | Select { relation; conditions=[]; projections; body } ->
     Format.fprintf fmt
       "@[<hv 0>select @[<hv 0>from %s@ with@ @[%a@]@]@ in@]@ %a"
       relation
       Fmt.(list ~sep:(always ",@ ") pp_projection) projections
       pp_expr body
  | Select { relation; conditions; projections; body } ->
     Format.fprintf fmt
       "@[<hv 0>select @[<hv 0>from %s@ when @[%a@]@ with @[%a@]@]@ in@]@ %a"
       relation
       Fmt.(list ~sep:(always ",@ ") pp_condition) conditions
       Fmt.(list ~sep:(always ",@ ") pp_projection) projections
       pp_expr body

let pp_initialiser fmt = function
  | vnm, None -> Format.fprintf fmt "%s = { }" vnm
  | vnm, Some vnm' -> Format.fprintf fmt "%s = %s" vnm vnm'

let rec pp_comm fmt = function
  | WhileNotEmpty (rels, body) ->
     Format.fprintf fmt "@[<v 4>whileNotEmpty (@[<hv>%a@]) do@,%a@]@,end"
       Fmt.(list ~sep:(always ",@ ") string) rels
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
       Fmt.(list ~sep:(always ",@ ") pp_initialiser) initialisers
       pp_comms body

and pp_comms fmt = function
  | [] -> ()
  | [c] -> pp_comm fmt c
  | c::cs ->
     pp_comm fmt c;
     Format.pp_print_cut fmt ();
     Format.pp_print_cut fmt ();
     pp_comms fmt cs

let pp_program fmt {edb_relvars; idb_relvars; commands} =
  let pp_relvar_decl typ fmt (nm, arity) =
    Format.fprintf fmt "%s %s : %d" typ nm arity
  in
  Format.fprintf fmt "@[<v>%a@,@,%a@,@,%a@]"
    Fmt.(list (pp_relvar_decl "ext")) edb_relvars
    Fmt.(list (pp_relvar_decl "int")) idb_relvars
    pp_comms                          commands

(**********************************************************************)
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
