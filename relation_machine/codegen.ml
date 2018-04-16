module Array = struct
  include Array
  let fold_lefti f init arr =
    let rec loop ~idx ~accum =
      if idx = Array.length arr then
        accum
      else
        let accum = f accum idx arr.(idx) in
        loop ~idx:(idx+1) ~accum
    in
    loop ~idx:0 ~accum:init
end

module type INDEXED_TABLE = sig
  module S : Idealised_algol.Syntax.S

  type handle

  val arity : int

  val declare : name:string -> (handle -> S.comm) -> S.comm

  val iterate :
    handle ->
    pat:[`Wild | `Fixed of int32 S.exp] array ->
    (int32 S.exp array -> S.comm) ->
    S.comm

  val iterate_all :
    handle ->
    (int32 S.exp array -> S.comm) ->
    S.comm

  val insert : handle -> int32 S.exp array -> S.comm

  val ifmember_pat :
    handle ->
    pat:[`Wild | `Fixed of int32 S.exp] array ->
    then_:S.comm ->
    else_:S.comm ->
    S.comm

  val ifmember :
    handle ->
    int32 S.exp array ->
    then_:S.comm ->
    else_:S.comm ->
    S.comm
end

module Make_Indexed_Table
    (S : Idealised_algol.Syntax.S)
    (A : sig
       val arity : int
       val indexes : int array array
     end)
    () : INDEXED_TABLE with module S = S =
struct

  module S = S

  let _ =
    (* FIXME: raise invalid_arg? *)
    assert (A.arity > 0);
    assert (Array.length A.indexes > 0);
    assert (Array.for_all (fun a -> Array.length a = A.arity) A.indexes)

  let arity = A.arity

  module Key = Codegen_inttuple.Make (S) (A) ()

  module BT =
    Idealised_algol.Btree.Make (S)
      (struct let min_children = 16l end)
      (Key)
      ()

  type handle = BT.handle array

  let declare ~name k =
    let rec loop i l =
      if i = Array.length A.indexes then
        k (Array.of_list (List.rev l))
      else
        BT.declare (fun v -> loop (i+1) (v::l))
    in
    loop 0 []

  let conv_pattern pat =
    pat
    |> Array.fold_lefti (fun l i -> function `Fixed _ -> i::l | `Wild -> l) []
    |> List.rev
    |> Array.of_list

  let is_prefix ar1 ar2 =
    if Array.length ar1 > Array.length ar2 then false
    else
      let rec loop i =
        if i = Array.length ar1 then true
        else if ar1.(i) <> ar2.(i) then false
        else loop (i+1)
      in
      loop 0

  let find_handle prefix handles =
    let rec loop i =
      if i = Array.length handles then
        failwith "Invalid search pattern"
      else if is_prefix prefix A.indexes.(i) then
        handles.(i), A.indexes.(i)
      else
        loop (i+1)
    in
    loop 0

  let invert perm =
    let inv = Array.make (Array.length perm) 0 in
    Array.iteri (fun i j -> inv.(j) <- i) perm;
    inv

  let get_fixed = function
    | `Fixed e -> e
    | `Wild    -> assert false

  let for_pattern h pat k =
    let prefix_pat   = conv_pattern pat in
    let handle, perm = find_handle prefix_pat h in
    let perm_inv     = invert perm in
    let minimum =
      Key.create
        (Array.init A.arity
           (fun i ->
              if i < Array.length prefix_pat then
                get_fixed (pat.(prefix_pat.(i)))
              else
                S.Int32.const 0l))
    in
    let maximum =
      Key.create
        (Array.init A.arity
           (fun i ->
              if i < Array.length prefix_pat then
                get_fixed (pat.(prefix_pat.(i)))
              else
                S.Int32.maximum))
    in
    k handle minimum maximum perm_inv

  let iterate h ~pat k =
    for_pattern h pat begin fun tree minimum maximum perm_inv ->
      BT.iterate_range
        minimum maximum tree
        begin fun key ->
          k (Array.init A.arity (fun i -> Key.get key perm_inv.(i)))
        end
    end

  let ifmember_pat h ~pat ~then_ ~else_ =
    for_pattern h pat begin fun tree minimum maximum _ ->
      BT.ifmember_range
        minimum maximum tree
        then_
        else_
    end

  let ifmember h key ~then_ ~else_ =
    let perm   = A.indexes.(0) in
    let handle = h.(0) in
    let key    = Key.create (Array.init A.arity (fun i -> key.(perm.(i)))) in
    BT.ifmember key handle then_ else_

  let iterate_all h k =
    let perm_inv = invert A.indexes.(0) in
    let handle = h.(0) in
    BT.iterate_all handle (fun key -> k (Array.init A.arity (fun i -> Key.get key perm_inv.(i))))

  let insert h exps =
    h
    |> Array.mapi (fun i ->
        BT.insert (Key.create (Array.map (fun j -> exps.(j)) A.indexes.(i))))
    |> let open! S in Array.fold_left (^^) empty

end

module Gen (IA : Idealised_algol.Syntax.S) () = struct

  let map_seq f =
    List.fold_left (fun code x -> IA.(^^) code (f x)) IA.empty

  let array_map_seq f =
    Array.fold_left (fun code x -> IA.(^^) code (f x)) IA.empty

  module BL = Idealised_algol.Block_list.Make (IA) ()

  module type INDEXED_TABLE = INDEXED_TABLE with module S = IA

  type value =
    | Plain   : BL.handle -> value
    | Indexed : (module INDEXED_TABLE with type handle = 'h) * 'h -> value

  module RelEnv = Map.Make (Syntax.RelVar)

  module AttrEnv = Map.Make (Syntax.Attr)

  let rec and_list = function
    | []    -> IA.Bool.true_
    | [e]   -> e
    | e::es -> IA.Bool.(&&) e (and_list es)

  let condition lenv exps = let open! IA.Int32 in function
    | (i, Syntax.Attr nm) -> exps.(i) == AttrEnv.find nm lenv
    | (i, Syntax.Lit j)   -> exps.(i) == const j

  let exp_of_scalar lenv = function
    | Syntax.Attr nm -> AttrEnv.find nm lenv
    | Syntax.Lit j   -> IA.Int32.const j

  let pattern_of_conditions arity lenv conditions =
    let pat = Array.make arity `Wild in
    List.iter
      (fun (i, scalar) -> pat.(i) <- `Fixed (exp_of_scalar lenv scalar))
      conditions;
    pat

  let print_strln s =
    begin%monoid.IA
      IA.print_str s;
      IA.print_newline
    end

  let print_exps exps =
    begin%monoid.IA
      for i = 0 to Array.length exps - 1 do
        if i > 0 then IA.print_str ",";
        IA.print_int exps.(i);
      done;
      IA.print_newline
    end

  let projections_to_lenv projections attrs lenv =
    List.fold_right
      (fun (i, nm) -> AttrEnv.add nm attrs.(i))
      projections
      lenv

  let if_conj conditions then_ =
    match conditions with
      | []         -> then_
      | conditions -> IA.ifthen (and_list conditions) ~then_

  let rec translate_expr expr env lenv k = match expr with
    | Syntax.Return { guard_relation=None; values } ->
       let vals = Array.of_list (List.map (exp_of_scalar lenv) values) in
       k vals

    | Syntax.Return { guard_relation=Some guard; values } ->
       begin
         match RelEnv.find guard env with
           | Plain _ ->
              failwith "plain relation used as a guard"
           | Indexed (m, handle) ->
              let module IT = (val m) in
              let vals = Array.of_list (List.map (exp_of_scalar lenv) values) in
              IT.ifmember handle vals ~then_:IA.empty ~else_:(k vals)
       end

    | Syntax.Select { relation; conditions; projections; cont } ->
       begin match RelEnv.find relation env with
         | Plain handle ->
            begin
              (* FIXME: emit a warning if conditions contains anything,
                 or if projections is empty. *)
              BL.iterate handle @@ fun attrs ->
              if_conj (List.map (condition lenv attrs) conditions)
                (let lenv = projections_to_lenv projections attrs lenv in
                 translate_expr cont env lenv k)
            end
         | Indexed (m, handle) ->
            begin
              let module IT = (val m) in
              match conditions, projections with
                | conditions, [] ->
                   let pat = pattern_of_conditions IT.arity lenv conditions in
                   IT.ifmember_pat handle ~pat
                     ~then_:(translate_expr cont env lenv k)
                     ~else_:IA.empty
                | [], projections ->
                   (IT.iterate_all handle @@ fun attrs ->
                    let lenv = projections_to_lenv projections attrs lenv in
                    translate_expr cont env lenv k)
                | conditions, projections ->
                   (let pat = pattern_of_conditions IT.arity lenv conditions in
                    IT.iterate handle ~pat @@ fun attrs ->
                    let lenv = projections_to_lenv projections attrs lenv in
                    translate_expr cont env lenv k)
            end
       end

  let rec translate_comm env = function
    | Syntax.WhileNotEmpty (vars, body) ->
       let check_empty nm =
         match RelEnv.find nm env with
           | Plain handle -> BL.is_empty handle
           | Indexed _    -> failwith "emptiness test on an indexed table"
       in
       IA.while_ (IA.Bool.not (and_list (List.map check_empty vars)))
         ~do_:(translate_comms env body)

    | Insert (relvar, expr) ->
       (translate_expr expr env AttrEnv.empty @@ fun vals ->
        match RelEnv.find relvar env with
          | Plain handle ->
             BL.insert handle vals
          | Indexed (m, handle) ->
             let module IT = (val m) in IT.insert handle vals)

    | Declare (vars, body) ->
       List.fold_right
         (fun (Syntax.{ident;arity} as varnm) k env ->
            BL.declare ~name:ident ~arity
              (fun handle -> k (RelEnv.add varnm (Plain handle) env)))
         vars
         (fun env -> translate_comms env body)
         env

    | Move { src; tgt } ->
       (match RelEnv.find src env, RelEnv.find tgt env with
         | Plain src, Plain tgt ->
            BL.move ~src ~tgt
         | _ ->
            failwith "Attempted move between indexed relations")

  and translate_comms env comms =
    map_seq (translate_comm env) comms

  let translate_idb_predicate indexes relvar k env =
    match List.assoc relvar indexes with
      | exception Not_found ->
         BL.declare
           ~name:relvar.Syntax.ident
           ~arity:relvar.Syntax.arity
           begin fun handle ->
             begin%monoid.IA
               k (RelEnv.add relvar (Plain handle) env);
               print_strln relvar.Syntax.ident;
               BL.iterate handle print_exps
             end
           end
      | indexes ->
         let module P = struct
           let arity = relvar.Syntax.arity
           let indexes = indexes
         end in
         let module IT = Make_Indexed_Table (IA) (P) () in
         let m = (module IT : INDEXED_TABLE with type handle = IT.handle) in
         IT.declare
           ~name:relvar.Syntax.ident
           begin fun handle ->
             begin%monoid.IA
               k (RelEnv.add relvar (Indexed (m, handle)) env);
               print_strln relvar.Syntax.ident;
               IT.iterate_all handle print_exps
             end
           end

  (* FIXME: do this properly, adding code to load the data from CSV files *)
  let translate_edb_predicate =
    translate_idb_predicate

  let translate_prog (Syntax.{ idb_relvars; edb_relvars; commands } as code) =
    let indexes = Indexes.indexes code in
    let prog =
      (fun env -> translate_comms env commands)
      |> List.fold_right (translate_idb_predicate indexes) idb_relvars
      |> List.fold_right (translate_edb_predicate indexes) edb_relvars
    in
    prog RelEnv.empty
end

let generator (type comm)
    (module IA : Idealised_algol.Syntax.S with type comm = comm)
    program =
  let module T = Gen (IA) () in
  T.translate_prog program

let translate program =
  let open! Idealised_algol in
  C.output { Syntax.generate = generator } program Format.std_formatter

let compile outname program =
  let open! Idealised_algol in
  C.compile outname { Syntax.generate = generator } program
