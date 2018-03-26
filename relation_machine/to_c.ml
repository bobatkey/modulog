module IntTupleKey (S : Idealised_algol.Syntax.S) (W : sig val width : int end) ()
  : sig
    include Idealised_algol.Btree.KEY with module S = S

    val create : int S.exp array -> t S.exp

    val get    : t S.exp -> int -> int S.exp
  end =
struct
  module S = S

  type key
  type t = key S.structure
  let t : t S.typ = S.structure "key"
  let val_fields =
    Array.init W.width (fun i -> S.field t (Printf.sprintf "x%d" i) S.int)
  let () = S.seal t

  let create exps =
    S.struct_const t (Array.to_list (Array.map (fun e -> S.Exp e) exps))

  let get x i =
    let open! S in
    x#.val_fields.(i)

  let eq x y =
    let rec loop i acc =
      if i = W.width then
        acc
      else
        loop (i+1)
          (let open! S in
           acc && x#.val_fields.(i) == y#.val_fields.(i))
    in
    loop 1
      (let open! S in x#.val_fields.(0) == y#.val_fields.(0))

  let le x y =
    let rec loop i =
      if i = W.width - 1 then
        let open! S in
        x#.val_fields.(i) <= y#.val_fields.(i)
      else
        let e = loop (i+1) in
        let open! S in
        x#.val_fields.(i) < y#.val_fields.(i)
        || (x#.val_fields.(i) == y#.val_fields.(i) && e)
    in
    loop 0

  let lt x y =
    let rec loop i =
      if i = W.width - 1 then
        let open! S in
        x#.val_fields.(i) < y#.val_fields.(i)
      else
        let e = loop (i+1) in
        let open! S in
        x#.val_fields.(i) < y#.val_fields.(i)
        || (x#.val_fields.(i) == y#.val_fields.(i) && e)
    in
    loop 0
end

module type INDEXED_TABLE = sig
  module S : Idealised_algol.Syntax.S

  type handle

  val arity : int

  val declare : name:string -> (handle -> S.comm) -> S.comm

  val iterate :
    handle ->
    pat:[`Wild | `Fixed of int S.exp] array ->
    (int S.exp array -> S.comm) ->
    S.comm

  val iterate_all :
    handle ->
    (int S.exp array -> S.comm) ->
    S.comm

  val insert : handle -> int S.exp array -> S.comm

  val ifmember_pat :
    handle ->
    pat:[`Wild | `Fixed of int S.exp] array ->
    then_:S.comm ->
    else_:S.comm ->
    S.comm

  val ifmember :
    handle ->
    pat:int S.exp array ->
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
    assert (A.arity > 0);
    assert (Array.length A.indexes > 0);
    assert (Array.for_all (fun a -> Array.length a = A.arity) A.indexes)

  let arity =
    A.arity

  module Key =
    IntTupleKey (S)
      (struct let width = A.arity end)
      ()

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
        BT.with_tree (fun v -> loop (i+1) (v::l))
    in
    loop 0 []

  let conv_pattern pat =
    Array.fold_left
      (fun (i, l) p ->
         (i+1,
          match p with
            | `Fixed _ -> i::l
            | `Wild    -> l))
      (0,[])
      pat
    |> snd
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
                S.const 0l))
    in
    let maximum =
      Key.create
        (Array.init A.arity
           (fun i ->
              if i < Array.length prefix_pat then
                get_fixed (pat.(prefix_pat.(i)))
              else
                S.int_max))
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

  let ifmember h ~pat ~then_ ~else_ =
    let perm = A.indexes.(0) in
    let handle = h.(0) in
    let key = Key.create (Array.init A.arity (fun i -> pat.(perm.(i)))) in
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

  module BL = Idealised_algol.Block_list.Make (IA) ()

  module type INDEXED_TABLE = INDEXED_TABLE with module S = IA

  type value =
    | Plain   : BL.handle -> value
    | Indexed : (module INDEXED_TABLE with type handle = 'h) * 'h -> value

  module Env =
    Map.Make
      (struct
        type t = Syntax.relvar
        let compare = Pervasives.compare
      end)

  module LEnv = Map.Make (String)

  let rec and_list = function
    | []    -> IA.true_
    | [e]   -> e
    | e::es -> IA.(&&) e (and_list es)

  let condition lenv exps = let open! IA in function
    | (i, Syntax.Attr nm) -> exps.(i) == LEnv.find nm lenv
    | (i, Syntax.Lit j)   -> exps.(i) == const j

  let exp_of_scalar lenv = function
    | Syntax.Attr nm -> LEnv.find nm lenv
    | Syntax.Lit j   -> IA.const j

  let pattern_of_conditions arity lenv conditions =
    let pat = Array.make arity `Wild in
    List.iter
      (fun (i, scalar) -> pat.(i) <- `Fixed (exp_of_scalar lenv scalar))
      conditions;
    pat

  let print_exps exps =
    let open! IA in
    begin%monoid
      IA.print_str "   ";
      (exps
       |> Array.map (fun e -> print_int e ^^ print_str " ")
       |> Array.fold_left (^^) empty);
      IA.print_newline
    end

  let projections_to_lenv projections attrs lenv =
    List.fold_right
      (fun (i, nm) -> LEnv.add nm attrs.(i))
      projections
      lenv

  let rec translate_expr expr env lenv k = match expr with
    | Syntax.Return { guard_relation=None; values } ->
       let vals = Array.of_list (List.map (exp_of_scalar lenv) values) in
       k vals

    | Syntax.Return { guard_relation=Some guard; values } ->
       (let vals = Array.of_list (List.map (exp_of_scalar lenv) values) in
        match Env.find guard env with
          | Plain _ ->
             failwith "plain relation used as a guard"
          | Indexed (m, handle) ->
             let module IT = (val m) in
             IT.ifmember handle
               ~pat:vals
               ~then_:begin%monoid.IA
                 (* IA.print_str "Not inserting:"; print_exps vals *)
               end
               ~else_:(k vals))

    | Syntax.Select { relation; conditions; projections; cont } ->
       (match Env.find relation env with
         | Plain handle ->
            (BL.iterate handle @@ fun attrs ->
             let body = begin%monoid.IA
               let lenv = projections_to_lenv projections attrs lenv in
               (* IA.print_str (relation.Syntax.ident ^ ":"); print_exps attrs; *)
               translate_expr cont env lenv k
               end
             in
             match conditions with
               | [] -> body
               | _  ->
                  IA.ifthen (and_list (List.map (condition lenv attrs) conditions))
                    ~then_:body)
         | Indexed (m, handle) ->
            (let module IT = (val m) in
             let pat = pattern_of_conditions IT.arity lenv conditions in
             match projections with
               | [] ->
                  IT.ifmember_pat handle ~pat
                    ~then_:(translate_expr cont env lenv k)
                    ~else_:IA.empty
               | projections ->
                  IT.iterate handle ~pat @@ fun attrs -> begin%monoid.IA
                    (* IA.print_str (relation.Syntax.ident ^ ":"); print_exps attrs; *)
                    let lenv = projections_to_lenv projections attrs lenv in
                    translate_expr cont env lenv k
                  end))

  let print_strln s =
    let open! IA in
    begin%monoid
      print_str s;
      print_newline
    end

  let rec translate_comm comm env =
    match comm with
      | Syntax.WhileNotEmpty (vars, body) ->
         let check_empty nm =
           match Env.find nm env with
             | Plain handle -> BL.is_empty handle
             | Indexed _    -> failwith "emptiness test on an indexed table"
         in
         IA.while_
           (IA.not (and_list (List.map check_empty vars)))
           ~do_:begin%monoid.IA
             (*print_strln "looping"; *)
             translate_comms body env
           end

      | Insert (relvar, expr) ->
         begin%monoid.IA
           (*IA.print_str ("Inserting into: " ^ relvar.Syntax.ident);
             IA.print_newline;*)
           translate_expr expr env LEnv.empty @@ fun vals -> begin%monoid.IA
             (* print_exps vals; *)
             match Env.find relvar env with
               | Plain handle ->
                  BL.insert handle vals
               | Indexed (m, handle) ->
                  let module IT = (val m) in
                  IT.insert handle vals
           end
         end

      | Declare (vars, body) ->
         List.fold_right
           (fun varnm k env ->
              BL.declare
                ~name:varnm.Syntax.ident
                ~arity:varnm.Syntax.arity
                (fun handle -> k (Env.add varnm (Plain handle) env)))
           vars
           (translate_comms body)
           env

      | Move { src; tgt } ->
         (match Env.find src env, Env.find tgt env with
           | Plain src, Plain tgt ->
              BL.move ~src ~tgt
           | _ ->
              failwith "Attempted move between indexed relations")

  and translate_comms comms env =
    let open! IA in
    List.fold_left
      (fun code comm -> code ^^ translate_comm comm env)
      empty
      comms

  let translate_prog (Syntax.{ idb_relvars; commands } as code) =
    let indexes = Indexes.indexes code in
    List.fold_right
      (fun relvar k env ->
         let module P = struct
           let arity = relvar.Syntax.arity
           let indexes =
             try Array.of_list (List.assoc relvar indexes)
             with Not_found -> [|Array.init arity (fun i -> i)|]
         end in
         let module IT = Make_Indexed_Table (IA) (P) () in
         IT.declare
           ~name:relvar.Syntax.ident
           (fun handle ->
              let m = (module IT : INDEXED_TABLE with type handle = IT.handle) in
              let value = Indexed (m, handle) in
              begin%monoid.IA
                k (Env.add relvar value env);
                IA.print_str relvar.Syntax.ident;
                IA.print_newline;
                IT.iterate_all handle print_exps
              end))
      idb_relvars
      (translate_comms commands)
      Env.empty
end

module C          = Idealised_algol.C.C ()
module Translator = Gen (C) ()

let translate program =
  let comm    = Translator.translate_prog program in
  let stmts   = C.gen comm in
  let structs = C.struct_decls () in
  Format.set_margin 300;
  Format.set_max_indent 280;
  Format.open_vbox 0;
  Format.printf "#include <stdlib.h>@,";
  Format.printf "#include <stdio.h>@,";
  Format.printf "#include <stdbool.h>@,";
  Format.printf "#include <limits.h>@,@,";
  structs |> List.iter begin fun struct_decl ->
    Idealised_algol.C.PP.pp_struct_decl Format.std_formatter struct_decl;
    Format.print_cut ()
  end;
  Format.print_cut ();
  Format.printf "@[<v 4>int main(int argc, char **argv) {@ ";
  Idealised_algol.C.PP.pp_stmts Format.std_formatter stmts;
  Format.printf "@]@,}@,";
  Format.close_box ()
