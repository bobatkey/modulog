(* Plan:
   1. Compute the indexes required for each idb and edb relvar
      - make a separate btree for each index
      - insertion inserts into all relevant btrees
      - iteration and search uses the appropriate index

   2. Code generated:

      1. Load all the EDB relvars
      2. Initialise all the IDB relvars
      3. Translate all the commands (see below)
      4. Output the intensional db to disk (all of it?)

   3. For each command:

      a) WhileNotEmpty: a while loop

      b) Insert (relvar, expr):
           i) translate 'Select' into an iteration over the structure
              (or an 'if' when there are no projections)
          ii) translate 'Return' into an insertion
              (with an 'if', if there is a guard)

      d) Move { src; tgt }: assume that they are both flat, and do src := tgt; tgt := null;

      e) Declare (relvars, body): initialise several new 'flat' relvars
*)

(* Environment provides access to:
   - for each IDB predicate:
     - an iterator (later, with search facilities)
     - an insertion command
     - a ismember predicate
   - for each temporary predicate:
     - an iterator
     - an insertion command
     - emptiness check
     - a move operation (between predicates)
*)

type ze = Ze
type 'n su = Su of 'n

type 'n is_nat =
  | Zero : ze is_nat
  | Succ : 'n is_nat -> 'n su is_nat

type nat = Nat : 'n is_nat -> nat [@ocaml.unboxed]

let rec nat_of_int = function
  | 0 ->
     Nat Zero
  | i when i > 0 ->
     let Nat n = nat_of_int (i-1) in
     Nat (Succ n)
  | _ ->
     invalid_arg "nat_of_int"

let rec int_of_nat : type n. n is_nat -> int =
  function
    | Zero   -> 0
    | Succ n -> 1 + int_of_nat n

type ('a,'n) vec =
  | Nil  : ('a,ze) vec
  | Cons : 'a * ('a,'n) vec -> ('a,'n su) vec

let rec list_of_vec : type a n. (a, n) vec -> a list =
  function
    | Nil -> []
    | Cons (a, v) -> a :: list_of_vec v

let rec vec_of_list : type a n. n is_nat -> a list -> (a, n) vec =
  fun n xs -> match n, xs with
    | Zero,   []    -> Nil
    | Succ n, x::xs -> Cons (x, vec_of_list n xs)
    | _ ->
       invalid_arg "vec_of_list"

let rec init_vec : type a n. n is_nat -> a -> (a, n) vec =
  fun n x -> match n with
    | Zero   -> Nil
    | Succ n -> Cons (x, init_vec n x)

module type BLOCK_LIST = sig
  module IA : Idealised_algol.Syntax.S

  type 'n handle

  val arity : 'n handle -> 'n is_nat

  val declare : name:string -> arity:'n is_nat -> ('n handle -> IA.comm) -> IA.comm

  (* FIXME: these ought to have the same arity, but this is easier for
     now *)
  val move : src:'n handle -> tgt:'n2 handle -> IA.comm

  val insert : 'n handle -> (int IA.exp, 'n) vec -> IA.comm

  val iterate : 'n handle -> ((int IA.exp, 'n) vec -> IA.comm) -> IA.comm

  val is_empty : 'n handle -> bool IA.exp

  val print_out : string -> 'n handle -> IA.comm
end

module type B_TREE = sig
  module S : Idealised_algol.Syntax.S

  type 'n handle

  val declare : 'n is_nat -> ('n handle -> S.comm) -> S.comm

  val insert : 'n handle -> (int S.exp, 'n) vec -> S.comm

  val iterate_range :
    'n handle ->
    start:(int S.exp, 'n) vec ->
    stop:(int S.exp, 'n) vec ->
    ((int S.exp, 'n) vec -> S.comm) ->
    S.comm
end

module type INDEXED_TABLE = sig
  module IA : Idealised_algol.Syntax.S

  type 'n handle

  (* implement this as a collection of BTrees, one for each index
     pattern.

     'iterate' is given a pattern: find the index that has all the
     fixed parts of this pattern as a prefix. 'ifmember' does the
     same.

     'insert' updates all the indexes.  *)

  val arity : 'n handle -> 'n is_nat

  (** The [indexes] argument ought to be a list of permutations of the
      allowed slots. There will be one B-tree for each index. *)
  val declare :
    name:string ->
    arity:'n is_nat ->
    indexes:int array list -> (* ('n fin,'n) vec list *)
    ('n handle -> IA.comm) ->
    IA.comm

  (** Given a pattern:
      - find the index suitable for this query: it will contain all the fixed parts of the pattern as a prefix.
      - reverse the index to correctly extract the attributes in the right order
  *)
  val iterate :
    'n handle ->
    pat:([`Wild | `Fixed of int IA.exp], 'n) vec ->
    ((int IA.exp, 'n) vec -> IA.comm) ->
    IA.comm

  (** Translate to insertion commands for each of the underlying
      B-trees. *)
  val insert :
    'n handle ->
    (int IA.exp, 'n) vec ->
    IA.comm

  val ifmember :
    'n handle ->
    pat:([`Wild | `Fixed of int IA.exp], 'n) vec ->
    IA.comm ->
    IA.comm

  (** All of the B-trees ought to be the same size, so we just check
     if the first one is empty. *)
  val is_empty : 'n handle -> bool IA.exp
end

module BL_Make (IA : Idealised_algol.Syntax.S) () : BLOCK_LIST with module IA = IA =
struct
  module IA = IA

  open IA

  let block_size = 16l

  type node
  let node : node structure typ = structure "list_node"
  let occupied = field node "occupied" int
  let next     = field node "next" (ptr node)
  let values   = field node "values" (array int 1l)
  let ()       = seal node

  type 'n handle =
    { arity : 'n is_nat
    ; var   : node structure ptr var
    }

  let arity handle =
    handle.arity

  let is_empty { var } =
    var =*= null

  let rec write : type n. int array var -> int exp -> (int exp, n) vec -> comm =
    fun arr offset vals -> match vals with
      | Nil ->
         empty
      | Cons (v, vs) ->
         (arr#@(offset) := v) ^^ write arr (offset + const 1l) vs

  let move ~src:{var=src} ~tgt:{var=tgt} =
    begin%monoid
      tgt := src;
      src := null
    end

  let new_block v ~arity ~vals ~next:nxt =
    begin%monoid
      let length = Int32.mul (Int32.of_int (int_of_nat arity)) block_size in
      malloc_ext v node (const length) int;
      v#->occupied := const 1l;
      v#->next := nxt;
      write (v#->values) (const 0l) vals
    end

  let insert { var; arity } vals =
    if_ (var =*= null)
      ~then_:begin%monoid
        new_block var ~arity ~vals ~next:null
      end
      ~else_:begin%monoid
        if_ (var#->occupied == const block_size)
          ~then_:begin%monoid
            declare ~name:"new_head" (ptr node) @@ fun new_head ->
            begin%monoid
              new_block new_head ~arity ~vals ~next:var;
              var := new_head
            end
          end
          ~else_:begin%monoid
            write (var#->values)
              (var#->occupied * const (Int32.of_int (int_of_nat arity))) vals;
            var#->occupied := var#->occupied + const 1l
          end
      end

  let rec read : type n. int array var -> int exp -> n is_nat -> (int exp, n) vec =
    fun arr idx n -> match n with
      | Zero   -> Nil
      | Succ n -> Cons (arr#@idx, read arr (idx + const 1l) n)

  let iterate { var; arity } body =
    declare ~name:"block_iterator" (ptr node) @@ fun node ->
    begin%monoid
      node := var;

      while_ (node =!*= null) ~do_:begin%monoid
        declare ~name:"i" int @@ fun i -> begin%monoid
          i := const 0l;

          while_ (i < node#->occupied) ~do_:begin%monoid
            body (read
                    (node#->values)
                    (i * const (Int32.of_int (int_of_nat arity)))
                    arity);

            i := i + const 1l;
          end;

          node := node#->next
        end
      end
    end

  let print_out nm hndl =
    begin%monoid
      print_str nm;
      print_newline;
      iterate hndl @@ fun attrs -> begin%monoid
        print_str "   ";
        attrs |> list_of_vec |> List.map (fun i -> begin%monoid
              print_int i; print_str " "
            end) |> List.fold_left (^^) empty;
        print_newline
      end
    end

  let declare ~name ~arity k =
    declare ~name (ptr node) @@ fun var ->
    begin%monoid
      var := null;
      k { arity; var };
      declare ~name:"ahead" (ptr node) @@ fun ahead ->
      while_ (var =!*= null) ~do_:begin%monoid
        ahead := var#->next;
        free var;
        var := ahead
      end
    end

end

module Gen (IA : Idealised_algol.Syntax.S) () = struct

  module BL = BL_Make (IA) ()

  open Syntax

  module Env = struct
    include Map.Make
        (struct
          type t = Syntax.relvar
          let compare = Pervasives.compare
        end)

    type value =
      | Plain   : 'n BL.handle -> value
      (* | Indexed : 'n Indx.handle -> value *)

    let add nm handle env =
      add nm (Plain handle) env
  end

  module LEnv = Map.Make (String)

  let rec and_list = function
    | [] -> IA.true_
    | [e] -> e
    | e::es -> IA.(&&) e (and_list es)

  let condition lenv exps = function
    | (i, Syntax.Attr nm) -> IA.(List.nth exps i == LEnv.find nm lenv)
    | (i, Syntax.Lit j)   -> IA.(List.nth exps i == const j)

  let rec translate_expr expr env lenv k = match expr with
    | Return { guard_relation; values } ->
       let vals =
         List.map (function Lit i   -> IA.const i
                          | Attr nm -> LEnv.find nm lenv)
           values
       in
       (* FIXME: check to see whether vals is in 'guard_relation' *)
       k vals

    | Select { relation; conditions=[]; projections; cont } ->
       (match Env.find relation env with
         | Env.Plain handle ->
            BL.iterate handle @@ fun attrs ->
            let attrs = list_of_vec attrs in
            let lenv =
              List.fold_right
                (fun (i, nm) -> LEnv.add nm (List.nth attrs i))
                projections
                lenv
            in
            translate_expr cont env lenv k
         (*| Env.Indexed handle ->
            let pat = init_vec (Indx.arity handle) `Wild in
            Indx.iterate handle ~pat @@ fun attrs ->
            let attrs = list_of_vec attrs in
            let lenv =
              List.fold_right
                (fun (i, nm) -> LEnv.add nm (List.nth attrs i))
                projections
                lenv
            in
            translate_expr cont env lenv k*))

    | Select { relation; conditions; projections; cont } ->
       (match Env.find relation env with
         | Env.Plain handle ->
            BL.iterate handle @@ fun attrs ->
            let attrs = list_of_vec attrs in
            IA.ifthen (and_list (List.map (condition lenv attrs) conditions))
              ~then_:begin
                let lenv =
                  List.fold_right
                    (fun (i, nm) -> LEnv.add nm (List.nth attrs i))
                    projections
                    lenv
                in
                translate_expr cont env lenv k
              end
         (*| Env.Indexed handle ->
            let pat = assert false in
            Indx.iterate handle ~pat @@ fun attrs ->
            let attrs = list_of_vec attrs in
            let lenv =
              List.fold_right
                (fun (i, nm) -> LEnv.add nm (List.nth attrs i))
                projections
                lenv
            in
            translate_expr cont env lenv k*) )

  let rec translate_comm comm env =
    match comm with
      | WhileNotEmpty (vars, body) ->
         let check_empty nm =
           match Env.find nm env with
             | Env.Plain handle -> BL.is_empty handle
             (*| Env.Indexed handle -> Indx.is_empty handle*)
         in
         IA.while_ (IA.not (and_list (List.map check_empty vars)))
           ~do_:(translate_comms body env)

      | Insert (relvar, expr) ->
         (match Env.find relvar env with
           | Env.Plain handle ->
              translate_expr expr env LEnv.empty @@ fun vals ->
              BL.insert handle (vec_of_list (BL.arity handle) vals)
              (*| Env.Indexed handle ->
                 translate_expr expr env LEnv.empty @@ fun vals ->
                 Indx.insert handle (vec_of_list (Indx.arity handle) vals)*))

      | Declare (vars, body) ->
         List.fold_right
           (fun varnm k env ->
              let Nat n = nat_of_int varnm.Syntax.arity in
              BL.declare
                ~name:varnm.Syntax.ident
                ~arity:n
                (fun handle -> k (Env.add varnm handle env)))
           vars
           (translate_comms body)
           env

      | Move { src; tgt } ->
         (match Env.find src env, Env.find tgt env with
           | Env.Plain src, Env.Plain tgt ->
              BL.move ~src ~tgt
              (*| _ ->
                 failwith "Attempted move between indexed relations" *))

  and translate_comms comms env =
    List.fold_left
      (fun code comm -> IA.(code ^^ translate_comm comm env))
      IA.empty
      comms

  let translate_prog { idb_relvars; commands } =
    List.fold_right
      (fun relvar k env ->
         let Nat arity = nat_of_int relvar.Syntax.arity in
         BL.declare
           ~name:relvar.Syntax.ident
           ~arity
         @@ fun handle -> begin%monoid.IA
           k (Env.add relvar handle env);
           BL.print_out relvar.Syntax.ident handle
         end)
      idb_relvars
      (translate_comms commands)
      Env.empty
end

module C          = Idealised_algol.C.C ()
module Translator = Gen (C) ()

let translate program =
  let stmts = C.gen (Translator.translate_prog program) in
  Format.set_margin 300;
  Format.set_max_indent 280;
  Idealised_algol.C.PP.pp_stmts Format.std_formatter stmts
