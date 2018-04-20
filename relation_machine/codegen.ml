module Buf (IA : Idealised_algol.Syntax.S) = struct
  module type S = Codegen_double_buf.S with module Syn = IA

  type t = Buf : (module S with type handle = 'h) * 'h -> t

  let declare arity k =
    let module Config = struct let arity = arity end in
    let module B = Codegen_double_buf.Make (IA) (Config) () in
    B.declare (fun h -> k (Buf ((module B), h)))

  let insert (Buf (m, h)) vals =
    let module B = (val m) in
    B.insert h vals

  let iterate_all (Buf (m, h)) k =
    let module B = (val m) in
    B.iterate_all h k

  let swap (Buf (m, h)) =
    let module B = (val m) in
    B.swap h

  let is_empty (Buf (m, h)) =
    let module B = (val m) in
    B.is_empty h
end

module Gen (IA : Idealised_algol.Syntax.S) () = struct

  let map_seq f =
    List.fold_left (fun code x -> IA.(^^) code (f x)) IA.empty

  module Buf = Buf (IA)
  
  (* FIXME: do the same thing for Tables as for Buffers *)
  module type INDEXED_TABLE = Codegen_indexed_table.S with module S = IA

  type value =
    | Buffer : Buf.t -> value
    | Table  : (module INDEXED_TABLE with type handle = 'h) * 'h -> value

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
    let open! IA.Stdio in
    printf stdout (lit s @@ lit "\n" @@ stop)

  let print_exps exps =
    let open! IA.Stdio in
    begin%monoid.IA
      for i = 0 to Array.length exps - 1 do
        if i > 0 then printf stdout (lit "," @@ stop);
        printf stdout (int32 @@ stop) exps.(i)
      done;
      printf stdout (lit "\n" @@ stop)
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
    | Syntax.Return { values } ->
       k (Array.map (exp_of_scalar lenv) values)

    | Syntax.Guard_NotIn { relation; values; cont } ->
       begin
         match RelEnv.find relation env with
           | Buffer _ ->
              (* FIXME: in some cases, it would make sense to check
                 the write end of the buffer for duplicates before
                 checking the relation itself. *)
              failwith "internal error: [codegen] buffer used as a guard"
           | Table (m, handle) ->
              let module IT = (val m) in
              let values = Array.map (exp_of_scalar lenv) values in
              IT.ifmember handle values
                ~then_:IA.empty
                ~else_:(translate_expr cont env lenv k)
       end

    | Syntax.Select { relation; conditions; projections; cont } ->
       begin match RelEnv.find relation env with
         | Buffer handle ->
            begin
              (* FIXME: emit a warning if conditions contains anything,
                 or if projections is empty. *)
              Buf.iterate_all handle @@ fun attrs ->
              if_conj (List.map (condition lenv attrs) conditions)
                (let lenv = projections_to_lenv projections attrs lenv in
                 translate_expr cont env lenv k)
            end
         | Table (m, handle) ->
            begin
              let module IT = (val m) in
              match conditions, projections with
                | conditions, [] ->
                   let pat = pattern_of_conditions IT.arity lenv conditions in
                   IT.ifmember_pat handle ~pat
                     ~then_:(translate_expr cont env lenv k)
                     ~else_:IA.empty
                | [], projections ->
                   IT.iterate_all handle ~do_:begin fun attrs ->
                     let lenv = projections_to_lenv projections attrs lenv in
                     translate_expr cont env lenv k
                   end
                | conditions, projections ->
                   let pat = pattern_of_conditions IT.arity lenv conditions in
                   IT.iterate handle ~pat ~do_:begin fun attrs ->
                     let lenv = projections_to_lenv projections attrs lenv in
                     translate_expr cont env lenv k
                   end
            end
       end

  let rec translate_comm env = function
    | Syntax.WhileNotEmpty (vars, body) ->
       let check_empty nm =
         match RelEnv.find nm env with
           | Buffer handle ->
              Buf.is_empty handle
           | Table _ ->
              failwith "internal error: [codegen] emptiness test on a table"
       in
       IA.while_ (IA.Bool.not (and_list (List.map check_empty vars)))
         ~do_:(translate_comms env body)

    | Insert (relvar, expr) ->
       (translate_expr expr env AttrEnv.empty @@ fun vals ->
        match RelEnv.find relvar env with
          | Buffer handle ->
             Buf.insert handle vals
          | Table (m, handle) ->
             (* FIXME: the membership check is not always required if
                doing a merge from a buffer. *)
             let module IT = (val m) in
             IT.ifmember handle vals
               ~then_:IA.empty
               ~else_:(IT.insert handle vals))

    | DeclareBuffers (vars, body) ->
       List.fold_right
         (fun (Syntax.{ident;arity} as varnm) k env ->
            Buf.declare (* ~name:ident ~ *)arity
              (fun handle -> k (RelEnv.add varnm (Buffer handle) env)))
         vars
         (fun env -> translate_comms env body)
         env

    | Swap relvar ->
       (match RelEnv.find relvar env with
         | Buffer buf ->
            Buf.swap buf
         | Table _ ->
            failwith "internal error: [codegen] attempt to swap a table")

  and translate_comms env comms =
    map_seq (translate_comm env) comms

  let translate_idb_predicate indexes relvar k env =
    let indexes =
      match List.assoc relvar indexes with
        | exception Not_found ->
           failwith "internal error: [codegen] relation missing an index"
        | indexes ->
           indexes
    in
    let module P = struct
      let arity = relvar.Syntax.arity
      let indexes = indexes
    end in
    let module IT = Codegen_indexed_table.Make (IA) (P) () in
    IT.declare
      ~name:relvar.Syntax.ident
      begin fun handle ->
        begin%monoid.IA
          k (RelEnv.add relvar (Table ((module IT), handle)) env);
          (* FIXME: dump to a CSV file (if this relation is marked as
             'output') *)
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
