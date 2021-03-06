(* TODO
   - Improve name generation
   - Wider range of integer types
*)

type 'a ocaml_array = 'a array



type _ ptr = Ptr
type _ array = Array
type _ structure = Structure
type _ typedef = Typedef

type 'a typ =
  | Void    : unit typ
  | Int32   : int32 typ
  | Bool    : bool typ
  | Pointer : 'a typ -> 'a ptr typ
  | Array   : 'a typ * int32 -> 'a array typ
  | Struct  : string -> 'a structure typ
  | Typedef : string -> 'a typedef typ

type c_type =
  | Some_type : 'a typ -> c_type

type structure_field =
  | StructField : { fld_name : string; fld_type : 'a typ } -> structure_field

type structure_desc =
  { name   : string
  ; fields : structure_field list
  }

module AST = struct

  type binop =
    | Plus
    | Sub
    | Mult
    | Div
    | Lt
    | Le
    | Eq
    | Ne
    | Ge
    | Gt
    | LAnd
    | LOr

  type unop =
    | Neg
    | LNot

  type exp =
    | Var       of string
    | Const     of string
    | Null
    | BoolLit   of bool
    | IntLit    of int32
    | Int32Max
    | StrLit    of string
    | StructLit of string * exp ocaml_array
    | Binop     of exp * binop * exp
    | Unop      of unop * exp
    | Deref     of exp
    | Field     of exp * string
    | Idx       of exp * exp
    | AddrOf    of exp
    | ECall     of string * exp list

  type stmt =
    | Assign  of exp * exp
    | Malloc  : exp * 'a typ * (exp * 'b typ) option -> stmt
    | Free    of exp
    | If      of exp * stmt * stmt option
    | While   of exp * stmt
    | Break
    | Block   of block_stmt list
    | Return  of exp
    | Call    of string * exp list

  and block_stmt =
    | Declaration : 'a typ * string * exp option -> block_stmt
    | Statement   : stmt                         -> block_stmt

  type fundecl =
    { return_type : c_type
    ; name        : string
    ; arg_decls   : (c_type * string) list
    ; body        : block_stmt list
    }
end

module PP = struct
  let rec pp_typ :
    type a. (bool -> Format.formatter -> unit) -> a typ Fmt.t =
    fun f fmt -> function
      | Void ->
         Format.fprintf fmt "void%t" (f true)
      | Int32 ->
         Format.fprintf fmt "int32_t%t" (f true)
      | Bool ->
         Format.fprintf fmt "bool%t" (f true)
      | Struct nm ->
         Format.fprintf fmt "struct %s%t" nm (f true)
      | Pointer typ ->
         pp_typ
           (fun top fmt ->
              if top then Format.fprintf fmt "*%t" (f true)
              else Format.fprintf fmt "(*%t)" (f true))
           fmt
           typ
      | Array (typ,n) ->
         pp_typ
           (fun top fmt -> Format.fprintf fmt "%t[%ld]" (f false) n)
           fmt
           typ
      | Typedef nm ->
         Format.fprintf fmt "%s%t" nm (f true)

  let pp_decl fmt (typ, ident) =
    pp_typ
      (fun l fmt -> Format.fprintf fmt " %s" ident)
      fmt
      typ

  let pp_struct_decl fmt { name; fields } =
    Format.fprintf fmt "@[<v>@[<v 4>struct %s {@ " name;
    let rec loop = function
      | [] ->
         ()
      | [StructField { fld_name; fld_type }] ->
         pp_decl fmt (fld_type, fld_name);
         Format.pp_print_string fmt ";"
      | StructField { fld_name; fld_type } :: fields ->
         pp_decl fmt (fld_type, fld_name);
         Format.pp_print_string fmt ";";
         Format.pp_print_cut fmt ();
         loop fields
    in
    loop fields;
    Format.fprintf fmt "@]@,};@]"

  let pp_typename fmt typ =
    pp_typ (fun l fmt -> ()) fmt typ

  open AST

  let prec_of_binop = function
    | Mult | Div        -> 5
    | Plus | Sub        -> 6
    | Lt | Le | Gt | Ge -> 8
    | Eq | Ne           -> 9
    | LAnd              -> 13
    | LOr               -> 14

  let str_of_binop = function
    | Mult -> "*"
    | Div  -> "/"
    | Plus -> "+"
    | Sub  -> "-"
    | Lt   -> "<"
    | Le   -> "<="
    | Ge   -> ">="
    | Gt   -> ">"
    | Eq   -> "=="
    | Ne   -> "!="
    | LAnd -> "&&"
    | LOr  -> "||"

  let rec pp_expr prec fmt = function
    | Binop (e1, (LAnd | LOr as op), e2) ->
       (* Special case these because GCC complains about parens in
          mixed &&, || expressions *)
       let level = prec_of_binop op in
       Format.fprintf fmt
         "(%a@ %s %a)"
         (pp_expr (level-1)) e1
         (str_of_binop op)
         (pp_expr level) e2
    | Binop (e1, op, e2) ->
       let level = prec_of_binop op in
       let s     = str_of_binop op in
       Format.fprintf fmt
         (if level > prec then "(%a %s@ %a)" else "%a %s@ %a")
         (pp_expr (level-1)) e1
         s
         (pp_expr level) e2
    | IntLit i ->
       Format.fprintf fmt "%ld" i
    | Int32Max ->
       Format.pp_print_string fmt "INT32_MAX"
    | BoolLit b ->
       Format.fprintf fmt "%b" b
    | StructLit (name, exps) ->
       Format.fprintf fmt
         "(struct %s){ @[<hv>%a@] }"
         name
         Fmt.(array ~sep:(Fmt.always ",@ ") (pp_expr 20)) exps
    | Var vnm ->
       Format.pp_print_string fmt vnm
    | Field (Deref expr, fnm) ->
       Format.fprintf fmt "%a->%s" (pp_expr 1) expr fnm
    | Field (expr, fnm) ->
       Format.fprintf fmt "%a.%s" (pp_expr 1) expr fnm
    | Idx (expr, iexpr) ->
       Format.fprintf fmt "%a[@[<hv>%a@]]" (pp_expr 1) expr (pp_expr 19) iexpr
    | Deref expr ->
       if prec < 3 then
         Format.fprintf fmt "(*%a)" (pp_expr 2) expr
       else
         Format.fprintf fmt "*%a" (pp_expr 2) expr
    | AddrOf expr ->
       if prec < 3 then
         Format.fprintf fmt "(&%a)" (pp_expr 2) expr
       else
         Format.fprintf fmt "&%a" (pp_expr 2) expr
    | Unop (Neg, expr) ->
       if prec < 3 then
         Format.fprintf fmt "(-%a)" (pp_expr 2) expr
       else
         Format.fprintf fmt "-%a" (pp_expr 2) expr
    | Unop (LNot, expr) ->
       if prec < 3 then
         Format.fprintf fmt "(!%a)" (pp_expr 2) expr
       else
         Format.fprintf fmt "!%a" (pp_expr 2) expr
    | Null ->
       Format.pp_print_string fmt "NULL"
    | Const s ->
       Format.pp_print_string fmt s
    | StrLit s ->
       (* FIXME: not sure this is the right escaping strategy *)
       Format.fprintf fmt "%S" s
    | ECall (nm, exps) ->
       Format.fprintf fmt "@[<hov>%s@,(@[<hv>%a)@]@]"
         nm
         Fmt.(list ~sep:(Fmt.always ",@ ") (pp_expr 20)) exps

  let pp_expr fmt e =
    Format.fprintf fmt "@[<hv>%a@]" (pp_expr 20) e

  let rec pp_stmts fmt stmts =
    Format.fprintf fmt "@[<v>";
    let rec loop previous stmts =
      match stmts with
        | Statement stmt :: stmts ->
           (match previous with
             | `Start ->
                pp_stmt fmt stmt
             | `Decl ->
                Format.pp_print_cut fmt ();
                Format.pp_print_cut fmt ();
                pp_stmt fmt stmt
             | `Stmt ->
                Format.pp_print_cut fmt ();
                pp_stmt fmt stmt);
           loop `Stmt stmts
        | Declaration (typ, ident, None) :: stmts ->
           (match previous with
             | `Start ->
                pp_decl fmt (typ, ident);
                Format.pp_print_string fmt ";"
             | `Decl ->
                Format.pp_print_cut fmt ();
                pp_decl fmt (typ, ident);
                Format.pp_print_string fmt ";"
             | `Stmt ->
                Format.pp_print_cut fmt ();
                Format.pp_print_cut fmt ();
                pp_decl fmt (typ, ident);
                Format.pp_print_string fmt ";");
           loop `Decl stmts
        | Declaration (typ, ident, Some init) :: stmts ->
           (match previous with
             | `Start ->
                Format.fprintf fmt "%a = %a;"
                  pp_decl (typ, ident)
                  pp_expr init
             | `Decl ->
                Format.fprintf fmt "@,%a = %a;"
                  pp_decl (typ, ident)
                  pp_expr init
             | `Stmt ->
                Format.fprintf fmt "@,@,%a = %a;"
                  pp_decl (typ, ident)
                  pp_expr init);
           loop `Decl stmts
        | [] ->
           ()
    in
    loop `Start stmts;
    Format.fprintf fmt "@]"

  and pp_stmt fmt = function
    | Assign (l_value, expr) ->
       Format.fprintf fmt "@[<hv 2>%a =@ %a@];"
         pp_expr l_value
         pp_expr expr
    | While (expr, Block stmts) ->
       Format.fprintf fmt "@[<v 4>while (@[<hv>%a@]) {@ %a@]@,}"
         pp_expr expr
         pp_stmts stmts
    | While (expr, stmt) ->
       Format.fprintf fmt "@[<v 4>while (@[<hv>%a@]) {@ %a@]@,}"
         pp_expr expr
         pp_stmt stmt
    | If (expr, Block then_stmts, None) ->
       Format.fprintf fmt "@[<v 4>if (@[<hv>%a@]) {@ %a@]@,}"
         pp_expr expr
         pp_stmts then_stmts
    | If (expr, then_stmt, None) ->
       Format.fprintf fmt "@[<v 4>if (@[<hv>%a@]) {@ %a@]@,}"
         pp_expr expr
         pp_stmt then_stmt
    | If (expr, Block then_stmt, Some (Block else_stmt)) ->
       Format.fprintf fmt
         "@[<v 4>if (@[<hv>%a@]) {@ %a@]@,@[<v 4>} else {@ %a@]@,}"
         pp_expr expr
         pp_stmts then_stmt
         pp_stmts else_stmt
    | If (expr, Block then_stmt, Some else_stmt) ->
       Format.fprintf fmt
         "@[<v 4>if (@[<hv>%a@]) {@ %a@]@,@[<v 4>} else {@ %a@]@,}"
         pp_expr expr
         pp_stmts then_stmt
         pp_stmt else_stmt
    | If (expr, then_stmt, Some (Block else_stmt)) ->
       Format.fprintf fmt
         "@[<v 4>if (@[<hv>%a@]) {@ %a@]@,@[<v 4>} else {@ %a@]@,}"
         pp_expr expr
         pp_stmt then_stmt
         pp_stmts else_stmt
    | If (expr, then_stmt, Some else_stmt) ->
       Format.fprintf fmt
         "@[<v 4>if (@[<hv>%a@]) {@ %a@]@,@[<v 4>} else {@ %a@]@,}"
         pp_expr expr
         pp_stmt then_stmt
         pp_stmt else_stmt
    | Block block_stmts ->
       Format.fprintf fmt "@[<v4>{@,%a@]@,}"
         pp_stmts block_stmts
    | Break ->
       Format.fprintf fmt "break;"
    | Malloc (l_value, typ, None) ->
       (* FIXME: abort if allocation fails *)
       Format.fprintf fmt "@[<hv 2>%a =@ malloc (sizeof (%a))@];"
         pp_expr     l_value
         pp_typename typ
    | Malloc (l_value, typ, Some (num, typ')) ->
       (* FIXME: abort if allocation fails *)
       Format.fprintf fmt
         "@[<hv 2>%a =@ malloc (sizeof (%a) + %a * sizeof(%a))@];"
         pp_expr     l_value
         pp_typename typ
         pp_expr     num
         pp_typename typ'
    | Free expr ->
       Format.fprintf fmt "@[<hv>free (%a)@];"
         pp_expr     expr
    | Return expr ->
       Format.fprintf fmt "return %a;" pp_expr expr
    | Call (nm, exps) ->
       Format.fprintf fmt "@[<hov 2>%s@,(@[<hv>%a)@]@];"
         nm
         Fmt.(list ~sep:(Fmt.always ",@ ") pp_expr) exps

  let pp_arg_decls =
    let pp_arg_decl fmt (Some_type ty, ident) =
      pp_decl fmt (ty, ident)
    in
    Fmt.(list ~sep:(Fmt.always ", ") pp_arg_decl)

  let pp_fundecl fmt { return_type; name; arg_decls; body } =
    let Some_type return_type = return_type in
    Format.fprintf fmt
      "@[<v 0>%a %s(%a)@,@[<v 5>{@,%a@]@,}@]"
      pp_typename return_type
      name
      pp_arg_decls arg_decls
      pp_stmts     body
end

module C () : sig
  include Syntax.S

  val fun_decls : unit -> AST.fundecl list

  val struct_decls : unit -> structure_desc list

  val gen : comm -> AST.block_stmt list
end = struct
  type (_,_) expr = Expr of AST.exp
  type 'a exp = ('a, [`exp]) expr
  type 'a var = ('a, [`exp|`var]) expr

  let un_expr (Expr e) = e

  let to_exp (Expr e) = Expr e

  type nonrec 'a typ = 'a typ


  type namegen = int

  let gen comm =
    snd (comm 0)

  type comm = namegen -> namegen * AST.block_stmt list

  (**********************************************************************)
  module Struct = struct
    type 'a t = 'a structure

    type ('s,'a) field = string

    let structures : (string, structure_field list) Hashtbl.t =
      Hashtbl.create 12
    let structures_ordered =
      ref []

    let make name =
      let name = Display_names.Fresh.choose (Hashtbl.mem structures) name in
      Hashtbl.add structures name [];
      structures_ordered := name :: !structures_ordered;
      Struct name

    let field (Struct name) fname typ =
      let rec add = function
        | [] -> [StructField { fld_name = fname; fld_type = typ }]
        | StructField { fld_name } as head :: rest ->
           if fld_name = fname then
             invalid_arg "Imp_syntax.S.field: duplicate fields in structure"
           else
             head :: add rest
      in
      let fields = Hashtbl.find structures name in
      Hashtbl.replace structures name (add fields);
      fname

    let seal (Struct name) =
      (* FIXME: do something here -- mark this structure as finished. *)
      ()

    let (#.) struct_exp field =
      Expr (Field (un_expr struct_exp, field))

    type exp_box = Exp : 'a exp -> exp_box

    let const (Struct name) exps =
      Expr (StructLit (name, Array.map (fun (Exp e) -> un_expr e) exps))
  end

  let struct_decls () =
    List.rev_map
      (fun name -> { name; fields = Hashtbl.find Struct.structures name })
      !Struct.structures_ordered

  (**********************************************************************)
  type _ arg_spec =
    | RetVoid : comm arg_spec
    | RetVal  : 'a typ -> ('a,[`exp]) expr arg_spec
    | Arg     : string * 'a typ * 'b arg_spec -> (('a, [`exp]) expr -> 'b) arg_spec
    | Ref     : string * 'a typ * 'b arg_spec -> (('a, [`exp|`var]) expr -> 'b) arg_spec

  let return_void = RetVoid
  let return t = RetVal t
  let (@->) (nm, t) s = Arg (nm, t, s)
  let (@&->) (nm, t) s = Ref (nm, t, s)

  let decld_functions = Hashtbl.create 20
  let decld_functions_order = ref []

  let fun_decls () =
    List.rev_map (Hashtbl.find decld_functions) !decld_functions_order

  let rec get_args : type a. a arg_spec -> (c_type * string) list -> c_type * (c_type * string) list = function
    | RetVoid          -> fun acc -> (Some_type Void, List.rev acc)
    | RetVal typ       -> fun acc -> (Some_type typ, List.rev acc)
    | Arg (nm, typ, a) -> fun acc -> get_args a ((Some_type typ, nm) :: acc)
    | Ref (nm, typ, a) -> fun acc -> get_args a ((Some_type (Pointer typ), nm) :: acc)

  (* FIXME: being very trusting about name collisions here *)
  let rec apply : type a. a arg_spec -> a -> AST.block_stmt list = function
    | RetVoid        -> fun c -> gen c
    | RetVal t       -> fun e -> [Statement (Return (un_expr e))]
    | Arg (nm, _, a) -> fun b -> apply a (b (Expr (Var nm)))
    | Ref (nm, _, a) -> fun b -> apply a (b (Expr (Deref (Var nm))))

  let declare_func ~name ~typ ~body =
    let name = Display_names.Fresh.choose (Hashtbl.mem decld_functions) name in
    decld_functions_order := name :: !decld_functions_order;
    let return_type, arg_decls = get_args typ [] in
    let decl = { AST.name; return_type; arg_decls; body = apply typ body } in
    Hashtbl.add decld_functions name decl;
    let rec gen_call : type a. a arg_spec -> AST.exp list -> a = function
      | RetVoid ->
         fun l ng -> ng, [Statement (Call (name, List.rev l))]
      | RetVal _ ->
         fun l -> Expr (ECall (name, List.rev l))
      | Arg (_, _, a) ->
         fun l (Expr e) -> gen_call a (e::l)
      | Ref (_, _, a) ->
         fun l (Expr e) -> gen_call a (AddrOf e::l)
    in
    gen_call typ []

  (**********************************************************************)

  open AST

  module Bool = struct
    let t = Bool
    let true_ = Expr (BoolLit true)
    let false_ = Expr (BoolLit false)
    let (&&) e1 e2 = Expr (Binop (un_expr e1, LAnd, un_expr e2))
    let (||) e1 e2 = Expr (Binop (un_expr e1, LOr, un_expr e2))
    let not e = Expr (Unop (LNot, un_expr e))
  end

  let empty ng =
    ng, []

  let (^^) c1 c2 ng =
    let ng, s1 = c1 ng in
    let ng, s2 = c2 ng in
    ng, s1 @ s2

  let rec block = function
    | [Statement (Block stmts)] -> block stmts
    | [Statement stmt]          -> stmt
    | stmts                     -> Block stmts

  let (:=) v e ng =
    ng, [Statement (Assign (un_expr v, un_expr e))]

  let while_ cond ~do_:body ng =
    let ng, body = body ng in
    ng, [Statement (While (un_expr cond, block body))]

  let break ng =
    ng, [Statement Break]

  let ifthen cond ~then_:body ng =
    let ng, body = body ng in
    ng, [Statement (If (un_expr cond,
                    block body,
                    None))]

  let if_ cond ~then_ ~else_ ng =
    let ng, then_ = then_ ng in
    let ng, else_ = else_ ng in
    ng, [Statement (If (un_expr cond,
                    block then_,
                    Some (block else_)))]

  let declare ?(name="x") typ ?init body ng =
    let name = String.map (function ':' -> '_' | x -> x) name in
    let nm   = Printf.sprintf "%s%d" name ng and ng = ng+1 in
    let init = match init with None -> None | Some e -> Some (un_expr e) in
    let decl = Declaration (typ, nm, init) in
    let ng, body = body (Expr (Var nm)) ng in
    ng, decl :: body


  module Int32 = struct

    let t = Int32
    let const i = Expr (IntLit i)
    let const_ i = Expr (IntLit (Int32.of_int i))
    let ( <  ) e1 e2 = Expr (Binop (un_expr e1, Lt, un_expr e2))
    let ( >  ) e1 e2 = Expr (Binop (un_expr e1, Gt, un_expr e2))
    let ( >= ) e1 e2 = Expr (Binop (un_expr e1, Ge, un_expr e2))
    let ( <= ) e1 e2 = Expr (Binop (un_expr e1, Le, un_expr e2))
    let ( == ) e1 e2 = Expr (Binop (un_expr e1, Eq, un_expr e2))
    let ( != ) e1 e2 = Expr (Binop (un_expr e1, Ne, un_expr e2))
    let ( +  ) e1 e2 = Expr (Binop (un_expr e1, Plus, un_expr e2))
    let ( *  ) e1 e2 = Expr (Binop (un_expr e1, Mult, un_expr e2))
    let ( -  ) e1 e2 = Expr (Binop (un_expr e1, Sub, un_expr e2))
    let maximum = Expr Int32Max

  end


  module RawArray = struct

    type nonrec 'a array = 'a array

    let array x n = Array (x,n)

    let (#@) array_exp idx_exp =
      Expr (Idx (un_expr array_exp, un_expr idx_exp))

  end


  module RawPtr = struct

    type nonrec 'a ptr = 'a ptr

    let ptr x = Pointer x

    let malloc v typ ng =
      ng, [Statement (Malloc (un_expr v, typ, None))]

    let malloc_ext v typ n typ' ng =
      ng, [Statement (Malloc (un_expr v, typ, Some (un_expr n, typ')))]

    let free e ng =
      ng, [Statement (Free (un_expr e))]

    let deref e = Expr (Deref (un_expr e))

    let ( =*= ) e1 e2 =
      Expr (Binop (un_expr e1, Eq, un_expr e2))

    let ( =!*= ) e1 e2 =
      Expr (Binop (un_expr e1, Ne, un_expr e2))

    let null = Expr Null

    let (#->) struct_ptr_exp field =
      Expr (Field (un_expr (deref struct_ptr_exp), field))

  end

  module Stdio = struct

  (* FIXME: https://stackoverflow.com/questions/9225567/how-to-print-a-int64-t-type-in-c *)

    type out_ch
    type in_ch

    (* FIXME: handle failure to open by checking the handle for NULL: just abort? *)
    (* FIXME: 'break' safety:
       need a kind of 'defer' and tracking of scopes. *)

    (* FIXME: need a special set of unshadowable special variables *)
    let stdout = Expr (Var "stdout")

    let with_file_output filename k =
      declare ~name:"f" (Pointer (Typedef "FILE"))
        ~init:(Expr (ECall ("fopen", [StrLit filename; StrLit "w"])))
        (fun h -> begin%monoid
             k h;
             (fun ng -> ng, [Statement (Call ("fclose", [un_expr h]))])
           end)

    let stdin = Expr (Var "stdin")

    let with_file_input filename k =
      declare ~name:"f" (Pointer (Typedef "FILE"))
        ~init:(Expr (ECall ("fopen", [StrLit filename; StrLit "r"])))
        (fun h -> begin%monoid
             k h;
             (fun ng -> ng, [Statement (Call ("fclose", [un_expr h]))])
           end)

    type 'a fmt =
      | Stop  : comm fmt
      | Int32 : 'a fmt -> ((int32, [`exp]) expr -> 'a) fmt
      | Lit   : string * 'a fmt -> 'a fmt

    let stop = Stop
    let int32 x = Int32 x
    let lit s x = Lit (s,x)

    let printf out_ch fmt =
      let rec build : type a. a fmt -> string -> AST.exp list -> a =
        function
          | Stop -> fun fmtstr exps ng ->
            let args = un_expr out_ch :: StrLit fmtstr :: List.rev exps in
            ng, [Statement (Call ("fprintf", args))]
          | Int32 fmt -> fun fmtstr exps exp ->
            build fmt (fmtstr ^ "%d") (un_expr exp::exps)
          | Lit (s, fmt) -> fun fmtstr exps ->
            (* FIXME: escape '%'s *)
            build fmt (fmtstr ^ s) exps
      in
      build fmt "" []

    (* FIXME: alter this to not use fscanf: read from the input
       directly. *)
    let scanf in_ch fmt ~parsed ~eof =
      let rec build : type a. a fmt -> string -> int -> AST.exp list -> a -> comm =
        function
          | Stop -> fun fmtstr n refs k ->
            let args = un_expr in_ch :: StrLit fmtstr :: List.rev refs in
            declare ~name:"rv" Int32
              ~init:(Expr (ECall ("fscanf", args)))
              begin fun rv ->
                let open! Int32 in
                if_ (rv == const_ n)
                  ~then_:k
                  ~else_:begin
                    if_ (rv == Expr (Const "EOF"))
                      ~then_:eof
                      ~else_:empty (* FIXME: handle errors *)
                  end
              end
          | Int32 fmt -> fun fmtstr n refs k ->
            declare Int32 begin fun x ->
              build fmt (fmtstr ^ "%d") (n+1) (AddrOf (un_expr x) :: refs) (k x)
            end
          | Lit (s, fmt) -> fun fmtstr n refs k ->
            build fmt (fmtstr ^ s) n refs k
      in
      build fmt "" 0 [] parsed
  end
end

let output f x fmt =
  let module C = C () in
  let main_comm = f.Syntax.generate (module C) x in
  let stmts   = C.gen main_comm in
  let structs = C.struct_decls () in
  let funcs   = C.fun_decls () in
  Format.pp_set_margin fmt 90;
  Format.pp_set_max_indent fmt 80;
  Format.pp_open_vbox fmt 0;
  Format.fprintf fmt "#include <stdlib.h>@,";
  Format.fprintf fmt "#include <stdio.h>@,";
  Format.fprintf fmt "#include <stdbool.h>@,";
  Format.fprintf fmt "#include <stdint.h>@,@,";
  structs |> List.iter begin fun struct_decl ->
    PP.pp_struct_decl fmt struct_decl;
    Format.pp_print_cut fmt ()
  end;
  Format.pp_print_cut fmt ();
  funcs |> List.iter begin fun fun_decl ->
    PP.pp_fundecl fmt fun_decl;
    Format.pp_print_cut fmt ();
    Format.pp_print_cut fmt ();
  end;
  Format.fprintf fmt "@[<v 4>int main(int argc, char **argv) {@ ";
  PP.pp_stmts fmt stmts;
  Format.fprintf fmt "@]@,}@,";
  Format.pp_close_box fmt ()

let compile outputname program x =
  (* FIXME: should probably quote 'output' somehow, or use a better API *)
  let cmd = Printf.sprintf "gcc -o %s -xc -O2 -" outputname in
  let ch  = Unix.open_process_out cmd in
  let fmt = Format.formatter_of_out_channel ch in
  output program x fmt;
  Format.pp_print_flush fmt ();
  flush ch;
  let status = Unix.close_process_out ch in
  match status with
    | Unix.WEXITED 0 -> ()
    | _ ->
       Printf.eprintf "GCC process exited with an error\n"
