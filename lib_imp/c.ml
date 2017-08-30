(* TODO
   - Improve name generation
   - Generation of struct decalarations
   - Generation of complete function declarations (and calls?)

   To be able to generate code for the Btrees:
   - a thing to turn a command into a function
   - to be able to use that function in later code (i.e., we get a new 'primitive')
*)


type _ ptr = Ptr
type _ array = Array
type _ structure = Structure

type 'a typ =
  | Int     : int typ
  | Bool    : bool typ
  | Pointer : 'a typ -> 'a ptr typ
  | Array   : 'a typ * int32 -> 'a array typ
  | Struct  : string -> 'a structure typ

type c_type =
  | A_type : 'a typ -> c_type

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

type unop =
  | Neg
  | LNot

type c_exp =
  | Var of string

  | Null

  | BoolLit of bool
  | IntLit of int32
  | StrLit of string

  | Binop of c_exp * binop * c_exp
  | Unop  of unop * c_exp

  | Deref of c_exp
  | Field of c_exp * string
  | Idx   of c_exp * c_exp

type stmt =
  | Assign  of c_exp * c_exp
  | Malloc  : c_exp * 'a typ * (c_exp * 'b typ) option -> stmt
  | Free    : c_exp -> stmt
  | If      of c_exp * stmt * stmt option
  | While   of c_exp * stmt
  | Break
  | Block   of block_stmt list
  | Return  of c_exp
  | Call    of string * c_exp list

and block_stmt =
  | Declaration : 'a typ * string -> block_stmt
  | Statement   : stmt            -> block_stmt

type fundecl =
  { return_type : c_type
  ; name        : string
  ; arg_decls   : (c_type * string) list
  ; body        : block_stmt list
  }

module PP = struct
  let rec pp_typ : type a. (bool -> Format.formatter -> unit) ->
    Format.formatter -> a typ -> unit =
    fun f fmt -> function
      | Int ->
         Format.fprintf fmt "int %t" (f true)
      | Bool ->
         Format.fprintf fmt "bool %t" (f true)
      | Struct nm ->
         Format.fprintf fmt "struct %s %t" nm (f true)
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

  let pp_decl fmt (typ, ident) =
    pp_typ (fun l fmt -> Format.pp_print_string fmt ident) fmt typ;
    Format.pp_print_string fmt ";"

  let pp_typename fmt typ =
    pp_typ (fun l fmt -> ()) fmt typ

  let rec pp_expr prec fmt = function
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
    | BoolLit b ->
       Format.fprintf fmt "%b" b
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
    | StrLit s ->
       Format.fprintf fmt "%S" s

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
        | Declaration (typ, ident) :: stmts ->
           (match previous with
             | `Start ->
                pp_decl fmt (typ, ident)
             | `Decl ->
                Format.pp_print_cut fmt ();
                pp_decl fmt (typ, ident)
             | `Stmt ->
                Format.pp_print_cut fmt ();
                Format.pp_print_cut fmt ();
                pp_decl fmt (typ, ident));
           loop `Decl stmts
        | [] ->
           ()
    in
    loop `Start stmts;
    Format.fprintf fmt "@]"

  and pp_stmt fmt = function
    | Assign (l_value, expr) ->
       Format.fprintf fmt "@[<hv>%a =@ %a@];"
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
       Format.fprintf fmt "@[<v 4>if (@[<hv>%a@]) {@ %a@]@,@[<v 4>} else {@ %a@]@,}"
         pp_expr expr
         pp_stmts then_stmt
         pp_stmts else_stmt
    | If (expr, Block then_stmt, Some else_stmt) ->
       Format.fprintf fmt "@[<v 4>if (@[<hv>%a@]) {@ %a@]@,@[<v 4>} else {@ %a@]@,}"
         pp_expr expr
         pp_stmts then_stmt
         pp_stmt else_stmt
    | If (expr, then_stmt, Some (Block else_stmt)) ->
       Format.fprintf fmt "@[<v 4>if (@[<hv>%a@]) {@ %a@]@,@[<v 4>} else {@ %a@]@,}"
         pp_expr expr
         pp_stmt then_stmt
         pp_stmts else_stmt
    | If (expr, then_stmt, Some else_stmt) ->
       Format.fprintf fmt "@[<v 4>if (@[<hv>%a@]) {@ %a@]@,@[<v 4>} else {@ %a@]@,}"
         pp_expr expr
         pp_stmt then_stmt
         pp_stmt else_stmt
    | Block block_stmts ->
       Format.fprintf fmt "@[<v4>{@,%a@]@,}"
         pp_stmts block_stmts
    | Break ->
       Format.fprintf fmt "break;@,"
    | Malloc (l_value, typ, None) ->
       (* FIXME: abort if allocation fails *)
       Format.fprintf fmt "@[<hv>%a =@ malloc (sizeof (%a))@];"
         pp_expr     l_value
         pp_typename typ
    | Malloc (l_value, typ, Some (num, typ')) ->
       (* FIXME: abort if allocation fails *)
       Format.fprintf fmt "@[<hv>%a =@ malloc (sizeof (%a) + %a * sizeof(%a))@];"
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
       Format.fprintf fmt "@[<hov>%s@,(@[<hv>%a)@]@];"
         nm
         Fmt.(list ~sep:(Fmt.always ",@ ") pp_expr) exps

  let pp_arg_decls =
    let pp_arg_decl fmt (A_type ty, ident) =
      pp_decl fmt (ty, ident)
    in
    Fmt.(list ~sep:(Fmt.always ", ") pp_arg_decl)

  let pp_fundecl fmt { return_type; name; arg_decls; body } =
    let A_type return_type = return_type in
    Format.fprintf fmt
      "@[<v 0>]%a %s(%a)@,@[<v 5>{@,%a@]@,}@]"
      pp_typename return_type
      name
      pp_arg_decls arg_decls
      pp_stmts body
end

module C () : sig
  include Syntax.S with type ('a,'b) expr = c_exp

  val gen : comm -> block_stmt list
end = struct
  type namegen = int

  let gen comm = match comm 0 with `Open stmts | `Closed stmts -> stmts

  type (_,_) expr = c_exp
  type 'a exp = c_exp
  type 'a var = c_exp
  type comm   =
    namegen -> [ `Closed of block_stmt list | `Open of block_stmt list ]

  type nonrec 'a typ = 'a typ
  type nonrec 'a ptr = 'a ptr
  type nonrec 'a array = 'a array
  type nonrec 'a structure = 'a structure

  let int = Int
  let bool = Bool
  let ptr x = Pointer x
  let array x n = Array (x,n)

  type ('s,'a) field = string

  (* FIXME: keep a record of each structure type, and generate the
     appropriate decls when asked to. *)
  type structure_field =
    | StructField : { fld_name : string; fld_type : 'a typ } -> structure_field

  let structures : (string, structure_field list) Hashtbl.t =
    Hashtbl.create 12

  let structure name =
    let name = Name_freshener.fresh_for (Hashtbl.mem structures) name in
    Hashtbl.add structures name [];
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

(*
  type (_,_) arg_spec =
    | Arg : 'a typ * ('r,'s) arg_spec -> ('a exp -> 'r,'a var -> 's) arg_spec
    | End : (comm, comm) arg_spec
*)

  (* type 'a arg_spec = *)
  (*   | Return : comm arg_spec *)
  (*   | Arg    : 'a typ * 'b arg_spec -> ('a var -> 'b) arg_spec *)

  let true_ = BoolLit true
  let false_ = BoolLit false
  let (&&) e1 e2 = Binop (e1, LAnd, e2)
  let (||) e1 e2 = Binop (e1, LOr, e2)
  let not e = Unop (LNot, e)

  let empty ng =
    `Open []

  let (^^) c1 c2 ng =
    match c1 ng, c2 ng with
      | `Open stmts1,   `Open stmts2   ->
         `Open (stmts1 @ stmts2)
      | `Open stmts1,   `Closed stmts2 ->
         `Closed (stmts1 @ stmts2)
      | `Closed stmts1, `Open stmts2   ->
         `Open (Statement (Block stmts1) :: stmts2)
      | `Closed stmts1, `Closed stmts2 ->
         `Closed (Statement (Block stmts1) :: stmts2)

  let rec block = function
    | `Open [Statement (Block stmts)]
    | `Closed [Statement (Block stmts)] -> block (`Open stmts)
    | `Open [Statement stmt]
    | `Closed [Statement stmt]          -> stmt
    | `Open stmts
    | `Closed stmts                     -> Block stmts

  let (:=) v e ng =
    `Open [Statement (Assign (v, e))]

  let while_ cond ~do_:body ng =
    `Open [Statement (While (cond, block (body ng)))]

  let break ng =
    `Open [Statement Break]

  let ifthen cond ~then_:body ng =
    `Open [Statement (If (cond, block (body ng), None))]

  let if_ cond ~then_ ~else_ ng =
    `Open [Statement (If (cond, block (then_ ng), Some (block (else_ ng))))]

  let declare ?(name="x") typ body ng =
    let nm = Printf.sprintf "%s%d" name ng and ng = ng+1 in
    let decl = Declaration (typ, nm) in
    match body (Var nm) ng with
      | `Closed stmts | `Open stmts ->
         `Closed (decl :: stmts)

  let malloc v typ ng =
    `Open [Statement (Malloc (v, typ, None))]

  let malloc_ext v typ n typ' ng =
    `Open [Statement (Malloc (v, typ, Some (n, typ')))]

  let free expr ng =
    `Open [Statement (Free expr)]

  let deref e = Deref e

  let (#@) array_exp idx_exp =
    Idx (array_exp, idx_exp)

  let (#.) struct_exp field =
    Field (struct_exp, field)

  let (#->) struct_ptr_exp field =
    Field (Deref struct_ptr_exp, field)

  let const i = IntLit i
  let ( <  ) e1 e2 = Binop (e1, Lt, e2)
  let ( >  ) e1 e2 = Binop (e1, Gt, e2)
  let ( >= ) e1 e2 = Binop (e1, Ge, e2)
  let ( <= ) e1 e2 = Binop (e1, Le, e2)
  let ( == ) e1 e2 = Binop (e1, Eq, e2)
  let ( != ) e1 e2 = Binop (e1, Ne, e2)
  let ( +  ) e1 e2 = Binop (e1, Plus, e2)
  let ( *  ) e1 e2 = Binop (e1, Mult, e2)
  let ( -  ) e1 e2 = Binop (e1, Sub, e2)

  let ( =*= ) e1 e2 = Binop (e1, Eq, e2)
  let ( =!*= ) e1 e2 = Binop (e1, Ne, e2)
  let null = Null

  let print_int e ng =
    `Open [Statement (Call ("printf", [StrLit "%d"; e]))]

  let print_newline ng =
    `Open [Statement (Call ("printf", [StrLit "\n"]))]

  let print_str str ng =
    `Open [Statement (Call ("printf", [StrLit "%s"; StrLit str]))]
end
