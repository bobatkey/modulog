type _ ptr = Ptr
type _ array = Array
type _ structure = Structure

type 'a typ =
  | Int     : int typ
  | Bool    : bool typ
  | Pointer : 'a typ -> 'a ptr typ
  | Array   : 'a typ * int32 -> 'a array typ
  | Struct  : string -> 'a structure typ

type c_exp =
  | Var of string

  | BoolLit of bool
  | IntLit of int32

  | Binop of c_exp * string * c_exp
  | Unop  of string * c_exp

  | Deref of c_exp
  | Field of c_exp * string
  | Idx   of c_exp * c_exp

type stmt =
  | Assign  of c_exp * c_exp
  | IfThen  of c_exp * stmt list
  | IfThenElse of c_exp * stmt list * stmt list
  | While   of c_exp * stmt list
  | Break
  | Decl    : 'a typ * string * stmt list -> stmt
  | Malloc  : c_exp * 'a typ -> stmt


module C () = struct
  type namegen = int

  type 'a exp = c_exp
  type 'a var = c_exp
  type comm   = namegen -> stmt list

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
     appropriate decls. *)
  let structure name = Struct name
  let field (Struct name) fname typ = fname
  let seal (Struct name) = ()

  let (!) e = e

  let true_ = BoolLit true
  let false_ = BoolLit false
  let (&&) e1 e2 = Binop (e1, "&&", e2)
  let (||) e1 e2 = Binop (e1, "||", e2)
  let not e = Unop ("!", e)

  let empty ng = []
  let (^^) c1 c2 ng = c1 ng @ c2 ng

  let (:=) v e ng =
    [Assign (v, e)]

  let while_ cond body ng =
    [While (cond, body ng)]

  let break ng = [Break]

  let ifthen cond body ng =
    [IfThen (cond, body ng)]

  let ifthenelse cond ~then_ ~else_ ng =
    [IfThenElse (cond, then_ ng, else_ ng)]

  let declare typ body ng =
    let nm = Printf.sprintf "x%d" ng and ng = ng+1 in
    [Decl (typ, nm, body (Var nm) ng)]

  let deref e = Deref e

  let (#@) array_exp idx_exp =
    Idx (array_exp, idx_exp)

  let (#.) struct_exp field =
    Field (struct_exp, field)

  let (#->) struct_ptr_exp field =
    Field (Deref struct_ptr_exp, field)

  let malloc v typ ng =
    [Malloc (v, typ)]

  let const i = IntLit i
  let ( <  ) e1 e2 = Binop (e1, "<", e2)
  let ( >  ) e1 e2 = Binop (e1, ">", e2)
  let ( >= ) e1 e2 = Binop (e1, ">=", e2)
  let ( == ) e1 e2 = Binop (e1, "==", e2)
  let ( +  ) e1 e2 = Binop (e1, "+", e2)
  let ( -  ) e1 e2 = Binop (e1, "-", e2)
end
