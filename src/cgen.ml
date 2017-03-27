module type CGEN = sig
  type 'a exp
  type 'a var
  type comm

  type node
  type key

  val (!) : 'a var -> 'a exp

  val true_  : bool exp
  val false_ : bool exp
  val ( && ) : bool exp -> bool exp -> bool exp
  val ( || ) : bool exp -> bool exp -> bool exp
  val not    : bool exp -> bool exp

  val const : int32 -> int exp
  val ( < ) : int exp -> int exp -> bool exp
  val ( > ) : int exp -> int exp -> bool exp
  val ( >= ) : int exp -> int exp -> bool exp
  val ( == ) : int exp -> int exp -> bool exp
  val ( + ) : int exp -> int exp -> int exp
  val ( - ) : int exp -> int exp -> int exp

  val empty : comm
  val (^^) : comm -> comm -> comm

  val ( := ) : 'a var -> 'a exp -> comm

  val while_ : bool exp -> (brk:comm -> comm) -> comm

  val ifthen : bool exp -> comm -> comm
  val ifthenelse : bool exp -> then_:comm -> else_:comm -> comm

  val declare_int     : (int var -> comm) -> comm
  val declare_nodeptr : (node var -> comm) -> comm

  val declare_int_array : int32 -> ((int exp -> int var) -> comm) -> comm
  val declare_nodeptr_array : int32 -> ((int exp -> node var) -> comm) -> comm

  val alloc_node : node var -> comm

  val node_leaf  : node exp -> bool var
  val node_nkeys : node exp -> int var
  val node_key   : node exp -> int exp -> key var
  val node_child : node exp -> int exp -> node var

  val key_lt : key exp -> key exp -> bool exp
  val key_eq : key exp -> key exp -> bool exp
  val key_le : key exp -> key exp -> bool exp
end

type c_exp =
  | VarNm of string

  | BoolLit of bool
  | IntLit of int32

  | Binop of c_exp * string * c_exp
  | Unop  of string * c_exp

  | Field of c_exp * string
  | Idx   of c_exp * c_exp

type stmt =
  | Assign  of c_exp * c_exp
  | IfThen  of c_exp * stmt list
  | IfThenElse of c_exp * stmt list * stmt list
  | While   of string * c_exp * stmt list
  | Goto    of string
  | Decl    of string * string * stmt list
  | Malloc  of c_exp * string

let pp_prog pp stmts =
  let module PP = Pretty.Stream in
  let rec pp_stmts = function
    | [] ->
       ()
    | Assign (lvalue, rvalue) :: stmts ->
       PP.start_group pp;
       PP.start_nest pp 2;
       pp_exp lvalue;
       PP.text pp " =";
       PP.break pp " ";
       pp_exp rvalue;
       PP.end_nest_or_align pp;
       PP.end_group pp;
       PP.text pp ";";
       if stmts <> [] then PP.break pp " ";
       pp_stmts stmts
    | Malloc (v, nm) :: stmts ->
       PP.start_group pp;
       PP.start_nest pp 2;
       pp_exp v;
       PP.text pp " =";
       PP.break pp " ";
       PP.text pp ("malloc (" ^ nm ^ ")");
       PP.end_nest_or_align pp;
       PP.end_group pp;
       PP.text pp ";";
       PP.break pp " ";
       pp_stmts stmts       
    | IfThen (cond, body) :: stmts ->
       PP.text pp "if (";
       pp_exp cond;
       PP.text pp ") {";
       PP.start_nest pp 5;
       PP.break pp " ";
       pp_stmts body;
       PP.end_nest_or_align pp;
       PP.break pp " ";
       PP.text pp "}";
       if stmts <> [] then PP.break pp " ";
       pp_stmts stmts
    | IfThenElse (cond, body, body2) :: stmts ->
       PP.text pp "if (";
       pp_exp cond;
       PP.text pp ") {";
       PP.start_nest pp 5;
       PP.break pp " ";
       pp_stmts body;
       PP.end_nest_or_align pp;
       PP.break pp " ";
       PP.text pp "} else {";
       PP.start_nest pp 5;
       PP.break pp " ";
       pp_stmts body2;
       PP.end_nest_or_align pp;
       PP.break pp " ";
       PP.text pp "}";
       if stmts <> [] then PP.break pp " ";
       pp_stmts stmts
    | While (label, cond, body) :: stmts ->
       PP.text pp "while (";
       pp_exp cond;
       PP.text pp ") {";
       PP.start_nest pp 5;
       PP.break pp " ";
       pp_stmts body;
       PP.end_nest_or_align pp;
       PP.break pp " ";
       PP.text pp "}";
       PP.break pp " ";
(*       PP.text pp (label^":");
         PP.break pp " ";*)
       pp_stmts stmts
    | Goto label :: _ ->
       PP.text pp "break;"
    (*PP.text pp ("goto "^label^";")*)
    | [Decl (typ, nm, body)] ->
       PP.text pp typ;
       PP.text pp " ";
       PP.text pp nm;
       PP.text pp ";";
       PP.break pp " ";
       pp_stmts body
    | Decl (typ, nm, body) :: stmts ->
       PP.text pp "{";
       PP.start_nest pp 1;
       PP.text pp typ;
       PP.text pp " ";
       PP.text pp nm;
       PP.text pp ";";
       PP.break pp " ";
       pp_stmts body;
       PP.end_nest_or_align pp;
       PP.break pp " ";
       PP.text pp "}";
       PP.break pp " ";
       pp_stmts stmts
  and pp_exp = function
    | VarNm x   -> PP.text pp x
    | BoolLit b -> PP.text pp (if b then "true" else "false")
    | IntLit i  -> PP.text pp (Int32.to_string i)
    | Binop (e1, op, e2) ->
       PP.start_group pp;
       PP.text pp "(";
       pp_exp e1;
       PP.break pp " ";
       PP.text pp op;
       PP.text pp " ";
       pp_exp e2;
       PP.text pp ")";
       PP.end_group pp
    | Unop (op, e) ->
       PP.text pp "(";
       PP.text pp op;
       pp_exp e;
       PP.text pp ")"
    | Field (e, nm) ->
       pp_exp e; PP.text pp "->"; PP.text pp nm
    | Idx (e, i) ->
       pp_exp e; PP.text pp "["; pp_exp i; PP.text pp "]"
  in
  pp_stmts stmts;
  PP.flush pp;
  print_newline ()

module C = struct
  open Printf

  type namegen = int

  type 'a exp = c_exp
  type 'a var = c_exp
  type comm   = namegen -> stmt list

  type node
  type key

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
    let lbl = sprintf "label%d" ng and ng = ng+1 in
    [While (lbl, cond, body ~brk:(fun ng -> [Goto lbl]) ng)]

  let ifthen cond body ng =
    [IfThen (cond, body ng)]

  let ifthenelse cond body body2 ng =
    [IfThenElse (cond, body ng, body2 ng)]

  let declare_int body ng =
    let nm = sprintf "i%d" ng and ng = ng+1 in
    [Decl ("int", nm, body (VarNm nm) ng)]

  let declare_nodeptr body ng =
    let nm = sprintf "x%d" ng and ng = ng+1 in
    [Decl ("node *", nm, body (VarNm nm) ng)]

  let alloc_node v ng =
    [Malloc (v, "sizeof(node)")]

  let node_leaf e = Field (e, "leaf")
  let node_nkeys e = Field (e, "nkeys")
  let node_key e i = Idx (Field (e, "key"), i)
  let node_child e i = Idx (Field (e, "children"), i)

  let const i = IntLit i
  let ( <  ) e1 e2 = Binop (e1, "<", e2)
  let ( >  ) e1 e2 = Binop (e1, ">", e2)
  let ( >= ) e1 e2 = Binop (e1, ">=", e2)
  let ( == ) e1 e2 = Binop (e1, "==", e2)
  let ( +  ) e1 e2 = Binop (e1, "+", e2)
  let ( -  ) e1 e2 = Binop (e1, "-", e2)

  let key_lt = ( < )
  let key_le = ( <= )
  let key_eq = ( == )
end


module type BTREE_PARAMETERS = sig
  val min_children : int32
end

module BTree (CGen : CGEN) (Parameters : BTREE_PARAMETERS) : sig
  open CGen

  val ifmember : key exp -> node exp -> comm -> comm -> comm
  val iterate_range : key exp -> key exp -> node exp -> (key exp -> comm) -> comm
  (* FIXME: need if_member_in_range *)
  val insert : key exp -> node var -> comm
end = struct
  open Parameters
  open CGen

  let min_keys = Int32.sub min_children 1l
  let max_keys = Int32.(sub (mul min_children 2l) 1l)
  
  let while__ cond body =
    while_ cond (fun ~brk -> body)

  let forever body =
    while_ true_ body

  let incr i =
    i := !i + const 1l

  let decr i =
    i := !i - const 1l

  (* FIXME: compute this from the min_children and a reasonable
     estimate of the maximum size of any tree. *)
  let max_stack_depth = 40l
  
  let stack body =
    declare_nodeptr_array max_stack_depth begin fun stack ->
      declare_int_array max_stack_depth begin fun stack_child ->
        declare_int begin fun stackptr ->
          begin%monoid
            let push x i =
              begin%monoid
                stack !stackptr := x;
                stack_child !stackptr := i;
                incr stackptr
              end
            and pop =
              decr stackptr
            and top =
              (stack (!stackptr - const 1l),
               stack_child (!stackptr - const 1l))
            and is_empty =
              !stackptr == const 0l
            in
            stackptr := const 0l;
            body ~push ~pop ~top ~is_empty
          end
        end
      end
    end

  let find_key i x key =
    begin%monoid
      i := const 0l;
      while__ (!i < !(node_nkeys x) && key_lt !(node_key x !i) key)
        (incr i)
    end

  let alloc_node body =
    declare_nodeptr (fun x ->
        begin%monoid
          alloc_node x;
          body x
        end)
  
  let declare_nodeptr e body =
    declare_nodeptr (fun x -> begin%monoid
          x := e;
          body x
        end)
  
  let ifmember (key : key exp) (t : node exp) (yes : comm) (no : comm) =
    declare_nodeptr t begin fun x ->
      declare_int begin fun i ->
        forever begin fun ~brk ->
          begin%monoid
            find_key i !x key;
            ifthen (!i < !(node_nkeys !x) && key_eq !(node_key !x !i) key)
              begin%monoid
                yes;
                brk
              end;
            ifthen !(node_leaf !x)
              begin%monoid
                no;
                brk
              end;
            x := !(node_child !x !i)
          end
        end
      end
    end

  let iterate_range from upto tree body =
    declare_nodeptr tree begin fun x ->
      stack begin fun ~push ~pop ~top ~is_empty ->
        declare_int begin fun i ->
          begin%monoid
            forever begin fun ~brk ->
              begin%monoid
                find_key i !x from;
                ifthen (!(node_leaf !x)) brk;
                ifthen (!i < !(node_nkeys !x)) (push !x !i);
                x := !(node_child !x !i)
              end
            end;
            forever begin fun ~brk ->
              begin%monoid
                while__ (!i < !(node_nkeys !x) && key_le !(node_key !x !i) upto)
                  begin%monoid body !(node_key !x !i); incr i end;

                ifthen (!i < !(node_nkeys !x) || is_empty) brk;

                x := !(fst top);
                i := !(snd top);

                ifthen (not (key_le !(node_key !x !i) upto)) brk;

                body !(node_key !x !i);

                ifthenelse (!i == !(node_nkeys !x))
                  ~then_:pop
                  ~else_:(incr (snd top));

                x := !(node_child !x (!i + const 1l));

                while__ (not !(node_leaf !x)) begin%monoid
                  pop; x := !(node_child !x (const 0l))
                end;

                i := const 0l;
              end
            end
          end
        end
      end
    end

  let move_keys_up x i =
    declare_int begin fun j ->
      begin%monoid
        j := !(node_nkeys x) - const 1l;
        while__ (!j >= i) begin%monoid
          node_key x (!j + const 1l) := !(node_key x !j);
          decr j
        end
      end
    end

  let copy ~n ~src ~dst =
    declare_int @@ fun j -> begin%monoid
      j := const 0l;
      while__ (!j < n) begin%monoid
        dst !j := src !j;
        incr j
      end
    end
  
  let split_child x i =
    declare_nodeptr !(node_child x i) begin fun y ->
      let y = !y in
      alloc_node @@ fun z -> begin%monoid
        let z = !z in
        node_leaf z  := !(node_leaf y);
        node_nkeys z := const min_keys;

        (* copy the keys over *)
        copy ~n:(const min_keys)
          ~src:(fun j -> !(node_key y (j + const min_children)))
          ~dst:(node_key z);

        (* copy the children over (if not a leaf node) *)
        ifthen (not !(node_leaf y)) begin
          copy ~n:(const min_children)
            ~src:(fun j -> !(node_child y (j + const min_children)))
            ~dst:(node_child z)
        end;

        (* truncate y *)
        node_nkeys y := const min_keys;

        (* shunt x's children up *)
        declare_int begin fun j ->
          begin%monoid
            j := !(node_nkeys x);
            while__ (!j > i) begin%monoid
              node_child x (!j + const 1l) := !(node_child x !j);
              decr j
            end
          end
        end;

        move_keys_up x i;

        node_child x (i + const 1l) := z;
        node_key x i := !(node_key y (const min_keys));
        incr (node_nkeys x)
      end
    end

  let node_is_full x = !(node_nkeys x) == const max_keys

  let insert_nonfull x key =
    declare_int begin fun i ->
      begin%monoid
        while__ (not !(node_leaf !x))
          begin%monoid
            find_key i !x key;
            ifthen (node_is_full !(node_child !x !i))
              begin%monoid
                split_child !x !i;
                ifthen (key_lt !(node_key !x !i) key) begin
                  incr i
                end
              end;
            x := !(node_child !x !i)
          end;

        find_key i !x key;
        move_keys_up !x !i;
        node_key !x !i := key;
        incr (node_nkeys !x)
      end
    end

  let insert key root = begin%monoid
    (* if the root is full, then split it by making a new root node
       with a single child, and using split_child *)
    ifthen (node_is_full !root) begin
      alloc_node @@ fun s -> begin%monoid
        node_leaf !s := false_;
        node_nkeys !s := const 0l;
        node_child !s (const 0l) := !root;
        split_child !s (const 0l);
        root := !s
      end
    end;
    (* Once the root is not full, insert the key into it. *)
    declare_nodeptr !root begin fun x ->
      insert_nonfull x key
    end
  end

end

(* key comparisons:

   - member :        <, ==
   - iterate_range : <
   - split_child: 
   - insert_nonfull: <
*)

(*module B = BTree (C) (struct let min_children = 2l end)*)

(*let stmts = B.ifmember (VarNm "key") (VarNm "tree") (C.empty) (C.empty) 0*)

