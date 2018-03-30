module type S = sig

  (**{2 Representation of generated types} *)

  type 'a typ
  type 'a ptr
  type 'a array
  type 'a structure

  (**{3 Representations of basic types}*)

  (** Representation of the type of machine integers. *)
  val int32 : int32 typ

  (** Representation of the type of booleans. *)
  val bool : bool typ

  (** Representation of pointers to memory containing a value of some
      other type. *)
  val ptr : 'a typ -> 'a ptr typ

  (** Representation of arrays of statically known size. *)
  val array : 'a typ -> int32 -> 'a array typ

  (** {3 Representations of structure types} *)

  type ('s, 'a) field

  (* Creation of types; a Ctypes style interface *)
  val structure : string -> 's structure typ
  val field     : 's structure typ -> string -> 'a typ -> ('s, 'a) field
  val seal      : 's structure typ -> unit

  (** {2 Expressions and Assignables} *)

  type ('a,_) expr
  type 'a exp = ('a,[`exp]) expr
  type 'a var = ('a,[`var|`exp]) expr

  val to_exp : 'a var -> 'a exp

  (** {3 Boolean expressions} *)

  val true_  : bool exp
  val false_ : bool exp
  val ( && ) : (bool, [>`exp]) expr -> (bool, [>`exp]) expr -> bool exp
  val ( || ) : (bool, [>`exp]) expr -> (bool, [>`exp]) expr -> bool exp
  val not    : (bool, [>`exp]) expr -> bool exp

  (** {3 Integer expressions} *)

  val const : int32 -> int32 exp
  val ( < ) : (int32, [>`exp]) expr -> (int32, [>`exp]) expr -> bool exp
  val ( > ) : (int32, [>`exp]) expr -> (int32, [>`exp]) expr -> bool exp
  val ( >= ) : (int32, [>`exp]) expr -> (int32, [>`exp]) expr -> bool exp
  val ( <= ) : (int32, [>`exp]) expr -> (int32, [>`exp]) expr -> bool exp
  val ( == ) : (int32, [>`exp]) expr -> (int32, [>`exp]) expr -> bool exp
  val ( != ) : (int32, [>`exp]) expr -> (int32, [>`exp]) expr -> bool exp
  val ( + ) : (int32, [>`exp]) expr -> (int32, [>`exp]) expr -> int32 exp
  val ( * ) : (int32, [>`exp]) expr -> (int32, [>`exp]) expr -> int32 exp
  val ( - ) : (int32, [>`exp]) expr -> (int32, [>`exp]) expr -> int32 exp
  val int32_max : int32 exp

  (** {3 Structs} *)

  (** Structure field access. *)
  val (#.) : ('s structure, [>`exp]) expr -> ('s, 'a) field -> ('a,[<`exp|`var]) expr

  type exp_box = Exp : 'a exp -> exp_box

  (** Structure literals. *)
  val struct_const : 's structure typ -> exp_box list -> 's structure exp

  (** {3 Pointer expressions *)

  (** Null pointer. *)
  val null : 'a ptr exp

  (** Pointer dereferencing. *)
  val deref : ('a ptr, [>`exp]) expr -> ('a,[<`exp|`var]) expr

  (** Pointer equality *)
  val (=*=) : ('a ptr, [>`exp]) expr -> ('a ptr, [>`exp]) expr -> bool exp

  (** Pointer disequality *)
  val (=!*=) : ('a ptr, [>`exp]) expr -> ('a ptr, [>`exp]) expr -> bool exp

  (** Array indexing. *)
  val (#@) : ('a array, [>`exp]) expr -> (int32, [>`exp]) expr -> ('a,[<`exp|`var]) expr

  (** Combined pointer dereference and structure field access. *)
  val (#->) : ('s structure ptr, [>`exp]) expr -> ('s, 'a) field -> ('a,[<`exp|`var]) expr

  (** {2 Commands} *)

  (** Representation of a command. A command represents some process
      for altering the current state. *)
  type comm

  (** The command that does nothing. *)
  val empty : comm

  (** Sequencing of commands. *)
  val (^^) : comm -> comm -> comm

  (** Assignment *)
  val (:=) : 'a var -> ('a,[>`exp]) expr -> comm

  (** While loops. *)
  val while_ : bool exp -> do_:comm -> comm

  (** Breaking out of a while loop. *)
  val break : comm

  (** If then else. *)
  val if_ : (bool, [>`exp]) expr -> then_:comm -> else_:comm ->  comm

  (** If then. *)
  val ifthen : (bool, [>`exp]) expr -> then_:comm -> comm

  (** Declare a new variable. Takes an optional name hint. *)
  val declare : ?name:string -> 'a typ -> ('a var -> comm) -> comm

  (** Declare a new variable with an expression for the initial
      value. Takes an optional name hint. *)
  val declare_init : ?name:string -> 'a typ -> ('a,[>`exp]) expr -> ('a var -> comm) -> comm

  (** Heap allocate some memory to hold values of a given type. *)
  val malloc : 'a ptr var -> 'a typ -> comm

  (** Heap allocate some memory to holds values of a given type, and
      dynamically some extra memory. *)
  val malloc_ext : 'a ptr var -> 'a typ -> (int32,[>`exp]) expr -> _ typ -> comm
  (* TODO: distinguish arbitrary length from fixed length structures somehow. *)

  (** Free heap allocated memory that came from malloc. *)
  val free : ('a ptr, [>`exp]) expr -> comm

  (** Print an integer to standard out. *)
  val print_int : (int32, [>`exp]) expr -> comm

  (** Print a newline to standard out. *)
  val print_newline : comm

  (** Print a (static) string to standard out. *)
  val print_str : string -> comm

  (** {3 Function declarations} *)

  type 'a arg_spec

  val return_void : comm arg_spec

  val return : 'a typ -> 'a exp arg_spec

  val (@->) : string * 'a typ -> 'b arg_spec -> ('a exp -> 'b) arg_spec

  val (@&->) : string * 'a typ -> 'b arg_spec -> ('a var -> 'b) arg_spec

  val declare_func : name:string -> typ:'t arg_spec -> body:'t -> 't
end

type 'a program =
  { generate : 'comm. (module S with type comm = 'comm) -> 'a -> 'comm }