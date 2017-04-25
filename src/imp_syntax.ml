module type S = sig
  type 'a exp
  type 'a var
  type comm

  type 'a typ
  type 'a ptr
  type 'a array
  type 'a structure

  val int : int typ
  val bool : bool typ
  val ptr : 'a typ -> 'a ptr typ
  val array : 'a typ -> int32 -> 'a array typ

  type ('s, 'a) field

  (* Creation of types; a Ctypes style interface *)
  val structure : string -> 's structure typ
  val field     : 's structure typ -> string -> 'a typ -> ('s, 'a) field
  val seal      : 's structure typ -> unit

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

  val (:=) : 'a var -> 'a exp -> comm

  val while_ : bool exp -> comm -> comm
  val break : comm

  val ifthenelse : bool exp -> then_:comm -> else_:comm -> comm
  val ifthen : bool exp -> comm -> comm

  val declare : 'a typ -> ('a var -> comm) -> comm

  val malloc : 'a ptr var -> 'a typ -> comm

  val (#@) : 'a array exp -> int exp -> 'a var

  val deref : 'a ptr exp -> 'a var

  val (#.) : 's structure exp -> ('s, 'a) field -> 'a var

  val (#->) : 's structure ptr exp -> ('s, 'a) field -> 'a var
end


