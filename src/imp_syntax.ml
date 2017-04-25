module type S = sig
  type ('a,_) expr
  type 'a exp = ('a,[`exp]) expr
  type 'a var = ('a,[`var|`exp]) expr
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

  val true_  : (bool, [`exp]) expr
  val false_ : (bool, [`exp]) expr
  val ( && ) : (bool, [>`exp]) expr -> (bool, [>`exp]) expr -> (bool, [`exp]) expr
  val ( || ) : (bool, [>`exp]) expr -> (bool, [>`exp]) expr -> (bool, [`exp]) expr
  val not    : (bool, [>`exp]) expr -> (bool, [>`exp]) expr

  val const : int32 -> int exp
  val ( < ) : (int, [>`exp]) expr -> (int, [>`exp]) expr -> bool exp
  val ( > ) : (int, [>`exp]) expr -> (int, [>`exp]) expr -> bool exp
  val ( >= ) : (int, [>`exp]) expr -> (int, [>`exp]) expr -> bool exp
  val ( <= ) : (int, [>`exp]) expr -> (int, [>`exp]) expr -> bool exp
  val ( == ) : (int, [>`exp]) expr -> (int, [>`exp]) expr -> bool exp
  val ( != ) : (int, [>`exp]) expr -> (int, [>`exp]) expr -> bool exp
  val ( + ) : (int, [>`exp]) expr -> (int, [>`exp]) expr -> int exp
  val ( - ) : (int, [>`exp]) expr -> (int, [>`exp]) expr -> int exp

  val empty : comm

  val (^^) : comm -> comm -> comm

  val (:=) : ('a,[`exp|`var]) expr -> ('a,[>`exp]) expr -> comm

  val while_ : bool exp -> do_:comm -> comm

  val break : comm

  val if_ : (bool, [>`exp]) expr -> then_:comm -> else_:comm ->  comm

  val ifthen : (bool, [>`exp]) expr -> then_:comm -> comm

  val declare : 'a typ -> (('a,[`exp|`var]) expr -> comm) -> comm

  val malloc : ('a ptr, [`exp|`var]) expr -> 'a typ -> comm

  val (#@) : ('a array, [>`exp]) expr -> (int, [>`exp]) expr -> ('a,[<`exp|`var]) expr

  val deref : ('a ptr, [>`exp]) expr -> ('a,[<`exp|`var]) expr

  val (#.) : ('s structure, [>`exp]) expr -> ('s, 'a) field -> ('a,[<`exp|`var]) expr

  val (#->) : ('s structure ptr, [>`exp]) expr -> ('s, 'a) field -> ('a,[<`exp|`var]) expr
end


