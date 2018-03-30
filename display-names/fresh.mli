
(** [choose used base] returns a string [s] such that [used s =
    false]. The argument [base] is used as a hint for the form of the
    string to be generated. In order for this function to terminate,
    [used] must be finitely supported. *)
val choose : (string -> bool) -> string -> string
