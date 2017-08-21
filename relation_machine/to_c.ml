open Syntax

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

      c) Merge { src; tgt }: assume that 'tgt' is an intensional relvar,
         and 'src' is a flat relvar. Iterate over 'src', inserting into 'tgt'.

      d) Move { src; tgt }: assume that they are both flat, and do src := tgt; tgt := null;

      e) Declare (relvars, body): initialise several new 'flat' relvars
*)

(*
module Gen (S : Imp_syntax.S) = struct
  open S

  type pat = [ `Fixed of int exp | `Reqd | `Ignore ]

  type db_relvar_info =
    { insert   : int exp list -> comm
    ; search   : pat list -> (int exp list -> comm) -> comm
    ; ifmember : int exp option -> comm -> comm -> comm
    }

  (* Given an idb relvar with search patterns, create an 'object' with
     the appropriate 
  *)

  (* Given a temporary relation, create a new 'block_list' with the
     appropriate iterate and insert 'methods'. *)
end
*)
