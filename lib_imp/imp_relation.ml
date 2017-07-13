module Make (S : Imp_syntax.S) = struct

  (* given an arity and a list of index patterns, create the
     implementation of a relation variable with those indexes. *)

  (*
  type ops =
    { insert : (int, 
*)

  let make arity search_patterns =
    ()

  (* Plan:
     - for each path in the patterns, order the attributes appropriately
     - generate a btree
     - Now:
       - searching: look up the search pattern and select the appropriate
         btree; invalid_arg otherwise
       - insertion: insert into all the trees (rearranging appropriately)
  *)
  
end
