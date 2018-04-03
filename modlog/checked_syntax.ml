module Core = struct
  include Core_syntax.Make (Modules.Syntax.Bound_names)

  let rec subst_deftype sub domtype =
    { domtype with
        domtype_data =
          match domtype.domtype_data with
            | Type_int          -> Type_int
            | Type_typename lid -> Type_typename (Modules.Subst.path sub lid)
            | Type_tuple tys    -> Type_tuple (List.map (subst_deftype sub) tys)
            | Type_enum syms    -> Type_enum syms
    }


  let subst_valtype sub = function
    | Predicate predty ->
       Predicate
         { predty with
             predty_data = List.map (subst_deftype sub) predty.predty_data
         }

    | Value domty ->
       Value (subst_deftype sub domty)

  let subst_kind sub () = ()
end

module Mod = Modules.Syntax.Mod_Syntax (Core)
