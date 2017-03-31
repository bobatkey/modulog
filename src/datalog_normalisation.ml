module Norm = struct
  module Core = Datalog_checker.Core_syntax

  open Core

  let subst_atom sub = function
    | {atom_data = Atom_predicate { pred; args }} as atom ->
       { atom with atom_data = Atom_predicate { pred = Modules_subst.path sub pred
                                              ; args }
       }

  let subst_rule sub rule =
    { rule with rule_rhs = List.map (subst_atom sub) rule.rule_rhs }
    
  let subst_decl sub decl =
    {decl with
       decl_type = Core.subst_valtype sub decl.decl_type
     ; decl_rules = List.map (subst_rule sub) decl.decl_rules
    }
  
  let subst_term sub decls =
    List.map (subst_decl sub) decls

end

include Modules_normalisation.Mod_normalise (Datalog_checker.Mod) (Norm)

