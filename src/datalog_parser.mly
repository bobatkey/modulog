%{
open Datalog_syntax

%}

%token COMMA
%token COLON_DASH
%token COLON
%token STAR DOT ARROW EQUALS
%token MODULE TYPE STRUCT SIG END FUNCTOR INT AND DEF UNDERSCORE
%token LPAREN RPAREN
%token<int32> INT_LITERAL
%token<string> IDENT
%token EOF

%start <Datalog_syntax.structure> program

%%

program:
| d=list(str_item); EOF
    { d }

%inline
in_parens(X):
| LPAREN; x=X; RPAREN
    { x }

/* Long identifiers */

longident:
| id=IDENT
    { Lid_ident id }
| lid=longident; DOT; f=IDENT
    { Lid_dot (lid, f) }

/* Datalog rules */

expr:
| v=IDENT
    { { expr_loc  = Location.mk $startpos $endpos
      ; expr_data = Expr_var v } }
| i=INT_LITERAL
    { { expr_loc  = Location.mk $startpos $endpos
      ; expr_data = Expr_literal i } }
| UNDERSCORE
    { { expr_loc  = Location.mk $startpos $endpos
      ; expr_data = Expr_underscore } }
| es=in_parens(separated_list(COMMA,expr))
    { { expr_loc  = Location.mk $startpos $endpos
      ; expr_data = Expr_tuple es } }

atom:
| pred=longident; args=in_parens(separated_list(COMMA,expr))
    { { atom_loc  = Location.mk $startpos $endpos
      ; atom_data = Atom_predicate { pred; args } } }

decl_head:
| name=IDENT; exprs=in_parens(separated_list(COMMA,expr))
    { (name, exprs) }

rule:
| head=decl_head; COLON_DASH; rhs=separated_nonempty_list(COMMA, atom)
    { { rule_loc  = Location.mk $startpos $endpos
      ; rule_pred = fst head
      ; rule_args = snd head
      ; rule_rhs  = rhs } }
| head=decl_head
    { { rule_loc  = Location.mk $startpos $endpos
      ; rule_pred = fst head
      ; rule_args = snd head
      ; rule_rhs  = [] } }

pred_decl:
| name=IDENT; COLON; types=predicate_type; rules=list(rule)
    { { decl_loc   = Location.mk $startpos $endpos
      ; decl_name  = name
      ; decl_type  = types
      ; decl_rules = rules } }

decl:
| DEF; d=pred_decl; ds=list(AND; d=pred_decl {d})
    { d :: ds }

/* types */

domain_type:
| dtys=separated_nonempty_list(STAR, domain_type0)
    { match dtys with
        | []    -> assert false
        | [dty] -> dty
        | dtys  -> { domtype_loc  = Location.mk $startpos $endpos
                   ; domtype_data = Type_tuple dtys } }

domain_type0:
| INT
    { { domtype_loc  = Location.mk $startpos $endpos
      ; domtype_data = Type_int } }
| lid=longident
    { { domtype_loc  = Location.mk $startpos $endpos
      ; domtype_data = Type_typename lid } }
| LPAREN; RPAREN
    { { domtype_loc  = Location.mk $startpos $endpos
      ; domtype_data = Type_tuple [] } }
| LPAREN; dty=domain_type; RPAREN
    { dty }

predicate_type:
| dtys=separated_nonempty_list(STAR, domain_type0)
    { { predty_loc  = Location.mk $startpos $endpos
      ; predty_data = dtys } }

/* the module language */

mod_type:
| lid=longident
    { { modtype_loc  = Location.mk $startpos $endpos
      ; modtype_data = Modtype_longident lid } }
| SIG; s=list(sig_item); END
    { { modtype_loc  = Location.mk $startpos $endpos
      ; modtype_data = Modtype_signature s } }
| FUNCTOR; LPAREN; id=IDENT; COLON; mty1=mod_type; RPAREN; ARROW; mty2=mod_type
    { { modtype_loc  = Location.mk $startpos $endpos
      ; modtype_data = Modtype_functor (id, mty1, mty2) } }

sig_item:
| id=IDENT; COLON; ty=predicate_type
    { { sigitem_loc  = Location.mk $startpos $endpos
      ; sigitem_data = Sig_value (id, ty) } }
| TYPE; id=IDENT
    { { sigitem_loc  = Location.mk $startpos $endpos
      ; sigitem_data = Sig_type (id, { kind = (); manifest = None }) } }
| TYPE; id=IDENT; EQUALS; ty=domain_type
    { { sigitem_loc  = Location.mk $startpos $endpos
      ; sigitem_data = Sig_type (id, { kind = (); manifest = Some ty }) } }
| MODULE; id=IDENT; COLON; mty=mod_type
    { { sigitem_loc  = Location.mk $startpos $endpos
      ; sigitem_data = Sig_module (id, mty) } }
| MODULE; TYPE; id=IDENT; EQUALS; mty=mod_type
    { { sigitem_loc  = Location.mk $startpos $endpos
      ; sigitem_data = Sig_modty (id, mty) } }

mod_term:
| FUNCTOR; LPAREN; id=IDENT; COLON; mty=mod_type; RPAREN; ARROW; modl=mod_term
    { { modterm_loc  = Location.mk $startpos $endpos
      ; modterm_data = Mod_functor (id, mty, modl) } }
| m=mod_term2
    { m }

mod_term2:
| mod1=mod_term2; LPAREN; mod2=mod_term; RPAREN
    { { modterm_loc  = Location.mk $startpos $endpos
      ; modterm_data = Mod_apply (mod1, mod2) } }
| lid=longident
    { { modterm_loc  = Location.mk $startpos $endpos
      ; modterm_data = Mod_longident lid } }
| STRUCT; items=list(str_item); END
    { { modterm_loc  = Location.mk $startpos $endpos
      ; modterm_data = Mod_structure items } }
| LPAREN; modl=mod_term; COLON; mty=mod_type; RPAREN
    { { modterm_loc  = Location.mk $startpos $endpos
      ; modterm_data = Mod_constraint (modl, mty) } }
| LPAREN; m=mod_term; RPAREN
    { m }

str_item:
| d=decl
    { { stritem_loc  = Location.mk $startpos $endpos
      ; stritem_data = Str_value d } }
| TYPE; id=IDENT; EQUALS; ty=domain_type
    { { stritem_loc  = Location.mk $startpos $endpos
      ; stritem_data = Str_type (id, (), ty) } }
| MODULE; id=IDENT; EQUALS; modl=mod_term
    { { stritem_loc  = Location.mk $startpos $endpos
      ; stritem_data = Str_module (id, modl) } }
| MODULE; TYPE; id=IDENT; EQUALS; mty=mod_type
    { { stritem_loc  = Location.mk $startpos $endpos
      ; stritem_data = Str_modty (id, mty) } }
