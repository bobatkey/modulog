%{
open Datalog_syntax
%}

%token COMMA
%token COLON_DASH
%token COLON
%token STAR DOT ARROW EQUALS BAR UNDERSCORE
%token MODULE TYPE STRUCT SIG END FUNCTOR INT AND DEF CONSTANT WITH
%token LPAREN RPAREN LBRACE RBRACE
%token<int32> INT_LITERAL
%token<string> IDENT ENUM_IDENT MV_IDENT
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
| v=MV_IDENT
    { { expr_loc  = Location.mk $startpos $endpos
      ; expr_data = Expr_var v } }
| lid=longident
    { { expr_loc  = Location.mk $startpos $endpos
      ; expr_data = Expr_lid lid } }
| i=INT_LITERAL
    { { expr_loc  = Location.mk $startpos $endpos
      ; expr_data = Expr_literal i } }
| UNDERSCORE
    { { expr_loc  = Location.mk $startpos $endpos
      ; expr_data = Expr_underscore } }
| es=in_parens(separated_list(COMMA,expr))
    { { expr_loc  = Location.mk $startpos $endpos
      ; expr_data = Expr_tuple es } }
| sym=ENUM_IDENT
    { { expr_loc  = Location.mk $startpos $endpos
      ; expr_data = Expr_enum sym } }

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

def:
| DEF; d=pred_decl; ds=list(AND; d=pred_decl {d})
    { PredicateDefs (d :: ds) }
| CONSTANT; name=IDENT; COLON; ty=domain_type; EQUALS; e=expr
    { ConstantDef { const_loc  = Location.mk $startpos $endpos
                  ; const_name = name
                  ; const_type = ty
                  ; const_expr = e
                  } }

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
| LBRACE; syms=separated_list(BAR, ENUM_IDENT); RBRACE
    { { domtype_loc  = Location.mk $startpos $endpos
      ; domtype_data = Type_enum syms } }

predicate_type:
| dtys=separated_nonempty_list(STAR, domain_type0)
    { { predty_loc  = Location.mk $startpos $endpos
      ; predty_data = dtys } }

/* the module language */

with_path:
| mod_path=separated_nonempty_list(DOT, IDENT)
    { mod_path }

mod_type:
| FUNCTOR; LPAREN; id=IDENT; COLON; mty1=mod_type; RPAREN; ARROW; mty2=mod_type
    { { modtype_loc  = Location.mk $startpos $endpos
      ; modtype_data = Modtype_functor (id, mty1, mty2) } }
| mty=mod_type2
    { mty }

mod_type2:
| lid=longident
    { { modtype_loc  = Location.mk $startpos $endpos
      ; modtype_data = Modtype_longident lid } }
| SIG; s=list(sig_item); END
    { { modtype_loc  = Location.mk $startpos $endpos
      ; modtype_data = Modtype_signature s } }
| mty=mod_type2; WITH; TYPE; path=with_path; EQUALS; ty=domain_type
    { { modtype_loc  = Location.mk $startpos $endpos
      ; modtype_data = Modtype_withtype (mty, path, (), ty) } }
| LPAREN; mty=mod_type; RPAREN
    { mty }

sig_item:
| id=IDENT; COLON; ty=predicate_type
    { { sigitem_loc  = Location.mk $startpos $endpos
      ; sigitem_data = Sig_value (id, Predicate ty) } }
| CONSTANT; id=IDENT; COLON; ty=domain_type
    { { sigitem_loc  = Location.mk $startpos $endpos
      ; sigitem_data = Sig_value (id, Value ty) } }
| TYPE; id=IDENT
    { { sigitem_loc  = Location.mk $startpos $endpos
      ; sigitem_data = Sig_type (id, { kind = (); manifest = None }) } }
| TYPE; id=IDENT; EQUALS; ty=domain_type
    { { sigitem_loc  = Location.mk $startpos $endpos
      ; sigitem_data = Sig_type (id, { kind = (); manifest = Some ty }) } }
| MODULE; id=IDENT; mty=functor_type_decls
    { { sigitem_loc  = Location.mk $startpos $endpos
      ; sigitem_data = Sig_module (id, mty) } }
| MODULE; TYPE; id=IDENT; EQUALS; mty=mod_type
    { { sigitem_loc  = Location.mk $startpos $endpos
      ; sigitem_data = Sig_modty (id, mty) } }

functor_type_decls:
| COLON; mty=mod_type
    { mty }
| LPAREN; id=IDENT; COLON; mty1=mod_type; RPAREN; mty2=functor_type_decls
    { { modtype_loc  = Location.mk $startpos $endpos
      ; modtype_data = Modtype_functor (id, mty1, mty2) } }

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
| d=def
    { { stritem_loc  = Location.mk $startpos $endpos
      ; stritem_data = Str_value d } }
| TYPE; id=IDENT; EQUALS; ty=domain_type
    { { stritem_loc  = Location.mk $startpos $endpos
      ; stritem_data = Str_type (id, (), ty) } }
| MODULE; id=IDENT; modl=functor_decls
    { { stritem_loc  = Location.mk $startpos $endpos
      ; stritem_data = Str_module (id, modl) } }
| MODULE; TYPE; id=IDENT; EQUALS; mty=mod_type
    { { stritem_loc  = Location.mk $startpos $endpos
      ; stritem_data = Str_modty (id, mty) } }

functor_decls:
| EQUALS; modl=mod_term
    { modl }
| LPAREN; id=IDENT; COLON; mty=mod_type; RPAREN; modl=functor_decls
    { { modterm_loc  = Location.mk $startpos $endpos
      ; modterm_data = Mod_functor (id, mty, modl) } }
