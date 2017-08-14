%{
open Syntax.Core
open Syntax.Mod
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

%start <Syntax.Mod.structure> program

%%

program:
| d=list(str_item); EOF
    { d }

%inline
in_parens(X):
| LPAREN; x=X; RPAREN
    { x }

/* Long identifiers */

%public
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

%public
str_value:
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

%public
str_type(NAME):
| TYPE; id=NAME; EQUALS; ty=domain_type
    { (id, (), ty) }

%public
sig_value:
| id=IDENT; COLON; ty=predicate_type
    { (id, Predicate ty) }
| CONSTANT; id=IDENT; COLON; ty=domain_type
    { (id, Value ty) }

%public
sig_type:
| TYPE; id=IDENT
    { (id, (), None) }
| TYPE; id=IDENT; EQUALS; ty=domain_type
    { (id, (), Some ty ) }
