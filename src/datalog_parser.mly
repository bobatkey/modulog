%{
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

%start <rule list> program

%%

program:
  d=list(str_item); EOF { d }

expr:
  v=IDENT       { Var v }
| i=INT_LITERAL { Lit i }
| UNDERSCORE    { Underscore }

atom:
  pred=IDENT; LPAREN; exprs=separated_list(COMMA,expr); RPAREN
   { { atm_pred=pred; atm_args=exprs } }

rule_decl:
  head=atom; COLON_DASH; rhs=separated_nonempty_list(COMMA, atom)
    { { head ; rhs } }
| head=atom
    { { head; rhs = [] } }

pred_decl:
  name=IDENT; COLON; types=separated_nonempty_list(STAR, typ);
  rules=list(rule_decl)
    { (name, types, rules) }

decl:
  DEF; d=pred_decl; ds=list(mutual_decl) { (d, ds) }

mutual_decl:
  AND; d=pred_decl { d }

typ:
  INT                                          { Int }
| lid=longident                                { Typename lid }
| LPAREN; ts=separated_list(STAR, typ); RPAREN { Tuple ts }

/* the module language */

longident:
  id=IDENT                    { Pident id }
| lid=longident; DOT; f=IDENT { Pdot (lid, f) }

mod_type:
  p=longident                { Mty_longident p }
| SIG; s=list(sig_item); END { Signature s }
| FUNCTOR; LPAREN; id=IDENT; COLON; mty1=mod_type; RPAREN; ARROW; mty2=mod_type
                             { Functor_type (id, mty1, mty2) }

sig_item:
  id=IDENT; COLON; ty=typ         { Sig_value (id, ty) }
| TYPE; id=IDENT                  { Sig_type (id, { kind = (); decl = None }) }
| TYPE; id=IDENT; EQUALS; ty=typ  { Sig_type (id, { kind = (); decl = Some ty}) }
| MODULE; id=IDENT; COLON; mty=mod_type
                                  { Sig_module (id, mty) }
| MODULE; TYPE; id=IDENT; EQUALS; mty=mod_type
                                  { Sig_modty (id, mty) }

mod_term:
| FUNCTOR; LPAREN; id=IDENT; COLON; mty=mod_type; RPAREN; ARROW; modl=mod_term
                                    { Functor (id, mty, modl) }
| m=mod_term2                       { m }

mod_term2:
| mod1=mod_term2; LPAREN; mod2=mod_term; RPAREN
                                    { Apply (mod1, mod2) }
| lid=longident                     { Longident lid }
| STRUCT; items=list(str_item); END { Structure items }
| LPAREN; modl=mod_term; COLON; mty=mod_type; RPAREN
                                    { Constraint (modl, mty) }
| LPAREN; m=mod_term; RPAREN        { m }

str_item:
  d=decl                          { Str_value d }
| TYPE; id=IDENT; EQUALS; ty=typ  { Str_type (id, (), ty) }
| MODULE; id=IDENT; EQUALS; modl=mod_term
                                  { Str_module (id, modl) }
| MODULE; TYPE; id=IDENT; EQUALS; mty=mod_type
                                  { Str_modty (id, mty) }
