%{
    open Core_syntax.SurfaceInnerSyntax
    open Core_syntax.SurfaceSyntax
    open Core_syntax
%}

%token MODULE TYPE SIG FUNCTOR STRUCT EQUALS WITH
%token DOT END AND REC
%token<string> IDENT
%token<string> SYMBOL

%token RPAREN LPAREN ARROW COLON STAR COMMA
%token RBRACE LBRACE
%token LSQBRACK RSQBRACK
%token CONJ DISJ FORALL EXISTS NEGATE
%token PRED AXIOM SORT CHECK
%token TRUE FALSE

%token SYNTH DISPLAY

%token EOF UNKNOWN

%start<Core_syntax.toplevel_item list> program

%%

program:
  | d=list(toplevel_item); EOF
    { d }

toplevel_item:
  | decl=str_item
    { Declaration decl }
  | c=command
    { Command c }

command:
  | SYNTH; nm=IDENT; COLON; mty=mod_type
    { Synth (nm, mty) }
  | DISPLAY; nm=longident
    { Display nm }

/* Long identifiers */

%public
longident:
  | id=IDENT
    { Lid_ident id }
  | lid=longident; DOT; f=IDENT
    { Lid_dot (lid, f) }

/* sort expressions */

sort_expr:
  | name=longident
    { SVar name }
/* FIXME: should probably unify enumerations and sums */
  | LBRACE; symbols=separated_list(COMMA, SYMBOL); RBRACE
    { Enumeration symbols }
  | LSQBRACK; variants=separated_list(COMMA, variant); RSQBRACK
    { Sum variants }
  | LPAREN; sorts=separated_list(STAR, sort_expr); RPAREN
    { Prod sorts }

variant:
  | lbl=SYMBOL; COLON; sort=sort_expr
    { lbl, sort }

/* value expressions */

value_expr:
  | var=IDENT
    { LocalVar var }
  | symbol=SYMBOL
    { Symbol symbol }
  | symbol=SYMBOL; e=value_expr
    { Variant (symbol, e) }
  | LPAREN; e=separated_list(COMMA, value_expr); RPAREN
    { Tuple e }

formula:
  | p=base_formula; CONJ; ps=separated_nonempty_list(CONJ, base_formula)
    { conjunction (p::ps) }
  | p=base_formula; DISJ; ps=separated_nonempty_list(DISJ, base_formula)
    { disjunction (p::ps) }
  | p=base_formula; ARROW; q=arrow_formula
    { Impl (p, q) }
  | FORALL; var=IDENT; COLON; sort=sort_expr; ARROW; p=formula
    { Forall (var, sort, p) }
  | EXISTS; var=IDENT; COLON; sort=sort_expr; ARROW; p=formula
    { Exists (var, sort, p) }
  | p=base_formula
    { p }

arrow_formula:
  | p=base_formula; ARROW; q=arrow_formula { Impl (p, q) }
  | p=base_formula { p }

base_formula:
  | TRUE
    { True }
  | FALSE
    { False }
  | pred=longident; LPAREN; exprs=separated_list(COMMA,value_expr); RPAREN
    { Atom (pred, exprs) }
  | e1=value_expr; EQUALS; e2=value_expr
    { Eq (e1, e2) }
  | NEGATE; p=base_formula
    { Not p }
  | LPAREN; p=formula; RPAREN
    { p }

%public
str_value:
  | CHECK; name=IDENT; COLON; property=formula
    { Check { name; property } }

%public
str_type(NAME):
  | SORT; id=NAME; EQUALS; expr=sort_expr
    { (id, Sort, SortDefn expr) }
  | PRED; id=NAME; COLON; sorts=separated_list(STAR, sort_expr); EQUALS;
    LPAREN; args=separated_list(COMMA, IDENT); RPAREN; DOT; predicate=formula
    { (id, Predicate sorts, PredDefn { args; predicate }) }

%public
sig_value:
  | AXIOM; id=IDENT; COLON; p=formula
    { (id, p) }

%public
sig_type:
  | SORT; id=IDENT
    { (id, Sort, None) }
  | PRED; id=IDENT; COLON; sorts=separated_list(STAR, sort_expr)
    { (id, Predicate sorts, None) }
  | manifest=str_type(IDENT)
    { let id, kind, decl = manifest in id, kind, Some decl }
