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
%token PRED AXIOM SORT CHECK FUNC
%token TRUE FALSE
%token EQUALS_EQUALS

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
  | LSQBRACK; variants=separated_list(COMMA, variant); RSQBRACK
    { Sum variants }
  | LPAREN; sorts=separated_list(STAR, sort_expr); RPAREN
    { Prod sorts }

variant:
  | lbl=SYMBOL; COLON; sort=sort_expr
    { lbl, sort }
  | lbl=SYMBOL
    { lbl, Prod [] }

/* value and formula expressions */

expr:
  | p=equality_expr; CONJ; ps=separated_nonempty_list(CONJ, equality_expr)
    { conjunction (p::ps) }
  | p=equality_expr; DISJ; ps=separated_nonempty_list(DISJ, equality_expr)
    { disjunction (p::ps) }
  | p=equality_expr; ARROW; q=arrow_expr
    { Impl (p, q) }
  | FORALL; var=IDENT; COLON; sort=sort_expr; ARROW; p=expr
    { Forall (var, sort, p) }
  | EXISTS; var=IDENT; COLON; sort=sort_expr; ARROW; p=expr
    { Exists (var, sort, p) }
  | p=equality_expr
    { p }

arrow_expr:
  | p=equality_expr; ARROW; q=arrow_expr
    { Impl (p, q) }
  | p=equality_expr
    { p }

equality_expr:
  | e1=base; EQUALS_EQUALS; e2=base
    { Eq (e1, e2) }
  | e=base
    { e }

base:
  | TRUE
    { True }
  | FALSE
    { False }
  | name=IDENT
    { Var name }
  | name=longident; LPAREN; exprs=separated_list(COMMA,expr); RPAREN
    { App (name, exprs) }
  | symbol=SYMBOL
    { Variant (symbol, Tuple []) }
  | symbol=SYMBOL; e=base (* FIXME: probably don't want to allow A B C to mean A (B (C)) *)
    { Variant (symbol, e) }
  | NEGATE; p=base
    { Not p }
  | LPAREN; p=separated_list(COMMA,expr); RPAREN
    { match p with [e] -> e | es -> Tuple es }

%public
str_value:
  | CHECK; name=IDENT; COLON; property=expr
    { Check { name; property } }
(*
  | PROVE; name=IDENT; COLON; property=formula; BY; p=proof
    { Proof { name; property; proof } }
*)

(*
(* Ooft, that's a lot of keywords! *)
proof:
  | INTRODUCE; n=IDENT; SEMICOLON; p=proof
  | TRUE; DOT
  | SPLIT; LBRACE; DASH; p1=proof; DASH; p2=proof; RBRACE
  | LEFT; SEMICOLON; p=proof
  | RIGHT; SEMICOLON; p=proof
  | EXISTS; v=value_expr; SEMICOLON; p=proof
  | NOTINTRO; nm=IDENT; SEMICOLON; p=proof
  | REFL
  | USE; nm=longident; SEMICOLON; p=proof
  | APPLY; LPAREN; p=proof; RPAREN; SEMICOLON; q=proof
  | INST; v=value_expr; SEMICOLON; p=proof
  | FIRST; SEMICOLON; p=proof
  | SECOND; SEMICOLON; p=proof
  | CASES; nm1=IDENT; nm2=IDENT; LBRACE; DASH; p1=proof; DASH; p2=proof; RBRACE
  | UNPACK; x=IDENT; h=IDENT; SEMICOLON; p=proof
  | FALSE
  | NOTELIM; p=proof
  | REWRITE; d=dir; SEMICOLON; p=proof
  | DONE
  | STORE; nm=IDENT; p=proof
(* AUTO ? Could this do more? E-matching, for instance? *)
(* CONTRADICTION? *)
(* Checked comments. *)
*)

%public
str_type(NAME):
  | SORT; id=NAME; EQUALS; expr=sort_expr
    { (id, Sort, SortDefn expr) }
  | PRED; id=NAME; COLON; sorts=separated_list(STAR, sort_expr); EQUALS;
    LPAREN; args=separated_list(COMMA, IDENT); RPAREN; DOT; body=expr
    { (id, Predicate sorts, ExprDefn { args; body }) }
  | FUNC; id=NAME; COLON; sorts=separated_list(STAR, sort_expr); ARROW; s=sort_expr; EQUALS;
    LPAREN; args=separated_list(COMMA, IDENT); RPAREN; DOT; body=expr
    { (id, Function (sorts, s), ExprDefn { args; body }) }
  | FUNC; id=NAME; COLON; s=sort_expr; EQUALS;
    LPAREN; args=separated_list(COMMA, IDENT); RPAREN; DOT; body=expr
    { (id, Function ([], s), ExprDefn { args; body }) }

%public
sig_value:
  | AXIOM; id=IDENT; COLON; p=expr
    { (id, Property p) }

%public
sig_type:
  | SORT; id=IDENT
    { (id, Sort, None) }
  | PRED; id=IDENT; COLON; sorts=separated_list(STAR, sort_expr)
    { (id, Predicate sorts, None) }
  | FUNC; id=IDENT; COLON; sorts=separated_list(STAR, sort_expr); ARROW; s=sort_expr
    { (id, Function (sorts, s), None) }
  | FUNC; id=IDENT; COLON; s=sort_expr
    { (id, Function ([], s), None) }
  | manifest=str_type(IDENT)
    { let id, kind, decl = manifest in id, kind, Some decl }
