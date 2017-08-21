(*
Requirements:
- Tokens: DOT, IDENT, FUNCTOR, LPAREN, COLON, RPAREN, ARROW, SIG, END, WITH
          MODULE, STRUCT, TYPE, EQUALS
- Non terminals: longident, str_value, str_type(NAME), sig_value, sig_type
- An implementation of MOD_SYNTAX_RAW is open

Provides:
- Non terminals: sig_item, str_item

*)

%%

(* the module language *)

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
| mty=mod_type2; WITH; ty_constraint=str_type(separated_nonempty_list(DOT, IDENT))
    { let path, kind, ty = ty_constraint in
      { modtype_loc  = Location.mk $startpos $endpos
      ; modtype_data = Modtype_withtype (mty, path, kind, ty) } }
| LPAREN; mty=mod_type; RPAREN
    { mty }

%public
sig_item:
| v=sig_value
    { let (id, ty) = v in
      { sigitem_loc  = Location.mk $startpos $endpos
      ; sigitem_data = Sig_value (id, ty) } }
| t=sig_type
    { let (id, kind, manifest) = t in
      { sigitem_loc  = Location.mk $startpos $endpos
      ; sigitem_data = Sig_type (id, { kind; manifest }) } }
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

%public
str_item:
| d=str_value
    { { stritem_loc  = Location.mk $startpos $endpos
      ; stritem_data = Str_value d } }
| t=str_type(IDENT)
    { let (id, kind, ty) = t in
      { stritem_loc  = Location.mk $startpos $endpos
      ; stritem_data = Str_type (id, kind, ty) } }
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
