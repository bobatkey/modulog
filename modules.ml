module type IDENT = sig
  type t
  val create : string -> t
  val name : t -> string
  val equal : t -> t -> bool
  type 'a tbl
  val emptytbl : 'a tbl
  val add : t -> 'a -> 'a tbl -> 'a tbl
  val find : t -> 'a tbl -> 'a (* may throw Not_found *)
end

module Ident : IDENT = struct
  type t = {name : string; stamp : int}
  let currstamp = ref 0
  let create s =
    incr currstamp; {name = s; stamp = !currstamp}
  let name id = id.name
  let equal id1 id2 = id1.stamp = id2.stamp
  type 'a tbl = (t * 'a) list
  let emptytbl = []
  let add id data tbl = (id, data)::tbl
  let rec find id1 = function
    | [] -> raise Not_found
    | (id2, data) :: tbl ->
       if equal id1 id2 then data else find id1 tbl
end

type path =
  | Pident of Ident.t
  | Pdot   of path * string

let rec path_equal p1 p2 =
  match p1, p2 with
    | Pident id1, Pident id2 -> Ident.equal id1 id2
    | Pdot (r1, field1), Pdot (r2, field2) ->
       path_equal r1 r2 && field1 = field2
    | _, _ ->
       false

module type SUBST = sig
  type t
  val identity : t
  val add : Ident.t -> path -> t -> t
  val path : path -> t -> path
end

module Subst : SUBST = struct
  type t = path Ident.tbl
  let identity = Ident.emptytbl
  let add = Ident.add
  let rec path p sub =
    match p with
      | Pident id -> (try Ident.find id sub with Not_found -> p)
      | Pdot (root, field) -> Pdot (path root sub, field)
end

module type CORE_SYNTAX = sig
  type term
  type val_type
  type def_type
  type kind
  val subst_valtype : val_type -> Subst.t -> val_type
  val subst_deftype : def_type -> Subst.t -> def_type
  val subst_kind : kind -> Subst.t -> kind
end

module type MOD_SYNTAX = sig
  module Core : CORE_SYNTAX

  type type_decl =
    { kind     : Core.kind
    ; manifest : Core.def_type option
    }

  type mod_type =
    | Signature    of signature
    | Functor_type of Ident.t * mod_type * mod_type
  and signature = specification list
  and specification =
    | Value_sig  of Ident.t * Core.val_type
    | Type_sig   of Ident.t * type_decl
    | Module_sig of Ident.t * mod_type

  type mod_term =
    | Longident  of path
    | Structure  of structure
    | Functor    of Ident.t * mod_type * mod_term
    | Apply      of mod_term * mod_term
    | Constraint of mod_term * mod_type
  and structure = definition list
  and definition =
    | Value_str  of Ident.t * Core.term
    | Type_str   of Ident.t * Core.kind * Core.def_type
    | Module_str of Ident.t * mod_term

  val subst_typedecl : type_decl -> Subst.t -> type_decl

  val subst_modtype  : mod_type -> Subst.t -> mod_type
end

module Mod_Syntax (Core_syntax : CORE_SYNTAX) =
struct
  module Core = Core_syntax

  type type_decl =
    { kind     : Core.kind
    ; manifest : Core.def_type option
    }

  type mod_type =
    | Signature    of signature
    | Functor_type of Ident.t * mod_type * mod_type
  and signature = specification list
  and specification =
    | Value_sig  of Ident.t * Core.val_type
    | Type_sig   of Ident.t * type_decl
    | Module_sig of Ident.t * mod_type

  type mod_term =
    | Longident  of path
    | Structure  of structure
    | Functor    of Ident.t * mod_type * mod_term
    | Apply      of mod_term * mod_term
    | Constraint of mod_term * mod_type
  and structure = definition list
  and definition =
    | Value_str  of Ident.t * Core.term
    | Type_str   of Ident.t * Core.kind * Core.def_type
    | Module_str of Ident.t * mod_term

  let subst_typedecl decl sub =
    { kind     = Core.subst_kind decl.kind sub
    ; manifest =
        match decl.manifest with
          | None     -> None
          | Some dty -> Some (Core.subst_deftype dty sub)
    }

  let rec subst_modtype mty sub = match mty with
    | Signature sg -> Signature (List.map (subst_sig_item sub) sg)
    | Functor_type (id, mty1, mty2) ->
       Functor_type (id, subst_modtype mty1 sub, subst_modtype mty2 sub)
  and subst_sig_item sub = function
    | Value_sig (id, vty)  -> Value_sig (id, Core.subst_valtype vty sub)
    | Type_sig (id, decl)  -> Type_sig (id, subst_typedecl decl sub)
    | Module_sig (id, mty) -> Module_sig (id, subst_modtype mty sub)

end

module type ENV = sig
  module Mod : MOD_SYNTAX

  type t

  val empty : t

  val add_value : Ident.t -> Mod.Core.val_type -> t -> t

  val add_type : Ident.t -> Mod.type_decl -> t -> t

  val add_module : Ident.t -> Mod.mod_type -> t -> t

  val add_spec : Mod.specification -> t -> t

  val add_signature : Mod.signature -> t -> t

  val find_value : path -> t -> Mod.Core.val_type

  val find_type : path -> t -> Mod.type_decl

  val find_module : path -> t -> Mod.mod_type
end

module Env (Mod_syntax : MOD_SYNTAX) = struct
  module Mod = Mod_syntax

  type binding =
    | Value  of Mod.Core.val_type
    | Type   of Mod.type_decl
    | Module of Mod.mod_type

  type t = binding Ident.tbl

  let empty = Ident.emptytbl

  let add_value id vty env = Ident.add id (Value vty) env
  let add_type id decl env = Ident.add id (Type decl) env
  let add_module id mty env = Ident.add id (Module mty) env
  let add_spec item env =
    match item with
      | Mod.Value_sig (id, vty) -> add_value id vty env
      | Mod.Type_sig (id, decl) -> add_type id decl env
      | Mod.Module_sig (id, mty) -> add_module id mty env
  let add_signature = List.fold_right add_spec

  let rec find path env =
    match path with
      | Pident id ->
         Ident.find id env
      | Pdot (root, field) ->
         match find_module root env with
           | Mod.Signature sg -> find_field root field Subst.identity sg
           | _ -> failwith "structure expected in dot access"
  and find_field p field sub = function
    | [] -> failwith "no such field in structure"
    | Mod.Value_sig (id, vty) :: rem ->
       if Ident.name id = field
       then Value (Mod.Core.subst_valtype vty sub)
       else find_field p field sub rem
    | Mod.Type_sig (id, decl) :: rem ->
       if Ident.name id = field
       then Type (Mod.subst_typedecl decl sub)
       else find_field p field (Subst.add id (Pdot(p,Ident.name id)) sub) rem
    | Mod.Module_sig (id, mty) :: rem ->
       if Ident.name id = field
       then Module (Mod.subst_modtype mty sub)
       else find_field p field (Subst.add id (Pdot(p,Ident.name id)) sub) rem
  and find_value path env =
    match find path env with
      | Value vty -> vty | _ -> failwith "value field expected"
  and find_type path env =
    match find path env with
      | Type decl -> decl | _ -> failwith "type field expected"
  and find_module path env =
    match find path env with
      | Module mty -> mty | _ -> failwith "module field expected"

end

module type CORE_TYPING = sig
  module Core : CORE_SYNTAX
  module Env  : ENV with module Mod.Core = Core

  val type_term : Env.t -> Core.term -> Core.val_type
  val kind_deftype : Env.t -> Core.def_type -> Core.kind
  val check_valtype : Env.t -> Core.val_type -> unit
  val check_kind : Env.t -> Core.kind -> unit

  val valtype_match : Env.t -> Core.val_type -> Core.val_type -> bool
  val deftype_equiv :
    Env.t -> Core.kind -> Core.def_type -> Core.def_type -> bool
  val kind_match : Env.t -> Core.kind -> Core.kind -> bool
  val deftype_of_path : path -> Core.kind -> Core.def_type
end

module type MOD_TYPING = sig
  module Mod : MOD_SYNTAX
  module Env : ENV with module Mod = Mod
  val type_module : Env.t -> Mod.mod_term -> Mod.mod_type
  val type_definition : Env.t -> Mod.definition -> Mod.specification
end

module Mod_typing
    (TheMod : MOD_SYNTAX)
    (TheEnv : ENV with module Mod = TheMod)
    (CT : CORE_TYPING with module Core = TheMod.Core and module Env = TheEnv)
= struct
  module Mod = TheMod
  module Env = TheEnv

  open Mod

  let rec modtype_match env mty1 mty2 =
    match mty1, mty2 with
      | Signature sig1, Signature sig2 ->
         let paired_components, subst =
           pair_signature_components sig1 sig2
         in
         let ext_env = Env.add_signature sig1 env in
         List.iter (specification_match ext_env subst) paired_components
      | Functor_type (param1, arg1, res1), Functor_type (param2, arg2, res2) ->
         let subst = Subst.add param1 (Pident param2) Subst.identity in
         let res1' = Mod.subst_modtype res1 subst in
         modtype_match env arg2 arg1;
         modtype_match (Env.add_module param2 arg2 env) res1' res2
      | _, _ ->
         failwith "module type mismatch"

  and pair_signature_components sig1 sig2 =
    match sig2 with
      | [] ->
         [], Subst.identity
      | item2 :: rem2 ->
         let rec find_matching_component = function
           | [] -> failwith "unmatched signature component"
           | item1 :: rem1 ->
              match item1, item2 with
                | Value_sig (id1, _), Value_sig (id2, _)
                | Type_sig (id1, _), Type_sig (id2, _)
                | Module_sig (id1, _), Module_sig (id2, _)
                  when Ident.name id1 = Ident.name id2 ->
                   (id1, id2, item1)
                | _ ->
                   find_matching_component rem1
         in
         let (id1, id2, item1) = find_matching_component sig1 in
         let (pairs, subst) = pair_signature_components sig1 rem2 in
         ((item1, item2) :: pairs, Subst.add id2 (Pident id1) subst)

  and specification_match env subst = function
    | Value_sig (_, vty1), Value_sig (_, vty2) ->
       if not (CT.valtype_match env vty1 (Core.subst_valtype vty2 subst))
       then failwith "value components do not match"
    | Type_sig (id, decl1), Type_sig (_, decl2) ->
       if not (typedecl_match env id decl1 (Mod.subst_typedecl decl2 subst))
       then failwith "type components do not match"
    | Module_sig (_, mty1), Module_sig (_, mty2) ->
       modtype_match env mty1 (Mod.subst_modtype mty2 subst)
    | _, _ ->
       assert false
  and typedecl_match env id decl1 decl2 =
    CT.kind_match env decl1.kind decl2.kind &&
    (match decl1.manifest, decl2.manifest with
      | _, None -> true
      | Some typ1, Some typ2 ->
         CT.deftype_equiv env decl2.kind typ1 typ2
      | None, Some typ2 ->
         CT.deftype_equiv env decl2.kind
           (CT.deftype_of_path (Pident id) decl1.kind)
           typ2)

  let rec strengthen_modtype path mty =
    match mty with
      | Signature sg -> Signature (List.map (strengthen_spec path) sg)
      | Functor_type _ -> mty
  and strengthen_spec path item =
    match item with
      | Value_sig (id, vty) -> item
      | Type_sig (id, decl) ->
         let m =
           match decl.manifest with
             | None ->
                Some (CT.deftype_of_path (Pdot (path, Ident.name id)) decl.kind)
             | Some ty ->
                Some ty
         in
         Type_sig (id, {decl with manifest = m })
      | Module_sig (id, mty) ->
         Module_sig (id, strengthen_modtype (Pdot (path, Ident.name id)) mty)
           

  let rec check_modtype env = function
    | Signature sg ->
       check_signature env [] sg
    | Functor_type (param, arg, res) ->
       check_modtype env arg;
       check_modtype (Env.add_module param arg env) res
  and check_signature env seen = function
    | [] -> ()
    | Value_sig (id, vty) :: rem ->
       if List.mem (Ident.name id) seen
       then failwith "repeated value name";
       CT.check_valtype env vty;
       check_signature env (Ident.name id :: seen) rem
    | Type_sig (id, decl) :: rem ->
       if List.mem (Ident.name id) seen
       then failwith "repeated type name";
       CT.check_kind env decl.kind;
       begin match decl.manifest with
         | None -> ()
         | Some typ ->
            if not (CT.kind_match env (CT.kind_deftype env typ) decl.kind)
            then failwith "kind mismatch in manifest type specification"
       end;
       check_signature (Env.add_type id decl env) (Ident.name id :: seen) rem
    | Module_sig (id, mty) :: rem ->
       if List.mem (Ident.name id) seen
       then failwith "repeated module name";
       check_modtype env mty;
       check_signature (Env.add_module id mty env) (Ident.name id :: seen) rem

  let rec type_module env = function
    | Longident path ->
       strengthen_modtype path (Env.find_module path env)
    | Structure str ->
       Signature (type_structure env [] str)
    | Functor (param, mty, body) ->
       check_modtype env mty;
       Functor_type (param, mty, type_module (Env.add_module param mty env) body)
    | Apply (funct, (Longident path as arg)) ->
       (match type_module env funct with
         | Functor_type (param, mty_param, mty_res) ->
            let mty_arg = type_module env arg in
            modtype_match env mty_arg mty_param;
            subst_modtype mty_res (Subst.add param path Subst.identity)
         | _ ->
            failwith "application of a non-functor")
    | Apply (funct, arg) ->
       failwith "application of a functor to a non-path"
    | Constraint (modl, mty) ->
       check_modtype env mty;
       modtype_match env (type_module env modl) mty;
       mty
  and type_structure env seen = function
    | [] -> []
    | stritem :: rem ->
       let sigitem, seen' = type_definition env seen stritem in
       sigitem :: type_structure (Env.add_spec sigitem env) seen' rem
  and type_definition env seen = function
    | Value_str (id, term) ->
       if List.mem (Ident.name id) seen
       then failwith "repeated value name";
       (Value_sig (id, CT.type_term env term), Ident.name id :: seen)
    | Module_str (id, modl) ->
       if List.mem (Ident.name id) seen
       then failwith "repeated module name";
       (Module_sig (id, type_module env modl), Ident.name id :: seen)
    | Type_str (id, kind, typ) ->
       if List.mem (Ident.name id) seen
       then failwith "repeated type name";
       CT.check_kind env kind;
       if not (CT.kind_match env (CT.kind_deftype env typ) kind)
       then failwith "kind mismatch in type definition";
       (Type_sig (id, {kind = kind; manifest = Some typ}),
        Ident.name id :: seen)
end

(*
module type Edges = sig
  type vertex

  pred edge : vertex * vertex
end

module MyEdges = struct
  type vertex = int32

  edge : vertex * vertex.
  edge (1, 2).
  edge (2, 3).
end

module Path (E : Edges) = struct

  path : E.vertex * E.vertex.
  path(X,Y) :- E.edge(X,Y).
  path(X,Z) :- path(X,Y), E.edge(Y,Z).

end

Need to implement 'open' (and 'include'?)

Parameterised types for datalog?
*)

module Datalog_syntax = struct
  type def_type =
    | Tuple of def_type list
    | Int32
    | Typename of path

  type val_type =
    def_type list

  type kind = unit

  type expr =
    | Expr_var   of string
    | Expr_lit   of int32
    | Expr_tuple of expr list

  type atom =
    { atm_head : path
    ; atm_args : expr list
    }

  type clause =
    { cls_head : expr list
    ; cls_rhs  : atom list
    }

  type term =
    { tm_type    : val_type
    ; tm_clauses : clause list
    }

  let rec subst_deftype ty sub =
    match ty with
      | Tuple tys ->
         Tuple (List.map (fun ty -> subst_deftype ty sub) tys)
      | Int32 ->
         Int32
      | Typename path ->
         Typename (Subst.path path sub)

  let subst_valtype ty sub =
    List.map (fun ty -> subst_deftype ty sub) ty

  let subst_kind () sub =
    ()
end

module Datalog_Mod_Syntax = Mod_Syntax (Datalog_syntax)
module Env = Env (Datalog_Mod_Syntax)

module Datalog_Typing = struct
  module Core = Datalog_syntax
  module Env = Env

  open Core

  let type_term env rules =
    (* for each clause:
       - get a type for each variable from the given type
       - make sure that any repeated variables get the same type, and literals are correctly typed.
       - work through the rhs, updating the environment
       - ensure that each clause is 'safe': all variables in the head are mentioned in the rhs.
    *)
    failwith "type_term"

  let kind_deftype env _ =
    ()

  let rec check_valtype env tys =
    tys |> List.iter begin function
      | Int32         -> ()
      | Typename path -> ignore (Env.find path env)
      | Tuple tys     -> check_valtype env tys
    end

  let check_kind env () = ()

  let valtype_match env ty1 ty2 =
    failwith "valtype_match"

  let deftype_equiv env () ty1 ty2 =
    failwith "deftype_equiv"

  let kind_match env () () =
    true

  let deftype_of_path path () =
    Typename path
end

module Modular_Datalog_typing =
  Mod_typing (Datalog_Mod_Syntax) (Env) (Datalog_Typing)
