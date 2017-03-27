module type IDENT = sig
  type t

  val create : string -> t

  val name : t -> string

  val equal : t -> t -> bool

  module Table : sig
    type key = t
    type 'a t
    val empty : 'a t
    val add : key -> 'a -> 'a t -> 'a t
    val find : key -> 'a t -> 'a (* may throw Not_found *)
  end

  module Set : sig
    type elt = t
    type t
    val empty : t
    val add : elt -> t -> t
    val mem : elt -> t -> bool
  end
end

module Ident : IDENT = struct
  type t =
    { name  : string
    ; stamp : int
    }

  let currstamp = ref 0

  let create s =
    incr currstamp; {name = s; stamp = !currstamp}

  let name id =
    id.name

  let equal id1 id2 =
    id1.stamp = id2.stamp

  module OT = struct
    type nonrec t = t
    let compare x y = compare x.stamp y.stamp
  end

  module Table = Map.Make (OT)
  module Set = Set.Make (OT)
end

type path =
  | Pident of Ident.t
  | Pdot   of path * string

let rec path_equal p1 p2 =
  match p1, p2 with
    | Pident id1, Pident id2 ->
       Ident.equal id1 id2
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
  type t = path Ident.Table.t
  let identity = Ident.Table.empty
  let add = Ident.Table.add
  let rec path p sub =
    match p with
      | Pident id -> (try Ident.Table.find id sub with Not_found -> p)
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
    | Mty_longident of path
    | Signature     of signature
    | Functor_type  of Ident.t * mod_type * mod_type
  and signature = specification list
  and specification =
    | Sig_value  of Ident.t * Core.val_type
    | Sig_type   of Ident.t * type_decl
    | Sig_module of Ident.t * mod_type
    | Sig_modty  of Ident.t * mod_type

  type mod_term =
    | Longident  of path
    | Structure  of structure
    | Functor    of Ident.t * mod_type * mod_term
    | Apply      of mod_term * mod_term
    | Constraint of mod_term * mod_type
  and structure = definition list
  and definition =
    | Str_value  of Core.term
    | Str_type   of Ident.t * Core.kind * Core.def_type
    | Str_module of Ident.t * mod_term
    | Str_modty  of Ident.t * mod_type

  val subst_typedecl : type_decl -> Subst.t -> type_decl

  val subst_modtype  : mod_type -> Subst.t -> mod_type
end

module Mod_Syntax (Core_syntax : CORE_SYNTAX)
  : MOD_SYNTAX with module Core = Core_syntax =
struct
  module Core = Core_syntax

  type type_decl =
    { kind     : Core.kind
    ; manifest : Core.def_type option
    }

  type mod_type =
    | Mty_longident of path
    | Signature     of signature
    | Functor_type  of Ident.t * mod_type * mod_type
  and signature = specification list
  and specification =
    | Sig_value  of Ident.t * Core.val_type
    | Sig_type   of Ident.t * type_decl
    | Sig_module of Ident.t * mod_type
    | Sig_modty  of Ident.t * mod_type

  type mod_term =
    | Longident  of path
    | Structure  of structure
    | Functor    of Ident.t * mod_type * mod_term
    | Apply      of mod_term * mod_term
    | Constraint of mod_term * mod_type
  and structure = definition list
  and definition =
    | Str_value  of Core.term
    | Str_type   of Ident.t * Core.kind * Core.def_type
    | Str_module of Ident.t * mod_term
    | Str_modty  of Ident.t * mod_type

  let subst_typedecl decl sub =
    { kind     = Core.subst_kind decl.kind sub
    ; manifest =
        match decl.manifest with
          | None     -> None
          | Some dty -> Some (Core.subst_deftype dty sub)
    }

  let rec subst_modtype mty sub = match mty with
    | Mty_longident p ->
       Mty_longident (Subst.path p sub)
    | Signature sg ->
       Signature (List.map (subst_sig_item sub) sg)
    | Functor_type (id, mty1, mty2) ->
       Functor_type (id, subst_modtype mty1 sub, subst_modtype mty2 sub)
  and subst_sig_item sub = function
    | Sig_value (id, vty)  -> Sig_value (id, Core.subst_valtype vty sub)
    | Sig_type (id, decl)  -> Sig_type (id, subst_typedecl decl sub)
    | Sig_module (id, mty) -> Sig_module (id, subst_modtype mty sub)
    | Sig_modty (id, mty)  -> Sig_modty (id, subst_modtype mty sub)
end

module type ENV = sig
  module Mod : MOD_SYNTAX

  type t

  val empty : t

  val add_value : Ident.t -> Mod.Core.val_type -> t -> t

  val add_type : Ident.t -> Mod.type_decl -> t -> t

  val add_module : Ident.t -> Mod.mod_type -> t -> t

  val add_modty : Ident.t -> Mod.mod_type -> t -> t

  val add_spec : Mod.specification -> t -> t

  val add_signature : Mod.signature -> t -> t

  val find_value : path -> t -> Mod.Core.val_type

  val find_type : path -> t -> Mod.type_decl

  val find_module : path -> t -> Mod.mod_type

  val find_modty : path -> t -> Mod.mod_type
end

module Env (Mod_syntax : MOD_SYNTAX) = struct
  module Mod = Mod_syntax

  type binding =
    | Value  of Mod.Core.val_type
    | Type   of Mod.type_decl
    | Module of Mod.mod_type
    | Modty  of Mod.mod_type

  type t = binding Ident.Table.t

  let empty = Ident.Table.empty

  let add_value id vty env = Ident.Table.add id (Value vty) env
  let add_type id decl env = Ident.Table.add id (Type decl) env
  let add_module id mty env = Ident.Table.add id (Module mty) env
  let add_modty id mty env = Ident.Table.add id (Modty mty) env
  let add_spec item env =
    match item with
      | Mod.Sig_value (id, vty)  -> add_value id vty env
      | Mod.Sig_type (id, decl)  -> add_type id decl env
      | Mod.Sig_module (id, mty) -> add_module id mty env
      | Mod.Sig_modty (id, mty)  -> add_modty id mty env
  let add_signature = List.fold_right add_spec

  let rec find path env =
    match path with
      | Pident id ->
         Ident.Table.find id env
      | Pdot (root, field) ->
         match find_module root env with
           | Mod.Signature sg -> find_field root field Subst.identity sg
           | _ -> failwith "structure expected in dot access"

  and find_field p field sub = function
    | [] ->
       failwith "no such field in structure"
    | Mod.Sig_value (id, vty) :: rem ->
       if Ident.name id = field
       then Value (Mod.Core.subst_valtype vty sub)
       else find_field p field sub rem
    | Mod.Sig_type (id, decl) :: rem ->
       if Ident.name id = field
       then Type (Mod.subst_typedecl decl sub)
       else find_field p field (Subst.add id (Pdot (p, Ident.name id)) sub) rem
    | Mod.Sig_module (id, mty) :: rem ->
       if Ident.name id = field
       then Module (Mod.subst_modtype mty sub)
       else find_field p field (Subst.add id (Pdot (p, Ident.name id)) sub) rem
    | Mod.Sig_modty (id, mty) :: rem ->
       if Ident.name id = field
       then Modty (Mod.subst_modtype mty sub)
       else find_field p field (Subst.add id (Pdot (p, Ident.name id)) sub) rem

  and find_value path env =
    match find path env with
      | Value vty -> vty
      | _ -> failwith "value field expected"

  and find_type path env =
    match find path env with
      | Type decl -> decl
      | _ -> failwith "type field expected"

  and find_module path env =
    match find path env with
      | Module mty -> mty
      | _ -> failwith "module field expected"

  and find_modty path env =
    match find path env with
      | Modty mty -> mty
      | _ -> failwith "module type field expected"
end

module type CORE_TYPING = sig
  module Core : CORE_SYNTAX

  module Env  : ENV with module Mod.Core = Core

  val type_term : Env.t -> Core.term -> (Ident.t * Core.val_type) list

  val kind_deftype : Env.t -> Core.def_type -> Core.kind

  val check_valtype : Env.t -> Core.val_type -> unit

  val check_kind : Env.t -> Core.kind -> unit

  val valtype_match : Env.t -> Core.val_type -> Core.val_type -> bool

  val deftype_equiv : Env.t -> Core.kind -> Core.def_type -> Core.def_type -> bool

  val kind_match : Env.t -> Core.kind -> Core.kind -> bool

  val deftype_of_path : path -> Core.kind -> Core.def_type
end

module type MOD_TYPING = sig
  module Mod : MOD_SYNTAX

  module Env : ENV with module Mod = Mod

  val type_module : Env.t -> Mod.mod_term -> Mod.mod_type
end

module Mod_typing
    (TheMod : MOD_SYNTAX)
    (CT : CORE_TYPING
     with module Core = TheMod.Core
      and module Env.Mod = TheMod)
  : MOD_TYPING with module Mod = TheMod
                and module Env = CT.Env
=
struct
  module Mod = TheMod
  module Env = CT.Env

  open Mod

  let rec modtype_match env mty1 mty2 =
    match mty1, mty2 with
      (* Expand out module type names *)
      | Mty_longident id1, mty2 ->
         modtype_match env (Env.find_modty id1 env) mty2
      | mty1, Mty_longident id2 ->
         modtype_match env mty1 (Env.find_modty id2 env)
      (* Check signatures via subsumption *)
      | Signature sig1, Signature sig2 ->
         let paired_components, subst = pair_signature_components sig1 sig2 in
         let ext_env = Env.add_signature sig1 env in
         List.iter (specification_match ext_env subst) paired_components
      (* *)
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
                | Sig_value (id1, _),  Sig_value (id2, _)
                | Sig_type (id1, _),   Sig_type (id2, _)
                | Sig_module (id1, _), Sig_module (id2, _)
                | Sig_modty (id1, _),  Sig_modty (id2, _)
                  when Ident.name id1 = Ident.name id2 ->
                   (id1, id2, item1)
                | _ ->
                   find_matching_component rem1
         in
         let (id1, id2, item1) = find_matching_component sig1 in
         let (pairs, subst) = pair_signature_components sig1 rem2 in
         ((item1, item2) :: pairs, Subst.add id2 (Pident id1) subst)

  and specification_match env subst = function
    | Sig_value (_, vty1), Sig_value (_, vty2) ->
       if not (CT.valtype_match env vty1 (Core.subst_valtype vty2 subst))
       then failwith "value components do not match"
    | Sig_type (id, decl1), Sig_type (_, decl2) ->
       if not (typedecl_match env id decl1 (Mod.subst_typedecl decl2 subst))
       then failwith "type components do not match"
    | Sig_module (_, mty1), Sig_module (_, mty2) ->
       modtype_match env mty1 (Mod.subst_modtype mty2 subst)
    | Sig_modty (_, mty1), Sig_modty (_, mty2) ->
       (* FIXME: not sure this is right? matching both ways =>
          equality up to permutation? *)
       modtype_match env mty1 (Mod.subst_modtype mty2 subst);
       modtype_match env (Mod.subst_modtype mty2 subst) mty1
    | _, _ ->
       assert false

  and typedecl_match env id decl1 decl2 =
    CT.kind_match env decl1.kind decl2.kind &&
    (match decl1.manifest, decl2.manifest with
      | _, None ->
         true
      | Some typ1, Some typ2 ->
         CT.deftype_equiv env decl2.kind typ1 typ2
      | None, Some typ2 ->
         CT.deftype_equiv env decl2.kind
           (CT.deftype_of_path (Pident id) decl1.kind)
           typ2)

  let rec strengthen_modtype env path mty =
    match mty with
      | Mty_longident p ->
         strengthen_modtype env path (Env.find_modty p env)
      | Signature sg ->
         Signature (List.map (strengthen_spec env path) sg)
      | Functor_type _ ->
         mty

  and strengthen_spec env path item =
    match item with
      | Sig_value (id, vty) ->
         item
      | Sig_type (id, decl) ->
         let m =
           match decl.manifest with
             | None ->
                Some (CT.deftype_of_path (Pdot (path, Ident.name id)) decl.kind)
             | Some ty ->
                Some ty
         in
         Sig_type (id, {decl with manifest = m })
      | Sig_module (id, mty) ->
         Sig_module (id, strengthen_modtype env (Pdot (path, Ident.name id)) mty)
      | Sig_modty (id, mty) ->
         Sig_modty (id, mty)

  let rec check_modtype env = function
    | Mty_longident path ->
       ignore (Env.find_modty path env)
    | Signature sg ->
       check_signature env [] sg
    | Functor_type (param, arg, res) ->
       check_modtype env arg;
       check_modtype (Env.add_module param arg env) res

  and check_signature env seen = function
    | [] ->
       ()
    | Sig_value (id, vty) :: rem ->
       if List.mem (Ident.name id) seen
       then failwith "repeated value name";
       CT.check_valtype env vty;
       check_signature env (Ident.name id :: seen) rem
    | Sig_type (id, decl) :: rem ->
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
    | Sig_module (id, mty) :: rem ->
       if List.mem (Ident.name id) seen
       then failwith "repeated module name";
       check_modtype env mty;
       check_signature (Env.add_module id mty env) (Ident.name id :: seen) rem
    | Sig_modty (id, mty) :: rem ->
       check_modtype env mty;
       check_signature (Env.add_modty id mty env) (Ident.name id :: seen) rem

  module SeenSet = Set.Make (String)

  let rec type_module env = function
    | Longident path ->
       strengthen_modtype env path (Env.find_module path env)
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
    | [] ->
       []
    | stritem :: rem ->
       let items, seen' = type_definition env seen stritem in
       items @ type_structure (Env.add_signature items env) seen' rem

  and type_definition env seen = function
    | Str_value term ->
       let items = CT.type_term env term in
       (* FIXME: assuming that CT.type_term doesn't return duplicates *)
       if List.exists (fun (id, _) -> List.mem (Ident.name id) seen) items
       then failwith "repeated value name";
       (List.map (fun (id, ty) -> Sig_value (id, ty)) items,
        List.map (fun (id, _) -> Ident.name id) items @ seen)
    | Str_module (id, modl) ->
       if List.mem (Ident.name id) seen
       then failwith "repeated module name";
       ([Sig_module (id, type_module env modl)], Ident.name id :: seen)
    | Str_type (id, kind, typ) ->
       if List.mem (Ident.name id) seen
       then failwith "repeated type name";
       CT.check_kind env kind;
       if not (CT.kind_match env (CT.kind_deftype env typ) kind)
       then failwith "kind mismatch in type definition";
       ([Sig_type (id, {kind = kind; manifest = Some typ})],
        Ident.name id :: seen)
    | Str_modty (id, mty) ->
       check_modtype env mty;
       ([Sig_modty (id, mty)], Ident.name id :: seen)
end

module type CORE_NORM = sig
  module Core : CORE_SYNTAX

  val subst_term : Core.term -> Subst.t -> Core.term
end

module Mod_normalise
    (Mod : MOD_SYNTAX)
    (CN  : CORE_NORM with module Core = Mod.Core) =
struct
  (* normalise module expressions by replacing functors with the
     substituted versions. *)

  open Mod

  let rec subst_modterm modl sub = match modl with
    | Longident lid -> Longident (Subst.path lid sub)
    | Structure str -> Structure (List.map (subst_def sub) str)
    | Functor (id, mty, modl) ->
       Functor (id, subst_modtype mty sub, subst_modterm modl sub)
    | Apply (modl1, modl2) ->
       Apply (subst_modterm modl1 sub, subst_modterm modl2 sub)
    | Constraint (modl, mty) ->
       Constraint (subst_modterm modl sub, subst_modtype mty sub)
  and subst_def sub = function
    | Str_value tm -> Str_value (CN.subst_term tm sub)
    | Str_type (id, kind, dty) ->
       Str_type (id, Core.subst_kind kind sub, Core.subst_deftype dty sub)
    | Str_module (id, modl) ->
       Str_module (id, subst_modterm modl sub)
    | Str_modty (id, mty) ->
       Str_modty (id, subst_modtype mty sub)

  module Env : sig
    type t
    val empty : t
    val find : path -> t -> mod_term option
    val add : Ident.t -> mod_term -> t -> t
  end = struct
    type t = mod_term Ident.Table.t

    let empty = Ident.Table.empty

    let add id mtm env = Ident.Table.add id mtm env

    let add_item item env =
      match item with
        | Str_value _ | Str_type _ | Str_modty _ -> env
        | Str_module (id, modl) -> add id modl env

    let add_structure = List.fold_right add_item

    let rec find path env =
      match path with
        | Pident id ->
           (match Ident.Table.find id env with
             | exception Not_found -> None
             | modl -> Some modl)
        | Pdot (root, field) ->
           match find root env with
             | Some (Structure str) ->
                Some (find_field root field Subst.identity str)
             | None ->
                None
             | _ ->
                failwith "structure expected in dot access"

    and find_field p field sub = function
      | [] ->
         failwith "no such field in structure"
      | Str_value _ :: rem ->
         find_field p field sub rem
      | Str_type (id, _, _) :: rem
      | Str_modty (id, _) :: rem ->
         if Ident.name id = field
         then failwith "expecting to find a module"
         else find_field p field (Subst.add id (Pdot (p, Ident.name id)) sub) rem
      | Str_module (id, modl) :: rem ->
         if Ident.name id = field
         then subst_modterm modl sub
         else find_field p field (Subst.add id (Pdot (p, Ident.name id)) sub) rem
  end

  let rec norm_modterm env = function
    | Longident lid ->
       (match Env.find lid env with
         | None ->
            (* If not found, must be abstract *)
            Longident lid
         | Some modl ->
            modl)
    | Structure items ->
       let _, defs = List.fold_left norm_def (env,[]) items in
       Structure (List.rev defs)
    | Functor (id, mty, modl) ->
       Functor (id, mty, norm_modterm env modl)
    | Apply (modl, (Longident lid as arg)) ->
       (match norm_modterm env modl with
         | Longident _ as modl ->
            Apply (modl, arg)
         | Functor (id, _, modl) ->
            norm_modterm env (subst_modterm modl Subst.(add id lid identity))
         | _ ->
            failwith "internal error: type error in module normalisation")
    | Apply (_, _) ->
       failwith "Application to non path"
    | Constraint (modl, _) ->
       modl

  and norm_def (env, defs) = function
    | (Str_value _ | Str_type _ | Str_modty _) as def ->
       (* FIXME: delete modtys? could be referred to in functors, but
          functors could be deleted too. *)
       (* FIXME: substitute out the types in values and types? so that
          everything is as concrete as possible? Then also delete
          Str_type entries. *)
       (env, def :: defs)
    | Str_module (id, modl) ->
       let modl = norm_modterm env modl in
       let env  = Env.add id modl env in
       (env, Str_module (id, modl) :: defs)
end

module Test_syn = struct
  type term = path
  type val_type = path
  type def_type = path
  type kind = unit
  let subst_valtype = Subst.path
  let subst_deftype = Subst.path
  let subst_kind () _ = ()
end

module Test_syn_norm = struct
  module Core = Test_syn
  let subst_term = Subst.path
end

module Test_mod = Mod_Syntax (Test_syn)
module Test_norm = Mod_normalise (Test_mod) (Test_syn_norm)

let test =
  let open Test_mod in
  let id = Ident.create in
  let a = id"a" in
  Structure begin
    let f = id"F" in
    let y = id"Y" in
    let z = id"z" in
    [ Str_module (f,
                  let t = id"t" in
                  let x = id"X" in
                  Functor (x,
                           Signature [Sig_type (t, {kind=();manifest=None})],
                           Structure [Str_type (id"u", (), Pdot (Pident x, "t"))]))
    ; Str_module (y, Structure [Str_type (id"t", (), Pident a)])
    ; Str_module (z, Apply (Longident (Pident f), Longident (Pident y)))
    ]
  end
