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
    val find : key -> 'a t -> 'a option
  end

  val pp : Format.formatter -> t -> unit
end

module Ident : IDENT = struct
  type t =
    { name  : string
    ; stamp : int
    }

  let currstamp = ref 0

  let create s =
    incr currstamp;
    {name = s; stamp = !currstamp}

  let name id =
    id.name

  let equal id1 id2 =
    id1.stamp = id2.stamp

  module OT = struct
    type nonrec t = t
    let compare x y =
      compare x.stamp y.stamp
  end

  module Table = struct
    include Map.Make (OT)
    let find key table =
      try Some (find key table) with Not_found -> None
  end

  let pp pp {name;stamp} =
    Format.fprintf pp "%s/%d" name stamp
end

module Path = struct
  type t =
    | Pident of Ident.t
    | Pdot   of t * string

  let rec equal p1 p2 =
    match p1, p2 with
      | Pident id1, Pident id2 ->
         Ident.equal id1 id2
      | Pdot (r1, field1), Pdot (r2, field2) ->
         equal r1 r2 && field1 = field2
      | _, _ ->
         false
end

module type SUBST = sig
  type t

  val identity : t

  val add : Ident.t -> Path.t -> t -> t

  val path : t -> Path.t -> Path.t
end

module Subst : SUBST = struct
  type t = Path.t Ident.Table.t

  let identity = Ident.Table.empty

  let add = Ident.Table.add

  let rec path sub = function
    | Path.Pident id as p ->
       (match Ident.Table.find id sub with
         | None   -> p
         | Some p -> p)
    | Path.Pdot (root, field) ->
       Path.Pdot (path sub root, field)
end

module type CORE_SYNTAX = sig
  type term
  type val_type
  type def_type
  type kind

  val subst_valtype : Subst.t -> val_type -> val_type

  val subst_deftype : Subst.t -> def_type -> def_type

  val subst_kind : Subst.t -> kind -> kind
end

module type MOD_SYNTAX = sig
  module Core : CORE_SYNTAX

  type type_decl =
    { kind     : Core.kind
    ; manifest : Core.def_type option
    }

  type mod_type =
    | Modtype_longident of Path.t
    | Modtype_signature of signature
    | Modtype_functor   of Ident.t * mod_type * mod_type

  and signature =
    sig_item list

  and sig_item =
    | Sig_value  of Ident.t * Core.val_type
    | Sig_type   of Ident.t * type_decl
    | Sig_module of Ident.t * mod_type
    | Sig_modty  of Ident.t * mod_type

  type mod_term =
    | Mod_longident  of Path.t
    | Mod_structure  of structure
    | Mod_functor    of Ident.t * mod_type * mod_term
    | Mod_apply      of mod_term * mod_term
    | Mod_constraint of mod_term * mod_type

  and structure =
    str_item list

  and str_item =
    | Str_value  of Core.term
    | Str_type   of Ident.t * Core.kind * Core.def_type
    | Str_module of Ident.t * mod_term
    | Str_modty  of Ident.t * mod_type

  val subst_typedecl : Subst.t -> type_decl -> type_decl

  val subst_modtype : Subst.t -> mod_type -> mod_type
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
    | Modtype_longident of Path.t
    | Modtype_signature of signature
    | Modtype_functor   of Ident.t * mod_type * mod_type

  and signature =
    sig_item list

  and sig_item =
    | Sig_value  of Ident.t * Core.val_type
    | Sig_type   of Ident.t * type_decl
    | Sig_module of Ident.t * mod_type
    | Sig_modty  of Ident.t * mod_type

  type mod_term =
    | Mod_longident  of Path.t
    | Mod_structure  of structure
    | Mod_functor    of Ident.t * mod_type * mod_term
    | Mod_apply      of mod_term * mod_term
    | Mod_constraint of mod_term * mod_type

  and structure =
    str_item list

  and str_item =
    | Str_value  of Core.term
    | Str_type   of Ident.t * Core.kind * Core.def_type
    | Str_module of Ident.t * mod_term
    | Str_modty  of Ident.t * mod_type

  let subst_typedecl sub decl =
    { kind     = Core.subst_kind sub decl.kind
    ; manifest =
        match decl.manifest with
          | None     -> None
          | Some dty -> Some (Core.subst_deftype sub dty)
    }

  let rec subst_modtype sub = function
    | Modtype_longident p ->
       Modtype_longident (Subst.path sub p)
    | Modtype_signature sg ->
       Modtype_signature (List.map (subst_sig_item sub) sg)
    | Modtype_functor (id, mty1, mty2) ->
       Modtype_functor (id, subst_modtype sub mty1, subst_modtype sub mty2)

  and subst_sig_item sub = function
    | Sig_value (id, vty)  -> Sig_value (id, Core.subst_valtype sub vty)
    | Sig_type (id, decl)  -> Sig_type (id, subst_typedecl sub decl)
    | Sig_module (id, mty) -> Sig_module (id, subst_modtype sub mty)
    | Sig_modty (id, mty)  -> Sig_modty (id, subst_modtype sub mty)
end

type checker_error = string

module type ENV = sig
  module Mod : MOD_SYNTAX

  type t

  val empty : t

  val add_value : Ident.t -> Mod.Core.val_type -> t -> t

  val add_type : Ident.t -> Mod.type_decl -> t -> t

  val add_module : Ident.t -> Mod.mod_type -> t -> t

  val add_modty : Ident.t -> Mod.mod_type -> t -> t

  val add_sigitem : Mod.sig_item -> t -> t

  val add_signature : Mod.signature -> t -> t

  val find_value : Path.t -> t -> Mod.Core.val_type

  val find_type : Path.t -> t -> Mod.type_decl

  val find_module : Path.t -> t -> Mod.mod_type

  val find_modty : Path.t -> t -> Mod.mod_type
end

module Env (Mod_syntax : MOD_SYNTAX) : ENV with module Mod = Mod_syntax =
struct
  module Mod = Mod_syntax

  type binding =
    | Value  of Mod.Core.val_type
    | Type   of Mod.type_decl
    | Module of Mod.mod_type
    | Modty  of Mod.mod_type

  type t = binding Ident.Table.t

  let empty = Ident.Table.empty

  let add_value id vty env =
    Ident.Table.add id (Value vty) env

  let add_type id decl env =
    Ident.Table.add id (Type decl) env

  let add_module id mty env =
    Ident.Table.add id (Module mty) env

  let add_modty id mty env =
    Ident.Table.add id (Modty mty) env

  let add_sigitem item env =
    match item with
      | Mod.Sig_value (id, vty)  -> add_value id vty env
      | Mod.Sig_type (id, decl)  -> add_type id decl env
      | Mod.Sig_module (id, mty) -> add_module id mty env
      | Mod.Sig_modty (id, mty)  -> add_modty id mty env

  let add_signature =
    List.fold_right add_sigitem

  let rec find path env = match path with
    | Path.Pident id ->
       (match Ident.Table.find id env with
         | None ->
            Error "identifier not found"
         | Some p ->
            Ok p)
    | Path.Pdot (root, field) ->
       match find_module root env with
         | Mod.Modtype_signature sg ->
            find_field root field Subst.identity sg
         | _ ->
            Error "structure expected in dot access"

  and find_field p field sub = function
    | [] ->
       Error "no such field in structure"
    | Mod.Sig_value (id, vty) :: rem ->
       if Ident.name id = field
       then Ok (Value (Mod.Core.subst_valtype sub vty))
       else find_field p field sub rem
    | Mod.Sig_type (id, decl) :: rem ->
       if Ident.name id = field
       then Ok (Type (Mod.subst_typedecl sub decl))
       else
         find_field p field (Subst.add id (Path.Pdot (p, Ident.name id)) sub) rem
    | Mod.Sig_module (id, mty) :: rem ->
       if Ident.name id = field
       then Ok (Module (Mod.subst_modtype sub mty))
       else
         find_field p field (Subst.add id (Path.Pdot (p, Ident.name id)) sub) rem
    | Mod.Sig_modty (id, mty) :: rem ->
       if Ident.name id = field
       then Ok (Modty (Mod.subst_modtype sub mty))
       else
         find_field p field (Subst.add id (Path.Pdot (p, Ident.name id)) sub) rem

  and find_value path env =
    match find path env with
      | Ok (Value vty) -> vty
      | Ok _ ->
         failwith "value field expected"
      | Error msg ->
         failwith msg

  and find_type path env =
    match find path env with
      | Ok (Type decl) -> decl
      | Ok _ -> failwith "type field expected"
      | Error msg -> failwith msg

  and find_module path env =
    match find path env with
      | Ok (Module mty) -> mty
      | Ok _ -> failwith "module field expected"
      | Error msg -> failwith msg

  and find_modty path env =
    match find path env with
      | Ok (Modty mty) -> mty
      | Ok _ -> failwith "module type field expected"
      | Error msg -> failwith msg
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

  val deftype_of_path : Path.t -> Core.kind -> Core.def_type
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
      | Modtype_longident id1, mty2 ->
         modtype_match env (Env.find_modty id1 env) mty2
      | mty1, Modtype_longident id2 ->
         modtype_match env mty1 (Env.find_modty id2 env)

      (* Check signatures via subsumption *)
      | Modtype_signature sig1, Modtype_signature sig2 ->
         let paired_components, sub = pair_signature_components sig1 sig2 in
         let ext_env = Env.add_signature sig1 env in
         List.iter (specification_match ext_env sub) paired_components

      (* *)
      | Modtype_functor (param1, arg1, res1), Modtype_functor (param2, arg2, res2) ->
         let sub   = Subst.add param1 (Path.Pident param2) Subst.identity in
         let res1' = Mod.subst_modtype sub res1 in
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
           | [] ->
              failwith "unmatched signature component"
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
         let id1, id2, item1 = find_matching_component sig1 in
         let pairs, sub = pair_signature_components sig1 rem2 in
         ((item1, item2) :: pairs, Subst.add id2 (Path.Pident id1) sub)

  and specification_match env sub = function
    | Sig_value (_, vty1), Sig_value (_, vty2) ->
       if not (CT.valtype_match env vty1 (Core.subst_valtype sub vty2))
       then failwith "value components do not match"

    | Sig_type (id, decl1), Sig_type (_, decl2) ->
       if not (typedecl_match env id decl1 (Mod.subst_typedecl sub decl2))
       then failwith "type components do not match"

    | Sig_module (_, mty1), Sig_module (_, mty2) ->
       modtype_match env mty1 (Mod.subst_modtype sub mty2)

    | Sig_modty (_, mty1), Sig_modty (_, mty2) ->
       (* FIXME: not sure this is right? matching both ways =>
          equality up to permutation? *)
       modtype_match env mty1 (Mod.subst_modtype sub mty2);
       modtype_match env (Mod.subst_modtype sub mty2) mty1

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
           (CT.deftype_of_path (Path.Pident id) decl1.kind)
           typ2)


  let rec strengthen_modtype env path = function
    | Modtype_longident p ->
       strengthen_modtype env path (Env.find_modty p env)

    | Modtype_signature sg ->
       Modtype_signature (List.map (strengthen_sigitem env path) sg)

    | Modtype_functor _ as mty ->
       mty

  and strengthen_sigitem env path = function
    | Sig_value _ as item ->
       item

    | Sig_type (id, decl) ->
       let m =
         match decl.manifest with
           | None ->
              Some (CT.deftype_of_path (Path.Pdot (path, Ident.name id)) decl.kind)
           | Some ty ->
              Some ty
       in
       Sig_type (id, {decl with manifest = m})

    | Sig_module (id, mty) ->
       Sig_module (id, strengthen_modtype env (Path.Pdot (path, Ident.name id)) mty)

    | Sig_modty (id, mty) ->
       Sig_modty (id, mty)

  
  module SeenSet = struct
    include Set.Make (String)
    let mem ident set =
      mem (Ident.name ident) set
    let add ident set =
      add (Ident.name ident) set
  end

  
  let rec check_modtype env = function
    | Modtype_longident path ->
       ignore (Env.find_modty path env)

    | Modtype_signature items ->
       check_signature env SeenSet.empty items

    | Modtype_functor (param, arg, res) ->
       check_modtype env arg;
       check_modtype (Env.add_module param arg env) res

  and check_signature env seen = function
    | [] ->
       ()

    | Sig_value (id, vty) :: rem ->
       if SeenSet.mem id seen
       then failwith "repeated value name";
       CT.check_valtype env vty;
       check_signature env (SeenSet.add id seen) rem

    | Sig_type (id, decl) :: rem ->
       if SeenSet.mem id seen
       then failwith "repeated type name";
       CT.check_kind env decl.kind;
       begin match decl.manifest with
         | None -> ()
         | Some typ ->
            if not (CT.kind_match env (CT.kind_deftype env typ) decl.kind)
            then failwith "kind mismatch in manifest type specification"
       end;
       check_signature (Env.add_type id decl env) (SeenSet.add id seen) rem

    | Sig_module (id, mty) :: rem ->
       if SeenSet.mem id seen
       then failwith "repeated module name";
       check_modtype env mty;
       check_signature (Env.add_module id mty env) (SeenSet.add id seen) rem

    | Sig_modty (id, mty) :: rem ->
       check_modtype env mty;
       check_signature (Env.add_modty id mty env) (SeenSet.add id seen) rem


  let rec type_module env = function
    | Mod_longident path ->
       strengthen_modtype env path (Env.find_module path env)

    | Mod_structure str ->
       let signature = type_structure env [] SeenSet.empty str in
       Modtype_signature signature

    | Mod_functor (param, mty, body) ->
       check_modtype env mty;
       Modtype_functor (param, mty, type_module (Env.add_module param mty env) body)

    | Mod_apply (funct, (Mod_longident path as arg)) ->
       (match type_module env funct with
         | Modtype_functor (param, mty_param, mty_res) ->
            let mty_arg = type_module env arg in
            modtype_match env mty_arg mty_param;
            subst_modtype (Subst.add param path Subst.identity) mty_res
         | _ ->
            failwith "application of a non-functor")

    | Mod_apply (funct, arg) ->
       failwith "application of a functor to a non-path"

    | Mod_constraint (modl, mty) ->
       check_modtype env mty;
       modtype_match env (type_module env modl) mty;
       mty

  and type_structure env rev_sig seen = function
    | [] ->
       List.rev rev_sig

    | Str_value term :: items ->
       let val_items = CT.type_term env term in
       (* FIXME: assuming that CT.type_term doesn't return duplicate
          names *)
       if List.exists (fun (id, _) -> SeenSet.mem id seen) val_items
       then failwith "repeated value name";
       let sigitems = List.rev_map (fun (id, ty) -> Sig_value (id, ty)) val_items in
       let env      = Env.add_signature sigitems env in
       let seen     = List.fold_left (fun s (id,_) -> SeenSet.add id s) seen val_items in
       type_structure env (sigitems @ rev_sig) seen items

    | Str_module (id, modl) :: items ->
       if SeenSet.mem id seen
       then failwith "repeated module name";
       let modty = type_module env modl in
       type_structure
         (Env.add_module id modty env)
         (Sig_module (id, modty) :: rev_sig)
         (SeenSet.add id seen)
         items

    | Str_type (id, kind, typ) :: items ->
       if SeenSet.mem id seen
       then failwith "repeated type name";
       CT.check_kind env kind;
       if not (CT.kind_match env (CT.kind_deftype env typ) kind)
       then failwith "kind mismatch in type definition";
       let tydecl = {kind = kind; manifest = Some typ} in
       type_structure
         (Env.add_type id tydecl env)
         (Sig_type (id, tydecl) :: rev_sig)
         (SeenSet.add id seen)
         items

    | Str_modty (id, mty) :: items ->
       check_modtype env mty;
       type_structure
         (Env.add_modty id mty env)
         (Sig_modty (id, mty) :: rev_sig)
         (SeenSet.add id seen)
         items
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
     substituted versions. 

     FIXME: could also substitute out type declarations in terms?
  *)

  open Mod

  let rec subst_modterm sub = function
    | Mod_longident lid ->
       Mod_longident (Subst.path sub lid)
    | Mod_structure str ->
       Mod_structure (List.map (subst_def sub) str)
    | Mod_functor (id, mty, modl) ->
       Mod_functor (id, subst_modtype sub mty, subst_modterm sub modl)
    | Mod_apply (modl1, modl2) ->
       Mod_apply (subst_modterm sub modl1, subst_modterm sub modl2)
    | Mod_constraint (modl, mty) ->
       Mod_constraint (subst_modterm sub modl, subst_modtype sub mty)

  and subst_def sub = function
    | Str_value tm -> Str_value (CN.subst_term tm sub)
    | Str_type (id, kind, dty) ->
       Str_type (id, Core.subst_kind sub kind, Core.subst_deftype sub dty)
    | Str_module (id, modl) ->
       Str_module (id, subst_modterm sub modl)
    | Str_modty (id, mty) ->
       Str_modty (id, subst_modtype sub mty)

  module Env : sig
    type t
    val empty : t
    val find : Path.t -> t -> mod_term option
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
        | Path.Pident id ->
           Ident.Table.find id env
        | Path.Pdot (root, field) ->
           match find root env with
             | Some (Mod_structure str) ->
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
         else find_field p field (Subst.add id (Path.Pdot (p, Ident.name id)) sub) rem
      | Str_module (id, modl) :: rem ->
         if Ident.name id = field
         then subst_modterm sub modl
         else find_field p field (Subst.add id (Path.Pdot (p, Ident.name id)) sub) rem
  end

  let rec norm_modterm env = function
    | Mod_longident lid ->
       (match Env.find lid env with
         | None ->
            (* If not found, must be abstract *)
            Mod_longident lid
         | Some modl ->
            modl)
    | Mod_structure items ->
       let _, defs = List.fold_left norm_def (env,[]) items in
       Mod_structure (List.rev defs)
    | Mod_functor (id, mty, modl) ->
       Mod_functor (id, mty, norm_modterm env modl)
    | Mod_apply (modl, (Mod_longident lid as arg)) ->
       (match norm_modterm env modl with
         | Mod_longident _ as modl ->
            Mod_apply (modl, arg)
         | Mod_functor (id, _, modl) ->
            norm_modterm env (subst_modterm Subst.(add id lid identity) modl)
         | _ ->
            failwith "internal error: type error in module normalisation")
    | Mod_apply (_, _) ->
       failwith "Application to non path"
    | Mod_constraint (modl, _) ->
       modl

  and norm_def (env, defs) = function
    | (Str_value _ | Str_type _ | Str_modty _) as def ->
       (env, def :: defs)
    | Str_module (id, modl) ->
       let modl = norm_modterm env modl in
       let env  = Env.add id modl env in
       (env, Str_module (id, modl) :: defs)
end

(*
module Test_syn = struct
  type term = Path.t
  type val_type = Path.t
  type def_type = Path.t
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
                           Structure [Str_type (id"u", (), Path.Pdot (Path.Pident x, "t"))]))
    ; Str_module (y, Structure [Str_type (id"t", (), Path.Pident a)])
    ; Str_module (z, Apply (Longident (Path.Pident f), Longident (Path.Pident y)))
    ]
  end
*)
