open Core_syntax
open Checker

module Env = TypeChecker.Env

(* FIXME: this ought to do something else: display whatever object is
   required. *)
let display env longident =
  match Env.find_module longident env with
  | Ok (path, mod_ty) ->
    Format.print_flush ();
    Format.printf "@[<v2>module %a :@ %a@]\n"
      Modules.Syntax.Bound_names.pp_longident path
      CheckedSyntax.pp_modtype mod_ty
  | Error lookup_error ->
    Format.eprintf "Lookup error %a\n"
      Modules.Typing_environment.pp_lookup_error lookup_error

let execute_script script =
  let rec loop env = function
    | [] ->
      Ok ()
    | Declaration str_item :: items ->
      (match TypeChecker.type_str_item env str_item with
      | Ok env ->
	loop env items
      | Error err ->
	Format.eprintf "%a" TypeChecker.pp_error err; Error "err")
    | Command (Display longident) :: items ->
      display env longident;
      loop env items
    | Command (Synth (ident, modty)) :: items ->
      (match Synthesis.synthesise env ident modty with
      | Ok () ->
	loop env items
      | Error (`Synth_error (loc, string)) ->
	Format.eprintf "at %a; %s\n" Location.pp loc string;
	Error "script failed"
      | Error (`TypeChecker err) ->
	Format.eprintf "%a\n" TypeChecker.pp_error err; Error "err")
  in
  loop Env.empty script
