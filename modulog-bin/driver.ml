open Modulog.Std

let (>>=) x f = match x with
  | Ok a    -> f a
  | Error e -> Error e
let (>>!) x f = match x with
  | Ok a    -> Ok a
  | Error e -> f e

let report_parse_error (pos, message, lexeme) =
  let msg =
    Fmt.str
      "@[<v 2>Parse error at %a:@ @[%s:@ %a@]@]\n"
      Modulog.Location.pp pos
      (match lexeme with
       | "" -> "At the end of the input"
       | lexeme -> Printf.sprintf "On the input '%s'" lexeme)
      Fmt.text message
  in
  Error msg

let report_check_error err =
  let msg =
    Fmt.str
      "@[<v>%a@]\n"
      Modulog.Checker.pp_error err
  in
  Error msg

let parse_and_check filename =
  read_structure_from_file filename >>! report_parse_error
  >>= fun structure ->
  Modulog.Checker.type_structure structure >>! report_check_error

let typecheck filename =
  parse_and_check filename >>= fun (str, sg) ->
  Format.printf
    "@[<v>%a@]\n"
    Modulog.Checked_syntax.Mod.pp_signature sg;
  Ok ()

let relmachine filename with_indexes =
  parse_and_check filename >>= fun (str, sg) ->
  let rules    = Modulog.To_rules.from_structure str in
  let code     = Relation_machine.Of_rules.translate rules in
  Format.printf
    "@[<v>%a@]\n%!"
    Relation_machine.Syntax.pp_program code;
  if with_indexes then begin
    let indexes = Relation_machine.Indexes.indexes code in
    Format.printf
      "\n@[<v>%a@]\n%!"
      Relation_machine.Indexes.pp_all_orderings indexes
  end;
  Ok ()

let gen_c filename =
  parse_and_check filename >>= fun (str, sg) ->
  let rules    = Modulog.To_rules.from_structure str in
  let code     = Relation_machine.Of_rules.translate rules in
  Relation_machine.Codegen.translate code;
  Ok ()

let compile filename outname =
  (* FIXME: check that filename ends with '.mlog' *)
  let outname =
    (* FIXME: some kind of validation here *)
    match outname with
      | None ->
         if Filename.check_suffix filename ".mlog" then
           Filename.chop_extension filename
         else
           filename ^ ".exe"
      | Some outname ->
         outname
  in
  parse_and_check filename >>= fun (str, sg) ->
  let rules = Modulog.To_rules.from_structure str in
  let code  = Relation_machine.Of_rules.translate rules in
  Relation_machine.Codegen.compile outname code;
  Ok ()


let rules filename =
  parse_and_check filename >>= fun (str, sg) ->
  let rules = Modulog.To_rules.from_structure str in
  Format.printf
    "@[<v>%a@]\n"
    Datalog.Ruleset.pp rules;
  Ok ()

let rules_graph filename =
  parse_and_check filename >>= fun (str, sg) ->
  let rules = Modulog.To_rules.from_structure str in
  Format.printf
    "@[<v>%a@]\n"
    Datalog.Graphviz.dot_of_ruleset rules;
  Ok ()

let exec filename =
  parse_and_check filename >>= fun (str, sg) ->
  let rules    = Modulog.To_rules.from_structure str in
  let code     = Relation_machine.Of_rules.translate rules in
  let env      = Relation_machine.Interpreter.eval code in
  Format.printf
    "@[<v>%a@]\n"
    Relation_machine.Interpreter.Env.pp env;
  Ok ()

(**********************************************************************)
(* The command line interface *)

open Cmdliner

let filename_arg =
  Arg.(required
       & pos 0 (some string) None
       & info []
         ~docv:"FILENAME"
         ~doc:"Name of modular datalog file to process")

let with_indexes_opt =
  let doc = "Whether to print computed index information" in
  Arg.(value & flag & info ["i";"with-indexes"] ~doc)

let output_file_arg =
  let doc = "Output filename" in
  Arg.(value & opt (some string) None & info ["o";"output"] ~doc)

let typecheck_cmd =
  let doc = "Typecheck a Modular Datalog program and print the signature" in
  Cmd.v
    Cmd.(info "typecheck" ~doc)
    Term.(term_result' (const typecheck $ filename_arg))

let relmachine_cmd =
  let doc = "Compile a Modular Datalog program to the RelMachine" in
  Cmd.v
    Cmd.(info "relmachine" ~doc)
    Term.(term_result' (const relmachine $ filename_arg $ with_indexes_opt))

let gen_c_cmd =
  let doc = "Compile a Modular Datalog program to C" in
  Cmd.v
    Cmd.(info "gen_c" ~doc)
    Term.(term_result' (const gen_c $ filename_arg))

let compile_cmd =
  let doc = "Compile a Modular Datalog program to an executable via C" in
  Cmd.v
    Cmd.(info "compile" ~doc)
    Term.(term_result' (const compile $ filename_arg $ output_file_arg))

let rules_cmd =
  let doc = "Compile a Modular Datalog program to flat datalog rules" in
  Cmd.v
    Cmd.(info "rules" ~doc)
    Term.(term_result' (const rules $ filename_arg))

let rules_graph_cmd =
  let doc = "Compile a Modular Datalog program to a graph of datalog rules" in
  Cmd.v
    Cmd.(info "rules-graph" ~doc)
    Term.(term_result' (const rules_graph $ filename_arg))

let exec_cmd =
  let doc = "Execute a Modular Datalog program" in
  Cmd.v
    Cmd.(info "exec" ~doc)
    Term.(term_result' (const exec $ filename_arg))

let default_cmd =
  let doc = "a Modular Datalog compiler" in
  let sdocs = Manpage.s_common_options in
  (*let man = help_secs in*)
  Cmd.group
    ~default:Term.(ret (const (`Help (`Pager, None))))
    Cmd.(info "modulog" ~version:"v1.0.0" ~doc ~sdocs)
    [ typecheck_cmd
    ; relmachine_cmd
    ; gen_c_cmd
    ; rules_cmd
    ; rules_graph_cmd
    ; exec_cmd
    ; compile_cmd ]

let () =
  let r = Cmd.eval default_cmd in
  exit r

(*
  Term.(exit (eval_choice default_cmd [ ))
 *)
