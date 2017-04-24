let typecheck filename =
  let structure = Modlog.read_structure_from_file filename in
  match Modlog.Checker.type_structure structure with
    | Ok (str, sg) ->
       Format.printf
         "@[<v>%a@]\n"
         Modlog.Checker.Mod.pp_signature sg

    | Error err ->
       Format.printf
         "@[<v>%a@]\n"
         Modlog.Checker.pp_error err

let relmachine filename with_indexes =
  let structure = Modlog.read_structure_from_file filename in
  match Modlog.Checker.type_structure structure with
    | Ok (str, sg) ->
       let rules    = Modlog.To_rules.from_structure str in
       let code     = Relmachine_of_rules.translate rules in
       Format.printf
         "@[<v>%a@]\n%!"
         Relmachine_syntax.pp_program code;
       if with_indexes then begin
         let indexes = Relmachine_indexes.indexes code in
         Format.printf
           "\n@[<v>%a@]\n%!"
           Relmachine_indexes.pp_all_indexes indexes
       end

    | Error err ->
       Format.printf
         "@[<v>%a@]\n"
         Modlog.Checker.pp_error err

let rules filename =
  let structure = Modlog.read_structure_from_file filename in
  match Modlog.Checker.type_structure structure with
    | Ok (str, sg) ->
       let rules = Modlog.To_rules.from_structure str in
       Format.printf
         "@[<v>%a@]\n"
         Datalog_ruleset.pp rules

    | Error err ->
       Format.printf
         "@[<v>%a@]\n"
         Modlog.Checker.pp_error err

let exec filename =
  let structure = Modlog.read_structure_from_file filename in
  match Modlog.Checker.type_structure structure with
    | Ok (str, sg) ->
       let rules    = Modlog.To_rules.from_structure str in
       let code     = Relmachine_of_rules.translate rules in
       let env      = Relmachine_interpreter.eval code in
       Format.printf
         "@[<v>%a@]\n"
         Relmachine_interpreter.pp_relvarenv env

    | Error err ->
       Format.printf
         "@[<v>%a@]\n"
         Modlog.Checker.pp_error err

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

let typecheck_cmd =
  let doc = "Typecheck a Modular Datalog program and print the signature" in
  Term.(const typecheck $ filename_arg),
  Term.info "typecheck" ~doc ~exits:Term.default_exits

let relmachine_cmd =
  let doc = "Compile a Modular Datalog program to the RelMachine" in
  Term.(const relmachine $ filename_arg $ with_indexes_opt),
  Term.info "relmachine" ~doc ~exits:Term.default_exits

let rules_cmd =
  let doc = "Compile a Modular Datalog program to flat datalog rules" in
  Term.(const rules $ filename_arg),
  Term.info "rules" ~doc ~exits:Term.default_exits

let exec_cmd =
  let doc = "Execute a Modular Datalog program" in
  Term.(const exec $ filename_arg),
  Term.info "exec" ~doc ~exits:Term.default_exits

let default_cmd =
  let doc = "a Modular Datalog compiler" in
  let sdocs = Manpage.s_common_options in
  let exits = Term.default_exits in
  (*let man = help_secs in*)
  Term.(ret (const (`Help (`Pager, None)))),
  Term.info "modlog" ~version:"v1.0.0" ~doc ~sdocs ~exits (*~man*)

let () =
  Term.(exit (eval_choice default_cmd [ typecheck_cmd
                                      ; relmachine_cmd
                                      ; rules_cmd
                                      ; exec_cmd ]))
