let read_structure_from_file filename =
  let ch = open_in filename in
  try
    let lexbuf = Lexing.from_channel (open_in filename) in
    lexbuf.Lexing.lex_curr_p <-
      { Lexing.pos_fname = filename
      ; pos_lnum = 1
      ; pos_cnum = 0
      ; pos_bol = 0
      };
    let structure = Datalog_parser.program Datalog_lexer.token lexbuf in
    close_in ch;
    structure
  with e ->
    close_in ch; raise e

let typecheck filename =
  let structure = read_structure_from_file filename in
  match Datalog_checker.type_structure structure with
    | Ok (str, sg) ->
       Format.printf
         "@[<v>%a@]\n"
         Datalog_checker.Mod.pp_signature sg
    | Error err ->
       Format.printf
         "@[<v>%a@]\n"
         Datalog_checker.pp_error err

let relmachine filename =
  let structure = read_structure_from_file filename in
  match Datalog_checker.type_structure structure with
    | Ok (str, sg) ->
       let rules    = Datalog_normalisation.from_structure str in
       let code     = Relmachine_of_rules.translate rules in
       Format.printf
         "@[<v>%a@]\n"
         Relmachine_syntax.pp_program code
    | Error err ->
       Format.printf
         "@[<v>%a@]\n"
         Datalog_checker.pp_error err

let rules filename =
  let structure = read_structure_from_file filename in
  match Datalog_checker.type_structure structure with
    | Ok (str, sg) ->
       let rules = Datalog_normalisation.from_structure str in
       Format.printf
         "@[<v>%a@]\n"
         Datalog_ruleset.pp rules
    | Error err ->
       Format.printf
         "@[<v>%a@]\n"
         Datalog_checker.pp_error err

let exec filename =
  let structure = read_structure_from_file filename in
  match Datalog_checker.type_structure structure with
    | Ok (str, sg) ->
       let rules    = Datalog_normalisation.from_structure str in
       let code     = Relmachine_of_rules.translate rules in
       let env      = Relmachine_interpreter.eval code in
       Format.printf
         "@[<v>%a@]\n"
         Relmachine_interpreter.pp_relvarenv env
    | Error err ->
       Format.printf
         "@[<v>%a@]\n"
         Datalog_checker.pp_error err

(**********************************************************************)
(* The command line interface *)

open Cmdliner

let filename_arg =
  Arg.(required
       & pos 0 (some string) None
       & info []
         ~docv:"FILENAME"
         ~doc:"Name of modular datalog file to process")

let typecheck_cmd =
  let doc = "Typecheck a Modular Datalog program and print the signature" in
  Term.(const typecheck $ filename_arg),
  Term.info "typecheck" ~doc ~exits:Term.default_exits

let relmachine_cmd =
  let doc = "Compile a Modular Datalog program to the RelMachine" in
  Term.(const relmachine $ filename_arg),
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
  (*let sdocs = Manpage.s_common_options in*)
  let exits = Term.default_exits in
  (*let man = help_secs in*)
  Term.(ret (const (`Help (`Pager, None)))),
  Term.info "modlog" ~version:"v1.0.0" ~doc (*~sdocs*) ~exits (*~man*)

let () =
  Term.(exit (eval_choice default_cmd [ typecheck_cmd
                                      ; relmachine_cmd
                                      ; rules_cmd
                                      ; exec_cmd ]))
