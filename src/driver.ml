open Cmdliner

module Modlog = struct
  module Parser    = Datalog_parser
  module Lexer     = Datalog_lexer
  module Checker   = Datalog_checker
  module Flattener = Datalog_normalisation
end

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

(* Possible tasks:
   1. Typecheck
   2. Print rules as graphviz
   3. Print abstract machine code
   4. Print indexes
*)

let go filename =
  let structure = read_structure_from_file filename in
  match Datalog_checker.type_structure structure with
    | Ok (str, sg) ->
       let rules    = Datalog_normalisation.rules_of_structure str in
       let code     = Relmachine_syntax.translate rules in
       let patterns = Relmachine_syntax.search_patterns code in
       let indexes  = Relmachine_syntax.indexes code in
       Format.printf
         "@[<v>%a@,%a@,%a@,%a@]\n"
         Datalog_ruleset_graphviz.dot_of_ruleset  rules
         Relmachine_syntax.pp_comms         code
         Relmachine_syntax.PredicatePats.pp patterns
         Relmachine_syntax.pp_all_indexes   indexes
    | Error err ->
       Format.printf
         "@[<v>%a@]\n"
         Datalog_checker.Typing.pp_error err

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
         Datalog_checker.Typing.pp_error err

let relmachine filename =
  let structure = read_structure_from_file filename in
  match Datalog_checker.type_structure structure with
    | Ok (str, sg) ->
       let rules    = Datalog_normalisation.rules_of_structure str in
       let code     = Relmachine_syntax.translate rules in
       Format.printf
         "@[<v>%a@]\n"
         Relmachine_syntax.pp_comms code
    | Error err ->
       Format.printf
         "@[<v>%a@]\n"
         Datalog_checker.Typing.pp_error err

let exec filename =
  let structure = read_structure_from_file filename in
  match Datalog_checker.type_structure structure with
    | Ok (str, sg) ->
       let rules    = Datalog_normalisation.rules_of_structure str in
       let code     = Relmachine_syntax.translate rules in
       let env      = Relmachine_interpreter.eval code in
       Format.printf
         "@[<v>%a@]\n"
         Relmachine_interpreter.pp_relvarenv env
    | Error err ->
       Format.printf
         "@[<v>%a@]\n"
         Datalog_checker.Typing.pp_error err

(**********************************************************************)

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
                                      ; exec_cmd ]))
