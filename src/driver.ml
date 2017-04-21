open Cmdliner

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
       let code     = Datalog_abstractmachine.translate rules in
       let patterns = Datalog_abstractmachine.search_patterns code in
       let indexes  = Datalog_abstractmachine.indexes code in
       Format.printf
         "@[<v>%a@@,%a@,%a@,%a@]\n"
         Datalog_ruleset_graphviz.dot_of_ruleset  rules
         Datalog_abstractmachine.pp_comms         code
         Datalog_abstractmachine.PredicatePats.pp patterns
         Datalog_abstractmachine.pp_all_indexes   indexes
    | Error err ->
       Format.printf
         "@[<v>%a@]\n"
         Datalog_checker.Typing.pp_error err

let filename =
  Arg.(required
       & pos 0 (some string) None
       & info []
         ~docv:"FILENAME"
         ~doc:"Name of modular datalog file to process")

let modlog = Term.(const go $ filename)

let info =
  let doc = "Compile a Modular Datalog program" in
  Term.info "modlog" ~version:"0.0.1" ~doc ~exits:Term.default_exits

let () =
  Term.exit @@ Term.eval (modlog, info)
