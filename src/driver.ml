let () =
  let filename  = Sys.argv.(1) in
  let lexbuf    = Lexing.from_channel (open_in filename) in
  lexbuf.Lexing.lex_curr_p <- { Lexing.pos_fname = filename
                              ; pos_lnum = 1
                              ; pos_cnum = 0
                              ; pos_bol = 0
                              };
  let structure = Datalog_parser.program Datalog_lexer.token lexbuf in
  match Datalog_checker.(Typing.type_structure Env.empty structure) with
    | Ok (str, sg) ->
       let rules = Datalog_normalisation.rules_of_structure str in
       let code = Datalog_abstractmachine.translate rules in
       Format.printf
         "@[<v 0>%a@]\n"
         Datalog_abstractmachine.pp_comms code
    | Error err ->
       Format.printf
         "@[<v>%a@]\n"
         Datalog_checker.Typing.pp_error err
