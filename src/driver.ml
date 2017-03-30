let () =
  let filename  = Sys.argv.(1) in
  let lexbuf    = Lexing.from_channel (open_in filename) in
  let structure = Datalog_parser.program Datalog_lexer.token lexbuf in
  let _str, sg  = Datalog_checker.(Typing.type_structure Env.empty structure)
  in
  Format.pp_open_vbox Format.std_formatter 0;
  Datalog_checker.Mod.pp_signature
    Format.std_formatter
    sg;
  Format.pp_close_box Format.std_formatter ();
  Format.pp_print_newline Format.std_formatter ();
  Format.pp_flush_formatter Format.std_formatter
