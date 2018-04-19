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
    let result = Parser_driver.parse lexbuf in
    close_in ch;
    result
  with e ->
    close_in ch; raise e

