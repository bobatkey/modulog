let read_structure_from_file filename =
  In_channel.with_open_text filename
    (fun ch ->
      let lexbuf = Lexing.from_channel ch in
      lexbuf.Lexing.lex_curr_p <- {
	Lexing.pos_fname = filename;
	pos_lnum = 1;
	pos_cnum = 0;
	pos_bol = 0;
      };
      Parser_driver.parse lexbuf)
