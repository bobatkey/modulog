module MI = Parser.MenhirInterpreter

let trim_newline s =
  if String.length s > 0 && s.[String.length s - 1] = '\n' then
    String.sub s 0 (String.length s -1)
  else
    s

let parse source lexbuf =
  let rec loop cp = match cp with
    | MI.Accepted a ->
       Ok a
    | MI.InputNeeded env ->
       let token = Lexer.token lexbuf in
       let spos  = Lexing.lexeme_start_p lexbuf in
       let epos  = Lexing.lexeme_end_p lexbuf in
       loop (MI.offer cp (token, spos, epos))
    | MI.Shifting _ | MI.AboutToReduce _ ->
       loop (MI.resume cp)
    | MI.HandlingError env ->
       let pos     = Location.of_lexbuf lexbuf in
       let lexeme  = Lexing.lexeme lexbuf in
       let state   = MI.current_state_number env in
       let message = trim_newline (Parser_messages.message state) in
       Error (pos, message, state, lexeme)
    | MI.Rejected ->
       assert false
  in
  let init_pos = lexbuf.Lexing.lex_curr_p in
  loop (Parser.Incremental.program init_pos)
