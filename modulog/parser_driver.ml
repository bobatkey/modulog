module MI = Parser.MenhirInterpreter

(* Plan: to use the same idea as CompCert's error messages and use $0,
   $1, etc. to refer to items on the parse stack. *)

(* FIXME: this doesn't handle UTF-8 at all... *)
let shorten s =
  if String.length s <= 35 then
    s
  else
    (String.sub s 0 15
     ^ "....."
     ^ String.sub s (String.length s - 15) 15)

let extract filename spos epos =
  assert (spos >= 0);
  assert (spos <= epos);
  let ch = open_in filename in
  seek_in ch spos;
  let str =
    match really_input_string ch (epos - spos) with
      | exception End_of_file -> "???"
      | str -> str
  in
  close_in ch;
  shorten (String.map (function '\x00' .. '\x1f' | '\x80' .. '\xff' -> ' ' | c -> c) str)

let digit_of_char c =
  Char.(code c - code '0')

(* Expand out references of the form $i to the piece of the input that
   is referred to by that element of the current parse stack. *)
let expand_message filename env message =
  let buf = Buffer.create (String.length message) in
  let add_extract sidx eidx =
    if sidx < eidx then
      Buffer.add_string buf "\"???\""
    else match MI.get sidx env, MI.get eidx env with
      | None, _ | _, None ->
         Buffer.add_string buf "\"???\""
      | Some (MI.Element (_, _, spos, _)),
        Some (MI.Element (_, _, _, epos)) ->
         let text = extract filename spos.Lexing.pos_cnum epos.Lexing.pos_cnum in
         Printf.bprintf buf "%S" text
  in
  let rec loop i =
    if i < String.length message
    then match message.[i] with
      | '$' -> read_stack_idx (i+1)
      | '\n' when i+1 = String.length message ->
         (* trim the newline off the end *)
         ()
      | c -> Buffer.add_char buf c; loop (i+1)
  and read_stack_idx i =
    if i = String.length message then
      Buffer.add_char buf '$'
    else match message.[i] with
      | '0' .. '9' as c ->
         read_stack_idx_int (digit_of_char c) (i+1)
      | c ->
         Buffer.add_char buf '$';
         Buffer.add_char buf c;
         loop (i+1)
  and read_stack_idx_int r i =
    if i = String.length message then
      add_extract r r
    else match message.[i] with
      | '0' .. '9' as c ->
         read_stack_idx_int (digit_of_char c + 10*r) (i+1)
      | '-' ->
         read_snd_stack_idx r (i+1)
      | c ->
         add_extract r r;
         Buffer.add_char buf c;
         loop (i+1)
  and read_snd_stack_idx r i =
    if i = String.length message then
      Printf.bprintf buf "$%d-" r
    else match message.[i] with
      | '0' .. '9' as c ->
         read_snd_stack_idx_int r (digit_of_char c) (i+1)
      | c ->
         Printf.bprintf buf "$%d-" r;
         Buffer.add_char buf c;
         loop (i+1)
  and read_snd_stack_idx_int r s i =
    if i = String.length message then
      add_extract r s
    else match message.[i] with
      | '0' .. '9' as c ->
         read_snd_stack_idx_int r (digit_of_char c + 10*s) (i+1)
      | c ->
         add_extract r s;
         Buffer.add_char buf c;
         loop (i+1)
  in
  loop 0;
  Buffer.contents buf

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
       let message =
         try Parser_messages.message state
         with Not_found -> "unknown parse error"
       in
       let message = expand_message source env message in
       Error (pos, message, lexeme)
    | MI.Rejected ->
       assert false
  in
  let init_pos = lexbuf.Lexing.lex_curr_p in
  loop (Parser.Incremental.program init_pos)
