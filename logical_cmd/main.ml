open Logical.Result_ext

let format_to_string formatter x =
  formatter Format.str_formatter x;
  Format.flush_str_formatter ()

let typecheck filename =
  let* script =
    In_channel.with_open_text filename
      (fun ch ->
	let lexbuf = Lexing.from_channel ch in
	match Logical.Parser.program Logical.Lexer.token lexbuf with
	| x -> Ok x
	| exception Logical.Parser.Error ->
	  let location =
	    Logical.Location.mk
	      lexbuf.Lexing.lex_start_p
	      lexbuf.Lexing.lex_curr_p
	  in
	  Error (format_to_string
	    (fun fmt -> Format.fprintf fmt "Parse error at %a" Logical.Location.pp)
	    location))
  in
  Logical.Toplevel.execute_script script

(******************************************************************************)
open Cmdliner

let filename_arg =
  let open Arg in
  required
  & pos 0 (some string) None
  & info []
    ~docv:"FILENAME"
    ~doc:"Name of logical theories to process"

let typecheck_cmd =
  let doc = "Typecheck a logical theory" in
  Cmd.v
    Cmd.(info "typecheck" ~doc)
    Term.(term_result' (const typecheck $ filename_arg))

let default_cmd =
  let doc = "a Modular Logical Structure tool" in
  let sdocs = Manpage.s_common_options in
  Cmd.group
    ~default:Term.(ret (const (`Help (`Pager, None))))
    Cmd.(info "logical" ~version:"v0.0.1" ~doc ~sdocs)
    [ typecheck_cmd ]

let () =
  let r = Cmd.eval default_cmd in
  exit r
