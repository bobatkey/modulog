{
open Parser
}

let ident = ['A'-'Z''a'-'z']['A'-'Z''a'-'z''_''0'-'9''\'']*

rule token = parse
  [' ''\t']        { token lexbuf }
| '\n'             { Lexing.new_line lexbuf; token lexbuf }
| "(*"             { comment lexbuf; token lexbuf }
| ','              { COMMA }
| ":-"             { COLON_DASH }
| ":"              { COLON }
| "*"              { STAR }
| "."              { DOT }
| "->"             { ARROW }
| "="              { EQUALS }
| "external"       { EXTERNAL }
| "module"         { MODULE }
| "type"           { TYPE }
| "struct"         { STRUCT }
| "sig"            { SIG }
| "end"            { END }
| "functor"        { FUNCTOR }
| "int"            { INT }
| "and"            { AND }
| "define"         { DEFINE }
| "constant"       { CONSTANT }
| "with"           { WITH }
| "rec"            { REC }
| "pred"           { PRED }
| '('              { LPAREN }
| ')'              { RPAREN }
| '{'              { LBRACE }
| '}'              { RBRACE }
| '|'              { BAR }
| ['0'-'9']+ as x  { INT_LITERAL (Int32.of_string x) }
| ident as x       { IDENT x }
| '`'(ident as x)  { ENUM_IDENT x }
| '?'(ident as x)  { MV_IDENT x }
| '_'              { UNDERSCORE }
| _                { UNKNOWN }
| eof              { EOF }

and comment = parse
  "*)"             { }
| '\n'             { Lexing.new_line lexbuf; comment lexbuf }
| "(*"             { comment lexbuf; comment lexbuf }
| _                { comment lexbuf }
