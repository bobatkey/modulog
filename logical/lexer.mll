{
open Parser
}

let ident = ['a'-'z']['A'-'Z''a'-'z''_''0'-'9''\'''-']*
let symbol = ['A'-'Z']['A'-'Z''a'-'z''_''0'-'9''\'''-']*

rule token = parse
  [' ''\t']        { token lexbuf }
| '\n'             { Lexing.new_line lexbuf; token lexbuf }
| "(*"             { comment lexbuf; token lexbuf }
| ","              { COMMA }
| ":"              { COLON }
| "*"              { STAR }
| "."              { DOT }
| "="              { EQUALS }
| '('              { LPAREN }
| ')'              { RPAREN }
| '['              { LSQBRACK }
| ']'              { RSQBRACK }

| "module"         { MODULE }
| "type"           { TYPE }
| "struct"         { STRUCT }
| "sig"            { SIG }
| "end"            { END }
| "functor"        { FUNCTOR }
| "and"            { AND }
| "rec"            { REC }
| "with"           { WITH }

| "pred"           { PRED }
| "func"           { FUNC }
| "axiom"          { AXIOM }
| "sort"           { SORT }
| "check"          { CHECK }

| "true"           { TRUE }
| "false"          { FALSE }
| "&"              { CONJ }
| "|"              { DISJ }
| "¬"              { NEGATE }
| "forall"         { FORALL }
| "exists"         { EXISTS }
| "->"             { ARROW }
| "=="             { EQUALS_EQUALS }

| "synth"          { SYNTH }
| "display"        { DISPLAY }

| ident as x       { IDENT x }
| symbol as x      { SYMBOL x }
| _                { UNKNOWN }
| eof              { EOF }

and comment = parse
  "*)"             { }
| '\n'             { Lexing.new_line lexbuf; comment lexbuf }
| "(*"             { comment lexbuf; comment lexbuf }
| _                { comment lexbuf }
