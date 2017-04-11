type t =
  | Generated
  | FromSource of { loc_start : Lexing.position
                  ; loc_end   : Lexing.position
                  }

let mk loc_start loc_end =
  FromSource { loc_start; loc_end }

let generated = Generated

open Lexing

let pp fmt = function
  | Generated ->
     Format.pp_print_string fmt "<generated>"
  | FromSource { loc_start; loc_end }
    when loc_start.pos_lnum = loc_end.pos_lnum ->
     Format.fprintf fmt
       "file %S, line %d, characters %d-%d"
       loc_start.pos_fname
       loc_start.pos_lnum
       (loc_start.pos_cnum-loc_start.pos_bol)
       (loc_end.pos_cnum-loc_end.pos_bol)
  | FromSource { loc_start; loc_end } ->
     Format.fprintf fmt
       "file %S, line %d, character %d, to line %d, character %d, "
       loc_start.pos_fname
       loc_start.pos_lnum
       (loc_start.pos_cnum-loc_start.pos_bol)
       loc_end.pos_lnum
       (loc_end.pos_cnum-loc_end.pos_bol)

let pp_without_filename fmt = function
  | Generated ->
     Format.pp_print_string fmt "<generated>"
  | FromSource { loc_start; loc_end }
    when loc_start.pos_lnum = loc_end.pos_lnum ->
     Format.fprintf fmt
       "line %d, characters %d-%d"
       loc_start.pos_lnum
       (loc_start.pos_cnum-loc_start.pos_bol)
       (loc_end.pos_cnum-loc_end.pos_bol)
  | FromSource { loc_start; loc_end } ->
     Format.fprintf fmt
       "line %d, character %d, to line %d, character %d, "
       loc_start.pos_lnum
       (loc_start.pos_cnum-loc_start.pos_bol)
       loc_end.pos_lnum
       (loc_end.pos_cnum-loc_end.pos_bol)
