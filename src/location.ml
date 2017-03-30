type t =
  | Generated
  | FromSource of { loc_start : Lexing.position
                  ; loc_end   : Lexing.position
                  }

let mk loc_start loc_end =
  FromSource { loc_start; loc_end }
