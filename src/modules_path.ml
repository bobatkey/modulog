type t =
  | Pident of Modules_ident.t
  | Pdot   of t * string

let rec equal p1 p2 =
  match p1, p2 with
    | Pident id1, Pident id2 ->
       Modules_ident.equal id1 id2
    | Pdot (r1, field1), Pdot (r2, field2) ->
       equal r1 r2 && field1 = field2
    | _, _ ->
       false
