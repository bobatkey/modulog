type t = Path.t Ident.Table.t

let identity = Ident.Table.empty

let add = Ident.Table.add

let rec path sub = function
  | Path.Pident id as p ->
     (match Ident.Table.find id sub with
       | None   -> p
       | Some p -> p)
  | Path.Pdot (root, field) ->
     Path.Pdot (path sub root, field)
