let ( let* ) = Result.bind

let rec map_result f = function
  | [] -> Ok []
  | x::xs ->
    let* y = f x in
    let* ys = map_result f xs in
    Ok (y::ys)
