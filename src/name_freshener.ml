let get_number_suffix str =
  let rec find_split i =
    if i = 0 then 0
    else match str.[i-1] with
      | '0' .. '9' -> find_split (i-1)
      | _          -> i
  in
  let l = String.length str in
  let i = find_split l in
  if i = l then
    (str, None)
  else
    (String.sub str 0 i,
     Some (int_of_string (String.sub str i (l - i))))

let fresh_for used base =
  if not (used base) then
    base
  else
    (* 1. split base into prefix + 00s + digit suffix *)
    let base, num = get_number_suffix base in
    let create_candidate = function
      | None   -> base, Some 1
      | Some i -> base ^ string_of_int i, Some (i+1)
    in
    let rec find suffix =
      let candidate, suffix = create_candidate suffix in
      if used candidate then find suffix
      else candidate
    in
    find num
