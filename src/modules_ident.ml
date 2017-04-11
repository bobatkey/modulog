type t =
  { name  : string
  ; stamp : int
  }

let currstamp = ref 0

let create s =
  incr currstamp;
  {name = s; stamp = !currstamp}

let name id =
  id.name

let full_name id = Printf.sprintf "%s/%d" id.name id.stamp

let equal id1 id2 =
  id1.stamp = id2.stamp

module OT = struct
  type nonrec t = t
  let compare x y =
    compare x.stamp y.stamp
end

module Table = struct
  include Map.Make (OT)
  let find key table =
    try Some (find key table) with Not_found -> None
end

let pp pp {name;stamp} =
  Format.fprintf pp "%s/%d" name stamp
