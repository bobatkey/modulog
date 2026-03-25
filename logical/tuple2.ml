type ('a, 'b) t = 'a * 'b

let map_snd f (a, b) = (a, f b)
