type t = string * int

let fresh = let c = ref 0 in fun n -> incr c; n, !c

let find v a b =
  List.assoc v (List.map2 (fun a b -> a, b) a b)

let uid (_, x) = x

let name (x, _) = x

let uname x =
  name x ^ string_of_int (uid x)
