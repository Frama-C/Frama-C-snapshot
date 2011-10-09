(* -------------------------------------------------------------------------- *)
(* --- LogicLang Tools w.r.t access paths                                 --- *)
(* -------------------------------------------------------------------------- *)

open LogicTau
open LogicLang

let basename = function
  | Integer -> "k"
  | Real -> "z"
  | Boolean -> "c"
  | Pointer -> "p"
  | ADT(id,_) | Record id -> LogicId.basename id
  | Array _ -> "A"
  | Set _ -> "S"
  | ALPHA _ -> "x"

let fresh pool tau = fresh pool (basename tau) tau

let e_shared pool tau term f =
  if is_atomic term then f term else
    let x = fresh pool tau in
    e_let x term (f (e_var x))

let p_shared pool tau term f =
  if is_atomic term then f term else
    let x = fresh pool tau in
    p_let x term (f (e_var x))
