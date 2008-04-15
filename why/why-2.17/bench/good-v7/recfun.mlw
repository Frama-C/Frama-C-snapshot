
(** Recursive functions *)

(** 1. Pure function *)

let rec f1 (x:int) : int { variant x } = 
  { x >= 0 } (if x > 0 then (f1 (x-1)) else x) { result = 0 }

(** 2. With effects but no argument *)

parameter x : int ref

let rec f2 (u:unit) : unit { variant x } =
  { x >= 0 } (if !x > 0 then begin x := !x - 1; (f2 void) end) { x = 0 }

(** 3. With effects and a pure argument *)

let rec f3 (a:int) : unit { variant a } =
  { a >= 0 } (if a > 0 then begin x := !x + 1; (f3 (a-1)) end) { x = x@ + a }

(** 4. With effects and a reference as argument *)

let rec f4 (a:int ref) : unit { variant a } =
  { a >= 0 } 
  (if !a > 0 then begin x := !x + 1; a := !a - 1; (f4 a) end)
  { x = x@ + a@ }

(** 5. The acid test: 
       partial application of a recursive function with effects *)

(*TODO
let rec f5 (a:int ref) (b:int ref) : unit { variant a } = !a + !b

let test_f5 = let f = (f5 x) in let b = ref 0 in (f b) { result = x }
*)

