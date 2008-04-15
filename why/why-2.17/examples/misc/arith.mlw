
(* Integer multiplication (follows the same principle---2n or 2n+1
   decomposition---that fast exponentiation (see file power.ml) *)

let mult = fun (x:int) (y:int) ->
  { x >= 0 and y >= 0 }
  (let a = ref x in
   let b = ref y in
   let p = ref 0 in
   begin
     while !a <> 0 do
       { invariant a >= 0 and p+a*b = x*y as Inv   variant a }
       if !a % 2 = 1 then p := !p + !b;
       a := !a / 2;
       b := 2 * !b
     done;
     !p
   end)
  { result = x * y }

