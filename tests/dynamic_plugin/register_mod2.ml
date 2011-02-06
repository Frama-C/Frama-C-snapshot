module L = List
open Datatype
module List = L

(* Test with a mutual recursive function *)
let g_test j=
  Format.printf "Use g_test %d@." j;
  if j mod 3 = 0 then j
  else
    Dynamic.get ~plugin:"Register_mod1" "f_test"
      (func int (func int int))
      (j-1)
      j

let __ : int -> int =
  Dynamic.register ~journalize:false ~plugin:"Register_mod2" "g_test"
    (func int int)
    g_test
