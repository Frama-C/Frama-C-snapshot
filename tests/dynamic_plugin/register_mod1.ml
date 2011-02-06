module L = List
open Datatype
module List = L

(* Test with a mutual recursive function *)
let f_test i j=
  Format.printf "Use f_test %d %d@." i j;
  if i = 0 then j
  else Dynamic.get ~plugin:"Register_mod2" "g_test" (func int int) (j-1)

let __ : int =
  Dynamic.register ~plugin:"Register_mod1" "f_test" ~journalize:false
    (func int (func int int)) f_test 3 4
