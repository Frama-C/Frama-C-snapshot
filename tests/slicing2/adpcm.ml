(*
ledit bin/toplevel.top  -no-annot -deps -slicing_level 2 tests/slicing2/adpcm.c
#use "tests/slicing2/select.ml";;
*)

include LibSelect;;

(* Cmdline.slicing_level := 2;;  = MinimizeNbCalls *)

(*
let resname = "tests/slicing2/adpcm.sliced" in
ignore (test "uppol2" ~do_prop_to_callers:true ~resname (select_retres));;
*)
ignore (test "uppol2" ~do_prop_to_callers:true (select_retres));;

