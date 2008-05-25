(* 
ledit bin/toplevel.top  -deps tests/slicing/horwitz.c
#use "tests/slicing/select.ml";;

tests/slicing/horwitz.byte -deps tests/slicing/horwitz.c
* *)

include LibSelect;;

ignore (test_select_data ~do_prop_to_callers:true "incr" "*pi");;


