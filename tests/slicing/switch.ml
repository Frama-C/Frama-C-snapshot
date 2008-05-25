(* ledit bin/toplevel.top -deps tests/slicing/switch.c
* *)

include LibSelect ;;

test_select_data "main" "x";;
test_select_data "main" "y";;
test_select_data "main" "z";;

