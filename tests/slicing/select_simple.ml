(* ledit bin/toplevel.top -deps tests/slicing/simple_intra_slice.c
* *)

include LibSelect ;;

 test_select_data "f1" "G";;
 test_select_retres "f1" ;;

 Cmdline.Slicing.Mode.Calls.set 2;;

 test_select_retres "f2" ;;

 test_select_data "f6" "n";;

 test_select_retres "f7" ;;
 test_select_data "f7" "S.a";;
 test_select_data "f7" "S.b";;
 test_select_data "f7" "S";;
 test_select_data "f7" "XXX";;

 test_select_data "f8" "ps->a";;
 test_select_data "f8" "ps->b";;
 test_select_data "f8" "ps->c";;
 test_select_data "f8" "*ps";;
