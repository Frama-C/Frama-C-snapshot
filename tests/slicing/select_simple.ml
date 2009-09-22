(* ledit bin/toplevel.top -deps tests/slicing/simple_intra_slice.c
* *)

include LibSelect ;;

let main _ =
  ignore (test_select_data "f1" "G");
  ignore (test_select_retres "f1");
  !Db.Slicing.set_modes ~calls:2 ();
  ignore (test_select_retres "f2");
  ignore (test_select_data "f6" "n");

  ignore (test_select_retres "f7");
  ignore (test_select_data "f7" "S.a");
  ignore (test_select_data "f7" "S.b");
  ignore (test_select_data "f7" "S");
  ignore (test_select_data "f7" "XXX");

  ignore (test_select_data "f8" "ps->a");
  ignore (test_select_data "f8" "ps->b");
  ignore (test_select_data "f8" "ps->c");
  ignore (test_select_data "f8" "*ps")

let () = Db.Main.extend main
