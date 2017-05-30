(* ledit bin/toplevel.top -deps tests/slicing/switch.c
* *)

include LibSelect ;;

let main _ =
  test_select_data "main" "x";
  test_select_data "main" "y";
  test_select_data "main" "z"
let () = Db.Main.extend main

