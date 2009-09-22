(* ledit bin/toplevel.top -deps tests/slicing/switch.c
* *)

include LibSelect ;;

let main _ =
  ignore (test_select_data "main" "x");
  ignore (test_select_data "main" "y");
  ignore (test_select_data "main" "z")
let () = Db.Main.extend main

