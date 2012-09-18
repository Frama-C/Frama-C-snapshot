let main () =
  !Db.Value.compute ();
  Globals.set_entry_point "main2" false;
  !Db.Value.compute ();
;;

let () = Db.Main.extend main
