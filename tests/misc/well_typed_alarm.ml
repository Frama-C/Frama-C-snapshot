let main () =
  !Db.Value.compute();
  Filecheck.check_ast "Check alarm";
  File.pretty_ast ()

let () = Db.Main.extend main
