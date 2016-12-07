let main () =
  !Db.Value.compute ();
  Dynamic.Parameter.String.set "" "";
  Dynamic.Parameter.String.set "" "tests/misc/issue109.i";
  File.init_from_cmdline ();
  !Db.Value.compute ()

let main = Db.Main.extend main
