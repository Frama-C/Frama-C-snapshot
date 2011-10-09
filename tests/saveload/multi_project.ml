let check name test =
  Kernel.log "Checking %S@." name;
  Project.on
    (Project.from_unique_name name)
    (fun () -> assert (test (Kernel.Files.get ()) [])) ()

let main () =
  ignore (Project.create_by_copy "foo");
  ignore (Project.create "foobar");
  Project.save_all "foo.sav";
  check "foo" (<>);
  check "foobar" (=);
  check "default" (<>);
  Kernel.Files.set [];
  Project.load_all "foo.sav";
  Extlib.safe_remove "foo.sav";
  ignore (Project.create_by_copy "bar");
  assert
    (Project.equal (Project.current ()) (Project.from_unique_name "default"));
  check "foo" (<>);
  check "foobar" (=);
  check "default" (<>);
  check "bar" (<>)

let () = Db.Main.extend main
