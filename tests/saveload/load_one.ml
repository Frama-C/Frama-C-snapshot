let () = at_exit (fun _ -> Sys.remove "tests/saveload/result/load_one.sav")

let main () =
  let sparecode () =
    !Db.Sparecode.get ~select_annot:false ~select_slice_pragma:false
  in
  let p = sparecode () in
  Project.save "tests/saveload/result/load_one.sav";
  Project.remove ~project:p ();
  let p = Project.load "tests/saveload/result/load_one.sav" in
  Project.on p (fun () -> !Db.Value.compute (); ignore (sparecode ())) ()

let () = Db.Main.extend main

(* testing Project.create_by_copy *)
let main2 () =
  !Db.Value.compute ();
  let prj = Project.create_by_copy "copy" in
  Format.printf "INIT AST@.";
  File.pretty_ast ();
  Format.printf "COPY AST@.";
  File.pretty_ast ~prj ()

let () = Db.Main.extend main2

