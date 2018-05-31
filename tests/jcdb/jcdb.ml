let print_json () =
  Kernel.feedback
    "Value of -json-compilation-database in %s is %s"
    (Project.get_name (Project.current()))
    (Kernel.JsonCompilationDatabase.get())

let run () =
  print_json ();
  Ast.compute();
  let prj = Project.create_by_copy ~last:true "copy" in
  Project.on prj print_json ()

let () = Db.Main.extend run
