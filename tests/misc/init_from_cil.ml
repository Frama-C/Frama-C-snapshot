let run () =
  let a = Ast.get () in
  let prj = Project.create "foo" in
  File.init_project_from_cil_file prj a;
  Project.set_current prj;
  Printer.pp_file Format.std_formatter (Ast.get())

let () = Db.Main.extend run
