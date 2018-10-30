let foo () =
  if Project.get_name (Project.current ()) <> "prj" then begin
    let prj = Project.create "prj" in
    let () = Project.set_current prj in
    File.init_from_c_files 
      [File.from_filename "tests/builtins/big_local_array.i"]
  end

let () = Db.Main.extend foo
