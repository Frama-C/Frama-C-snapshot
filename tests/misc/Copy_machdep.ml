let run () =
  let proj =
    File.create_project_from_visitor "machdep"
      (fun prj -> new Visitor.frama_c_copy prj)
  in
  Kernel.feedback "Machdep is %spreserved"
    (if Kernel.Machdep.get () = Project.on proj Kernel.Machdep.get () then
        "" else "not ");
  Kernel.feedback "Enums is %spreserved"
    (if Kernel.Enums.get () = Project.on proj Kernel.Enums.get () then
        "" else "not ");
  Kernel.feedback "Unicode is %spreserved"
    (if Kernel.Unicode.get () = Project.on proj Kernel.Unicode.get () then
        "" else "not ")

let () = Db.Main.extend run

