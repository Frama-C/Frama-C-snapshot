open Cil_types

class visitor prj =
  object
    inherit Visitor.frama_c_copy prj
    method vfunc f = f.svar.vname <- "g"; Cil.SkipChildren
  end

let run () =
  ignore
    (File.create_project_from_visitor 
       "change_main" (fun prj -> new visitor prj))

let () = Db.Main.extend run
