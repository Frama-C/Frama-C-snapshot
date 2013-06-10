open Cil_types

let main () =
  Ast.compute ();
  let o prj = object(self)
    inherit Visitor.frama_c_copy prj
    method vglob_aux _g = Cil.JustCopy
  end
  in
  ignore (File.create_project_from_visitor "justcopy" o)

let () = Db.Main.extend main
