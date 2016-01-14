
let main _ =
  let all = Cil_datatype.Fundec.Set.empty in
  let new_proj = !Db.Constant_Propagation.get all true in
  Project.on
    new_proj
    (fun () ->
      Kernel.CodeOutput.output
        (fun fmt -> Format.fprintf fmt "After Constant propagation :@."))
    ();
  File.pretty_ast ~prj:new_proj ();;

let () = Db.Main.extend main
