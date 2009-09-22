
let main _ =
  let all = Cilutil.StringSet.empty in
  let new_proj = !Db.Constant_Propagation.get all true in
  Project.set_current new_proj;
  Parameters.CodeOutput.output "After Constant propagation :@.";
  File.pretty ~prj:new_proj ();;

let () = Db.Main.extend main
