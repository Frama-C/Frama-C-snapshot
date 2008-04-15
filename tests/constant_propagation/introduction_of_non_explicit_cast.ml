let fmt =  Format.std_formatter;;

let all = Cilutil.StringSet.empty;;
let new_proj = !Db.Constant_Propagation.get all true;;

Project.set_current new_proj;;
Format.printf "After Constant propagation :@." ; File.pretty ~prj:new_proj fmt;;
