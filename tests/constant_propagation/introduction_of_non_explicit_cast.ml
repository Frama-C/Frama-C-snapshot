let fmt =  Format.std_formatter;;

!Db.Value.compute ();;

let all = Cilutil.StringSet.empty;;
let new_proj = !Db.Constant_Propagation.run_propagation all ~cast_intro:true;;

Project.set_current new_proj;;
Format.printf "After Constant propagation :@." ; File.pretty ~prj:new_proj fmt;;
