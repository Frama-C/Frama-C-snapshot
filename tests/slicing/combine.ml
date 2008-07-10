
open LibSelect;;

let project = S.Project.mk_project "slicing1";;

let kf_main = Globals.Functions.find_def_by_name "main";;
let kf_f = Globals.Functions.find_def_by_name "f";;

!S.Project.change_slicing_level project kf_f 2;;

let ff_main = !S.Slice.create project kf_main;;
let select = select_retres project kf_main;;
let _ = !S.Request.add_slice_selection_internal project ff_main select;;
  !S.Request.apply_all_internal project;;

extract_and_print project;;

Format.printf "Let's split 'f':@.";;

let ff_f = match !S.Slice.get_all project kf_f with
  | f :: [] -> f | _ -> assert false;;

!S.Request.split_slice project ff_f;;
!S.Request.apply_all_internal project;;

let f_slice_names kf src_called fnum =
  let fname = Kernel_function.get_name kf in
  if (fname = "main") || (fnum = 1 && not src_called) then fname
  else (fname ^ "_s_" ^ (string_of_int (fnum)))
;;

let proj2 = !S.Project.extract "slicing_result" ~f_slice_names project;;
Project.set_current proj2;;
Format.printf "After Slicing :@." ; File.pretty ~prj:proj2 fmt;;

(*
open Cil_types;;
let infos = object
  inherit Cil.nopCilVisitor
  method vfunc { svar = v } =
    Cil.log "function definition of %s (id %d at address %x)@."
            v.vname v.vid (Obj.magic v);
    Cil.DoChildren
  method vvdec v =
    Cil.log "variable definition of %s (id %d at address %x)@."
            v.vname v.vid (Obj.magic v);
    Cil.SkipChildren
  method vvrbl v =
    Cil.log "use of %s (id %d at address %x)@." v.vname v.vid (Obj.magic v);
    Cil.SkipChildren
end;;

let new_cil_file = Cil_state.file () in
Cil.visitCilFile infos new_cil_file (* the cil file after slicing *);;
*)

!Db.Value.compute ();;
let all = Cilutil.StringSet.empty;;
let proj3 = !Db.Constant_Propagation.run_propagation all ~cast_intro:true;;
Project.set_current proj3;;
Format.printf "After Constant propagation :@." ; File.pretty ~prj:proj3 fmt;;

let proj4 = !Db.Sparecode.run ~select_annot:true ~select_slice_pragma:true;;
Format.printf "After Sparecode :@." ; File.pretty ~prj:proj4 fmt;;
