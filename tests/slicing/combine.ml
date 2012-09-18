
open LibSelect;;

let f_slice_names kf src_called fnum =
  let fname = Kernel_function.get_name kf in
  if (fname = "main") || (fnum = 1 && not src_called) then fname
  else (fname ^ "_s_" ^ (string_of_int (fnum)))

(* To be able to build framac-journal.ml *)
let f_slice_names =
  Journal.register
    "Combine.f_slice_names"
    (Datatype.func Kernel_function.ty
       (Datatype.func Datatype.bool
	  (Datatype.func Datatype.int Datatype.string)))
    f_slice_names

let main _ =
  let project = mk_project () in

  let kf_main = Globals.Functions.find_def_by_name "main" in
  let kf_f = Globals.Functions.find_def_by_name "f" in

  !S.Project.change_slicing_level project kf_f 2;

  let ff_main = !S.Slice.create project kf_main in
  let select = select_retres project kf_main in
  let _ = !S.Request.add_slice_selection_internal project ff_main select in
  !S.Request.apply_all_internal project;

  extract_and_print project;

  Format.printf "Let's split 'f':@.";

  let ff_f = match !S.Slice.get_all project kf_f with
    | f :: [] -> f | _ -> assert false
  in

  ignore (!S.Request.split_slice project ff_f);
  !S.Request.apply_all_internal project;

  let proj2 = !S.Project.extract "slicing_result" ~f_slice_names project in
  Project.set_current proj2;
  Format.printf "After Slicing :@." ; File.pretty_ast ();

  (*
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

    let new_cil_file = Ast.get () in
    Cil.visitCilFile infos new_cil_file (* the cil file after slicing *);;
  *)

  !Db.Value.compute ();
  let all = Datatype.String.Set.empty in
  let proj3 = !Db.Constant_Propagation.get all ~cast_intro:true in
  Project.set_current proj3;
  Format.printf "After Constant propagation :@.";
  File.pretty_ast ~prj:proj3 ();

  let proj4 = !Db.Sparecode.get ~select_annot:true ~select_slice_pragma:true in
  Format.printf "After Sparecode :@.";
  File.pretty_ast ~prj:proj4 ();;

let () = Db.Main.extend main
