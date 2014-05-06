(* ledit bin/toplevel.top -deps tests/slicing/select_by_annot.c \
   < tests/slicing/select_by_annot.ml
*)


open LibSelect;;

let main _ =
  let project = mk_project () in
  let pretty_pdg fmt kf = !Db.Pdg.pretty fmt (!Db.Pdg.get kf) in
  let add_annot kf =
    let mark = !S.Mark.make ~data:true ~addr:false ~ctrl:false in
    let select = S.Select.empty_selects in
    let select = !S.Select.select_func_annots select mark
      ~spare:true ~threat:false ~user_assert:false ~slicing_pragma:true
      ~loop_inv:true ~loop_var:true kf
    in
    !Db.Slicing.Request.add_persistent_selection project select
      (*!S.Request.read_annotations project kf_main ;;*)
  in
  let kf_main = Globals.Functions.find_def_by_name "main" in
  add_annot kf_main;
  Format.printf "@[%a@]@\n" pretty_pdg kf_main;

  let kf_modifS = Globals.Functions.find_def_by_name "modifS" in
  (*!S.Request.read_annotations project kf_modifS ;;*)
  add_annot kf_modifS;
  Format.printf "@[%a@]@\n" pretty_pdg kf_modifS;

  !S.Request.pretty Format.std_formatter project;
  !S.Request.apply_all_internal  project;

  !S.Project.pretty Format.std_formatter project;
  extract_and_print project;

  (** create another slice for "main" to check if it also contains the previous
      * selection. *)

  let ff = !S.Slice.create project kf_main in
  let select = LibSelect.select_data "b" project kf_main in
  !S.Request.add_slice_selection_internal project ff select;

  !S.Request.apply_all_internal  project;

  !S.Project.pretty Format.std_formatter project;
  extract_and_print project

let () = Db.Main.extend main

