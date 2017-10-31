(* ledit bin/toplevel.top -deps tests/slicing/select_by_annot.c \
   < tests/slicing/select_by_annot.ml
*)


open LibSelect;;

let main _ =
  Slicing.Api.Project.reset_slicing ();
  let pretty_pdg fmt kf = !Db.Pdg.pretty fmt (!Db.Pdg.get kf) in
  let add_annot kf =
    let mark = Slicing.Api.Mark.make ~data:true ~addr:false ~ctrl:false in
    let select = Slicing.Api.Select.empty_selects in
    let select = Slicing.Api.Select.select_func_annots select mark
      ~spare:true ~threat:false ~user_assert:false ~slicing_pragma:true
      ~loop_inv:true ~loop_var:true kf
    in
    Slicing.Api.Request.add_persistent_selection select
      (*Slicing.Api.Request.read_annotations project kf_main ;;*)
  in
  let kf_main = Globals.Functions.find_def_by_name "main" in
  add_annot kf_main;
  Format.printf "@[%a@]@\n" pretty_pdg kf_main;

  let kf_modifS = Globals.Functions.find_def_by_name "modifS" in
  (*Slicing.ApiRequest.read_annotations project kf_modifS ;;*)
  add_annot kf_modifS;
  Format.printf "@[%a@]@\n" pretty_pdg kf_modifS;

  Slicing.Api.Request.pretty Format.std_formatter;
  Slicing.Api.Request.apply_all_internal ();

  Slicing.Api.Project.pretty Format.std_formatter;
  extract_and_print ();

  (** create another slice for "main" to check if it also contains the previous
      * selection. *)

  let ff = Slicing.Api.Slice.create kf_main in
  let select = LibSelect.select_data "b" kf_main in
  Slicing.Api.Request.add_slice_selection_internal ff select;

  Slicing.Api.Request.apply_all_internal ();

  Slicing.Api.Project.pretty Format.std_formatter;
  extract_and_print ()

let () = Db.Main.extend main
