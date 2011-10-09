(* ledit bin/toplevel.top -deps tests/slicing/mark_all_slices.c
  #use "tests/slicing/select.ml";;
ou
  #use "tests/slicing/mark_all_slices.ml";;

*)

include LibSelect;;

let main _ =

  (* we are interesting in having several slices,
   * so use mode PreciseSlices *)
  (* SlicingKernel.Mode.Calls.set 3; *)

  (*~~~~~~~~~~~~ Project 1 : *)

  let project = mk_project () in
  let kf_main = Globals.Functions.find_def_by_name "main" in
  let kf_all = Globals.Functions.find_def_by_name "all" in

  (* create main_1 and select A2 in it *)
  let ff_main = !S.Slice.create project kf_main in
  let select = select_data "A2" project kf_main in
  !S.Request.add_slice_selection_internal project ff_main select;
  !S.Request.apply_all_internal project;
  extract_and_print project;

  (* add a global selection in 'all' to always compute its result.
   * This should modify the existing slice (all_1)
   * *)
  let select = select_retres project kf_all in
  !S.Request.add_selection_internal project select;
  !S.Request.apply_next_internal project;
  print_requests project;
  !S.Request.apply_all_internal project;
  extract_and_print project;

  (* select B2 in main_1 : this should create a second slice all_2
   * and its result should be computed even if it is not needed by this request
   *)
  let select = select_data "B2" project kf_main in
  !S.Request.add_slice_selection_internal project ff_main select;
  !S.Request.apply_next_internal project;
  print_requests project;
  !S.Request.apply_all_internal project;
  extract_and_print project;

  (*~~~~~~~~~~~~ Project 2 : *)

  let project = mk_project () in
  let kf_main = Globals.Functions.find_def_by_name "main" in
  let kf_all = Globals.Functions.find_def_by_name "all" in

  (* first all the global selection in 'all' to always compute its result.
   * This creates a first all_1 slice : I am not sure that this should be done.
   * *)
  let select = select_retres project kf_all in
  !S.Request.add_selection_internal project select;
  !S.Request.apply_next_internal project;
  print_requests project;
  !S.Request.apply_all_internal project;
  extract_and_print project;

  (* create main_1 and select A2 in it : this will create a new slice for all
   * that computes A and the result. *)
  let ff_main = !S.Slice.create project kf_main in
  let select = select_data "A2" project kf_main in
  !S.Request.add_slice_selection_internal project ff_main select;
  !S.Request.apply_all_internal project;
  extract_and_print project

let () = Db.Main.extend main
