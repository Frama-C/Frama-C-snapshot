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

  Slicing.Api.Project.reset_slicing ();
  let kf_main = Globals.Functions.find_def_by_name "main" in
  let kf_all = Globals.Functions.find_def_by_name "all" in

  (* create main_1 and select A2 in it *)
  let ff_main = Slicing.Api.Slice.create kf_main in
  let select = select_data "A2" kf_main in
  Slicing.Api.Request.add_slice_selection_internal ff_main select;
  Slicing.Api.Request.apply_all_internal ();
  extract_and_print ();

  (* add a global selection in 'all' to always compute its result.
   * This should modify the existing slice (all_1)
   * *)
  let select = select_retres kf_all in
  Slicing.Api.Request.add_selection_internal select;
  Slicing.Api.Request.apply_next_internal ();
  print_requests ();
  Slicing.Api.Request.apply_all_internal ();
  extract_and_print ();

  (* select B2 in main_1 : this should create a second slice all_2
   * and its result should be computed even if it is not needed by this request
   *)
  let select = select_data "B2" kf_main in
  Slicing.Api.Request.add_slice_selection_internal ff_main select;
  Slicing.Api.Request.apply_next_internal ();
  print_requests ();
  Slicing.Api.Request.apply_all_internal ();
  extract_and_print ();

  (*~~~~~~~~~~~~ Project 2 : *)

  Slicing.Api.Project.reset_slicing ();
  let kf_main = Globals.Functions.find_def_by_name "main" in
  let kf_all = Globals.Functions.find_def_by_name "all" in

  (* first all the global selection in 'all' to always compute its result.
   * This creates a first all_1 slice : I am not sure that this should be done.
   * *)
  let select = select_retres kf_all in
  Slicing.Api.Request.add_selection_internal select;
  Slicing.Api.Request.apply_next_internal ();
  print_requests ();
  Slicing.Api.Request.apply_all_internal ();
  extract_and_print ();

  (* create main_1 and select A2 in it : this will create a new slice for all
   * that computes A and the result. *)
  let ff_main = Slicing.Api.Slice.create kf_main in
  let select = select_data "A2" kf_main in
  Slicing.Api.Request.add_slice_selection_internal ff_main select;
  Slicing.Api.Request.apply_all_internal ();
  extract_and_print ()

let () = Db.Main.extend main
