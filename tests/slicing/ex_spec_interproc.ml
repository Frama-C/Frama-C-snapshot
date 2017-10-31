(* ledit bin/toplevel.top -deps tests/slicing/ex_spec_interproc.c
  #use "tests/slicing/select.ml";;
ou
  #use "tests/slicing/ex_spec_interproc.ml";;

*)

include LibSelect;;

let main _ =

  (*--------------------------*)
  (* find the kernel functions *)
  let _kf_g = Globals.Functions.find_def_by_name "g" in
  let kf_f = Globals.Functions.find_def_by_name "f" in
  let kf_main = Globals.Functions.find_def_by_name "main" in

  (* add a request to select f result (output 0) in the project *)
  let select_f_out0 () =
    let ff_f = Slicing.Api.Slice.create kf_f in
    let select = select_retres kf_f in
    Slicing.Api.Request.add_slice_selection_internal ff_f select;
    print_requests ();
    ff_f
  in


  (*=========================================================================*)
  (* DEBUT DU TEST *)
  (*=========================================================================*)
  (* mode DontSliceCalls *)
  Slicing.Api.set_modes ~calls:0 () ;

  Slicing.Api.Project.reset_slicing ();
  let _ff_f = select_f_out0 () in
  Slicing.Api.Request.apply_all_internal (); print_project ();
  extract_and_print ();

  (*=========================================================================*)
  (* mode PropagateMarksOnly *)
  Slicing.Api.set_modes ~calls:1 () ;

  Slicing.Api.Project.reset_slicing ();
  let _ff_f = select_f_out0 () in
  Slicing.Api.Request.apply_all_internal (); print_project ();
  extract_and_print ();

  (*=========================================================================*)
  (* mode MinimizeNbCalls *)
  Slicing.Api.set_modes ~calls:2 () ;

  Slicing.Api.Project.reset_slicing ();

  (* slice 'f' to compute its result (output 0) and propagate to 'g' *)
  let ff_f = select_f_out0 () in
  Slicing.Api.Request.apply_all_internal (); print_project ();

  (* call 'f' slice in 'main' *)
  let ff_main = Slicing.Api.Slice.create kf_main in
  Slicing.Api.Request.add_call_slice ~caller:ff_main ~to_call:ff_f;
  Slicing.Api.Request.apply_all_internal ();
  print_project ();

  extract_and_print ();

  (*---------------------------------------------- *)
  (* test remove_slice and select_stmt_computation *)

  (* we remove ff_main : ff_f should not be called anymore *)
  Slicing.Api.Slice.remove ff_main;
  print_project ();

  (* try to change ff_f to check that ff_main is not in its called_by anymore *)
  (* select "a" before inst 14 (d++) *)
  (* VP: initial value of 34 does not refer to d++ (was 30) 9 corresponds
     to d++. old ki 34 corresponds to return(X), new ki 13 *)
  print_stmt kf_f;
  let ki = get_stmt 10(*34*) in (* d++ *)
  let select = select_data_before_stmt "a" ki kf_f in
  Slicing.Api.Request.add_slice_selection_internal ff_f select;
  print_requests ();
  Slicing.Api.Request.apply_all_internal (); print_project ();

  (*=========================================================================*)
  (* Test 'extract' when there are 2 slices for the same function *)
  Slicing.Api.set_modes ~calls:2 () ;
  Slicing.Api.Project.reset_slicing ();

  let ff_f_1 = Slicing.Api.Slice.create kf_f in
  let select = select_retres kf_f in
  Slicing.Api.Request.add_slice_selection_internal ff_f_1 select;

  let ff_f_2 = Slicing.Api.Slice.create kf_f in
  let select = select_data "Z" kf_f in
  Slicing.Api.Request.add_slice_selection_internal ff_f_2 select;

  Slicing.Api.Request.apply_all_internal ();
  print_ff ff_f_2;

  extract_and_print ();
  (*=========================================================================*)
  (* mode PreciseSlices *)
  Slicing.Api.set_modes ~calls:3 () ;

  test_select_retres ~do_prop_to_callers:true "f";

  print_project ();;
(*=========================================================================*)

let () = Db.Main.extend main
