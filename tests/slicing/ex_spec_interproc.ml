(* ledit bin/toplevel.top -deps tests/slicing/ex_spec_interproc.c
  #use "tests/slicing/select.ml";;
ou
  #use "tests/slicing/ex_spec_interproc.ml";;

*)

include LibSelect;;

(*--------------------------*)
(* find the kernel functions *)
let kf_g = Globals.Functions.find_def_by_name "g";;
let kf_f = Globals.Functions.find_def_by_name "f";;
let kf_main = Globals.Functions.find_def_by_name "main";;

(* add a request to select f result (output 0) in the project *)
let select_f_out0 project =
  let ff_f = !S.Slice.create project kf_f in
  let select = select_retres project kf_f in
    !S.Request.add_slice_selection_internal project ff_f select;
    print_requests project;
    ff_f
;;


(*===========================================================================*)
(* DEBUT DU TEST *)
(*===========================================================================*)
(* mode DontSliceCalls *)
Cmdline.Slicing.Mode.Calls.set 0;;

let project = mk_project ();;
let _ff_f = select_f_out0 project;;
!S.Request.apply_all_internal project; print_project project;;
extract_and_print project;;

(*===========================================================================*)
(* mode PropagateMarksOnly *)
Cmdline.Slicing.Mode.Calls.set 1;;

let project = mk_project ();;
let _ff_f = select_f_out0 project;;
!S.Request.apply_all_internal project; print_project project;;
extract_and_print project;;

(*===========================================================================*)
(* mode MinimizeNbCalls *)
Cmdline.Slicing.Mode.Calls.set 2;;

let project = mk_project ();;

(* slice 'f' to compute its result (output 0) and propagate to 'g' *)
let ff_f = select_f_out0 project;;
!S.Request.apply_all_internal project; print_project project;;

(* call 'f' slice in 'main' *)
let ff_main = !S.Slice.create project kf_main;;
!S.Request.add_call_slice project ~caller:ff_main ~to_call:ff_f;;
!S.Request.apply_all_internal project; print_project project;

extract_and_print project;;

(*---------------------------------------------- *)
(* test remove_slice and select_stmt_computation *)

(* we remove ff_main : ff_f should not be called anymore *)
!S.Slice.remove project ff_main;;
print_project project;;

(* try to change ff_f to check that ff_main is not in its called_by anymore *)
(* select "a" before inst 14 (d++) *)
(* VP: initial value of 34 does not refer to d++ (was 30) 9 corresponds
   to d++. old ki 34 corresponds to return(X), new ki 13 *)
print_stmt project kf_f;;
let ki,_ = Kernel_function.find_from_sid 10(*34*) in (* d++ *)
let select = select_data_before_stmt "a" ki project kf_f in
  !S.Request.add_slice_selection_internal project ff_f select;;
print_requests project;;
!S.Request.apply_all_internal project; print_project project;;

(*===========================================================================*)
(* Test 'extract' when there are 2 slices for the same function *)
Cmdline.Slicing.Mode.Calls.set 2;;
let project = mk_project ();;

let ff_f_1 = !S.Slice.create project kf_f;;
let select = select_retres project kf_f in
  !S.Request.add_slice_selection_internal project ff_f_1 select;;

let ff_f_2 = !S.Slice.create project kf_f;;
let select = select_data "Z" project kf_f in
  !S.Request.add_slice_selection_internal project ff_f_2 select;;

!S.Request.apply_all_internal project;;
print_ff ff_f_2;;

extract_and_print project;;
(*===========================================================================*)
(* mode PreciseSlices *)
Cmdline.Slicing.Mode.Calls.set 3;;

let project = test_select_retres ~do_prop_to_callers:true "f" ;;

print_project project;;
(*===========================================================================*)
