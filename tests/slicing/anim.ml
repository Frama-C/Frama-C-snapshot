(* 
* Small example to view graphically the building process of a slicing project.
* To try it, use the following commands :

  make tests/slicing/anim.byte; \
  tests/slicing/anim.byte -deps -lib-entry g -slicing-level 3 -slice-callers \
                          tests/slicing/select_return_bis.c
*)

(*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*)

let add_select_fun_calls project kf =
  let selections = Db.Slicing.Select.empty in
  let selections =
    !Db.Slicing.Select.select_func_calls_into selections ~spare:false kf
  in List.iter 
       (fun (s, kf) -> !Db.Slicing.Request.add_selection_internal project s) 
       selections

(*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*)

let proj_name = "slicing_project";;
let project = Db.Slicing.Project.mk_project proj_name;;

let n = 0;;

let title = "Before start";;
let n = LibAnim.print_proj proj_name title project n;;

let kf_send = Db.find_function_by_name "send";;
add_select_fun_calls project kf_send;;

let title = "Select 'send' calls";;
let n = LibAnim.print_proj proj_name title project n;;
let title = ("Apply : "^title);;
let n = LibAnim.build_all_graphs proj_name title project n;;

let kf_send_bis = Db.find_function_by_name "send_bis";;
add_select_fun_calls project kf_send_bis;;

let title = "Select 'send_bis' calls";;
let n = LibAnim.print_proj proj_name title project n;;
let title = ("Apply : "^title);;
let n = LibAnim.build_all_graphs proj_name title project n;;

LibAnim.print_help proj_name;;

(*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*)

