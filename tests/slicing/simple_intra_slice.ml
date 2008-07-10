(* ledit bin/toplevel.top -deps tests/slicing/simple_intra_slice.c \
  < tests/slicing/simple_intra_slice.ml
*)


include LibSelect;;

let project = mk_project ();;

let pretty_pdg fmt kf = !Db.Pdg.pretty fmt (!Db.Pdg.get kf);;
let apply_all_actions = !S.Request.apply_all_internal
let print_slice = !S.Slice.pretty
let print_fct_stmts kf = Slicing.Register.print_fct_stmts fmt (project, kf)


let get_fct name =
  let kf = Globals.Functions.find_def_by_name name in
    kf

let select_stmt_and_print kf num_stmt =
  let stmt = get_stmt num_stmt in
  let mark = !S.Mark.make ~data:true ~addr:true ~ctrl:true in
  let select = !S.Select.select_stmt_internal kf stmt mark in
  let ff = !S.Slice.create project kf in
  let _ = !S.Request.add_slice_selection_internal project ff select in
  !S.Request.pretty fmt project;
  apply_all_actions  project;
  print_slice fmt ff
;;

let select_and_print kf select =
  let ff = !S.Slice.create project kf in
  let _ = !S.Request.add_slice_selection_internal project ff select in
  !S.Request.pretty fmt project;
  apply_all_actions  project;
  print_slice fmt ff
;;
let select_out_data_and_print kf data =
  let select = select_data data project kf in
  select_and_print kf select
;;

let select_out0_and_print kf =
  let select = select_retres project kf in
  select_and_print kf select
;;
let select_ctrl_and_print kf numstmt =
  let select = select_ctrl numstmt project  kf in
  select_and_print kf select
;;


let print_outputs fct_name =
  let fct = Globals.Functions.find_by_name fct_name in
  let outs = !Db.Outputs.get_external fct in
  Format.printf "Sorties de la fonction %s = %a\n"
    fct_name Locations.Zone.pretty outs
;;

let kf = get_fct "f1" ;;
Format.printf "@[%a@]@\n" pretty_pdg kf;;
print_fct_stmts kf;;
select_stmt_and_print kf 3;; (* G=x+a; *)

let kf = get_fct "f2" ;;
Format.printf "@[%a@]@\n" pretty_pdg kf;;
print_fct_stmts kf;;
select_stmt_and_print kf 7;; (* c=3; *)

let kf = get_fct "f3" ;;
Format.printf "@[%a@]@\n" pretty_pdg kf;;
print_fct_stmts kf;;
select_out0_and_print kf;;

let kf = get_fct "f4" ;;
Format.printf "@[%a@]@\n" pretty_pdg kf;;
print_fct_stmts kf;;
select_out0_and_print kf;;
select_stmt_and_print kf 21;; (* G=a; in then branch of if (c>Unknown) *)

let kf = get_fct "f5" ;;
print_outputs "f5";;

select_out_data_and_print kf "G";;

Format.printf "@[%a@]@\n" pretty_pdg kf;;
print_fct_stmts kf;;

select_out0_and_print kf;;

select_ctrl_and_print kf 29;;
(* G++. VP 2008-02-04: Was ki 113, and corresponded to
   if(c<Unknown) { goto L2; }, not to G++
   Fixed ki number to the test instead of the incrementation.
   As of this date, ki for G++ is 31.
   VP 2008-06-25 ki for G++ is 32
 *)

let kf = get_fct "f6" ;;
Format.printf "@[%a@]@\n" pretty_pdg kf;;
print_fct_stmts kf;;

select_ctrl_and_print kf 49;;
(* return_label
VP 2008-02-04: Was ki 135, corresponding to first stmt in the else
branch of if (i) { __retres = 0; goto return_label; }
          else { /* here*/__retres = 10*n; goto return_label; }
Fixed ki number for this particular ki.
As of this date, ki for return_label is 92
VP 2008-06-25: ki for return_label is 96
*)

!S.Project.pretty Format.std_formatter  project;;
