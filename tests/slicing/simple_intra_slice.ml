(* ledit bin/toplevel.top -deps tests/slicing/simple_intra_slice.c \
  < tests/slicing/simple_intra_slice.ml
*)


include LibSelect;;

let main _ =
  Slicing.Api.Project.reset_slicing ();
  let pretty_pdg fmt kf = !Db.Pdg.pretty fmt (!Db.Pdg.get kf) in
  let apply_all_actions = Slicing.Api.Request.apply_all_internal in
  let print_slice = Slicing.Api.Slice.pretty in
  let print_fct_stmts kf =
    Slicing.PrintSlice.print_fct_stmts fmt kf in
  let get_fct name =
    let kf = Globals.Functions.find_def_by_name name in
    kf
  in
  let select_stmt_and_print kf num_stmt =
    let stmt = get_stmt num_stmt in
    let mark = Slicing.Api.Mark.make ~data:true ~addr:true ~ctrl:true in
    let select = Slicing.Api.Select.select_stmt_internal kf stmt mark in
    let ff = Slicing.Api.Slice.create kf in
    Slicing.Api.Request.add_slice_selection_internal ff select;
    Slicing.Api.Request.pretty fmt;
    apply_all_actions ();
    print_slice fmt ff
  in
  let select_and_print kf select =
    let ff = Slicing.Api.Slice.create kf in
    Slicing.Api.Request.add_slice_selection_internal ff select;
    Slicing.Api.Request.pretty fmt ;
    apply_all_actions ();
    print_slice fmt ff
  in
  let select_out_data_and_print kf data =
    let select = select_data data kf in
    select_and_print kf select
  in
  let select_out0_and_print kf =
    let select = select_retres kf in
    select_and_print kf select
  in
  let select_ctrl_and_print kf numstmt =
    let select = select_ctrl numstmt kf in
    select_and_print kf select
  in
  let print_outputs fct_name =
    let fct = Globals.Functions.find_by_name fct_name in
    let outs = !Db.Outputs.get_external fct in
    Format.printf "Sorties de la fonction %s = %a\n"
      fct_name Locations.Zone.pretty outs
  in
  let kf = get_fct "f1"  in
  Format.printf "@[%a@]@\n" pretty_pdg kf;
  print_fct_stmts kf;
  select_stmt_and_print kf 3; (* G=x+a; *)

  let kf = get_fct "f2" in
  Format.printf "@[%a@]@\n" pretty_pdg kf;
  print_fct_stmts kf;
  select_stmt_and_print kf 9; (* c=3; *)

  let kf = get_fct "f3" in
  Format.printf "@[%a@]@\n" pretty_pdg kf;
  print_fct_stmts kf;
  select_out0_and_print kf;

  let kf = get_fct "f4" in
  Format.printf "@[%a@]@\n" pretty_pdg kf;
  print_fct_stmts kf;
  select_out0_and_print kf;
  select_stmt_and_print kf 28; (* G=a; in then branch of if (c>Unknown) *)

  let kf = get_fct "f5" in
  print_outputs "f5";
  select_out_data_and_print kf "G";
  Format.printf "@[%a@]@\n" pretty_pdg kf;
  print_fct_stmts kf;
  select_out0_and_print kf;
  select_ctrl_and_print kf 40;
(* G++. VP 2008-02-04: Was ki 113, and corresponded to
   if(c<Unknown) { goto L2; }, not to G++
   Fixed ki number to the test instead of the incrementation.
   As of this date, ki for G++ is 31.
   VP 2008-06-25 ki for G++ is 32
   VP 2008-07-17 ki for G++ is 37
   BY 2011-04-14 sid for G++ is 38
   VP 2012-04-09 sid for G++ is 44
   VP 2017-02-16 sid for G++ is 43
 *)

  let kf = get_fct "f6"  in
  Format.printf "@[%a@]@\n" pretty_pdg kf;
  print_fct_stmts kf;
  select_ctrl_and_print kf 68;
(* return_label
VP 2008-02-04: Was ki 135, corresponding to first stmt in the else
branch of if (i) { __retres = 0; goto return_label; }
          else { /* here*/__retres = 10*n; goto return_label; }
Fixed ki number for this particular ki.
As of this date, ki for return_label is 92
VP 2008-06-25: ki for return_label is 96
VP 2008-07-17: ki for return_label is 112
BY 2011-04-14 sid for return_label is 128
VP 2012-04-09: sid for return_label is 134
*)

  Slicing.Api.Project.pretty Format.std_formatter

let () = Db.Main.extend main
