(*
make -s tests/pdg/dyn_dpds.byte ; \
tests/pdg/dyn_dpds.byte -deps  tests/pdg/dyn_dpds.c; \
zgrviewer tests/pdg/dyn_dpds_1.dot ; \
zgrviewer tests/pdg/dyn_dpds_2.dot ;
*)

let get_zones str_data (kinst, kf) =
  let lval_term = !Db.Properties.Interp.lval kf kinst str_data in
  let lval = !Db.Properties.Interp.term_lval_to_lval lval_term in
  let loc =
    !Db.From.find_deps_no_transitivity
      (Cil_types.Kstmt kinst)
      (Cil_types.Lval lval)
  in
  loc
;;

let memo_debug = Cmdline.Debug.get () in
Cmdline.Debug.set 1;
let files = Cil_state.file () in
Format.printf "@[%a@]" (Cil.d_file (new Printer.print ())) files;

Cmdline.Debug.set memo_debug ;;

let kf =  Globals.Functions.find_def_by_name "main";;
let pdg = !Db.Pdg.get kf;;

!Db.Pdg.pretty Format.std_formatter pdg;;
!Db.Pdg.extract pdg "tests/pdg/dyn_dpds_0.dot";;

let assert_sid = 4;; (* assert ( *p>G) *)
let assert_stmt, kf = Kernel_function.find_from_sid assert_sid;;


let assert_node =
  match !Db.Pdg.find_stmt_nodes pdg assert_stmt with
    | n::[] -> n | _ -> assert false;;

let star_p = get_zones "*p" (assert_stmt, kf);;
let data_nodes, undef =
  !Db.Pdg.find_location_nodes_at_stmt pdg assert_stmt ~before:true star_p;;

assert (Locations.Zone.equal undef Locations.Zone.bottom);;

let g_zone = get_zones "G" (assert_stmt, kf);;
let g_nodes, undef =
  !Db.Pdg.find_location_nodes_at_stmt pdg assert_stmt ~before:true g_zone;;

let data_nodes = g_nodes @ data_nodes;;

Format.printf "Warning : cannot select %a in this function...@\n"
  Locations.Zone.pretty undef;;

Format.printf "Add some dynamic dependencies\n";
!Db.Pdg.add_dynamic_dpds pdg ~data:data_nodes assert_node ;;

!Db.Pdg.pretty Format.std_formatter pdg;;
!Db.Pdg.extract pdg "tests/pdg/dyn_dpds_1.dot";;

Format.printf "Clear the dynamic dependencies\n";
!Db.Pdg.clear_dynamic_dpds pdg;;

!Db.Pdg.pretty Format.std_formatter pdg;;
!Db.Pdg.extract pdg "tests/pdg/dyn_dpds_2.dot";;
