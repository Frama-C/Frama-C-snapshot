(* 
* Small example to test function merge_slices.
* To try it, use the following commands :

  make tests/slicing/merge.byte; \
  tests/slicing/merge.byte -deps -lib-entry g -slicing-level 3 \
                          tests/slicing/merge.c
*)

(*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*)

include LibSelect;;

let main _ =

  let proj_name = "slicing_merge" in
  let project = mk_project () in

  let kf_init = Globals.Functions.find_def_by_name "init" in
  let _kf_add = Globals.Functions.find_def_by_name "add" in
  let kf_g = Globals.Functions.find_def_by_name "g" in

  let n = 0 in

  (* build graphs representation if there is something in [anim_title] *)
  let build_slice kf data n anim_title apply =
    let ff = !S.Slice.create project kf in
    let select = select_data data project kf in
    let _ = !S.Request.add_slice_selection_internal project ff select in
    let n = 
      if anim_title = ""
      then (if apply then !S.Request.apply_all_internal project; n)
      else LibAnim.build_all_graphs proj_name anim_title project n 
    in n, ff
  in

  ignore (LibAnim.print_proj proj_name "Beginning" project n);
  let n = n+1 in

  let title = "Select G1 in init" in
  let n, ff_init1 = build_slice kf_init "G1" n title true in
  let title = "Select G1 in g" in
  let n, _ff_g1 = build_slice kf_g "G1" n title true in

  (*
    Format.printf "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n";;
    Format.printf "=== Function g_1 computes G1 and should call init_1 :\n";
    !S.Project.export None project;;
    Format.printf "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n";;
  *)

  let n, ff_init2 = build_slice kf_init "G2" n "" true in
  let n, _ff_init3 = build_slice kf_init "G3" n "" true in

  let n, _ff_g2 = build_slice kf_g "G2" n "" false in
  let n, ff_g3 = build_slice kf_g "G3" n "" true in

  (*
    Format.printf "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n";;
    Format.printf "=== g_2 sould call init_2 and g_3, init_3 :\n";
    !S.Project.export None project;;
    Format.printf "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n";;
  *)

  ignore (LibAnim.print_proj proj_name "After selections" project n);
  let n = n+1 in

  ignore (!S.Request.merge_slices project  ff_init1 ff_init2 ~replace:true);
  let title = "merging init_1 and init_2" in
  ignore (LibAnim.print_proj proj_name title project n);
  let n = n+1 in

  let title = "merging init_1 and init_2" in
  let n = LibAnim.build_all_graphs proj_name title project n in

  !S.Slice.remove project ff_init1;
  !S.Slice.remove project ff_init2;

  let title = "After removing init_1 and init_2" in
  ignore (LibAnim.print_proj proj_name title project n);
  let _n = n+1 in

  let _ = !S.Request.copy_slice project ff_g3 in

  extract_and_print project;

  (* in automatic tests, we remove the generated files.
   * Change [view_graphs] below to be able to display the graphs *)
  let view_graphs = false in
  if view_graphs then
    LibAnim.print_help proj_name
  else
    LibAnim.remove_all_files proj_name
;;

(*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*)

let () = Db.Main.extend main
