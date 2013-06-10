(* when using toplevel.top :
bin/topleval.top -val tests/scope/zones.c
#directory "cil/src";;
*)

let fmt =  Format.std_formatter;;

(*
let old_debug = Kernel.Debug.get ();;
Kernel.Debug.set 1;; (* to see sid *)
Format.fprintf fmt "@[%a@]" Printer.pp_file ( Ast.get ());;
Kernel.Debug.set old_debug;;
*)

let find_ret kf_name =
  let kf = Globals.Functions.find_by_name kf_name in
  let stmt = Kernel_function.find_return kf in
    Format.printf "Current program point = return in function %s@\n" kf_name;
    stmt, kf
;;

let find_sid sid =
  let stmt, kf = Kernel_function.find_from_sid sid in
    Format.printf "Current program point = before stmt %d in function %a@\n"
      sid Kernel_function.pretty kf;
    stmt, kf
;;

let find_label kf_name lab_name =
  let kf = Globals.Functions.find_by_name kf_name in
  let stmt = !(Kernel_function.find_label kf lab_name) in
    Format.printf "Current program point = label %s in function %s@\n"
      lab_name kf_name;
    stmt, kf

let compute_and_print pp str_data =
  let stmt, kf = pp in
  let lval_term = !Db.Properties.Interp.lval kf stmt str_data in
  let lval = !Db.Properties.Interp.term_lval_to_lval ~result:None lval_term in
  let (_used_stmts, zones) = !Db.Scope.build_zones kf stmt lval in
    Format.printf "Zones for %s at current program point =@.%a\n@\n"
      str_data !Db.Scope.pretty_zones zones
;;

let main _ =
  let pp = find_ret "simple" in
  compute_and_print pp "x";

  let pp = find_ret "array1" in
  compute_and_print pp "T[0]";
  compute_and_print pp "T[1]";
  compute_and_print pp "T[x]";

  let pp = find_ret "struct1" in
  compute_and_print pp "s.a";
  compute_and_print pp "s.b";
  compute_and_print pp "s";

  let pp = find_ret "ctrl1" in
  compute_and_print pp "a";
  let pp = find_label "ctrl1" "Lt2" in
  compute_and_print pp "a";

  let pp = find_ret "caller" in
  compute_and_print pp "Yf"

let () = Db.Main.extend main
