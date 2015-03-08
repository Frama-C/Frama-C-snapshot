let print () =
  File.pretty_ast ();
  Kernel.log "================================"

let print_status () =
  Kernel.log "printing status";
  let _, _, get_signedOv_status = !Db.RteGen.get_signedOv_status () in
  let _, _, get_precond_status = !Db.RteGen.get_precond_status () in
  Globals.Functions.iter
    (fun kf ->
      Kernel.log "kf = %s rte_gen_status = %b precond_status = %b\n"
	(Kernel_function.get_name kf)
	(get_signedOv_status kf)
	(get_precond_status kf))

let main () =
  Dynamic.Parameter.Bool.set "-rte-all" true;
  Dynamic.Parameter.Bool.set "-rte-precond" true;
  Kernel.SignedOverflow.on ();

  if not(Ast.is_computed ()) then Ast.compute () ;
  print ();

  Globals.Functions.iter (fun kf -> !Db.RteGen.annotate_kf kf);
  print () ;
  print_status ();

  Kernel.log "Removing some rte annotations" ;
  let _, set_signed, _ = !Db.RteGen.get_signedOv_status () in
  let emitter = Dynamic.get ~plugin:"RteGen" "emitter" Emitter.ty in
  let filter = function
    | Alarms.Overflow _ -> true
    | _ -> false
  in
  Alarms.remove ~filter emitter;
  print ();
  print_status ();

(*  Dynamic.Parameter.Bool.set "-rte-all" true;*)
  let one_on_two = ref true in
  Globals.Functions.iter
    (fun kf ->
      if !one_on_two then begin
	set_signed kf false;
	!Db.RteGen.annotate_kf kf
      end;
      one_on_two := not !one_on_two);
  print ()  ;
  print_status ()

let () = Db.Main.extend main
