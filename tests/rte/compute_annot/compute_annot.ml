let print () =
  File.pretty_ast ();
  Kernel.log "================================"

let print_status () =
  Kernel.log "printing status";
  let  _, _, get_signedOv_status = !Db.RteGen.get_signedOv_status () in
  let  _, _, get_precond_status = !Db.RteGen.get_precond_status () in
  Globals.Functions.iter
    (fun kf ->
      Kernel.log "kf = %s rte_gen_status = %b precond_status = %b\n"
	(Kernel_function.get_name kf)
	(get_signedOv_status kf)
	(get_precond_status kf))

let main () =
  Dynamic.Parameter.Bool.set "-rte-all" false;
  Dynamic.Parameter.Bool.set "-rte-precond" true;
  print ();
  print_status ();

  Kernel.log "computing -rte-precond annotations" ;
  !Db.RteGen.compute () ;

  print ();
  print_status ();

  Kernel.log "computing rte-div annotations" ;
  Dynamic.Parameter.Bool.set "-rte-div" true ;
  !Db.RteGen.compute () ;
  print ();
  print_status ();

  Kernel.log "removing rte-div alarms" ;
  let emitter = Dynamic.get ~plugin:"RteGen" "emitter" Emitter.ty in
  let filter = function
    | Alarms.Division_by_zero _ -> true
    | _ -> false
  in
  Alarms.remove ~filter emitter;
  !Db.RteGen.compute () ;
  print ();
  print_status ()

let () = Db.Main.extend main
