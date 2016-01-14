open Cil_types

let emitter = 
  Emitter.create "Test" [ Emitter.Property_status ] ~correctness:[] ~tuning:[]

let emitter2 = 
  Emitter.create "Test2" [ Emitter.Property_status ] ~correctness:[] ~tuning:[]

let set_status ?(emitter=emitter) p hyps s =
  Kernel.feedback "SETTING STATUS OF %a TO %a" 
    Property.pretty p
    Property_status.Emitted_status.pretty s;
  Property_status.emit emitter p ~hyps s

let print_status =
  Dynamic.get
    ~plugin:"Report"
    "print" 
    (Datatype.func Datatype.unit Datatype.unit)

let clear () =
  Kernel.feedback "CLEARING";
  Project.clear
    ~selection:(State_selection.with_dependencies Property_status.self)
    ()

let main () =
  Ast.compute ();
  print_status ();
  let main, _, _, h, g =
    let l =
      Annotations.fold_all_code_annot
	(fun stmt _ ca acc ->
	  let kf = Kernel_function.find_englobing_kf stmt in
	  let ps = Property.ip_of_code_annot kf stmt ca in
	  match ps with
	  | [ p ] -> p :: acc
	  | _ -> assert false)
	[]
    in
    match l with
    | [ p1; p2; p3; p4; p5 ] -> p1, p2, p3, p4, p5
    | _ -> assert false
  in
  let ensures =
    let kf = Globals.Functions.find_by_name "f" in
    let spec = Annotations.funspec kf in
    Property.ip_post_cond_of_spec kf Kglobal spec
  in
  (* *********************************************************************** *)
  (* hyp = never_tried *)
  (* unknown *)
  set_status h [ main ] Property_status.Dont_know;
  print_status ();
  (* true *)
  set_status h [ main ] Property_status.True;
  print_status ();
  clear ();
  (* false *)
  set_status h [ ] Property_status.False_and_reachable;
  print_status ();
  (* *********************************************************************** *)
  (* hyp = considered_valid *)
  clear ();
  (* unknown *)
  set_status h ensures Property_status.Dont_know;
  print_status ();
  (* true *)
  set_status h ensures Property_status.True;
  print_status ();
  clear ();
  (* false *)
  set_status h [] Property_status.False_and_reachable;
  print_status ();
  (* *********************************************************************** *)
  (* hyp = valid *)
  clear ();
  (* unknown *)
  set_status main [] Property_status.True;
  set_status h [ main ] Property_status.Dont_know;
  print_status ();
  (* true *)
  set_status h [ main ] Property_status.True;
  print_status ();
  clear ();
  (* false *)
  set_status main [] Property_status.True;
  set_status h [ ] Property_status.False_and_reachable;
  print_status ();
  (* *********************************************************************** *)
  (* hyp = valid under hyp *)
  clear ();
  (* unknown *)
  set_status g [] Property_status.Dont_know;
  set_status main [ g ] Property_status.True;
  set_status h [ main ] Property_status.Dont_know;
  print_status ();
  (* true *)
  set_status h [ main ] Property_status.True;
  print_status ();
  clear ();
  (* false *)
  set_status g [] Property_status.Dont_know;
  set_status main [ g ] Property_status.True;
  set_status h [ ] Property_status.False_and_reachable;
  print_status ();
  (* *********************************************************************** *)
  (* hyp = dont_know *)
  clear ();
  (* unknown *)
  set_status main [] Property_status.Dont_know;
  set_status h [ main ] Property_status.Dont_know;
  print_status ();
  (* true *)
  set_status h [ main ] Property_status.True;
  print_status ();
  clear ();
  (* false *)
  set_status main [] Property_status.Dont_know;
  set_status h [ ] Property_status.False_and_reachable;
  print_status ();
  (* *********************************************************************** *)
  (* hyp = invalid *)
  clear ();
  (* unknown *)
  set_status main [] Property_status.False_and_reachable;
  set_status h [ main ] Property_status.Dont_know;
  print_status ();
  (* true *)
  set_status h [ main ] Property_status.True;
  print_status ();
  clear ();
  (* false *)
  set_status main [] Property_status.False_and_reachable;
  set_status h [ ] Property_status.False_and_reachable;
  print_status ();
  (* *********************************************************************** *)
  (* hyp = invalid under hyp *)
  clear ();
  (* unknown *)
  set_status g [] Property_status.Dont_know;
  set_status main [ ] Property_status.False_and_reachable;
  set_status h [ main ] Property_status.Dont_know;
  print_status ();
  (* true *)
  set_status h [ main ] Property_status.True;
  print_status ();
  clear ();
  (* false *)
  set_status g [] Property_status.Dont_know;
  set_status main [ ] Property_status.False_and_reachable;
  set_status h [ ] Property_status.False_and_reachable;
  print_status ();
  (* *********************************************************************** *)
  (* hyp = invalid but dead *)
  clear ();
  (* unknown *)
  set_status g [] Property_status.False_and_reachable;
  set_status main [ ] Property_status.False_and_reachable;
  set_status h [ main ] Property_status.Dont_know;
  print_status ();
  (* true *)
  set_status h [ main ] Property_status.True;
  print_status ();
  clear ();
  (* false *)
  set_status g [] Property_status.False_and_reachable;
  set_status main [ ] Property_status.False_and_reachable;
  set_status h [ ] Property_status.False_and_reachable;
  print_status ();
  (* *********************************************************************** *)
  (* hyp = valid but dead *)
  clear ();
  (* unknown *)
  set_status g [] Property_status.False_and_reachable;
  set_status main [ g ] Property_status.True;
  set_status h [ main ] Property_status.Dont_know;
  print_status ();
  (* true *)
  set_status h [ main ] Property_status.True;
  print_status ();
  clear ();
  (* false *)
  set_status g [] Property_status.False_and_reachable;
  set_status main [ g ] Property_status.True;
  set_status h [ ] Property_status.False_and_reachable;
  print_status ();
  (* *********************************************************************** *)
  (* hyp = unknown but dead *)
  clear ();
  (* unknown *)
  set_status g [] Property_status.False_and_reachable;
  set_status main [ g ] Property_status.Dont_know;
  set_status h [ main ] Property_status.Dont_know;
  print_status ();
  (* true *)
  set_status h [ main ] Property_status.True;
  print_status ();
  clear ();
  (* false *)
  set_status g [] Property_status.False_and_reachable;
  set_status main [ g ] Property_status.Dont_know;
  set_status h [ ] Property_status.False_and_reachable;
  print_status ();
  (* *********************************************************************** *)
  (* hyp = inconsistent *)
  clear ();
  (* unknown *)
  set_status main [] Property_status.True;
  set_status ~emitter:emitter2 main [] Property_status.False_and_reachable;
  set_status h [ main ] Property_status.Dont_know;
  print_status ();
  (* true *)
  set_status h [ main ] Property_status.True;
  print_status ();
  clear ();
  (* false *)
  set_status main [] Property_status.True;
  set_status ~emitter:emitter2 main [] Property_status.False_and_reachable;
  set_status h [ ] Property_status.False_and_reachable;
  print_status ();
  (* *********************************************************************** *)
  ()

let () = Db.Main.extend main

