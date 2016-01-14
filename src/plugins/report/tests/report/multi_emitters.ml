open Cil_types

let emitter1 = 
  Emitter.create "Test1" [ Emitter.Property_status ] ~correctness:[] ~tuning:[]

let emitter2 = 
  Emitter.create "Test2" [ Emitter.Property_status ] ~correctness:[] ~tuning:[]

let set_status e s =
  Kernel.feedback "%a SET STATUS TO %a" 
    Emitter.pretty e Property_status.Emitted_status.pretty s;
  Annotations.iter_all_code_annot
    (fun stmt _ ca ->
      let kf = Kernel_function.find_englobing_kf stmt in
      let ps = Property.ip_of_code_annot kf stmt ca in
      List.iter (fun p -> Property_status.emit e p ~hyps:[] s) ps)

let print_status =
  Dynamic.get
    ~plugin:"Report"
    "print" 
    (Datatype.func Datatype.unit Datatype.unit)

let clear () =
  Kernel.feedback "CLEARING";
  Project.clear
    ~selection:(State_selection.Static.with_dependencies Property_status.self)
    ()

let main () =
  Ast.compute ();
  print_status ();
  set_status emitter1 Property_status.Dont_know;
  set_status emitter2 Property_status.Dont_know;
  (* unknow /\ unknown *)
  print_status (); 
  (* unknow /\ true *)
  set_status emitter1 Property_status.True;
  print_status ();
  (* true /\ true *)
  set_status emitter2 Property_status.True;
  print_status ();
  clear ();
  (* true /\ false_if_reachable *)
  set_status emitter1 Property_status.Dont_know;
  set_status emitter2 Property_status.False_if_reachable;
  print_status ();
  (* true /\ false *)
  set_status emitter2 Property_status.False_and_reachable;
  print_status ();
  clear ();
  (* false_if_reachable /\ false_if_reachable *)
  set_status emitter1 Property_status.False_if_reachable;
  set_status emitter2 Property_status.False_if_reachable;
  print_status ();
  (* false_if_reachable /\ false *)
  set_status emitter1 Property_status.False_if_reachable;
  set_status emitter2 Property_status.False_and_reachable;
  print_status ();
  (* false /\ false *)
  set_status emitter1 Property_status.False_and_reachable;
  print_status ()

let () = Db.Main.extend main
