open Cil_types

let emitter = 
  Emitter.create
    "Test" 
    [ Emitter.Property_status ] 
    ~correctness:[ Kernel.LibEntry.parameter ] 
    ~tuning:[ Kernel.SafeArrays.parameter ]

let set_status s =
  Annotations.iter_all_code_annot
    (fun stmt _ ca ->
      let kf = Kernel_function.find_englobing_kf stmt in
      let ps = Property.ip_of_code_annot kf stmt ca in
      List.iter (fun p -> Property_status.emit emitter p ~hyps:[] s) ps)

let print_status =
  Dynamic.get
    ~plugin:"Report"
    "print" 
    (Datatype.func Datatype.unit Datatype.unit)

let main () =
  Ast.compute ();
  Kernel.feedback "SETTING STATUS TO unknown IN p";
  set_status Property_status.Dont_know;
  print_status ();
  let p' = Project.create "foobar" in
  Kernel.feedback "CHANGING DEFAULT PROJECT TO p'";
  Project.on p' (fun () -> ()) ();
  Project.remove ~project:p' ();
  Kernel.feedback "GOING BACK TO PROJECT p";
  print_status ();
  let p = Project.current () in
  Kernel.feedback "CREATING p2 by COPYING p";
  let p2 = File.create_project_from_visitor "p2" (new Visitor.frama_c_copy) in
  print_status ();
  Kernel.feedback "CHANGING DEFAULT PROJECT TO p2";
  Project.set_current p2;
  print_status ();
  Kernel.feedback "SETTING STATUS TO false_and_reachable";
  set_status Property_status.False_and_reachable;
  print_status ();
  Kernel.feedback "CHANGING DEFAULT PROJECT TO p";
  Project.set_current p;  
  print_status ();
  Kernel.feedback "SETTING A CORRECTNESS PARAMETER";
  Kernel.LibEntry.on ();
  print_status ();
  Kernel.feedback "SETTING STATUS TO unknown IN p";
  set_status Property_status.Dont_know;
  print_status ();
  Kernel.feedback "SETTING A TUNING PARAMETER";
  Kernel.SafeArrays.off ();
  print_status ();
  Kernel.feedback "SETTING STATUS TO unknown IN p";
  set_status Property_status.Dont_know;
  print_status ()

let () = Db.Main.extend main
