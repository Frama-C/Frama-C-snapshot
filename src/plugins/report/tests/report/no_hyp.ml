open Cil_types

let emitter = 
  Emitter.create "Test" [ Emitter.Property_status ] ~correctness:[] ~tuning:[]

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

let clear () =
  Project.clear
    ~selection:(State_selection.Static.with_dependencies Property_status.self)
    ()

let main () =
  Ast.compute ();
  Kernel.feedback "SETTING STATUS TO dont_know";
  set_status Property_status.Dont_know;
  print_status ();
  Kernel.feedback "SETTING STATUS TO true";
  set_status Property_status.True;
  print_status ();
  Kernel.feedback "SETTING STATUS TO false_if_reachable";
  (try set_status Property_status.False_if_reachable
   with Property_status.Inconsistent_emitted_status(s1, s2) ->
     Kernel.result "inconsistency between %a and %a" 
       Property_status.Emitted_status.pretty s1
       Property_status.Emitted_status.pretty s2);
  Kernel.feedback "CLEARING";
  clear ();
  Kernel.feedback "SETTING STATUS TO false_if_reachable";
  set_status Property_status.False_if_reachable;
  print_status ();
  Kernel.feedback "SETTING STATUS TO false_and_reachable";
  set_status Property_status.False_and_reachable;
  print_status ()

let () = Db.Main.extend main
