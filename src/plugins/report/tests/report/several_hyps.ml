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
    ~selection:(State_selection.Static.with_dependencies Property_status.self)
    ()

let main () =
  Ast.compute ();
  print_status ();
  let main, j, i, h, g =
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
  let hyps = [ g; h ] in
  let ensures =
    let kf = Globals.Functions.find_by_name "f" in
    let spec = Annotations.funspec kf in
    Property.ip_post_cond_of_spec kf Kglobal spec
  in
  let ensures2 =
    let kf = Globals.Functions.find_by_name "f2" in
    let spec = Annotations.funspec kf in
    Property.ip_post_cond_of_spec kf Kglobal spec
  in
  let reset f =
    clear ();
    f ()
  in
  let test msg ?(hyps=hyps) set_status_hyps = 
    Kernel.feedback msg;
    reset set_status_hyps;
    (* unknown *)
    set_status main hyps Property_status.Dont_know;
    print_status ();
    (* true *)
    set_status main hyps Property_status.True;
    print_status ();
    reset set_status_hyps;
    (* false *)
    set_status main [] Property_status.False_and_reachable;
    print_status ()
  in
  let nothing () = () in
  let valid ?(g=g) ?(i=i) () = 
    let _i = i in set_status g [] Property_status.True 
  in
  let valid_under_hyp ?(g=g) ?(i=i) () = 
    set_status g [ i ] Property_status.True in
  let unknown ?(g=g) ?(i=i) () = 
    let _i = i in set_status g [] Property_status.Dont_know 
  in
  let invalid ?(g=g) ?(i=i) () = 
    let _i = i in set_status g [] Property_status.False_and_reachable 
  in
  let invalid_under_hyp ?(g=g) ?(i=i) () = 
    set_status g [ ] Property_status.False_and_reachable 
  in
  let invalid_but_dead ?(g=g) ?(i=i) () =
    set_status i [] Property_status.False_and_reachable;
    set_status g [ ] Property_status.False_and_reachable 
  in
  let valid_but_dead ?(g=g) ?(i=i) () = 
    set_status i [] Property_status.False_and_reachable;
    set_status g [ ] Property_status.True 
  in
  let unknown_but_dead ?(g=g) ?(i=i) () = 
    set_status i [] Property_status.False_and_reachable;
    set_status g [ i ] Property_status.Dont_know 
  in
  let inconsistent ?(g=g) ?(i=i) () = 
    let _i = i in
    set_status g [ ] Property_status.True;
    set_status ~emitter:emitter2 g [] Property_status.False_and_reachable
  in
  (***************************************************************************)
  test "NEVER_TRIED + NEVER_TRIED" nothing;
  test "NEVER_TRIED + CONSIDERED_VALID" ~hyps:(g :: ensures) nothing;
  test "NEVER_TRIED + VALID" valid;
  test "NEVER_TRIED + VALID_UNDER_HYP" valid_under_hyp;
  test "NEVER_TRIED + UNKNOWN" unknown;
  test "NEVER_TRIED + INVALID" invalid;
  test "NEVER_TRIED + INVALID_UNDER_HYP" invalid_under_hyp;
  test "NEVER_TRIED + INVALID_BUT_DEAD" invalid_but_dead;
  test "NEVER_TRIED + VALID_BUT_DEAD" valid_but_dead;
  test "NEVER_TRIED + UNKNOWN_BUT_DEAD" unknown_but_dead;
  test "NEVER_TRIED + INCONSISTENT" inconsistent;
  (***************************************************************************)
  test "CONSIDERED_VALID + CONSIDERED_VALID" ~hyps:(ensures @ ensures2) nothing;
  let hyps = g :: ensures in
  test "CONSIDERED_VALID + VALID" ~hyps valid;
  test "CONSIDERED_VALID + VALID_UNDER_HYP" ~hyps valid_under_hyp;
  test "CONSIDERED_VALID + UNKNOWN" ~hyps unknown;
  test "CONSIDERED_VALID + INVALID" ~hyps invalid;
  test "CONSIDERED_VALID + INVALID_UNDER_HYP" ~hyps invalid_under_hyp;
  test "CONSIDERED_VALID + INVALID_BUT_DEAD" ~hyps invalid_but_dead;
  test "CONSIDERED_VALID + VALID_BUT_DEAD" ~hyps valid_but_dead;
  test "CONSIDERED_VALID + UNKNOWN_BUT_DEAD" ~hyps unknown_but_dead;
  test "CONSIDERED_VALID + INCONSISTENT" ~hyps inconsistent;
  (***************************************************************************)
  let set status_g status_h () =
(*      (status_h: ?g:Property.t -> ?i:Property.t -> unit -> unit) () =*)
    status_g ();
(*    status_h ~g:h ~i:j ()*)
    status_h ?g:(Some h) ?i:(Some j) ()
  in
  test "VALID + VALID" (set valid valid);
  test "VALID + VALID_UNDER_HYP"  (set valid valid_under_hyp);
  test "VALID + UNKNOWN"  (set valid unknown);
  test "VALID + INVALID"  (set valid invalid);
  test "VALID + INVALID_UNDER_HYP" (set valid invalid_under_hyp);
  test "VALID + INVALID_BUT_DEAD" (set valid invalid_but_dead);
  test "VALID + VALID_BUT_DEAD" (set valid valid_but_dead);
  test "VALID + UNKNOWN_BUT_DEAD" (set valid unknown_but_dead);
  test "VALID + INCONSISTENT" (set valid inconsistent);
  (***************************************************************************)
  test "VALID_UNDER_HYP + VALID_UNDER_HYP" 
    (set valid_under_hyp valid_under_hyp);
  test "VALID_UNDER_HYP + UNKNOWN" (set valid_under_hyp unknown);
  test "VALID_UNDER_HYP + INVALID"  (set valid_under_hyp invalid);
  test "VALID_UNDER_HYP + INVALID_UNDER_HYP" 
    (set valid_under_hyp invalid_under_hyp);
  test "VALID_UNDER_HYP + INVALID_BUT_DEAD" 
    (set valid_under_hyp invalid_but_dead);
  test "VALID_UNDER_HYP + VALID_BUT_DEAD" (set valid_under_hyp valid_but_dead);
  test "VALID_UNDER_HYP + UNKNOWN_BUT_DEAD" 
    (set valid_under_hyp unknown_but_dead);
  test "VALID_UNDER_HYP + INCONSISTENT" (set valid_under_hyp inconsistent);
  (***************************************************************************)
  test "UNKNOWN + UNKNOWN" (set unknown unknown);
  test "UNKNOWN + INVALID"  (set unknown invalid);
  test "UNKNOWN + INVALID_UNDER_HYP" (set unknown invalid_under_hyp);
  test "UNKNOWN + INVALID_BUT_DEAD" (set unknown invalid_but_dead);
  test "UNKNOWN + VALID_BUT_DEAD" (set unknown valid_but_dead);
  test "UNKNOWN + UNKNOWN_BUT_DEAD" (set unknown unknown_but_dead);
  test "UNKNOWN + INCONSISTENT" (set unknown inconsistent);
  (***************************************************************************)
  test "INVALID + INVALID"  (set invalid invalid);
  test "INVALID + INVALID_UNDER_HYP" (set invalid invalid_under_hyp);
  test "INVALID + INVALID_BUT_DEAD" (set invalid invalid_but_dead);
  test "INVALID + VALID_BUT_DEAD" (set invalid valid_but_dead);
  test "INVALID + UNKNOWN_BUT_DEAD" (set invalid unknown_but_dead);
  test "INVALID + INCONSISTENT" (set invalid inconsistent);
  (***************************************************************************)
  test "INVALID_UNDER_HYP + INVALID_UNDER_HYP" 
    (set invalid_under_hyp invalid_under_hyp);
  test "INVALID_UNDER_HYP + INVALID_BUT_DEAD" 
    (set invalid_under_hyp invalid_but_dead);
  test "INVALID_UNDER_HYP + VALID_BUT_DEAD" 
    (set invalid_under_hyp valid_but_dead);
  test "INVALID_UNDER_HYP + UNKNOWN_BUT_DEAD"
    (set invalid_under_hyp unknown_but_dead);
  test "INVALID_UNDER_HYP + INCONSISTENT" 
    (set invalid_under_hyp inconsistent);
  (***************************************************************************)
  test "INVALID_BUT_DEAD + INVALID_BUT_DEAD" 
    (set invalid_but_dead invalid_but_dead);
  test "INVALID_BUT_DEAD + VALID_BUT_DEAD" 
    (set invalid_but_dead valid_but_dead);
  test "INVALID_BUT_DEAD + UNKNOWN_BUT_DEAD"
    (set invalid_but_dead unknown_but_dead);
  test "INVALID_BUT_DEAD + INCONSISTENT" 
    (set invalid_but_dead inconsistent);
  (***************************************************************************)
  test "VALID_BUT_DEAD + VALID_BUT_DEAD" (set valid_but_dead valid_but_dead);
  test "VALID_BUT_DEAD + UNKNOWN_BUT_DEAD"
    (set valid_but_dead unknown_but_dead);
  test "VALID_BUT_DEAD + INCONSISTENT" (set valid_but_dead inconsistent);
  (***************************************************************************)
  test "UNKNOWN_BUT_DEAD + UNKNOWN_BUT_DEAD"
    (set unknown_but_dead unknown_but_dead);
  test "UNKNOWN_BUT_DEAD + INCONSISTENT" (set unknown_but_dead inconsistent);
  (***************************************************************************)
  test "INCONSISTENT + INCONSISTENT" (set inconsistent inconsistent)

let () = Db.Main.extend main

