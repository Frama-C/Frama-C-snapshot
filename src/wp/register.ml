(**************************************************************************)
(*                                                                        *)
(*  This file is part of WP plug-in of Frama-C.                           *)
(*                                                                        *)
(*  Copyright (C) 2007-2013                                               *)
(*    CEA (Commissariat a l'energie atomique et aux energies              *)
(*         alternatives)                                                  *)
(*                                                                        *)
(*  you can redistribute it and/or modify it under the terms of the GNU   *)
(*  Lesser General Public License as published by the Free Software       *)
(*  Foundation, version 2.1.                                              *)
(*                                                                        *)
(*  It is distributed in the hope that it will be useful,                 *)
(*  but WITHOUT ANY WARRANTY; without even the implied warranty of        *)
(*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the         *)
(*  GNU Lesser General Public License for more details.                   *)
(*                                                                        *)
(*  See the GNU Lesser General Public License version 2.1                 *)
(*  for more details (enclosed in the file licenses/LGPLv2.1).            *)
(*                                                                        *)
(**************************************************************************)

open Factory
let job_key= Wp_parameters.register_category "trace-job"

(* --------- Command Line ------------------- *)

let cmdline () : setup =
  begin
    match Wp_parameters.Model.get () with
      | ["Runtime"] ->
	  Wp_parameters.abort
	    "Model 'Runtime' is no more available.@\nIt will be reintroduced \
             in a future release."
      | ["Logic"] ->
	  Wp_parameters.warning ~once:true
	    "Deprecated 'Logic' model.@\nUse 'Typed' with option '-wp-ref' \
             instead." ;
	  {
	    mheap = Factory.Typed MemTyped.Fits ;
	    mvar = Factory.Ref ;
	    cint = Cint.Natural ;
	    cfloat = Cfloat.Real ;
	  }
      | ["Store"] ->
	  Wp_parameters.warning ~once:true
	    "Deprecated 'Store' model.@\nUse 'Typed' instead." ;
	  {
	    mheap = Factory.Typed MemTyped.Fits ;
	    mvar = Factory.Var ;
	    cint = Cint.Natural ;
	    cfloat = Cfloat.Real ;
	  }
      | spec -> Factory.parse spec
  end

let set_model (s:setup) =
  Wp_parameters.Model.set [Factory.id s]

(* --------- WP Computer -------------------- *)

let computer () =
  Driver.load_drivers () ;
  if Wp_parameters.Model.get () = ["Dump"] 
  then CfgDump.create ()
  else Factory.computer (cmdline ())

(* ------------------------------------------------------------------------ *)
(* ---  Printing informations                                           --- *)
(* ------------------------------------------------------------------------ *)

let do_wp_print () =
  (* Printing *)
  if Wp_parameters.Print.get () then
    try
      Wpo.iter ~on_goal:(fun _ -> raise Exit) () ;
      Wp_parameters.result "No proof obligations"
    with Exit ->
      Log.print_on_output
        (fun fmt ->
           Wpo.iter
             ~on_behavior:(Wpo.pp_function fmt)
             ~on_goal:(Wpo.pp_goal_flow fmt) ())

let do_wp_print_for goals =
  if Wp_parameters.Print.get () then
    if Bag.is_empty goals
    then Wp_parameters.result "No proof obligations"
    else Log.print_on_output
      (fun fmt -> Bag.iter (Wpo.pp_goal_flow fmt) goals)

let do_wp_report () =
  let rfiles = Wp_parameters.Report.get () in
  if rfiles <> [] then
    begin
      let stats = WpReport.fcstat () in
      List.iter (WpReport.export stats) rfiles ;
    end

(* ------------------------------------------------------------------------ *)
(* ---  Wp Results                                                      --- *)
(* ------------------------------------------------------------------------ *)

let already_valid goal =
  List.exists (fun (_,r) -> Wpo.is_valid r) (Wpo.get_results goal)

let pp_result wpo fmt r =
  VCS.pp_result fmt r ;
  match r.VCS.verdict with
    | VCS.Unknown | VCS.Timeout | VCS.Stepout ->
	let ws = Wpo.warnings wpo in
	if ws <> [] then
	  let n = List.length ws in
	  let s = List.exists (fun w -> w.Warning.severe) ws in
	  begin
	    match s , n with
	      | true , 1 -> Format.fprintf fmt " (Degenerated)"
	      | true , _ -> Format.fprintf fmt " (Degenerated, %d warnings)" n
	      | false , 1 -> Format.fprintf fmt " (Stronger)"
	      | false , _ -> Format.fprintf fmt " (Stronger, %d warnings)" n
	  end
    | _ -> ()

let do_wpo_start goal prover =
  if Wp_parameters.has_dkey "prover" then
    Wp_parameters.feedback "[%a] Goal %s preprocessing" 
      VCS.pp_prover prover (Wpo.get_gid goal)
  
let do_wpo_feedback goal prover result =
  if Wpo.is_verdict result then
    begin
      Wp_parameters.feedback "[%a] Goal %s : %a"
	VCS.pp_prover prover (Wpo.get_gid goal) (pp_result goal) result;
      if Wp_parameters.ProofTrace.get () || Wp_parameters.UnsatModel.get () then
	Log.print_on_output
	  begin fun fmt ->
	    let logout = Wpo.get_file_logout goal prover in
	    let logerr = Wpo.get_file_logerr goal prover in
	    if Sys.file_exists logout then Command.pp_from_file fmt logout ;
	    if Sys.file_exists logerr then Command.pp_from_file fmt logerr ;
	  end
    end

let wp_why3ide_launch task =
  let server = ProverTask.server () in
  (** Do on_server_stop save why3 session *)
  Task.spawn server task;
  Task.launch server

(* ------------------------------------------------------------------------ *)
(* ---  Checking prover printing                                        --- *)
(* ------------------------------------------------------------------------ *)

let do_wp_check_iter iter_on_goals =
  let provers = [VCS.Coq; VCS.AltErgo; VCS.Why3 "altergo"] in
  let provers = List.map (fun p -> (false,p)) provers in
  Wp_parameters.WhyFlags.add     "--type-only";
  Wp_parameters.AltErgoFlags.add "-type-only";
  let server = ProverTask.server () in
  ignore (Wp_parameters.Share.dir ()); (* To prevent further errors *)
  let do_wpo_feedback goal prover result =
    match result.VCS.verdict with
    | VCS.Computing _ -> ()
    | VCS.Timeout | VCS.Stepout | VCS.Failed ->
      Wp_parameters.feedback "[%a] Type error %s : %a"
	VCS.pp_prover prover (Wpo.get_gid goal) (pp_result goal) result;
    | VCS.NoResult | VCS.Invalid | VCS.Unknown | VCS.Valid
        when Wp_parameters.has_dkey "prover" ->
      Wp_parameters.feedback "[%a] Type ok %s : %a"
	VCS.pp_prover prover (Wpo.get_gid goal) (pp_result goal) result;
    | VCS.NoResult | VCS.Invalid | VCS.Unknown | VCS.Valid -> ()
  in
  iter_on_goals
    (fun goal ->
      if not (already_valid goal) then
	Prover.spawn goal 
	  ~callin:do_wpo_start ~callback:do_wpo_feedback provers
    ) ;
  Task.launch server


let do_wp_check () =
  if Wp_parameters.wpcheck () then
    do_wp_check_iter (fun f -> Wpo.iter ~on_goal:f ())

let do_wp_check_for goals =
  if Wp_parameters.wpcheck () then
    do_wp_check_iter (fun f -> Bag.iter f goals)


	
(* ------------------------------------------------------------------------ *)
(* ---  Proving                                                         --- *)
(* ------------------------------------------------------------------------ *)

let do_wpo_display goal =
  let result = if Wpo.is_trivial goal then "trivial" else "not tried" in
  Wp_parameters.feedback "Goal %s : %s" (Wpo.get_gid goal) result
    
let do_wp_proofs_iter ~provers iter_on_goals =
  if provers <> [] then
    begin
      let server = ProverTask.server () in
      ignore (Wp_parameters.Share.dir ()); (* To prevent further errors *)
      if not (Wp_parameters.has_dkey "no-goals-info") then
	begin
	  let n = ref 0 in
	  iter_on_goals (fun goal -> if not (already_valid goal) then incr n) ;
	  if !n > 1
	  then Wp_parameters.feedback "%d goals scheduled" !n 
	  else Wp_parameters.feedback "%d goal scheduled" !n ;
	end ;
      iter_on_goals
	(fun goal ->
	   if not (already_valid goal) then
	     Prover.spawn goal 
	       ~callin:do_wpo_start ~callback:do_wpo_feedback provers
	) ;
      Task.launch server
    end
  else if not (Wp_parameters.Print.get ()) then
    iter_on_goals
      (fun goal ->
	 if not (already_valid goal) then
	   do_wpo_display goal)

let do_wp_proofs_iter iter =
  let do_why3_ide = ref false in
  let provers = 
    List.fold_right
      (fun pname pvs ->
	 match Wpo.prover_of_name pname with
	   | None -> pvs
           | Some VCS.Why3ide -> do_why3_ide := true; pvs
	   | Some prover -> (VCS.is_interactive pname , prover) :: pvs)
      (match Wp_parameters.Provers.get () 
       with [] -> [ "alt-ergo" ] | pvs -> pvs) [] in 
  begin
    if !do_why3_ide
    then wp_why3ide_launch (Prover.wp_why3ide ~callback:do_wpo_feedback iter) ;
    do_wp_proofs_iter ~provers iter ;
  end
    
let do_wp_proofs () = do_wp_proofs_iter (fun f -> Wpo.iter ~on_goal:f ())

let do_wp_proofs_for goals = do_wp_proofs_iter (fun f -> Bag.iter f goals)

(* ------------------------------------------------------------------------ *)
(* ---  Secondary Entry Points                                          --- *)
(* ------------------------------------------------------------------------ *)

(* Registered entry point in Dynamic. *)

let wp_compute_deprecated kf bhv ipopt =
  Wp_parameters.warning ~once:true "Dynamic 'wp_compute' is now deprecated." ;
  let model = computer () in
  let goals =
    match ipopt with 
      | None -> Generator.compute_kf model ?kf ~bhv () 
      | Some ip -> Generator.compute_ip model ip
  in do_wp_proofs_for goals

let wp_compute_kf kf bhv prop =
  let model = computer () in
  do_wp_proofs_for (Generator.compute_kf model ?kf ~bhv ~prop ())

let wp_compute_ip ip =
  let model = computer () in
  do_wp_proofs_for (Generator.compute_ip model ip)

let wp_compute_call stmt =
  do_wp_proofs_for (Generator.compute_call (computer ()) stmt)

let wp_clear () = Wpo.clear ()

(* ------------------------------------------------------------------------ *)
(* ---  Command-line Entry Points                                       --- *)
(* ------------------------------------------------------------------------ *)

let cmdline_run () =
  let wp_main fct =
    Wp_parameters.feedback "Running WP plugin...";
    Ast.compute ();
    if Wp_parameters.has_dkey "logicusage" then 
      begin
	LogicUsage.compute ();
	LogicUsage.dump ();
      end ;
    if Wp_parameters.has_dkey "varusage" then 
      begin
	VarUsage.compute ();
	VarUsage.dump ();
      end ;
    if Wp_parameters.has_dkey "builtins" then 
      begin
	LogicBuiltins.dump ();
      end ;
    Variables_analysis.precondition_compute ();
    let bhv = Wp_parameters.Behaviors.get () in
    let prop = Wp_parameters.Properties.get () in
    let computer = computer () in
    if Wp_parameters.Froms.get () 
    then Generator.compute_froms computer ~fct ()
    else Generator.compute_selection computer ~fct ~bhv ~prop ()
  in
  match Wp_parameters.job () with
    | Wp_parameters.WP_None -> ()
    | Wp_parameters.WP_All ->
	begin
          ignore (wp_main Generator.F_All);
          do_wp_proofs ();
          do_wp_print ();
	  do_wp_report ();
          do_wp_check ();
	end
    | jb ->
	let fct = 
	  let open Wp_parameters in
	  match jb with
	    | WP_None -> Generator.F_List []
	    | WP_All -> Generator.F_All
	    | WP_Fct fs -> Generator.F_List fs
	    | WP_SkipFct fs -> Generator.F_Skip fs
	in
	begin
          let goals = wp_main fct in
          do_wp_proofs_for goals ;
          do_wp_print_for goals ;
	  do_wp_report () ;
          do_wp_check_for goals;
	end

(* ------------------------------------------------------------------------ *)
(* ---  Register external functions                                     --- *)
(* ------------------------------------------------------------------------ *)

(* DEPRECATED *)
let wp_compute =
  let module OLS = Datatype.List(Datatype.String) in
  let module OKF = Datatype.Option(Kernel_function) in
  let module OP = Datatype.Option(Property) in
  Dynamic.register ~plugin:"Wp" "wp_compute"
    (Datatype.func3 OKF.ty OLS.ty OP.ty Datatype.unit)
    ~journalize:false (*LC: Because of Property is not journalizable. *)
    wp_compute_deprecated

let wp_compute_kf =
  let module OKF = Datatype.Option(Kernel_function) in
  let module OLS = Datatype.List(Datatype.String) in
  Dynamic.register ~plugin:"Wp" "wp_compute_kf"
    (Datatype.func3 OKF.ty OLS.ty OLS.ty Datatype.unit)
    ~journalize:true
    wp_compute_kf

let wp_compute_ip =
  Dynamic.register ~plugin:"Wp" "wp_compute_ip"
    (Datatype.func Property.ty Datatype.unit)
    ~journalize:false (*LC: Because of Property is not journalizable. *)
    wp_compute_ip

let wp_compute_call =
  Dynamic.register ~plugin:"Wp" "wp_compute_call"
    (Datatype.func Cil_datatype.Stmt.ty Datatype.unit)
    ~journalize:true (*LC: Because of Property is not journalizable. *)
    wp_compute_call

let wp_clear =
  Dynamic.register ~plugin:"Wp" "wp_clear"
    (Datatype.func Datatype.unit Datatype.unit)
    ~journalize:false (*LC: To be consistent with Wp_Compute *)
    wp_clear

let run = Dynamic.register ~plugin:"Wp" "run"
  (Datatype.func Datatype.unit Datatype.unit)
  ~journalize:true
  cmdline_run

(* ------------------------------------------------------------------------ *)
(* ---  Tracing WP Invocation                                           --- *)
(* ------------------------------------------------------------------------ *)

let pp_wp_parameters fmt =
  begin
    Format.pp_print_string fmt "# frama-c -wp" ;
    if Wp_parameters.RTE.get () then Format.pp_print_string fmt " -wp-rte" ;
    let spec = Wp_parameters.Model.get () in
    if spec <> [] && spec <> ["Typed"] then
      ( let descr = Factory.descr (Factory.parse spec) in
	Format.fprintf fmt " -wp-model '%s'" descr ) ;
    if not (Wp_parameters.Let.get ()) then Format.pp_print_string fmt
      " -wp-no-let" ;
    if Wp_parameters.Let.get () && not (Wp_parameters.Prune.get ())
    then Format.pp_print_string fmt " -wp-no-prune" ;
    if Wp_parameters.Split.get () then Format.pp_print_string fmt " -wp-split" ;
    let tm = Wp_parameters.Timeout.get () in
    if tm > 10 then Format.fprintf fmt " -wp-timeout %d" tm ;
    let st = Wp_parameters.Steps.get () in
    if tm > 10 then Format.fprintf fmt " -wp-steps %d" st ;
    let dp = Wp_parameters.Depth.get () in
    if dp > 0 then Format.fprintf fmt " -wp-depth %d" dp ;
    Format.pp_print_string fmt " [...]" ;
    Format.pp_print_newline fmt () ;
  end

let () = Cmdline.run_after_setting_files 
  (fun _ -> 
     if Wp_parameters.has_dkey "shell" then
       Log.print_on_output pp_wp_parameters)

let do_prover_detect () =
  if not !Config.is_gui && Wp_parameters.Detect.get () then
    ProverWhy3.detect_why3 
      begin function
	| None -> Wp_parameters.error ~current:false "Why3 not found"
	| Some dps ->
	    List.iter
	      (fun dp ->
		 let open ProverWhy3 in
		 Wp_parameters.result "Prover %10s %-10s [%s]" 
		   dp.dp_name dp.dp_version dp.dp_prover
	      ) dps
      end

(* ------------------------------------------------------------------------ *)
(* ---  Main Entry Point                                                --- *)
(* ------------------------------------------------------------------------ *)

let do_finally job1 job2 () = 
  if Wp_parameters.has_dkey "raised" then
    begin
      job1 () ; 
      job2 () ;
    end
  else
    let r1 = try job1 () ; None with error -> Some error in
    let r2 = try job2 () ; None with error -> Some error in
    match r1 , r2 with
      | None , None -> ()
      | Some e1 , _ -> raise e1
      | None , Some e2 -> raise e2

let (&&&) = do_finally
let rec sequence jobs = match jobs with
  | [] -> fun () -> ()
  | head::tail -> head &&& sequence tail

let tracelog () =
  if Datatype.String.Set.is_empty (Wp_parameters.Debug_category.get ()) then
    Wp_parameters.debug
      "Logging keys : %s." (Wp_parameters.Debug_category.get_set())
  
let main = sequence [
  (fun () -> Wp_parameters.debug ~dkey:job_key "Start WP plugin...@.") ;
  do_prover_detect ;
  cmdline_run ;
  tracelog ;
  Wp_parameters.reset ;
  (fun () -> Wp_parameters.debug ~dkey:job_key "Stop WP plugin...@.") ;
]

let () = Db.Main.extend main
