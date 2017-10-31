(**************************************************************************)
(*                                                                        *)
(*  This file is part of WP plug-in of Frama-C.                           *)
(*                                                                        *)
(*  Copyright (C) 2007-2017                                               *)
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
  Wp_parameters.Model.set [Factory.ident s]

(* --------- WP Computer -------------------- *)

let computer () =
  if Wp_parameters.Model.get () = ["Dump"]
  then CfgDump.create ()
  else CfgWP.computer (cmdline ()) (Driver.load_driver ())

(* ------------------------------------------------------------------------ *)
(* ---  Separation Hypotheses                                           --- *)
(* ------------------------------------------------------------------------ *)

module Models = Model.S.Set
module Fmap = Kernel_function.Map

let wp_iter_model ?ip ?index job =
  begin
    let pool : Models.t Fmap.t ref = ref Fmap.empty in
    Wpo.iter ?ip ?index ~on_goal:(fun wpo ->
        match Wpo.get_index wpo with
        | Wpo.Axiomatic _ -> ()
        | Wpo.Function(kf,_) ->
            let m = Wpo.get_model wpo in
            let ms = try Fmap.find kf !pool with Not_found -> Models.empty in
            if not (Models.mem m ms) then
              pool := Fmap.add kf (Models.add m ms) !pool ;
      ) () ;
    Fmap.iter (fun kf ms -> Models.iter (fun m -> job kf m) ms) !pool
  end

let wp_print_separation fmt =
  begin
    Format.fprintf fmt "//-------------------------------------------@\n" ;
    Format.fprintf fmt "//--- Separation Hypotheses@\n" ;
    Format.fprintf fmt "//-------------------------------------------@\n" ;
    let k = ref 0 in
    let printer = new Printer.extensible_printer () in
    let pp_vdecl = printer#without_annot printer#vdecl in
    wp_iter_model
      (fun kf m ->
         let sep = Separation.requires (Model.get_separation m kf) in
         let vkf = Kernel_function.get_vi kf in
         if sep <> [] then
           begin
             incr k ;
             Format.fprintf fmt
               "@[<hv 0>(*@ behavior %s:" (Model.get_id m) ;
             List.iter
               (fun clause ->
                  Format.fprintf fmt "@ @[<hov 2>requires %a;@]"
                    Separation.pp_clause clause
               ) sep ;
             Format.fprintf fmt "@ *)@\n%a;@]@\n" pp_vdecl vkf ;
             Format.fprintf fmt "//-------------------------------------------@." ;
           end
      ) ;
    if !k>1 then
      begin
        Format.fprintf fmt "/* (%d hypotheses) */@\n" !k ;
        Format.fprintf fmt "//-------------------------------------------@." ;
      end
  end

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
             ~on_axiomatics:(Wpo.pp_axiomatics fmt)
             ~on_behavior:(Wpo.pp_function fmt)
             ~on_goal:(Wpo.pp_goal_flow fmt) ())

let do_wp_print_for goals =
  if Wp_parameters.Print.get () then
    if Bag.is_empty goals
    then Wp_parameters.result "No proof obligations"
    else Log.print_on_output
        (fun fmt -> Bag.iter (Wpo.pp_goal_flow fmt) goals)

let do_wp_report () =
  begin
    let rfiles = Wp_parameters.Report.get () in
    if rfiles <> [] then
      begin
        let stats = WpReport.fcstat () in
        List.iter (WpReport.export stats) rfiles ;
      end ;
    if Wp_parameters.Separation.get () then
      Log.print_on_output wp_print_separation ;
  end

(* ------------------------------------------------------------------------ *)
(* ---  Wp Results                                                      --- *)
(* ------------------------------------------------------------------------ *)

let pp_warnings fmt wpo =
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

let auto_check = function
  | { Wpo.po_formula = Wpo.GoalCheck _ } -> true
  | _ -> false

let launch task =
  let server = ProverTask.server () in
  (** Do on_server_stop save why3 session *)
  Task.spawn server (Task.thread task) ;
  Task.launch server

(* ------------------------------------------------------------------------ *)
(* ---  Prover Stats                                                    --- *)
(* ------------------------------------------------------------------------ *)

let do_wpo_display goal =
  let result = if Wpo.is_trivial goal then "trivial" else "not tried" in
  Wp_parameters.feedback "Goal %s : %s" (Wpo.get_gid goal) result

module PM =
  FCMap.Make(struct
    type t = VCS.prover
    let compare = VCS.cmp_prover
  end)

type pstat = {
  mutable proved : int ;
  mutable unknown : int ;
  mutable interrupted : int ;
  mutable failed : int ;
  mutable n_time : int ;   (* nbr of measured times *)
  mutable a_time : float ; (* sum of measured times *)
  mutable u_time : float ; (* max time *)
  mutable d_time : float ; (* min time *)
  mutable steps : int ;
}

module GOALS = Wpo.S.Set

let scheduled = ref 0
let exercised = ref 0
let spy = ref false
let session = ref GOALS.empty
let proved = ref GOALS.empty
let provers = ref PM.empty

let begin_session () = session := GOALS.empty ; spy := true
let clear_session () = session := GOALS.empty
let end_session   () = session := GOALS.empty ; spy := false
let iter_session f  = GOALS.iter f !session

let clear_scheduled () =
  begin
    scheduled := 0 ;
    exercised := 0 ;
    proved := GOALS.empty ;
    provers := PM.empty ;
  end

let get_pstat p =
  try PM.find p !provers with Not_found ->
    let s = {
      proved = 0 ;
      unknown = 0 ;
      interrupted = 0 ;
      failed = 0 ;
      steps = 0 ;
      n_time = 0 ;
      a_time = 0.0 ;
      u_time = 0.0 ;
      d_time = max_float ;
    } in provers := PM.add p s !provers ; s

let add_step s n =
  if n > s.steps then s.steps <- n

let add_time s t =
  if t > 0.0 then
    begin
      s.n_time <- succ s.n_time ;
      s.a_time <- t +. s.a_time ;
      if t < s.d_time then s.d_time <- t ;
      if t > s.u_time then s.u_time <- t ;
    end

let do_list_scheduled iter_on_goals =
  if not (Wp_parameters.has_dkey VCS.dkey_no_goals_info) then
    begin
      clear_scheduled () ;
      iter_on_goals
        (fun goal -> if not (Wpo.is_proved goal) then
            begin
              incr scheduled ;
              if !spy then session := GOALS.add goal !session ;
            end) ;
      let n = !scheduled in
      if n > 1
      then Wp_parameters.feedback "%d goals scheduled" n
      else Wp_parameters.feedback "%d goal scheduled" n ;
    end

let dkey_prover = Wp_parameters.register_category "prover"

let do_wpo_start goal =
  begin
    incr exercised ;
    if Wp_parameters.has_dkey dkey_prover then
      Wp_parameters.feedback "[Qed] Goal %s preprocessing" (Wpo.get_gid goal) ;
  end

let do_wpo_wait () =
  Wp_parameters.feedback ~ontty:`Transient "[wp] Waiting provers..."

let do_wpo_prover goal _prover =
  begin
    if !scheduled > 0 then
      let pp = int_of_float (100.0 *. float !exercised /. float !scheduled) in
      let pp = max 0 (min 100 pp) in
      Wp_parameters.feedback ~ontty:`Transient "[%02d%%] %s" pp goal.Wpo.po_sid ;
  end

let do_wpo_stat goal prover res =
  let s = get_pstat prover in
  let open VCS in
  match res.verdict with
  | Checked | NoResult | Computing _ | Invalid | Unknown ->
      s.unknown <- succ s.unknown
  | Stepout | Timeout ->
      s.interrupted <- succ s.interrupted
  | Failed ->
      s.failed <- succ s.failed
  | Valid ->
      if not (Wpo.is_tactic goal) then
        proved := GOALS.add goal !proved ;
      s.proved <- succ s.proved ;
      add_step s res.prover_steps ;
      add_time s res.prover_time ;
      if prover <> Qed then
        add_time (get_pstat Qed) res.solver_time

let do_wpo_result goal prover res =
  if VCS.is_verdict res then
    begin
      if Wp_parameters.Check.get () then
        begin
          let open VCS in
          let ontty = if res.verdict = Checked then `Feedback else `Message in
          Wp_parameters.feedback ~ontty
            "[%a] Goal %s : %a"
            VCS.pp_prover prover (Wpo.get_gid goal)
            VCS.pp_result res ;
        end ;
      if prover = VCS.Qed then do_wpo_prover goal prover ;
      do_wpo_stat goal prover res ;
    end

let do_why3_result goal prover res =
  if VCS.is_verdict res then
    begin
      do_wpo_stat goal prover res ;
      let open VCS in
      if res.verdict <> Valid then
        Wp_parameters.result
          "[%a] Goal %s : %a"
          VCS.pp_prover prover (Wpo.get_gid goal)
          VCS.pp_result res ;
    end

let do_wpo_success goal s =
  if not (Wp_parameters.Check.get ()) then
    match s with
    | None ->
        if not (Wp_parameters.Generate.get ()) then
          begin
            match Wpo.get_results goal with
            | [p,r] ->
                Wp_parameters.result "[%a] Goal %s : %a%a"
                  VCS.pp_prover p (Wpo.get_gid goal)
                  VCS.pp_result r pp_warnings goal
            | pres ->
                Wp_parameters.result "[Failed] Goal %s%t" (Wpo.get_gid goal)
                  begin fun fmt ->
                    pp_warnings fmt goal ;
                    List.iter
                      (fun (p,r) ->
                         Format.fprintf fmt "@\n%8s: @[<hv>%a@]"
                           (VCS.title_of_prover p) VCS.pp_result r
                      ) pres ;
                  end
          end
    | Some prover ->
        if not (auto_check goal) then
          Wp_parameters.feedback ~ontty:`Silent
            "[%a] Goal %s : Valid" VCS.pp_prover prover (Wpo.get_gid goal)

let do_tac_result goal prv res =
  if VCS.is_verdict res then
    begin
      if res.VCS.verdict <> VCS.Valid then
        Wp_parameters.result "[%a] Goal %s : %a"
          VCS.pp_prover prv (Wpo.get_gid goal)
          VCS.pp_result res ;
      do_wpo_stat goal prv res ;
    end

let do_tac_success goal result =
  begin
    List.iter
      (fun (p,r) -> if VCS.is_verdict r then do_wpo_stat goal p r)
      (Wpo.get_results goal) ;
    do_wpo_success goal result ;
  end

let do_report_prover_stats pp_prover fmt (p,s) =
  begin
    let name = VCS.title_of_prover p in
    Format.fprintf fmt "%a %4d " pp_prover name s.proved ;
    begin
      if s.n_time > 0 &&
         s.u_time > Rformat.epsilon &&
         not (Wp_parameters.has_dkey VCS.dkey_no_time_info)
      then
        let mean = s.a_time /. float s.n_time in
        let epsilon = 0.05 *. mean in
        let delta = s.u_time -. s.d_time in
        if delta < epsilon then
          Format.fprintf fmt " (%a)" Rformat.pp_time mean
        else
          let middle = (s.u_time +. s.d_time) *. 0.5 in
          if abs_float (middle -. mean) < epsilon then
            Format.fprintf fmt " (%a-%a)"
              Rformat.pp_time s.d_time
              Rformat.pp_time s.u_time
          else
            Format.fprintf fmt " (%a-%a-%a)"
              Rformat.pp_time s.d_time
              Rformat.pp_time mean
              Rformat.pp_time s.u_time
    end ;
    if s.steps > 0  &&
       not (Wp_parameters.has_dkey VCS.dkey_no_step_info) then
      Format.fprintf fmt " (%d)" s.steps ;
    if s.interrupted > 0 then
      Format.fprintf fmt " (interrupted: %d)" s.interrupted ;
    if s.unknown > 0 then
      Format.fprintf fmt " (unknown: %d)" s.unknown ;
    if s.failed > 0 then
      Format.fprintf fmt " (failed: %d)" s.failed ;
    Format.fprintf fmt "@\n" ;
  end

let do_report_scheduled () =
  if not (Wp_parameters.has_dkey VCS.dkey_no_goals_info) then
    if Wp_parameters.Generate.get () then
      let plural = if !exercised > 1 then "s" else "" in
      Wp_parameters.result "%d goal%s generated" !exercised plural
    else
      begin
        let proved = GOALS.cardinal !proved in
        Wp_parameters.result "%t"
          (fun fmt ->
             Format.fprintf fmt "Proved goals: %4d / %d@\n" proved !scheduled ;
             Pretty_utils.pp_items
               ~min:12 ~align:`Left
               ~title:(fun (prover,_) -> VCS.title_of_prover prover)
               ~iter:(fun f -> PM.iter (fun p s -> f (p,s)) !provers)
               ~pp_title:(fun fmt a -> Format.fprintf fmt "%s:" a)
               ~pp_item:do_report_prover_stats fmt) ;
      end

let do_list_scheduled_result () =
  begin
    do_report_scheduled () ;
    clear_scheduled () ;
  end

(* ------------------------------------------------------------------------ *)
(* ---  Proving                                                         --- *)
(* ------------------------------------------------------------------------ *)

type mode = {
  mutable why3ide : bool ;
  mutable tactical : bool ;
  mutable update : bool ;
}

let spawn_wp_proofs_iter ~mode ~provers iter_on_goals =
  if mode.tactical || provers<>[] then
    begin
      let server = ProverTask.server () in
      ignore (Wp_parameters.Share.dir ()); (* To prevent further errors *)
      iter_on_goals
        (fun goal ->
           if not (Wpo.is_proved goal) then
             begin
               if mode.tactical && ProofSession.exists goal then
                 ProverScript.spawn
                   ~failed:false
                   ~provers:(List.map snd provers)
                   ~start:do_wpo_start
                   ~callin:do_wpo_prover
                   ~callback:do_tac_result
                   ~success:do_tac_success
                   goal
               else
                 Prover.spawn goal
                   ~start:do_wpo_start
                   ~callin:do_wpo_prover
                   ~callback:do_wpo_result
                   ~success:do_wpo_success
                   provers
             end ;
        ) ;
      Task.on_server_wait server do_wpo_wait ;
      Task.launch server
    end

let get_prover_names () =
  match Wp_parameters.Provers.get () with [] -> [ "alt-ergo" ] | pnames -> pnames

let compute_provers ~mode () =
  List.fold_right
    (fun pname prvs ->
       match VCS.prover_of_name pname with
       | None -> prvs
       | Some VCS.Why3ide ->
           mode.why3ide <- true; prvs
       | Some VCS.Tactical ->
           mode.tactical <- true ;
           if pname = "tip" then mode.update <- true ;
           prvs
       | Some prover ->
           (VCS.mode_of_prover_name pname , prover) :: prvs)
    (get_prover_names ()) []

let do_update_session mode iter =
  if mode.update then
    begin
      let removed = ref 0 in
      let updated = ref 0 in
      let invalid = ref 0 in
      iter
        begin fun goal ->
          let results = Wpo.get_results goal in
          let autoproof (p,r) =
            (p=VCS.Qed) || (VCS.is_auto p && VCS.is_valid r && VCS.autofit r) in
          if List.exists autoproof results then
            begin
              if ProofSession.exists goal then
                (incr removed ; ProofSession.remove goal)
            end
          else
            let scripts = ProofEngine.script (ProofEngine.proof ~main:goal) in
            if scripts <> [] then
              begin
                let keep = function
                  | ProofScript.Prover(p,r) -> VCS.is_auto p && VCS.is_valid r
                  | ProofScript.Tactic(n,_,_) -> n=0
                  | ProofScript.Error _ -> false in
                let strategy = List.filter keep scripts in
                if strategy <> [] then
                  begin
                    incr updated ;
                    ProofSession.save goal (ProofScript.encode strategy)
                  end
                else
                if not (ProofSession.exists goal) then
                  begin
                    incr invalid ;
                    ProofSession.save goal (ProofScript.encode scripts)
                  end
              end
        end ;
      let r = !removed in
      let u = !updated in
      let f = !invalid in
      ( if r = 0 && u = 0 && f = 0 then
          Wp_parameters.result "No updated script." ) ;
      ( if r > 0 then
          let s = if r > 1 then "s" else "" in
          Wp_parameters.result "Updated session with %d new automated proof%s." r s );
      ( if u > 0 then
          let s = if u > 1 then "s" else "" in
          Wp_parameters.result "Updated session with %d new valid script%s." u s ) ;
      ( if f > 0 then
          let s = if f > 1 then "s" else "" in
          Wp_parameters.result "Updated session with %d new script%s to complete." f s );
    end

let do_wp_proofs_iter iter =
  let mode = {
    why3ide=false ;
    tactical=false ;
    update=false ;
  } in
  let provers = compute_provers ~mode () in
  let spawned = mode.why3ide || mode.tactical || provers <> [] in
  begin
    if spawned then do_list_scheduled iter ;
    if mode.why3ide then
      launch (ProverWhy3ide.prove ~callback:do_why3_result ~iter) ;
    spawn_wp_proofs_iter ~mode ~provers iter ;
    if spawned then
      begin
        do_list_scheduled_result () ;
        do_update_session mode iter ;
      end
    else if not (Wp_parameters.Print.get ()) then
      iter
        (fun goal ->
           if not (Wpo.is_proved goal) then
             do_wpo_display goal)
  end

let do_wp_proofs () = do_wp_proofs_iter (fun f -> Wpo.iter ~on_goal:f ())

let do_wp_proofs_for goals = do_wp_proofs_iter (fun f -> Bag.iter f goals)

(* ------------------------------------------------------------------------ *)
(* ---  Secondary Entry Points                                          --- *)
(* ------------------------------------------------------------------------ *)

(* Deprecated entry point in Dynamic. *)

let deprecated_wp_compute kf bhv ipopt =
  let model = computer () in
  let goals =
    match ipopt with
    | None -> Generator.compute_kf model ?kf ~bhv ()
    | Some ip -> Generator.compute_ip model ip
  in do_wp_proofs_for goals

let deprecated_wp_compute_kf kf bhv prop =
  let model = computer () in
  do_wp_proofs_for (Generator.compute_kf model ?kf ~bhv ~prop ())

let deprecated_wp_compute_ip ip =
  Wp_parameters.warning ~once:true "Dynamic 'wp_compute_ip' is now deprecated." ;
  let model = computer () in
  do_wp_proofs_for (Generator.compute_ip model ip)

let deprecated_wp_compute_call stmt =
  Wp_parameters.warning ~once:true "Dynamic 'wp_compute_ip' is now deprecated." ;
  do_wp_proofs_for (Generator.compute_call (computer ()) stmt)

let deprecated_wp_clear () =
  Wp_parameters.warning ~once:true "Dynamic 'wp_compute_ip' is now deprecated." ;
  Wpo.clear ()

(* ------------------------------------------------------------------------ *)
(* ---  Command-line Entry Points                                       --- *)
(* ------------------------------------------------------------------------ *)

let dkey_logicusage = Wp_parameters.register_category "logicusage"
let dkey_refusage = Wp_parameters.register_category "refusage"
let dkey_builtins = Wp_parameters.register_category "builtins"

let cmdline_run () =
  let wp_main fct =
    Wp_parameters.feedback ~ontty:`Feedback "Running WP plugin...";
    Ast.compute ();
    Dyncall.compute ();
    if Wp_parameters.has_dkey dkey_logicusage then
      begin
        LogicUsage.compute ();
        LogicUsage.dump ();
      end ;
    if Wp_parameters.has_dkey dkey_refusage then
      begin
        RefUsage.compute ();
        RefUsage.dump ();
      end ;
    if Wp_parameters.has_dkey dkey_builtins then
      begin
        LogicBuiltins.dump ();
      end ;
    let bhv = Wp_parameters.Behaviors.get () in
    let prop = Wp_parameters.Properties.get () in
    let computer = computer () in
    Generator.compute_selection computer ~fct ~bhv ~prop ()
  in
  match Wp_parameters.job () with
  | Wp_parameters.WP_None -> ()
  | Wp_parameters.WP_All ->
      begin
        ignore (wp_main Generator.F_All);
        do_wp_proofs ();
        do_wp_print ();
        do_wp_report ();
      end
  | jb ->
      let fct =
        let open Wp_parameters in
        match jb with
        | WP_None -> Generator.F_List Cil_datatype.Kf.Set.empty
        | WP_All -> Generator.F_All
        | WP_Fct fs -> Generator.F_List fs
        | WP_SkipFct fs -> Generator.F_Skip fs
      in
      begin
        let goals = wp_main fct in
        do_wp_proofs_for goals ;
        do_wp_print_for goals ;
        do_wp_report () ;
      end

(* ------------------------------------------------------------------------ *)
(* ---  Register external functions                                     --- *)
(* ------------------------------------------------------------------------ *)

let deprecated name =
  Wp_parameters.warning ~once:true ~current:false
    "Dynamic '%s' now is deprecated. Use `Wp.VC` api instead." name

let register name ty code =
  let _ = 
    Dynamic.register ~plugin:"Wp" name ty
      ~journalize:false (*LC: Because of Property is not journalizable. *)
      (fun x -> deprecated name ; code x)
  in ()

(* DEPRECATED *)
let () =
  let module OLS = Datatype.List(Datatype.String) in
  let module OKF = Datatype.Option(Kernel_function) in
  let module OP = Datatype.Option(Property) in
  register "wp_compute"
    (Datatype.func3 OKF.ty OLS.ty OP.ty Datatype.unit)
    deprecated_wp_compute

let () =
  let module OKF = Datatype.Option(Kernel_function) in
  let module OLS = Datatype.List(Datatype.String) in
  register "wp_compute_kf"
    (Datatype.func3 OKF.ty OLS.ty OLS.ty Datatype.unit)
    deprecated_wp_compute_kf

let () =
  register "wp_compute_ip"
    (Datatype.func Property.ty Datatype.unit)
    deprecated_wp_compute_ip

let () =
  register "wp_compute_call"
    (Datatype.func Cil_datatype.Stmt.ty Datatype.unit)
    deprecated_wp_compute_call

let () =
  register "wp_clear"
    (Datatype.func Datatype.unit Datatype.unit)
    deprecated_wp_clear

let run = Dynamic.register ~plugin:"Wp" "run"
    (Datatype.func Datatype.unit Datatype.unit)
    ~journalize:true
    cmdline_run

let () =
  let open Datatype in
  begin
    let t_job = func Unit.ty Unit.ty in
    let t_iter = func (func Wpo.S.ty Unit.ty) Unit.ty in
    let register name ty f =
      ignore (Dynamic.register name ty ~plugin:"Wp" ~journalize:false f)
    in
    register "wp_begin_session" t_job  begin_session ;
    register "wp_end_session"   t_job  end_session   ;
    register "wp_clear_session" t_job  clear_session ;
    register "wp_iter_session"  t_iter iter_session  ;
  end

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
    if not (Kernel.SignedOverflow.get ()) then
      Format.pp_print_string fmt " -no-warn-signed-overflow" ;
    if Kernel.UnsignedOverflow.get () then
      Format.pp_print_string fmt " -warn-unsigned-overflow" ;
    if Kernel.SignedDowncast.get () then
      Format.pp_print_string fmt " -warn-signed-downcast" ;
    if Kernel.UnsignedDowncast.get () then
      Format.pp_print_string fmt " -warn-unsigned-downcast" ;
    if not (Wp_parameters.Volatile.get ()) then
      Format.pp_print_string fmt " -wp-no-volatile" ;
    Format.pp_print_string fmt " [...]" ;
    Format.pp_print_newline fmt () ;
  end

let dkey_shell = Wp_parameters.register_category "shell"

let () = Cmdline.run_after_setting_files
    (fun _ ->
       if Wp_parameters.has_dkey dkey_shell then
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

(*
(* This filter can be changed to make exceptions interrupting
   the sequence immediately *)
let catch_exn (_:exn) =
  not (Wp_parameters.has_dkey "raised")

(* This order can be changed *)
let reraised_exn (first:exn) (_last:exn) = Some first

(* Don't use Extlib.try_finally:
   No exception is used for control here.
   Backtrace is dumped here for debugging purpose.
   We just record one of the raised exceptions (to be raised again),
   while ensuring all tasks are finally executed. *)
let protect err job =
  try job ()
  with e when catch_exn e ->
    let b = Printexc.get_raw_backtrace () in
    Wp_parameters.failure "%s@\n%s"
      (Printexc.to_string e)
      (Printexc.raw_backtrace_to_string b) ;
    match !err with
    | None -> err := Some e
    | Some previous -> err := reraised_exn previous e

let sequence jobs =
  let err = ref None in
  List.iter (protect err) jobs ;
  match !err with None -> () | Some e -> raise e
*)

let rec try_sequence jobs () = match jobs with
  | [] -> ()
  | head :: tail ->
      Extlib.try_finally ~finally:(try_sequence tail) head ()

let dkey_raised = Wp_parameters.register_category "raised"

let sequence jobs () =
  if Wp_parameters.has_dkey dkey_raised
  then List.iter (fun f -> f ()) jobs
  else try_sequence jobs ()

let tracelog () =
  if Wp_parameters.Debug_category.is_empty () then
    Wp_parameters.debug
      "Logging keys: %s." (Wp_parameters.Debug_category.As_string.get ())

let main = sequence [
    (fun () -> Wp_parameters.debug ~dkey:job_key "Start WP plugin...@.") ;
    do_prover_detect ;
    cmdline_run ;
    tracelog ;
    Wp_parameters.reset ;
    (fun () -> Wp_parameters.debug ~dkey:job_key "Stop WP plugin...@.") ;
  ]

let () = Db.Main.extend main
