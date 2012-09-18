(**************************************************************************)
(*                                                                        *)
(*  This file is part of WP plug-in of Frama-C.                           *)
(*                                                                        *)
(*  Copyright (C) 2007-2012                                               *)
(*    CEA (Commissariat a l'énergie atomique et aux énergies              *)
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

let job_key= "trace-job";

(* -------------------------------------------------------------------------- *)
(* --- WP Models for VC generation                                        --- *)
(* -------------------------------------------------------------------------- *)

module F = Fol_formula
module A = Mint_natural.Create(F)
module R = Mfloat_natural.Create(F)

module Hoare = Hoare_mem.Create(F)(A)(R)
module Store = Store_mem.Create(F)(A)(R)
module Runtime = Runtime_mem.Create(F)(A)(R)

module Th =
struct
  let tau_of_ctype_logic t =
    Hoare.tau_of_object (Ctypes.object_of t)
end
module HW = Fol_why.EWhy(Th)
module HQ = Fol_coq.ECoq(Th)
module HE = Fol_ergo.Make(Th)

module Ts =
struct
  let tau_of_ctype_logic t =
    Store.tau_of_object (Ctypes.object_of t)
end
module SW = Fol_why.EWhy(Ts)
module SQ = Fol_coq.ECoq(Ts)
module SE = Fol_ergo.Make(Ts)

module Tr =
struct
  let tau_of_ctype_logic t =
    Runtime.tau_of_object (Ctypes.object_of t)
end
module RW = Fol_why.EWhy(Tr)
module RQ = Fol_coq.ECoq(Tr)
module RE = Fol_ergo.Make(Tr)

module MCriteria : Funvar_mem.Criteria = struct let isHoare = false end

module HCriteria : Funvar_mem.Criteria = struct let isHoare = true end

(* --------- WP Calculus Engines ------------------ *)

module WP_Hoare =
  CfgProof.Create(Funvar_mem.Create(HCriteria)(Hoare))(HW)(HQ)(HE)
    (Fol_split)
    (struct
       let shared = "hoare"
       let context = "hoare"
       let updater = "Hoare"
       let name = "Hoare"
     end)

module WP_Store = CfgProof.Create(Store)(SW)(SQ)(SE)
  (Fol_split)
  (struct
     let shared = "store"
     let context = "store_full"
     let updater = "Store-Full"
     let name = "Store(full memory)"
   end)

module WP_Storefun =
  CfgProof.Create(Funvar_mem.Create(MCriteria)(Store))(SW)(SQ)(SE)
    (Fol_split)
  (struct
     let shared = "store"
     let context = "store"
     let updater = "Store"
     let name = "Store"
   end)

module WP_Logic =
  CfgProof.Create(Logic_mem.Create(Store))(SW)(SQ)(SE)(Fol_split)
    (struct
       let shared = "store"
       let context = "logic"
       let updater = "Logic"
       let name = "Logic"
     end)

module WP_Runtime = CfgProof.Create(Runtime)(RW)(RQ)(RE)
  (Fol_split)
  (struct
     let shared = "runtime"
     let context = "runtime_full"
     let updater = "Runtime-Full"
     let name = "Runtime(full memory)"
   end)

module WP_Runtimefun =
  CfgProof.Create(Funvar_mem.Create(MCriteria)(Runtime))(RW)(RQ)(RE)
  (Fol_split)
  (struct
     let shared = "runtime"
     let context = "runtime"
     let updater = "Runtime"
     let name = "Runtime"
   end)

(* --------- NEW WP Computers -------------------- *)

module VarHoare : MemVar.VarUsage = 
struct 
  let datatype = "Value"
  let param _x = MemVar.ByValue 
end
module VarRef0 : MemVar.VarUsage = 
struct
  let datatype = "Ref0"
  let param x = match Variables_analysis.dispatch_cvar x with
    | Variables_analysis.Fvar -> MemVar.ByValue
    | _ -> MemVar.InHeap
end
module VarRef1 : MemVar.VarUsage = 
struct
  let datatype = "Ref1"
  let param x = match Variables_analysis.dispatch_cvar x with
    | Variables_analysis.Fvar -> MemVar.ByValue
    | Variables_analysis.PRarg -> MemVar.ByRef
    | _ -> MemVar.InHeap
end
module VarRef2 : MemVar.VarUsage = 
struct
  let datatype = "Ref2"
  let param x = match VarUsage.of_cvar x with
    | VarUsage.NotUsed | VarUsage.ByValue | VarUsage.ByArray _ 
    | VarUsage.ByRefArray _ -> MemVar.ByValue
    | VarUsage.ByReference -> MemVar.ByRef
    | VarUsage.ByAddress -> MemVar.InHeap
end
module MPure = MemVar.Make(VarHoare)(MemEmpty)
module MTypedVar = MemVar.Make(VarRef0)(MemTyped)
module MTypedRef = MemVar.Make(VarRef2)(MemTyped)

module WP_Pure = CfgWP.Computer(MPure)
module WP_TypedAll = CfgWP.Computer(MemTyped)
module WP_TypedVar = CfgWP.Computer(MTypedVar)
module WP_TypedRef = CfgWP.Computer(MTypedRef)

module MODELS =
struct

  module H = Datatype.String.Map
  let h = ref H.empty (* NOT PROJECTIFIED: OCaml link phase only *)
      
  let add 
      (computer : Model.t -> Generator.computer) 
      (registry:Model.registry) 
      ~name ~id ~descr ?tuning () 
      =
    let model = registry ~name ~id ~descr ?tuning () in
    h := H.add id (computer , model) !h ; model

  let create id : Generator.computer =
    try let (create,model) = H.find id !h in create model
    with Not_found -> 
      Wp_parameters.abort "Unknown model '%s'" id

end


let m_pure = MODELS.add WP_Pure.create MPure.register 
  ~id:"pure" 
  ~name:"Pure" 
  ~descr:"Pure Hoare Model" ()

let m_typedall = MODELS.add WP_TypedAll.create MemTyped.register 
  ~id:"typedraw"
  ~name:"Typed (Raw)"
  ~descr:"Typed Memory Model (only)" ()

let m_typedvar = MODELS.add WP_TypedVar.create MemTyped.register 
  ~id:"typed" 
  ~name:"Typed (Var)"
  ~descr:"Typed Memory Model with Variables" ()

let m_typedref = MODELS.add WP_TypedRef.create MemTyped.register 
  ~id:"typedref" 
  ~name:"Typed (Ref)"
  ~descr:"Typed Memory Model with References" ()

let m_typedfit = MODELS.add WP_TypedRef.create MemTyped.register 
  ~id:"typedfit"
  ~name:"Typed (Fit)"
  ~tuning:[MemTyped.fits]
  ~descr:"Typed Memory Model with References and fitting pointer casts" ()

(* --------- WP Dispatcher ------------------ *)

open Generator

type feature = NA | Yes | No

type wp_model = {
  wp_name : string ;
  wp_qed : feature ;
  wp_logicvar : feature ;
  wp_effect_supported : bool ;
  wp_assigns_supported : bool ;
  wp_computer : unit -> Generator.computer ;
  wp_altmodel : (unit -> Generator.computer) option ;
}

let wp_model name = {
  wp_name = name ;
  wp_qed = NA ;
  wp_logicvar = NA ;
  wp_effect_supported = true ;
  wp_assigns_supported = true ;
  wp_computer = (fun () -> Wp_parameters.fatal "computer not implemented") ;
  wp_altmodel = None ;
}

let assigns_method w =
  let mth = Wp_parameters.get_assigns_method () in
  match mth with
    | Wp_parameters.NoAssigns -> Wp_parameters.NoAssigns
    | Wp_parameters.EffectAssigns when w.wp_effect_supported -> 
	Wp_parameters.EffectAssigns
    | _ ->
        if w.wp_assigns_supported
        then Wp_parameters.NormalAssigns
        else Wp_parameters.NoAssigns

let option opt = function
  | NA -> true
  | Yes -> opt ()
  | No -> not (opt ())

let dispatch models =
  try
    let model = List.find
      (fun m ->
         List.for_all
           (fun (opt,f) ->
              match f with
                | NA -> true
                | Yes -> opt ()
                | No -> not (opt ()) )
           [
             (fun () -> false) , m.wp_qed ;
             Wp_parameters.LogicVar.get , m.wp_logicvar ;
           ]
      ) models in
    let computer = model.wp_computer () in
    match assigns_method model , model.wp_altmodel with
      | Wp_parameters.NoAssigns , None -> NonAssigns computer
      | Wp_parameters.NoAssigns , Some alt -> TwoPasses( computer , alt () )
      | _ -> Generic computer
  with Not_found ->
    Wp_parameters.abort "No model found with provided criteria"

(* --------- WP Computer -------------------- *)

(*
  computer returns :
  - either one computer for both assigns and non-assigns
  - or a unique computer for only non-assigns
  - or two computers, one for assigns and one for non-assigns
*)

let computer () = 
  match Wp_parameters.get_model () with

  | Wp_parameters.M_Q "Dump" -> Generic (CfgDump.create ())
  | Wp_parameters.M_Q "Pure" -> Generic (WP_Pure.create m_pure)
  | Wp_parameters.M_Q "Typed" -> 
      if Wp_parameters.LogicVar.get () then
	match Wp_parameters.RefVar.get () , Wp_parameters.Fits.get () with
	  | true , false -> Generic (WP_TypedRef.create m_typedref)
	  | _ , true -> Generic (WP_TypedRef.create m_typedfit)
	  | _ -> Generic (WP_TypedVar.create m_typedvar)
      else Generic (WP_TypedAll.create m_typedall)

  | Wp_parameters.M_Q model ->
      Generic (MODELS.create model)

  | Wp_parameters.M_Hoare  ->
      NonAssigns (WP_Hoare.create ())
	
  | Wp_parameters.M_Logic ->
      dispatch [
	{ (wp_model "Logic") with
	    wp_logicvar = Yes ;
	    wp_assigns_supported = false ;
	    wp_computer = WP_Logic.create ;
	    wp_altmodel = Some WP_Store.create ;
	} ;
	{ (wp_model "Store") with
            wp_logicvar = No ;
            wp_computer = WP_Store.create ;
        }
      ]
	
  | Wp_parameters.M_Store ->
      dispatch [
        { (wp_model "Store") with
            wp_logicvar = Yes ;
            wp_computer = WP_Storefun.create ;
	    wp_assigns_supported = false ;
            wp_altmodel = Some WP_Store.create ;
        } ;
        { (wp_model "Store") with
            wp_logicvar = No ;
            wp_computer = WP_Store.create ;
        }
      ]
	
  | Wp_parameters.M_Runtime ->
      dispatch [
        { (wp_model "Runtime") with
            wp_logicvar = Yes ;
            wp_computer = WP_Runtimefun.create ;
	    wp_assigns_supported = false ;
            wp_altmodel = Some WP_Runtime.create ;
        } ;
        { (wp_model "Runtime") with
            wp_logicvar = No ;
            wp_computer = WP_Runtime.create ;
        }
      ]

(* ------------------------------------------------------------------------ *)
(* --- Shared Functions for both GUI and command line                   --- *)
(* ------------------------------------------------------------------------ *)

let dot_lannots lannots =
  let do_dot annots =
    let cfg = WpStrategy.cfg_of_strategy annots in
    let pp_annots fmt e =
      WpStrategy.pp_annots fmt (WpStrategy.get_annots annots e)
    in
    let bhv = WpStrategy.behavior_name_of_strategy annots in
    ignore (Cil2cfg.dot_annots cfg bhv pp_annots)
  in List.iter do_dot lannots

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
             ~on_environment:
	     (fun env -> Wpo.VC_Legacy.pp_environment fmt ~env)
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
(* ---  Proving                                                         --- *)
(* ------------------------------------------------------------------------ *)

let already_valid goal =
  List.exists (fun (_,r) -> Wpo.is_valid r) (Wpo.get_results goal)

let pp_result wpo fmt r =
  match r.VCS.verdict with
    | VCS.Unknown | VCS.Timeout | VCS.Stepout ->
	let ws = Wpo.warnings wpo in
	if ws = [] then VCS.pp_result fmt r else
	  let n = List.length ws in
	  let s = List.exists (fun w -> w.Warning.wrn_severe) ws in
	  begin
	    match s , n with
	      | true , 1 -> Format.fprintf fmt "Degenerated (1 warning)"
	      | true , _ -> Format.fprintf fmt "Degenerated (%d warnings)" n
	      | false , 1 -> Format.fprintf fmt "Stronger (1 warning)"
	      | false , _ -> Format.fprintf fmt "Stronger (%d warnings)" n
	  end
    | _ -> VCS.pp_result fmt r

let do_wpo_feedback goal prover result =
  if Wpo.is_verdict result then
    Wp_parameters.feedback "[%a] Goal %s : %a"
      VCS.pp_prover prover (Wpo.get_gid goal) (pp_result goal) result

let do_wpo_display goal =
  Wp_parameters.feedback "Goal %s : not tried" (Wpo.get_gid goal) 
    
let do_wp_proofs_iter iter_on_goals =
  let provers = 
    List.fold_right
      (fun pname pvs ->
	 match Wpo.prover_of_name pname with
	   | None -> pvs
	   | Some prover -> 
	       let ide = VCS.is_interactive pname in
	       (ide,prover) :: pvs)
      (Wp_parameters.get_provers()) []
  in
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
	     Prover.spawn goal ~callback:do_wpo_feedback provers
	) ;
      Task.launch server
    end
  else if not (Wp_parameters.Print.get ()) then
    iter_on_goals
      (fun goal ->
	 if not (already_valid goal) then
	   do_wpo_display goal)

let do_wp_proofs () = do_wp_proofs_iter (fun f -> Wpo.iter ~on_goal:f ())

let do_wp_proofs_for goals = do_wp_proofs_iter (fun f -> Bag.iter f goals)

(* ------------------------------------------------------------------------ *)
(* ---  Type Checking prover's inputs                                   --- *)
(* ------------------------------------------------------------------------ *)

let do_check_feedback g _vcd lang result =
  Wp_parameters.feedback "[%a] Goal %s : %a"
    VCS.pp_language lang (Wpo.get_gid g) VCS.pp_result result

let do_wp_check server lang g =
  match g.Wpo.po_formula with
    | Wpo.Legacy vcd -> 
	let callback = do_check_feedback g in
	let task = ProverVCD.check g vcd ~callback lang in
	Task.spawn server task
    | Wpo.GoalAnnot { Wpo.VC_Annot.model=model }
    | Wpo.GoalLemma { Wpo.VC_Lemma.model=model } -> 
	Wp_parameters.warning ~once:true 
	  "No check for model '%s'" (Model.get_name model)

let do_wp_checks () =
  match VCS.language_of_name (Wp_parameters.Check.get ()) with
    | None -> ()
    | Some lang ->
        let server = ProverTask.server () in
        try
          Wpo.iter ~on_goal:(do_wp_check server lang) () ;
          Task.launch server ;
        with e ->
          Task.cancel_all server ;
          raise e

let do_wp_checks_for goals =
  match VCS.language_of_name (Wp_parameters.Check.get ()) with
    | None -> ()
    | Some lang ->
        let server = ProverTask.server () in
        try
          Bag.iter (do_wp_check server lang) goals ;
          Task.launch server ;
        with e ->
          Task.cancel_all server ;
          raise e

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

let wp_clear () =
  begin
    F.clear () ;
    Wpo.clear () ;
  end

(* ------------------------------------------------------------------------ *)
(* ---  Command-line Entry Points                                       --- *)
(* ------------------------------------------------------------------------ *)

let cmdline_run () =
  let wp_main fct =
    Wp_parameters.feedback "Running WP plugin...@.";
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
    then Generator.compute_froms computer ?fct ()
    else Generator.compute_selection computer ?fct ~bhv ~prop ()
  in
  match Wp_parameters.job () with
    | Wp_parameters.WP_None -> ()
    | Wp_parameters.WP_All ->
	begin
          ignore (wp_main None);
          do_wp_checks ();
          do_wp_proofs ();
          do_wp_print ();
	  do_wp_report ();
	end
    | Wp_parameters.WP_Select fcts ->
	begin
          let goals = wp_main (Some fcts) in
          do_wp_checks_for goals ;
          do_wp_proofs_for goals ;
          do_wp_print_for goals ;
	  do_wp_report () ;
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
    let m = Wp_parameters.Model.get () in
    if m <> "Store" then Format.fprintf fmt " -wp-model %s" m ;
    if not (Wp_parameters.LogicVar.get ()) then Format.pp_print_string fmt " -wp-no-logicvar" ;
    if Wp_parameters.RefVar.get () then Format.pp_print_string fmt " -wp-byreference" ;
    if Wp_parameters.Qed.get () then Format.pp_print_string fmt " -wp-qed" ;
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

let tracelog () =
  let ks = Wp_parameters.get_debug_keyset () in
  if ks <> [] then
    let pp_keys : Format.formatter -> string list -> unit = 
      Pretty_utils.pp_flowlist ~left:"" ~sep:"," ~right:"." Format.pp_print_string
    in Wp_parameters.debug ~level:0 "Logging keys : %a" pp_keys ks

let (&&&) = do_finally
  
let main =
  (fun () -> Wp_parameters.debug ~dkey:job_key "Start WP plugin...@.") &&&
  cmdline_run &&& tracelog &&& Wp_parameters.reset &&&
  (fun () -> Wp_parameters.debug ~dkey:job_key "Stop WP plugin...@.")

let () = Db.Main.extend main

(*
Local Variables:
compile-command: "make -C ../.."
End:
*)

