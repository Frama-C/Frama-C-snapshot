(**************************************************************************)
(*                                                                        *)
(*  This file is part of WP plug-in of Frama-C.                           *)
(*                                                                        *)
(*  Copyright (C) 2007-2011                                               *)
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

(*
module FQed = Fol_qed.F
module DQed = Datalib.Create(FQed)
module AQed = Mint_natural.Create(DQed)
module RQed = Mfloat_natural.Create(DQed)
module EQed = Fol_qed.E
*)

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

(* --------- WP Dispatcher ------------------ *)

type feature = NA | Yes | No

type wp_model = {
  wp_name : string ;
  wp_qed : feature ;
  wp_logicvar : feature ;
  wp_method : unit -> Mcfg.assigns_method ;
  wp_computer : unit -> CfgProof.computer ;
  wp_altmodel : (unit -> CfgProof.computer) option ;
}

let wp_model name = {
  wp_name = name ;
  wp_qed = NA ;
  wp_logicvar = NA ;
  wp_method = (fun () -> Wp_parameters.fatal "no method implemented") ;
  wp_computer = (fun () -> Wp_parameters.fatal "computer not implemented") ;
  wp_altmodel = None ;
}

let option opt = function
  | NA -> true
  | Yes -> opt ()
  | No -> not (opt ())

type 'a computing_methods =
  | OneforBoth of 'a
  | NonAssigns of 'a
  | OneforEach of 'a * 'a (*non assigns, assigns only*)

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
             (fun () -> false) (*Wp_parameters.Qed.get*) , m.wp_qed ;
             Wp_parameters.LogicVar.get , m.wp_logicvar ;
           ]
      ) models in
    let computer = model.wp_computer () in
    match model.wp_method () , model.wp_altmodel with
      | Mcfg.NoAssigns , None -> NonAssigns computer
      | Mcfg.NoAssigns , Some alt -> OneforEach( computer , alt () )
      | _ -> OneforBoth computer
  with Not_found ->
    Wp_parameters.abort "No model found with provided criteria"

(* --------- WP Computer -------------------- *)

(*
  computer returns :
  - either one computer for both assigns and non-assigns
  - or a unique computer for only non-assigns
  - or two computers, one for assigns and one for non-assigns
*)

let computer = function

    | Wp_parameters.M_Hoare  ->
        NonAssigns (WP_Hoare.create ())
    | Wp_parameters.M_Store ->
        dispatch [
          { (wp_model "Store") with
              wp_logicvar = Yes ;
              wp_method = WP_Storefun.assigns_method ;
              wp_computer = WP_Storefun.create ;
              wp_altmodel = Some WP_Store.create ;
          } ;
          { (wp_model "Store") with
              wp_logicvar = No ;
              wp_method = WP_Store.assigns_method ;
              wp_computer = WP_Store.create ;
          }
        ]

    | Wp_parameters.M_Runtime ->
        dispatch [
          { (wp_model "Runtime") with
              wp_logicvar = Yes ;
              wp_method = WP_Runtimefun.assigns_method ;
              wp_computer = WP_Runtimefun.create ;
              wp_altmodel = Some WP_Runtime.create ;
          } ;
          { (wp_model "Runtime") with
              wp_logicvar = No ;
              wp_method = WP_Runtime.assigns_method ;
              wp_computer = WP_Runtime.create ;
          }
        ]

(* ------------------------------------------------------------------------ *)
(* --- Iterators                                                        --- *)
(* ------------------------------------------------------------------------ *)

(* TODO: clean that because we now do the same thing on functions with or
* without definition...
*)

let on_definition phi kf =
  match kf.Cil_types.fundec with
    | Cil_types.Declaration _ (* ->
        Wp_parameters.warning ~current:false "Function %s has no body (skipped)"
          (Kernel_function.get_name kf) *)
    | Cil_types.Definition _ -> phi kf

let on_all_functions = fun do_body ->
  Globals.Functions.iter (fun kf ->
                            !Db.progress ();
                            (*match kf.fundec with
                              | Declaration _ -> ()
                              | Definition _ -> *)
                                  on_definition do_body kf)

let on_function_names fcts = fun do_body ->
  List.iter
    (fun fname ->
       try
         let kf = Globals.Functions.find_by_name fname in
         on_definition do_body kf
       with Not_found ->
         Wp_parameters.error "Unknown function '%s' (skipped)" fname
    ) fcts

let on_function kf = fun do_body -> on_definition do_body kf

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

type prop =
  | NamedProp of string
  | IdProp of Property.t

let do_compute
    (goals : Wpo.t Bag.t ref)
    (computer : CfgProof.computer)
    fun_iter assigns behaviors property call_stmt_opt
    =
  let do_kf kf =
    !Db.progress () ;
    let annots = match behaviors, property with
      | [], None ->
          let s = match call_stmt_opt with
            | Some stmt -> WpAnnot.get_call_pre_strategies stmt
            | None -> 
                if Wp_parameters.Froms.get () 
                then  WpFroms.get_strategies_for_froms kf
                else WpAnnot.get_function_strategies ~assigns kf
          in s
      | (_ :: _) as bhvs, None ->
        WpAnnot.get_behavior_strategies ~assigns kf bhvs
      | _, Some (IdProp p) ->
        WpAnnot.get_id_prop_strategies ~assigns p
      | _, Some (NamedProp p) ->
        WpAnnot.get_prop_strategies ~assigns kf (behaviors, p)
    in
    if Wp_parameters.Dot.get () then dot_lannots annots;
    computer#add annots
  in
  begin
    fun_iter do_kf ;
    goals := Bag.concat !goals (Bag.list computer#compute) ;
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
             ~on_environment:(Wpo.pp_environment fmt)
             ~on_behavior:(Wpo.pp_function fmt)
             ~on_goal:(Wpo.pp_goal_flow fmt) ())

let do_wp_print_for goals =
  if Wp_parameters.Print.get () then
    if Bag.is_empty goals
    then Wp_parameters.result "No proof obligations"
    else Log.print_on_output
      (fun fmt -> Bag.iter (Wpo.pp_goal_flow fmt) goals)

(* ------------------------------------------------------------------------ *)
(* ---  Proving                                                         --- *)
(* ------------------------------------------------------------------------ *)

let do_wpo_feedback g prover result =
  Wp_parameters.feedback "[%a] Goal %s : %a"
    Wpo.pp_prover prover g.Wpo.po_gid Wpo.pp_result result

let do_wp_proof server interactive prover g =
  let already_valid (_,r) = r=Wpo.Valid in
  if not (List.exists already_valid (Wpo.get_results g))
  then begin
    Task.spawn server
    (Prover.prove ~callout:do_wpo_feedback g ~interactive prover)
  end
let do_wp_proofs () =
  let pname = Wp_parameters.Prover.get () in
  match Wpo.prover_of_name pname with
    | None -> ()
    | Some prover ->
        let interactive = Wpo.is_interactive pname in
        let server = Prover.server () in
        try
          Wpo.iter ~on_goal:(do_wp_proof server interactive prover) () ;
          Task.launch server ;
        with e ->
          Task.cancel_all server ;
          raise e

let do_wp_proofs_for goals =
  let pname = Wp_parameters.Prover.get () in
  match Wpo.prover_of_name pname with
    | None -> ()
    | Some prover ->
        let interactive = Wpo.is_interactive pname in
        let server = Prover.server () in
        try
          Bag.iter (do_wp_proof server interactive prover) goals ;
          Task.launch server ;
        with e ->
          Task.cancel_all server ;
          raise e

(* ------------------------------------------------------------------------ *)
(* ---  Type Checking prover's inputs                                   --- *)
(* ------------------------------------------------------------------------ *)

let do_check_feedback g lang result =
  Wp_parameters.feedback "[%a] Goal %s : %a"
    Wpo.pp_language lang g.Wpo.po_gid Wpo.pp_result result

let do_wp_check server lang g =
 Task.spawn server (Prover.check ~callout:do_check_feedback g lang)

let do_wp_checks () =
  match Wpo.language_of_name (Wp_parameters.Check.get ()) with
    | None -> ()
    | Some lang ->
        let server = Prover.server () in
        try
          Wpo.iter ~on_goal:(do_wp_check server lang) () ;
          Task.launch server ;
        with e ->
          Task.cancel_all server ;
          raise e

let do_wp_checks_for goals =
  match Wpo.language_of_name (Wp_parameters.Check.get ()) with
    | None -> ()
    | Some lang ->
        let server = Prover.server () in
        try
          Bag.iter (do_wp_check server lang) goals ;
          Task.launch server ;
        with e ->
          Task.cancel_all server ;
          raise e

(* ------------------------------------------------------------------------ *)
(* ---  Filtering WP passes                                             --- *)
(* ------------------------------------------------------------------------ *)

let do_wp_passes fun_iter behaviors property call_stmt_opt =
  let model = Wp_parameters.get_model () in
  let goals = ref Bag.empty in
  let wp_pass computer assigns =
    do_compute goals computer fun_iter assigns behaviors property call_stmt_opt
  in
  begin
    match computer model with
      | NonAssigns c ->
          Wp_parameters.warning
            ~current:false ~once:true
            "Ignored Assigns-Goals with '%s' model"
            (Wp_parameters.Model.get ()) ;
          wp_pass c WpAnnot.NoAssigns
      | OneforBoth c ->
          wp_pass c WpAnnot.WithAssigns
      | OneforEach(c1,c2) ->
	match property with
	| None
	| Some
	    (NamedProp _ | IdProp(Property.IPOther _ 
				     | Property.IPBehavior _
				     | (* [JS 2011/08/05] I put this case here
					  but not sure of that *) 
					 Property.IPUnreachable _))
	  ->
	  begin
	    wp_pass c1 WpAnnot.NoAssigns ;
	    wp_pass c2 WpAnnot.OnlyAssigns ;
	  end
	| Some(IdProp(Property.IPPredicate _ 
			 | Property.IPAxiom _ | Property.IPLemma _
			 | Property.IPAxiomatic _
			 | Property.IPCodeAnnot _ | Property.IPComplete _
			 | Property.IPDisjoint _ | Property.IPDecrease _ )) ->
	  wp_pass c1 WpAnnot.NoAssigns
	| Some (IdProp(Property.IPAssigns _ | Property.IPFrom _)) ->
	  wp_pass c2 WpAnnot.OnlyAssigns
  end; 
  !goals

let generic_compute kf_opt behaviors p call_stmt =
  let fun_iter =
    match kf_opt with
      | Some kf -> on_function kf
      | None -> on_all_functions
  in
  let goals = do_wp_passes fun_iter behaviors p call_stmt in
  if not (Bag.is_empty goals) then do_wp_proofs_for goals

(* ------------------------------------------------------------------------ *)
(* ---  Secondary Entry Points                                          --- *)
(* ------------------------------------------------------------------------ *)

(* Registered entry point in Dynamic. *)

let wp_compute kf_opt behaviors property =
  let p = match property with None -> None | Some p -> Some (IdProp p) in
    generic_compute kf_opt behaviors p None

let wp_compute_call ~kf_caller ~kf_called stmt =
  ignore kf_called ;
  generic_compute (Some kf_caller) [] None (Some stmt)

(* ------------------------------------------------------------------------ *)
(* ---  Command-line Entry Points                                       --- *)
(* ------------------------------------------------------------------------ *)

let cmdline_run () =
  let wp_main kf_list =
    Ast.compute ();
    Variables_analysis.precondition_compute ();
    let fun_iter = match kf_list with
      | [] -> on_all_functions
      | names -> on_function_names names
    in
    let bhvs = Wp_parameters.Behaviors.get () in
    let property =
      match Wp_parameters.Properties.get () with
        | [] -> None
        | [ p ] -> Some (NamedProp p)
        | ps ->
          Wp_parameters.not_yet_implemented 
	    "several properties (%a) in -wp-prop"
	    (Pretty_utils.pp_list ~sep:"," Format.pp_print_string) ps
    in
    do_wp_passes fun_iter bhvs property None
  in
  match Wp_parameters.job () with
    | Wp_parameters.WP_None -> ()
    | Wp_parameters.WP_All ->
        ignore (wp_main []);
        do_wp_checks ();
        do_wp_proofs ();
        do_wp_print ()
    | Wp_parameters.WP_Select fcts ->
        let goals = wp_main fcts in
        do_wp_checks_for goals ;
        do_wp_proofs_for goals ;
        do_wp_print_for goals

(* ------------------------------------------------------------------------ *)
(* ---  Register external functions                                     --- *)
(* ------------------------------------------------------------------------ *)

let property_of_id =
  Dynamic.register ~plugin:"Wp" "WpAnnot.property_of_id" ~journalize:false
    (Datatype.func WpPropId.Prop_id_datatype.ty Property.ty)
    WpPropId.property_of_id

let wp_compute =
  let module OLS = Datatype.List(Datatype.String) in
  let module OKF = Datatype.Option(Kernel_function) in
  let module OP = Datatype.Option(Property) in
  Dynamic.register ~plugin:"Wp" "wp_compute"
    (Datatype.func3 OKF.ty OLS.ty OP.ty Datatype.unit)
    ~journalize:false (*LC: Because of Property is not journalizable. *)
    wp_compute

(** Use options to know what to do *)
let run = Dynamic.register ~plugin:"Wp" "run"
  (Datatype.func Datatype.unit Datatype.unit)
  ~journalize:true
  cmdline_run

(* ------------------------------------------------------------------------ *)
(* ---  Main Entry Point                                                --- *)
(* ------------------------------------------------------------------------ *)

let do_finally job1 job2 () = 
  let r1 = try job1 () ; None with error -> Some error in
  let r2 = try job2 () ; None with error -> Some error in
  match r1 , r2 with
    | None , None -> ()
    | Some e1 , None -> raise e1
    | _ , Some e2 -> raise e2

let tracelog () =
  let ks = Wp_parameters.get_debug_keyset () in
  if ks <> [] then
    let pp_keys : Format.formatter -> string list -> unit = 
      Pretty_utils.pp_flowlist ~left:"" ~sep:"," ~right:"." Format.pp_print_string
    in Wp_parameters.debug ~level:0 "Logging keys : %a" pp_keys ks

let (&&&) = do_finally
  
let main = cmdline_run &&& tracelog &&& Wp_parameters.reset

let () = Db.Main.extend main

(*
Local Variables:
compile-command: "make -C ../.."
End:
*)

