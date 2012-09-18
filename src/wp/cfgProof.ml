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

open LogicUsage

let dkey = "cfgproof" (* debugging key *)

module type Description =
sig

  val shared : string   (** Shared resource basename for the model *)
  val context : string  (** Basename for environment files ; unique per updater *)
  val updater : string  (** Unique name for the internal updater *)
  val name : string     (** Public name for user feedback *)

end

module Create
  (WpModel:Mwp.S)
  (Why:Mcfg.Export  with type pred = WpModel.F.pred and type decl = WpModel.F.decl)
  (Coq:Mcfg.Export with type pred = WpModel.F.pred and type decl = WpModel.F.decl)
  (Ergo:Mcfg.Export with type pred = WpModel.F.pred and type decl = WpModel.F.decl)
  (Splitter:Mcfg.Splitter with type pred = WpModel.F.pred)
  (Descr:Description)
  =
struct

  module Me = State_builder.Ref
    (Datatype.Unit)
    (struct
       let dependencies = [Ast.self]
       let name = "WP-" ^ Descr.updater
       let default () = ()
     end)

  module F = WpModel.F
  module L = WpModel.L
  module VC =
    CfgWeakestPrecondition.Create
      (struct
         include WpModel
         let model = Descr.updater
       end)
  module PO = Cfgpropid.Create(VC)
  module WP = Calculus.Cfg(PO)

  let emitter =
    Emitter.create
      Me.name 
      [ Emitter.Property_status ]
      ~correctness:[] 
      ~tuning:[ Wp_parameters.Provers.parameter ]

  (* ------------------------------------------------------------------------ *)
  (* --- Proof Obligations : Export to WHY                                --- *)
  (* ------------------------------------------------------------------------ *)

  type exportation = {
    envkey : string ;
    mutable goals : Wpo.t list ;
  }

  (* ------------------------------------------------------------------------ *)
  (* --- Goal Registering                                                 --- *)
  (* ------------------------------------------------------------------------ *)

  open Wpo
 
  let active_lgg () =
    if !Config.is_gui || Wp_parameters.debug_atleast 2 then
      [ VCS.L_why ; VCS.L_coq ; VCS.L_altergo ]
    else
      let lgs = ref [] in
      let add f x =
	match f x with None -> () | Some lg -> 
	  if not (List.mem lg !lgs) then lgs := lg :: !lgs in
      begin
	List.iter (add VCS.language_of_prover_name) (Wp_parameters.get_provers()) ;
	add VCS.language_of_name (Wp_parameters.Check.get ()) ; !lgs
      end

  let export_wpo export index wrn dep propid gpred =
    let gname = WpPropId.get_propid propid in
    let context = Wpo.kf_context index in
    let gid = Wpo.gid ~model:Descr.context ~propid in
    (* --- HEADER --- *)
    Wp_parameters.debug ~dkey "Export PO %s " gname;
    Command.pp_to_file (VC_Legacy.file_for_head ~gid)
      (fun fmt ->
         Format.fprintf fmt "@[<v 0>Proof Obligation %s:@]@\n" gname ;
         Format.fprintf fmt "Environment: %s@\n" export.envkey ;
         Wpo.pp_dependencies context fmt dep ;
	 Wpo.pp_warnings fmt wrn ;
      ) ;
    (* --- BODY --- *)
    Command.pp_to_file (VC_Legacy.file_for_body ~gid)
      (fun fmt ->
	 Format.fprintf fmt "@[<v 2>Goal %s:@ %a@]" gid F.pp_pred gpred ;
      ) ;
    (* --- WHY Others --- *)
    let export_lgg l =
      Command.pp_to_file (VC_Legacy.file_for_goal ~gid l)
	(fun fmt ->
           match l with
             | VCS.L_why -> Why.export_goal fmt gid gpred
             | VCS.L_coq -> Coq.export_goal fmt gid gpred
             | VCS.L_altergo -> Ergo.export_goal fmt gid gpred
	) ;
    in
    List.iter export_lgg (active_lgg ()) ;
    
    (* --- Warnings --- *)
    if wrn <> [] then
      begin
        let pp_warnings fmt ws =
          let n = List.length ws in if n = 1
          then Format.pp_print_string fmt "1 warning"
          else Format.fprintf fmt "%d warnings" n
        in
        let degenerated = List.exists (fun w -> w.Warning.wrn_severe) wrn in
        if not (Wp_parameters.Details.get ()) then
          Wp_parameters.warning ~current:false ~once:true
            "Use -wp-warnings for details about 'Stronger' and 'Degenerated' goals" ;
        Wp_parameters.warning ~current:false
          "%s goal %s (%a)"
          (if degenerated then "Degenerated" else "Stronger")
          gid pp_warnings wrn ;
        if Wp_parameters.Details.get () then
          List.iter
            (fun w ->
               Log.print_on_output 
		 (fun fmt -> Warning.pretty fmt w ; Format.pp_print_newline fmt ())
            ) wrn ;
      end ;
    
    (* --- WPO --- *)
    let wpo = {
      po_idx    = index ;
      po_name   = gname ;
      po_gid    = gid ;
      po_pid    = propid ;
      po_updater = emitter ;
      po_formula = Legacy {
	VC_Legacy.mid = Descr.shared ;
	VC_Legacy.env = export.envkey ;
	VC_Legacy.wrn = wrn ;
	VC_Legacy.dep = dep ;
      } ;
    } in
    Wpo.add wpo ;
    if F.is_true gpred then Wpo.set_result wpo VCS.WP VCS.valid ;
    export.goals <- wpo :: export.goals

  (* ------------------------------------------------------------------------ *)
  (* --- Goal Splitting                                                   --- *)
  (* ------------------------------------------------------------------------ *)

  let build_wpos export index wrn dep propid gpred =
    let gpred = Splitter.simplify gpred in
    if Wp_parameters.Split.get () 
      || Wp_parameters.Invariants.get () 
      || WpPropId.is_assigns propid
    then
      let goals = Splitter.split (WpPropId.is_assigns propid) gpred in
      if Bag.is_empty goals then
        export_wpo export index wrn dep propid F.p_true
      else
        WpAnnot.split (export_wpo export index wrn dep) propid goals
    else
      export_wpo export index wrn dep propid gpred

  let add_goal export index po =
    let gpred = VC.zip po.PO.g_prop in
    let wrn = ref [] in
    let dep = ref [] in
    PO.iter_description
      (fun w -> wrn := w :: !wrn)
      (fun d -> dep := d :: !dep)
      po.PO.g_descr ;
    build_wpos export index (List.rev !wrn) (List.rev !dep) po.PO.g_id gpred

  (* ------------------------------------------------------------------------ *)
  (* --- Proof Obligation Generation                                      --- *)
  (* ------------------------------------------------------------------------ *)

  class computer =
  object

    val mutable wptasks = []

    method lemma = false

    method add_strategy strategy =
      wptasks <- strategy :: wptasks

    method add_lemma (thm : logic_lemma) =
      Wp_parameters.warning ~once:true ~source:thm.lem_position
        "Proof obligation for lemma '%s' not generated." thm.lem_name

    method compute =
      begin

        Wp_error.set_model Descr.name ;
        F.clear () ;
        let env = Wpo.new_env ~context:Descr.context in
        let export = { envkey=env ; goals=[] } in

        (* Generates Wpos and accumulate exported goals *)
        List.iter
          (fun strategy ->
	     let cfg = WpStrategy.cfg_of_strategy strategy in
	     let kf = Cil2cfg.cfg_kf cfg in
	     let names = WpAnnot.missing_rte kf in
	     if names <> [] then
	       Wp_parameters.warning ~current:false ~once:true
                 "Missing RTE guards" ;
	     !Db.progress ();
	     let bhv = WpStrategy.behavior_name_of_strategy strategy in
	     let goals,annotations = WP.compute cfg strategy in
	     if Wp_parameters.Dot.get () then
	       ignore (Cil2cfg.dot_wp_res cfg Descr.shared annotations) ;
	     let index = Wpo.Function(kf,bhv) in
	     List.iter (List.iter (add_goal export index)) goals
	  ) wptasks ;
	wptasks <- [] ;

        if export.goals <> [] then
          begin

            (* --- Env Description --- *)
            let ctxt = VC_Legacy.file_for_ctxt ~env in
            Wp_parameters.debug ~dkey "DO ENV DESCP %s" ctxt ;
            Command.pp_to_file ctxt
              (fun fmt -> F.iter_all (F.pp_section fmt) (F.pp_decl fmt)) ;
             Wp_parameters.debug ~dkey "DONE ENV DESCP %s" ctxt ;

            let export_env_lgg l =
              Wp_parameters.debug ~dkey "DO export env for %a" VCS.pp_language l ;
              Command.pp_to_file (VC_Legacy.file_for_env ~env l)
                (fun fmt ->
                   match l with
                     | VCS.L_coq -> Format.fprintf fmt
                         "Require Import Reals.@\n\
                          Require Import wp.@\n\
                          Require Import %s.@\n"
                           (VC_Legacy.coq_for_model Descr.shared) ;
                         F.iter_all (Coq.export_section fmt) (Coq.export_decl fmt)
                     | VCS.L_why ->
                         F.iter_all (Why.export_section fmt) (Why.export_decl fmt)
                     | VCS.L_altergo -> 
			 F.iter_all (Ergo.export_section fmt) (Ergo.export_decl fmt)
                )
            in
	    List.iter export_env_lgg (active_lgg()) ;
	    
          end
        else
          Wpo.release_env env ;

        (* --- Generated Goals --- *)
	wptasks <- [] ;
        Bag.list export.goals

      end (* method compute *)

  end (* class computer *)

  let create () = (new computer :> Generator.computer)

end

(*
Local Variables:
compile-command: "make -C ../.."
End:
*)
