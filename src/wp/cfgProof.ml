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

open Cil_types
open Cfgpropid

let dkey = "cfgproof" (* debugging key *)

class type computer =
object

  method add : WpStrategy.strategy list -> unit
  method compute : Wpo.t list

end

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
       let kind = `Correctness
       let default () = ()
     end)

  module F = WpModel.F
  module L = WpModel.L
  module CV =
    CfgWeakestPrecondition.Create
      (struct
         include WpModel
         let model = Descr.updater
       end)
  module PO = Cfgpropid.Create(CV)
  module WP = Calculus.Cfg(PO)

  (* ------------------------------------------------------------------------ *)
  (* --- Proof Obligations : Export to WHY                                --- *)
  (* ------------------------------------------------------------------------ *)

  type exportation = {
    env : string ;
    mutable goals : Wpo.t list ;
  }

  let assigns_method = PO.assigns_method

  (* ------------------------------------------------------------------------ *)
  (* --- Goal Registering                                                 --- *)
  (* ------------------------------------------------------------------------ *)

  let active_lgg () =
    match Wpo.language_of_prover_name (Wp_parameters.Prover.get ()) with
      | None ->
          (match Wpo.language_of_name (Wp_parameters.Check.get ()) with
             | None -> None
             | Some l -> Some l
          )
      | Some l -> Some l

  let export_wpo export kf bhv wrn dep propid gpred =
    let gname = WpPropId.prop_id_name propid in
    let gid = Wpo.gid ~context:Descr.context ~kf ~bhv ~propid in
    (* --- HEADER --- *)
    Wp_parameters.debug ~dkey "Export PO %s " gname;
    let fhead = Wpo.file_for_head ~gid in
     Wp_parameters.debug ~dkey "DO HEADER in %s" fhead ;
    Command.pp_to_file fhead
      (fun fmt ->
         Format.fprintf fmt "@[<v 0>Proof Obligation %s:@]@\n" gname ;
         Format.fprintf fmt "Environment: %s@\n" export.env ;
         List.iter (fun d -> Format.fprintf fmt "%a@\n" (Wpo.pp_dependency kf) d) dep ;
         List.iter (fun w -> Format.fprintf fmt "%a@\n" Wpo.pp_warning w) wrn ;
      ) ;
    Wp_parameters.debug ~dkey "DONE HEADER in %s" fhead;
    (* --- BODY --- *)
    let fbody = Wpo.file_for_body ~gid in
     Wp_parameters.debug ~dkey "DO BODY in %s" fbody ;
    Command.pp_to_file fbody
      (fun fmt ->
	 Format.fprintf fmt "@[<v 2>Goal %s:@ %a@]" gid F.pp_pred gpred ;
      ) ;
    Wp_parameters.debug ~dkey "DONE BODY in %s" fbody ;
    (* --- WHY Others --- *)
    let export_lgg l =
      Wp_parameters.debug ~dkey "DO export goal in language %a"
        Wpo.pp_language l;
    Command.pp_to_file (Wpo.file_for_goal ~gid l )
      (fun fmt ->
         match l with
           | Wpo.L_why -> Why.export_goal fmt gid gpred
           | Wpo.L_coq -> Coq.export_goal fmt gid gpred
           | Wpo.L_altergo -> Ergo.export_goal fmt gid gpred
      ) ;
    Wp_parameters.debug ~dkey "DONE export goal in language %a"
      Wpo.pp_language l;
    in

    let export_all () =
      export_lgg Wpo.L_why ;
      export_lgg Wpo.L_coq ;
      export_lgg Wpo.L_altergo;
    in
    (
      if !Config.is_gui || (Wp_parameters.debug_atleast 2) then
        (Wp_parameters.debug ~dkey
          "Into gui config, goal has to be produce in all languages";
        export_all () )
      else
        (match active_lgg () with
          | None -> ()
          | Some l ->export_lgg l
        )
    );

    (* --- Warnings --- *)
    if wrn <> [] then
      begin
        let pp_warnings fmt ws =
          let n = List.length ws in if n = 1
          then Format.pp_print_string fmt "1 warning"
          else Format.fprintf fmt "%d warnings" n
        in
        let degenerated = List.exists (fun w -> w.Wpo.wrn_severe) wrn in
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
		 (fun fmt -> Wpo.pp_warning fmt w ; Format.pp_print_newline fmt ())
            ) wrn ;
      end ;

    let emitter =
      Emitter.create Me.name 
        ~correctness:[] ~tuning:[ Wp_parameters.Prover.parameter ]
    in
	
    (* --- WPO --- *)
    let wpo = {
      Wpo.po_fun    = kf ;
      Wpo.po_bhv    = bhv ;
      Wpo.po_name   = gname ;
      Wpo.po_gid    = gid ;
      Wpo.po_model  = Descr.shared ;
      Wpo.po_env    = export.env ;
      Wpo.po_pid    = propid ;
      Wpo.po_dep    = dep ;
      Wpo.po_warn   = wrn ;
      Wpo.po_updater = emitter;
    } 
    in
    Wpo.add wpo ;
    if F.is_true gpred then
      Wpo.set_result wpo Wpo.WP Wpo.Valid ;
    export.goals <- wpo :: export.goals

  (* ------------------------------------------------------------------------ *)
  (* --- Goal Splitting                                                   --- *)
  (* ------------------------------------------------------------------------ *)

  let build_wpos export kf bhv wrn dep propid gpred =
    let gpred = Splitter.simplify gpred in
    if Wp_parameters.Split.get () || Wp_parameters.Invariants.get () ||
      (WpPropId.is_assigns propid && PO.assigns_method() = Mcfg.EffectAssigns)
    then
      let goals = Splitter.split (PO.assigns_method ()) gpred in
      if Bag.is_empty goals then
        export_wpo export kf bhv wrn dep propid F.p_true
      else
        WpAnnot.split (export_wpo export kf bhv wrn dep) propid goals
    else
      export_wpo export kf bhv wrn dep propid gpred

  let add_goal export kf bhv po =
    let gpred = CV.zip po.PO.g_prop in
    let wrn = ref [] in
    let dep = ref [] in
    PO.iter_description
      (fun w -> wrn := w :: !wrn)
      (fun d -> dep := d :: !dep)
      po.PO.g_descr ;
    build_wpos export kf bhv (List.rev !wrn) (List.rev !dep) po.PO.g_id gpred

  (* ------------------------------------------------------------------------ *)
  (* --- Proof Obilgation Generation                                      --- *)
  (* ------------------------------------------------------------------------ *)

  class computer =
  object

    val mutable wptasks = []
    val mutable exported = None

    method add strategies =
      wptasks <- strategies :: wptasks

    method compute =
      begin

        exported <- None ;
        Wp_error.set_model Descr.name ;
        F.clear () ;
        let env = Wpo.new_env ~context:Descr.context in
        let export = { env=env ; goals=[] } in

        (* Generates Wpos and accumulate exported goals *)
        List.iter
          (fun (strategies) ->
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
                  List.iter
                    (List.iter
                       (add_goal export kf bhv))
                    goals
               ) strategies
          ) wptasks ;

        if export.goals <> [] then
          begin

            (* --- Env Description --- *)
            let ctxt = Wpo.file_for_ctxt ~env in
            Wp_parameters.debug ~dkey "DO ENV DESCP %s" ctxt ;
            Command.pp_to_file ctxt
              (fun fmt -> F.iter_all (F.pp_section fmt) (F.pp_decl fmt)) ;
             Wp_parameters.debug ~dkey "DONE ENV DESCP %s" ctxt ;

            let export_env_lgg l =
              Wp_parameters.debug ~dkey "DO export env for %a" Wpo.pp_language l ;
              Command.pp_to_file (Wpo.file_for_env ~env l)
                (fun fmt ->
                   match l with
                     | Wpo.L_coq -> Format.fprintf fmt
                         "Require Import Reals.@\n\
                          Require Import wp.@\n\
                          Require Import %s.@\n"
                           (Wpo.coq_for_model Descr.shared) ;
                         F.iter_all (Coq.export_section fmt) (Coq.export_decl fmt)
                     | Wpo.L_why ->
                         F.iter_all (Why.export_section fmt) (Why.export_decl fmt)
                     | Wpo.L_altergo -> 
			 F.iter_all (Ergo.export_section fmt) (Ergo.export_decl fmt)
                )
            in
            if !Config.is_gui || Wp_parameters.debug_atleast 2 then
              begin
                export_env_lgg Wpo.L_why ;
                export_env_lgg Wpo.L_coq ;
                export_env_lgg Wpo.L_altergo ;
              end
            else
              begin
                match active_lgg () with
                  | None -> ()
                  | Some l -> export_env_lgg l
              end

          end
        else
          Wpo.release_env env ;

        (* --- Generated Goals --- *)
        export.goals

      end (* method compute *)

  end (* class computer *)

  let create () = (new computer :> computer)

end

(*
Local Variables:
compile-command: "make -C ../.."
End:
*)
