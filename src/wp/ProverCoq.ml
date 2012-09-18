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

(* -------------------------------------------------------------------------- *)
(* --- Prover Coq Interface                                               --- *)
(* -------------------------------------------------------------------------- *)

open Cil_types
open Qed
open Lang
open Definitions

let cluster_file c = 
  let dir = Model.directory () in
  let base = cluster_id c in
  Printf.sprintf "%s/%s.v" dir base

let _shared f = Wp_parameters.Share.file ~error:true f

let user_path f =
  if Sys.file_exists f then Some f else
    if Filename.is_relative f then
      let rec lookup f = function
	| [] -> None
	| d::ds ->
	    let path = Printf.sprintf "%s/%s" d f in
	    if Sys.file_exists path then Some path else lookup f ds
      in lookup f (Wp_parameters.Includes.get ())
    else None

let user_module f = Filename.chop_extension (Filename.basename f)

(* -------------------------------------------------------------------------- *)
(* --- Exporting Formulae to Coq                                          --- *)
(* -------------------------------------------------------------------------- *)

type depend =
  | D_theory of string   (* They are copied in <out>/wp/*.v from <share>/wp *)
  | D_cluster of cluster (* They are generated in <out>/<model>/*.v *)
  | D_userlib of string     (* They are copied    as <out>/<model>/*.v or *.vo *)

let engine = 
  let module E = Qed.Export_coq.Make(Lang.F) in
object
  inherit E.engine
  method datatype = ADT.id
  method field = Field.id
  method link = Lang.link
end

class visitor fmt c =
object(self)
  
  inherit Definitions.visitor c 
  inherit ProverTask.printer fmt (cluster_title c)

  val mutable deps = []

  (* --- Managing Formatter --- *)

  method flush =
    begin
      Format.pp_print_newline fmt () ;
      List.rev deps
    end

  (* --- Files, Theories and Clusters --- *)

  method add_theory f = 
    self#lines ;
    Format.fprintf fmt "Require Import %s.@\n" f ;
    deps <- (D_theory f) :: deps

  method add_userlib f =
    self#lines ;
    match user_path f with
      | Some path ->
	  Format.fprintf fmt "Require Import %s.@\n" (user_module f) ;
	  deps <- (D_userlib path) :: deps
      | None ->
	  Format.fprintf fmt "(* User lib '%s' not found *)@\n" f
	  
  method on_cluster c = 
    self#lines ;
    Format.fprintf fmt "Require Import %s.@\n" (cluster_id c) ;
    deps <- (D_cluster c) :: deps

  method on_theory = function
    | "qed"    -> ()
    | "cint"   -> self#add_theory "Cint"
    | "cfloat" -> self#add_theory "Cfloat"
    | "vset"   -> self#add_theory "Vset"
    | "memory" -> self#add_theory "Memory"
    | "cmath" -> self#add_theory "Cmath"
    | thy -> Wp_parameters.fatal ~current:false 
	"Unregistered theory '%s' for Coq" thy

  method on_type lt def =
    begin
      self#lines ;
      engine#declare_type fmt (Lang.atype lt) (List.length lt.lt_params) def ;
    end

  method on_comp c fts =
    begin
      (*TODO:NUPW: manage UNIONS *)
      self#paragraph ;
      engine#declare_type fmt (Lang.comp c) 0 (Qed.Engine.Trec fts) ;
    end

  method on_dlemma l =
    begin
      self#paragraph ;
      engine#declare_axiom fmt 
	(Lang.lemma_id l.l_name) 
	l.l_forall l.l_triggers
	(F.e_prop l.l_lemma)
    end

  method on_dfun d =
    begin
      self#paragraph ;
      match d.d_definition with
	| Logic t ->
	    engine#declare_signature fmt 
	      d.d_lfun (List.map F.tau_of_var d.d_params) t ;
	| Value(t,_,v) ->
	    engine#declare_definition fmt 
	      d.d_lfun d.d_params t v
	| Predicate(_,p) ->
	    engine#declare_definition fmt 
	      d.d_lfun d.d_params Logic.Prop (F.e_prop p)
	| Inductive cases -> 
	    engine#declare_signature fmt
	      d.d_lfun (List.map F.tau_of_var d.d_params) Logic.Prop ;
	    List.iter self#on_dlemma cases
    end

end

let write_cluster c =
  let f = cluster_file c in
  Wp_parameters.debug ~dkey:"prover" "Generate '%s'" f ;
  Command.print_file f
    begin fun fmt ->
      let v = new visitor fmt c in
      v#lines ;
      v#printf "Require Import ZArith.@\n" ;
      v#printf "Require Import Reals.@\n" ;
      v#add_theory "Qedlib" ;
      v#vself ;
      v#flush ;
    end

let need_recompile ~source ~target =
  try
    let t_src = (Unix.stat source).Unix.st_mtime in
    let t_tgt = (Unix.stat target).Unix.st_mtime in
    t_src >= t_tgt
  with Unix.Unix_error _ -> true
  
(* -------------------------------------------------------------------------- *)
(* --- Assembling Goal                                                    --- *)
(* -------------------------------------------------------------------------- *)

let theories = ref [] (*[LC] Shared : not projectified. *)

module CLUSTERS = Model.Index
  (struct
     type key = cluster
     type data = int * depend list
     let name = "ProverCoq.FILES"
     let compare = cluster_compare
     let pretty = pp_cluster
   end)

type coqcc = {
  mutable sources : string list ; (* in reverse order *)
}

let add_source coqcc file =
  if not (List.mem file coqcc.sources) then coqcc.sources <- file :: coqcc.sources

let rec assemble coqcc = function
  | D_theory thy -> assemble_theory coqcc thy
  | D_cluster c -> assemble_cluster coqcc c
  | D_userlib path -> assemble_userlib coqcc path

and assemble_cluster coqcc c =
  let (age,deps) = try CLUSTERS.find c with Not_found -> (-1,[]) in
  let deps = 
    if age < cluster_age c then
      let deps = write_cluster c in
      CLUSTERS.update c (cluster_age c , deps) ; deps
    else deps in
  List.iter (assemble coqcc) deps ;
  add_source coqcc (cluster_file c)

and assemble_theory coqcc thy =
  let tgtdir = Wp_parameters.get_output_dir "wp" in
  let target = Printf.sprintf "%s/%s.v" tgtdir thy in
  if not (List.mem thy !theories) then
    begin
      let source = Wp_parameters.Share.file ~error:true (thy ^ ".v") in
      Command.copy source target ;
      theories := thy :: !theories ;
    end ;
  add_source coqcc target

and assemble_userlib coqcc source =
  let tgtdir = Model.directory () in
  let target = Printf.sprintf "%s/%s" tgtdir (Filename.basename source) in
  if need_recompile ~source ~target then Command.copy source target ;
  add_source coqcc target

(* -------------------------------------------------------------------------- *)
(* --- Assembling Goal                                                    --- *)
(* -------------------------------------------------------------------------- *)

let assemble_goal ~pid axioms prop =
  let title = Pretty_utils.to_string WpPropId.pretty pid in
  let wpd = Wp_parameters.get_output_dir "wp" in
  let dir = Model.directory () in
  let id = WpPropId.get_propid pid in
  let file = Printf.sprintf "%s/%s.coq" dir id in
  let goal = cluster ~id ~title () in
  let deps = Command.print_file file
    begin fun fmt ->
      let v = new visitor fmt goal in
      v#printf "Require Import ZArith.@\n" ;
      v#printf "Require Import Reals.@\n" ;
      v#add_theory "Qedlib" ;
      v#vgoal axioms prop ;
      let libs = Wp_parameters.CoqLibs.get () in
      if libs <> [] then
	begin
	  v#section "Additional Libraries" ;
	  List.iter v#add_userlib libs ;
	  v#hline ;
	end ;
      v#paragraph ;
      engine#global
	begin fun () ->
	  v#printf "@[<hv 2>Goal@ %a.@]@."
	    engine#pp_prop (F.e_prop prop) ;
	end ;
      v#flush
    end in
  let coqcc = { sources = [] } in
  List.iter (assemble coqcc) deps ;
  [ wpd ; dir ] , List.rev coqcc.sources , file

(* -------------------------------------------------------------------------- *)
(* --- Running Coq                                                        --- *)
(* -------------------------------------------------------------------------- *)

open Task
open VCS

let coq_timeout () =
  let coqtimeout = Wp_parameters.CoqTimeout.get () in
  let gentimeout = Wp_parameters.Timeout.get () in
  max coqtimeout gentimeout

let coqidelock = Task.mutex ()
    
class runcoq includes source =
  let base = Filename.chop_extension source in
  let logout = base ^ "_Coq.out" in
  let logerr = base ^ "_Coq.err" in
object(coq)
  
  inherit ProverTask.command "coq"
    
  initializer
    begin
      coq#add_list ~name:"-I" includes ;
      coq#add [ "-noglob" ] ;
    end

      
  method failed : 'a. 'a task =
    begin
      let name = Filename.basename source in
      Wp_parameters.feedback "[Coq] '%s' compilation failed." name ;
      if Sys.file_exists logout then
	Log.print_on_output (fun fmt -> Command.pp_from_file fmt logout) ;
      if Sys.file_exists logerr then
	Log.print_on_output (fun fmt -> Command.pp_from_file fmt logerr) ;
      Task.failed "Compilation of '%s' failed." name ;
    end
      
  method compile =
    coq#set_command "coqc" ;
    coq#add [ source ] ;
    coq#timeout (coq_timeout ()) ;
    coq#run ~logout ~logerr () >>= fun r ->
      if r <> 0 then coq#failed 
      else
	begin
	  let name = Filename.basename source in
	  Wp_parameters.feedback "[Coq] '%s' compiled." name ; 
	  Task.return () ;
	end

  method check =
    coq#set_command "coqc" ;
    coq#add [ source ] ;
    coq#timeout (coq_timeout ()) ;
    coq#run ~logout ~logerr () >>= function
      | 0 -> Task.return true
      | 1 -> Task.return false
      | _ -> coq#failed

  method coqide headers =
    coq#set_command "coqide" ;
    coq#add [ source ] ;
    let script = Wp_parameters.Script.get () in
    if Sys.file_exists script then coq#add [ script ] ;
    coq#add headers ;
    Task.sync coqidelock (coq#run ~logout ~logerr)

end

(* -------------------------------------------------------------------------- *)
(* --- Compilation Helpers                                                --- *)
(* -------------------------------------------------------------------------- *)

let shared_demon = ref true
let shared_headers : (string,unit Task.shared) Hashtbl.t = Hashtbl.create 120

let shared includes source =
  try Hashtbl.find shared_headers source
  with Not_found -> 
    if !shared_demon then
      begin
	shared_demon := false ;
	let server = ProverTask.server () in
	Task.on_server_stop server (fun () -> Hashtbl.clear shared_headers) ;
      end ;
    let descr = Printf.sprintf "Coqc '%s'" source in
    let shared = Task.shared ~descr ~retry:true
      (fun () -> (new runcoq includes source)#compile)
    in Hashtbl.add shared_headers source shared ; shared

let rec compile_headers includes forced = function
  | [] -> Task.nop
  | source::headers ->
      let target = source ^ "o" in
      if forced || need_recompile ~source ~target then
	begin
	  let cc = shared includes source in
	  Task.share cc >>= fun () -> compile_headers includes true headers
	end
      else compile_headers includes forced headers

(* -------------------------------------------------------------------------- *)
(* --- Coq Prover                                                         --- *)
(* -------------------------------------------------------------------------- *)

open Wpo

type coq_wpo = {
  cw_pid : WpPropId.prop_id ;
  cw_gid : string ;
  cw_goal : string ; (* filename for goal without proof *)
  cw_script : string ; (* filename for goal with proof script *)
  cw_includes : string list ; (* -I ... *)
}

let make_script w script =
  Command.print_file w.cw_script
    begin fun fmt ->
      Command.pp_from_file fmt w.cw_goal ;
      Format.fprintf fmt "Proof.@\n%sQed.@\n@." script ;
    end

let try_script w script =
  make_script w script ; (new runcoq w.cw_includes w.cw_script)#check

let rec try_hints w = function
  | [] -> Task.return false
  | (kind,script) :: hints ->
      Wp_parameters.feedback "[Coq] Goal %s : %s" w.cw_gid kind ;
      try_script w script >>= fun succeed ->
	if succeed then
	  let required,hints = WpPropId.prop_id_keys w.cw_pid in
	  let keys = List.merge String.compare required hints in
	  Proof.add_script w.cw_gid keys script ; 
	  Task.return true
	else
	  try_hints w hints
	    
let try_prove w () =
  begin
    match Proof.script_for ~pid:w.cw_pid ~gid:w.cw_gid with
      | Some script -> 
	  Wp_parameters.feedback "[Coq] Goal %s : Saved script" w.cw_gid ;
	  try_script w script
      | None -> Task.return false
  end
  >>= fun succeed ->
    if succeed then 
      Task.return true
    else
      try_hints w (Proof.hints_for ~pid:w.cw_pid)

let try_coqide w headers =
  let script = Proof.script_for_ide ~pid:w.cw_pid ~gid:w.cw_gid in
  make_script w script ;
  (new runcoq w.cw_includes w.cw_script)#coqide headers >>= fun st ->
    if st = 0 then
      match Proof.parse_coqproof w.cw_script with
        | None ->
            Wp_parameters.feedback "[Coq] No proof found" ;
            Task.return false
        | Some script ->
	    if Proof.is_empty script then
	      begin
		Proof.delete_script w.cw_gid ; 
		Task.canceled () ;
	      end
	    else
	      begin
		let req,hs = WpPropId.prop_id_keys w.cw_pid in
		let hints = List.merge String.compare req hs in
		Proof.add_script w.cw_gid hints script ;
		Wp_parameters.feedback "[Coq] Goal %s : Script" w.cw_gid ;
		try_script w script
	      end
    else 
      Task.failed "[Coq] coqide exit with status %d" st
		
let prove_prop wpo ~interactive ~model ~axioms ~prop =
  let pid = wpo.po_pid in
  let gid = wpo.po_gid in
  let script = DISK.file_goal ~pid ~model ~prover:Coq in
  let includes , headers , goal = 
    Model.with_model model (assemble_goal ~pid axioms) prop
  in 
  let w = {
    cw_pid = pid ;
    cw_gid = gid ;
    cw_goal = goal ;
    cw_script = script ;
    cw_includes = includes ;
  } in
  begin
    compile_headers includes false headers >>= try_prove w >>> function
      | Task.Result true -> Task.return true
      | Task.Failed e -> Task.raised e
      | Task.Canceled | Task.Timeout | Task.Result false ->
	  if interactive then try_coqide w headers
	  else Task.return false
  end
  >>= Task.call (fun r -> if r then VCS.valid else VCS.unknown)
    
let prove_annot wpo vcq ~interactive = 
  Task.todo
    begin fun () ->
      let model = vcq.VC_Annot.model in
      let prop = GOAL.proof vcq.VC_Annot.goal in
      prove_prop wpo ~interactive ~model ~axioms:None ~prop
    end
    
let prove_lemma wpo vca ~interactive = 
  Task.todo
    begin fun () ->
      let model = vca.VC_Lemma.model in
      let lemma = vca.VC_Lemma.lemma in
      let depends = vca.VC_Lemma.depends in
      let prop = F.p_forall lemma.l_forall lemma.l_lemma in
      let axioms = Some(lemma.l_cluster,depends) in
      prove_prop wpo ~interactive ~model ~axioms ~prop
    end

let prove wpo ~interactive =
  match wpo.Wpo.po_formula with
    | Legacy _ -> assert false
    | GoalAnnot vcq -> prove_annot wpo vcq ~interactive
    | GoalLemma vca -> prove_lemma wpo vca ~interactive
