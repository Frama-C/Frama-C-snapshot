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

(* -------------------------------------------------------------------------- *)
(* --- Prover Coq Interface                                               --- *)
(* -------------------------------------------------------------------------- *)

open Cil_types
open Qed
open Lang
open Definitions

let dkey = Wp_parameters.register_category "prover"

let cluster_file c = 
  let dir = Model.directory () in
  let base = cluster_id c in
  Printf.sprintf "%s/%s.v" dir base

(* -------------------------------------------------------------------------- *)
(* --- Exporting Formulae to Coq                                          --- *)
(* -------------------------------------------------------------------------- *)

type depend =
  | D_module of string   (* D_module A : <share>/A.v copied in <out>/wp/A.v *)
  | D_cluster of cluster (* D_cluster A : Generated in <out>/<model>/A.v *)
  | D_file of string * string (* D_file(F,A) : File F copied in <out>/<model>/A.v *)

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

  method add_module f = 
    let dm = D_module f in
    if not (List.mem dm deps) then
      begin
	self#lines ;
	Format.fprintf fmt "Require Import %s.@\n" f ;
	deps <- dm :: deps
      end

  method add_library file lib =
    self#lines ;
    Format.fprintf fmt "Require Import %s.@\n" lib ;
    deps <- (D_file(file,lib)) :: deps

  method add_extlib name =
    let file = Wp_parameters.find_lib name in
    let lib = Filename.chop_extension (Filename.basename file) in
    self#add_library file lib
	  
  method on_cluster c = 
    self#lines ;
    Format.fprintf fmt "Require Import %s.@\n" (cluster_id c) ;
    deps <- (D_cluster c) :: deps

  method on_theory = function
    | "qed" | "driver" -> ()
    | "cint"   -> self#add_module "Cint"
    | "cbits"  -> List.iter self#add_module [ "Cint" ; "Bits" ; "Cbits" ]
    | "cfloat" -> self#add_module "Cfloat"
    | "vset"   -> self#add_module "Vset"
    | "memory" -> self#add_module "Memory"
    | "cmath" -> self#add_module "Cmath"
    | thy -> Wp_parameters.fatal 
	~current:false "No builtin theory '%s' for Coq" thy

  method on_library thy = 
    let lib = String.capitalize thy in
    let file = Wp_parameters.find_lib (lib ^ ".v") in
    self#add_library file lib

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
	| Value(t,mu,v) ->
	    let pp = match mu with
	      | Rec -> engine#declare_fixpoint ~prefix:"Fix"
	      | Def -> engine#declare_definition
	    in pp fmt d.d_lfun d.d_params t v
	| Predicate(mu,p) ->
	    let pp = match mu with
	      | Rec -> engine#declare_fixpoint ~prefix:"Fix"
	      | Def -> engine#declare_definition
	    in pp fmt d.d_lfun d.d_params Logic.Prop (F.e_prop p)
	| Inductive _ ->
	    engine#declare_signature fmt
	      d.d_lfun (List.map F.tau_of_var d.d_params) Logic.Prop
    end

end

let write_cluster c =
  let f = cluster_file c in
  Wp_parameters.debug ~dkey "Generate '%s'" f ;
  Command.print_file f
    begin fun fmt ->
      let v = new visitor fmt c in
      v#lines ;
      v#printf "Require Import ZArith.@\n" ;
      v#printf "Require Import Reals.@\n" ;
      v#add_module "Qedlib" ;
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

(** theories -> needed directory to include *)
let compiled_theories : string option Datatype.String.Hashtbl.t
    = Datatype.String.Hashtbl.create 10
(*[LC] Shared : not projectified. *)

module CLUSTERS = Model.Index
  (struct
     type key = cluster
     type data = int * depend list
     let name = "ProverCoq.FILES"
     let compare = cluster_compare
     let pretty = pp_cluster
   end)

type coqcc = {
  (* in reverse order: *)
  mutable includes : string list ; (* directories where .vo are found *)
  mutable sources : string list ; (* file .v to recompile *)
}

let add_include coqcc dir =
  if not (List.mem dir coqcc.includes) then coqcc.includes <- dir :: coqcc.includes

let add_source coqcc file =
  if not (List.mem file coqcc.sources) then coqcc.sources <- file :: coqcc.sources

let rec assemble coqcc = function
  | D_module thy -> assemble_theory coqcc thy
  | D_cluster c -> assemble_cluster coqcc c
  | D_file(path,_) -> assemble_userlib coqcc path

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
  try
    let dirinclude = Datatype.String.Hashtbl.find compiled_theories thy in
    match dirinclude with
    | None -> ()
    | Some dirinclude -> add_include coqcc dirinclude
  with Not_found ->
    let source = Wp_parameters.Share.file ~error:true (thy ^ ".v") in
    if Sys.file_exists (source ^ "o") then
      begin
        let dirinclude = (Filename.dirname source) in
	add_include coqcc dirinclude;
        Datatype.String.Hashtbl.add compiled_theories thy (Some dirinclude)
      end
    else
      begin
	let tgtdir = Wp_parameters.get_output_dir "wp" in
	let target = Printf.sprintf "%s/%s.v" tgtdir thy in
	Command.copy source target ;
	add_source coqcc target;
        Datatype.String.Hashtbl.add compiled_theories thy None
      end

and assemble_userlib coqcc source =
  if Sys.file_exists (source ^ "o") then
    add_include coqcc (Filename.dirname source)
  else
    begin
      let tgtdir = Model.directory () in
      let coqsrc = Filename.basename source in
      let target = Printf.sprintf "%s/%s" tgtdir coqsrc in
      if need_recompile ~source ~target then Command.copy source target ;
      add_source coqcc target
    end

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
      v#add_module "Qedlib" ;
      v#vgoal axioms prop ;
      let libs = Wp_parameters.CoqLibs.get () in
      if libs <> [] then
	begin
	  v#section "Additional Libraries" ;
	  List.iter v#add_extlib libs ;
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
  let coqcc = { includes = [] ; sources = [] } in
  List.iter (assemble coqcc) deps ;
  let includes = wpd :: List.rev (dir :: coqcc.includes) in
  let sources = List.rev coqcc.sources in
  includes , sources , file

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
    Task.call 
      (fun () ->
        if not (Wp_parameters.wpcheck ()) then
	  let name = Filename.basename source in
	  Wp_parameters.feedback "[Coq] Compiling '%s'." name) ()
    >>= coq#run ~logout ~logerr 
    >>= fun r ->
      if r <> 0 then coq#failed 
      else Task.return ()

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
  cw_headers : string list ; (* filename for libraries *)
  cw_includes : string list ; (* -I ... *)
}

let make_script ?(admitted=false) w script =
  Command.print_file w.cw_script
    begin fun fmt ->
      Command.pp_from_file fmt w.cw_goal ;
      if admitted then
        Format.fprintf fmt "Proof.@\nAdmitted.@\n@."
      else
        Format.fprintf fmt "Proof.@\n%sQed.@\n@." script ;
    end

let try_script ?admitted w script =
  make_script ?admitted w script ; (new runcoq w.cw_includes w.cw_script)#check

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

let try_coqide w =
  let script = Proof.script_for_ide ~pid:w.cw_pid ~gid:w.cw_gid in
  make_script w script ;
  (new runcoq w.cw_includes w.cw_script)#coqide w.cw_headers >>= fun st ->
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

let prove_session ~interactive w =
  begin
    compile_headers w.cw_includes false w.cw_headers >>= try_prove w >>> function
      | Task.Result true -> Task.return true
      | Task.Failed e -> Task.raised e
      | Task.Canceled | Task.Timeout | Task.Result false ->
	  if interactive then try_coqide w
	  else Task.return false
  end
  >>= Task.call (fun r -> if r then VCS.valid else VCS.unknown)

exception Admitted_not_proved

let check_session w =
  compile_headers w.cw_includes false w.cw_headers >>=
    (fun () -> try_script ~admitted:true w "") >>> function
    | Task.Result true -> Task.return VCS.unknown
    | Task.Failed e -> Task.raised e
    | Task.Canceled | Task.Timeout | Task.Result false ->
      Task.raised Admitted_not_proved

let prove_session ~interactive w =
  if Wp_parameters.wpcheck ()
  then check_session w
  else prove_session ~interactive w

let prove_prop wpo ~interactive ~axioms ~prop =
  let pid = wpo.po_pid in
  let gid = wpo.po_gid in
  let model = wpo.po_model in
  let script = DISK.file_goal ~pid ~model ~prover:Coq in
  let includes , headers , goal = 
    Model.with_model model (assemble_goal ~pid axioms) prop
  in 
  if Wp_parameters.Generate.get () 
  then Task.return VCS.no_result
  else prove_session ~interactive {
    cw_pid = pid ;
    cw_gid = gid ;
    cw_goal = goal ;
    cw_script = script ;
    cw_headers = headers ;
    cw_includes = includes ;
  }
    
let prove_annot wpo vcq ~interactive = 
  Task.todo
    begin fun () ->
      let prop = GOAL.compute_proof vcq.VC_Annot.goal in
      prove_prop wpo ~interactive ~axioms:None ~prop
    end
    
let prove_lemma wpo vca ~interactive = 
  Task.todo
    begin fun () ->
      let lemma = vca.VC_Lemma.lemma in
      let depends = vca.VC_Lemma.depends in
      let prop = F.p_forall lemma.l_forall lemma.l_lemma in
      let axioms = Some(lemma.l_cluster,depends) in
      prove_prop wpo ~interactive ~axioms ~prop
    end

let prove wpo ~interactive =
  match wpo.Wpo.po_formula with
    | GoalAnnot vcq -> prove_annot wpo vcq ~interactive
    | GoalLemma vca -> prove_lemma wpo vca ~interactive
