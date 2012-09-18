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
(* --- Prover Alt-Ergo Interface                                          --- *)
(* -------------------------------------------------------------------------- *)

open Cil_types
open Qed
open Lang
open Definitions
  
(* -------------------------------------------------------------------------- *)
(* --- Making Goal File                                                   --- *)
(* -------------------------------------------------------------------------- *)

let altergo_gui = 
  lazy 
    begin
      let x = Command.command "altgr-ergo" [| "-version" |] in 
      match x with
	| Unix.WEXITED 0 ->  true
	| _ ->  false 
    end

let append_file out file =
  let lines = ref 0 in
  Command.read_lines file
    begin fun line ->
      output_string out line ;
      output_string out "\n" ;
      incr lines ;
    end ; 
  !lines

let find_lib includes file =
  if Sys.file_exists file then file else 
    let rec lookup file = function
      | [] -> Wp_parameters.abort "Alt-Ergo library '%s' not found" file
      | dir::dirs ->
	  let path = Printf.sprintf "%s/%s" dir file in
	  if Sys.file_exists path then path else lookup file dirs
    in lookup file includes

let rec locate_error files file line =
  match files with
    | [] -> ProverTask.location file line
    | (f,n)::files ->
	if line <= n then ProverTask.location f line 
	else locate_error files file (line-n)

let cluster_file c = 
  let dir = Model.directory () in
  let base = cluster_id c in
  Printf.sprintf "%s/%s.ergo" dir base

let shared f = Wp_parameters.Share.file ~error:true f

(* -------------------------------------------------------------------------- *)
(* --- Exporting Formulae to Alt-Ergo                                     --- *)
(* -------------------------------------------------------------------------- *)
    
type depend =
  | D_file of string
  | D_cluster of cluster

module TYPES = Model.Index
  (struct
     type key = adt
     type data = tau
     let name = "ProverErgo.TYPES"
     let compare = ADT.compare
     let pretty = ADT.pretty
   end)

let engine = 
  let module E = Qed.Export_altergo.Make(Lang.F) in
object
  inherit E.engine
  method datatype = ADT.id
  method field = Field.id
  method link = Lang.link
  method set_typedef = TYPES.define
  method get_typedef = TYPES.get
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

  method add_file f = deps <- (D_file f) :: deps
	  
  method on_cluster c = deps <- (D_cluster c) :: deps

  method on_theory = function
    | "qed"    -> ()
    | "cint"   -> self#add_file (shared "cint.mlw")
    | "cfloat" -> self#add_file (shared "cfloat.mlw")
    | "vset"   -> self#add_file (shared "vset.mlw")
    | "memory" -> self#add_file (shared "memory.mlw")
    | "cmath" -> self#add_file (shared "cmath.mlw")
    | thy -> Wp_parameters.fatal ~current:false 
	"Unregistered theory '%s' for alt-ergo" thy

  method on_type lt def =
    begin
      self#lines ;
      engine#declare_type fmt (Lang.atype lt) (List.length lt.lt_params) def ;
    end

  method on_comp c fts =
    begin
      (*TODO:NUPW: manage UNIONS *)
      self#lines ;
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

let write_cluster c job =
  let f = cluster_file c in
  Wp_parameters.debug ~dkey:"prover" "Generate '%s'" f ;
  let output = Command.print_file f
    begin fun fmt -> 
      let v = new visitor fmt c in
      job v ; v#flush
    end
  in
  if Wp_parameters.has_dkey "goal" then
    Log.print_on_output
      begin fun fmt ->
	Format.fprintf fmt "---------------------------------------------@\n" ;
	Format.fprintf fmt "--- File '%s.ergo' @\n" (cluster_id c) ;
	Format.fprintf fmt "---------------------------------------------@\n" ;
	Command.pp_from_file fmt f ;
      end ;
  output

(* -------------------------------------------------------------------------- *)
(* --- File Assembly                                                      --- *)
(* -------------------------------------------------------------------------- *)

module CLUSTERS = Model.Index
  (struct
     type key = cluster
     type data = int * depend list
     let name = "ProverErgo.CLUSTERS"
     let compare = cluster_compare
     let pretty = pp_cluster
   end)

type export = {
  out : out_channel ;
  mutable files : (string * int) list ;
}

let rec assemble export = function
  | D_file file -> assemble_file export file
  | D_cluster c -> assemble_cluster export c

and assemble_file export file =
  if List.for_all (fun (f,_) -> f <> file) export.files 
  then 
    let lines = append_file export.out file in
    export.files <- (file,lines) :: export.files
  
and assemble_cluster export c =
  let (age,deps) = try CLUSTERS.find c with Not_found -> (-1,[]) in
  let deps = 
    if age < cluster_age c then
      let deps = write_cluster c (fun v -> v#vself) in
      CLUSTERS.update c (cluster_age c , deps) ; deps
    else deps
  in List.iter (assemble export) deps ; 
  assemble_file export (cluster_file c)

let assemble_lib export includes lib =
  assemble_file export (find_lib includes lib)

(* -------------------------------------------------------------------------- *)
(* --- Assembling Goal                                                    --- *)
(* -------------------------------------------------------------------------- *)

let assemble_goal ~file ~id ~title ~axioms prop =
  let goal = cluster ~id ~title () in
  let model = if Wp_parameters.UnsatModel.get () then 1 else 0 in
  let deps = write_cluster goal
    begin fun v ->
      v#vgoal axioms prop ;
      v#paragraph ;
      engine#global 
	begin fun () ->
	  v#printf "@[<hv 2>goal %s:@ %a@]@." id 
	    (engine#pp_goal ~model) 
	    (F.e_prop prop) ;
	end
    end in
  Command.write_file file
    begin fun out ->
      let export = { files = [] ; out = out } in
      assemble_file export (shared "qed.mlw") ;
      List.iter (assemble export) deps ;
      let includes = Wp_parameters.Includes.get () in
      let libs = Wp_parameters.AltErgoLibs.get () in
      List.iter (assemble_lib export includes) libs ;
      assemble_file export (cluster_file goal) ;
      List.rev export.files
    end

(* -------------------------------------------------------------------------- *)
(* --- Running AltErgo                                                    --- *)
(* -------------------------------------------------------------------------- *)

open ProverTask

(*bug in Alt-Ergo: sometimes error messages are repeated. *)
(*let p_loc = "^File " ... *)

let p_loc = "File " ^ p_string ^ ", line " ^ p_int ^ ", [^:]+:"
let p_valid = p_loc ^ "Valid (" ^ p_float ^ ") (" ^ p_int ^ ")"
let p_unknown = p_loc ^ "I don't know"
let p_limit = "^Steps limit reached: " ^ p_int

let re_error = Str.regexp p_loc
let re_valid = Str.regexp p_valid
let _re_unknown = Str.regexp p_unknown
let re_limit = Str.regexp p_limit

class altergo ~pid ~gui ~file ~lines ~logout ~logerr =
object(ergo)
  
  inherit ProverTask.command "alt-ergo"

  val mutable files = []
  val mutable error = None
  val mutable valid = false
  val mutable limit = false
  val mutable time = 0.0
  val mutable steps = 0

  method private time t = time <- t

  method private error (a : pattern) = 
    let lpos = locate_error files (a#get_string 1) (a#get_int 2) in
    let message = a#get_after ~offset:1 2 in
    error <- Some ( lpos , message )
      
  method private valid (a : pattern) =
    begin
      valid <- true ;
      time <- a#get_float 3 ;
      steps <- a#get_int 4 ;
    end

  method private limit (a : pattern) =
    begin
      limit <- true ;
      steps <- pred (a#get_int 1) ;
    end

  method result r =
    if r = 0 && not valid && Wp_parameters.UnsatModel.get () then 
      begin
	let message = Pretty_utils.sfprintf "Model for %a" WpPropId.pretty pid in
	ProverTask.pp_file ~message ~file:logout ;
      end ;
    match error with
      | Some(pos,message) -> 
	  Wp_parameters.error ~source:pos "Alt-Ergo error:@\n%s" message ;
	  VCS.failed ~pos message
      | None ->
	  if r = 0 || r = 1 then
	    let verdict = 
	      if valid then VCS.Valid else 
		if limit then VCS.Stepout else
		  VCS.Unknown in
	    VCS.result ~time:(if gui then 0.0 else time) ~steps verdict
	  else
	    begin
	      ProverTask.pp_file ~message:"Alt-Ergo (stdout)" ~file:logout ;
	      ProverTask.pp_file ~message:"Alt-Ergo (stderr)" ~file:logerr ;
	      VCS.failed (Printf.sprintf "Alt-Ergo exits with status [%d]" r)
	    end
	      
  method prove =
    let depth = Wp_parameters.Depth.get () in
    let steps = Wp_parameters.Steps.get () in
    let time = Wp_parameters.Timeout.get () in
    files <- lines ;
    if gui then ergo#set_command "altgr-ergo" ;
    ergo#add_positive ~name:"-age-limite" ~value:depth ;
    ergo#add_positive ~name:"-stop" ~value:depth ;
    ergo#add_positive ~name:"-steps" ~value:steps ;
    ergo#add_parameter ~name:"-proof" Wp_parameters.ProofTrace.get ;
    ergo#add_parameter ~name:"-model" Wp_parameters.UnsatModel.get ;
    ergo#add [ file ] ;
    if not gui then ergo#timeout time ;
    ergo#validate_time ergo#time ;
    ergo#validate_pattern ~logs:ERR re_error ergo#error ;
    ergo#validate_pattern ~logs:OUT re_valid ergo#valid ;
    ergo#validate_pattern ~logs:OUT re_limit ergo#limit ;
    ergo#run ~logout ~logerr
      
end

open VCS
open Wpo
open Task

let try_prove ~pid ~gui ~file ~lines ~logout ~logerr =
  let ergo = new altergo ~pid ~gui ~file ~lines ~logout ~logerr in
  ergo#prove () >>> function
    | Task.Timeout -> Task.return VCS.timeout
    | Task.Result r -> Task.call ergo#result r
    | st -> Task.status (Task.map (fun _ -> assert false) st)

let prove_file ~pid ~interactive ~file ~lines ~logout ~logerr =
  try_prove ~pid ~gui:false ~file ~lines ~logout ~logerr >>= function
    | { verdict=(VCS.Unknown|VCS.Timeout|VCS.Stepout) } 
	when interactive && Lazy.force altergo_gui ->
	try_prove ~pid ~gui:true ~file ~lines ~logout ~logerr
    | r -> Task.return r
    
let prove_prop ~pid ~interactive ~model ~axioms ~prop =
  let prover = AltErgo in
  let file = DISK.file_goal ~pid ~model ~prover in
  let logout = DISK.file_logout ~pid ~model ~prover in
  let logerr = DISK.file_logerr ~pid ~model ~prover in
  let id = WpPropId.get_propid pid in
  let title = Pretty_utils.to_string WpPropId.pretty pid in
  let lines = Model.with_model model 
    (assemble_goal ~file ~id ~title ~axioms) prop in
  prove_file ~pid ~interactive ~file ~lines ~logout ~logerr

let prove_annot pid vcq ~interactive =
  Task.todo
    begin fun () ->
      let model = vcq.Wpo.VC_Annot.model in
      let axioms = None in
      let prop = GOAL.proof vcq.VC_Annot.goal in
      prove_prop ~pid ~interactive ~model ~axioms ~prop
    end

let prove_lemma pid vca ~interactive =
  Task.todo
    begin fun () ->
      let model = vca.Wpo.VC_Lemma.model in
      let lemma = vca.Wpo.VC_Lemma.lemma in
      let depends = vca.Wpo.VC_Lemma.depends in
      let prop = F.p_forall lemma.l_forall lemma.l_lemma in
      let axioms = Some(lemma.l_cluster,depends) in
      prove_prop ~pid ~interactive ~model ~axioms ~prop
    end

let prove wpo ~interactive =
  let pid = wpo.Wpo.po_pid in
  match wpo.Wpo.po_formula with
    | Wpo.Legacy _ -> assert false
    | Wpo.GoalAnnot vcq -> prove_annot pid vcq ~interactive
    | Wpo.GoalLemma vca -> prove_lemma pid vca ~interactive

