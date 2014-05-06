(**************************************************************************)
(*                                                                        *)
(*  This file is part of WP plug-in of Frama-C.                           *)
(*                                                                        *)
(*  Copyright (C) 2007-2014                                               *)
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
(* --- Prover Alt-Ergo Interface                                          --- *)
(* -------------------------------------------------------------------------- *)

open Cil_types
open Qed
open Lang
open Definitions

let dkey = Wp_parameters.register_category "prover"

let option_file = LogicBuiltins.create_option
                    (fun ~driver_dir x -> Filename.concat driver_dir x)
                    "altergo" "file"

(* -------------------------------------------------------------------------- *)
(* --- Making Goal File                                                   --- *)
(* -------------------------------------------------------------------------- *)

let altergo_gui = 
  lazy 
    begin
      let x = Command.command "altgr-ergo" [| "-version" |] in 
      match x with
	| Unix.WEXITED 0 ->  true
	| _ -> Wp_parameters.error ~current:false "Command 'altgr-ergo' does not work." ; false 
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

(* -------------------------------------------------------------------------- *)
(* --- Exporting Formulae to Alt-Ergo                                     --- *)
(* -------------------------------------------------------------------------- *)
    
type depend =
  | D_file of string
  | D_cluster of cluster

let pp_depend fmt = function
  | D_file file -> Format.fprintf fmt "File %s" file
  | D_cluster cluster -> Format.fprintf fmt "Cluster %a"
    Definitions.pp_cluster cluster

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
  inherit E.engine as super
  inherit Lang.idprinting

  method infoprover p = p.altergo
  method set_typedef = TYPES.define
  method get_typedef = TYPES.get
  method! typeof_call = Lang.tau_of_lfun
  method! typeof_getfield = Lang.tau_of_field
  method! typeof_setfield = Lang.tau_of_record 
  val mutable share = true
  method! is_shareable e = share && super#is_shareable e
  method! declare_axiom fmt a xs tgs phi =
    try share <- false ; super#declare_axiom fmt a xs tgs phi ; share <- true
    with err -> share <- true ; raise err
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

  method add_dfile f = 
    let df = D_file f in
    if not (List.mem df deps) then deps <- df :: deps

  method add_shared f = self#add_dfile (Wp_parameters.Share.file ~error:true f)
  method add_library f = self#add_dfile f
	  
  method on_cluster c = deps <- (D_cluster c) :: deps

  method on_library thy =
    let iter file = self#add_library file in
    List.iter iter
      (LogicBuiltins.get_option option_file ~library:thy)

  method on_type lt def =
    begin
      self#lines ;
      engine#declare_type fmt (Lang.atype lt) (List.length lt.lt_params) def ;
    end

  method on_comp c fts =
    begin
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
	| Inductive _ ->
	    engine#declare_signature fmt
	      d.d_lfun (List.map F.tau_of_var d.d_params) Logic.Prop
    end

end

let write_cluster c job =
  let f = cluster_file c in
  Wp_parameters.debug ~dkey "Generate '%s'" f ;
  let output = Command.print_file f
    begin fun fmt -> 
      let v = new visitor fmt c in
      job v ; v#flush
    end
  in
  if Wp_parameters.has_dkey "cluster" then
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
  in
  List.iter (assemble export) deps ; 
  let file = cluster_file c in
  assemble_file export file

and assemble_lib export lib = 
  assemble_file export (LogicBuiltins.find_lib lib)

(* -------------------------------------------------------------------------- *)
(* --- Assembling Goal                                                    --- *)
(* -------------------------------------------------------------------------- *)

let assemble_goal ~file ~id ~title ~axioms prop =
  let goal = cluster ~id ~title () in
  let model = if Wp_parameters.ProofTrace.get () then 1 else 0 in
  let deps = write_cluster goal
    begin fun v ->
      v#on_library "qed";
      v#vgoal axioms prop ;
      v#paragraph ;
      try
	let qlet = List.mem "qlet" (Wp_parameters.AltErgoFlags.get ()) in
	engine#set_quantify_let qlet ;
      engine#global 
	begin fun () ->
	  v#printf "@[<hv 2>goal %s:@ %a@]@." id 
	    (engine#pp_goal ~model)
	    (F.e_prop prop) ;
	end ;
	engine#set_quantify_let false ;
      with error ->
	engine#set_quantify_let false ;
	raise error
    end in
  Command.write_file file
    begin fun out ->
      let export = { files = [] ; out = out } in
      List.iter (assemble export) deps ;
      let libs = Wp_parameters.AltErgoLibs.get () in
      List.iter (assemble_lib export) libs ;
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
let p_unsat = p_loc ^ "I don't know"
let p_limit = "^Steps limit reached: " ^ p_int

let re_error = Str.regexp p_loc
let re_valid = Str.regexp p_valid
let re_limit = Str.regexp p_limit
let re_unsat = Str.regexp p_unsat

class altergo ~pid ~gui ~file ~lines ~logout ~logerr =
object(ergo)
  
  initializer ignore pid

  inherit ProverTask.command "alt-ergo"

  val mutable files = []
  val mutable error = None
  val mutable valid = false
  val mutable limit = false
  val mutable unsat = false
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

  method private unsat (_ : pattern) =
    begin
      unsat <- true ;
    end

  method result r =
    match error with
      | Some(pos,message) -> 
	  Wp_parameters.error ~source:pos "Alt-Ergo error:@\n%s" message ;
	  VCS.failed ~pos message
      | None ->
	  try
	    let verdict = 
	      if unsat then VCS.Unknown else
		if valid then VCS.Valid else 
		  if limit then VCS.Stepout else
		    raise Not_found in
	    VCS.result ~time:(if gui then 0.0 else time) ~steps verdict
	  with
          | Not_found when Wp_parameters.Check.get () ->
            if r = 0 then VCS.no_result
            else
              begin
	        ProverTask.pp_file ~message:"Alt-Ergo (stdout)" ~file:logout ;
	        ProverTask.pp_file ~message:"Alt-Ergo (stderr)" ~file:logerr ;
                VCS.failed "Alt-Ergo type-checking failed"
              end
          | Not_found ->
	    begin
	      ProverTask.pp_file ~message:"Alt-Ergo (stdout)" ~file:logout ;
	      ProverTask.pp_file ~message:"Alt-Ergo (stderr)" ~file:logerr ;
	      if r <> 0 then
		VCS.failed (Printf.sprintf "Alt-Ergo exits with status [%d]" r)
	      else
		VCS.failed "Can not understand Alt-Ergo output."
	    end

  method prove =
    let depth = Wp_parameters.Depth.get () in
    let steps = Wp_parameters.Steps.get () in
    let time = Wp_parameters.Timeout.get () in
    files <- lines ;
    if gui then ergo#set_command "altgr-ergo" ;
    if Wp_parameters.Check.get () then
      ergo#add ["-type-only"]
    else
      begin
	ergo#add_positive ~name:"-age-bound" ~value:depth ;
	ergo#add_positive ~name:"-stop-bound" ~value:depth ;
	ergo#add_positive ~name:"-steps-bound" ~value:steps ;
	ergo#add_parameter ~name:"-proof" Wp_parameters.ProofTrace.get ;
	ergo#add_parameter ~name:"-model" Wp_parameters.ProofTrace.get ;
      end ;
    let flags = List.filter 
      (fun p -> p <> "qlet") 
      (Wp_parameters.AltErgoFlags.get ()) in
    ergo#add flags ;
    ergo#add [ file ] ;
    if not gui then ergo#timeout time ;
    ergo#validate_time ergo#time ;
    ergo#validate_pattern ~logs:`ERR re_error ergo#error ;
    ergo#validate_pattern ~logs:`OUT re_valid ergo#valid ;
    ergo#validate_pattern ~logs:`OUT re_limit ergo#limit ;
    ergo#validate_pattern ~logs:`OUT re_unsat ergo#unsat ;
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

let prove_file ~pid ~mode ~file ~lines ~logout ~logerr =
  let gui = match mode with
    | EditMode -> Lazy.force altergo_gui
    | BatchMode | FixMode -> false in
  try_prove ~pid ~gui ~file ~lines ~logout ~logerr >>= function
    | { verdict=(VCS.Unknown|VCS.Timeout|VCS.Stepout) } 
    when mode = FixMode && Lazy.force altergo_gui ->
	try_prove ~pid ~gui:true ~file ~lines ~logout ~logerr
    | r -> Task.return r
    
let prove_prop ~pid ~mode ~model ~axioms ~prop =
  let prover = AltErgo in
  let file = DISK.file_goal ~pid ~model ~prover in
  let logout = DISK.file_logout ~pid ~model ~prover in
  let logerr = DISK.file_logerr ~pid ~model ~prover in
  let id = WpPropId.get_propid pid in
  let title = Pretty_utils.to_string WpPropId.pretty pid in
  let lines = Model.with_model model 
    (assemble_goal ~file ~id ~title ~axioms) prop in
  if Wp_parameters.Generate.get ()
  then Task.return VCS.no_result
  else prove_file ~pid ~mode ~file ~lines ~logout ~logerr

let prove_annot model pid vcq ~mode =
  Task.todo
    begin fun () ->
      let axioms = None in
      let prop = GOAL.compute_proof vcq.VC_Annot.goal in
      prove_prop ~pid ~mode ~model ~axioms ~prop
    end

let prove_lemma model pid vca ~mode =
  Task.todo
    begin fun () ->
      let lemma = vca.Wpo.VC_Lemma.lemma in
      let depends = vca.Wpo.VC_Lemma.depends in
      let prop = F.p_forall lemma.l_forall lemma.l_lemma in
      let axioms = Some(lemma.l_cluster,depends) in
      prove_prop ~pid ~mode ~model ~axioms ~prop
    end

let prove_check model pid vck ~mode =
  Task.todo 
    begin fun () ->
      let axioms = None in
      let prop = vck.VC_Check.goal in
      prove_prop ~pid ~mode ~model ~axioms ~prop
    end

let prove mode wpo =
  let pid = wpo.Wpo.po_pid in
  let model = wpo.Wpo.po_model in
  match wpo.Wpo.po_formula with
    | Wpo.GoalAnnot vcq -> prove_annot model pid vcq ~mode
    | Wpo.GoalLemma vca -> prove_lemma model pid vca ~mode
    | Wpo.GoalCheck vck -> prove_check model pid vck ~mode

