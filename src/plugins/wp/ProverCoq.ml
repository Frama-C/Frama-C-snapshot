(**************************************************************************)
(*                                                                        *)
(*  This file is part of WP plug-in of Frama-C.                           *)
(*                                                                        *)
(*  Copyright (C) 2007-2016                                               *)
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
(* --- External Coq Libraries                                             --- *)
(* -------------------------------------------------------------------------- *)

(* Applies to both WP resources from the Share, and User-defined libraries *)
let option_file = LogicBuiltins.create_option
    (fun ~driver_dir x -> driver_dir ^ "/" ^ x)
    "coq" "file"

type coqlib = {
  c_id : string ; (* Identifies the very original file. *)
  c_source : string ;  (* Original file directory. *)
  c_file : string ;    (* Relative Coq source file. *)
  c_path : string ;    (* Relative Coq source directory. *)
  c_name : string ;    (* Module package. *)
  c_module : string ;  (* Full module name. *)
}

(* example:

   {
   c_id="/mydir/foobar:a/b/User.v" ;
   c_source="/mydir/foobar" ;
   c_file= "a/b/User.v" ;
   c_path = "a/b" ;
   c_name = "a.b" ;
   c_module  = "a.b.User" ;
   }

*)

(* Take the directory name and changes all '/' into '.' *)
let name_of_path path =
  if path = "." then ""
  else
    String.map (fun c -> if c = '/' || c = '\\' then '.' else c) path

let find_nonwin_column opt =
  let p = String.rindex opt ':' in
  if String.length opt >= 3 &&
     opt.[1] = ':' && (opt.[2] = '/' || opt.[2] = '\\') && p = 1 then
    (* windows absolute path, not <source>:<dir>/<file.v> format. *)
    raise Not_found
  else p

(* Parses the coq.file option from the driver. *)
let parse_c_option opt =
  try
    (* Format "<source>:<dir>/<file.v>" *)
    let p = find_nonwin_column opt in
    let c_source = String.sub opt 0 p in
    let c_file = String.sub opt (p+1) (String.length opt - p - 1) in
    let c_path = Filename.dirname c_file in
    let c_name = name_of_path c_path in
    let coqid = Filename.chop_extension (Filename.basename c_file) in
    let c_module =
      Printf.sprintf "%s.%s" c_name
        (Transitioning.String.capitalize_ascii coqid)
    in
    { c_id = opt ; c_source ; c_file ; c_path ; c_name ; c_module }
  with Not_found ->
    (* Format "<source>/<file.v>" *)
    let c_source = Filename.dirname opt in
    let c_file = Filename.basename opt in
    let c_module =
      Transitioning.String.capitalize_ascii (Filename.chop_extension c_file)
    in
    { c_id = opt ; c_source ; c_file ; c_path = "." ; c_name = "" ; c_module }

let coqlibs = Hashtbl.create 128 (*[LC] Not Projectified. *)
let c_option opt =
  try Hashtbl.find coqlibs opt
  with Not_found ->
    let clib = parse_c_option opt in
    Hashtbl.add coqlibs opt clib ; clib
(* -------------------------------------------------------------------------- *)
(* --- Dependencies                                                       --- *)
(* -------------------------------------------------------------------------- *)

type depend =
  | D_cluster of cluster (* Generated in <out>/<model>/A.v *)
  | D_coqlib of coqlib   (* From <source>/ or <out>/coqwp/ *)

(* -------------------------------------------------------------------------- *)
(* --- Exporting Formulae to Coq                                          --- *)
(* -------------------------------------------------------------------------- *)

let engine =
  let module E = Qed.Export_coq.Make(Lang.F) in
  object(self)
    inherit E.engine as super
    inherit Lang.idprinting
    method infoprover p = p.coq
                            
    method! pp_fun cmode fct ts =
      if fct == Vlist.f_concat
      then Vlist.pp_concat self ts
      else super#pp_fun cmode fct ts
    
  end


class visitor fmt c =
  object(self)

    inherit Definitions.visitor c
    inherit ProverTask.printer fmt (cluster_title c)

    val mutable deps : depend list = []

    (* --- Managing Formatter --- *)

    method flush =
      begin
        Format.pp_print_newline fmt () ;
        List.rev deps
      end

    (* --- Files, Theories and Clusters --- *)

    method add_coqfile opt =
      let clib = c_option opt in
      Format.fprintf fmt "Require Import %s.@\n" clib.c_module ;
      deps <- (D_coqlib clib) :: deps

    method on_library thy =
      let files = LogicBuiltins.get_option option_file ~library:thy in
      List.iter self#add_coqfile files

    method on_cluster c =
      self#lines ;
      Format.fprintf fmt "Require Import %s.@\n" (cluster_id c) ;
      deps <- (D_cluster c) :: deps



    method on_type lt def =
      begin
        self#lines ;
        engine#declare_type fmt (Lang.atype lt) (List.length lt.lt_params) def ;
      end

    method on_comp c fts =
      begin
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
      v#on_library "qed" ;
      v#vself ;
      v#flush ;
    end

(* -------------------------------------------------------------------------- *)
(* --- Assembling Goal                                                    --- *)
(* -------------------------------------------------------------------------- *)

(* Returns whether source was modified after target *)
let need_recompile ~source ~target =
  try
    let t_src = (Unix.stat source).Unix.st_mtime in
    let t_tgt = (Unix.stat target).Unix.st_mtime in
    t_src >= t_tgt
  with Unix.Unix_error _ -> true

(* Used to mark version of clusters already available *)

module CLUSTERS = Model.Index
    (struct
      type key = cluster
      type data = int * depend list
      let name = "ProverCoq.FILES"
      let compare = cluster_compare
      let pretty = pp_cluster
    end)

(* Used to mark coqlib versions to use *)
module Marked = Set.Make
    (struct
      type t = depend
      let compare d1 d2 =
        match d1 , d2 with
        | D_coqlib _ , D_cluster _ -> (-1)
        | D_cluster _ , D_coqlib _ -> 1
        | D_cluster c1 , D_cluster c2 -> Definitions.cluster_compare c1 c2
        | D_coqlib c1 , D_coqlib c2 -> String.compare c1.c_id c2.c_id
    end)

type included = string * string
(* -R <path> <name>, name possibly empty, use -I instead *)
type coqcc = {
  mutable marked : Marked.t ;
  mutable includes : included list ; (* (reversed) includes with as *)
  mutable sources : string list ;    (* (reversed) file .v to recompile *)
}

let add_include coqcc dir =
  if not (List.mem dir coqcc.includes) then coqcc.includes <- dir :: coqcc.includes

let add_source coqcc file =
  if not (List.mem file coqcc.sources) then coqcc.sources <- file :: coqcc.sources

(* Recursive assembly: some file need further dependencies *)

let rec assemble coqcc d =
  if not (Marked.mem d coqcc.marked) then
    begin
      coqcc.marked <- Marked.add d coqcc.marked ;
      match d with
      | D_cluster cluster -> assemble_cluster coqcc cluster
      | D_coqlib clib -> assemble_coqlib coqcc clib
    end

and assemble_cluster coqcc c =
  let (age,deps) = try CLUSTERS.find c with Not_found -> (-1,[]) in
  let deps =
    if age < cluster_age c then
      let deps = write_cluster c in
      CLUSTERS.update c (cluster_age c , deps) ; deps
    else deps in
  List.iter (assemble coqcc) deps ;
  add_source coqcc (cluster_file c)

and assemble_coqlib coqcc c =
  let compiled = Printf.sprintf "%s/%so" c.c_source c.c_file in
  if Sys.file_exists compiled then
    let dir = Printf.sprintf "%s/%s" c.c_source c.c_path in
    add_include coqcc (dir,c.c_name)
  else
    begin
      let tgtdir = Wp_parameters.get_output_dir "coqwp" in
      let source = Printf.sprintf "%s/%s" c.c_source c.c_file in
      let target = Printf.sprintf "%s/%s" tgtdir c.c_file in
      let dir = Printf.sprintf "%s/%s" tgtdir c.c_path in
      if need_recompile ~source ~target then
        begin
          Wp_parameters.make_output_dir dir ;
          Command.copy source target ;
        end ;
      add_include coqcc (dir,c.c_name) ;
      add_source coqcc target;
    end

(* -------------------------------------------------------------------------- *)
(* --- Assembling Goal                                                    --- *)
(* -------------------------------------------------------------------------- *)

let assemble_goal ~pid axioms prop =
  let title = Pretty_utils.to_string WpPropId.pretty pid in
  let model = Model.directory () in
  let id = WpPropId.get_propid pid in
  let file = Printf.sprintf "%s/%s.coq" model id in
  let goal = cluster ~id ~title () in
  let deps = Command.print_file file
      begin fun fmt ->
        let v = new visitor fmt goal in
        v#printf "Require Import ZArith.@\n" ;
        v#printf "Require Import Reals.@\n" ;
        v#on_library "qed" ;
        v#vgoal axioms prop ;
        let libs = Wp_parameters.CoqLibs.get () in
        if libs <> [] then
          begin
            v#section "Additional Libraries" ;
            List.iter v#add_coqfile libs ;
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
  let coqcc = { marked = Marked.empty ; includes = [] ; sources = [] } in
  List.iter (assemble coqcc) deps ;
  let includes = (model , "") :: List.rev coqcc.includes in
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

let coqide_lock = Task.mutex ()
let emacs_regexp = Str.regexp_string_case_fold "emacs"
let is_emacs cmd =
  try ignore (Str.search_forward emacs_regexp cmd 0) ; true
  with Not_found -> false

class runcoq includes source =
  let base = Filename.chop_extension source in
  let logout = base ^ "_Coq.out" in
  let logerr = base ^ "_Coq.err" in
  object(coq)

    inherit ProverTask.command "coq"
      
    method private project =
      let dir = Filename.dirname source in
      let p = Wp_parameters.CoqProject.get () in
      Command.pp_to_file (Printf.sprintf "%s/%s" dir p)
        begin fun fmt ->
          List.iter
            (fun (dir,name) ->
               if name = "" then
                 Format.fprintf fmt "-R %s ''@\n" dir
               else
                 Format.fprintf fmt "-R %s %s@\n" dir name
            ) includes ;
          Format.fprintf fmt "-arg -noglob@\n" ;
        end
    
    method private options =
      begin
        List.iter
          (fun (dir,name) ->
             if name = "" then
               coq#add ["-R";dir;""]
             else
               coq#add ["-R";dir;name]
          ) includes ;
        coq#add [ "-noglob" ] ;
      end

    method failed : 'a. 'a task =
      begin
        let name = Filename.basename source in
        Wp_parameters.feedback ~ontty:`Message "[Coq] '%s' compilation failed." name ;
        if Wp_parameters.verbose_atleast 1 then
          begin
            ProverTask.pp_file ~message:"Coqc (stdout)" ~file:logout ;
            ProverTask.pp_file ~message:"Coqc (stderr)" ~file:logerr ;
          end ;
        Task.failed "Compilation of '%s' failed." name ;
      end

    method compile =
      let cmd = Wp_parameters.CoqCompiler.get () in
      coq#set_command cmd ;
      coq#options ;
      coq#add [ source ] ;
      coq#timeout (coq_timeout ()) ;
      Task.call
        (fun () ->
           if not (Wp_parameters.Check.get ()) then
             let name = Filename.basename source in
             Wp_parameters.feedback ~ontty:`Transient
               "[Coq] Compiling '%s'." name) ()
      >>= coq#run ~logout ~logerr
      >>= fun r ->
      if r = 127 then Task.failed "Command '%s' not found" cmd
      else if r <> 0 then coq#failed
      else Task.return ()

    method check =
      let cmd = Wp_parameters.CoqCompiler.get () in
      coq#set_command cmd ;
      coq#options ;
      coq#add [ source ] ;
      coq#timeout (coq_timeout ()) ;
      coq#run ~logout ~logerr () >>= function
      | 127 -> Task.failed "Command '%s' not found" cmd
      | 0 -> Task.return true
      | 1 -> Task.return false
      | _ -> coq#failed

    method script =
      let script = Wp_parameters.Script.get () in
      if Sys.file_exists script then coq#add [ script ]
    
    method coqide =
      let coqide = Wp_parameters.CoqIde.get () in
      coq#set_command coqide ;
      if is_emacs coqide then
        begin
          coq#project ;
          coq#script ;
          coq#add [ source ] ;
        end
      else
        begin
          coq#options ;
          coq#add [ source ] ;
          coq#script ;
        end ;
      Task.sync coqide_lock (coq#run ~logout ~logerr)
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

let ontty = `Feedback

open Wpo

type coq_wpo = {
  cw_pid : WpPropId.prop_id ;
  cw_gid : string ;
  cw_goal : string ; (* filename for goal without proof *)
  cw_script : string ; (* filename for goal with proof script *)
  cw_headers : string list ; (* filename for libraries *)
  cw_includes : included list ; (* -R ... ... *)
}

let make_check w =
  Command.print_file w.cw_script
    begin fun fmt ->
      Command.pp_from_file fmt w.cw_goal ;
      Format.fprintf fmt "Proof.@\nAdmitted.@\n@." ;
    end

let make_script w script closing =
  Command.print_file w.cw_script
    begin fun fmt ->
      Command.pp_from_file fmt w.cw_goal ;
      Format.fprintf fmt "Proof.@\n%s%s@\n@." script closing ;
    end

let try_script w script closing =
  make_script w script closing ;
  (new runcoq w.cw_includes w.cw_script)#check

let check_script w =
  make_check w ;
  (new runcoq w.cw_includes w.cw_script)#check

let rec try_hints w = function
  | [] -> Task.return false
  | (kind,script,closing) :: hints ->
      Wp_parameters.feedback ~ontty "[Coq] Goal %s : %s" w.cw_gid kind ;
      try_script w script closing >>= fun succeed ->
      if succeed then
        let required,hints = WpPropId.prop_id_keys w.cw_pid in
        let keys = List.merge String.compare required hints in
        Proof.add_script w.cw_gid keys script closing ;
        Task.return true
      else
        try_hints w hints

let try_prove w =
  begin
    match Proof.script_for ~pid:w.cw_pid ~gid:w.cw_gid with
    | Some (script,closing) ->
        Wp_parameters.feedback ~ontty "[Coq] Goal %s : Saved script" w.cw_gid ;
        try_script w script closing
    | None -> Task.return false
  end
  >>= fun succeed ->
  if succeed then
    Task.return true
  else
    try_hints w (Proof.hints_for ~pid:w.cw_pid)

let try_coqide w =
  let script,closing = Proof.script_for_ide ~pid:w.cw_pid ~gid:w.cw_gid in
  make_script w script closing ;
  (new runcoq w.cw_includes w.cw_script)#coqide >>= fun st ->
  if st = 0 then
    match Proof.parse_coqproof w.cw_script with
    | None ->
        Wp_parameters.feedback "[Coq] No proof found" ;
        Task.return false
    | Some(script,closing) ->
        if Proof.is_empty script then
          begin
            Proof.delete_script w.cw_gid ;
            Task.canceled () ;
          end
        else
          begin
            let req,hs = WpPropId.prop_id_keys w.cw_pid in
            let hints = List.merge String.compare req hs in
            Proof.add_script w.cw_gid hints script closing ;
            Wp_parameters.feedback ~ontty "[Coq] Goal %s : Script" w.cw_gid ;
            try_script w script closing
          end
  else if st = 127
  then Task.failed "CoqIde command '%s' not found" (Wp_parameters.CoqIde.get ())
  else Task.failed "CoqIde exits with status %d." st

let prove_session ~mode w =
  begin
    compile_headers w.cw_includes false w.cw_headers >>=
    begin fun () ->
      match mode with
      | BatchMode -> try_prove w
      | EditMode -> try_coqide w
      | FixMode ->
          begin
            try_prove w >>> function
            | Task.Result true -> Task.return true
            | Task.Failed e -> Task.raised e
            | Task.Canceled | Task.Timeout _ | Task.Result false -> try_coqide w
          end
    end
  end
  >>= Task.call (fun r -> if r then VCS.valid else VCS.unknown)

exception Admitted_not_proved

let check_session w =
  compile_headers w.cw_includes false w.cw_headers >>=
  (fun () -> check_script w) >>> function
  | Task.Result true -> Task.return VCS.checked
  | Task.Failed e -> Task.raised e
  | Task.Canceled | Task.Timeout _ | Task.Result false ->
      Task.raised Admitted_not_proved

let prove_session ~mode w =
  if Wp_parameters.Check.get ()
  then check_session w
  else prove_session ~mode w

let prove_prop wpo ~mode ~axioms ~prop =
  let pid = wpo.po_pid in
  let gid = wpo.po_gid in
  let model = wpo.po_model in
  let script = DISK.file_goal ~pid ~model ~prover:Coq in
  let includes , headers , goal =
    Model.with_model model (assemble_goal ~pid axioms) prop
  in
  Wp_parameters.print_generated script;
  if Wp_parameters.Generate.get ()
  then Task.return VCS.no_result
  else prove_session ~mode {
      cw_pid = pid ;
      cw_gid = gid ;
      cw_goal = goal ;
      cw_script = script ;
      cw_headers = headers ;
      cw_includes = includes ;
    }

let prove_annot wpo vcq ~mode =
  Task.todo
    begin fun () ->
      let prop = GOAL.compute_proof vcq.VC_Annot.goal in
      prove_prop wpo ~mode ~axioms:None ~prop
    end

let prove_lemma wpo vca ~mode =
  Task.todo
    begin fun () ->
      let lemma = vca.VC_Lemma.lemma in
      let depends = vca.VC_Lemma.depends in
      let prop = F.p_forall lemma.l_forall lemma.l_lemma in
      let axioms = Some(lemma.l_cluster,depends) in
      prove_prop wpo ~mode ~axioms ~prop
    end

let prove_check wpo vck ~mode =
  Task.todo
    begin fun () ->
      let axioms = None in
      let prop = vck.VC_Check.goal in
      prove_prop wpo ~mode ~axioms ~prop
    end

let prove mode wpo =
  match wpo.Wpo.po_formula with
  | GoalAnnot vcq -> prove_annot wpo vcq ~mode
  | GoalLemma vca -> prove_lemma wpo vca ~mode
  | GoalCheck vck -> prove_check wpo vck ~mode
