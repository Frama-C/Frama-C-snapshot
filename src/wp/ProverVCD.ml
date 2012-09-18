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
(* --- Prover Implementation against Task API                             --- *)
(* -------------------------------------------------------------------------- *)

open Task
open VCS
open Wpo
open Wpo.VC_Legacy
type outcome = Valid of float option | Invalid | Unknown

(* -------------------------------------------------------------------------- *)
(* --- File Preparation Utilities                                         --- *)
(* -------------------------------------------------------------------------- *)

let cat files cout =
  let buffer = String.create 2048 in
  List.iter
    (fun f ->
       let cin = open_in f in
       try
         Command.bincopy buffer cin cout ;
         close_in cin
       with e ->
         close_in cin ; raise e)
    files

let export file preludes pp =
  let cout = open_out file in
  try
    cat preludes cout ;
    flush cout ;
    let fmt = Format.formatter_of_out_channel cout in
    pp fmt ;
    Format.pp_print_newline fmt () ;
    Format.pp_print_flush fmt () ;
    close_out cout ;
  with err ->
    close_out cout ;
    raise err

(* removes file extension *)
let file_without_ext file =
  try Filename.chop_extension file
  with Invalid_argument _ -> file

(* removes path directory and file extension *)
let basename_without_ext file =
  file_without_ext (Filename.basename file)

(* replace the extension by [.ext] when [file] does not match [*.ext] and [ext<>""] *)
let file_with_ext file ~ext =
  match ext with |  "" -> file
    | _ -> if not (Filename.check_suffix file ext) 
           then (file_without_ext file) ^ "." ^ ext 
	   else file

(* looking for [file] or [file.ext] when [ext]<>"" *)
let find file ~ext =
  let file = file_with_ext file ~ext in
  if Sys.file_exists file then Some file else
    if Filename.is_relative file then
      let rec lookup file = function
	| [] -> None
	| d::ds ->
	    let path = Printf.sprintf "%s/%s" d file in
	    if Sys.file_exists path then Some path else lookup file ds
      in
      lookup file (Wp_parameters.Includes.get())
    else None
	
(* managment of the file PO_log.txt generated into the output_dir *)
module Logs =
struct

  type t = {
    logout : out_channel ;
    logfmt : Format.formatter ;
    mutable logcmd : (string * string array) option ;
    mutable files : string list ;
  }

  let create logfile =
    let outc = open_out logfile in
    let fmt = Format.formatter_of_out_channel outc in
    {
      logout = outc ;
      logfmt = fmt ;
      logcmd = None ;
      files = [] ;
    }

  let pp_command fmt cmd args =
    begin
      Format.fprintf fmt "@[<hov 4>%s" cmd ;
      Array.iter
        (fun arg -> Format.fprintf fmt "@ %s" arg)
        args ;
      Format.fprintf fmt "@]@." ;
    end

  let pp_current fmt t =
    match t.logcmd with
      | None -> ()
      | Some (cmd,args) -> pp_command fmt cmd args

  let pp_status fmt =
    function
      | Task.Result res ->
          if res <> 0 then Format.fprintf fmt "Exit [%d]@." res
      | Task.Canceled ->
          Format.fprintf fmt "Canceled@."
      | Task.Timeout ->
          Format.fprintf fmt "Timeout@."
      | Task.Failed exn ->
          Format.fprintf fmt "Failed \"%s\"@." (Printexc.to_string exn)

  let command t cmd args =
    begin
      t.logcmd <- Some (cmd,args) ;
      pp_command t.logfmt cmd args ;
    end

  let add_file t f = t.files <- f :: t.files

  let log_status t st = Format.fprintf t.logfmt "Run %a" pp_status st

  let is_error ?(status=[0]) = function
    | Task.Result r -> not (List.mem r status)
    | Task.Timeout | Task.Canceled -> false
    | Task.Failed _ -> true

  let output t ?status (stdout:Buffer.t) st =
    let msg = Buffer.contents stdout in
    begin
      log_status t st ;
      Format.pp_print_string t.logfmt msg ;
      Format.pp_print_newline t.logfmt () ;
      if is_error ?status st then
        begin
          let cname = match t.logcmd with None -> "<?>" | Some (cmd,_) -> cmd in
          Wp_parameters.error "command '%s' failed." cname ;
	  if not !Config.is_gui then
            Log.print_on_output (fun fmt -> Format.fprintf fmt "%a%s" pp_current t msg) ;
        end
    end

  let printf logs msg = Format.fprintf logs.logfmt msg

  let clean files =
    if not (Wp_parameters.is_out ()) then
      List.iter
        (fun file ->
           try if Sys.file_exists file then Unix.unlink file ;
           with Unix.Unix_error(err,_,_) ->
             Wp_parameters.debug ~dkey:"tmp" "removing tmp file '%s':@ %s"
               file (Unix.error_message err)
        ) files

  let close_out t =
    try
      Format.pp_print_flush t.logfmt () ;
      Pervasives.close_out t.logout ;
    with exn ->
      Wp_parameters.failure "Can not close log file (%s)"
        (Printexc.to_string exn)

  let close t =
    begin
      close_out t ;
      clean t.files ;
      t.files <- [] ;
      t.logcmd <- None ;
    end

  let output_and_close t ?status (stdout:Buffer.t) st =
    output t ?status stdout st ; close t

  let valid time = Valid (if !time > 0.0 then Some !time else None)

end

(* -------------------------------------------------------------------------- *)
(* --- Why & Other SMT                                                    --- *)
(* -------------------------------------------------------------------------- *)

module PO =
struct

  let files_for_lib lang = 
    let llib = 
      match lang with
	| L_altergo -> Wp_parameters.AltErgoLibs.get ()
	| _ -> Wp_parameters.WhyLibs.get () 
    in 
    List.map (fun lib ->
      match find lib ~ext:"why" with
	| None -> Wp_parameters.abort "Library file '%s.why' not found" (basename_without_ext lib)
	| Some path -> path
    ) llib
	
  let make language w d =
    let gfile = VC_Legacy.file_for_po ~gid:w.po_gid language in
    let fmodel = VC_Legacy.file_for_model ~model:d.VC_Legacy.mid language in
    let fenv = VC_Legacy.file_for_env ~env:d.VC_Legacy.env language in
    let fgoal = VC_Legacy.file_for_goal ~gid:w.po_gid language in
    let flibs = files_for_lib language in
    export gfile 
      ( [fmodel;fenv] @ flibs @ [fgoal] )
      (fun _fmt -> ()) ;
    gfile

end

module SMT =
struct

  type t = {
    ext : string ; (* file extension *)
    why : string array ; (* why options *)
    wdp : string array ; (* why-dp options *)
  }

  let yices = {
    ext = "smt" ;
    why = [| "--smtlib" |] ;
    wdp = [| "-smt-solver" ; "yices" |] ;
  }

  let cvc3 = {
    ext = "smt" ;
    why = [| "--smtlib" |] ;
    wdp = [| "-smt-solver" ; "cvc3" |] ;
  }

  let z3 = {
    ext = "z3.smt" ;
    why = [| "--z3" |] ;
    wdp = [| "-smt-solver" ; "z3" |] ;
  }

  let simplify = {
    ext = "sx" ;
    why = [| "--simplify" |] ;
    wdp = [| |] ;
  }

  let vampire = {
    ext = "vp";
    why = [| "--vampire" |];
    wdp = [| |];
  }

  let altergo = {
    ext = "why" ;
    why = [| "--alt-ergo" |] ;
    wdp = [| |] ;
  }

  let zenon = {
    ext = "znn" ;
    why = [| "--zenon" |] ;
    wdp = [| |] ;
  }

  let verit = {
    ext = "smt";
    why = [| "--smtlib" |];
    wdp = [| "-smt-solver"; "verit" |];
  }

  let of_name dp =
    match String.lowercase dp with
      | "yices" -> yices
      | "cvc3" -> cvc3
      | "z3" -> z3
      | "vampire" -> vampire
      | "simplify" -> simplify
      | "alt-ergo" -> altergo
      | "zenon" -> zenon
      | "verit" -> verit
      | _ -> Wp_parameters.abort "Unknown prover '%s'" dp

  let translate logs smt w vcd =
    let goal = PO.make L_why w vcd in
    Logs.add_file logs goal ;
    let base = Filename.chop_suffix goal ".why" in
    let file = Printf.sprintf "%s_why.%s" base smt.ext in
    Logs.add_file logs file ;
    let args = Array.append smt.why [| goal |] in
    let stdout = Buffer.create 512 in
    Logs.command logs "why" args ;
    Task.command ~stdout "why" args
    >>? Logs.output logs stdout
    >>= fun res ->
          if res = 0 then Task.return file
          else Task.failed "Why exit %d" res

  let whydp logs smt file =
     let timeout = Wp_parameters.Timeout.get () in
    let stdout = Buffer.create 80 in
    let stderr = Buffer.create 80 in
    let my_args =
      if Wp_parameters.ProofTrace.get () then
        [| "-batch"; "-debug"; file |]
      else
        [| "-batch"; file |]
    in
    let my_args =
      if Wp_parameters.ProverSwitch.get () = "" then my_args
      else
        Array.append
          [| "-extra-switch"; Wp_parameters.ProverSwitch.get() |]
          my_args
    in
    let args = Array.append smt.wdp my_args in
    let time = ref 0.0 in
    Logs.command logs "why-dp" args ;
    Task.command ~timeout ~time ~stdout ~stderr "why-dp" args
    >>? Logs.output logs ~status:[0;1;2;3;4] stdout
    >>? Logs.output logs ~status:[0;1;2;3;4] stderr
    (* why-dp returns the output of the prover on stderr *)
    >>= fun res ->
      if res = 0 then Task.return (Logs.valid time)
      else
        if (1<= res && res <= 4) then Task.return Unknown
        else Task.failed "Why-dp exit %d" res

  let prove dp log w vcd =
    let smt = of_name dp in
    translate log smt w vcd >>= whydp log smt

  let check logs w vcd =
    let timeout = Wp_parameters.Timeout.get () in
    let stdout = Buffer.create 80 in
    let goal = PO.make L_why w vcd in
    Logs.add_file logs goal ;
    let args = [| "--type-only" ; goal|] in
    Logs.command logs "why" args;
    Task.command ~timeout ~stdout "why" args
    >>? Logs.output logs stdout
    >>= fun res ->
      if res = 0 then Task.return (Valid None) else Task.return Invalid

end

(* -------------------------------------------------------------------------- *)
(* --- Prover Alt-Ergo                                                    --- *)
(* -------------------------------------------------------------------------- *)

module AltErgo =
struct

  let valid = Str.regexp "\\bValid\\b"

  let _detect pattern response = (* Unused *)
    try ignore(Str.search_forward pattern response 0) ; true
    with Not_found -> false

  let prove logs w vcd =
    let timeout = Wp_parameters.Timeout.get () in
    let stdout = Buffer.create 80 in
    let goal = PO.make L_altergo w vcd in
    Logs.add_file logs goal ;
    let cmd = ref [goal] in
    if Wp_parameters.ProofTrace.get () then cmd := "-proof" :: !cmd ;
    let depth = Wp_parameters.Depth.get () in
    if depth > 0 then
      ( let s = string_of_int depth in
	cmd := [ "-age-limite" ; s ; "-stop" ; s ] @ !cmd ) ;
    let args = Array.of_list !cmd in
    let time = ref 0.0 in
    Logs.command logs "alt-ergo" args ;
    Task.command ~timeout ~time ~stdout "alt-ergo" args
    >>? Logs.output logs stdout
    >>= fun s ->
      if s=0 then
        let response = Buffer.contents stdout in
        try
          ignore (Str.search_forward valid response 0) ;
          Task.return (Logs.valid time)
        with Not_found ->
          Task.return Unknown
      else
        Task.failed "Alt-Ergo exit %d" s

  let check logs w vcd =
    let timeout = Wp_parameters.Timeout.get () in
    let stdout = Buffer.create 80 in
    let goal = PO.make L_altergo w vcd in
    Logs.add_file logs goal ;
    let args = [| "-type-only"; goal |] in
    Logs.command logs "alt-ergo" args ;
    Task.command ~timeout ~stdout "alt-ergo" args
    >>? Logs.output logs stdout
    >>= fun s ->
      if (s = 0) then Task.return (Valid None)
      else Task.return Invalid

end


(* -------------------------------------------------------------------------- *)
(* --- Prover Coq                                                         --- *)
(* -------------------------------------------------------------------------- *)

module Coq =
struct

  let compiled : (string,unit Task.shared) Hashtbl.t = Hashtbl.create 83

  let once key cc =
    try Task.share (Hashtbl.find compiled key)
    with Not_found ->
      let s = Task.shared ~descr:key ~retry:true cc in
      Hashtbl.add compiled key s ; Task.share s

  let result_compile descr file r =
    if r<>0
    then Task.failed "Compilation of '%s' failed" file
    else 
      ( Wp_parameters.feedback "[Coq] '%s.v' compiled." descr ; Task.return () )

  let coq_timeout () =
    let coqtimeout = Wp_parameters.CoqTimeout.get () in
    let gentimeout = Wp_parameters.Timeout.get () in
    max coqtimeout gentimeout

  let require_wp logs =
    Logs.printf logs "Compile 'wp.vo' in background.@." ;
    once "wp"
      (fun () ->
         let denv = Wp_parameters.get_output () in
         let shared = Wp_parameters.Share.file "wp.v" in
         let work = denv ^ "/wp.v" in
	 let logfile = denv ^ "/wp_coq.txt" in
         Command.copy shared work ;
	 let logs = Logs.create logfile in
         let args = [| "-noglob" ; work |] in
         let stdout = Buffer.create 512 in
         let timeout = coq_timeout () in
         Logs.command logs "coqc" args ;
         Task.command ~timeout ~stdout "coqc" args
	 >>? Logs.output_and_close logs stdout
	 >>= result_compile "wp" work)

  let require_model logs model =
    Logs.printf logs "Compile '%s.vo' in background.@." model ;
    once model
      (fun () ->
         let file = VC_Legacy.file_for_model ~model L_coq in
         let fcoq = VC_Legacy.coqc_for_model ~model in
	 let flog = VC_Legacy.coqlog_for_model ~model in
         Command.copy file fcoq ;
         let denv = Wp_parameters.get_output () in
         let args = [| "-noglob" ; "-I" ; denv ; fcoq |] in
         let stdout = Buffer.create 512 in
         let timeout = coq_timeout () in
	 let logs = Logs.create flog in
         Logs.command logs "coqc" args ;
         Task.command ~timeout ~stdout "coqc" args
         >>? Logs.output_and_close logs stdout
         >>= result_compile model fcoq)

  let require_env logs env =
    Logs.printf logs "Compile '%s.vo' in background.@." env ;
    once env
      (fun () ->
         let denv = Wp_parameters.get_output () in
         let file = VC_Legacy.file_for_env ~env L_coq in
	 let flog = VC_Legacy.coqlog_for_env ~env in
         let args =  [| "-noglob"; "-I" ; denv ; file |] in
         let stdout = Buffer.create 512 in
         let timeout = coq_timeout () in
	 let logs = Logs.create flog in
         Logs.command logs "coqc" args ;
         Task.command ~timeout ~stdout "coqc" args
         >>? Logs.output_and_close logs stdout
         >>= result_compile env file)

  let require_lib logs lib =
    let name = basename_without_ext lib in
    Logs.printf logs "Compile '%s.vo' in background.@." name ;
    once name
      (fun () ->
	match find lib ~ext:"v" with
	  | Some path ->
	    let dir = Wp_parameters.get_output () in
	    let file = Printf.sprintf "%s/%s.v" dir name in
	    Command.copy path file ;
	    let flog = Printf.sprintf "%s/%s_coq.txt" dir name in
	    let args =  [| "-noglob"; "-I" ; dir ; file |] in
	    let stdout = Buffer.create 512 in
	    let timeout =  coq_timeout () in
	    let logs = Logs.create flog in
	    Logs.command logs "coqc" args ;
	    Task.command ~timeout ~stdout "coqc" args
	    >>? Logs.output_and_close logs stdout
	    >>= result_compile name file
	  | None ->
	    Wp_parameters.warning ~once:true ~current:false "[Coq] '%s' not found." lib ;
	    Task.failed "Library '%s.v' not found" lib)

  let rec require_libs logs = function
    | [] -> Task.return ()
    | lib::libs -> require_lib logs lib >>= fun () -> require_libs logs libs
	
  let require_coqlibs logs = require_libs logs (Wp_parameters.CoqLibs.get())

  type proof = Admitted | CoqProof of string

  let make_proof_file logs w vcd proof =
    let gid = w.po_gid in
    let env = vcd.env in
    let model = vcd.mid in
    let goal = VC_Legacy.file_for_goal ~gid L_coq in
    let pfile = VC_Legacy.file_for_po ~gid L_coq in
    Command.pp_to_file pfile
      (fun fmt ->
         Format.fprintf fmt "Require Import Reals.@\n";
         Format.fprintf fmt "Require Import wp.@\n";
         Format.fprintf fmt "Require Import %s.@\n" (VC_Legacy.coq_for_model ~model);
         Format.fprintf fmt "Require Import %s.@\n" (VC_Legacy.coq_for_env ~env);
	 List.iter
	   (fun lib ->
	      Format.fprintf fmt "Require Import %s.@\n" (Filename.basename lib)
	   ) (Wp_parameters.CoqLibs.get()) ;
         Command.pp_from_file fmt goal ;
         begin
           match proof with
             | CoqProof script ->
                 Format.fprintf fmt "Proof.@\n%sQed.@\n@." script ;
             | Admitted -> (* This case is for check*)
                 Format.fprintf fmt "Admitted.@\n@." ;
         end
      ) ;
    Logs.add_file logs pfile ;
    Task.return pfile

  let coqc logs w vcd script =
    make_proof_file logs w vcd script >>=
      fun fgoal ->
        let fgoal = Filename.chop_suffix fgoal ".v" in
        Logs.add_file logs (fgoal ^ ".vo") ;
        let denv = Wp_parameters.get_output() in
        let timeout = coq_timeout () in
        let stdout = Buffer.create 512 in
        let args = [|  "-noglob";"-I" ; denv ; "-compile"; fgoal |] in
        Logs.command logs "coqtop" args ;
        Task.command ~timeout ~stdout "coqtop" args
        >>? Logs.output logs ~status:[0;1] stdout

  let rec try_coqc logs w vcd = function
    | [] -> Task.return None
    | (kind,script) :: others ->
	Wp_parameters.feedback "[Coq] Goal %s : %s" w.po_gid kind ;
	coqc logs w vcd (CoqProof script) >>= fun r ->
	  if r = 0 then Task.return (Some script) else
	    try_coqc logs w vcd others
	    
  let coq_headers logs vcd =
    require_wp logs >>= fun () -> 
      require_model logs vcd.mid >>= fun () -> 
	require_env logs vcd.env >>= fun () -> 
	  require_coqlibs logs

  let prove logs w vcd =
    let pid = w.po_pid in
    let gid = w.po_gid in
    coq_headers logs vcd >>= fun () ->
      begin
	match Proof.script_for ~gid ~pid with
	  | Some s -> coqc logs w vcd (CoqProof s)
	  | None -> Task.return 1
      end >>= fun r ->
	if r = 0 then Task.return (Valid None)
	else
	  try_coqc logs w vcd (Proof.hints_for ~pid) >>= 
	    function
	      | Some script ->
		  let required,hints = WpPropId.prop_id_keys pid in
		  let keys = List.merge String.compare required hints in
		  Proof.add_script gid keys script ; 
		  Task.return (Valid None)
	      | None ->
		  Task.return Unknown
	  
  let check log w vcd =
    coq_headers log vcd >>= fun () ->
      coqc log w vcd Admitted >>= 
	Task.call (fun r -> if r=0 then (Valid None) else Invalid)

end

module Coqide =
struct

  let coqidelock = Task.mutex ()

  let run_coqide logs w vcd proof =
    let denv = Wp_parameters.get_output() in
    let gid = w.po_gid in
    let coqide () =
      let head  = VC_Legacy.file_for_head gid in
      let env   = VC_Legacy.file_for_env vcd.env L_coq in
      let model = VC_Legacy.coqc_for_model ~model:vcd.mid in
      let wp_share = Wp_parameters.Share.file "wp.v" in
      let coqincludes = "-I" :: denv :: 
	(List.fold_left
	   (fun args lib -> "-I"::lib::args) 
	   [] (Wp_parameters.Includes.get()) )
      in
      let coqsources =
	[ proof ; head ; env ; model ; wp_share ]
      in
      let coqscripts =
	let fscript = Wp_parameters.Script.get() in
	if Sys.file_exists fscript then [fscript] else []
      in
      let args = Array.of_list ( coqincludes @ coqsources @ coqscripts ) in
      Logs.command logs "coqide" args ;
      Task.command "coqide" args
      >>? Logs.log_status logs
    in
    Task.sync coqidelock coqide >>=
      fun s ->
        if s <> 0 then Task.return Unknown
        else
          match Proof.parse_coqproof proof with
            | None ->
                Wp_parameters.feedback "[Coq] No proof found" ;
                Task.return Unknown
            | Some script ->
		if Proof.is_empty script then
		  ( Proof.delete_script gid ; Task.canceled () )
		else
		  let req,hs = WpPropId.prop_id_keys w.po_pid in
		  let hints = List.merge String.compare req hs in
                  Proof.add_script gid hints script ;
                  Wp_parameters.feedback "[Coq] Goal %s : Script" w.po_gid ;
                  Coq.coqc logs w vcd (Coq.CoqProof script) >>=
                    Task.call (fun s -> if s=0 then (Valid None) else Unknown)

  let prove log w vcd =
    Coq.prove log w vcd >>> function
      | Task.Result (Valid _ as v) -> Task.return v
      | Task.Failed e -> Task.raised e
      | Task.Canceled | Task.Timeout | Task.Result (Unknown | Invalid) ->
	  let script = Proof.script_for_ide ~pid:w.po_pid ~gid:w.po_gid in
          Coq.make_proof_file log w vcd (Coq.CoqProof script) 
	  >>= run_coqide log w vcd

end

(* -------------------------------------------------------------------------- *)
(* --- Main Dispatcher                                                    --- *)
(* -------------------------------------------------------------------------- *)

let task_for interactive = function
  | AltErgo -> AltErgo.prove
  | Why dp -> SMT.prove dp
  | Coq -> if interactive then Coqide.prove else Coq.prove
  | WP -> fun _ _ _ -> Task.return Unknown

let result = function
  | Task.Result (Valid time) -> VCS.result ?time VCS.Valid
  | Task.Result Invalid -> VCS.invalid
  | Task.Result Unknown -> VCS.unknown
  | Task.Canceled -> VCS.no_result
  | Task.Timeout -> VCS.timeout
  | Task.Failed exn -> VCS.failed (Printexc.to_string exn)

let prove wpo vcd ?(interactive=false) prover =
  Task.todo
    begin
      fun () ->
	let logf = VC_Legacy.file_for_log_proof ~gid:wpo.po_gid prover in
	let logs = Logs.create logf in
	task_for interactive prover logs wpo vcd
	>>> fun s ->
	  Logs.close logs ;
	  Task.return (result s)
    end

let check_by = function
  | L_altergo -> AltErgo.check
  | L_why -> SMT.check
  | L_coq -> Coq.check

let check wpo vcd ?(callback=fun _ _ _ -> ()) lang =
  Task.todo
    begin
      fun () ->
        let logf = VC_Legacy.file_for_log_check ~gid:wpo.po_gid lang in
        let log = Logs.create logf in
        check_by lang log wpo vcd
        >>! fun s ->
          let r = result s in
          Logs.close log ;
          callback wpo lang r
    end
