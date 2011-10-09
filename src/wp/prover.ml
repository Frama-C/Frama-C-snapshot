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
(* --- Prover Implementation against Task API                             --- *)
(* -------------------------------------------------------------------------- *)

open Task
open Wpo
type verdict = Valid | Invalid | Unknown

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

(* managment of the file PO_log.txt generated into the output_dir *)
module Logs =
struct

  type t = {
    logid : string ;
    logout : out_channel ;
    logfmt : Format.formatter ;
    mutable logcmd : (string * string array) option ;
    mutable files : string list ;
  }

  let create w logfile =
    let outc = open_out logfile in
    let fmt = Format.formatter_of_out_channel outc in
    {
      logid = w.po_gid ;
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
    | Task.Canceled -> false
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

end

(* -------------------------------------------------------------------------- *)
(* --- Why & Other SMT                                                    --- *)
(* -------------------------------------------------------------------------- *)

module PO =
struct

  let make language w =
    let gfile = Wpo.file_for_po ~gid:w.po_gid language in
    export gfile [
      file_for_model ~model:w.po_model language ;
      file_for_env   ~env:w.po_env language ;
      file_for_goal  ~gid:w.po_gid language ;
    ] (fun _fmt -> ()) ;
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

  let of_name dp =
    match String.lowercase dp with
      | "yices" -> yices
      | "cvc3" -> cvc3
      | "z3" -> z3
      | "vampire" -> vampire
      | "simplify" -> simplify
      | "alt-ergo" -> altergo
      | "zenon" -> zenon
      | _ -> Wp_parameters.abort "Unknown prover '%s'" dp

  let translate logs smt w =
    let goal = PO.make L_why w in
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
    let args = Array.append smt.wdp my_args in
    Logs.command logs "why-dp" args ;
    Task.command ~timeout ~stdout ~stderr "why-dp" args
    >>? Logs.output logs ~status:[0;1;2;3;4] stdout
    >>? Logs.output logs stderr
    (* why-dp returns the output of the prover on stderr *)
    >>= fun res ->
      if res = 0 then Task.return Valid
      else
        if (1<= res && res <= 4) then Task.return Unknown
        else Task.failed "Why-dp exit %d" res

  let prove dp log w =
    let smt = of_name dp in
    translate log smt w >>= whydp log smt

  let check logs w =
    let timeout = Wp_parameters.Timeout.get () in
    let stdout = Buffer.create 80 in
    let goal = PO.make L_why w in
    Logs.add_file logs goal ;
    let args = [| "--type-only" ; goal|] in
    Logs.command logs "why" args;
    Task.command ~timeout ~stdout "why" args
    >>? Logs.output logs stdout
    >>= fun res ->
      if res = 0 then Task.return Valid else Task.return Invalid

end

(* -------------------------------------------------------------------------- *)
(* --- Prover Alt-Ergo                                                    --- *)
(* -------------------------------------------------------------------------- *)

module AltErgo =
struct

  let valid = Str.regexp "\\bValid\\b"

  let prove logs w =
    let timeout = Wp_parameters.Timeout.get () in
    let stdout = Buffer.create 80 in
    let goal = PO.make L_altergo w in
    Logs.add_file logs goal ;
    let args =
      if Wp_parameters.ProofTrace.get ()
      then [| "-proof" ; goal |]
      else [| goal |]
    in
    Logs.command logs "alt-ergo" args ;
    Task.command ~timeout ~stdout "alt-ergo" args
    >>? Logs.output logs stdout
    >>= fun s ->
      if s=0 then
        let response = Buffer.contents stdout in
        try
          ignore (Str.search_forward valid response 0) ;
          Task.return Valid
        with Not_found ->
          Task.return Unknown
      else
        Task.failed "Alt-Ergo exit %d" s

  let check logs w =
    let timeout = Wp_parameters.Timeout.get () in
    let stdout = Buffer.create 80 in
    let goal = PO.make L_altergo w in
    Logs.add_file logs goal ;
    let args = [| "-type-only"; goal |] in
    Logs.command logs "alt-ergo" args ;
    Task.command ~timeout ~stdout "alt-ergo" args
    >>? Logs.output logs stdout
    >>= fun s ->
      if (s = 0) then Task.return Valid
      else Task.return Invalid

end


(* -------------------------------------------------------------------------- *)
(* --- Prover Coq                                                         --- *)
(* -------------------------------------------------------------------------- *)


module Coq =
struct

  let compiled : (string,unit Task.task) Hashtbl.t = Hashtbl.create 83
  let revert_on_error key = function
    | Task.Result _ -> ()
    | _ -> Hashtbl.remove compiled key
  let once key cc =
    try Hashtbl.find compiled key
    with Not_found ->
      let t = Task.nop >>= cc >>? revert_on_error key in
      Hashtbl.add compiled key t ; t

  let result_compile file r =
    if r<>0
    then Task.failed "Compilation of '%s' failed" file
    else Task.return ()

  let require_wp logs =
    once "wp"
      (fun () ->
         let denv = Wp_parameters.get_output () in
         let dshare = Wp_parameters.get_share() in
         let shared = Format.sprintf "%s/wp.v" dshare in
         let work = Format.sprintf "%s/wp.v" denv in
         Command.copy shared work ;
         (* no added ! -> incremental compilation *)
         let args = [| "-noglob" ; work |] in
         let stdout = Buffer.create 512 in
         let timeout = Wp_parameters.Timeout.get() in
         Logs.command logs "coqc" args ;
         Task.command ~timeout ~stdout "coqc" args
         >>? Logs.output logs stdout
         >>= result_compile work)

  let require_model logs w =
    let model = w.po_model in
    once model
      (fun () ->
         let file = Wpo.file_for_model ~model L_coq in
         let fcoq = Wpo.coqc_for_model ~model in
         Command.copy file fcoq ;
         (* not added ! -> incremental compilation *)
         let denv = Wp_parameters.get_output () in
         let args = [| "-noglob" ; "-I" ; denv ; fcoq |] in
         let stdout = Buffer.create 512 in
         let timeout =  Wp_parameters.Timeout.get() in
         Logs.command logs "coqc" args ;
         Task.command ~timeout ~stdout "coqc" args
         >>? Logs.output logs stdout
         >>= result_compile fcoq)

  let require_env logs w =
    let env = w.po_env in
    once env
      (fun () ->
         let denv = Wp_parameters.get_output () in
         let file = file_for_env env L_coq in
         (* not added ! -> incremental compilation *)
         let args =  [| "-noglob"; "-I" ; denv ; file |] in
         let stdout = Buffer.create 512 in
         let timeout =  Wp_parameters.Timeout.get() in
         Logs.command logs "coqc" args ;
         Task.command ~timeout ~stdout "coqc" args
         >>? Logs.output logs stdout
         >>= result_compile file)

  let default =
    "  (** FILL PROOF HERE.**)\n  \
       intros; repeat(split; intros); auto;\n  \
       try contradiction; auto; eauto ; try omega.\n"

  let proof logs w script =
    let gid = w.po_gid in
    let env = w.po_env in
    let model = w.po_model in
    let goal = Wpo.file_for_goal ~gid L_coq in
    let proof = Wpo.file_for_po ~gid L_coq in
    Command.pp_to_file proof
      (fun fmt ->
         Format.fprintf fmt "Require Import Reals.@\n";
         Format.fprintf fmt "Require Import wp.@\n";
         Format.fprintf fmt "Require Import %s.@\n" (Wpo.coq_for_model ~model);
         Format.fprintf fmt "Require Import %s.@\n" (Wpo.coq_for_env ~env);
         Command.pp_from_file fmt goal ;
         begin
           match script with
             | Some script ->
                 Format.fprintf fmt "Proof.@\n%sQed.@\n@." script ;
             | None -> (* This case is for check*)
                 Format.fprintf fmt "Proof.@\nAdmitted.@\n@." ;
         end
      ) ;
    Logs.add_file logs proof ;
    Task.return proof

  let coqc logs w script =
    proof logs w script >>=
      fun fgoal ->
        let fgoal = Filename.chop_suffix fgoal ".v" in
        Logs.add_file logs (fgoal ^ ".vo") ;
        let denv = Wp_parameters.get_output() in
        let timeout = Wp_parameters.Timeout.get() in
        let stdout = Buffer.create 512 in
        let args = [|  "-noglob";"-I" ; denv ; "-compile"; fgoal |] in
        Logs.command logs "coqtop" args ;
        Task.command ~timeout ~stdout "coqtop" args
        >>? Logs.output logs ~status:[0;1] stdout

  let script_for w =
    Some
      ( match Proof.find_script_for_goal w.po_gid with
          | None -> default
          | Some script -> script )

  let prove log w =
   require_wp log
    >>= fun _ -> require_model log w
      >>= fun _ -> require_env log w
        >>= fun _ -> coqc log w (script_for w)
          >>= Task.call (fun r -> if r=0 then Valid else Unknown)

  let check log w =
    require_wp log
    >>= fun _ -> require_model log w
      >>= fun _ -> require_env log w
        >>= fun _ -> coqc log w None
          >>= Task.call (fun r -> if r=0 then Valid else Invalid)


end

module Coqide =
struct

  let coqidelock = Task.mutex ()

  let run_coqide logs w proof =
    let denv = Wp_parameters.get_output() in
    let gid = w.po_gid in
    let coqide () =
      let head  = Wpo.file_for_head gid in
      let env   = Wpo.file_for_env w.po_env L_coq in
      let model = Wpo.coqc_for_model ~model:w.po_model in
      let dshare = Wp_parameters.get_share() in
      let wp_share = Format.sprintf "%s/wp.v" dshare in
      let args =
        match Wp_parameters.Script.get() with
          | fscript when Sys.file_exists fscript ->
              [|  "-I" ; denv ; proof; head; env; model; wp_share; fscript |]
          | _ ->
              [|  "-I" ; denv ; proof; head; env; model; wp_share |]
      in
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
                Wp_parameters.feedback "No proof found" ;
                Task.return Unknown
            | Some script ->
                Wp_parameters.feedback "Check proof" ;
                Coq.coqc logs w (Some script) >>=
                  Task.call
                    (fun s ->
                       Proof.add_script gid [] script ;
                       if s=0 then Valid else Unknown)

  let prove log w =
    Coq.prove log w >>= function
      | Unknown | Invalid ->
          Coq.proof log w (Coq.script_for w) >>=
            run_coqide log w
      | Valid -> Task.return Valid

end

(* -------------------------------------------------------------------------- *)
(* --- Main Dispatcher                                                    --- *)
(* -------------------------------------------------------------------------- *)

let task_for interactive = function
  | AltErgo -> AltErgo.prove
  | Why dp -> SMT.prove dp
  | Coq -> if interactive then Coqide.prove else Coq.prove
  | WP -> fun _ _ -> Task.return Unknown

let result = function
  | Task.Result Valid -> Wpo.Valid
  | Task.Result Invalid -> Wpo.Invalid
  | Task.Result Unknown -> Wpo.Unknown
  | Task.Canceled -> Wpo.Timeout
  | Task.Failed exn -> Wpo.Failed (Printexc.to_string exn)

let prove
    ?(callin=fun _ _ -> ())
    ?(callout=fun _ _ _ -> ())
    wpo ~interactive prover
    =
  Task.todo
    begin
      fun () ->
	let logf = Wpo.file_for_log_proof ~gid:wpo.po_gid prover in
	let logs = Logs.create wpo logf in
	(callin wpo prover ;
	 task_for interactive prover logs wpo)
	  >>! fun s ->
	    let r = result s in
	    Logs.close logs ;
	    Wpo.set_result wpo prover r ;
	    callout wpo prover r
    end

let check_by = function
  | L_altergo -> AltErgo.check
  | L_why -> SMT.check
  | L_coq -> Coq.check

let check
  ?(callout=fun _ _ _ -> ())
    wpo lang
    =
  Task.todo
    begin
      fun () ->
        let logf = Wpo.file_for_log_check ~gid:wpo.po_gid lang in
        let log = Logs.create wpo logf in
        check_by lang log wpo
          >>! fun s ->
            let r = result s in
            Logs.close log ;
            callout wpo lang r
    end

let server = ref None

let server () =
  match !server with
    | Some s ->
        let procs = Wp_parameters.Procs.get () in
        Task.set_procs s procs ; s
    | None ->
        let procs = Wp_parameters.Procs.get () in
        let s = Task.server ~procs () in
        Task.on_server_stop s Proof.savescripts ;
        server := Some s ; s
