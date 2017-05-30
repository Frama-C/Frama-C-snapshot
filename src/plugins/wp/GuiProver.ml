(**************************************************************************)
(*                                                                        *)
(*  This file is part of WP plug-in of Frama-C.                           *)
(*                                                                        *)
(*  Copyright (C) 2007-2017                                               *)
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

let no_status = `Share "theme/default/never_tried.png"
let ok_status = `Share "theme/default/surely_valid.png"
let ko_status = `Share "theme/default/unknown.png"
let go_status = `Share "theme/default/valid_under_hyp.png"
let wg_status = `Share "theme/default/invalid.png"

let filter = function
  | VCS.Qed | VCS.Tactical | VCS.Why3ide | VCS.Coq -> false
  | VCS.Why3 _ | VCS.AltErgo -> true

(* -------------------------------------------------------------------------- *)
(* --- Palette Tool                                                       --- *)
(* -------------------------------------------------------------------------- *)

let timeout_for = function
  | VCS.AltErgo | VCS.Why3 _ ->
      let value = Wp_parameters.Timeout.get () in
      let spin = new Widget.spinner
        ~tooltip:"Prover Timeout (0 for none)"
        ~min:0 ~step:5 ~value () in
      Some spin
  | _ -> None

let stepout_for = function
  | VCS.AltErgo ->
      let value = Wp_parameters.Steps.get () in
      let spin = new Widget.spinner
        ~tooltip:"Prover Step Limit (0 for none)"
        ~min:0 ~step:100 ~value () in
      Some spin
  | _ -> None

let depth_for = function
  | VCS.AltErgo ->
      let value = Wp_parameters.Depth.get () in
      let spin = new Widget.spinner
        ~tooltip:"Search Depth (-age-bound, 0 for prover default)"
        ~min:0 ~step:100 ~value () in
      Some spin
  | _ -> None

let configure widget option =
  match widget , option with
  | Some spinner , Some value -> spinner#set value
  | _ -> ()

class prover ~(console:Wtext.text) ~prover =
  let tooltip = "Configure Prover" in
  let content = new Wpane.form () in
  let timeout = timeout_for prover in
  let stepout = stepout_for prover in
  let depth = depth_for prover in
  object(self)
    inherit Wpalette.tool ~tooltip ~content:content#widget ()
    initializer
      begin
        assert (filter prover) ;
        Wutil.on timeout (fun spin -> content#add_field ~label:"Timeout" spin#coerce) ;
        Wutil.on stepout (fun spin -> content#add_field ~label:"Steps" spin#coerce) ;
        Wutil.on depth (fun spin -> content#add_field ~label:"Depth" spin#coerce) ;
      end

    method prover = prover

    method private log wpo res =
      begin
        let fout = Wpo.get_file_logout wpo prover in
        let ferr = Wpo.get_file_logerr wpo prover in
        let lout = Sys.file_exists fout in
        let lerr = Sys.file_exists ferr in
        if lout || lerr then console#hrule ;
        console#scroll () ;
        console#printf "[%a] %a@." VCS.pp_prover prover VCS.pp_result res ;
        if lout then Command.pp_from_file console#fmt fout ;
        if lerr then Command.pp_from_file console#fmt ferr ;
        if lout || lerr then console#hrule ;
      end

    method private run wpo =
      begin
        let spinner = function None -> None | Some s -> Some s#get in
        let config = {
          VCS.valid = false ;
          VCS.timeout = spinner timeout ;
          VCS.stepout = spinner stepout ;
          VCS.depth = spinner depth ;
        } in
        let callback wpo prv res =
          Wpo.set_result wpo prv res ;
          self#update wpo in
        let task = Prover.prove ~config ~callback wpo prover in
        let thread = Task.thread task in
        let kill () =
          Wpo.set_result wpo prover VCS.no_result ;
          Task.cancel thread in
        Wpo.set_result wpo prover (VCS.computing kill) ;
        let server = ProverTask.server () in
        Task.spawn server thread ;
        Task.launch server ;
        self#update wpo ;
      end

    method private retry wpo ~icn ~msg ?(cfg=msg) = function
      | Some spin when spin#get > 0 ->
          begin
            self#set_status icn ;
            let value = 2 * spin#get in
            let tooltip =
              Format.asprintf "Retry Prover with extended %s (%d)" cfg value in
            let callback () = spin#set value ; self#run wpo in
            self#set_action ~tooltip ~icon:`MEDIA_FORWARD ~callback () ;
            Pretty_utils.ksfprintf self#set_label "%a (%s)" VCS.pp_prover prover msg ;
          end
      | _ ->
          begin
            self#set_status icn ;
            let callback () = self#run wpo in
            self#set_action ~tooltip:"Retry Prover" ~icon:`MEDIA_FORWARD ~callback () ;
            Pretty_utils.ksfprintf self#set_label "%a (%s)" VCS.pp_prover prover msg ;
          end

    method clear =
      begin
        self#set_status no_status ;
        self#set_action ~icon:`MEDIA_PLAY ~tooltip:"Run Prover" ?callback:None () ;
        Pretty_utils.ksfprintf self#set_label "%a" VCS.pp_prover prover ;
      end

    method update wpo =
      let res = Wpo.get_result wpo prover in
      if VCS.is_verdict res then
        begin
          let cfg = VCS.configure res in
          configure timeout cfg.VCS.timeout ;
          configure stepout cfg.VCS.stepout ;
          configure depth cfg.VCS.depth ;
        end ;
      match res.VCS.verdict with
      | VCS.NoResult ->
          let callback () = self#run wpo in
          self#set_status no_status ;
          self#set_action ~tooltip:"Run Prover" ~icon:`MEDIA_PLAY ~callback () ;
          Pretty_utils.ksfprintf self#set_label "%a" VCS.pp_prover prover ;
      | VCS.Computing signal ->
          self#set_status `EXECUTE ;
          self#set_action ~tooltip:"Interrrupt Prover" ~icon:`STOP ~callback:signal () ;
          Pretty_utils.ksfprintf self#set_label "%a (...)" VCS.pp_prover prover ;
      | VCS.Valid | VCS.Checked ->
          self#set_status ok_status ;
          self#set_action ~tooltip:"Run Prover" ~icon:`MEDIA_PLAY () ;
          Pretty_utils.ksfprintf self#set_label "%a (%a)" VCS.pp_prover prover
            Rformat.pp_time res.VCS.prover_time ;
      | VCS.Failed ->
          self#set_status `DIALOG_WARNING ;
          let callback () = self#log wpo res in
          self#set_action ~tooltip:"Dump Logs" ~icon:`FILE ~callback () ;
          Pretty_utils.ksfprintf self#set_label "%a (failed)" VCS.pp_prover prover ;
      | VCS.Invalid -> self#retry wpo ~icn:wg_status ~msg:"invalid" None
      | VCS.Unknown -> self#retry wpo ~icn:ko_status ~msg:"unknown" ~cfg:"depth" depth
      | VCS.Timeout -> self#retry wpo ~icn:`CUT ~msg:"timeout" ~cfg:"time" timeout
      | VCS.Stepout -> self#retry wpo ~icn:`CUT ~msg:"stepout" ~cfg:"steps" stepout

  end
