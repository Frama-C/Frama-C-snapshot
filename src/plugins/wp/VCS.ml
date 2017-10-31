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

(* -------------------------------------------------------------------------- *)
(* --- Provers                                                            --- *)
(* -------------------------------------------------------------------------- *)

let dkey_no_time_info = Wp_parameters.register_category "no-time-info"
let dkey_no_step_info = Wp_parameters.register_category "no-step-info"
let dkey_no_goals_info = Wp_parameters.register_category "no-goals-info"

type prover =
  | Why3 of string (* Prover via WHY *)
  | Why3ide
  | AltErgo       (* Alt-Ergo *)
  | Coq           (* Coq and Coqide *)
  | Qed           (* Qed Solver *)
  | Tactical      (* Interactive Prover *)

type mode =
  | BatchMode (* Only check scripts *)
  | EditMode  (* Edit then check scripts *)
  | FixMode   (* Try check script, then edit script on non-success *)

type language =
  | L_why3
  | L_coq
  | L_altergo

let prover_of_name = function
  | "" | "none" -> None
  | "qed" | "Qed" -> Some Qed
  | "alt-ergo" | "altgr-ergo" -> Some AltErgo
  | "coq" | "coqide" -> Some Coq
  | "script" -> Some Tactical
  | "tip" -> Some Tactical
  | "why3ide" -> Some Why3ide
  | s ->
      match Extlib.string_del_prefix "why3:" s with
      | Some "" -> None
      | Some "ide" -> Some Why3ide
      | Some s' -> Some (Why3 s')
      | None -> Some (Why3 s)

let name_of_prover = function
  | Why3ide -> "why3ide"
  | Why3 s -> "why3:" ^ s
  | AltErgo -> "alt-ergo"
  | Coq -> "coq"
  | Qed -> "qed"
  | Tactical -> "script"

let title_of_prover = function
  | Why3ide -> "Why3"
  | Why3 s -> s
  | AltErgo -> "Alt-Ergo"
  | Coq -> "Coq"
  | Qed -> "Qed"
  | Tactical -> "Script"

let title_of_mode = function
  | FixMode -> "Fix"
  | EditMode -> "Edit"
  | BatchMode -> "Batch"

let sanitize_why3 s =
  let buffer = Buffer.create 80 in
  assert (s <> "ide");
  Buffer.add_string buffer "Why3_" ;
  String.iter
    (fun c ->
       let c = if
         ('0' <= c && c <= '9') ||
         ('a' <= c && c <= 'z') ||
         ('A' <= c && c <= 'Z')
         then c else '_'
       in Buffer.add_char buffer c) s ;
  Buffer.contents buffer

let filename_for_prover = function
  | Why3 s -> sanitize_why3 s
  | Why3ide -> "Why3_ide"
  | AltErgo -> "Alt-Ergo"
  | Coq -> "Coq"
  | Qed -> "Qed"
  | Tactical -> "Tactical"

let language_of_name = function
  | "" | "none" -> None
  | "alt-ergo" | "altgr-ergo" -> Some L_altergo
  | "coq" | "coqide"-> Some L_coq
  | "why" -> Some L_why3
  | s -> Wp_parameters.abort "Language '%s' unknown" s

let language_of_prover = function
  | Why3 _ -> L_why3
  | Why3ide -> L_why3
  | Coq -> L_coq
  | AltErgo -> L_altergo
  | Qed | Tactical -> L_why3

let language_of_prover_name = function
  | "" | "none" -> None
  | "alt-ergo" | "altgr-ergo" -> Some L_altergo
  | "coq" | "coqide" -> Some L_coq
  | _ -> Some L_why3

let mode_of_prover_name = function
  | "coqedit" -> EditMode
  | "coqide" | "altgr-ergo" | "tactical" -> FixMode
  | _ -> BatchMode

let is_auto = function
  | Qed | AltErgo | Why3 _ -> true
  | Tactical | Why3ide | Coq -> false

let cmp_prover p q =
  match p,q with
  | Qed , Qed -> 0
  | Qed , _ -> (-1)
  | _ , Qed -> 1
  | AltErgo , AltErgo -> 0
  | AltErgo , _ -> (-1)
  | _ , AltErgo -> 1
  | Tactical , Tactical -> 0
  | Tactical , _ -> (-1)
  | _ , Tactical -> 1
  | Coq , Coq -> 0
  | Coq , _ -> (-1)
  | _ , Coq -> 1
  | Why3 p , Why3 q -> String.compare p q
  | Why3 _, _ -> (-1)
  | _, Why3 _ ->   1
  | Why3ide, Why3ide -> 0

let pp_prover fmt = function
  | AltErgo -> Format.pp_print_string fmt "Alt-Ergo"
  | Why3ide -> Format.pp_print_string fmt "Why3ide"
  | Coq -> Format.pp_print_string fmt "Coq"
  | Why3 smt ->
      if Wp_parameters.debug_atleast 1 then
        Format.pp_print_string fmt ("Why:"^smt)
      else
        Format.pp_print_string fmt smt
  | Qed -> Format.fprintf fmt "Qed"
  | Tactical -> Format.pp_print_string fmt "Tactical"

let pp_language fmt = function
  | L_altergo -> Format.pp_print_string fmt "Alt-Ergo"
  | L_coq -> Format.pp_print_string fmt "Coq"
  | L_why3 -> Format.pp_print_string fmt "Why3"

let pp_mode fmt m = Format.pp_print_string fmt (title_of_mode m)

module P = struct type t = prover let compare = cmp_prover end
module Pset = Set.Make(P)
module Pmap = Map.Make(P)

(* -------------------------------------------------------------------------- *)
(* --- Config                                                             --- *)
(* -------------------------------------------------------------------------- *)

type config = {
  valid : bool ;
  timeout : int option ;
  stepout : int option ;
  depth : int option ;
}

let param f = let v = f() in if v>0 then Some v else None

let current () = {
  valid = false ;
  timeout = param Wp_parameters.Timeout.get ;
  stepout = param Wp_parameters.Steps.get ;
  depth = param Wp_parameters.Depth.get ;
}

let default = { valid = false ; timeout = None ; stepout = None ; depth = None }

let get_timeout = function
  | { timeout = None } -> Wp_parameters.Timeout.get ()
  | { timeout = Some t } -> t

let get_stepout = function
  | { stepout = None } -> Wp_parameters.Steps.get ()
  | { stepout = Some t } -> t

let get_depth = function
  | { depth = None } -> Wp_parameters.Depth.get ()
  | { depth = Some t } -> t

(* -------------------------------------------------------------------------- *)
(* --- Results                                                            --- *)
(* -------------------------------------------------------------------------- *)

type verdict =
  | NoResult
  | Invalid
  | Unknown
  | Timeout
  | Stepout
  | Computing of (unit -> unit) (* kill function *)
  | Checked
  | Valid
  | Failed

type result = {
  verdict : verdict ;
  solver_time : float ;
  prover_time : float ;
  prover_steps : int ;
  prover_depth : int ;
  prover_errpos : Lexing.position option ;
  prover_errmsg : string ;
}

let is_verdict r = match r.verdict with
  | Valid | Checked | Unknown | Invalid | Timeout | Stepout | Failed -> true
  | NoResult | Computing _ -> false

let is_valid r = r.verdict = Valid

let configure r =
  let valid = (r.verdict = Valid) in
  let timeout =
    let t = r.prover_time in
    if t > 0.0 then
      let timeout = Wp_parameters.Timeout.get() in
      let margin = Wp_parameters.TimeExtra.get() + int_of_float (t +. 0.5) in
      Some(max timeout margin)
    else
      None in
  let stepout =
    if r.prover_steps > 0 && r.prover_time <= 0.0 then
      let stepout = Wp_parameters.Steps.get () in
      let margin = 1000 + r.prover_depth in
      Some(max stepout margin)
    else None in
  let depth =
    if r.prover_depth > 0 then Some r.prover_depth else None
  in
  {
    valid ;
    timeout ;
    stepout ;
    depth ;
}

let time_fits t =
  t = 0.0 ||
  let timeout = Wp_parameters.Timeout.get () in
  timeout = 0 ||
  let margin = Wp_parameters.TimeMargin.get () in
  t < float (timeout - margin)

let step_fits n =
  n = 0 ||
  let stepout = Wp_parameters.Steps.get () in
  stepout = 0 || n < stepout

let depth_fits n =
  n = 0 ||
  let depth = Wp_parameters.Depth.get () in
  depth = 0 || n < depth

let autofit r =
  time_fits r.prover_time &&
  step_fits r.prover_steps &&
  depth_fits r.prover_depth 

let result ?(solver=0.0) ?(time=0.0) ?(steps=0) ?(depth=0) verdict =
  {
    verdict ;
    solver_time = solver ;
    prover_time = time ;
    prover_steps = steps ;
    prover_depth = depth ;
    prover_errpos = None ;
    prover_errmsg = "" ;
  }

let no_result = result NoResult
let valid = result Valid
let checked = result Checked
let invalid = result Invalid
let unknown = result Unknown
let timeout t = result ~time:(float t) Timeout
let stepout = result Stepout
let computing kill = result (Computing kill)
let failed ?pos msg = {
  verdict = Failed ;
  solver_time = 0.0 ;
  prover_time = 0.0 ;
  prover_steps = 0 ;
  prover_depth = 0 ;
  prover_errpos = pos ;
  prover_errmsg = msg ;
}

let kfailed ?pos msg = Pretty_utils.ksfprintf (failed ?pos) msg

let perfo extended dkey = extended || not (Wp_parameters.has_dkey dkey)

let pp_perf ~extended fmt r =
  begin
    let t = r.solver_time in
    if t > Rformat.epsilon && perfo extended dkey_no_time_info
    then Format.fprintf fmt " (Qed:%a)" Rformat.pp_time t ;
    let t = r.prover_time in
    if t > Rformat.epsilon && perfo extended dkey_no_time_info
    then Format.fprintf fmt " (%a)" Rformat.pp_time t ;
    let s = r.prover_steps in
    if s > 0 && perfo extended dkey_no_step_info
    then Format.fprintf fmt " (%d)" s
  end

let pp_res ~extended fmt r =
  match r.verdict with
  | NoResult -> Format.pp_print_string fmt (if extended then "No Result" else "-")
  | Invalid -> Format.pp_print_string fmt "Invalid"
  | Computing _ -> Format.pp_print_string fmt "Computing"
  | Valid -> Format.fprintf fmt "Valid%a" (pp_perf ~extended) r
  | Checked -> Format.fprintf fmt "Typechecked"
  | Unknown -> Format.fprintf fmt "Unknown%a" (pp_perf ~extended) r
  | Timeout -> Format.fprintf fmt "Timeout%a" (pp_perf ~extended) r
  | Stepout -> Format.fprintf fmt "Step limit%a" (pp_perf ~extended) r
  | Failed -> Format.fprintf fmt "Failed@ %s" r.prover_errmsg

let pp_result = pp_res ~extended:false
let pp_result_perf = pp_res ~extended:true

let compare p q =
  let rank = function
    | NoResult | Computing _ -> 0
    | Failed -> 1
    | Unknown -> 2
    | Timeout | Stepout -> 3
    | Valid -> 4
    | Invalid -> 5
    | Checked -> 6
  in
  let r = rank q.verdict - rank p.verdict in
  if r <> 0 then r else
    let s = Pervasives.compare p.prover_steps q.prover_steps in
    if s <> 0 then s else
      let t = Pervasives.compare p.prover_time q.prover_time in
      if t <> 0 then t else
        Pervasives.compare p.solver_time q.solver_time
