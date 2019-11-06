(**************************************************************************)
(*                                                                        *)
(*  This file is part of WP plug-in of Frama-C.                           *)
(*                                                                        *)
(*  Copyright (C) 2007-2019                                               *)
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
(* --- Prover Results                                                     --- *)
(* -------------------------------------------------------------------------- *)

let dkey_no_time_info = Wp_parameters.register_category "no-time-info"
let dkey_no_step_info = Wp_parameters.register_category "no-step-info"
let dkey_no_goals_info = Wp_parameters.register_category "no-goals-info"
let dkey_no_cache_info = Wp_parameters.register_category "no-cache-info"
let dkey_success_only = Wp_parameters.register_category "success-only"

type prover =
  | Why3 of Why3Provers.t (* Prover via WHY *)
  | NativeAltErgo (* Direct Alt-Ergo *)
  | NativeCoq     (* Direct Coq and Coqide *)
  | Qed           (* Qed Solver *)
  | Tactical      (* Interactive Prover *)

type mode =
  | BatchMode (* Only check scripts *)
  | EditMode  (* Edit then check scripts *)
  | FixMode   (* Try check script, then edit script on non-success *)

let prover_of_name = function
  | "" | "none" -> None
  | "qed" | "Qed" -> Some Qed
  | "native-alt-ergo" (* for wp-reports *)
  | "native:alt-ergo" | "native:altgr-ergo"
    ->
      Wp_parameters.warning ~once:true ~current:false
        "native support for alt-ergo is deprecated, use why3 instead" ;
      Some NativeAltErgo
  | "native-coq" (* for wp-reports *)
  | "native:coq" | "native:coqide" | "native:coqedit"
    ->
      Wp_parameters.warning ~once:true ~current:false
        "native support for coq is deprecated, use tip instead" ;
      Some NativeCoq
  | "script" -> Some Tactical
  | "tip" -> Some Tactical
  | "why3" -> Some (Why3 { Why3.Whyconf.prover_name = "why3";
                           Why3.Whyconf.prover_version = "";
                           Why3.Whyconf.prover_altern = "generate only" })
  | s ->
      match Extlib.string_del_prefix "why3:" s with
      | Some "" -> None
      | Some s' -> Some (Why3 (Why3Provers.find s'))
      | None -> Some (Why3 (Why3Provers.find s))

let mode_of_prover_name = function
  | "native:coqedit" -> EditMode
  | "native:coqide" | "native:altgr-ergo" -> FixMode
  | _ -> BatchMode

let name_of_prover = function
  | Why3 s -> "why3:" ^ (Why3Provers.print s)
  | NativeAltErgo -> "alt-ergo"
  | NativeCoq -> "coq"
  | Qed -> "qed"
  | Tactical -> "script"

let title_of_prover = function
  | Why3 s -> Why3Provers.title s
  | NativeAltErgo -> "Alt-Ergo"
  | NativeCoq -> "Coq"
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
  | Why3 s -> sanitize_why3 (Why3Provers.print s)
  | NativeAltErgo -> "Alt-Ergo"
  | NativeCoq -> "Coq"
  | Qed -> "Qed"
  | Tactical -> "Tactical"

let is_auto = function
  | Qed | NativeAltErgo | Why3 _ -> true
  | Tactical | NativeCoq -> false

let cmp_prover p q =
  match p,q with
  | Qed , Qed -> 0
  | Qed , _ -> (-1)
  | _ , Qed -> 1
  | NativeAltErgo , NativeAltErgo -> 0
  | NativeAltErgo , _ -> (-1)
  | _ , NativeAltErgo -> 1
  | Tactical , Tactical -> 0
  | Tactical , _ -> (-1)
  | _ , Tactical -> 1
  | NativeCoq , NativeCoq -> 0
  | NativeCoq , _ -> (-1)
  | _ , NativeCoq -> 1
  | Why3 p , Why3 q -> Why3Provers.compare p q

let pp_prover fmt = function
  | NativeAltErgo -> Format.pp_print_string fmt "Alt-Ergo (Native)"
  | NativeCoq -> Format.pp_print_string fmt "Coq (Native)"
  | Why3 smt ->
      if Wp_parameters.debug_atleast 1 then
        Format.fprintf fmt "Why:%s" (Why3Provers.print smt)
      else
        Format.pp_print_string fmt (Why3Provers.title smt)
  | Qed -> Format.fprintf fmt "Qed"
  | Tactical -> Format.pp_print_string fmt "Tactical"

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
}

let param f = let v = f() in if v>0 then Some v else None

let current () = {
  valid = false ;
  timeout = param Wp_parameters.Timeout.get ;
  stepout = param Wp_parameters.Steps.get ;
}

let default = { valid = false ; timeout = None ; stepout = None }

let get_timeout = function
  | { timeout = None } -> Wp_parameters.Timeout.get ()
  | { timeout = Some t } -> t

let get_stepout = function
  | { stepout = None } -> Wp_parameters.Steps.get ()
  | { stepout = Some t } -> t

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
  cached : bool ;
  solver_time : float ;
  prover_time : float ;
  prover_steps : int ;
  prover_errpos : Lexing.position option ;
  prover_errmsg : string ;
}

let is_verdict r = match r.verdict with
  | Valid | Checked | Unknown | Invalid | Timeout | Stepout | Failed -> true
  | NoResult | Computing _ -> false

let is_valid = function { verdict = Valid } -> true | _ -> false
let is_computing = function { verdict=Computing _ } -> true | _ -> false

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
      let margin = 1000 in
      Some(max stepout margin)
    else None in
  {
    valid ;
    timeout ;
    stepout ;
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

let autofit r =
  time_fits r.prover_time &&
  step_fits r.prover_steps

let result ?(cached=false) ?(solver=0.0) ?(time=0.0) ?(steps=0) verdict =
  {
    verdict ;
    cached = cached ;
    solver_time = solver ;
    prover_time = time ;
    prover_steps = steps ;
    prover_errpos = None ;
    prover_errmsg = "" ;
  }

let no_result = result NoResult
let valid = result Valid
let checked = result Checked
let invalid = result Invalid
let unknown = result Unknown
let timeout t = result ~time:(float t) Timeout
let stepout n = result ~steps:n Stepout
let computing kill = result (Computing kill)
let failed ?pos msg = {
  verdict = Failed ;
  cached = false ;
  solver_time = 0.0 ;
  prover_time = 0.0 ;
  prover_steps = 0 ;
  prover_errpos = pos ;
  prover_errmsg = msg ;
}

let cached r = if is_verdict r then { r with cached=true } else r

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
    then Format.fprintf fmt " (%d)" s ;
    if r.cached && perfo extended dkey_no_cache_info
    then Format.fprintf fmt " (cached)" ;
  end

let pp_res ~extended fmt r =
  match r.verdict with
  | NoResult -> Format.pp_print_string fmt (if extended then "No Result" else "-")
  | Computing _ -> Format.pp_print_string fmt "Computing"
  | Checked -> Format.fprintf fmt "Typechecked"
  | Invalid -> Format.pp_print_string fmt "Invalid"
  | Valid when Wp_parameters.has_dkey dkey_success_only ->
      Format.pp_print_string fmt "Valid"
  | (Timeout|Stepout|Unknown) when Wp_parameters.has_dkey dkey_success_only ->
      Format.pp_print_string fmt "Unsuccess"
  | Valid -> Format.fprintf fmt "Valid%a" (pp_perf ~extended) r
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
    let s = Transitioning.Stdlib.compare p.prover_steps q.prover_steps in
    if s <> 0 then s else
      let t = Transitioning.Stdlib.compare p.prover_time q.prover_time in
      if t <> 0 then t else
        Transitioning.Stdlib.compare p.solver_time q.solver_time

let combine v1 v2 =
  match v1 , v2 with
  | Valid , Valid -> Valid
  | Failed , _ | _ , Failed -> Failed
  | Invalid , _ | _ , Invalid -> Invalid
  | Timeout , _ | _ , Timeout -> Timeout
  | Stepout , _ | _ , Stepout -> Stepout
  | _ -> Unknown

let merge r1 r2 =
  let err = if r1.prover_errmsg <> "" then r1 else r2 in
  {
    verdict = combine r1.verdict r2.verdict ;
    cached = r1.cached && r2.cached ;
    solver_time = max r1.solver_time r2.solver_time ;
    prover_time = max r1.prover_time r2.prover_time ;
    prover_steps = max r1.prover_steps r2.prover_steps ;
    prover_errpos = err.prover_errpos ;
    prover_errmsg = err.prover_errmsg ;
  }

let choose r1 r2 =
  match is_valid r1 , is_valid r2 with
  | true , false -> r1
  | false , true -> r2
  | _ -> if compare r1 r2 <= 0 then r1 else r2

let best = List.fold_left choose no_result
