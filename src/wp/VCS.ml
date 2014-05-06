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
(* --- Provers                                                            --- *)
(* -------------------------------------------------------------------------- *)

type prover =
  | Why3 of string (* Prover via WHY *)
  | Why3ide
  | AltErgo       (* Alt-Ergo *)
  | Coq           (* Coq and Coqide *)
  | Qed           (* Qed Solver *)

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
  | "why3ide" -> Some Why3ide
  | s ->
      match Extlib.string_del_prefix "why3:" s with
	| Some "" -> None
	| Some "ide" -> Some Why3ide
	| Some s' -> Some (Why3 s')
	| None -> Some (Why3 s)

let name_of_prover = function
  | Why3ide -> "Why3"
  | Why3 s -> s
  | AltErgo -> "Alt-Ergo"
  | Coq -> "Coq"
  | Qed -> "Qed"
 
let name_of_mode = function 
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
  | Qed -> L_why3

let language_of_prover_name = function
  | "" | "none" -> None
  | "alt-ergo" | "altgr-ergo" -> Some L_altergo
  | "coq" | "coqide" -> Some L_coq
  | _ -> Some L_why3

let mode_of_prover_name = function
  | "coqedit" -> EditMode
  | "coqide" | "altgr-ergo" -> FixMode
  | _ -> BatchMode

let cmp_prover p q =
  match p,q with
    | Qed , Qed -> 0
    | Qed , _ -> (-1)
    | _ , Qed -> 1
    | AltErgo , AltErgo -> 0
    | AltErgo , _ -> (-1)
    | _ , AltErgo -> 1
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

let pp_language fmt = function
  | L_altergo -> Format.pp_print_string fmt "Alt-Ergo"
  | L_coq -> Format.pp_print_string fmt "Coq"
  | L_why3 -> Format.pp_print_string fmt "Why3"

let pp_mode fmt m = Format.pp_print_string fmt (name_of_mode m)

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
  | Valid
  | Failed

type result = {
  verdict : verdict ; 
  solver_time : float ;
  prover_time : float ; 
  prover_steps : int ;
  prover_errpos : Lexing.position option ;
  prover_errmsg : string ;
}

let result ?(solver=0.0) ?(time=0.0) ?(steps=0) verdict = { 
  verdict = verdict ; 
  solver_time = solver ;
  prover_time = time ; 
  prover_steps = steps ;
  prover_errpos = None ;
  prover_errmsg = "" ;
}

let no_result = result NoResult
let valid = result Valid
let invalid = result Invalid
let unknown = result Unknown
let timeout = result Timeout
let stepout = result Stepout
let computing kill = result (Computing kill)
let failed ?pos msg = {
  verdict = Failed ;
  solver_time = 0.0 ;
  prover_time = 0.0 ;
  prover_steps = 0 ;
  prover_errpos = pos ; 
  prover_errmsg = msg ;
}

let pp_perf fmt r =
  begin
    let t = r.solver_time in
    if t > Rformat.epsilon && not (Wp_parameters.has_dkey "no-time-info") 
    then Format.fprintf fmt " (Qed:%a)" Rformat.pp_time t ;
    let t = r.prover_time in
    if t > Rformat.epsilon && not (Wp_parameters.has_dkey "no-time-info") 
    then Format.fprintf fmt " (%a)" Rformat.pp_time t ;
    let s = r.prover_steps in
    if s > 0 && not (Wp_parameters.has_dkey "no-step-info")
    then Format.fprintf fmt " (%d)" s
  end

let pp_result fmt r = 
  match r.verdict with
    | NoResult -> Format.pp_print_string fmt "-"
    | Invalid -> Format.pp_print_string fmt "Invalid"
    | Computing _ -> Format.pp_print_string fmt "Computing"
    | Valid -> Format.fprintf fmt "Valid%a" pp_perf r
    | Unknown -> Format.fprintf fmt "Unknown%a" pp_perf r
    | Timeout -> Format.fprintf fmt "Timeout%a" pp_perf r
    | Stepout -> Format.fprintf fmt "Step limit%a" pp_perf r
    | Failed -> Format.fprintf fmt "Failed@\nError: %s" r.prover_errmsg
