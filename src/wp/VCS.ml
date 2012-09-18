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
(* --- Provers                                                            --- *)
(* -------------------------------------------------------------------------- *)

type prover =
  | Why of string (* Prover via WHY *)
  | AltErgo       (* Alt-Ergo *)
  | Coq           (* Coq and Coqide *)
  | WP            (* Simplifier *)

type language =
  | L_why
  | L_coq
  | L_altergo

let prover_of_name = function
  | "" | "none" -> None
  | "alt-ergo" | "altgr-ergo" -> Some AltErgo
  | "coq" | "coqide" -> Some Coq
  | s -> Some (Why s)

let name_of_prover = function
  | Why s -> String.capitalize s
  | AltErgo -> "Alt-Ergo"
  | Coq -> "Coq"
  | WP -> "WP"

let language_of_name = function
  | "" | "none" -> None
  | "alt-ergo" | "altgr-ergo" -> Some L_altergo
  | "coq" | "coqide"-> Some L_coq
  | "why" -> Some L_why
  | s -> Wp_parameters.abort "Language '%s' unknown" s

let language_of_prover = function
  | Why _ -> L_why
  | Coq -> L_coq
  | AltErgo -> L_altergo
  | WP -> L_why

let language_of_prover_name = function
  | "" | "none" -> None
  | "alt-ergo" | "altgr-ergo" -> Some L_altergo
  | "coq" | "coqide" -> Some L_coq
  | _ -> Some L_why

let is_interactive = function
  | "coqide" | "altgr-ergo" -> true
  | _ -> false

let gui_provers = [ 
  WP ; 
  AltErgo ; 
  Coq ;
  Why "z3" ; 
  Why "simplify" ;
  Why "vampire";
  Why "cvc3" ; 
  Why "yices" ;
  Why "zenon" ;
]

let pp_prover fmt = function
  | AltErgo -> Format.pp_print_string fmt "Alt-Ergo"
  | Coq -> Format.pp_print_string fmt "Coq"
  | Why smt ->
      if Wp_parameters.debug_atleast 1 then
         Format.pp_print_string fmt ("Why:"^(String.capitalize smt))
      else
        Format.pp_print_string fmt (String.capitalize smt)
  | WP -> Format.fprintf fmt "WP"

let pp_language fmt = function
  | L_altergo -> Format.pp_print_string fmt "Alt-Ergo"
  | L_coq -> Format.pp_print_string fmt "Coq"
  | L_why -> Format.pp_print_string fmt "Why"

(* -------------------------------------------------------------------------- *)
(* --- Results                                                            --- *)
(* -------------------------------------------------------------------------- *)

type verdict =
  | NoResult
  | Invalid
  | Unknown
  | Timeout
  | Stepout
  | Computing
  | Valid
  | Failed

type result = {
  verdict : verdict ; 
  prover_time : float ; 
  prover_steps : int ;
  prover_errpos : Lexing.position option ;
  prover_errmsg : string ;
}

let result ?(time=0.0) ?(steps=0) verdict = { 
  verdict = verdict ; 
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
let computing = result Computing
let failed ?pos msg = {
  verdict = Failed ;
  prover_time = 0.0 ;
  prover_steps = 0 ;
  prover_errpos = pos ; 
  prover_errmsg = msg ;
}

let pp_perf fmt r =
  begin
    let t = r.prover_time in
    if t > 0.0 && not (Wp_parameters.has_dkey "no-time-info") 
    then Format.fprintf fmt " (%a)" Rformat.pp_time t ;
    let s = r.prover_steps in
    if s > 0 && not (Wp_parameters.has_dkey "no-step-info")
    then Format.fprintf fmt " (%d)" s
  end

let pp_result fmt r = 
  match r.verdict with
    | NoResult -> Format.pp_print_string fmt "-"
    | Invalid -> Format.pp_print_string fmt "Invalid"
    | Computing -> Format.pp_print_string fmt "Computing"
    | Valid -> Format.fprintf fmt "Valid%a" pp_perf r
    | Unknown -> Format.fprintf fmt "Unknown%a" pp_perf r
    | Timeout -> Format.fprintf fmt "Timeout%a" pp_perf r
    | Stepout -> Format.fprintf fmt "Step limit%a" pp_perf r
    | Failed -> Format.fprintf fmt "Failed@\nError: %s" r.prover_errmsg
