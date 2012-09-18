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

include Plugin.S

val reset : unit -> unit

(** {2 Goal Selection} *)

module WP          : Plugin.Bool
module Functions   : Plugin.String_list
module Behaviors   : Plugin.String_list
module Properties  : Plugin.String_list
module StatusAll   : Plugin.Bool
module StatusTrue  : Plugin.Bool
module StatusFalse : Plugin.Bool
module StatusMaybe : Plugin.Bool

type job =
  | WP_None
  | WP_All
  | WP_Select of string list

val job : unit -> job

(** {2 Model Selection} *)

type model_kind =
  | M_Q of string
  | M_Hoare
  | M_Logic
  | M_Store
  | M_Runtime

type assigns_method =
  | NoAssigns
  | NormalAssigns
  | EffectAssigns

val get_model : unit -> model_kind
val get_assigns_method : unit -> assigns_method
val has_dkey : string -> bool

module Model : Plugin.String
module LogicVar : Plugin.Bool
module RefVar : Plugin.Bool
module Fits : Plugin.Bool
module Assigns : Plugin.String
module Natural : Plugin.Bool
module ExternArrays: Plugin.Bool
module ExtEqual : Plugin.Bool

(** {2 Computation Strategies} *)

type norm = Let | Exp | Eqs | Cc
val get_norm : unit -> norm

module RTE: Plugin.Bool
module Simpl: Plugin.Bool
module Qed: Plugin.Bool
module Split: Plugin.Bool
module Invariants: Plugin.Bool
module SplitDim: Plugin.Int
module Norm: Plugin.String
module Huge: Plugin.Int

(** {2 Prover Interface} *)

module Provers: Plugin.String_set
module Check: Plugin.String
module Includes: Plugin.String_list
module Script: Plugin.String
module UpdateScript: Plugin.Bool
module Timeout: Plugin.Int
module CoqTimeout: Plugin.Int
module Depth: Plugin.Int
module Steps: Plugin.Int
module Procs: Plugin.Int
module Trace: Plugin.Bool
module ProofTrace: Plugin.Bool
module UnsatModel: Plugin.Bool
module ProverSwitch: Plugin.String
module CoqLibs: Plugin.String_list
module CoqTactic: Plugin.String
module Hints: Plugin.Int
module TryHints: Plugin.Bool
module WhyLibs: Plugin.String_list
module AltErgoLibs: Plugin.String_list

val get_provers : unit -> string list

(** {2 Proof Obligations} *)

module Dot: Plugin.Bool
module Print: Plugin.Bool
module Details: Plugin.Bool
module Report: Plugin.String_list
module ReportName: Plugin.String

(** {2 Experimental} *)

module Froms: Plugin.Bool

(** {2 Environment Variables} *)

val get_env : ?default:string -> string -> string
val is_out : unit -> bool (* -wp-out <dir> positionned *)
val get_output : unit -> string
val get_output_dir : string -> string

(*
Local Variables:
compile-command: "make -C ../.."
End:
*)
