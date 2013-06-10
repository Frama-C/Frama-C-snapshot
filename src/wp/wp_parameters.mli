(**************************************************************************)
(*                                                                        *)
(*  This file is part of WP plug-in of Frama-C.                           *)
(*                                                                        *)
(*  Copyright (C) 2007-2013                                               *)
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

include Plugin.S

val reset : unit -> unit

(** {2 Goal Selection} *)

module WP          : Plugin.Bool
module Behaviors   : Plugin.String_list
module Properties  : Plugin.String_list
module StatusAll   : Plugin.Bool
module StatusTrue  : Plugin.Bool
module StatusFalse : Plugin.Bool
module StatusMaybe : Plugin.Bool

type job =
  | WP_None
  | WP_All
  | WP_SkipFct of string list
  | WP_Fct of string list

val job : unit -> job

(** {2 Model Selection} *)

val has_dkey : string -> bool

module Model : Plugin.String_list
module ExternArrays: Plugin.Bool
module ExtEqual : Plugin.Bool
module Literals : Plugin.Bool

(** {2 Computation Strategies} *)

module RTE: Plugin.Bool
module Simpl: Plugin.Bool
module Let: Plugin.Bool
module Prune: Plugin.Bool
module Clean: Plugin.Bool
module Split: Plugin.Bool
module Invariants: Plugin.Bool

(** {2 Prover Interface} *)

module Detect: Plugin.Bool
module Generate:Plugin.Bool
module Provers: Plugin.String_list
module Drivers: Plugin.String_list
module Includes: Plugin.String_list
module Script: Plugin.String
module UpdateScript: Plugin.Bool
module Timeout: Plugin.Int
module CoqTimeout: Plugin.Int
module Depth: Plugin.Int
module Steps: Plugin.Int
module Procs: Plugin.Int
module ProofTrace: Plugin.Bool
module UnsatModel: Plugin.Bool
module CoqLibs: Plugin.String_list
module CoqTactic: Plugin.String
module Hints: Plugin.Int
module TryHints: Plugin.Bool
module WhyLibs: Plugin.String_list
module WhyFlags: Plugin.String_list
module AltErgoLibs: Plugin.String_list
module AltErgoLightInt: Plugin.Bool
module AltErgoFlags: Plugin.String_list

(** {2 Proof Obligations} *)

module Print: Plugin.Bool
module Report: Plugin.String_list
module ReportName: Plugin.String
module Check: Plugin.Bool
val wpcheck: unit -> bool

(** {2 Experimental} *)

module Froms: Plugin.Bool

(** {2 Environment Variables} *)

val get_env : ?default:string -> string -> string
val is_out : unit -> bool (* -wp-out <dir> positionned *)
val get_output : unit -> string
val get_output_dir : string -> string
val find_lib : string -> string
