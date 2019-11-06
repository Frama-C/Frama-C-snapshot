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

include Plugin.S

val reset : unit -> unit

(** {2 Function Selection} *)

type functions =
  | Fct_none
  | Fct_all
  | Fct_skip of Cil_datatype.Kf.Set.t
  | Fct_list of Cil_datatype.Kf.Set.t

val get_kf : unit -> functions
val get_wp : unit -> functions
val iter_fct : (Kernel_function.t -> unit) -> functions -> unit
val iter_kf : (Kernel_function.t -> unit) -> unit
val iter_wp : (Kernel_function.t -> unit) -> unit

(** {2 Goal Selection} *)

module WP          : Parameter_sig.Bool
module Behaviors   : Parameter_sig.String_list
module Properties  : Parameter_sig.String_list
module StatusAll   : Parameter_sig.Bool
module StatusTrue  : Parameter_sig.Bool
module StatusFalse : Parameter_sig.Bool
module StatusMaybe : Parameter_sig.Bool

(** {2 Model Selection} *)

val has_dkey : category -> bool

module Model : Parameter_sig.String_list
module ByValue : Parameter_sig.String_set
module ByRef : Parameter_sig.String_set
module InHeap : Parameter_sig.String_set
module AliasInit: Parameter_sig.Bool
module InCtxt : Parameter_sig.String_set
module ExternArrays: Parameter_sig.Bool
module Literals : Parameter_sig.Bool
module Volatile : Parameter_sig.Bool

module Region: Parameter_sig.Bool
module Region_rw: Parameter_sig.Bool
module Region_pack: Parameter_sig.Bool
module Region_flat: Parameter_sig.Bool
module Region_annot: Parameter_sig.Bool
module Region_inline: Parameter_sig.Bool
module Region_fixpoint: Parameter_sig.Bool
module Region_cluster: Parameter_sig.Bool

(** {2 Computation Strategies} *)

module Init: Parameter_sig.Bool
module InitWithForall: Parameter_sig.Bool
module BoundForallUnfolding: Parameter_sig.Int
module RTE: Parameter_sig.Bool
module Simpl: Parameter_sig.Bool
module Let: Parameter_sig.Bool
module Core: Parameter_sig.Bool
module Prune: Parameter_sig.Bool
module Clean: Parameter_sig.Bool
module Filter: Parameter_sig.Bool
module Parasite: Parameter_sig.Bool
module Prenex: Parameter_sig.Bool
module Bits: Parameter_sig.Bool
module Ground: Parameter_sig.Bool
module Reduce: Parameter_sig.Bool
module ExtEqual : Parameter_sig.Bool
module UnfoldAssigns : Parameter_sig.Bool
module Split: Parameter_sig.Bool
module SplitDepth: Parameter_sig.Int
module DynCall : Parameter_sig.Bool
module SimplifyIsCint : Parameter_sig.Bool
module SimplifyLandMask : Parameter_sig.Bool
module SimplifyForall : Parameter_sig.Bool
module SimplifyType : Parameter_sig.Bool
module CalleePreCond : Parameter_sig.Bool
module PrecondWeakening : Parameter_sig.Bool

(** {2 Prover Interface} *)

module Detect: Parameter_sig.Bool
module Generate:Parameter_sig.Bool
module Provers: Parameter_sig.String_list
module Cache: Parameter_sig.String
module Drivers: Parameter_sig.String_list
module Script: Parameter_sig.String
module UpdateScript: Parameter_sig.Bool
module Timeout: Parameter_sig.Int
module TimeExtra: Parameter_sig.Int
module TimeMargin: Parameter_sig.Int
module CoqTimeout: Parameter_sig.Int
module CoqCompiler : Parameter_sig.String
module CoqIde : Parameter_sig.String
module CoqProject : Parameter_sig.String
module Steps: Parameter_sig.Int
module Procs: Parameter_sig.Int
module ProofTrace: Parameter_sig.Bool
module CoqLibs: Parameter_sig.String_list
module CoqTactic: Parameter_sig.String
module Hints: Parameter_sig.Int
module TryHints: Parameter_sig.Bool
module Why3Flags: Parameter_sig.String_list
module AltErgo: Parameter_sig.String
module AltGrErgo: Parameter_sig.String
module AltErgoLibs: Parameter_sig.String_list
module AltErgoFlags: Parameter_sig.String_list

module Auto: Parameter_sig.String_list
module AutoDepth: Parameter_sig.Int
module AutoWidth: Parameter_sig.Int
module BackTrack: Parameter_sig.Int

(** {2 Proof Obligations} *)

module TruncPropIdFileName: Parameter_sig.Int
module Print: Parameter_sig.Bool
module Report: Parameter_sig.String_list
module ReportJson: Parameter_sig.String
module ReportName: Parameter_sig.String
module MemoryContext: Parameter_sig.Bool
module Check: Parameter_sig.Bool

(** {2 Getters} *)

val has_out : unit -> bool
val has_session : unit -> bool
val get_session : unit -> string
val get_session_dir : string -> string
val get_output : unit -> string
val get_output_dir : string -> string
val make_output_dir : string -> unit
val get_overflows : unit -> bool

(** {2 Debugging Categories} *)
val has_print_generated: unit -> bool
val print_generated: ?header:string -> string -> unit
(** print the given file if the debugging category
    "print-generated" is set *)
val cat_print_generated: category
