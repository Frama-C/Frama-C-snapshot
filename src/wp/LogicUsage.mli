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
(* --- Dependencies of Logic Definitions                                  --- *)
(* -------------------------------------------------------------------------- *)

open Cil_types
open Cil_datatype
open Clabels

val basename : varinfo -> string (** Trims the original name *)

type logic_lemma = {
  lem_name : string ;
  lem_position : Lexing.position ;
  lem_axiom : bool ;
  lem_types : string list ;
  lem_labels : logic_label list ;
  lem_property : predicate named ;
  lem_depends : logic_lemma list ; (** in reverse order *)
}

type axiomatic = {
  ax_name : string ;
  ax_position : Lexing.position ;
  ax_property : Property.t ;
  mutable ax_types : logic_type_info list ;
  mutable ax_logics : logic_info list ;
  mutable ax_lemmas : logic_lemma list ;
  mutable ax_reads : Varinfo.Set.t ; (* read-only *)
}

type logic_section =
  | Toplevel of int
  | Axiomatic of axiomatic

val compute : unit -> unit (** To force computation *)

val ip_lemma : logic_lemma -> Property.t
val iter_lemmas : (logic_lemma -> unit) -> unit
val logic_lemma : string -> logic_lemma
val axiomatic : string -> axiomatic
val section_of_lemma : string -> logic_section
val section_of_type : logic_type_info -> logic_section
val section_of_logic : logic_info -> logic_section
val proof_context : unit -> logic_lemma list
  (** Lemmas that are not in an axiomatic. *)

val is_recursive : logic_info -> bool
val get_induction_labels : logic_info -> string -> LabelSet.t LabelMap.t
  (** Given an inductive [phi{...A...}].
      Whenever in [case C{...B...}] we have a call to [phi{...B...}],
      then [A] belongs to [(induction phi C).[B]]. *)

val get_name : logic_info -> string
val pp_profile : Format.formatter -> logic_info -> unit

val dump : unit -> unit (** Print on output *)

