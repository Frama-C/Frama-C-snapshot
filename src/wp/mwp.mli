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
(** Model for the interpretation of ACSL/C                                    *)
(* -------------------------------------------------------------------------- *)

open Ctypes
open Clabels
open Formula
open Cil_types

module type Export =
sig
  type pred
  type decl
  val export_goal : Format.formatter -> string -> pred -> unit
  val export_decl : Format.formatter -> decl -> unit
end

module type S =
sig

  include Mlogic.S
    
  (**
      [update m h p] binds free variables in [p] representing
      the state [m] to the current memory [h].
  *)

  val update : at:mem -> here:mem -> F.pred -> F.pred

  (** [quantify_at_label m p] quantifies the free variables in [p]
      representing the memort state [m].

      It generalize the goal up-to the state [m].
  *)

  val quantify : mem -> F.pred -> F.pred

  (** [subst_lval frame l te v p] binds in [p] the free variables
      representing the value at location [l] in the current memory to
      the actual value [v].

      It updates the memory-map [frame] such that now the current memory in [p]
      has been updated by storing [v] at [l].
  *)

  val subst_lval : mem -> Ctypes.c_object -> loc -> value -> F.pred -> F.pred

  (** Binds the free variables in
      the wp representing the memory locations that live in [zone].
      Actually, subst_havoc must no do the substitution it-self to avoid
      any variable capture un region. Rather, [subst_havoc] should returns the
      list of substitutions to be applied modulo alpha-conversion.
  *)
  val subst_havoc : mem -> loc F.assigned -> F.havoc list

  (** {2 Assigns} *)

  (** [assigns_goal M1 region M2] returns a predicates
      establishing the assigns clause [region] with dependencies [depends].
      - [M1] is the memory {i before} of the execution of the assigning statement.
      - [M2] is the memory {i after} of the execution of the assigning statement.
  *)

  val assigns_goal :
    mem ->
    loc F.assigned list ->
    mem ->
    F.pred

  val assigns_supported : bool

  (** {2 Assigns with Zones} *)

  type m_dzone
  type dzone = m_dzone F.term
  val tau_of_dzone : tau

  val dzone_assigned : mem -> loc F.assigned -> dzone
  val dzone_subset : dzone -> dzone -> F.pred
  val dzone_union : dzone -> dzone -> dzone
  val dzone_empty : unit -> dzone

  val effect_supported : bool



  val global_scope :  mem -> F.pred -> F.pred

  (** [local_scope m l] transforms the predicate [p] at the
      enter-point of a block or function that
      declares the local variables in the list.  It is time to add
      hypotheses about those local variables. *)
  val local_scope : mem -> Cil_types.varinfo list -> Mcfg.scope -> F.pred -> F.pred

end



(*
Local Variables:
compile-command: "make -C ../.."
End:
*)
