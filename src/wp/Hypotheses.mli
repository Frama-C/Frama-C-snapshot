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
(** Bunch of possibly Simplified hypotheses. *)
(* -------------------------------------------------------------------------- *)

open Cil_types
open Lang.F

type t (** The representation of hypotheses [H1..Hn] *)

val pretty : Format.formatter -> t -> unit

val empty : t (** True *)

val mark : ?descr:string -> ?stmt:stmt -> t -> t

val assume : t -> pred list -> t
  (** Adds hypotheses to the representation. *)

val conditions : t -> pred list
  (** Returns a set of equivalent hypotheses to [t] *)

val occurs : var -> t -> bool
val intersect : pred -> t -> bool

val factorize : t -> t -> pred list * t * pred list
  (** Computes the factorization of [A] and [B] into [P,H,Q] where:
       - [P] are only in [A]
       - [H] if the common parts from [A] and [B]
       - [Q] are only in [B] *)

val m_or : t -> t -> t
  (** Computes [A \/ B] with factorization of common predicates. 
      Has the general form [E /\ (P\/Q)] where:
       - [E] are common parts from [A] and [B]
       - [P] are only in [A] and not in [B]
       - [Q] are only in [B] and not in [A]
  *)
      
val m_if : pred -> t -> t -> t
  (** Computes [ H.A \/ ~H.B ] with factorization of common predicates.
      Has the general form [E /\ (H?P:Q)] where:
       - [E] are common parts from A and B
       - [P] are only in [A] and not in [B]
       - [Q] are only in [B] and not in [A] *)

val m_either : t list -> t option (** None if empty list *)

(* -------------------------------------------------------------------------- *)
(* --- Simplification                                                     --- *)
(* -------------------------------------------------------------------------- *)

val add_guards : t -> pred -> pred list -> t
  (** Given H and P, add those relevant guards *)

val simplify_hyps : t -> t

val simplify_goal : t -> pred -> t * pred
  (** Given H and P, [simplify_goal H P] returns
      a sequent [H'|-P'] equivalent to [H|-P]. *)

val close : t -> pred -> pred 
  (** Build (H->P) and quantify free variables *)
