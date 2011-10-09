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

(* ------------------------------------------------------------------------ *)
(** Splitting Goals                                                         *)
(* ------------------------------------------------------------------------ *)

(** {2 Zone-unions Flattening}
    The [unfolding] methods below only applies to variables that holds
    a union of [zone] values. Other variables are left letified. *)

module Env : Map.S with type key = Fol.Var.t

val e_unfold : Fol.term Env.t -> Fol.term -> Fol.term
val p_unfold : Fol.term Env.t -> Fol.pred -> Fol.pred

(** {2 Splitter Interface} *)

type pred = Fol.pred
val simplify : pred -> pred
val split : Mcfg.assigns_method -> pred -> pred Bag.t
  (** First, unfold zones when method is effect-assigns.
      Then applies [dispatch].
      Note: [split] may deliver stronger sub-predicates *)
