(**************************************************************************)
(*                                                                        *)
(*  This file is part of WP plug-in of Frama-C.                           *)
(*                                                                        *)
(*  Copyright (C) 2007-2017                                               *)
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
(** Sequent Cleaning *)
(* -------------------------------------------------------------------------- *)

open Lang

(**
   Erase parts of a predicate that do not satisfies the condition.
   The erased parts are replaced by:
    - [true] when [~polarity:false] (for hypotheses)
    - [false] when [~polarity:true] (for goals)

   Hence, we have:
    - [filter ~polarity:true f p ==> p]
    - [p ==> filter ~polarity:false f p]

   See [theory/filtering.why] for proofs.
*)

val filter : polarity:bool -> (F.pred -> bool) -> F.pred -> F.pred

open Conditions

val compute : ?anti:bool -> sequent -> sequent




(* -------------------------------------------------------------------------- *)
