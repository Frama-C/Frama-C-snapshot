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

open Task
open VCS

(* -------------------------------------------------------------------------- *)
(* --- Why3 Multi-Theorem Prover                                          --- *)
(* -------------------------------------------------------------------------- *)

type goal =
  {
    file : string;
    theory : string;
    goal : string;
  }

val assemble_goal: Wpo.t -> (string list (* includes *) * goal) option
(** None if the po is trivial *)

val prove : ?timeout:int -> prover:string -> Wpo.t -> result task
(** The string must be a valid why3 prover identifier
    Return NoResult if it is already proved by Qed *)

(* -------------------------------------------------------------------------- *)
(* --- Why3 Multi-Theorem Prover                                          --- *)
(* -------------------------------------------------------------------------- *)

module Goal :
sig
  type t = goal
  val compare : t -> t -> int
  val pretty : Format.formatter -> t -> unit
end

(* -------------------------------------------------------------------------- *)
