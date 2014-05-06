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

open Task
open VCS

(* -------------------------------------------------------------------------- *)
(* --- Why3 Multi-Theorem Prover                                          --- *)
(* -------------------------------------------------------------------------- *)

type goal_id =
  {
    gfile : string;
    gtheory : string;
    ggoal : string;
  }

val assemble_wpo: Wpo.t -> (string list (* includes *) * goal_id) option
(** None if the po is trivial *)

val prove : Wpo.t -> prover:string -> result task
(** The string must be a valid why3 prover identifier
    Return NoResult if it is already proved by Qed
*)

val call_ide : includes:string list -> files:string list ->
  session:string -> bool Task.task

type dp = {
  dp_name : string ;
  dp_version : string ;
  dp_prover : string ;
}

val detect_why3 : (dp list option -> unit) -> unit
val detect_provers : (dp list -> unit) -> unit

val find : string -> dp list -> dp
val parse : string -> dp
