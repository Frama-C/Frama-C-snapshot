(**************************************************************************)
(*                                                                        *)
(*  This file is part of WP plug-in of Frama-C.                           *)
(*                                                                        *)
(*  Copyright (C) 2007-2016                                               *)
(*    CEA (Commissariat a l'energie atomique et aux energies              *)
(*         alternatives)                                                  *)
(*                                                                        *)
(*  All rights reserved.                                                  *)
(*  Contact CEA LIST for licensing.                                       *)
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

val assemble_wpo: Wpo.t -> (string list (* includes *) * goal) option
(** None if the po is trivial *)

val prove : Wpo.t -> prover:string -> result task
(** The string must be a valid why3 prover identifier
    Return NoResult if it is already proved by Qed
*)

type dp = {
  dp_name : string ;
  dp_version : string ;
  dp_prover : string ;
}

val detect_why3 : (dp list option -> unit) -> unit
val detect_provers : (dp list -> unit) -> unit

val find : string -> dp list -> dp
val parse : string -> dp

(* -------------------------------------------------------------------------- *)
(* --- Why3 Multi-Theorem Prover                                          --- *)
(* -------------------------------------------------------------------------- *)

module Goal :
sig
  type t = goal
  val compare : t -> t -> int
  val pretty : Format.formatter -> t -> unit
end
