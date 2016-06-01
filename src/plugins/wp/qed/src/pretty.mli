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

(* -------------------------------------------------------------------------- *)
(**    Pretty Printer for Qed Output.                                         *)
(* -------------------------------------------------------------------------- *)

open Logic
open Format

module Make(T : Term) :
sig
  open T

  type env

  val empty : env
  val marks : env -> marks
  val known : env -> Vars.t -> env
  val fresh : env -> term -> string * env
  val bind : string -> term -> env -> env

  val pp_tau : formatter -> tau -> unit
  val pp_term : env -> formatter -> term -> unit
  val pp_def : env -> formatter -> term -> unit

end
