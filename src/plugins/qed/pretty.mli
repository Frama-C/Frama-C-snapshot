(**************************************************************************)
(*                                                                        *)
(*  This file is part of WP plug-in of Frama-C.                           *)
(*                                                                        *)
(*  Copyright (C) 2007-2018                                               *)
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
(**    Pretty Printer for Qed Output.                                         *)
(* -------------------------------------------------------------------------- *)

open Logic
open Format

module Make(T : Term) :
sig
  open T

  type env (** environment for pretty printing *)

  val empty : env
  val marks : env -> marks
  val known : env -> Vars.t -> env
  val fresh : env -> term -> string * env
  val bind : string -> term -> env -> env

  val pp_tau : formatter -> tau -> unit

  (** print with the given environment without modifying it *)
  val pp_term : env -> formatter -> term -> unit
  val pp_def : env -> formatter -> term -> unit

  (** print with the given environment and update it *)
  val pp_term_env : env -> formatter -> term -> unit
  val pp_def_env : env -> formatter -> term -> unit

end
