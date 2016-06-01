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

open Cil_types

val pp_calls : Format.formatter -> kernel_function list -> unit

val property : kf:kernel_function -> ?bhv:string -> stmt:stmt ->
  calls:kernel_function list -> Property.t
(** Returns an property identifier for the precondition. *)

val get : ?bhv:string -> stmt -> kernel_function list
(** Returns empty list if there is no specified dynamic call. *)

val compute : unit -> unit
(** Forces computation of dynamic calls.
    Otherwize, they are computed lazily on [get].
    Requires [-wp-dynamic]. *)
