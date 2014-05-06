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
