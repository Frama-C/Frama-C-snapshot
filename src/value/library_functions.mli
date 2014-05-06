(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2014                                               *)
(*    CEA (Commissariat à l'énergie atomique et aux énergies              *)
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

val add_retres_to_state:
  with_alarms:CilE.warn_mode -> 
  kernel_function ->
  Cvalue.V_Offsetmap.t ->
  Cvalue.Model.t ->
  varinfo * Cvalue.Model.t

val returned_value:
  kernel_function ->
  Cvalue.Model.t ->
  Cvalue.V.t * Cvalue.Model.t

(** Auxiliary function that registers a new variable declared by Value
    within the kernel internal tables *)
val register_new_var: varinfo -> typ -> unit

(*
Local Variables:
compile-command: "make -C ../.."
End:
*)
