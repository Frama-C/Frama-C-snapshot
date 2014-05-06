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
open Cvalue

val compute_call_ref :
  (kernel_function ->
   call_kinstr:kinstr ->
   Model.t ->
   (exp * V_Offsetmap.t) list ->
   Value_types.call_result)
  ref

val do_assign :
  with_alarms:CilE.warn_mode ->
  kernel_function ->
  Locals_scoping.clobbered_set ->
  Model.t -> lval -> exp -> Model.t

val interp_call :
  with_alarms:CilE.warn_mode ->
  Locals_scoping.clobbered_set ->
  stmt ->
  lval option ->
  exp ->
  exp list ->
  Model.t -> Model.t list * Value_types.cacheable

exception AlwaysOverlap

val check_non_overlapping :
  Model.t -> lval list -> lval list -> unit

val check_unspecified_sequence :
  Model.t ->
  (stmt * lval list * lval list * lval list * stmt ref list) list ->
  unit

val externalize :
  with_alarms:CilE.warn_mode ->
  kernel_function ->
  return_lv:lval option ->
  Locals_scoping.clobbered_set ->
  Model.t ->
  V_Offsetmap.t option * Model.t

(*
Local Variables:
compile-command: "make -C ../.."
End:
*)
