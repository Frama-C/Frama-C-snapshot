(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2019                                               *)
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

(* -------------------------------------------------------------------------- *)
(** Filtering Categories of Alarms *)
(* -------------------------------------------------------------------------- *)

(** Flags for controling the low-level API. Each flag control whether
    a category of alarms will be visited or not. *)
type t = {
  remove_trivial: bool;
  initialized: bool;
  mem_access: bool;
  div_mod: bool;
  shift: bool;
  left_shift_negative: bool;
  right_shift_negative: bool;
  signed_overflow: bool;
  unsigned_overflow: bool;
  signed_downcast: bool;
  unsigned_downcast: bool;
  float_to_int: bool;
  finite_float: bool;
  pointer_call: bool;
  bool_value: bool;
}

(** Defaults flags are taken from the Kernel and RTE plug-in options. *)
val default :
  ?remove_trivial:bool ->
  ?initialized:bool ->
  ?mem_access:bool ->
  ?div_mod:bool ->
  ?shift:bool ->
  ?left_shift_negative:bool ->
  ?right_shift_negative:bool ->
  ?signed_overflow:bool ->
  ?unsigned_overflow:bool ->
  ?signed_downcast:bool ->
  ?unsigned_downcast:bool ->
  ?float_to_int:bool ->
  ?finite_float:bool ->
  ?pointer_call:bool ->
  ?bool_value:bool ->
  unit -> t

(** All flags set to [true]. *)
val all : t

(** All flags set to [false]. *)
val none : t

(* -------------------------------------------------------------------------- *)
