(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2016                                               *)
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

(** Syntax extension for widening hints, used by Value. *)

open Cil_types

val dkey: Log.category

(** String used for hints applying to all variables. *)
val all_vars_str : string

(** Type of widening hints: an optional variable (None means "all variables")
    for which the hints will apply and a list of names (e.g. global). *)
type hint_lval = {
  vars : lval option;
  names : string list;
  loc : Cil_datatype.Location.t;
}

type t = hint_lval * term list

(** [get_stmt_widen_hint_terms s] returns the list of widen hints associated to
    [s]. *)
val get_stmt_widen_hint_terms : stmt -> t list

(** [is_global wh] returns true iff widening hint [wh] has a "global" prefix. *)
val is_global : t -> bool
