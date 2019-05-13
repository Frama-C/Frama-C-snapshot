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

open Cil_types

val inline_calls : file -> unit

(** [inline_term ~inline term] inlines in [term] the application of predicates
    and logic functions for which [inline] is true. If provided, [current] is
    the current label of the term; it is [Here] by default. Returns [None]
    if the inlining of a predicate or a logic function fails, in particular
    when they are recursive or have no direct definitiion. *)
val inline_term:
  inline:(logic_info -> bool) -> ?current:logic_label -> term -> term option

(** Inlines predicates and logic functions in a predicate. See [inline_term]
    for details. *)
val inline_predicate:
  inline:(logic_info -> bool) -> ?current:logic_label ->
  predicate -> predicate option
