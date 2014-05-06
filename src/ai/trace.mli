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

(* Type of traces. *)
type t;;
val pretty : Format.formatter -> t -> unit;;

(* No trace. Should be used only as a base case for a no-op join. *)
val bottom: t;;
(* Unknown trace. Should be used only to forget a trace. *)
val top: t;;

val join: t -> t -> t;;

(* Create a trace, or add an element at the end of a trace.  *)
val initial: kernel_function -> t;;
val add_disjunction: Property.t -> predicate named -> t -> t;;
val add_statement: stmt -> t -> t;;

(* Set to false to set all traces to top. *)
val set_compute_trace: bool -> unit;;


