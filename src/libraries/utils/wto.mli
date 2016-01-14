(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2015                                               *)
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

(** Hierarchical Strongly Connected Components: performs Bourdoncle
    computation of a weak topological order on a graph represented by
    contiguous integers. Used by {!Wto_statement}.  *)

type partition =
  | Nil
  | Node of int * partition
  | Component of partition * partition

val pretty: partition Pretty_utils.formatter


type succ = (int -> unit) -> int -> unit

val partition : size:int -> succ:succ -> root:int -> partition
(** Returns a weak partial order with Bourdoncle's algorithm. *)

val fixpoint : (level:int -> int -> bool) -> (int -> unit) -> partition -> unit
(** Iterate over a weak partial order.
    The first function is supposed to update the given node and return [true]
    when stable. It must eventually apply widening to stabilize.
    The second function simply update the given node. It should never apply
    widening. *)
