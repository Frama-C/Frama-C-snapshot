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

open Cil_types

(** An [ordered_stmt] is an int representing a stmt in a particular
    function.  They are sorted by the topological orderering of
    stmts (s1 < s2 if s1 precedes s2, or s2 does not precede s1); they
    are contiguous and start from 0.

    Note: due to the presence of unreachable statements in the graph,
    you should not assume that the entry point is statement number 0
    and the return is statement number |nb_stmts - 1|. Use
    [Kernel_function.find_first_stmt] and
    [Kernel_function.find_return] instead.*)
type ordered_stmt = (* private *) int

(** As [ordered_stmts] are contiguous and start from 0, they are
    suitable for use by index in a array. This type denotes arrays
    whose index are ordered stmts. *)
type 'a ordered_stmt_array = 'a array;;

(** Types of conversion tables between stmt and ordered_stmt.  *)
type ordered_to_stmt = stmt ordered_stmt_array;;
type stmt_to_ordered

(** Conversion functions between stmt and ordered_stmt.  *)
val to_ordered: stmt_to_ordered -> stmt -> ordered_stmt
val to_stmt: ordered_to_stmt -> ordered_stmt -> stmt

(** This function computes, caches, and returns the conversion tables
    between a stmt and an [ordered_stmt], and a table mapping each
    ordered_stmt to a connex component number (connex component number
    are also sorted in topological order *)
val get_conversion_tables:
  kernel_function -> stmt_to_ordered * ordered_to_stmt * int ordered_stmt_array
