(****************************************************************************)
(*                                                                          *)
(*  Copyright (C) 2001-2003                                                 *)
(*   George C. Necula    <necula@cs.berkeley.edu>                           *)
(*   Scott McPeak        <smcpeak@cs.berkeley.edu>                          *)
(*   Wes Weimer          <weimer@cs.berkeley.edu>                           *)
(*   Ben Liblit          <liblit@cs.berkeley.edu>                           *)
(*  All rights reserved.                                                    *)
(*                                                                          *)
(*  Redistribution and use in source and binary forms, with or without      *)
(*  modification, are permitted provided that the following conditions      *)
(*  are met:                                                                *)
(*                                                                          *)
(*  1. Redistributions of source code must retain the above copyright       *)
(*  notice, this list of conditions and the following disclaimer.           *)
(*                                                                          *)
(*  2. Redistributions in binary form must reproduce the above copyright    *)
(*  notice, this list of conditions and the following disclaimer in the     *)
(*  documentation and/or other materials provided with the distribution.    *)
(*                                                                          *)
(*  3. The names of the contributors may not be used to endorse or          *)
(*  promote products derived from this software without specific prior      *)
(*  written permission.                                                     *)
(*                                                                          *)
(*  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS     *)
(*  "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT       *)
(*  LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS       *)
(*  FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE          *)
(*  COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,     *)
(*  INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,    *)
(*  BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;        *)
(*  LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER        *)
(*  CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT      *)
(*  LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN       *)
(*  ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE         *)
(*  POSSIBILITY OF SUCH DAMAGE.                                             *)
(*                                                                          *)
(*  File modified by CEA (Commissariat à l'énergie atomique et aux          *)
(*                        énergies alternatives)                            *)
(*               and INRIA (Institut National de Recherche en Informatique  *)
(*                          et Automatique).                                *)
(****************************************************************************)

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
