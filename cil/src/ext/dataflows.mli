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

open Cil_types;;
open Ordered_stmt;;

(* Environment relative to the function being processed, and function to
   create them from Kf.  *)
module type FUNCTION_ENV = sig
  val to_ordered: stmt -> ordered_stmt
  val to_stmt: ordered_stmt -> stmt
  val nb_stmts: int
  val kf: Kernel_function.t
end
val function_env: kernel_function -> (module FUNCTION_ENV);;


module type JOIN_SEMILATTICE = sig
  type t

  (* Must be idempotent (join a a = a), commutative, and associative. *)
  val join: t -> t -> t

  (* Must verify that forall a, join a bottom = a. *)
  val bottom: t

  (* Must verify: a is_included b <=> a join b = b. The dataflow does
     not require this function. *)
  (* val is_included: t -> t -> bool *)

  (* This function is used by the dataflow algorithm to determine if
     something has to be recomputed. Joining and inclusion testing are
     similar operations, so it is often more efficient to do both at
     the same time (e.g. when joining with bottom). Note that the
     names [smaller] and [larger] are actually correct only if there
     is an inclusion.

     Instead of defining it directly, it can be defined from join and
     equal, or from is_included, for instance by
     [if is_included new old then (true,old) else (false, join old new)] or
     [let j = join old new in (equal j new, j)]. *)
  val join_and_is_included: t -> t -> (t * bool)

  (* Display the contents of an element of the lattice. *)
  val pretty: Format.formatter -> t -> unit

end


(* Edge-based forward dataflow. It is edge-based because the transfer
   function can differentiate the state after a statement between
   different successors. In particular, the state can be reduced
   according to the conditions in if statements. *)
module type FORWARD_MONOTONE_PARAMETER = sig
  include JOIN_SEMILATTICE

  (* [transfer_stmt s state] must returns a list of pairs in which the
     first element is a statement [s'] in [s.succs], and the second
     element a value that will be joined with the current result for
     before [s'].

     Note that it is allowed that not all succs are present in the
     list returned by [transfer_stmt], or that succs are present several
     times (this is useful to handle switchs). *)
  val transfer_stmt: stmt -> t -> (stmt * t) list

  (* The initial value for each statement. Statements in this list are
     given the associated value, and are added to the worklist. Other
     statements are initialized to bottom. *)
  val init: (stmt * t) list
end


module Simple_forward(Fenv:FUNCTION_ENV)(P:FORWARD_MONOTONE_PARAMETER)
  :sig val before:P.t Ordered_stmt.ordered_stmt_array (* TODO: Should disappear, together with Fenv? *)
       val fold_on_result: ('a -> stmt -> P.t -> 'a) -> 'a -> 'a
       val iter_on_result: (stmt -> P.t -> unit) -> unit
end;;

(* The following functions allow implementing [transfer_stmt] for the
   [If] and [Switch] instruction, from a [transfer_guard] function. 

   [transfer_guard] receives a conditional expression, the current
   statement, and the current state, and returns the new state when
   the expression evaluates to respectively true and false. *)
val transfer_if_from_guard:
  (stmt -> exp -> 'a -> 'a * 'a) -> stmt -> 'a -> (stmt * 'a) list
val transfer_switch_from_guard: 
  (stmt -> exp -> 'a -> 'a * 'a) -> stmt -> 'a -> (stmt * 'a) list

(*
Local Variables:
compile-command: "make -C ../../.."
End:
*)
