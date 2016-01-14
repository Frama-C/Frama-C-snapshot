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

(** Implementation of data flow analyses over user-supplied domains. *)

(* Instead of defining a single dataflow interface that tries to
   accomodate with all the options (as was done in {!Dataflow2}),
   having a set of dataflows allow to keep things simple in the
   general case; specific demands are handled by using more general
   dataflows. Simpler-to-instanciate dataflows are instances of the
   more general dataflows. *)

open Cil_types;;
open Ordered_stmt;;

(** Environment relative to the function being processed, and function to
   create them from Kf.  *)
module type FUNCTION_ENV = sig
  val to_ordered: stmt -> ordered_stmt
  val to_stmt: ordered_stmt -> stmt
  val connected_component: ordered_stmt -> int
  val nb_stmts: int
  val kf: Kernel_function.t
end
val function_env: kernel_function -> (module FUNCTION_ENV);;


module type JOIN_SEMILATTICE = sig
  type t

  (** Must be idempotent (join a a = a), commutative, and associative. *)
  val join: t -> t -> t

  (** Must verify that forall a, join a bottom = a. *)
  val bottom: t

  (** Must verify: a is_included b <=> a join b = b. The dataflow does
      not require this function. *)
  val is_included: t -> t -> bool

  (** This function is used by the dataflow algorithm to determine if
      something has to be recomputed. Joining and inclusion testing are
      similar operations, so it is often more efficient to do both at
      the same time (e.g. when joining with bottom).

      Instead of defining it directly, it can be defined from join and
      equal, or from is_included, for instance by
      [if is_included new old then (true,old) else (false, join old new)] or
      [let j = join old new in (equal j new, j)]. *)
  val join_and_is_included: t -> t -> (t * bool)

  (** Display the contents of an element of the lattice. *)
  val pretty: Format.formatter -> t -> unit

end

(** {2 Backward dataflow} *)

(** Statement-based backward dataflow. Contrary to the forward dataflow,
   the transfer function cannot differentiate the state before a
   statement between different predecessors. *)
module type BACKWARD_MONOTONE_PARAMETER = sig
  include JOIN_SEMILATTICE

  (** [transfer_stmt s state] must implement the transfer function for [s]. *)
  val transfer_stmt: stmt -> t -> t

  (** The initial state after each statement. Statements in this list are
      given the associated value, and are added to the worklist. Other
      statements are initialized to bottom.

      To get results for an entire function, this list should contain
      information for the following statements:
      - the final statement of the function ({!Kernel_function.find_return})
      - all the statements with no successors
      - at least one statement per non-terminating loop
  *)
  val init: (stmt * t) list
end

module Simple_backward(Fenv:FUNCTION_ENV)(P:BACKWARD_MONOTONE_PARAMETER) : sig

  (** {3 Retrieving the state before and after a statement.} *)

  val post_state: stmt -> P.t
  val pre_state: stmt -> P.t
  (** This function calls [transfer_stmt] on the result of [post_state]. Beware
      if [transfert_stmt] is impure or costly *)

  (** {3 Iterations on the results of the dataflow.}

      In this dataflow, the results are the post-states of all the statements
      that may reach the statements in [P.init]. *)

  val fold_on_result: ('a -> stmt -> P.t -> 'a) -> 'a -> 'a
  val iter_on_result: (stmt -> P.t -> unit) -> unit


  (**/**)
  val after:P.t Ordered_stmt.ordered_stmt_array
  (**/**)

end


(** {2 Forward dataflow} *)

(** Edge-based forward dataflow. It is edge-based because the transfer
   function can differentiate the state after a statement between
   different successors. In particular, the state can be reduced
   according to the conditions in if statements. *)
module type FORWARD_MONOTONE_PARAMETER = sig
  include JOIN_SEMILATTICE

  (** [transfer_stmt s state] must returns a list of pairs in which the
      first element is a statement [s'] in [s.succs], and the second
      element a value that will be joined with the current result for
      before [s'].

      Note that it is allowed that not all succs are present in the
      list returned by [transfer_stmt] (in which case, the successor is
      assumed to be unreachable in the current state), or that succs are
      present several times (this is useful to handle switchs).

      Helper functions are provided for [If] and [Switch] statements. See
      {!transfer_if_from_guard} and {!transfer_switch_from_guard} below. *)
  val transfer_stmt: stmt -> t -> (stmt * t) list

  (** The initial value for each statement. Statements in this list are
      given the associated value, and are added to the worklist. Other
      statements are initialized to bottom.

      Unless you want to do something very specific, supplying only the state
      for the first statement of the function (as found by
      {!Kernel_function.find_first_stmt}) is sufficient. *)
  val init: (stmt * t) list
end


module Simple_forward(Fenv:FUNCTION_ENV)(P:FORWARD_MONOTONE_PARAMETER) : sig

  (** {3 Retrieve the state before and after a statement.} *)

  val pre_state: stmt -> P.t
  val post_state: stmt -> P.t
  (** This function calls [transfer_stmt] on the result of [pre_state]. Beware
      if [transfert_stmt] is impure or costly *)


  (** {3 Iterations on the results of the dataflow.}

      In this dataflow, the results are the pre-states of all the statements
      reachable from the statements from [P.init]. *)
  
  val fold_on_result: ('a -> stmt -> P.t -> 'a) -> 'a -> 'a
  val iter_on_result: (stmt -> P.t -> unit) -> unit


  (**/**)
  val before:P.t Ordered_stmt.ordered_stmt_array
  (* TODO: Should disappear, together with Fenv? *)
  (**/**)
  
end;;

(** {3 Helper functions for forward dataflow.} *)

val transfer_if_from_guard:
  (stmt -> exp -> 'a -> 'a * 'a) -> stmt -> 'a -> (stmt * 'a) list
(** [transfer_if_from_guard] implements
    [FORWARD_MONOTONE_PARAMETER.transfer_stmt] for the [If] statement, given a
    function [transfer_guard].

    [transfer_guard] receives a conditional expression, the current
    statement, and the current state, and must return two states in which
    the conditional is assumed to be true and false respectively. Returning
    twice the current state is a valid, albeit imprecise, result. *)

val transfer_switch_from_guard:
  (stmt -> exp -> 'a -> 'a * 'a) -> stmt -> 'a -> (stmt * 'a) list
(** Same as {!transfer_if_from_guard}, but for a [Switch] statement. The
    same function [transfer_guard] can be used for [transfer_if_from_guard]
    and [transfer_switch_from_guard]. *)

(*
Local Variables:
compile-command: "make -C ../../.."
End:
*)
