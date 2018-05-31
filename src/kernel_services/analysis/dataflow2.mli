(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2018                                               *)
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

(** Implementation of data flow analyses over user-supplied domains. *)

type 't action =
    Default (** The default action *)
  | Done of 't (** Do not do the default action. Use this result *)
  | Post of ('t -> 't) (** The default action, followed by the given
                        * transformer *)

type 't stmtaction =
    SDefault   (** The default action *)
  | SDone      (** Do not visit this statement or its successors *)
  | SUse of 't (** Visit the instructions and successors of this statement
                  as usual, but use the specified state instead of the
                  one that was passed to doStmt *)

(** For if statements *)
type 't guardaction =
    GDefault      (** The default state *)
  | GUse of 't    (** Use this data for the branch *)
  | GUnreachable  (** The branch will never be taken. *)

module type StmtStartData = sig
  type data
  val clear: unit -> unit
  val mem: Cil_types.stmt -> bool
  val find: Cil_types.stmt -> data
  val replace: Cil_types.stmt -> data -> unit
  val add: Cil_types.stmt -> data -> unit
  val iter: (Cil_types.stmt -> data -> unit) -> unit
  val length: unit -> int
end

(** This module can be used to instantiate the [StmtStartData] components
    of the functors below. It is implemented through stmt-indexed hashtables. *)
module StartData(X:sig type t val size: int end) :
  StmtStartData with type data = X.t

(* ************************************************************************* *)
(** {2 Forwards Dataflow Analysis} *)
(* ************************************************************************* *)

(** Interface to provide for a backward dataflow analysis. *)
module type ForwardsTransfer = sig

  val name: string (** For debugging purposes, the name of the analysis *)
  val debug: bool (** Whether to turn on debugging *)

  type t
  (** The type of the data we compute for each block start. May be
      imperative.  *)

  val copy: t -> t
  (** Make a deep copy of the data. Useful when {!t} is a mutable type.
      A copy of the data stored for a statement is made each time this
      statement is processed, just before {!doStmt} is called. *)

  val pretty: Format.formatter -> t -> unit
  (** Pretty-print the state. *)

  val computeFirstPredecessor: Cil_types.stmt -> t -> t
  (** [computeFirstPredecessor s d] is called when [s] is reached for the
      first time (i.e. no previous data is associated with it). The data [d]
      is propagated to [s] from an unspecified preceding statement [s'].
      The result of the call is stored as the new data for [s].
      
      [computeFirstPredecessor] usually leaves [d] unchanged, but may
      potentially change it. It is also possible to perform a side-effect,
      for dataflows that store information out of the type [t]. *)

  val combinePredecessors: Cil_types.stmt -> old:t -> t -> t option
  (** Take some old data for the given statement, and some new data for the
      same point. Return None if the combination is identical to the old
      data, to signify that a fixpoint is currently reached for this statement.
      Otherwise, compute the combination, and return it. *)

  val doInstr: Cil_types.stmt -> Cil_types.instr -> t -> t
  (** The (forwards) transfer function for an instruction, internally
      called by {!doStmt} when the returned action is not {!SDone}.
      The current location is updated before this function is called.
      The argument of type [stmt] is the englobing statement. *)

  val doGuard: Cil_types.stmt -> Cil_types.exp -> t -> 
    t guardaction * t guardaction
  (** Generate the successors [act_th, act_el] to an [If] statement. [act_th]
      (resp. [act_el]) corresponds to the case where the given expression
      evaluates to zero (resp. non-zero). It is always possible to return
      [GDefault, GDefault], especially for analyses that do not use guard
      information. This is equivalent to returning [GUse d, GUse d], where [d]
      is the input state.  A return value of GUnreachable indicates
      that this half of the branch will not be taken and should not be explored.
      [stmt] is the corresponding [If] statement, passed as information only. *)

  val doStmt: Cil_types.stmt -> t -> t stmtaction
  (** The (forwards) transfer function for a statement. The [(Cil.CurrentLoc.get
      ())] * is set before calling this. The default action is to do the
      instructions * in this statement, if applicable, and continue with the
      successors. *)

  val doEdge: Cil_types.stmt -> Cil_types.stmt -> t -> t
  (** what to do when following the edge between the two given statements.
      Can default to identity if nothing special is required. *)

  module StmtStartData: StmtStartData with type data = t
(** For each statement id, the data at the start. Not found in the hash table
    means nothing is known about the state at this point. At the end of the
    analysis this means that the block is not reachable. *)

end

module Forwards(T : ForwardsTransfer) : sig
  val compute: Cil_types.stmt list -> unit
  (** Fill in the T.stmtStartData, given a number of initial statements to
      start from. All of the initial statements must have some entry in
      T.stmtStartData (i.e., the initial data should not be bottom) *)

  val compute_strategy: Cil_types.stmt list -> Wto_statement.wto -> unit
  (** Same as compute but using a given strategy, instead of the default
      strategy computed by the Wto module. *)

  val compute_worklist: Cil_types.stmt list -> unit
  (** Same as compute but using only a working list of statements instead
      of iterating on a weak topological ordering. *)
end


(* ************************************************************************* *)
(** {2 Backwards Dataflow Analysis} *)
(* ************************************************************************* *)

(** Interface to provide for a backward dataflow analysis. *)
module type BackwardsTransfer = sig

  val name: string (** For debugging purposes, the name of the analysis *)
  val debug: bool (** Whether to turn on debugging *)

  type t  
  (** The type of the data we compute for each block start. In many
      presentations of backwards data flow analysis we maintain the data at the
      block end. This is not easy to do with JVML because a block has many
      exceptional ends. So we maintain the data for the statement start. *)

  val pretty: Format.formatter -> t -> unit
  (** Pretty-print the state *)

  val funcExitData: t
  (** The data at function exit.  Used for statements with no successors.
      This is usually bottom, since we'll also use doStmt on Return
      statements. *)

  val combineStmtStartData: Cil_types.stmt -> old:t -> t -> t option
  (** When the analysis reaches the start of a block, combine the old data with
      the one we have just computed. Return None if the combination is the same
      as the old data, otherwise return the combination. In the latter case, the
      predecessors of the statement are put on the working list. *)

  val combineSuccessors: t -> t -> t
  (** Take the data from two successors and combine it *)

  val doStmt: Cil_types.stmt -> t action
  (** The (backwards) transfer function for a branch. The [(Cil.CurrentLoc.get
      ())] is set before calling this. If it returns None, then we have some
      default handling. Otherwise, the returned data is the data before the
      branch (not considering the exception handlers) *)

  val doInstr: Cil_types.stmt -> Cil_types.instr -> t -> t action
  (** The (backwards) transfer function for an instruction. The
      [(Cil.CurrentLoc.get ())] is set before calling this. If it returns None,
      then we have some default handling. Otherwise, the returned data is the
      data before the branch (not considering the exception handlers) *)

  val filterStmt: Cil_types.stmt -> Cil_types.stmt -> bool
  (** Whether to put this predecessor block in the worklist. We give the
      predecessor and the block whose predecessor we are (and whose data has
      changed) *)

  module StmtStartData: StmtStartData with type data = t
  (** For each block id, the data at the start. This data structure must be
      initialized with the initial data for each block *)

end

module Backwards(T : BackwardsTransfer) : sig
  val compute: Cil_types.stmt list -> unit
(** Fill in the T.stmtStartData, given a number of initial statements to start
    from (the sinks for the backwards data flow). All of the statements (not
    just the initial ones!) must have some entry in T.stmtStartData If you want
    to use bottom for the initial data, you should pass the complete list of
    statements to {!compute}, so that everything is visited.  {!find_stmts} may
    be useful here. *)
end

val find_stmts: Cil_types.fundec -> (Cil_types.stmt list * Cil_types.stmt list)
(** @return (all_stmts, sink_stmts), where all_stmts is a list of the statements
    in a function, and sink_stmts is a list of the return statements (including
    statements that fall through the end of a void function).  Useful when you
    need an initial set of statements for BackwardsDataFlow.compute. *)

(*
  Local Variables:
  compile-command: "make -C ../../.."
  End:
 *)
