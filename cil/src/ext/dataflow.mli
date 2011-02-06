(**************************************************************************)
(*                                                                        *)
(*  Copyright (C) 2001-2003                                               *)
(*   George C. Necula    <necula@cs.berkeley.edu>                         *)
(*   Scott McPeak        <smcpeak@cs.berkeley.edu>                        *)
(*   Wes Weimer          <weimer@cs.berkeley.edu>                         *)
(*   Ben Liblit          <liblit@cs.berkeley.edu>                         *)
(*  All rights reserved.                                                  *)
(*                                                                        *)
(*  Redistribution and use in source and binary forms, with or without    *)
(*  modification, are permitted provided that the following conditions    *)
(*  are met:                                                              *)
(*                                                                        *)
(*  1. Redistributions of source code must retain the above copyright     *)
(*  notice, this list of conditions and the following disclaimer.         *)
(*                                                                        *)
(*  2. Redistributions in binary form must reproduce the above copyright  *)
(*  notice, this list of conditions and the following disclaimer in the   *)
(*  documentation and/or other materials provided with the distribution.  *)
(*                                                                        *)
(*  3. The names of the contributors may not be used to endorse or        *)
(*  promote products derived from this software without specific prior    *)
(*  written permission.                                                   *)
(*                                                                        *)
(*  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS   *)
(*  "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT     *)
(*  LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS     *)
(*  FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE        *)
(*  COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,   *)
(*  INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,  *)
(*  BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;      *)
(*  LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER      *)
(*  CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT    *)
(*  LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN     *)
(*  ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE       *)
(*  POSSIBILITY OF SUCH DAMAGE.                                           *)
(*                                                                        *)
(*  File modified by CEA (Commissariat à l'énergie atomique et aux        *)
(*                        énergies alternatives).                         *)
(**************************************************************************)

(** A framework for data flow analysis for CIL code.  Before using
    this framework, you must initialize the Control-flow Graph for your
    program, e.g using [Cfg.computeFileCFG].
    @plugin development guide *)

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

(* For if statements *)
type 't guardaction =
    GDefault      (** The default state *)
  | GUse of 't    (** Use this data for the branch *)
  | GUnreachable  (** The branch will never be taken. *)

module type StmtStartData = sig
  type data
  val clear: unit -> unit
  val mem: int -> bool
  val find: int -> data
  val replace: int -> data -> unit
  val add: int -> data -> unit
  val iter: (int -> data -> unit) -> unit
  val length: unit -> int
end

module StmtStartData(X:sig type t val size: int end) :
  StmtStartData with type data = X.t

(******************************************************************
 **********
 **********         FORWARDS
 **********
 ********************************************************************)

module type ForwardsTransfer = sig
  val name: string (** For debugging purposes, the name of the analysis *)

  val debug: bool ref (** Whether to turn on debugging *)

  type t  (** The type of the data we compute for each block start. May be
           * imperative.  *)

  module StmtStartData: StmtStartData with type data = t
  (** For each statement id, the data at the start. Not found in the hash
   * table means nothing is known about the state at this point. At the end
   * of the analysis this means that the block is not reachable. *)

  val copy: t -> t
  (** Make a deep copy of the data *)

  val pretty: Format.formatter -> t -> unit
  (** Pretty-print the state *)

  val computeFirstPredecessor: Cil_types.stmt -> t -> t
  (** Give the first value for a predecessors, compute the value to be set
   * for the block *)

  val combinePredecessors: Cil_types.stmt -> old:t -> t -> t option
  (** Take some old data for the start of a statement, and some new data for
   * the same point. Return None if the combination is identical to the old
   * data. Otherwise, compute the combination, and return it. *)

  val doInstr: Cil_types.stmt -> Cil_types.instr -> t -> t action
  (** The (forwards) transfer function for an instruction. The
   * [(Cil.CurrentLoc.get ())] is set before calling this. The default action is to
   * continue with the state unchanged. *)

  val doGuard: Cil_types.stmt -> Cil_types.exp -> t -> 
    t guardaction * t guardaction
  (** Generate the successors [th, el] to an 
    *  If statement assuming the given expression
    * is respectively nonzero and zero. 
    * Analyses that don't need guard information can return
    * GDefault, GDefault; this is equivalent to returning GUse of the input.
    * A return value of GUnreachable indicates that this half of the branch
    * will not be taken and should not be explored.  This will be called
    * once per If.
    * [stmt] is the corresponding [If] statement FYI only.
    *)

  val doStmt: Cil_types.stmt -> t -> t stmtaction
  (** The (forwards) transfer function for a statement. The [(Cil.CurrentLoc.get ())]
   * is set before calling this. The default action is to do the instructions
   * in this statement, if applicable, and continue with the successors. *)

  val filterStmt: Cil_types.stmt -> bool
  (** Whether to put this statement in the worklist. This is called when a
   * block would normally be put in the worklist. *)

  val stmt_can_reach : Cil_types.stmt -> Cil_types.stmt -> bool

   val doEdge: Cil_types.stmt -> Cil_types.stmt -> t -> t
    (** what to do when following the edge between the two given statements.
        Can default to identity if nothing special is required.
     *)

end

module ForwardsDataFlow(T : ForwardsTransfer) : sig
  val reachedStatement : Cil_types.stmt -> Cil_types.stmt -> T.t -> unit
  val compute: Cil_types.stmt list -> unit
  (** Fill in the T.stmtStartData, given a number of initial statements to
   * start from. All of the initial statements must have some entry in
   * T.stmtStartData (i.e., the initial data should not be bottom) *)
end

(******************************************************************
 **********
 **********         BACKWARDS
 **********
 ********************************************************************)
module type BackwardsTransfer = sig
  val name: string (** For debugging purposes, the name of the analysis *)

  val debug: bool ref (** Whether to turn on debugging *)

  type t  (** The type of the data we compute for each block start. In many
           * presentations of backwards data flow analysis we maintain the
           * data at the block end. This is not easy to do with JVML because
           * a block has many exceptional ends. So we maintain the data for
           * the statement start. *)

  module StmtStartData: StmtStartData with type data = t
  (** For each block id, the data at the start. This data structure must be
   * initialized with the initial data for each block *)

  val pretty: Format.formatter -> t -> unit (** Pretty-print the state *)

  val funcExitData: t
  (** The data at function exit.  Used for statements with no successors.
      This is usually bottom, since we'll also use doStmt on Return
      statements. *)

  val combineStmtStartData: Cil_types.stmt -> old:t -> t -> t option

  (** When the analysis reaches the start of a block, combine the old data
   * with the one we have just computed. Return None if the combination is
   * the same as the old data, otherwise return the combination. In the
   * latter case, the predecessors of the statement are put on the working
   * list. *)


  val combineSuccessors: t -> t -> t
  (** Take the data from two successors and combine it *)


  val doStmt: Cil_types.stmt -> t action
  (** The (backwards) transfer function for a branch. The [(Cil.CurrentLoc.get ())] is
   * set before calling this. If it returns None, then we have some default
   * handling. Otherwise, the returned data is the data before the branch
   * (not considering the exception handlers) *)

  val doInstr: Cil_types.stmt -> Cil_types.instr -> t -> t action
  (** The (backwards) transfer function for an instruction. The
   * [(Cil.CurrentLoc.get ())] is set before calling this. If it returns None, then we
   * have some default handling. Otherwise, the returned data is the data
   * before the branch (not considering the exception handlers) *)

  val filterStmt: Cil_types.stmt -> Cil_types.stmt -> bool
  (** Whether to put this predecessor block in the worklist. We give the
   * predecessor and the block whose predecessor we are (and whose data has
   * changed)  *)

end

module BackwardsDataFlow(T : BackwardsTransfer) : sig
  val compute: Cil_types.stmt list -> unit
  (** Fill in the T.stmtStartData, given a number of initial statements to
   * start from (the sinks for the backwards data flow). All of the statements
   * (not just the initial ones!) must have some entry in T.stmtStartData
   * If you want to use bottom for the initial data, you should pass the
   * complete list of statements to {!compute}, so that everything is visited.
   * {!find_stmts} may be useful here. *)
end

(** Returns (all_stmts, sink_stmts), where all_stmts is a list of the
  statements in a function, and sink_stmts is a list of the return statments
  (including statements that fall through the end of a void function).
  Useful when you need an initial set of statements for
  BackwardsDataFlow.compute. *)
val find_stmts: Cil_types.fundec -> (Cil_types.stmt list * Cil_types.stmt list)
