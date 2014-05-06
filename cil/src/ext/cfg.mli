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

(** Code to compute the control-flow graph of a function or file.
    This will fill in the [preds] and [succs] fields of {!Cil.stmt}

    This is required for several other extensions, such as {!Dataflow2}.
    @plugin development guide *)

open Cil_types

(** Compute the CFG for an entire file, by calling cfgFun on each function. *)
val computeFileCFG: file -> unit

(** clear the sid (except when clear_id is explicitly set to false),
    succs, and preds fields of each statement. *)
val clearFileCFG: ?clear_id:bool -> file -> unit

(** Compute a control flow graph for fd.  Stmts in fd have preds and succs
  filled in *)
val cfgFun : fundec -> unit

(** clear the sid, succs, and preds fields of each statment in a function *)
val clearCFGinfo: ?clear_id:bool -> fundec -> unit


(** print control flow graph (in dot form) for fundec to channel *)
val printCfgChannel : Format.formatter -> fundec -> unit

(** Print control flow graph (in dot form) for fundec to file *)
val printCfgFilename : string -> fundec -> unit

(* [VP] These two functions were initially in Cil, but now depends on stuff in
   Logic_utils. Put there to avoid circular dependencies. *)

(** Prepare a function for CFG information computation by
  * {!Cil.computeCFGInfo}. This function converts all [Break], [Switch],
  * [Default] and [Continue] {!Cil_types.stmtkind}s and {!Cil_types.label}s into [If]s
  * and [Goto]s, giving the function body a very CFG-like character. This
  * function modifies its argument in place. *)
val prepareCFG: ?keepSwitch:bool -> fundec -> unit

(** Compute the CFG information for all statements in a fundec and return a
  * list of the statements. The input fundec cannot have [Break], [Switch],
  * [Default], or [Continue] {!Cil_types.stmtkind}s or {!Cil_types.label}s. Use
  * {!Cil.prepareCFG} to transform them away.  The second argument should
  * be [true] if you wish a global statement number, [false] if you wish a
  * local (per-function) statement numbering. The list of statements is set
  * in the sallstmts field of a fundec.
  *
  * NOTE: unless you want the simpler control-flow graph provided by
  * prepareCFG, or you need the function's smaxstmtid and sallstmt fields
  * filled in, we recommend you use [Cfg.computeFileCFG] instead of this
  * function to compute control-flow information.
  * [Cfg.computeFileCFG] is newer and will handle switch, break, and
  * continue correctly.*)
val computeCFGInfo: fundec -> bool -> unit

(* [VP] End import from Cil *)

val clear_sid_info_ref: (unit -> unit) ref

(*
(* Next statement id that will be assigned. *)
val start_id: int ref

(** All of the nodes in a file. *)
val nodeList : stmt list ref

(** number of nodes in the CFG *)
val numNodes : int ref
*)
