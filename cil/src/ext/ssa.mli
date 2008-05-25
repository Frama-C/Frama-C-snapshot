(**************************************************************************)
(*                                                                        *)
(*  Copyright (C) 2001-2003,                                              *)
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
(*  File modified by CEA (Commissariat à l'Énergie Atomique).             *)
(**************************************************************************)

type cfgInfo = {
    name: string; (* The function name *)
    start   : int;          
    size    : int;    
    blocks: cfgBlock array; (** Dominating blocks must come first *)
    successors: int list array; (* block indices *)
    predecessors: int list array;   
    mutable nrRegs: int;
    mutable regToVarinfo: Cil.varinfo array; (** Map register IDs to varinfo *)
  } 

(** A block corresponds to a statement *)
and cfgBlock = { 
    bstmt: Cil.stmt;

    (* We abstract the statement as a list of def/use instructions *)
    instrlist: instruction list;
    mutable livevars: (reg * int) list;  
    (** For each variable ID that is live at the start of the block, the 
     * block whose definition reaches this point. If that block is the same 
     * as the current one, then the variable is a phi variable *)
    mutable reachable: bool;
  }
  
and instruction = (reg list * reg list) 
  (* lhs variables, variables on rhs.  *)


and reg = int

type idomInfo = int array  (* immediate dominator *)

and dfInfo = (int list) array  (* dominance frontier *)

and oneSccInfo = {
    nodes: int list;
    headers: int list;
    backEdges: (int*int) list;
  } 

and sccInfo = oneSccInfo list 

val add_ssa_info: cfgInfo -> unit
val stronglyConnectedComponents: cfgInfo -> bool -> sccInfo 
val prune_cfg: cfgInfo -> cfgInfo
