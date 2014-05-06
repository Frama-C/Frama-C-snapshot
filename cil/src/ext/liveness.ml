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

(* Calculate which variables are live at
 * each statememnt.
 *
 *
 *
 *)

open Cil_types
open Cil

module UD = Usedef
module VS = Cil_datatype.Varinfo.Set

let debug = ref false

let live_label = ref ""
let live_func = ref ""

let debug_print fmt vs = VS.fold
    (fun vi _d ->
      Format.fprintf fmt "name: %s id:%d " vi.vname
	vi.vid)
    vs ();
  Format.fprintf fmt "@\n"


let min_print fmt vs = 
  VS.iter
    (fun vi -> Format.fprintf fmt "%s(%a)," vi.vname Cil_printer.pp_typ vi.vtype)
    vs;
  Format.fprintf fmt "@\n"

let printer = ref debug_print

module LiveFlow = struct
  let name = "Liveness"
  let debug = false
  type t = VS.t
  module StmtStartData =
    Dataflow2.StartData(struct type t = VS.t let size = 32 end)

  let pretty fmt vs =
    let fn = !printer in
    fn fmt vs

  let funcExitData = VS.empty

  let combineStmtStartData (_stm:stmt) ~(old:t) (now:t) =
    if not(VS.compare old now = 0)
    then Some now
    else None

  let combineSuccessors = VS.union

  let doStmt stmt =
    if debug then Kernel.debug "looking at: %a\n" Cil_printer.pp_stmt stmt;
    match stmt.succs with
      [] -> let u,_d = UD.computeUseDefStmtKind stmt.skind in
      if debug then (Kernel.debug "doStmt: no succs %d\n" stmt.sid);
      Dataflow2.Done u
    | _ ->
	let handle_stm vs = match stmt.skind with
	  Instr _ -> vs
	| s -> let u, d = UD.computeUseDefStmtKind s in
	  VS.union u (VS.diff vs d)
	in
	Dataflow2.Post handle_stm

  let doInstr _ i _vs =
    let transform vs' =
      let u,d = UD.computeUseDefInstr i in
      VS.union u (VS.diff vs' d)
    in
    Dataflow2.Post transform

  let filterStmt _stm1 _stm2 = true
end

module L = Dataflow2.Backwards(LiveFlow)

(* XXX: This does not compute the best ordering to
 * give to the work-list algorithm.
 *)
let all_stmts = ref []
class nullAdderClass = object
  inherit nopCilVisitor

  method! vstmt s =
    all_stmts := s :: (!all_stmts);
    LiveFlow.StmtStartData.add s VS.empty;
    DoChildren

end

let null_adder fdec =
  ignore(visitCilFunction (new nullAdderClass) fdec);
  !all_stmts

let computeLiveness fdec =
  LiveFlow.StmtStartData.clear ();
  UD.onlyNoOffsetsAreDefs := false;
  all_stmts := [];
  let a = null_adder fdec in
  L.compute a

let getLiveSet sid =
  try Some(LiveFlow.StmtStartData.find sid)
  with Not_found -> None

let print_everything () =
  LiveFlow.StmtStartData.iter (fun s vs ->
             Format.printf "%d: %a" s.sid LiveFlow.pretty vs)

let match_label lbl = match lbl with
  Label(str,_,_b) ->
    if !debug then (Kernel.debug "Liveness: label seen: %s\n" str);
    (*b && *)(String.compare str (!live_label) = 0)
| _ -> false

class doFeatureClass = object
  inherit nopCilVisitor

  method! vfunc fd =
    if String.compare fd.svar.vname (!live_func) = 0 then
      (Cfg.clearCFGinfo fd;
       ignore(Cfg.cfgFun fd);
       computeLiveness fd;
       if String.compare (!live_label) "" = 0 then
	 (printer := min_print;
	  print_everything ();
	  SkipChildren)
       else DoChildren)
    else SkipChildren

  method! vstmt s =
    if List.exists match_label s.labels then try
      let vs = LiveFlow.StmtStartData.find s in
      (printer := min_print;
       Format.printf "%a" LiveFlow.pretty vs;
       SkipChildren)
    with Not_found ->
      if !debug then (Kernel.debug "Liveness: stmt: %d not found\n" s.sid);
      DoChildren
    else
      (if List.length s.labels = 0 then
	if !debug then (Kernel.debug "Liveness: no label at sid=%d\n" s.sid);
      DoChildren)

end

let do_live_feature (f:file) =
  visitCilFile (new doFeatureClass) f

let feature =
  {
   fd_name = "Liveness";
   fd_enabled = ref false;
   fd_description = "Spit out live variables at a label";
   fd_extraopt = [
   "--live_label",
   Arg.String (fun s -> live_label := s),
   "Output the variables live at this label";
   "--live_func",
   Arg.String (fun s -> live_func := s),
   "Output the variables live at each statement in this function.";
   "--live_debug",
   Arg.Unit (fun _n -> debug := true),
   "Print lots of debugging info";];
   fd_doit = do_live_feature;
   fd_post_check = false
 }
