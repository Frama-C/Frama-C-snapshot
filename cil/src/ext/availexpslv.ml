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

(* compute available expressions, although in a somewhat
   non-traditional way. the abstract state is a mapping from
   lvalues to expressions as opposed to a set of
   expressions *)

open Cil_types
open Cil

let debug = ref false


(*
 * When ignore_inst returns true, then
 * the instruction in question has no
 * effects on the abstract state.
 * When ignore_call returns true, then
 * the instruction only has side-effects
 * from the assignment if there is one.
 *)
let ignore_inst = ref (fun _i -> false)
let ignore_call = ref (fun _i -> false)

let registerIgnoreInst (f : instr -> bool) : unit =
  let f' = !ignore_inst in
  ignore_inst := (fun i -> (f i) || (f' i))

let registerIgnoreCall (f : instr -> bool) : unit =
  let f' = !ignore_call in
  ignore_call := (fun i -> (f i) || (f' i))


module LvExpHash = Cil_datatype.LvalStructEq.Hashtbl

(* exp LvExpHash.t -> exp LvExpHash.t -> bool *)
let lvh_equals lvh1 lvh2 =
  if not(LvExpHash.length lvh1 = LvExpHash.length lvh2)
  then false
  else LvExpHash.fold (fun lv e b ->
    if not b then b else
    try let e2 = LvExpHash.find lvh2 lv in
    if not (Expcompare.compareExpStripCasts e e2)
    then false
    else true
    with Not_found -> false)
      lvh1 true

let lvh_pretty fmt lvh =
  LvExpHash.iter
    (fun lv e ->
      Format.fprintf fmt "@\n%a -> %a" 
	Cil_printer.pp_lval lv Cil_printer.pp_exp e)
    lvh

(* the result must be the intersection of eh1 and eh2 *)
let lvh_combine lvh1 lvh2 =
  if !debug then Kernel.debug ~level:2 "lvh_combine: combining %a\n and\n %a"
    lvh_pretty lvh1 lvh_pretty lvh2;
  let lvh' = LvExpHash.copy lvh1 in (* eh' gets all of eh1 *)
  LvExpHash.iter (fun lv e1 ->
    try let e2l = LvExpHash.find_all lvh2 lv in
    if not(List.exists (fun e2 -> Expcompare.compareExpStripCasts e1 e2) e2l)
    (* remove things from eh' that eh2 doesn't have *)
    then let e1l = LvExpHash.find_all lvh' lv in
    let e1l' =
      List.filter (fun e -> not(Expcompare.compareExpStripCasts e e1)) e1l
    in
    LvExpHash.remove lvh' lv;
    List.iter (fun e -> LvExpHash.add lvh' lv e) e1l'
    with Not_found ->
      LvExpHash.remove lvh' lv) lvh1;
  if !debug then Kernel.debug "with result %a" lvh_pretty lvh';
  lvh'


(* On a memory write, kill expressions containing memory reads
   variables whose address has been taken, and globals. *)
class memReadOrAddrOfFinderClass br = object
  inherit nopCilVisitor

  method! vexpr e = match e.enode with
  | Lval(Mem _, _) -> begin
      br := true;
      SkipChildren
  end
  | AddrOf(Var _vi, NoOffset) ->
      (* Writing to memory won't change the address of something *)
      SkipChildren
  | _ -> DoChildren

  method! vvrbl vi =
    if vi.vaddrof || vi.vglob then
      (br := true;
       SkipChildren)
    else DoChildren

end

(* exp -> bool *)
let exp_has_mem_read e =
  let br = ref false in
  let vis = new memReadOrAddrOfFinderClass br in
  ignore(visitCilExpr vis e);
  !br

let lval_has_mem_read lv =
  let br = ref false in
  let vis = new memReadOrAddrOfFinderClass br in
  ignore(visitCilLval vis lv);
  !br

let lvh_kill_mem lvh =
  LvExpHash.iter (fun lv e ->
    if exp_has_mem_read e || lval_has_mem_read lv
    then LvExpHash.remove lvh lv)
    lvh

(* need to kill exps containing a particular vi sometimes *)
class viFinderClass vi br = object
  inherit nopCilVisitor

  method! vvrbl vi' =
    if vi.vid = vi'.vid
    then (br := true; SkipChildren)
    else DoChildren

end

let exp_has_vi vi e =
  let br = ref false in
  let vis = new viFinderClass vi br in
  ignore(visitCilExpr vis e);
  !br

let lval_has_vi vi lv =
  let br = ref false in
  let vis = new viFinderClass vi br in
  ignore(visitCilLval vis lv);
  !br

let lvh_kill_vi lvh vi =
  LvExpHash.iter (fun lv e ->
    if exp_has_vi vi e || lval_has_vi vi lv
    then LvExpHash.remove lvh lv)
    lvh

(* need to kill exps containing a particular lval sometimes *)
class lvalFinderClass lv br = object
  inherit nopCilVisitor

  method! vlval l =
    if compareLval l lv
    then (br := true; SkipChildren)
    else DoChildren

end

let exp_has_lval lv e =
  let br = ref false in
  let vis = new lvalFinderClass lv br in
  ignore(visitCilExpr vis e);
  !br

let lval_has_lval lv (host,hostoff) =
  let br = ref false in
  let vis = new lvalFinderClass lv br in
  (match host with
  | Mem e -> ignore(visitCilExpr vis e)
  | _ -> ());
  ignore(visitCilOffset vis hostoff);
  !br

let lvh_kill_lval lvh lv =
  LvExpHash.iter (fun lv' e ->
    if exp_has_lval lv e || lval_has_lval lv lv'
    then LvExpHash.remove lvh lv')
    lvh


class volatileFinderClass br = object
  inherit nopCilVisitor

  method! vexpr e =
    if (hasAttribute "volatile" (typeAttrs (typeOf e)))
    then (br := true; SkipChildren)
    else DoChildren
end

let exp_is_volatile e : bool =
  let br = ref false in
  let vis = new volatileFinderClass br in
  ignore(visitCilExpr vis e);
  !br

class addrOfOrGlobalFinderClass br = object
  inherit nopCilVisitor

  method! vvrbl vi =
    if vi.vaddrof || vi.vglob
    then (br := true; SkipChildren)
    else DoChildren

end

let lval_has_addrof_or_global lv =
  let br = ref false in
  let vis = new addrOfOrGlobalFinderClass br in
  ignore(visitCilLval vis lv);
  !br

let lvh_kill_addrof_or_global lvh =
  LvExpHash.iter (fun lv _ ->
    if lval_has_addrof_or_global lv
    then LvExpHash.remove lvh lv)
    lvh


let lvh_handle_inst i lvh =
  if (!ignore_inst) i then lvh else
  match i with
    Set(lv,e,_) -> begin
      match lv with
      | (Mem _, _) -> begin
          LvExpHash.replace lvh lv e;
	  lvh_kill_mem lvh;
	  lvh_kill_addrof_or_global lvh;
	  lvh
      end
      | _ when not (exp_is_volatile e) -> begin
	  (* ignore x = x *)
	  if Expcompare.compareExpStripCasts (dummy_exp (Lval lv)) e then lvh
	  else begin
	    LvExpHash.replace lvh lv e;
	    lvh_kill_lval lvh lv;
	    lvh
	  end
      end
      | _ -> begin (* e is volatile *)
	  (* must remove mapping for lv *)
	  if !debug then 
	    Kernel.debug "lvh_handle_inst: %a is volatile. killing %a"
              Cil_printer.pp_exp e Cil_printer.pp_lval lv;
	  LvExpHash.remove lvh lv;
	  lvh_kill_lval lvh lv;
	  lvh
      end
    end
  | Call(Some lv,_,_,_) -> begin
      LvExpHash.remove lvh lv;
      lvh_kill_lval lvh lv;
      if not((!ignore_call) i) then begin
	lvh_kill_mem lvh;
	lvh_kill_addrof_or_global lvh
      end;
      lvh
  end
  | Call(_,_,_,_) -> begin
      if not((!ignore_call) i) then begin
	lvh_kill_mem lvh;
	lvh_kill_addrof_or_global lvh;
      end;
      lvh
  end
  | Asm(_,_,_,_,_,_,_) -> begin
      let _,d = Usedef.computeUseDefInstr i in
      Cil_datatype.Varinfo.Set.iter (fun vi ->
	lvh_kill_vi lvh vi) d;
      lvh
  end
  | Code_annot _ | Skip _ -> lvh

module AvailableExps =
  struct

    let name = "Available Expressions"

    let debug = false

    (* mapping from var id to expression *)
    type t = exp LvExpHash.t

    module StmtStartData =
      Dataflow2.StartData(struct type t = exp LvExpHash.t let size = 64 end)

    let copy = LvExpHash.copy

    let pretty = lvh_pretty

    let computeFirstPredecessor _stm lvh = lvh

    let combinePredecessors (_stm:stmt) ~(old:t) (lvh:t) =
      if lvh_equals old lvh then None else
      Some(lvh_combine old lvh)

    let doInstr _ i lvh = lvh_handle_inst i lvh

    let doStmt _stm _astate = Dataflow2.SDefault

    let doGuard _ _c _astate = Dataflow2.GDefault, Dataflow2.GDefault

    let doEdge _ _ d = d

  end

module AE = Dataflow2.Forwards(AvailableExps)


(*
 * Computes AEs for function fd.
 *
 *
 *)
let computeAEs fd =
  try let slst = fd.sbody.bstmts in
  let first_stm = List.hd slst in
  AvailableExps.StmtStartData.clear ();
  AvailableExps.StmtStartData.add first_stm (LvExpHash.create 4);
  AE.compute [first_stm]
  with Failure "hd" -> if !debug then Kernel.debug "fn w/ no stmts?"
  | Not_found -> if !debug then Kernel.debug "no data for first_stm?"


(* get the AE data for a statement *)
let getAEs sid =
  try Some(AvailableExps.StmtStartData.find sid)
  with Not_found -> None

(* get the AE data for an instruction list *)
let instrAEs il _sid lvh _out =
  if !debug then Kernel.debug "instrAEs" ;
  let proc_one hil i =
    match hil with
      [] -> let lvh' = LvExpHash.copy lvh in
      let lvh'' = lvh_handle_inst i lvh' in
       lvh''::hil
    | lvh'::_ehrst as l ->
	let lvh' = LvExpHash.copy lvh' in
	let lvh'' = lvh_handle_inst i lvh' in
	lvh''::l
  in
  let folded = List.fold_left proc_one [lvh] il in
  let foldednotout = List.rev (List.tl folded) in
  foldednotout

class aeVisitorClass = object (self)
  inherit nopCilVisitor

  val mutable ae_dat_lst = []

  val mutable cur_ae_dat = None

  method! vstmt stm =
    match getAEs stm with
    | None ->
	if !debug then Kernel.debug "aeVis: stm %d has no data" stm.sid ;
	cur_ae_dat <- None;
	DoChildren
    | Some eh ->
	match stm.skind with
	  Instr il ->
	    if !debug then Kernel.debug "aeVist: visit il" ;
	    ae_dat_lst <- instrAEs [il] stm.sid eh false;
	    DoChildren
	| _ ->
	    if !debug then Kernel.debug "aeVisit: visit non-il" ;
	    cur_ae_dat <- None;
	    DoChildren

  method! vinst i =
    if !debug then 
      Kernel.debug "aeVist: before %a, ae_dat_lst is %d long"
	Cil_printer.pp_instr i (List.length ae_dat_lst);
    try
      let data = List.hd ae_dat_lst in
      cur_ae_dat <- Some(data);
      ae_dat_lst <- List.tl ae_dat_lst;
      if !debug then Kernel.debug "aeVisit: data is %a" lvh_pretty data;
      DoChildren
    with Failure "hd" ->
      if !debug then Kernel.debug "aeVis: il ae_dat_lst mismatch";
      DoChildren

  method get_cur_eh () =
    match cur_ae_dat with
    | None -> getAEs (Extlib.the self#current_stmt)
    | Some eh -> Some eh

end
