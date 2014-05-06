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

(* Calculate reaching definitions for each instruction.
 * Determine when it is okay to replace some variables with
 * expressions.
 *
 * After calling computeRDs on a fundec,
 * ReachingDef.stmtStartData will contain a mapping from
 * statement ids to data about which definitions reach each
 * statement. ReachingDef.defIdStmtHash will contain a
 * mapping from definition ids to the statement in which
 * that definition takes place.
 *
 * instrRDs takes a list of instructions, and the
 * definitions that reach the first instruction, and
 * for each instruction figures out which definitions
 * reach into or out of each instruction.
 *
 *)
open Cil_types
open Cil

module DF = Dataflow2
module UD = Usedef
module L = Liveness
module IH = Datatype.Int.Hashtbl
  (* This module always uses "int = varinfo.vid", but generate some new ids
     at some point. Thus, it cannot be easily be replaced by
     Cil_datatype.Varinfo.Hashtbl... *)

let debug_fn = ref ""

module IOS =
  FCSet.Make(struct
    type t = int option
    let compare io1 io2 =
      match io1, io2 with
	Some i1, Some i2 -> Datatype.Int.compare i1 i2
      | Some _i1, None -> 1
      | None, Some _i2 -> -1
      | None, None -> 0
  end)

let debug = ref false

(* return the intersection of
   Datatype.Int.Hashtbles ih1 and ih2 *)
let ih_inter ih1 ih2 =
  let ih' = IH.copy ih1 in
  IH.iter (fun id _vi ->
    if not(IH.mem ih2 id) then
      IH.remove ih' id else
      ()) ih1;
  ih'

let ih_union ih1 ih2 =
  let ih' = IH.copy ih1 in
  IH.iter (fun id vi ->
    if not(IH.mem ih' id)
    then IH.add ih' id vi
    else ()) ih2;
  ih'

(* Lookup varinfo in iosh. If the set contains None
   or is not a singleton, return None, otherwise
   return Some of the singleton *)
(* IOS.t IH.t -> varinfo -> int option *)
let iosh_singleton_lookup iosh vi =
  if IH.mem iosh vi.vid then
    let ios = IH.find iosh vi.vid in
    if not (IOS.cardinal ios = 1) then None
    else IOS.choose ios
  else None

(* IOS.t IH.t -> varinfo -> IOS.t *)
let iosh_lookup iosh vi =
  if IH.mem iosh vi.vid
  then Some(IH.find iosh vi.vid)
  else None

(* return Some(vid) if iosh contains defId.
   return None otherwise *)
(* IOS.t IH.t -> int -> int option *)
let iosh_defId_find iosh defId =
  (* int -> IOS.t -> int option -> int option*)
  let get_vid vid ios io =
    match io with
      Some(i) -> Some(i)
    | None ->
	let there = IOS.exists
	    (function None -> false
	      | Some(i') -> defId = i') ios in
	if there then Some(vid) else None
  in
  IH.fold get_vid iosh None

(* The resulting iosh will contain the
   union of the same entries from iosh1 and
   iosh2. If iosh1 has an entry that iosh2
   does not, then the result will contain
   None in addition to the things from the
   entry in iosh1. *)
(* XXX this function is a performance bottleneck *)
let iosh_combine iosh1 iosh2 =
  let iosh' = IH.copy iosh1 in
  IH.iter (fun id ios1 ->
    try let ios2 = IH.find iosh2 id in
    let newset = IOS.union ios1 ios2 in
    IH.replace iosh' id newset;
    with Not_found ->
      let newset = IOS.add None ios1 in
      IH.replace iosh' id newset) iosh1;
  IH.iter (fun id ios2 ->
    try ignore(IH.find iosh1 id)
    with Not_found -> begin
    (*if not(IH.mem iosh1 id) then*)
      let newset = IOS.add None ios2 in
      IH.add iosh' id newset end) iosh2;
  iosh'


(* determine if two IOS.t IH.t s are the same *)
let iosh_equals iosh1 iosh2 =
(*  if IH.length iosh1 = 0 && not(IH.length iosh2 = 0) ||
  IH.length iosh2 = 0 && not(IH.length iosh1 = 0)*)
  if not(IH.length iosh1 = IH.length iosh2)
  then
    (Kernel.debug "iosh_equals: length not same" ; false)
  else
    IH.fold (fun vid ios b ->
      if not b then b else
      try let ios2 = IH.find iosh2 vid in
      if not(IOS.compare ios ios2 = 0) then
	(Kernel.debug "iosh_equals: sets for vid %d not equal\n" vid ; false)
      else true
      with Not_found ->
	(Kernel.debug "iosh_equals: vid %d not in iosh2\n" vid ; false)) 
      iosh1 true

(* replace an entire set with a singleton.
   if nothing was there just add the singleton *)
(* IOS.t IH.t -> int -> varinfo -> unit *)
let iosh_replace iosh i vi =
  if IH.mem iosh vi.vid then
    let newset = IOS.singleton (Some i) in
    IH.replace iosh vi.vid newset
  else
    let newset = IOS.singleton (Some i) in
    IH.add iosh vi.vid newset


let iosh_filter_dead iosh vs =
  IH.iter (fun vid _ ->
    if not(UD.VS.exists (fun vi -> vid = vi.vid) vs)
    then IH.remove iosh vid)
    iosh


(* remove definitions that are killed.
   add definitions that are gend *)
(* Takes the defs, the data, and a function for
   obtaining the next def id *)
(* VS.t -> IOS.t IH.t -> (unit->int) -> unit *)
let proc_defs vs iosh f =
  let pd vi =
    let newi = f() in
    (*if !debug then
      ignore (E.log "proc_defs: genning %d\n" newi);*)
    iosh_replace iosh newi vi
  in
  UD.VS.iter pd vs

let idMaker () start =
  let counter = ref start in
  fun () ->
    let ret = !counter in
    counter := !counter + 1;
    ret

(* given reaching definitions into a list of
   instructions, figure out the definitions that
   reach in/out of each instruction *)
(* if out is true then calculate the definitions that
   go out of each instruction, if it is false then
   calculate the definitions reaching into each instruction *)
(* instr list -> int -> (varinfo IH.t * int) -> bool -> (varinfo IH.t * int) list *)
let iRDsHtbl = Hashtbl.create 128
let instrRDs il sid (_ivih, s, iosh) out =
  if Hashtbl.mem iRDsHtbl (sid,out) then Hashtbl.find iRDsHtbl (sid,out) else

(*   let print_instr i (_,s', iosh') = *)
(*     let d = d_instr () i ++ line in *)
(*     fprint stdout 80 d; *)
(*     flush stdout *)
(*   in *)

  let proc_one hil i =
    match hil with
    | [] ->
	let _, defd = UD.computeUseDefInstr i in
	if UD.VS.is_empty defd
	then ((*if !debug then print_instr i ((), s, iosh);*)
	      [((), s, iosh)])
	else
	  let iosh' = IH.copy iosh in
	  proc_defs defd iosh' (idMaker () s);
	  (*if !debug then
	    print_instr i ((), s + UD.VS.cardinal defd, iosh');*)
	  ((), s + UD.VS.cardinal defd, iosh')::hil
    | (_, s', iosh')::_hrst as l ->
	let _, defd = UD.computeUseDefInstr i in
	if UD.VS.is_empty defd
	then
	  ((*if !debug then
	    print_instr i ((),s', iosh');*)
	   ((), s', iosh')::l)
	else let iosh'' = IH.copy iosh' in
	proc_defs defd iosh'' (idMaker () s');
	(*if !debug then
	  print_instr i ((), s' + UD.VS.cardinal defd, iosh'');*)
	((),s' + UD.VS.cardinal defd, iosh'')::l
  in
  let folded = List.fold_left proc_one [((),s,iosh)] [il] in
  let foldedout = List.tl (List.rev folded) in
  let foldednotout = List.rev (List.tl folded) in
  Hashtbl.add iRDsHtbl (sid,true) foldedout;
  Hashtbl.add iRDsHtbl (sid,false) foldednotout;
  if out then foldedout else foldednotout



(* The right hand side of an assignment is either
   a function call or an expression *)
type rhs = RDExp of exp | RDCall of instr

module ReachingDef =
  struct

    let name = "Reaching Definitions"

    let debug = false

    (* Should the analysis calculate may-reach
       or must-reach *)
    let mayReach = ref false

    (* An integer that tells the id number of
       the first definition *)
    (* Also a hash from variable ids to a set of
       definition ids that reach this statement.
       None means there is a path to this point on which
       there is no definition of the variable *)
    type t = (unit * int * IOS.t IH.t)

    module StmtStartData =
      Dataflow2.StartData
	(struct type t = (unit * int * IOS.t IH.t) let size = 32 end)
    (* entries for starting statements must
       be added before calling compute *)

    let copy (_, i, iosh) = ((), i, Datatype.Int.Hashtbl.copy iosh)

    (* a mapping from definition ids to
       the statement corresponding to that id *)
    let defIdStmtHash = Datatype.Int.Hashtbl.create 32

    (* mapping from statement ids to statements
       for better performance of ok_to_replace *)
    let sidStmtHash = Datatype.Int.Hashtbl.create 64

    (* pretty printer *)
    let pretty _fmt _ = () (* prettyprint defIdStmtHash stmtStartData*)


    (* The first id to use when computeFirstPredecessor
       is next called *)
    let nextDefId = ref 0

    (* Count the number of variable definitions in
       a statement *)
    let num_defs stm =
      match stm.skind with
	Instr(il) -> List.fold_left (fun s i ->
	  let _, d = UD.computeUseDefInstr i in
	  s + UD.VS.cardinal d) 0 [il]
      | _ -> let _, d = UD.computeUseDefStmtKind stm.skind in
	UD.VS.cardinal d

    (* the first predecessor is just the data in along with
       the id of the first definition of the statement,
       which we get from nextDefId *)
    let computeFirstPredecessor stm (_, s, iosh) =
      let startDefId = max !nextDefId s in
      let numds = num_defs stm in
      let rec loop n =
	if n < 0
	then ()
	else
	  (Kernel.debug "RD: defId %d -> stm %d\n" (startDefId + n) stm.sid ;
	   Datatype.Int.Hashtbl.add defIdStmtHash (startDefId + n) stm;
	   loop (n-1))
      in
      loop (numds - 1);
      nextDefId := startDefId + numds;
      ((), startDefId, Datatype.Int.Hashtbl.copy iosh)


    let combinePredecessors (_stm:stmt) ~(old:t) ((_, _s, iosh):t) =
      match old with (_, os, oiosh) -> begin
	if iosh_equals oiosh iosh
	then None
	else
	  Some((), os, iosh_combine oiosh iosh)
      end

    (* return an action that removes things that
       are redefinied and adds the generated defs *)
    let doInstr _ inst (_, s, iosh) =
      let _, defd = UD.computeUseDefInstr inst in
      proc_defs defd iosh (idMaker () s);
      ((), s + UD.VS.cardinal defd, iosh)

    (* all the work gets done at the instruction level *)
    let doStmt stm (_, _s, iosh) =
      if not(Datatype.Int.Hashtbl.mem sidStmtHash stm.sid) then
	Datatype.Int.Hashtbl.add sidStmtHash stm.sid stm;
      if debug then Kernel.debug "RD: looking at %a\n" Cil_printer.pp_stmt stm;
      match L.getLiveSet stm with
      | None -> DF.SDefault
      | Some vs -> begin
	  iosh_filter_dead iosh vs;
	  DF.SDefault
      end


    let doGuard _ _condition _ = DF.GDefault, DF.GDefault

    let doEdge _ _ d = d

end

module RD = Dataflow2.Forwards(ReachingDef)

(* take the id number of a definition and return
   the rhs of the definition if there is one.
   Returns None if, for example, the definition is
   caused by an assembly instruction *)
(* stmt IH.t -> (()*int*IOS.t IH.t) IH.t -> int -> (rhs * int * IOS.t IH.t) option *)
let rhsHtbl = IH.create 64 (* to avoid recomputation *)
let getDefRhs didstmh defId =
  if IH.mem rhsHtbl defId then IH.find rhsHtbl defId else
  let stm =
    try IH.find didstmh defId
    with Not_found -> Kernel.fatal "getDefRhs: defId %d not found\n" defId in
  let (_,s,iosh) =
    try ReachingDef.StmtStartData.find stm
    with Not_found -> Kernel.fatal "getDefRhs: sid %d not found \n" stm.sid in
  match stm.skind with
    Instr il ->
      let ivihl = instrRDs il stm.sid ((),s,iosh) true in (* defs that reach out of each instr *)
      let ivihl_in = instrRDs il stm.sid ((),s,iosh) false in (* defs that reach into each instr *)
      begin try
	let iihl = List.combine (List.combine [il] ivihl) ivihl_in in
	(try let ((i,(_,_,_diosh)),(_,_,iosh_in)) = List.find (fun ((i,(_,_,iosh')),_) ->
	  match iosh_defId_find iosh' defId with
	    Some vid ->
	      (match i with
		Set((Var vi',NoOffset),_,_) -> vi'.vid = vid (* _ -> NoOffset *)
	      | Call(Some(Var vi',NoOffset),_,_,_) -> vi'.vid = vid (* _ -> NoOffset *)
	      | Call(None,_,_,_) -> false
	      | Asm(_,_,sll,_,_,_,_) -> List.exists
		    (function (_,_,(Var vi',NoOffset)) -> vi'.vid = vid | _ -> false) sll
	      | _ -> false)
	  | None -> false) iihl in
	(match i with
	  Set((lh,_),e,_) ->
	    (match lh with
	      Var _vi' ->
		(IH.add rhsHtbl defId (Some(RDExp(e),stm.sid,iosh_in));
		 Some(RDExp(e), stm.sid, iosh_in))
	       | _ -> Kernel.fatal "Reaching Defs getDefRhs: right vi not first")
	| Call(_lvo,_e,_el,_) ->
	    (IH.add rhsHtbl defId (Some(RDCall(i),stm.sid,iosh_in));
	     Some(RDCall(i), stm.sid, iosh_in))
        | Skip _ | Code_annot _ -> None
	| Asm(_a,_sl,_slvl,_sel,_sl',_,_) -> None) (* ? *)
	with Not_found ->
	  (if !debug then (Kernel.debug "getDefRhs: No instruction defines %d" defId);
	   IH.add rhsHtbl defId None;
	   None))
      with Invalid_argument _ -> None end
  | _ -> Kernel.fatal "getDefRhs: defining statement not an instruction list %d" defId
	(*None*)

let prettyprint _fmt _didstmh _stmdat () (_,_s,_iosh) = ()
  (*seq line (fun (vid,ios) ->
    num vid ++ text ": " ++
      IOS.fold (fun io d -> match io with
	None -> d ++ text "None "
      | Some i ->
	  let stm = IH.find didstmh i in
	  match getDefRhs didstmh stmdat i with
	    None -> d ++ num i
	  | Some(RDExp(e),_,_) ->
	      d ++ num i ++ text " " ++ (d_exp () e)
	  | Some(RDCall(c),_,_) ->
	      d ++ num i ++ text " " ++ (d_instr () c))
      ios nil)
    (IH.tolist iosh)*)

(* map all variables in vil to a set containing
   None in iosh *)
(* IOS.t IH.t -> varinfo list -> () *)
let iosh_none_fill iosh vil =
  List.iter (fun vi ->
    IH.add iosh vi.vid (IOS.singleton None))
    vil

let clearMemos () =
  IH.clear rhsHtbl;
  Hashtbl.clear iRDsHtbl


(* Computes the reaching definitions for a
   function. *)
(* Cil.fundec -> unit *)
let computeRDs fdec =
  try
    if String.compare fdec.svar.vname (!debug_fn) = 0 then
      (debug := true;
       Kernel.debug "%s =\n%a\n" (!debug_fn) Cil_printer.pp_block fdec.sbody);
    let bdy = fdec.sbody in
    let slst = bdy.bstmts in
    ReachingDef.StmtStartData.clear ();
    IH.clear ReachingDef.defIdStmtHash;
    IH.clear rhsHtbl;
    Hashtbl.clear iRDsHtbl;
    ReachingDef.nextDefId := 0;
    let fst_stm = List.hd slst in
    let fst_iosh = IH.create 32 in
    UD.onlyNoOffsetsAreDefs := false;
    ReachingDef.StmtStartData.add fst_stm ((), 0, fst_iosh);
    L.computeLiveness fdec;
    ignore(ReachingDef.computeFirstPredecessor fst_stm ((), 0, fst_iosh));
    if !debug then Kernel.debug "computeRDs: fst_stm.sid=%d\n" fst_stm.sid ;
    RD.compute [fst_stm];
    if String.compare fdec.svar.vname (!debug_fn) = 0 then
      debug := false
    (* now ReachingDef.stmtStartData has the reaching def data in it *)
  with Failure "hd" -> if String.compare fdec.svar.vname (!debug_fn) = 0 then
    debug := false

(* return the definitions that reach the statement
   with statement id sid *)
let getRDs sid =
  try
    Some (ReachingDef.StmtStartData.find sid)
  with Not_found ->
    None
(*    E.s (E.error "getRDs: sid %d not found\n" sid) *)

let getDefIdStmt defid =
  try
    Some(IH.find ReachingDef.defIdStmtHash defid)
  with Not_found ->
    None

let getStmt sid =
  try Some(IH.find ReachingDef.sidStmtHash sid)
  with Not_found -> None

(* returns the rhs for the definition *)
let getSimpRhs defId =
  let rhso = getDefRhs ReachingDef.defIdStmtHash defId in
  match rhso with None -> None
  | Some(r,_,_) -> Some(r)

(* check if i is responsible for defId *)
(* instr -> int -> bool *)
let isDefInstr i defId =
  match getSimpRhs defId with
    Some(RDCall i') -> Cil_datatype.Instr.equal i i'
  | _ -> false

(*
(* Pretty print the reaching definition data for
   a function *)
let ppFdec fdec =
  seq line (fun stm ->
    let ivih = IH.find ReachingDef.stmtStartData stm.sid in
    ReachingDef.pretty () ivih) fdec.sbody.bstmts

*)
(* If this class is extended with a visitor on expressions,
   then the current rd data is available at each expression *)
class rdVisitorClass = object (self)
  inherit nopCilVisitor

  (* if a list of instructions is being processed,
     then this is the corresponding list of
     reaching definitions *)
  val mutable rd_dat_lst = []

  (* these are the reaching defs for the current
     instruction if there is one *)
  val mutable cur_rd_dat = None

  method! vstmt stm =
    match getRDs stm with
    | None ->
	if !debug then (Kernel.debug "rdVis: stm %d had no data\n" stm.sid);
	cur_rd_dat <- None;
	DoChildren
    | Some(_,s,iosh) ->
	match stm.skind with
	  Instr il ->
	    if !debug then (Kernel.debug "rdVis: visit il\n");
	    rd_dat_lst <- instrRDs il stm.sid ((),s,iosh) false;
	    DoChildren
	| _ ->
	    if !debug then (Kernel.debug "rdVis: visit non-il\n");
	    cur_rd_dat <- None;
	    DoChildren

  method! vinst i =
    if !debug then Kernel.debug "rdVis: before %a, rd_dat_lst is %d long\n"
      Cil_printer.pp_instr i (List.length rd_dat_lst);
    try
      cur_rd_dat <- Some(List.hd rd_dat_lst);
      rd_dat_lst <- List.tl rd_dat_lst;
      DoChildren
    with Failure "hd" ->
      if !debug then (Kernel.debug "rdVis: il rd_dat_lst mismatch\n");
      DoChildren

  method get_cur_iosh () =
    match cur_rd_dat with
      None -> (match getRDs (Extlib.the self#current_stmt) with
	None -> None
      | Some(_,_,iosh) -> Some iosh)
    | Some(_,_,iosh) -> Some iosh

end
