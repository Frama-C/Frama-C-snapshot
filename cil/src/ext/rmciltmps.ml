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

(* These are functions etc. for removing CIL generated
   temporary variables. Some can be removed immediately,
   others must wait until pretty printing *)

open Cil_types
open Cil

module RD = Reachingdefs
module AELV = Availexpslv
module UD = Usedef
module IH = Datatype.Int.Hashtbl
module IS = Datatype.Int.Set

let debug = RD.debug


(* Type for the form of temporary variable names *)
type nameform = Suffix of string | Prefix of string | Exact of string

(* take the id number of a definition and return
   the rhs of the definition if there is one.
   Returns None if, for example, the definition is
   caused by an assembly instruction *)
(* int -> (rhs * int * IOS.t IH.t) option *)
let getDefRhs = RD.getDefRhs RD.ReachingDef.defIdStmtHash

(* exp_is_ok_replacement -
   Returns false if the argument contains a pointer dereference
   or a variable whose address is taken anywhere *)

let exp_ok = ref true
class memReadOrAddrOfFinderClass = object
  inherit nopCilVisitor

  method! vexpr e = match e.enode with
    Lval(Mem _, _) ->
      exp_ok := false;
      SkipChildren
  | _ -> DoChildren

  method! vvrbl vi =
    if vi.vglob then
      (if !debug then (Kernel.debug "memReadOrAddrOfFinder: %s is a global\n"
                               vi.vname);
       exp_ok := false;
       SkipChildren)
    else if vi.vaddrof then
      (if !debug then
         (Kernel.debug "memReadOrAddrOfFinder: %s has its address taken\n"
                  vi.vname);
       exp_ok := false;
       SkipChildren)
    else (if !debug then (Kernel.debug "memReadOrAddrOfFinder: %s does not have its address taken\n"
                                  vi.vname);
          DoChildren)

end

let memReadOrAddrOfFinder = new memReadOrAddrOfFinderClass

(* exp -> bool *)
let exp_is_ok_replacement e =
  if !debug then 
    Kernel.debug "exp_is_ok_replacement: in exp_is_ok_replacement with %a\n" 
      Cil_printer.pp_exp e;
  exp_ok := true;
  ignore(visitCilExpr memReadOrAddrOfFinder e);
  !exp_ok

let emptyStmt = mkEmptyStmt ()
let fsr = ref emptyStmt
class stmtFinderClass sid = object
  inherit nopCilVisitor

  method! vstmt stm =
    if stm.sid = sid
    then (fsr := stm; SkipChildren)
    else DoChildren

end

let find_statement _f sid = RD.getStmt sid

(* Are there writes to memory in between
   the two statements with the given ids *)
(* fundec -> int -> int -> bool *)
let wbHtbl = Hashtbl.create 256
let writes_between f dsid sid =
  if Hashtbl.mem wbHtbl (dsid,sid) then Hashtbl.find wbHtbl (dsid,sid) else
  let dstmo = find_statement f dsid in
  let stmo = find_statement f sid in
  let find_write s = match s.skind with
    Instr il -> List.exists (fun i ->
      match i with
        Set((Mem _,_),_,_) -> true (* pointer write *)
      | Set((_,Index (_,_)),_,_) -> true (* array write *)
      | Call(_,_,_,_) -> true
      | _ -> false) [il]
  | _ -> false
  in
  (* is there a path from start to goal that includes an
     instruction that writes to memory? Do a dfs *)
  let visited_sid_isr = ref IS.empty in
  let rec dfs goal b start =
    if !debug then 
      Kernel.debug "writes_between: dfs visiting %a\n" Cil_printer.pp_stmt start;
    if start.sid = goal.sid then
      let wh = find_write start in
      (if !debug && b then 
	  Kernel.debug "writes_between: start=goal and found a write\n";
       if !debug && (not b) then 
	 Kernel.debug "writes_between: start=goal and no write\n";
       if !debug && wh then 
	 Kernel.debug "writes_between: start=goal and write here\n";
       if !debug && (not wh) then 
	 Kernel.debug "writes_between: start=goal and no write here\n";
       b || (find_write start))
    else
    if IS.mem start.sid (!visited_sid_isr) then false else
    let w = find_write start in
    if !debug && w then 
      Kernel.debug "writes_between: found write %a" Cil_printer.pp_stmt start;
    visited_sid_isr := IS.add start.sid (!visited_sid_isr);
    let rec proc_succs sl = match sl with [] -> false
    | s::rest -> if dfs goal (w || b) s then true else proc_succs rest
    in
    proc_succs start.succs
  in
  match stmo, dstmo with
    None, _ | _, None -> Kernel.fatal "writes_between: defining stmt not an instr"
  | Some stm, Some dstm ->
      let _ = visited_sid_isr := IS.singleton stm.sid in
      let from_stm = List.fold_left (dfs stm) false stm.succs in
      let _ = visited_sid_isr := IS.empty in
      let from_dstm = dfs stm false dstm in
      (Hashtbl.add wbHtbl (dsid,sid) (from_stm || from_dstm);
      from_stm || from_dstm)

(* returns true when the variables in uses
 * have the same definition ids in both curiosh
 * and defiosh or are global and not defined in
 * the current function *)
let verify_unmodified uses fdefs curiosh defiosh =
  UD.VS.fold (fun vi b ->
    let curido = RD.iosh_singleton_lookup curiosh vi in
    let defido = RD.iosh_singleton_lookup defiosh vi in
    match curido, defido with
      Some(curid), Some(defid) ->
        (if !debug then (Kernel.debug "verify_unmodified: curido: %d defido: %d" curid defid);
         curid = defid && b)
    | None, None ->
        if not(UD.VS.mem vi fdefs) then
          (if !debug then (Kernel.debug "verify_unmodified: %s not defined in function" vi.vname);
           b)
        else (* if the same set of definitions reaches, we can replace, also *)
          let curios = try IH.find curiosh vi.vid
          with Not_found -> RD.IOS.empty in
          let defios = try IH.find defiosh vi.vid
          with Not_found -> RD.IOS.empty in
          RD.IOS.compare curios defios == 0 && b
    | _, _ ->
        (if !debug then Kernel.debug "verify_unmodified: %s has conflicting definitions. cur: %a\n def: %a"
           vi.vname RD.ReachingDef.pretty ((),0,curiosh)
           RD.ReachingDef.pretty ((),0,defiosh);
         false))
    uses true

let fdefs = ref UD.VS.empty
let udDeepSkindHtbl = IH.create 64
class defCollectorClass = object
  inherit nopCilVisitor

  method! vstmt s =
    let _,d = if IH.mem udDeepSkindHtbl s.sid
    then IH.find udDeepSkindHtbl s.sid
    else let u',d' = UD.computeDeepUseDefStmtKind s.skind in
    IH.add udDeepSkindHtbl s.sid (u',d');
    (u',d') in
    fdefs := UD.VS.union !fdefs d;
    DoChildren

end

let defCollector = new defCollectorClass

let collect_fun_defs fd =
  fdefs := UD.VS.empty;
  ignore(visitCilFunction defCollector fd);
  !fdefs

(* ok_to_replace *)
(* is it alright to replace a variable use with the expression
   that the variable was defined to be? *)
(* Takes the definitions that reached the place where the
   variable was defined and the definitions that reach the
   place the variable is used. If the same definitions for
   the variables used in the expression reach both places,
   then it is okay to replace the variable with the expression. *)
(* With regards to globals and parameters there are two
   possibilities if the reverse lookup returns None for both
   sets of reaching definitions:
   1) The global or parameter is actually not redefined.
   2) At both points no one definition *must* reach there.
   For this reason, this function also takes the fundec,
   so that it can be figured out which is the case *)
(* varinfo -> varinfo IH.t -> sid -> varinfo IH.t -> fundec -> rhs -> bool *)
(* sid is an int that is the statement id of the statement where
   we are trying to do a replacement *)
(* vi is the varinfo of the variable that we are trying to replace *)
let ok_to_replace vi curiosh sid defiosh dsid f r =
  let uses, safe = match r with
    RD.RDExp e -> (UD.computeUseExp e, exp_is_ok_replacement e)
  | RD.RDCall (Call(_,_,el,_) as i) ->
      let safe = List.fold_left (fun b e ->
        (exp_is_ok_replacement e) && b) true el in
      let u,_d = UD.computeUseDefInstr i in
      u, safe
  | _ -> Kernel.fatal "ok_to_replace: got non Call in RDCall."
  in
  let target_addrof = if vi.vaddrof || vi.vglob then
    (if !debug then (Kernel.debug "ok_to_replace: target %s had its address taken or is a global" vi.vname);
    true)
  else (if !debug then (Kernel.debug "ok_to_replace: target %s does not have its address taken" vi.vname);
        false) in
  let writes = if safe && not(target_addrof) then false else (writes_between f dsid sid) in
  if (not safe || target_addrof) && writes
  then
    (if !debug then (Kernel.debug "ok_to_replace: replacement not safe because of pointers or addrOf");
     false)
  else let fdefs = collect_fun_defs f in
  let _ = if !debug then (Kernel.debug "ok_to_replace: card fdefs = %d" (UD.VS.cardinal fdefs)) in
  let _ = if !debug then (Kernel.debug "ok_to_replace: card uses = %d" (UD.VS.cardinal uses)) in
  verify_unmodified uses fdefs curiosh defiosh

let useList = ref []
(* Visitor for making a list of statements that use a definition *)
class useListerClass (defid:int) (vi:varinfo) = object(self)
    inherit RD.rdVisitorClass

  method! vexpr e =
    match e.enode with
      Lval(Var vi', _off) ->
        (match self#get_cur_iosh() with
          Some iosh ->
            let vido = RD.iosh_defId_find iosh defid in
            let exists = match vido with Some _ -> true | None -> false in
            if vi.vid = vi'.vid && exists
            then (useList :=
                    (Extlib.the self#current_stmt)::(!useList); DoChildren)
            else DoChildren
        | _ -> Kernel.fatal "useLister: no data for statement")
    | _ -> DoChildren

end

(* ok_to_replace_with_incdec *)
(* Find out if it is alright to replace the use of a variable
   with a post-incrememnt/decrement of the variable it is assigned to be *)
(* Takes the definitions reaching the variable use, the definitions
   reaching the place where the variable was defined, the fundec,
   the varinfo for the variable being considered and the right
   hand side of its definition. *)
let ok_to_replace_with_incdec curiosh defiosh f id vi r =

  (* number of uses of vi where definition id reaches *)
  let num_uses () =
    let _ = useList := [] in
    let ulc = new useListerClass id vi in
    let _ = visitCilFunction (ulc :> cilVisitor) f in
    List.length (!useList)
  in

  (* Is e the addition or subtraction of one to vi?
     Return Some(PlusA) if it's an addition,
     Some(MinusA) if it's a subtraction,
     and None otherwise *)
  let inc_or_dec e vi =
    match e.enode with
      BinOp((PlusA|PlusPI|IndexPI),
            {enode = Lval(Var vi', NoOffset)},
            {enode = Const(CInt64(one,_,_))},_) ->
              if vi.vid = vi'.vid && Integer.equal one Integer.one
              then Some(PlusA)
              else if vi.vid = vi'.vid && 
                     Integer.equal one Integer.minus_one
              then Some(MinusA)
              else None
    | BinOp((MinusA|MinusPI),
            {enode = Lval(Var vi', NoOffset)},
            {enode = Const(CInt64(one,_,_))},_) ->
              if vi.vid = vi'.vid && Integer.equal one Integer.one
              then Some(MinusA)
              else None
    | _ -> None
  in

  match r with
    RD.RDExp({enode = Lval(Var rhsvi, NoOffset)}) ->
      let curido = RD.iosh_singleton_lookup curiosh rhsvi in
      let defido = RD.iosh_singleton_lookup defiosh rhsvi in
      (match  curido, defido with
        Some(curid), _ ->
          let defios = try IH.find defiosh rhsvi.vid
          with Not_found -> RD.IOS.empty in
          let redefrhso = getDefRhs curid in
          (match redefrhso with
            None -> (if !debug then (Kernel.debug "ok_to_replace: couldn't get rhs for redef: %d" curid);
                     None)
          | Some(redefrhs, _, redefiosh) ->
              let tmprdido = RD.iosh_singleton_lookup redefiosh vi in
              match tmprdido with
                None -> (if !debug then (Kernel.debug "ok_to_replace: conflicting defs of %s reach redef of %s" vi.vname rhsvi.vname);
                         None)
              | Some tmprdid ->
                  if not (tmprdid = id) then
                    (if !debug then (Kernel.debug "ok_to_replace: initial def of %s doesn't reach redef of %s" vi.vname rhsvi.vname);
                     None)
                  else let redefios = try IH.find redefiosh rhsvi.vid
                  with Not_found -> RD.IOS.empty in
                  let curdef_stmt =
		    try IH.find RD.ReachingDef.defIdStmtHash curid
                    with Not_found ->
		      Kernel.fatal "ok_to_replace: couldn't find statement defining %d" curid in
                  if not (RD.IOS.compare defios redefios = 0) then
                    (if !debug then
		       (Kernel.debug
			  "ok_to_replace: different sets of definitions of %s reach the def of %s and the redef of %s"
                          rhsvi.vname
                          vi.vname
                          rhsvi.vname);
                     None)
                  else
                    (match redefrhs with
                      RD.RDExp(e) -> (match inc_or_dec e rhsvi with
                        Some(PlusA) ->
                          if num_uses () = 1 then
                            Some(curdef_stmt.sid, curid, rhsvi, PlusA)
                          else (if !debug then (Kernel.debug "ok_to_replace: tmp used more than once");
                                None)
                      | Some(MinusA) ->
                          if num_uses () = 1 then
                            Some(curdef_stmt.sid, curid, rhsvi, MinusA)
                          else (if !debug then (Kernel.debug "ok_to_replace: tmp used more than once");
                                None)
                      | None ->
                          (if !debug then (Kernel.debug "ok_to_replace: redef isn't adding or subtracting one from itself");
                           None)
                      | _ -> (Kernel.fatal "ok_to_replace: unexpected op in inc/dec info."))
                    | _ -> (if !debug then (Kernel.debug "ok_to_replace: redef a call");
                            None)))
      | _ -> (if !debug then (Kernel.debug "ok_to_replace: %s has conflicting definitions" rhsvi.vname);
              None))
  | _ -> (if !debug then (Kernel.debug "ok_to_replace: rhs not of correct form");
          None)

(* A hash from variable ids to Call instruction
   options. If a variable id is in this table,
   and it is mapped to Some(Call()), then the
   function call can be printed instead of the
   variable *)
let iioh = IH.create 16

(* A hash from variable ids to information that
   can be used to print a post increment/decrement
   that can replace the variable *)
let incdecHash = IH.create 16

(* A hash from variable ids to a list of statement ids.
   Because a post-inc/dec will be printed elsewhere,
   the assignments of the variable in these statements
   don't need to be printed *)
let idDefHash = IH.create 16

(* Add a pair to the list for vid and create a list if one
   doesn't exist *)
let id_dh_add vid p =
  if IH.mem idDefHash vid then
    let oldlist = IH.find idDefHash vid in
    let newlist = p::oldlist in
    IH.replace idDefHash vid newlist
  else
    IH.add idDefHash vid [p]

(* check if a name matches a form *)
(* string -> nameform -> bool *)
let check_form s f =
    match f with
      Suffix sfx ->
        let frmlen = String.length sfx in
        let slen = String.length s in
        slen >= frmlen &&
        String.compare (String.sub s (slen - frmlen) frmlen) sfx = 0
    | Prefix pfx ->
        let frmlen = String.length pfx in
        String.length s >= frmlen &&
        String.compare (String.sub s 0 frmlen) pfx = 0
    | Exact ext ->
        let frmlen = String.length ext in
        String.length s = frmlen &&
        String.compare s ext = 0

(* check a name against a list of forms
   if it matches any then return true *)
(* string -> nameform list -> bool *)
let check_forms s fl =
  List.fold_left (fun b f -> b || check_form s f)
    false fl

let forms = [Exact "tmp";
             Prefix "tmp___";
             Prefix "__cil_tmp";
             Suffix "__e";
             Suffix "__b";]

(* action: 'a -> varinfo -> fundec -> bool -> exp option
 * iosh: 'a
 * fd: fundec
 * nofrm: bool
 *
 * Replace Lval(Var vi, NoOffset) with
 * e where action iosh sid vi fd nofrm returns Some(e) *)
let varXformClass action data sid fd nofrm = object
    inherit nopCilVisitor

  method! vexpr e = match e.enode with
    Lval(Var vi, NoOffset) ->
      (match action data sid vi fd nofrm with
        None -> DoChildren
      | Some e' ->
          (* Cast e' to the correct type. *)
          let e'' = mkCast e' vi.vtype in
          ChangeTo e'')
  | Lval(Mem e', off) ->
      (* don't substitute constants in memory lvals *)
      let post e = match e.enode with
        Lval(Mem({enode = Const _}),off') ->
          { e with enode = Lval(Mem e', off')}
      | _ -> e
      in
      ChangeDoChildrenPost(new_exp ~loc:e.eloc (Lval(Mem e', off)), post)
  | _ -> DoChildren

end

(* action: 'a -> lval -> fundec -> bool -> exp option
 * lvh: 'a
 * fd: fundec
 * nofrm: bool
 *
 * Replace Lval(lv) with
 * e where action lvh sid lv fd nofrm returns Some(e) *)
let lvalXformClass action data sid fd nofrm = object
  inherit nopCilVisitor

  method! vexpr e =
    let castrm e =
      Expcompare.stripCastsDeepForPtrArith e
    in
    match e.enode with
    | Lval((Mem e', off) as lv)-> begin
        match action data sid lv fd nofrm with
        | None ->
            (* don't substitute constants in memory lvals *)
            let post e =
              match e.enode with
              | Lval(Mem({enode = Const _}),off') ->
                  new_exp ~loc:e.eloc (Lval(Mem e', off'))
              | _ -> Expcompare.stripCastsDeepForPtrArith e
            in
            ChangeDoChildrenPost(new_exp ~loc:e.eloc (Lval(Mem e', off)), post)
        | Some e ->
            let newt = typeOf(new_exp ~loc:e.eloc (Lval lv)) in
            let e'' = mkCast e newt in
            ChangeDoChildrenPost(e'', castrm)
    end
    | Lval lv -> begin
        match action data sid lv fd nofrm with
        | None -> DoChildren
        | Some e' -> begin
            (* Cast e' to the correct type. *)
            let e'' = mkCast e' (typeOf(dummy_exp(Lval lv))) in
            ChangeDoChildrenPost(e'', castrm)
        end
    end
    | _ -> ChangeDoChildrenPost(castrm e, castrm)

end

(* Returns the set of definitions of vi in iosh that
   are not due to assignments of the form x = x *)
(* IOS.t IH.t -> varinfo -> int option *)
let iosh_get_useful_def iosh vi =
  if IH.mem iosh vi.vid then
    let ios = IH.find iosh vi.vid in
    let ios' = RD.IOS.filter (fun ido  ->
      match ido with None -> true | Some(id) ->
        match getDefRhs id with
            Some(RD.RDExp({enode = Lval(Var vi',NoOffset)}),_,_)
        | Some(RD.RDExp
                 ({enode = CastE(_,{enode = Lval(Var vi',NoOffset)})}),_,_) ->
            not(vi.vid = vi'.vid) (* false if they are the same *)
        | _ -> true) ios
    in
    if not(RD.IOS.cardinal ios' = 1)
    then (if !debug then (Kernel.debug "iosh_get_useful_def: multiple different defs of %d:%s(%d)"
                                  vi.vid vi.vname (RD.IOS.cardinal ios'));
          None)
    else RD.IOS.choose ios'
  else (if !debug then (Kernel.debug "iosh_get_useful_def: no def of %s reaches here" vi.vname);
        None)

let ae_tmp_to_exp_change = ref false
let ae_tmp_to_exp eh _sid vi _fd nofrm =
  if nofrm || (check_forms vi.vname forms)
  then try begin
    let e = IH.find eh vi.vid in
    if !debug then 
      Kernel.debug "tmp_to_exp: changing %s to %a" vi.vname Cil_printer.pp_exp e;
    match e.enode with
    | Const(CStr _)
    | Const(CWStr _) -> None (* don't fwd subst str lits *)
    | _ -> begin
        ae_tmp_to_exp_change := true;
        Some e
    end
  end
  with Not_found -> None
  else None

let ae_lval_to_exp_change = ref false
let ae_lval_to_exp lvh _sid lv _fd nofrm =
  match lv, nofrm with
  | (Var vi, NoOffset), false ->
      (* If the var is not a temp, then don't replace *)
      if check_forms vi.vname forms then begin
        try
          let e = AELV.LvExpHash.find lvh lv in
          match e.enode with
          | Const(CStr _)
          | Const(CWStr _) -> None
          | _ -> begin
              ae_lval_to_exp_change := true;
              if !debug then 
		Kernel.debug "ae: replacing %a with %a"
                  Cil_printer.pp_lval lv Cil_printer.pp_exp e;
              Some e
          end
        with Not_found -> None
      end else None
  | _, true -> begin
     (* replace everything *)
      try
        let e = AELV.LvExpHash.find lvh lv in
        match e.enode with
        | Const(CStr _)
        | Const(CWStr _) -> None
        | _ -> begin
            ae_lval_to_exp_change := true;
            Kernel.debug "ae: replacing %a with %a"
              Cil_printer.pp_lval lv Cil_printer.pp_exp e;
            Some e
        end
      with Not_found -> None
  end
  | _, _ -> None


(* if the temp with varinfo vi can be
   replaced by an expression then return
   Some of that expression. o/w None.
   If b is true, then don't check the form *)
(* IOS.t IH.t -> sid -> varinfo -> fundec -> bool -> exp option *)
let rd_tmp_to_exp_change = ref false
let rd_tmp_to_exp iosh sid vi fd nofrm =
  if nofrm || (check_forms vi.vname forms)
  then let ido = iosh_get_useful_def iosh vi in
  match ido with None ->
    if !debug then (Kernel.debug "tmp_to_exp: non-single def: %s"
                            vi.vname);
    None
  | Some(id) -> let defrhs = getDefRhs id in
    match defrhs with None ->
      if !debug then
        (Kernel.debug "tmp_to_exp: no def of %s" vi.vname);
      None
    | Some(RD.RDExp(e) as r, dsid , defiosh) ->
        if ok_to_replace vi iosh sid defiosh dsid fd r
        then
          (if !debug then
	      Kernel.debug "tmp_to_exp: changing %s to %a"
		vi.vname Cil_printer.pp_exp e;
           match e.enode with
           | Const(CStr _)
           | Const(CWStr _) -> None
           | _ -> begin
               rd_tmp_to_exp_change := true;
               Some e
           end)
        else
          (if !debug then (Kernel.debug "tmp_to_exp: not ok to replace %s" vi.vname);
           None)
    | _ ->
        if !debug then (Kernel.debug "tmp_to_exp: rhs is call %s" vi.vname);
        None
  else
    (if !debug then (Kernel.debug "tmp_to_exp: %s didn't match form or nofrm" vi.vname);
     None)

let rd_fwd_subst data sid e fd nofrm =
  rd_tmp_to_exp_change := false;
  let e' = visitCilExpr (varXformClass rd_tmp_to_exp data sid fd nofrm) e in
  (e', !rd_tmp_to_exp_change)

let ae_fwd_subst data sid e fd nofrm =
  ae_tmp_to_exp_change := false;
  let e' = visitCilExpr (varXformClass ae_tmp_to_exp data sid fd nofrm) e in
  (e', !ae_tmp_to_exp_change)

let ae_lv_fwd_subst data sid e fd nofrm =
  ae_lval_to_exp_change := false;
  let e' = visitCilExpr (lvalXformClass ae_lval_to_exp data sid fd nofrm) e in
  (e', !ae_lval_to_exp_change)

let ae_simp_fwd_subst data e nofrm =
  ae_lv_fwd_subst data (-1) e (emptyFunction "@dummy@") nofrm

let ae_tmp_to_const_change = ref false
let ae_tmp_to_const eh _sid vi _fd nofrm =
  if nofrm || check_forms vi.vname forms then
    try begin let e = IH.find eh vi.vid in
    match e.enode with Const c -> begin
      ae_tmp_to_const_change := true;
      Some(new_exp ~loc:e.eloc (Const c)) end
    | _ -> None end
    with Not_found -> None
  else None

(* See if vi can be replaced by a constant
   by checking all of the definitions reaching
   this use of vi *)
let tmp_to_const_change = ref false
let tmp_to_const iosh sid vi fd nofrm =
  if nofrm || check_forms vi.vname forms then
    match RD.iosh_lookup iosh vi with
      None -> None
    | Some(ios) ->
        let defido =
          try RD.IOS.choose ios
          with Not_found -> None in
        match defido with None -> None | Some defid ->
          match getDefRhs defid with
            None -> None
          | Some(RD.RDExp({enode = Const c;eloc=loc}), _, defiosh) ->
              (match RD.getDefIdStmt defid with
                None -> (Kernel.fatal "tmp_to_const: defid has no statement")
              | Some(stm) ->
                  if ok_to_replace vi iosh sid defiosh stm.sid fd
                    (RD.RDExp(dummy_exp (Const c)))
                  then
                  let same = RD.IOS.for_all (fun defido ->
                    match defido with None -> false | Some defid ->
                      match getDefRhs defid with
                        None -> false
                      | Some(RD.RDExp({enode = Const c'}),_,defiosh) ->
                          if Cil_datatype.Constant.equal c c' then
                            match RD.getDefIdStmt defid with
                              None -> (Kernel.fatal "tmp_to_const: defid has no statement")
                            | Some(stm) ->
                                ok_to_replace vi iosh sid defiosh stm.sid fd
                                  (RD.RDExp(dummy_exp (Const c')))
                          else false
                      | _ -> false) ios
                  in
                  if same
                  then (tmp_to_const_change := true; 
                        Some(new_exp ~loc (Const c)))
                  else None
              else None)
          | _ -> None
  else None

let const_prop iosh sid e fd nofrm =
  tmp_to_const_change := false;
  let e' = visitCilExpr (varXformClass tmp_to_const iosh sid fd nofrm) e in
  (e', !tmp_to_const_change)

let ae_const_prop eh sid e fd nofrm =
  ae_tmp_to_const_change := false;
  let e' = visitCilExpr (varXformClass ae_tmp_to_const eh sid fd nofrm) e in
  (e', !ae_tmp_to_const_change)

class expTempElimClass (fd:fundec) = object (self)
  inherit RD.rdVisitorClass

  method! vexpr e =

    let do_change iosh vi =
      let ido = RD.iosh_singleton_lookup iosh vi in
      (match ido with
        Some id ->
          let riviho = getDefRhs id in
          (match riviho with
            Some(RD.RDExp(e) as r, dsid, defiosh) ->
              if !debug then 
		Kernel.debug "Can I replace %s with %a?"
		  vi.vname Cil_printer.pp_exp e;
              if ok_to_replace
                vi iosh (Extlib.the self#current_stmt).sid defiosh dsid fd r
              then
                (if !debug then (Kernel.debug "Yes.");
                 ChangeTo(e))
              else (if !debug then (Kernel.debug "No.");
                    DoChildren)
          | _ -> DoChildren)
      | _ -> DoChildren)
    in

    match e.enode with
      Lval (Var vi,NoOffset) ->
        (if check_forms vi.vname forms then
         (* only allowed to replace a tmp with a function call once *)
          (match cur_rd_dat with
            Some(_,_s,iosh) -> do_change iosh vi
          | None -> let iviho = RD.getRDs (Extlib.the self#current_stmt) in
            match iviho with
              Some(_,_s,iosh) ->
                (if !debug then
                   (Kernel.debug "Try to change %s outside of instruction." vi.vname);
                 do_change iosh vi)
            | None ->
                (if !debug then
                   (Kernel.debug "%s in statement w/o RD info" vi.vname);
                 DoChildren))
        else DoChildren)
    | _ -> DoChildren

end

class expLvTmpElimClass (fd : fundec) = object(self)
  inherit AELV.aeVisitorClass

  method! vexpr e =
    match self#get_cur_eh () with
    | None -> DoChildren
    | Some eh -> begin
       let e', _ =
         ae_lv_fwd_subst eh (Extlib.the self#current_stmt).sid e fd false in
       ChangeTo e'
    end

end

class incdecTempElimClass (fd:fundec) = object (self)
  inherit RD.rdVisitorClass

  method! vexpr e =

    let do_change iosh vi =
      let ido = RD.iosh_singleton_lookup iosh vi in
      (match ido with
        Some id ->
          let riviho = getDefRhs id in
          (match riviho with
            Some(RD.RDExp _e as r, _, defiosh) ->
              (match ok_to_replace_with_incdec iosh defiosh fd id vi r with
                Some(curdef_stmt_id,redefid, rhsvi, b) ->
                  (if !debug then (Kernel.debug "No, but I can replace it with a post-inc/dec");
                   if !debug then (Kernel.debug "cdsi: %d redefid: %d name: %s"
                                           curdef_stmt_id redefid
                                           rhsvi.vname);
                   IH.add incdecHash vi.vid (redefid, rhsvi, b);
                   id_dh_add rhsvi.vid (curdef_stmt_id, redefid);
                   DoChildren)
              | None ->
                  (if !debug then (Kernel.debug "No.");
                   DoChildren))
          | _ -> DoChildren)
      | _ -> DoChildren)
    in

    match e.enode with
      Lval (Var vi,NoOffset) ->
        (if check_forms vi.vname forms then
         (* only allowed to replace a tmp with an inc/dec if there is only one use *)
          (match cur_rd_dat with
            Some(_,_s,iosh) -> do_change iosh vi
          | None -> let iviho = RD.getRDs (Extlib.the self#current_stmt) in
            match iviho with
              Some(_,_s,iosh) ->
                (if !debug then (Kernel.debug "Try to change %s outside of instruction." vi.vname);
                 do_change iosh vi)
            | None ->
                (if !debug then (Kernel.debug "%s in statement w/o RD info" vi.vname);
                 DoChildren))
        else DoChildren)
    | _ -> DoChildren

end

class callTempElimClass (fd:fundec) = object (self)
  inherit RD.rdVisitorClass

  method! vexpr e =

    let do_change iosh vi =
      let ido = RD.iosh_singleton_lookup iosh vi in
      (match ido with
        Some id ->
          let riviho = getDefRhs id in
          (match riviho with
               Some(RD.RDCall(i) as r, dsid, defiosh) ->
		 if !debug then 
		   Kernel.debug "Can I replace %s with %a?" 
		     vi.vname Cil_printer.pp_instr i;
		 if ok_to_replace
                   vi iosh (Extlib.the self#current_stmt).sid defiosh dsid fd r
		 then (if !debug then (Kernel.debug "Yes.");
                       IH.add iioh vi.vid (Some(i));
                       DoChildren)
		 else (if !debug then (Kernel.debug "No.");
                       DoChildren)
             | _ -> DoChildren)
	 | _ -> DoChildren)
    in

    match e.enode with
      Lval (Var vi,NoOffset) ->
        (if check_forms vi.vname forms then
         (* only allowed to replace a tmp with a function call if there is only one use *)
          if IH.mem iioh vi.vid
          then (IH.replace iioh vi.vid None; DoChildren)
          else
            (match cur_rd_dat with
              Some(_,_s,iosh) -> do_change iosh vi
            | None -> let iviho = RD.getRDs (Extlib.the self#current_stmt) in
              match iviho with
                Some(_,_s,iosh) ->
                  (if !debug then (Kernel.debug "Try to change %s:%d outside of instruction." vi.vname vi.vid);
                   do_change iosh vi)
              | None ->
                  (if !debug then (Kernel.debug "%s in statement w/o RD info" vi.vname);
                   DoChildren))
          else DoChildren)
    | _ -> DoChildren

    (* Unused definitions cause multiple replacements
       unless they are found and the replacement prevented.
       It will be possible to replace more temps if dead
       code elimination is performed before printing. *)
  method! vinst i =
    (* Need to copy this from rdVisitorClass because we are overriding *)
    if !debug then 
      Kernel.debug "rdVis: before %a, rd_dat_lst is %d long"
	Cil_printer.pp_instr i (List.length rd_dat_lst);
    (try
      cur_rd_dat <- Some(List.hd rd_dat_lst);
      rd_dat_lst <- List.tl rd_dat_lst
    with Failure "hd" ->
      if !debug then (Kernel.debug "rdVis: il rd_dat_lst mismatch"));
    match i with
      Set((Var vi,_off),_,_) ->
        if IH.mem iioh vi.vid
        then (IH.replace iioh vi.vid None; DoChildren)
        else (IH.add iioh vi.vid None; DoChildren)
    | _ -> DoChildren

end



(* Remove local declarations that aren't set or used *)
(* fundec -> unit *)
let rm_unused_locals fd =
  let oldIgnoreSizeof = !UD.ignoreSizeof in
  UD.ignoreSizeof := false;
  let used = List.fold_left (fun u s ->
    let u', d' = UD.computeDeepUseDefStmtKind s.skind in
    UD.VS.union u (UD.VS.union u' d')) UD.VS.empty fd.sbody.bstmts in
  UD.ignoreSizeof := oldIgnoreSizeof;

  let good_var vi = UD.VS.mem vi used in
  let good_locals = List.filter good_var fd.slocals in
  let remove_block_locals = object
    inherit Cil.nopCilVisitor
    method! vblock b =
      b.blocals <- List.filter good_var b.blocals;
      DoChildren
  end
  in
  fd.slocals <- good_locals;
  ignore (visitCilBlock remove_block_locals fd.sbody)



(* see if a vi is volatile *)
let is_volatile vi =
  let vi_vol =
    List.exists (function (Attr("volatile",_)) -> true
      | _ -> false) vi.vattr in
  let typ_vol =
    List.exists (function (Attr("volatile",_)) -> true
      | _ -> false) (typeAttrs vi.vtype) in
  if !debug && (vi_vol || typ_vol) then
    (Kernel.debug "unusedRemover: %s is volatile" vi.vname);
  if !debug && not(vi_vol || typ_vol) then
    (Kernel.debug "unusedRemover: %s is not volatile" vi.vname);
  vi_vol || typ_vol


(* Remove temp variables that are set but not used *)
(* This is different from dead code elimination because
   temps that can be eliminated during pretty printing
   are also considered *)
class unusedRemoverClass : cilVisitor = object(self)
    inherit nopCilVisitor

  val mutable unused_set = UD.VS.empty
  val mutable cur_func = emptyFunction "@dummy@"

    (* a filter function for picking out
       the local variables that need to be kept *)
  method private good_var vi =
      (is_volatile vi) ||
      (not(UD.VS.mem vi unused_set) &&
      (not(IH.mem iioh vi.vid) ||
      (match IH.find iioh vi.vid with
        None -> true | Some _ -> false)) &&
      not(IH.mem incdecHash vi.vid))

  (* figure out which locals aren't used *)
  method! vfunc f =
    cur_func <- f;
    (* the set of used variables *)
    let used = List.fold_left (fun u s ->
      let u', _ = UD.computeDeepUseDefStmtKind s.skind in
      UD.VS.union u u') UD.VS.empty f.sbody.bstmts in
    let used = UD.computeUseLocalTypes ~acc_used:used f in

    (* the set of unused locals *)
    let unused = List.fold_left (fun un vi ->
      if UD.VS.mem vi used
      then un
      else (if !debug then (Kernel.debug "unusedRemoverClass: %s is unused" vi.vname);
            UD.VS.add vi un)) UD.VS.empty f.slocals in
    unused_set <- unused;
    let good_locals = List.filter self#good_var f.slocals in
    f.slocals <- good_locals;
    DoChildren

  (* remove instructions that set variables
     that aren't used. Also remove instructions
     that set variables mentioned in iioh *)
  method! vstmt stm =

    (* return the list of pairs with fst = f *)
    let findf_in_pl f (pl : (int * int) list)  =
      List.filter (fun (fst,_snd) ->
        if fst = f then true else false)
        pl
    in

    (* Return true if the assignment of this
       variable in this statement is going to be
       replaced by a post-inc/dec *)
    let check_incdec vi e =
      if IH.mem idDefHash vi.vid then
        let pl = IH.find idDefHash vi.vid in
        match findf_in_pl stm.sid pl with (_sid,redefid)::_l ->
          let rhso = getDefRhs redefid in
          (match rhso with
            None -> (if !debug then (Kernel.debug "check_incdec: couldn't find rhs for def %d" redefid);
                     false)
          | Some(rhs, _, _indiosh) ->
              (match rhs with
                 RD.RDCall _ -> (if !debug then Kernel.debug "check_incdec: rhs not an expression";
                                false)
              | RD.RDExp e' ->
                  if compareExp e e' then true
                  else (if !debug then 
		      Kernel.debug
			"check_incdec: rhs of %d: %a, and needed redef %a \
                          not equal"
                        redefid Cil_printer.pp_exp e' Cil_printer.pp_exp e;
                        false)))
        | [] -> 
	  (if !debug then
	      Kernel.debug "check_incdec: current statement not in list: %d. \
%s = %a"
		stm.sid
		vi.vname
		Cil_printer.pp_exp e;
           false)
      else (if !debug then Kernel.debug "check_incdec: %s not in idDefHash"
              vi.vname;
            false)
    in

    (* return true if the rhs will get
       pretty printed as a function call *)
    let will_be_call e =
      match e.enode with
        Lval(Var vi,NoOffset) ->
          if not(IH.mem iioh vi.vid) then false
          else (match IH.find iioh vi.vid with
            None -> false | Some _ -> true)
      | _ -> false
    in

    (* a filter function for picking out
       the instructions that we want to keep *)
    (* instr -> bool *)
    let good_instr i =
      match i with
        Set((Var(vi),_),e,_) ->
          if will_be_call e &&
            not(List.mem vi cur_func.slocals)
          then cur_func.slocals <- vi::cur_func.slocals;
          is_volatile vi ||
          (not (UD.VS.mem vi unused_set) &&
           not (IH.mem incdecHash vi.vid) &&
           not (check_incdec vi e)) ||
           will_be_call e
         | Call (Some(Var(vi),_),_,_,_) ->
             (* If not in the table or entry is None,
                then it's good *)
             not (IH.mem iioh vi.vid) ||
             (match IH.find iioh vi.vid with
               None -> true | Some _ -> false)
           | Asm(_,_,slvlst,_,_,_,_) ->
               (* make sure the outputs are in the locals list *)
               List.iter (fun (_,_s,lv) ->
                 match lv with (Var vi,_) ->
                   if List.mem vi cur_func.slocals
                   then ()
                   else cur_func.slocals <- vi::cur_func.slocals
                 |_ -> ()) slvlst;
               true
           | _ -> true
    in

    (* If the result of a function call isn't used,
       then change to Call(None,...) *)
    let call_fixer i =
      match i with
        Call (Some(Var(vi),_),e,el,l) as c ->
          if UD.VS.mem vi unused_set then
            Call(None,e,el,l)
          else c
      | _ -> i
    in

    match stm.skind with
      Instr il ->
        (*let newil = List.filter good_instr [il] in
        let newil' = List.map call_fixer newil in*)
        stm.skind <-
          Instr (if good_instr il then call_fixer il
                 else Skip Cil_datatype.Location.unknown);
        SkipChildren
    | _ -> DoChildren

  method! vblock b =
    b.blocals <- List.filter self#good_var b.blocals;
    DoChildren

end

(* from cleaner.ml *)

(* Lifts child blocks into parents if the block has no attributes or labels *)
let rec fold_blocks b =
    b.bstmts <- List.fold_right
        (fun s acc ->
          match s.skind with
            Block ib ->
              fold_blocks ib;
              if (List.length ib.battrs = 0 &&
                  List.length s.labels = 0) then
                ib.bstmts @ acc
              else
                s::acc
          | Instr (Skip _) when s.labels = [] ->
              acc
          | _ -> s::acc)
        b.bstmts
        []

class removeBrackets = object
  inherit nopCilVisitor
  method! vblock b =
    fold_blocks b;
    DoChildren
end

(* clean up the code and
   eliminate some temporaries
   for pretty printing a whole function *)
(* Cil.fundec -> Cil.fundec *)
let eliminate_temps f =
  ignore(visitCilFunction (new removeBrackets) f);
  Cfg.clearCFGinfo f;
  ignore(Cfg.cfgFun f);
  UD.ignoreSizeof := false;
  RD.computeRDs f;
  IH.clear iioh;
  IH.clear incdecHash;
  IH.clear idDefHash;
  let etec = new expLvTmpElimClass f in
  let f' = visitCilFunction (etec :> cilVisitor) f in
  let idtec = new incdecTempElimClass f' in
  let f' = visitCilFunction (idtec :> cilVisitor) f' in
  let ctec = new callTempElimClass f' in
  let f' = visitCilFunction (ctec :> cilVisitor) f' in
  visitCilFunction (new unusedRemoverClass) f'

(* same as above, but doesn't remove the
   obviated instructions and declarations.
   Use this before using zrapp to print
   expressions without temps *)
let eliminateTempsForExpPrinting f =
  Cfg.clearCFGinfo f;
  ignore(Cfg.cfgFun f);
  UD.ignoreSizeof := false;
  RD.computeRDs f;
  IH.clear iioh;
  IH.clear incdecHash;
  IH.clear idDefHash;
  let etec = new expLvTmpElimClass f in
  let f' = visitCilFunction (etec :> cilVisitor) f in
  RD.clearMemos (); (* we changed instructions and invalidated the "cache" *)
  let idtec = new incdecTempElimClass f' in
  let f' = visitCilFunction (idtec :> cilVisitor) f' in
  let ctec = new callTempElimClass f' in
  let f' = visitCilFunction (ctec :> cilVisitor) f' in
  f'
