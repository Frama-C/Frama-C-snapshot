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

open Cil_types
open Cil_datatype
open Cabscond

(* -------------------------------------------------------------------------- *)
(* --- Branching                                                          --- *)
(* -------------------------------------------------------------------------- *)

type branches = { 
  s_info : info ;
  s_leftmost : int ; (* eid of the left-most atom *)
  mutable s_then : block list ; 
  mutable s_else : block list ;
}

let branches : (int,branches) Hashtbl.t = Hashtbl.create 371 (* indexed by info.id *)

type target = Cond | Then | Else

let link_target cbs tgt block =
  match tgt with
    | Cond -> ()
    | Then -> cbs.s_then <- block :: cbs.s_then
    | Else -> cbs.s_else <- block :: cbs.s_else

let rec link_branches cbs cond atom tt tf st sf =
  match cond with
    | And(a,b) -> 
	link_branches cbs a atom Cond tf st sf ;
	link_branches cbs b atom tt tf st sf
    | Or(a,b) ->
	link_branches cbs a atom tt Cond st sf ;
	link_branches cbs b atom tt tf st sf
    | Not a ->
	link_branches cbs a atom tf tt st sf
    | Atom leaf ->
	if leaf.eid = atom.eid then
	  ( link_target cbs tt st ; link_target cbs tf sf )
    | Blob -> ()

let rec leftmost = function
  | And(cond,_) | Or(cond,_) | Not cond -> leftmost cond
  | Atom e -> e.eid
  | Blob -> 0

let link_stmt stmt = 
  match stmt.skind with
    | If(e,b_then,b_else,_) ->
	begin
	  match lookup e with
	    | Some info ->
		let cbs = 
		  try Hashtbl.find branches info.id
		  with Not_found -> 
		    let eid = leftmost info.cond in
		    let cbs = { 
		      s_info=info ; 
		      s_leftmost=eid ;
		      s_then=[] ; 
		      s_else=[] ; 
		    } in
		    Hashtbl.add branches info.id cbs ; cbs
		in 
		link_branches cbs info.cond e Then Else b_then b_else
	  | None -> ()
	end
    | _ -> ()
  
let rec adherence adh = function
  | [] -> adh
  | { bstmts = stmt::_ }::bs -> adherence (Stmt.Set.add stmt adh) bs
  | { bstmts = [] }::bs -> adherence adh bs
    
let filter_block adh = function
  | { bstmts = {skind=Goto _;succs=succs}::_ } -> 
      List.for_all (fun s -> not (Stmt.Set.mem s adh)) succs 
  | _ -> true

type branch_info = {
  mutable b_then : info list ;
  mutable b_else : info list ;
}

let branches_info : branch_info Stmt.Hashtbl.t = Stmt.Hashtbl.create 371

let get_branch_info s = 
  try Stmt.Hashtbl.find branches_info s
  with Not_found ->
    let binfo = { b_then=[] ; b_else=[] } in
    Stmt.Hashtbl.add branches_info s binfo ; binfo

let add_branch_then info block =
  match block.bstmts with [] -> () | s::_ ->
    let binfo = get_branch_info s in
    binfo.b_then <- info :: binfo.b_then
	  
let add_branch_else info block =
  match block.bstmts with [] -> () | s::_ ->
    let binfo = get_branch_info s in
    binfo.b_else <- info :: binfo.b_else

let non_empty cbs = cbs.s_then <> [] && cbs.s_else <> [] 

let filter_internal cbs =
  let adh = adherence (adherence Stmt.Set.empty cbs.s_then) cbs.s_else in
  begin
    cbs.s_then <- List.filter (filter_block adh) cbs.s_then ;
    cbs.s_else <- List.filter (filter_block adh) cbs.s_else ;
    if non_empty cbs then
      begin
	List.iter (add_branch_then cbs.s_info) cbs.s_then ;
	List.iter (add_branch_else cbs.s_info) cbs.s_else ;
      end
  end

class link_branches =
object

  inherit Visitor.frama_c_inplace

  method! vinit _ _ _ = Cil.SkipChildren
  method! vtype _ = Cil.SkipChildren
  method! vattr _ = Cil.SkipChildren
  method! vinst _ = Cil.SkipChildren
  method! vexpr _ = Cil.SkipChildren
  method! vlval _ = Cil.SkipChildren
  method! vlogic_type _ = Cil.SkipChildren
  method! vterm _ = Cil.SkipChildren
  method! vpredicate _ = Cil.SkipChildren
  method! vpredicate_named _ = Cil.SkipChildren
  method! vbehavior _ = Cil.SkipChildren
  method! vspec _ = Cil.SkipChildren
  method! vcode_annot _ = Cil.SkipChildren
  method! vannotation _ = Cil.SkipChildren
  method! vstmt_aux stmt = link_stmt stmt ; Cil.DoChildren

end

let computed = ref false	    
let compute () =
  if !active && not !computed then 
    begin
      Kernel.feedback "Computing Branches" ;
      Visitor.visitFramacFile (new link_branches) (Ast.get()) ;
      Hashtbl.iter (fun _ -> filter_internal) branches ;
      computed := true ;
    end

let branches stmt =
  compute () ;
  match stmt.skind with
    | If(e,_,_,_) ->
	begin
	  match lookup e with
	    | None -> None
	    | Some info ->
		try
		  let cbs = Hashtbl.find branches info.id in
		  if non_empty cbs && cbs.s_leftmost = e.eid 
		  then Some ( cbs.s_then , cbs.s_else )
		  else None
		with Not_found -> None
	end
    | _ -> None

let pp_comment fmt stmt =
  if !active then
    try
      let binfo = Stmt.Hashtbl.find branches_info stmt in
      Format.fprintf fmt "@[<hov 0>/*" ;
      List.iter
	(fun info -> Format.fprintf fmt "[THEN:%d]@," info.id)
	binfo.b_then ;
      List.iter
	(fun info -> Format.fprintf fmt "[ELSE:%d]@," info.id)
	binfo.b_else ;
      Format.fprintf fmt "*/@]@ " ;
    with Not_found -> ()

let () = Printer.cabsbranches_pp_comment := pp_comment
