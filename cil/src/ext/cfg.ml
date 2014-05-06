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
(*  File modified by CEA (Commissariat Ã  l'Ã©nergie atomique et aux          *)
(*                        Ã©nergies alternatives)                            *)
(*               and INRIA (Institut National de Recherche en Informatique  *)
(*                          et Automatique).                                *)
(****************************************************************************)

(* Authors: Aman Bhargava, S. P. Rahul *)
(* sfg: this stuff was stolen from optim.ml - the code to print the cfg as
   a dot graph is mine *)

open Cil
open Cil_types
open Cil_datatype

(* entry points: cfgFun, printCfgChannel, printCfgFilename *)

(* known issues:
 * -sucessors of if somehow end up with two edges each
 *)

(*------------------------------------------------------------*)
(* Notes regarding CFG computation:
   1) Initially only succs and preds are computed. sid's are filled in
      later, in whatever order is suitable (e.g. for forward problems, reverse
      depth-first postorder).
   2) If a stmt (return, break or continue) has no successors, then
      function return must follow.
      No predecessors means it is the start of the function
   3) We use the fact that initially all the succs and preds are assigned []
*)

(* Fill in the CFG info for the stmts in a block
   next = succ of the last stmt in this block
   break = succ of any Break in this block
   cont  = succ of any Continue in this block
   None means the succ is the function return. It does not mean the break/cont
   is invalid. We assume the validity has already been checked.
*)
(* At the end of CFG computation,
   - numNodes = total number of CFG nodes
   - length(nodeList) = numNodes
*)

(*let numNodes = ref 0 (* number of nodes in the CFG *)*)
let nodeList : stmt list ref = ref [] (* All the nodes in a flat list *) (* ab: Added to change dfs from quadratic to linear *)

class caseLabeledStmtFinder slr = object
    inherit nopCilVisitor

    method! vstmt s =
        if List.exists (fun l ->
            match l with | Case(_, _) | Default _ -> true | _ -> false)
            s.labels
        then begin
            slr := s :: (!slr);
            match s.skind with
            | Switch(_,_,_,_) -> SkipChildren
            | _ -> DoChildren
        end else match s.skind with
        | Switch(_,_,_,_) -> SkipChildren
        | _ -> DoChildren
end

let findCaseLabeledStmts (b : block) : stmt list =
    let slr = ref [] in
    let vis = new caseLabeledStmtFinder slr in
    ignore(visitCilBlock vis b);
    !slr


(* entry point *)

(** Compute a control flow graph for fd.  Stmts in fd have preds and succs
  filled in *)
let rec cfgFun (fd : fundec) =
  nodeList := [];
  cfgBlock fd.sbody None None None;
  fd.smaxstmtid <- Some(Cil.Sid.next ());
  fd.sallstmts <- List.rev !nodeList;
  nodeList := []


and cfgStmts (ss: stmt list)
    (next:stmt option) (break:stmt option) (cont:stmt option) =
  match ss with
    [] -> ();
  | [s] -> cfgStmt s next break cont
  | hd::tl ->
      cfgStmt hd (Some (List.hd tl))  break cont;
      cfgStmts tl next break cont

and cfgBlock  (blk: block)
    (next:stmt option) (break:stmt option) (cont:stmt option) =
  cfgStmts blk.bstmts next break cont


(* Fill in the CFG info for a stmt
   Meaning of next, break, cont should be clear from earlier comment
*)
and cfgStmt (s: stmt) (next:stmt option) (break:stmt option) (cont:stmt option) =
  if s.sid = -1 then s.sid <- Cil.Sid.next ();
  nodeList := s :: !nodeList; (* Future traversals can be made in linear time. e.g.  *)
  if s.succs <> [] then
    Kernel.fatal 
      "CFG must be cleared before being computed! Stmt %d '%a' \
       has %d successors" 
      s.sid Cil_printer.pp_stmt s (List.length s.succs);
  let addSucc (n: stmt) =
    if not (List.memq n s.succs) then s.succs <- n::s.succs;
    if not (List.memq s n.preds) then n.preds <- s::n.preds
  in
  let addOptionSucc (n: stmt option) =
    match n with
      None -> ()
    | Some n' -> addSucc n'
  in
  let addBlockSucc (b: block) =
    match b.bstmts with
      [] -> addOptionSucc next
    | hd::_ -> addSucc hd
  in
  let addBlockSuccFull (next:stmt) (b: block) =
    match b.bstmts with
      [] -> addSucc next
    | hd::_ -> addSucc hd
  in
  let instrFallsThrough (i : instr) : bool = match i with
      Call (_, {enode = Lval (Var vf, NoOffset)}, _, _) ->
        (* See if this has the noreturn attribute *)
        not (hasAttribute "noreturn" vf.vattr)
    | Call (_, f, _, _) ->
        not (hasAttribute "noreturn" (typeAttrs (typeOf f)))
    | _ -> true
  in
  match s.skind with
    Instr il  ->
      if instrFallsThrough il then
        addOptionSucc next
      else
        ()
  | Return _  -> ()
  | Goto (p,_) -> addSucc !p
  | Break _ -> addOptionSucc break
  | Continue _ -> addOptionSucc cont
  | If (_, blk1, blk2, _) ->
      (* The succs of If is [true branch;false branch] *)
      addBlockSucc blk2;
      addBlockSucc blk1;
      cfgBlock blk1 next break cont;
      cfgBlock blk2 next break cont

  | UnspecifiedSequence seq ->
      addBlockSucc (block_from_unspecified_sequence seq);
      cfgBlock (block_from_unspecified_sequence seq) next break cont
  | Block b ->
      addBlockSucc b;
      cfgBlock b next break cont

  | Switch(_,blk,_l,_) ->
      let bl = findCaseLabeledStmts blk in
      (* if there's no default, need to connect s->next *)
      if not (List.exists
                (fun stmt -> List.exists
                   (function Default _ -> true | _ -> false)
                   stmt.labels)
                bl)
      then addOptionSucc next;
      (* Then add cases, that will come first in final 'succs' list. bl
         is already reversed, so the order is ok. *)
      List.iter addSucc bl;
      cfgBlock blk next next cont
  | Loop(_,blk,_,_,_) ->
      addBlockSuccFull s blk;
      cfgBlock blk (Some s) next (Some s)

  (* Since all loops have terminating condition true, we don't put
     any direct successor to stmt following the loop *)
  | TryExcept _ | TryFinally _ ->
      Kernel.fatal "try/except/finally"

(*------------------------------------------------------------*)

(**************************************************************)
(* do something for all stmts in a fundec *)

let forallStmts todo (fd : fundec) =
  let vis = object
    inherit nopCilVisitor
    method! vstmt stmt = ignore (todo stmt); DoChildren
  end
  in
  ignore (visitCilFunction vis fd)

(**************************************************************)
(* printing the control flow graph - you have to compute it first *)

let d_cfgnodename fmt (s : stmt) = Format.fprintf fmt "%d" s.sid

let d_cfgnodelabel fmt (s : stmt) =
  let label =
    begin
      match s.skind with
	| If (_, _, _, _)  -> "if" (*sprint ~width:999 (dprintf "if %a" d_exp e)*)
	| Loop _ -> "loop"
	| Break _ -> "break"
	| Continue _ -> "continue"
	| Goto _ -> "goto"
	| Instr _ -> "instr"
	| Switch _ -> "switch"
	| Block _ -> "block"
	| Return _ -> "return"
	| TryExcept _ -> "try-except"
	| TryFinally _ -> "try-finally"
	| UnspecifiedSequence _ -> "unspecifiedsequence"
    end in
  Format.fprintf fmt "%d: %s" s.sid label

let d_cfgedge src fmt dest =
  Format.fprintf fmt "%a -> %a"
    d_cfgnodename src
    d_cfgnodename dest

let d_cfgnode fmt (s : stmt) =
  Format.fprintf fmt "%a [label=\"%a\"]\n\t@[<hov 2>%a@]"
    d_cfgnodename s
    d_cfgnodelabel s
    (Pretty_utils.pp_list ~sep:"@." (d_cfgedge s)) s.succs

(**********************************************************************)
(* entry points *)

(** print control flow graph (in dot form) for fundec to channel *)
let printCfgChannel fmt (fd : fundec) =
  let pnode (s:stmt) = Format.fprintf fmt "%a\n" d_cfgnode s in
  begin
    Format.fprintf fmt "digraph CFG_%s {\n" fd.svar.vname ;
    forallStmts pnode fd;
    Format.fprintf fmt  "}\n" ;
    end

(** Print control flow graph (in dot form) for fundec to file *)
let printCfgFilename (filename : string) (fd : fundec) =
  let chan = open_out filename in
    begin
      printCfgChannel (Format.formatter_of_out_channel chan) fd ;
      close_out chan;
    end


;;

(**********************************************************************)


let clearCFGinfo ?(clear_id=true) (fd : fundec) =
  let clear s =
    if clear_id then s.sid <- -1;
    s.succs <- [];
    s.preds <- [];
  in
  forallStmts clear fd

let clearFileCFG ?(clear_id=true) (f : file) =
  iterGlobals f (fun g ->
    match g with GFun(fd,_) ->
      clearCFGinfo ~clear_id fd
    | _ -> ())

let clear_sid_info_ref = Extlib.mk_fun "Cfg.clear_sid_info_ref"

let computeFileCFG (f : file) =
  !clear_sid_info_ref ();
  iterGlobals f (fun g -> match g with GFun(fd,_) -> cfgFun fd | _ -> ())

(* Cfg computation *)

open Logic_const

let statements : stmt list ref = ref []
(* Clear all info about the CFG in statements *)

class clear : cilVisitor = object
  inherit nopCilVisitor
  method! vstmt s = begin
    s.sid <- Sid.next ();
    statements := s :: !statements;
    s.succs <- [] ;
    s.preds <- [] ;
    DoChildren
  end
  method! vexpr _ = SkipChildren
  method! vtype _ = SkipChildren
  method! vinst _ = SkipChildren
end

let link source dest = begin
  if not (List.mem dest source.succs) then
    source.succs <- dest :: source.succs ;
  if not (List.mem source dest.preds) then
    dest.preds <- source :: dest.preds
end
let trylink source dest_option = match dest_option with
    None -> ()
  | Some(dest) -> link source dest


 (** Compute the successors and predecessors of a block, given a fallthrough *)
let rec succpred_block b fallthrough =
  let rec handle sl = match sl with
      [] -> ()
    | [a] -> succpred_stmt a fallthrough
    | hd :: ((next :: _) as tl) ->
      succpred_stmt hd (Some next) ;
      handle tl
  in handle b.bstmts


and succpred_stmt s fallthrough =
  match s.skind with
      Instr _ -> trylink s fallthrough
    | Return _ -> ()
    | Goto(dest,_) -> link s !dest
    | Break _
    | Continue _
    | Switch _ ->
      failwith "computeCFGInfo: cannot be called on functions with break, continue or switch statements. Use prepareCFG first to remove them."

    | If(_e1,b1,b2,_) ->
      (match b1.bstmts with
	  [] -> trylink s fallthrough
        | hd :: _ -> (link s hd ; succpred_block b1 fallthrough )) ;
      (match b2.bstmts with
	  [] -> trylink s fallthrough
        | hd :: _ -> (link s hd ; succpred_block b2 fallthrough ))

    | Loop(_,b,_,_,_) ->
      begin match b.bstmts with
	  [] -> failwith "computeCFGInfo: empty loop"
        | hd :: _ ->
	  link s hd ;
	  succpred_block b (Some(hd))
      end

    | Block(b) -> begin match b.bstmts with
	[] -> trylink s fallthrough
	| hd :: _ -> link s hd ;
	  succpred_block b fallthrough
    end
    | UnspecifiedSequence (((s1,_,_,_,_)::_) as seq) ->
      link s s1;
      succpred_block (block_from_unspecified_sequence seq) fallthrough
    | UnspecifiedSequence [] ->
      trylink s fallthrough
    | TryExcept _ | TryFinally _ ->
      failwith "computeCFGInfo: structured exception handling not implemented"


 (* This alphaTable is used to prevent collision of label names when
    transforming switch statements and loops. It uses a *unit*
    alphaTableData ref because there isn't any information we need to
    carry around. *)
let labelAlphaTable : (string, unit Alpha.alphaTableData ref) Hashtbl.t =
  Hashtbl.create 11

let freshLabel (base:string) =
  fst (Alpha.newAlphaName labelAlphaTable base ())


let xform_switch_block ?(keepSwitch=false) b =
  let breaks_stack = Stack.create () in
  let continues_stack = Stack.create () in
   (* NB: these are two stacks of stack, as the scope of
      breaks/continues clauses depends on two things: First,
      /*@ breaks P */ while(1) {} is not the same thing as
      while(1) { /*@ breaks P */ }:
      only the latter applies to the break of the current loop.
      Second
      while(1) { /*@ breaks P1 */ { /*@ breaks P2 */{}}}
      requires maintaining an inner stack, since the breaks
      of the current loop are under two different, nested, breaks
      clauses *)
  let () = Stack.push (Stack.create()) breaks_stack in
  let () = Stack.push (Stack.create()) continues_stack in
  let assert_of_clause f ca =
    match ca.annot_content with
      | AAssert _ | AInvariant _ | AVariant _ | AAssigns _ | AAllocation _ | APragma _ -> ptrue
      | AStmtSpec (_bhv,s) ->
        List.fold_left
          (fun acc bhv ->
            pand
              (acc,
               pimplies
                 (pands
                    (List.map
                       (fun p ->
                         pold ~loc:p.ip_loc
                           (Logic_utils.named_of_identified_predicate p))
                       bhv.b_assumes),
                  pands
                    (List.fold_left
                       (fun acc (kind,p) ->
                         if f kind then
                           Logic_utils.named_of_identified_predicate p
                           :: acc
                         else acc)
                       [ptrue] bhv.b_post_cond)
                 )))
          ptrue s.spec_behavior
  in
  let assert_of_continues ca =
    assert_of_clause (function Continues -> true | _ -> false) ca
  in
  let assert_of_breaks ca =
    assert_of_clause (function Breaks -> true | _ -> false) ca
  in
  let add_clause s ca =
    let cont_clause = assert_of_continues ca in
    let break_clause = assert_of_breaks ca in
    if not (Stack.is_empty continues_stack) then begin
      let old_clause = Stack.top continues_stack in
      let cont_clause = Logic_utils.translate_old_label s cont_clause in
      Stack.push cont_clause old_clause;
    end else begin
      Kernel.fatal "No stack where to put continues clause"
    end;
    if not (Stack.is_empty breaks_stack) then begin
      let old_clause = Stack.top breaks_stack in
      let break_clause = Logic_utils.translate_old_label s break_clause
      in
      Stack.push break_clause old_clause;
    end else begin
      Kernel.fatal "No stack where to put breaks clause"
    end
  in
  let rec popn n =
    if n > 0 then begin
      if
        Stack.is_empty breaks_stack || Stack.is_empty continues_stack
      then
        Kernel.fatal ~current:true "Cannot remove breaks/continues in clause stack";
      let breaks = Stack.top breaks_stack in
      if Stack.is_empty breaks then
        Kernel.fatal ~current:true "Cannot remove breaks in toplevel clause stack";
      ignore (Stack.pop breaks);
      let continues = Stack.top continues_stack in
      if Stack.is_empty continues then
        Kernel.fatal ~current:true "Cannot remove continues in toplevel clause stack";
      ignore (Stack.pop continues);
      popn (n-1);
    end
  in
  let rec xform_switch_stmt stmts break_dest cont_dest label_index popstack =
    match stmts with
        [] -> []
      | s :: rest ->
        begin
          CurrentLoc.set (Stmt.loc s);
          if not keepSwitch then
            s.labels <- List.map (fun lab -> match lab with
                Label _ -> lab
              | Case(e,l) ->
	        let suffix =
	          match isInteger e with
	            | Some value ->
	              if Integer.lt value Integer.zero then
		        "neg_" ^ Integer.to_string (Integer.neg value)
	              else
		        Integer.to_string value
	            | None ->
	              "exp"
	        in
	        let str = Format.sprintf "switch_%d_%s" label_index suffix in
	        (Label(freshLabel str,l,false))
              | Default(l) ->
                Label(freshLabel
		        (Printf.sprintf "switch_%d_default" label_index),
		      l, false)
            ) s.labels ;
          match s.skind with
            | Instr (Code_annot (ca,_)) ->
              add_clause s ca;
              s::
                xform_switch_stmt
                rest break_dest cont_dest label_index (popstack+1)
            | Instr _ | Return _ | Goto _  ->
              popn popstack;
              s::
                xform_switch_stmt
                rest break_dest cont_dest label_index 0
            | Break(l) ->
              if Stack.is_empty breaks_stack then
                Kernel.fatal "empty breaks stack";
              s.skind <- Goto(break_dest (),l);
              let breaks = Stack.top breaks_stack in
              let assertion = ref ptrue in
              Stack.iter (fun p -> assertion := pand (p,!assertion)) breaks;
              (match !assertion with
                  { content = Ptrue } ->
                    popn popstack;
                    s ::
                      xform_switch_stmt
                      rest break_dest cont_dest label_index 0
                | p ->
                  let a = Logic_const.new_code_annotation (AAssert ([],p)) in
                  let assertion = mkStmt (Instr(Code_annot(a,l))) in
                  popn popstack;
                  assertion:: s ::
                    xform_switch_stmt
                    rest break_dest cont_dest label_index 0)
            | Continue(l) ->
              if Stack.is_empty continues_stack then
                Kernel.fatal "empty continues stack";
              s.skind <- Goto(cont_dest (),l);
              let continues = Stack.top continues_stack in
              let assertion = ref ptrue in
              Stack.iter (fun p -> assertion := pand(p,!assertion)) continues;
              (match !assertion with
                  { content = Ptrue } ->
                    popn popstack;
                    s ::
                      xform_switch_stmt
                      rest break_dest cont_dest label_index 0
                | p ->
                  let a = Logic_const.new_code_annotation (AAssert([],p)) in
                  let assertion = mkStmt (Instr(Code_annot(a,l))) in
                  popn popstack;
                  assertion :: s ::
                    xform_switch_stmt
                    rest break_dest cont_dest label_index 0)
            | If(e,b1,b2,l) ->
              let b1 = xform_switch_block b1 break_dest cont_dest label_index
              in
              let b2 = xform_switch_block b2 break_dest cont_dest label_index
              in
              popn popstack;
              s.skind <- If(e,b1,b2,l);
              s:: xform_switch_stmt rest break_dest cont_dest label_index 0
            | Switch(e,b,sl,(_, snd_l as l)) ->
              let loc = snd_l, snd_l in
              if keepSwitch then begin
                let label_index = label_index + 1 in
                let break_stmt = mkStmt (Instr (Skip loc)) in
                break_stmt.labels <-
                  [Label
                      (freshLabel
                         (Printf.sprintf "switch_%d_break" label_index),
                       l, false)] ;
                Stack.push (Stack.create()) breaks_stack;
                let b =
                  xform_switch_block
                    b (fun () -> ref break_stmt) cont_dest label_index
                in
                ignore (Stack.pop breaks_stack);
                popn popstack;
                s.skind <- Switch (e,b,sl,l);
                s::break_stmt::
                  xform_switch_stmt rest break_dest cont_dest label_index 0
              end else begin
                 (* change
	          * switch (se) {
	          *   case 0: s0 ;
	          *   case 1: s1 ; break;
	          *   ...
	          * }
	          *
	          * into:
	          *
	          * if (se == 0) goto label_0;
	          * else if (se == 1) goto label_1;
	          * ...
	          * else goto label_break;
                  *  { // body_block
	          *  label_0: s0;
	          *  label_1: s1; goto label_break;
	          *  ...
	          * }
	          * label_break: ; // break_stmt
	          *
	          *)
                let label_index = label_index + 1 in
                let break_stmt = mkStmt (Instr (Skip loc)) in
                break_stmt.labels <-
	          [Label(freshLabel
                           (Printf.sprintf
                              "switch_%d_break" label_index), l, false)] ;
                 (* The default case, if present, must be used only if *all*
                    non-default cases fail [ISO/IEC 9899:1999, §6.8.4.2, ¶5]. As a
                    result, we sort the order in which we handle the labels (but not the
                    order in which we print out the statements, so fall-through still
                    works as expected). *)
                let compare_choices s1 s2 = match s1.labels, s2.labels with
                  | (Default(_) :: _), _ -> 1
                  | _, (Default(_) :: _) -> -1
                  | _, _ -> 0
                in
                let rec handle_choices sl =
                  match sl with
	              [] ->
                        (* If there's no case that matches and no default,
                           we just skip the entire switch (6.8.4.2.5)*)
                        Goto (ref break_stmt,l)
                    | stmt_hd :: stmt_tl ->
	              let rec handle_labels lab_list =
	                match lab_list with
	                    [] -> handle_choices stmt_tl
	                  | Case(ce,cl) :: lab_tl ->
                              (* begin replacement: *)
	                    let pred =
		              match ce.enode with
		                  Const (CInt64 (z,_,_)) 
                                    when Integer.equal z Integer.zero
                                      ->
		                    new_exp ~loc:ce.eloc (UnOp(LNot,e,intType))
		                | _ ->
		                  new_exp ~loc:ce.eloc (BinOp(Eq,e,ce,intType))
	                    in
                              (* end replacement *)
	                    let then_block =
                              mkBlock [ mkStmt (Goto(ref stmt_hd,cl)) ] in
	                    let else_block =
                              mkBlock [ mkStmt (handle_labels lab_tl) ] in
	                    If(pred,then_block,else_block,cl)
	                  | Default(dl) :: lab_tl ->
	                      (* ww: before this was 'if (1) goto label',
                                 but as Ben points out this might confuse
                                 someone down the line who doesn't have special
                                 handling for if(1) into thinking
                                 that there are two paths here.
                                 The simpler 'goto label' is what we want. *)
	                    Block(mkBlock [ mkStmt (Goto(ref stmt_hd,dl)) ;
			                    mkStmt (handle_labels lab_tl) ])
	                  | Label(_,_,_) :: lab_tl -> handle_labels lab_tl
                      in
	              handle_labels stmt_hd.labels
                in
                let sl = List.sort compare_choices sl in
                let ifblock = mkStmt (handle_choices sl) in
                Stack.push (Stack.create()) breaks_stack;
                let switch_block =
                  xform_switch_block
                    b (fun () -> ref break_stmt) cont_dest label_index
                in
                ignore (Stack.pop breaks_stack);
                popn popstack;
                s.skind <- Block switch_block;
                (match switch_block.bstmts with
                    ({ skind = Instr(Code_annot _) } as ca):: tl ->
                      (* We move the annotation outside of the block, since
                         the \old would otherwise be attached to a label which
                         by construction is never reached.
                       *)
                      switch_block.bstmts <- ca :: ifblock :: tl
                  | l -> switch_block.bstmts <- ifblock :: l);
                s :: break_stmt ::
                  xform_switch_stmt rest break_dest cont_dest label_index 0
              end
            | Loop(a,b,(fst_l, snd_l as l),_,_) ->
	      let label_index = label_index + 1 in
              let loc_break = snd_l, snd_l in
	      let break_stmt = mkStmt (Instr (Skip loc_break)) in
              break_stmt.labels <-
	        [Label(freshLabel
                         (Printf.sprintf
                            "while_%d_break" label_index),l,false)] ;
              let cont_loc = fst_l, fst_l in
              let cont_stmt = mkStmt (Instr (Skip cont_loc)) in
              cont_stmt.labels <-
	        [Label
                    (freshLabel
                       (Printf.sprintf
                          "while_%d_continue" label_index),l,false)] ;
              b.bstmts <- cont_stmt :: b.bstmts ;
              let my_break_dest () = ref break_stmt in
              let my_cont_dest () = ref cont_stmt in
              Stack.push (Stack.create ()) breaks_stack;
              Stack.push (Stack.create ()) continues_stack;
              let b =
                xform_switch_block b my_break_dest my_cont_dest label_index
              in
              s.skind <- Loop(a,b,l,Some(cont_stmt),Some(break_stmt));
              break_stmt.succs <- s.succs ;
              ignore (Stack.pop breaks_stack);
              ignore (Stack.pop continues_stack);
              popn popstack;
              s :: break_stmt ::
                xform_switch_stmt rest break_dest cont_dest label_index 0
            | Block b ->
              let b = xform_switch_block b break_dest cont_dest label_index in
              popn popstack;
              s.skind <- Block b;
              s :: xform_switch_stmt rest break_dest cont_dest label_index 0
            | UnspecifiedSequence seq ->
              let seq =
                xform_switch_unspecified seq break_dest cont_dest label_index
              in
              popn popstack;
              s.skind <- UnspecifiedSequence seq;
              s :: xform_switch_stmt rest break_dest cont_dest label_index 0
            | TryExcept _ | TryFinally _ ->
              Kernel.fatal
                "xform_switch_statement: \
                  structured exception handling not implemented"
        end
  and xform_switch_block b break_dest cont_dest label_index =
     (* [VP] I fail to understand what link_succs is supposed to do. The head
        of the block has as successors all the statements in the block? *)
     (*     let rec link_succs sl = match sl with
            | [] -> ()
            | hd :: tl -> (if hd.succs = [] then hd.succs <- tl) ; link_succs tl
            in
            link_succs b.bstmts ; *)
    { b with bstmts =
        xform_switch_stmt b.bstmts break_dest cont_dest label_index 0 }
  and xform_switch_unspecified seq break_dest cont_dest label_index =
    let treat_one (s,m,w,r,c) =
       (* NB: this assumes that we don't have any statement contract in
          an unspecified sequence.
        *)
      let res = xform_switch_stmt [s] break_dest cont_dest label_index 0
      in
      (List.hd res, m,w,r,c)
      ::(List.map (fun s -> (s,[],[],[],[])) (List.tl res))
    in
    (List.concat (List.map treat_one seq))
  in
  xform_switch_block b
    (fun () -> Kernel.abort "break outside of loop or switch")
    (fun () -> Kernel.abort "continues outside of loop")
    (-1)

(* Enter all the labels in a function into an alpha renaming table to
   prevent duplicate labels when transforming loops and switch
   statements. *)
class registerLabelsVisitor : cilVisitor = object
  inherit nopCilVisitor
  method! vstmt { labels = labels } = begin
    List.iter
      (function
        | Label (name,_,_) ->
          Alpha.registerAlphaName labelAlphaTable name ()
        | _ -> ())
      labels;
    DoChildren
  end
  method! vexpr _ = SkipChildren
  method! vtype _ = SkipChildren
  method! vinst _ = SkipChildren
end

(* prepare a function for computeCFGInfo by removing break, continue,
 * default and switch statements/labels and replacing them with Ifs and
 * Gotos. *)
let prepareCFG ?(keepSwitch=false) (fd : fundec) : unit =
  (* Labels are local to a function, so start with a clean slate by
     clearing labelAlphaTable. Then register all labels. *)
  Hashtbl.clear labelAlphaTable;
  ignore (visitCilFunction (new registerLabelsVisitor) fd);
  let b = xform_switch_block ~keepSwitch fd.sbody in
  fd.sbody <- b

(* make the cfg and return a list of statements *)
let computeCFGInfo (f : fundec) (_global_numbering : bool) : unit =
  statements := [];
  let clear_it = new clear in
  ignore (visitCilBlock clear_it f.sbody) ;
  f.smaxstmtid <- Some (Sid.next ()) ;
  succpred_block f.sbody (None);
  let res = List.rev !statements in
  statements := [];
  f.sallstmts <- res;
  ()

(*
Local Variables:
compile-command: "make -C ../../.."
End:
*)
