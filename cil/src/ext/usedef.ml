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
open Cil

(** compute use/def information *)

module VS = Cil_datatype.Varinfo.Set

(** Set this global to how you want to handle function calls.
    This also returns a modified argument list which will be used for the
    purpose of Use analysis, in case you have a function that needs special
    treatment of its args. *)
let getUseDefFunctionRef: (exp -> exp list -> VS.t * VS.t * exp list) ref =
  ref (fun _func args -> (VS.empty, VS.empty, args))

(** Say if you want to consider a variable use.  This applies to
  variable reads only; see also considerVariableAddrOfAsUse *)
let considerVariableUse: (varinfo -> bool) ref =
  ref (fun _ -> true)


(** Say if you want to consider a variable def *)
let considerVariableDef: (varinfo -> bool) ref =
  ref (fun _ -> true)

(** Say if you want to consider a variable addrof as a use *)
let considerVariableAddrOfAsUse: (varinfo -> bool) ref =
  ref (fun _ -> true)

(** Return any vars that should be considered "used" by an expression,
    other than the ones it refers to directly.  Deputy uses this for
    variables in Cast annotations. *)
let extraUsesOfExpr: (exp -> VS.t) ref =
  ref (fun _ -> VS.empty)

(* When this is true, only definitions of a variable without
   an offset are counted as definitions. So:
   a = 5; would be a definition, but
   a[1] = 5; would not.
   Exception: writing to a union field is considered to be a definition of
   the union even if this is set to true.*)
let onlyNoOffsetsAreDefs: bool ref = ref false

(** Should we ignore the contents of sizeof and alignof? *)
let ignoreSizeof: bool ref = ref true

let varUsed: VS.t ref = ref VS.empty
let varDefs: VS.t ref = ref VS.empty

class useDefVisitorClass : cilVisitor = object (self)
  inherit nopCilVisitor

  (** this will be invoked on variable definitions only because we intercept
   * all uses of variables in expressions ! *)
  method! vvrbl (v: varinfo) =
    if (!considerVariableDef) v &&
      not(!onlyNoOffsetsAreDefs) then
      varDefs := VS.add v !varDefs;
    SkipChildren

  (** If onlyNoOffsetsAreDefs is true, then we need to see the
   *  varinfo in an lval along with the offset. Otherwise just
   *  DoChildren *)
  method! vlval (l: lval) =
    if !onlyNoOffsetsAreDefs then
      match l with
	(Var vi, NoOffset) ->
	  if (!considerVariableDef) vi then
	    varDefs := VS.add vi !varDefs;
	  SkipChildren
      | (Var vi, Field(fi, NoOffset)) when not fi.fcomp.cstruct ->
          (* If we are writing to a union field, treat that the same
             as a write to a union. *)
	  if (!considerVariableDef) vi then
	    varDefs := VS.add vi !varDefs;
	  SkipChildren
      | _ -> DoChildren
    else DoChildren

  method! vexpr (e:exp) =
    let extra = (!extraUsesOfExpr) e in
    if not (VS.is_empty extra) then
      varUsed := VS.union extra !varUsed;
    match e.enode with
      Lval (Var v, off) ->
        ignore (visitCilOffset (self :> cilVisitor) off);
        if (!considerVariableUse) v then
          varUsed := VS.add v !varUsed;
        SkipChildren (* So that we do not see the v *)

    | AddrOf (Var v, off)
    | StartOf (Var v, off) ->
        ignore (visitCilOffset (self :> cilVisitor) off);
        if (!considerVariableAddrOfAsUse) v then
          varUsed := VS.add v !varUsed;
        SkipChildren

    | SizeOfE _
    | AlignOfE _ when !ignoreSizeof -> SkipChildren

    | _ -> DoChildren

  (* For function calls, do the transitive variable read/defs *)
  method! vinst = function
      Call (lvo, f, args, _) -> begin
        (* we will compute the use and def that appear in
         * this instruction. We also add in the stuff computed by
         * getUseDefFunctionRef *)
        let use, def, args' = !getUseDefFunctionRef f args in
        varUsed := VS.union !varUsed use;
        varDefs := VS.union !varDefs def;

        (* Now visit the children of  "Call (lvo, f, args', _)" *)
        let self: cilVisitor = (self :> cilVisitor) in
        (match lvo with None -> ()
         | Some lv -> ignore (visitCilLval self lv));
        ignore (visitCilExpr self f);
        List.iter (fun arg -> ignore (visitCilExpr self arg)) args';
        SkipChildren;
      end
    | Asm(_,_,slvl,_,_,_,_) -> List.iter (fun (_,s,lv) ->
	match lv with (Var v, _off) ->
	  if s.[0] = '+' then
	    varUsed := VS.add v !varUsed;
	| _ -> ()) slvl;
	DoChildren
    | _ -> DoChildren

end

let useDefVisitor = new useDefVisitorClass

(** Compute the use information for an expression (accumulate to an existing
 * set) *)
let computeUseExp ?(acc=VS.empty) (e: exp) : VS.t =
  varUsed := acc;
  ignore (visitCilExpr useDefVisitor e);
  !varUsed


(** Compute the use/def information for an instruction *)
let computeUseDefInstr ?(acc_used=VS.empty)
                       ?(acc_defs=VS.empty)
                       (i: instr) : VS.t * VS.t =
  varUsed := acc_used;
  varDefs := acc_defs;
  ignore (visitCilInstr useDefVisitor i);
  !varUsed, !varDefs


(** Compute the use/def information for a statement kind. Do not descend into
 * the nested blocks. *)
let computeUseDefStmtKind ?(acc_used=VS.empty)
                          ?(acc_defs=VS.empty)
                          (sk: stmtkind) : VS.t * VS.t =
  varUsed := acc_used;
  varDefs := acc_defs;
  let ve e = ignore (visitCilExpr useDefVisitor e) in
  let _ =
    match sk with
      Return (None, _) -> ()
    | Return (Some e, _) -> ve e
    | If (e, _, _, _) -> ve e
    | Break _ | Goto _ | Continue _ -> ()
    | Loop (_,_, _, _, _) -> ()
    | Switch (e, _, _, _) -> ve e
    | Instr il ->
        ignore (visitCilInstr useDefVisitor il)
    | TryExcept _ | TryFinally _ -> ()
    | Block _ | UnspecifiedSequence _ -> ()
  in
  !varUsed, !varDefs

(* Compute the use/def information for a statement kind.
   DO descend into nested blocks *)
let rec computeDeepUseDefStmtKind ?(acc_used=VS.empty)
                                  ?(acc_defs=VS.empty)
                                   (sk: stmtkind) : VS.t * VS.t =
  let handle_block b =
    List.fold_left (fun (u,d) s ->
      let u',d' = computeDeepUseDefStmtKind s.skind in
      (VS.union u u', VS.union d d')) (VS.empty, VS.empty)
      b.bstmts
  in
  varUsed := acc_used;
  varDefs := acc_defs;
  let ve e = ignore (visitCilExpr useDefVisitor e) in
  match sk with
    Return (None, _) -> !varUsed, !varDefs
  | Return (Some e, _) ->
      let _ = ve e in
      !varUsed, !varDefs
  | If (e, tb, fb, _) ->
      let _ = ve e in
      let u, d = !varUsed, !varDefs in
      let u', d' = handle_block tb in
      let u'', d'' = handle_block fb in
      (VS.union (VS.union u u') u'', VS.union (VS.union d d') d'')
  | Break _ | Goto _ | Continue _ -> !varUsed, !varDefs
  | Loop (_,b, _, _, _) -> handle_block b
  | Switch (e, b, _, _) ->
      let _ = ve e in
      let u, d = !varUsed, !varDefs in
      let u', d' = handle_block b in
      (VS.union u u', VS.union d d')
  | Instr il ->
      ignore (visitCilInstr useDefVisitor il);
      !varUsed, !varDefs
  | TryExcept _ | TryFinally _ -> !varUsed, !varDefs
  | Block b -> handle_block b
  | UnspecifiedSequence seq -> handle_block
      (block_from_unspecified_sequence seq)

let computeUseLocalTypes ?(acc_used=VS.empty)
                         (fd : fundec)
    =
  List.fold_left (fun u vi ->
    ignore(visitCilType useDefVisitor vi.vtype);
    VS.union u (!varUsed)) acc_used fd.slocals
