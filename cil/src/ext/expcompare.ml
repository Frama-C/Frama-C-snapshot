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

(* Remove casts that do not effect the value of the expression, such
 * as casts between different pointer types.  Of course, these casts
 * change the type, so don't use this within e.g. an arithmetic
 * expression.
 *
 * We remove casts from pointer types to unsigned int or unsigned long.
 *
 * We also prune casts between equivalent integer types, such as a
 * difference in sign or int vs long.  But we keep other arithmetic casts,
 * since they actually change the value of the expression. *)
let rec stripNopCasts (e:exp): exp =
  match e.enode with
    CastE(t, e') -> begin
      match unrollType (typeOf e'), unrollType t  with
        TPtr _, TPtr _ -> (* okay to strip *)
          stripNopCasts e'
      (* strip casts from pointers to unsigned int/long*)
      | (TPtr _ as t1), (TInt(ik,_) as t2)
          when bitsSizeOf t1 = bitsSizeOf t2
            && not (isSigned ik) ->
          stripNopCasts e'
      | (TInt _ as t1), (TInt _ as t2)
          when bitsSizeOf t1 = bitsSizeOf t2 -> (* Okay to strip.*)
          stripNopCasts e'
      |  _ -> e
    end
  | _ -> e

let compareExpStripCasts (e1: exp) (e2: exp) : bool =
  compareExp (stripNopCasts e1) (stripNopCasts e2)

(* A more conservative form of stripNopCasts.  Here, we only strip pointer
   casts if the base types have the same width.  Using this on the left operand
   of pointer arithmetic shouldn't change the resulting value. *)
let rec stripCastsForPtrArith (e:exp): exp =
  match e.enode with
  | CastE(t, e') -> begin
      match unrollType (typeOf e'), unrollType t with
        TPtr (bt1, _), TPtr (bt2, _) -> begin
          try
            if bitsSizeOf bt1 = bitsSizeOf bt2 then (* Okay to strip *)
              stripCastsForPtrArith e'
            else
              e
          with SizeOfError _ -> (* bt1 or bt2 is abstract; don't strip. *)
            e
        end
      (* strip casts from pointers to unsigned int/long*)
      | (TPtr _ as t1), (TInt(ik,_) as t2)
          when bitsSizeOf t1 = bitsSizeOf t2
            && not (isSigned ik) ->
          stripCastsForPtrArith e'
      | (TInt _ as t1), (TInt _ as t2)
          when bitsSizeOf t1 = bitsSizeOf t2 -> (* Okay to strip.*)
          stripCastsForPtrArith e'
      |  _ -> e
    end
  | _ -> e


class volatileFinderClass br = object
  inherit nopCilVisitor

  method! vtype (t : typ)  =
    if hasAttribute "volatile" (typeAttrs t) then begin
      br := true;
      SkipChildren
    end
    else
      DoChildren

end

let isTypeVolatile t =
  let br = ref false in
  let vis = new volatileFinderClass br in
  ignore(visitCilType vis t);
  !br

(* strip every cast between equal pointer types *)
let rec stripCastsDeepForPtrArith (e:exp): exp =
  match e.enode with
  | CastE(t, e') when not(isTypeVolatile t) -> begin
      let e' = stripCastsDeepForPtrArith e' in
      match unrollType (typeOf e'), unrollType t with
      | TPtr (bt1, _), TPtr (bt2, _) -> begin
          try
            if bitsSizeOf bt1 = bitsSizeOf bt2 then (* Okay to strip *)
              e'
            else
              new_exp ~loc:e.eloc (CastE(t,e'))
          with SizeOfError _ -> (* bt1 or bt2 is abstract; don't strip. *)
            new_exp ~loc:e.eloc (CastE(t,e'))
        end
      | _, _ -> new_exp ~loc:e.eloc (CastE(t,e'))
    end
  | UnOp(op,e',t) ->
      let e' = stripCastsDeepForPtrArith e' in
      new_exp ~loc:e.eloc (UnOp(op, e', t))
  | BinOp(MinusPP,e1,e2,t) ->
      let e1 = stripCastsDeepForPtrArith e1 in
      let e2 = stripCastsDeepForPtrArith e2 in
      if not(Cil_datatype.Typ.equal (typeOf e1) (typeOf e2))
      then new_exp ~loc:e.eloc 
        (BinOp(MinusPP, mkCast e1 (typeOf e2), e2, t))
      else new_exp ~loc:e.eloc (BinOp(MinusPP, e1, e2, t))
  | BinOp(op,e1,e2,t) ->
      let e1 = stripCastsDeepForPtrArith e1 in
      let e2 = stripCastsDeepForPtrArith e2 in
      new_exp ~loc:e.eloc (BinOp(op,e1,e2,t))
  | Lval lv -> new_exp ~loc:e.eloc (Lval(stripCastsForPtrArithLval lv))
  | AddrOf lv -> new_exp ~loc:e.eloc (AddrOf(stripCastsForPtrArithLval lv))
  | StartOf lv -> new_exp ~loc:e.eloc (StartOf(stripCastsForPtrArithLval lv))
  | _ -> e

and stripCastsForPtrArithLval (lv : lval) : lval =
  match lv with
  | (Var vi, off) -> (Var vi, stripCastsForPtrArithOff off)
  | (Mem e, off) ->
      let e = stripCastsDeepForPtrArith e in
      let off = stripCastsForPtrArithOff off in
      (Mem e, off)

and stripCastsForPtrArithOff (off : offset ) : offset =
  match off with
  | NoOffset -> NoOffset
  | Field(fi, off) -> Field(fi, stripCastsForPtrArithOff off)
  | Index(e, off) ->
      let e = stripCastsDeepForPtrArith e in
      let off = stripCastsForPtrArithOff off in
      Index(e, off)

let compareExpDeepStripCasts (e1 : exp) (e2 : exp) : bool =
  compareExp (stripCastsDeepForPtrArith e1) (stripCastsDeepForPtrArith e2)
