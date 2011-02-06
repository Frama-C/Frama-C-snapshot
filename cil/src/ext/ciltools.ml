(**************************************************************************)
(*                                                                        *)
(*  Copyright (C) 2001-2003                                               *)
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
(*  File modified by CEA (Commissariat à l'énergie atomique et aux        *)
(*                        énergies alternatives).                         *)
(**************************************************************************)

open Cil_types
open Cil

(* Contributed by Nathan Cooprider *)

let isOne e = 
  isInteger e = Some Int64.one


(* written by Zach *)
let is_volatile_tp tp =
  List.exists (function (Attr("volatile",_)) -> true 
    | _ -> false) (typeAttrs tp) 
    
(* written by Zach *)
let is_volatile_vi vi =
  let vi_vol =
    List.exists (function (Attr("volatile",_)) -> true 
      | _ -> false) vi.vattr in
  let typ_vol = is_volatile_tp vi.vtype in
  vi_vol || typ_vol

(*****************************************************************************
 * A collection of useful functions that were not already in CIL as far as I 
 * could tell. However, I have been surprised before . . . 
 ****************************************************************************)

type sign = Signed | Unsigned 

exception Not_an_integer

(*****************************************************************************
 * A bunch of functions for accessing integers. Originally written for 
 * somebody who didn't know CIL and just wanted to mess with it at the 
 * OCaml level. 
 ****************************************************************************)

let unbox_int_type (ye : typ) : (int * sign) =
  let tp = unrollType ye in
  let s = 
    match tp with 
      TInt (i, _) -> 
	if (isSigned i) then
	  Signed
	else
	  Unsigned
    | _ -> raise Not_an_integer
  in
  (bitsSizeOf tp), s
  
(* depricated. Use isInteger directly instead *)
let unbox_int_exp (e : exp) : int64 = 
  match isInteger e with 
    None -> raise Not_an_integer
  | Some (x) -> x
  
let box_int_to_exp (n : int64) (ye : typ) : exp =
  let tp = unrollType ye in
  match tp with 
    TInt (i, _) -> 
      kinteger64 ~loc:Cil_datatype.Location.unknown i n 
  | _ -> raise Not_an_integer

let cil_to_ocaml_int (e : exp) : (int64 * int * sign) = 
  let v, s = unbox_int_type (typeOf e) in
  unbox_int_exp (e), v, s

exception Weird_bitwidth

(* (int64 * int * sign) : exp *)
let ocaml_int_to_cil v n s =
  let char_size = bitsSizeOf charType in 
  let int_size = bitsSizeOf intType in
  let short_size = bitsSizeOf (TInt(IShort,[]))in 
  let long_size = bitsSizeOf longType in
  let longlong_size = bitsSizeOf (TInt(ILongLong,[])) in
  let i = 
    match s with
      Signed ->
	if (n = char_size) then 
	  ISChar
	else if (n = int_size) then
	  IInt
	else if (n = short_size) then
	  IShort
	else if (n = long_size) then
	  ILong
	else if (n = longlong_size) then
	  ILongLong
	else
	  raise Weird_bitwidth
    | Unsigned ->
	if (n = char_size) then 
	  IUChar
	else if (n = int_size) then
	  IUInt
	else if (n = short_size) then
	  IUShort
	else if (n = long_size) then
	  IULong
	else if (n = longlong_size) then
	  IULongLong
	else
	  raise Weird_bitwidth
  in
  kinteger64 i v

(*****************************************************************************
 * a couple of type functions that I thought would be useful:
 ****************************************************************************)

let rec isCompositeType tp =
  match tp with
    TComp _  -> true
  | TPtr(x, _) -> isCompositeType x
  | TArray(x,_,_,_) -> isCompositeType x
  | TFun(x,_,_,_) -> isCompositeType x
  | TNamed (x,_) -> isCompositeType x.ttype
  | _ -> false

(** START OF deepHasAttribute ************************************************)
let visited = ref [] 
class attribute_checker target rflag = object 
  inherit nopCilVisitor
  method vtype t =
    match t with 
      TComp(cinfo, _, _) ->
	if(not (List.exists (fun x -> cinfo.cname = x) !visited )) then begin
	  visited := cinfo.cname :: !visited;
	  List.iter 
	    (fun f -> 
	      if (hasAttribute target f.fattr) then 
		rflag := true
	      else
		ignore(visitCilType (new attribute_checker target rflag) 
			 f.ftype)) cinfo.cfields;
	end;
	DoChildren	
    | TNamed(t1, _a) ->
	if(not (List.exists (fun x -> t1.tname = x) !visited )) then begin
	  visited := t1.tname :: !visited;
	  ignore(visitCilType (new attribute_checker target rflag) t1.ttype);
	end;
	DoChildren
    | _ ->
	DoChildren
  method vattr attr =
    if (attributeName attr = target) then rflag := true;
    DoChildren
end

let deepHasAttribute s t =
  let found = ref false in
  visited := [];
  ignore(visitCilType (new attribute_checker s found) t);
  !found
(** END OF deepHasAttribute **************************************************)

(** Stuff from ptranal, slightly modified ************************************)

(*****************************************************************************
 * A transformation to make every instruction be in its own statement.  
 ****************************************************************************)

class callBBVisitor = object
  inherit nopCilVisitor 

  method vstmt s =
    match s.skind with
      Instr _ -> begin
	  SkipChildren
      end
    | _ -> DoChildren

  method vvdec _ = SkipChildren
  method vexpr _ = SkipChildren
  method vlval _ = SkipChildren
  method vtype _ = SkipChildren
end 

let one_instruction_per_statement f =
  let thisVisitor = new callBBVisitor in
  visitCilFileSameGlobals thisVisitor f  

(*****************************************************************************
 * A transformation that gives each variable a unique identifier. 
 ****************************************************************************)

class vidVisitor = object
  inherit nopCilVisitor 
  val count = ref 0 

  method vvdec vi = 
    vi.vid <- !count ;
    incr count ; SkipChildren
end 

let globally_unique_vids f =
  let thisVisitor = new vidVisitor in
  visitCilFileSameGlobals thisVisitor f 

(** End of stuff from ptranal ************************************************)

class sidVisitor = object
  inherit nopCilVisitor 
  val count = ref 0 

  method vstmt s = 
    s.sid <- !count ;
    incr count ;
    DoChildren
end 

let globally_unique_sids f =
  let thisVisitor = new sidVisitor in
  visitCilFileSameGlobals thisVisitor f 
