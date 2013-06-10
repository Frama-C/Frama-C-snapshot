(**************************************************************************)
(*                                                                        *)
(*  This file is part of WP plug-in of Frama-C.                           *)
(*                                                                        *)
(*  Copyright (C) 2007-2013                                               *)
(*    CEA (Commissariat a l'energie atomique et aux energies              *)
(*         alternatives)                                                  *)
(*                                                                        *)
(*  you can redistribute it and/or modify it under the terms of the GNU   *)
(*  Lesser General Public License as published by the Free Software       *)
(*  Foundation, version 2.1.                                              *)
(*                                                                        *)
(*  It is distributed in the hope that it will be useful,                 *)
(*  but WITHOUT ANY WARRANTY; without even the implied warranty of        *)
(*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the         *)
(*  GNU Lesser General Public License for more details.                   *)
(*                                                                        *)
(*  See the GNU Lesser General Public License version 2.1                 *)
(*  for more details (enclosed in the file licenses/LGPLv2.1).            *)
(*                                                                        *)
(**************************************************************************)

(* -------------------------------------------------------------------------- *)
(* --- Integer Arithmetics Model                                          --- *)
(* -------------------------------------------------------------------------- *)

open Qed
open Qed.Logic
open Lang
open Lang.F

(* -------------------------------------------------------------------------- *)
(* --- Library Cint                                                       --- *)
(* -------------------------------------------------------------------------- *)

let theory = "cint"

let make_fun_int op i = 
  Lang.extern_f ~theory ~result:Logic.Sint "%s_%a" op Ctypes.pp_int i
let make_pred_int op i = 
  Lang.extern_f ~theory ~result:Logic.Sprop "%s_%a" op Ctypes.pp_int i

(* let fun_int op = Ctypes.imemo (make_fun_int op) *) (* unused for now *)
(* let pred_int op = Ctypes.imemo (make_pred_int op) *) (* unused for now *)

(* is_<cint> : int -> prop *)
let p_is_int = Ctypes.imemo 
  (fun iota ->
     let f = make_pred_int "is" iota in
     let simplify = function
       | [e] -> 
	   begin
	     match F.repr e with
	       | Logic.Kint k ->
		   let vmin,vmax = Ctypes.c_int_bounds iota in
		   F.e_bool (Z.leq vmin k && Z.lt k vmax)
	       | Logic.If(p,b,c) -> F.e_if p (e_fun f [b]) (e_fun f [c])
	       | _ -> raise Not_found
	   end
       | _ -> raise Not_found
     in F.add_builtin f simplify ; f)

let f_to_int = Ctypes.imemo
  (fun iota ->
     let f = make_fun_int "to" iota in
     let simplify = function
       | [e] -> 
	   begin
	     match F.repr e with
	       | Logic.Kint value ->
		   let vmin,vmax = Ctypes.c_int_bounds iota in
                   let v = Z.cast_max ~max:vmax
		             ~signed:(Z.lt vmin Z.zero) ~value
		   in F.e_zint v
	       | _ -> raise Not_found
	   end
       | _ -> raise Not_found
     in F.add_builtin f simplify ; f)

let f_of_real = extern_f ~theory:"qed" ~result:Logic.Sint "int_of_real"

(* Signature int,int -> int over Z *)
let result = Logic.Sint
let ac = {
  associative = true ;
  commutative = true ;
  idempotent = false ;
  inversible = false ;
  neutral = E_none ;
  absorbant = E_none ;
}

(* -------------------------------------------------------------------------- *)
(* --- Library Cbits                                                      --- *)
(* -------------------------------------------------------------------------- *)

let theory = "cbits"
let balance = Lang.Left

let op_lxor = { ac with neutral = E_int 0 ; inversible = true }
let op_lor  = { ac with neutral = E_int 0 ; absorbant = E_int (-1); idempotent = true }
let op_land = { ac with neutral = E_int (-1); absorbant = E_int 0 ; idempotent = true }

let f_lnot = Lang.extern_f ~theory ~result "lnot"
let f_lor  = Lang.extern_f ~theory ~result ~category:(Operator op_lor) ~balance "lor"
let f_land = Lang.extern_f ~theory ~result ~category:(Operator op_land) ~balance "land"
let f_lxor = Lang.extern_f ~theory ~result ~category:(Operator op_lxor) ~balance "lxor"
let f_lsl = Lang.extern_f ~theory ~result "lsl"
let f_lsr = Lang.extern_f ~theory ~result "lsr"

(* -------------------------------------------------------------------------- *)
(* --- Conversion Symbols                                                 --- *)
(* -------------------------------------------------------------------------- *)

let of_real i a = e_fun (f_to_int i) [e_fun f_of_real [a]]
let irange i a = p_call (p_is_int i) [a]
let iconvert i a = e_fun (f_to_int i) [a]
let iconvert_unsigned i x = if Ctypes.signed i then x else iconvert i x

type model =
  | Natural (** Integer arithmetics *)
  | Machine  (** Modulo arithmetics *)

let model = Context.create ~default:Natural "Cint.model"

let ibinop f i x y  = 
  let z = f x y in 
  match Context.get model with Natural -> z | Machine -> iconvert i z

let iunop f i x =
  let z = f x in
  match Context.get model with Natural -> z | Machine -> iconvert i z

(* -------------------------------------------------------------------------- *)
(* --- Arithmetics                                                        --- *)
(* -------------------------------------------------------------------------- *)

(* C Code Semantics *)
let iopp = iunop e_opp
let iadd = ibinop e_add
let isub = ibinop e_sub
let imul = ibinop e_mul  
let idiv = ibinop e_div
let imod = ibinop e_mod

(* -------------------------------------------------------------------------- *)
(* --- Bits                                                               --- *)
(* -------------------------------------------------------------------------- *)

let op1 f smp =
  let once = ref false in
  fun e ->
    begin
      if not !once then
	begin
	  F.add_builtin f smp ;
	  once := true ;
	end ;
      e_fun f [e]
    end

let op2 f smp =
  let once = ref false in
  fun a b ->
    begin
      if not !once then
	begin
	  F.add_builtin f smp ;
	  once := true ;
	end ;
      e_fun f [a;b]
    end

let smp1 zf =  (* f(c1) ~> zf(c1) *)
  function
    | [e] -> begin match F.repr e with
	| Logic.Kint c1 -> e_zint (zf c1)
	| _ -> raise Not_found
      end
    | _ -> raise Not_found

let smp2 f zf = (* f(c1,c2) ~> zf(c1,c2),  f(c1,c2,...) ~> f(zf(c1,c2),...) *)
  function
    | e1::e2::others -> begin match (F.repr e1), (F.repr e2) with
	  (* integers should be at the begining of the list *)
	| Logic.Kint c1, Logic.Kint c2 -> 
	    let z12 = ref (zf c1 c2) in
	    let rec smp2 = function (* look at the other integers *)
	      | [] -> []
              | (e::r) as l -> begin match (F.repr e) with
		  | Logic.Kint c -> z12 := zf !z12 c; smp2 r
		  | _ -> l
		end
	    in let others = smp2 others
	    in let c12 = e_zint !z12 in
	    if others = [] || F.is_absorbant f c12
	    then c12
	    else if F.is_neutral f c12 then
              match others with
		| [x] -> x
		| _ -> e_funraw f others
            else e_funraw f (c12::others)
	| _ -> raise Not_found
      end
    | _ -> raise Not_found
	
let smp_shift zf = (* f(e1,0)~>e1,  c2>0==>f(c1,c2)~>zf(c1,c2) *)
  function
    | [e1;e2] -> begin match (F.repr e1), (F.repr e2) with
        | _, Logic.Kint c2 when (Qed.Z.null c2) -> e1
        | Logic.Kint c1, Logic.Kint c2 (* undefined when c2 is negative *)
            when (Qed.Z.positive c2) -> e_zint (zf c1 c2)
        | _ -> raise Not_found
      end
    | _ -> raise Not_found

(* ACSL Semantics *)
let l_not = op1 f_lnot (smp1 Qed.Z.bitwise_not)
let l_xor = op2 f_lxor (smp2 f_lxor Qed.Z.bitwise_xor)
let l_or  = op2 f_lor  (smp2 f_lor Qed.Z.bitwise_or)
let l_and = op2 f_land (smp2 f_land Qed.Z.bitwise_and)

(* shift as mult: (0<<y)~>0 is invalid in ACSL when y is negative *)
let l_lsl = op2 f_lsl (smp_shift Qed.Z.bitwise_shift_left)

(* shift as div: (0>>y)~>0, (-1>>y)~>-1 are invalid in ACSL when y is negative *)
let l_lsr = op2 f_lsr (smp_shift Qed.Z.bitwise_shift_right)

(* C Code Semantics *)
let bnot i x   = iconvert_unsigned i (l_not x)
let bxor i x y = iconvert_unsigned i (l_xor x y)
let bor _i  = l_or  (* no needs of range conversion *)
let band _i = l_and (* no needs of range conversion *)

let blsl i x y = iconvert i (l_lsl x y)
let blsr _i = l_lsr (* no needs of range conversion *)

(* -------------------------------------------------------------------------- *)
