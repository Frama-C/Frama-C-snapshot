(**************************************************************************)
(*                                                                        *)
(*  This file is part of WP plug-in of Frama-C.                           *)
(*                                                                        *)
(*  Copyright (C) 2007-2012                                               *)
(*    CEA (Commissariat a l'énergie atomique et aux énergies              *)
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
(* --- Library                                                            --- *)
(* -------------------------------------------------------------------------- *)

let theory = "cint"

let fun_int op = Ctypes.imemo
  (fun i -> Lang.extern_f ~theory ~sort:Logic.Sint "%s_%a" op Ctypes.pp_int i)
let pred_int p = Ctypes.imemo
  (fun i -> Lang.extern_f ~theory ~sort:Logic.Sprop "%s_%a" p Ctypes.pp_int i)

let p_is_int = pred_int "is"  (* is_<cint> : int -> prop *)
let f_to_int = fun_int "to"  (* to_<cint> : int -> int *)
let f_of_real = extern_f ~theory:"qed" ~sort:Logic.Sint "int_of_real"

(* Signature int,int -> int over Z *)
let sort = Logic.Sint
let ac = {
  associative = true ;
  commutative = true ;
  stable = false ;
  neutral = E_none ;
  absorbant = E_none ;
}

let op_lor = { ac with stable = true ; neutral = E_int 0 }
let op_land = { ac with stable = true ; absorbant = E_int 0 }
let f_lnot = Lang.extern_f ~theory ~sort "lnot"
let f_lor  = Lang.extern_f ~theory ~sort ~category:(Operator op_lor) "lor"
let f_land = Lang.extern_f ~theory ~sort ~category:(Operator op_land) "land"
let f_lxor = Lang.extern_f ~theory ~sort ~category:(Operator ac) "lxor"
let f_lsl = Lang.extern_f ~theory ~sort "lsl"
let f_lsr = Lang.extern_f ~theory ~sort "lsr"

let apply2 f x y = e_fun f [x;y]

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

let natural m = Model.set_parameter m model Natural "Mathematic Integers"
let modulo m = Model.set_parameter m model Machine "Machine Integers"

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

(* ACSL Semantics *)
let l_not x = e_fun f_lnot [x]
let l_xor = apply2 f_lxor
let l_or  = apply2 f_lor
let l_and = apply2 f_land

let l_lsl = apply2 f_lsl
let l_lsr = apply2 f_lsr

(* C Code Semantics *)
let bnot i x   = iconvert_unsigned i (l_not x)
let bxor i x y = iconvert_unsigned i (l_xor x y)
let bor _i  = l_or  (* no needs of range conversion *)
let band _i = l_and (* no needs of range conversion *)

let blsl i x y = iconvert i (l_lsl x y)
let blsr _i = l_lsr (* no needs of range conversion *)

(* -------------------------------------------------------------------------- *)
