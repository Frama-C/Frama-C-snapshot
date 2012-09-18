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
(* --- Floats Arithmetic Model                                            --- *)
(* -------------------------------------------------------------------------- *)

open Qed
open Lang
open Lang.F

(* -------------------------------------------------------------------------- *)
(* --- Library                                                            --- *)
(* -------------------------------------------------------------------------- *)

let theory = "cfloat"

let fun_float = Ctypes.fmemo
  (fun f -> extern_f ~theory ~sort:Logic.Sreal "to_%a" Ctypes.pp_float f)

let f_of_int = extern_f ~theory:"qed" ~sort:Logic.Sreal "real_of_int"

(* Signature real,real -> {0,1} *)
let f_feq = extern_f ~theory ~sort:Logic.Sreal "feq"
let f_flt = extern_f ~theory ~sort:Logic.Sreal "flt"
let f_fneq = extern_f ~theory ~sort:Logic.Sreal "fneq"
let f_fleq = extern_f ~theory ~sort:Logic.Sreal "fleq"

let apply2 f x y = e_fun f [x;y]
  
(* -------------------------------------------------------------------------- *)
(* --- Conversion Symbols                                                 --- *)
(* -------------------------------------------------------------------------- *)

type model = Real | Float

let model = Context.create ~default:Real "Cfloat.model"
let real m = Model.set_parameter m model Real "Floats as Real"
let machine m = Model.set_parameter m model Float "Machine floats"

let fconvert f a = 
  match Context.get model with 
    | Real -> a | Float -> e_fun (fun_float f) [a]

let of_int f a = fconvert f (e_fun f_of_int [a])

let fbinop op f x y = fconvert f (op x y) 
let funop op f x = fconvert f (op x)

(* -------------------------------------------------------------------------- *)
(* --- Arithmetics                                                        --- *)
(* -------------------------------------------------------------------------- *)

let fopp = funop e_opp
let fadd = fbinop e_add
let fsub = fbinop e_sub
let fmul = fbinop e_mul  
let fdiv = fbinop e_div

(* -------------------------------------------------------------------------- *)
(* --- Comparison-to-int                                                  --- *)
(* -------------------------------------------------------------------------- *)

let feq = apply2 (f_feq)
let flt = apply2 (f_flt)
let fneq = apply2 (f_fneq)
let fleq = apply2 (f_fleq)

(* -------------------------------------------------------------------------- *)
