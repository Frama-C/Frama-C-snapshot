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
(* --- Floats Arithmetic Model                                            --- *)
(* -------------------------------------------------------------------------- *)

open Qed
open Lang
open Lang.F

(* -------------------------------------------------------------------------- *)
(* --- Library                                                            --- *)
(* -------------------------------------------------------------------------- *)

let theory = "cfloat"

let result = Logic.Sreal
let params = [Logic.Sreal]
let binop = [Logic.Sreal;Logic.Sreal]

let make_fun_float name = Ctypes.fmemo
  (fun f -> extern_f ~theory ~result ~params "%s_%a" name Ctypes.pp_float f)

let make_pred_float name = Ctypes.fmemo
  (fun f -> extern_f ~theory ~result ~params "%s_%a" name Ctypes.pp_float f)
  
let f_of_int = extern_f ~theory:"qed" ~result "real_of_int"
let r_opp = extern_f ~theory ~result ~params "ropp"
let r_add = extern_f ~theory ~result ~params:binop "radd"
let r_sub = extern_f ~theory ~result ~params:binop "rsub"
let r_mul = extern_f ~theory ~result ~params:binop "rmul"
let r_div = extern_f ~theory ~result ~params:binop "rdiv"

let apply2 f x y = e_fun f [x;y]
  
(* -------------------------------------------------------------------------- *)
(* --- Model Setting                                                      --- *)
(* -------------------------------------------------------------------------- *)

type model = Real | Float

let model = Context.create ~default:Real "Cfloat.model"
  
(* -------------------------------------------------------------------------- *)
(* --- Litterals                                                          --- *)
(* -------------------------------------------------------------------------- *)

let code_lit f =
  match Context.get model with
    | Real  -> e_mthfloat f
    | Float -> e_hexfloat f

let acsl_lit = 
  let open Cil_types in
  function { r_literal ; r_nearest } ->
    match Context.get model with
      | Float ->
	  let n = String.length r_literal in
	  let suffixed = n > 0 && 
	    match r_literal.[n-1] 
	    with 'f' | 'F' | 'd' | 'D' | 'l' | 'L' -> true | _ -> false
	  in 
	  if suffixed 
	  then e_hexfloat r_nearest 
	  else e_real (R.of_string r_literal)
      | Real -> 
	  e_mthfloat r_nearest

(* -------------------------------------------------------------------------- *)
(* --- Conversion Symbols                                                 --- *)
(* -------------------------------------------------------------------------- *)

let fconvert f a = 
  match Context.get model with
    | Real -> a
    | Float -> e_fun (make_fun_float "to" f) [a]

let real_of_int a = e_fun f_of_int [a]
let float_of_int f a = fconvert f (real_of_int a)

let frange f a = p_call (make_pred_float "is" f) [a]

let runop op f x =
  match Context.get model with
    | Real -> op x
    | Float -> e_fun f [x]

let rbinop op f x y =
  match Context.get model with
    | Real -> op x y
    | Float -> e_fun f [x;y]

let funop op f x = fconvert f (op x)

let fbinop op name f x y = 
  match Context.get model with
    | Real -> op x y
    | Float -> e_fun (make_fun_float name f) [x;y]

(* -------------------------------------------------------------------------- *)
(* --- Real Arithmetics                                                  --- *)
(* -------------------------------------------------------------------------- *)

let ropp = runop e_opp r_opp
let radd = rbinop e_add r_add
let rsub = rbinop e_sub r_sub
let rmul = rbinop e_mul r_mul
let rdiv = rbinop e_div r_div

(* -------------------------------------------------------------------------- *)
(* --- Float Arithmetics                                                  --- *)
(* -------------------------------------------------------------------------- *)

let fopp = funop e_opp
let fadd = fbinop e_add "add"
let fsub = fbinop e_sub "sub"
let fmul = fbinop e_mul "mul"
let fdiv = fbinop e_div "div"

(* -------------------------------------------------------------------------- *)
(* --- Float Simplifiers                                                  --- *)
(* -------------------------------------------------------------------------- *)

let compute_f_of_int = function
  | [e] ->
      begin
	match F.repr e with
	  | Qed.Logic.Kint k -> 
	      let m = Z.to_string k in
	      let r = R.of_string (m ^ ".0") in
	      F.e_real r
	  | _ -> raise Not_found
      end
  | _ -> raise Not_found
  
let compute_r_opp = function
  | [e] ->
      begin
	match F.repr e with
	  | Qed.Logic.Kreal r -> 
	      let r = R.to_string r in
	      let s =
		if r.[0] = '-' 
		then String.sub r 1 (String.length r -1)
		else "-" ^ r
	      in e_real (R.of_string s)
	  | _ -> raise Not_found
      end
  | _ -> raise Not_found

let () =
  begin
    F.add_builtin f_of_int compute_f_of_int ;
    F.add_builtin r_opp compute_r_opp ;
  end

(* -------------------------------------------------------------------------- *)
