(**************************************************************************)
(*                                                                        *)
(*  This file is part of WP plug-in of Frama-C.                           *)
(*                                                                        *)
(*  Copyright (C) 2007-2014                                               *)
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

open Ctypes
open Qed
open Lang
open Lang.F

(* -------------------------------------------------------------------------- *)
(* --- Library                                                            --- *)
(* -------------------------------------------------------------------------- *)

let library = "cfloat"

let result = Logic.Real
let params = [Logic.Sreal;Logic.Sreal] (* We can provide more parameters *)
let link f = Lang.infoprover (Qed.Engine.F_call f)

let make_fun_float name f =
  extern_f ~library ~result ~params "%s_%a" name Ctypes.pp_float f
    
let make_pred_float name f =
  extern_f ~library ~result:Logic.Prop ~params "%s_%a" name Ctypes.pp_float f
  
let f_of_int = 
  extern_f ~library:"qed" ~result "real_of_int"

let f_sqrt = 
  extern_f ~library:"cmath" ~result ~params ~link:(link "sqrt") "\\sqrt"

let f_iabs = 
  extern_f ~library:"cmath" ~link:{altergo = Qed.Engine.F_call "abs_int";
                                   why3     = Qed.Engine.F_call "IAbs.abs";
                                   coq      = Qed.Engine.F_call "Z.abs";
                                  } "\\iabs"

let f_rabs = 
  extern_f ~library:"cmath"
    ~result ~params
    ~link:{altergo = Qed.Engine.F_call "abs_real";
           why3     = Qed.Engine.F_call "RAbs.abs";
           coq      = Qed.Engine.F_call "R.abs";
          } "\\rabs"

let f_model = 
  extern_f ~library ~result ~params ~link:(link "model") "\\model"

let f_delta = 
  extern_f ~library ~result ~params ~link:(link "delta") "\\delta"

let f_epsilon = 
  extern_f ~library ~result ~params ~link:(link "epsilon") "\\epsilon"

let () =
  let open LogicBuiltins in
  begin
    LogicBuiltins.add_builtin "\\abs" [Z] f_iabs ;
    LogicBuiltins.add_builtin "\\abs" [R] f_rabs ;
    LogicBuiltins.add_builtin "\\sqrt" [R] f_sqrt ;
  end

(* -------------------------------------------------------------------------- *)
(* --- Model Setting                                                      --- *)
(* -------------------------------------------------------------------------- *)

type model = Real | Float

let model = Context.create ~default:Real "Cfloat.model"
let configure = Context.set model 
  
(* -------------------------------------------------------------------------- *)
(* --- Litterals                                                          --- *)
(* -------------------------------------------------------------------------- *)

let code_lit f =
  match Context.get model with
    | Real  -> e_mthfloat f
    | Float -> e_hexfloat f

let suffixed r_literal =
  let n = String.length r_literal in
  n > 0 && 
  match r_literal.[n-1] 
  with 'f' | 'F' | 'd' | 'D' | 'l' | 'L' -> true | _ -> false

let acsl_lit = 
  let open Cil_types in
  function { r_literal ; r_nearest } ->
    match Context.get model with
      | Float ->
	  if suffixed r_literal
	  then e_hexfloat r_nearest 
	  else e_real (R.of_string r_literal)
      | Real -> 
	  e_mthfloat r_nearest

let round_lit flt r =
  let open Floating_point in
  let p = match flt with
    | Float32 -> single_precision_of_string r
    | Float64 -> double_precision_of_string r
  in e_hexfloat p.f_nearest

(* -------------------------------------------------------------------------- *)
(* --- Maths                                                              --- *)
(* -------------------------------------------------------------------------- *)

let is_lt0 z b = eval_lt  b z
let is_le0 z b = eval_leq b z
let is_eq0 z b = eval_eq  b z

let builtin_positive_eq lfun z a b =
  let open Qed.Logic in
  begin match F.repr a , F.repr b with
    | Fun(f,[_]) , _ when f = lfun && is_lt0 z b -> e_false
    | Fun(f,[a]) , _ when f = lfun && is_eq0 z b -> e_eq a b
    | _ -> raise Not_found
  end

let builtin_positive_leq lfun z a b = 
  let open Qed.Logic in
  begin match F.repr a , F.repr b with
    | Fun(f,[_]) , _ when f = lfun && is_lt0 z b -> e_false
    | Fun(f,[a]) , _ when f = lfun && is_eq0 z b -> e_eq a b
    | _ , Fun(f,[_]) when f = lfun && is_le0 z a -> e_true
    | _ -> raise Not_found
  end

(* -a is \model(m) *)
let is_model a m = F.eval_eq a (e_fun f_model [m])

(* a is \delta(m) *)
let is_delta a m = F.eval_eq a (e_fun f_delta [m])

let builtin_abs f z = function
  | [e] ->
      let open Qed.Logic in
      begin match F.repr e with
        | Times(k,a) -> e_times (Integer.abs k) (e_fun f [a])
        | Kint k -> e_zint (Integer.abs k)
        | Kreal r when Qed.R.negative r -> e_real (Qed.R.opp r)
        | Add [a;m] when is_model (e_opp a) m -> e_fun f_delta [m]
        | Add [m;a] when is_model (e_opp a) m -> e_fun f_delta [m]
        | Div (a,m) when is_delta a m -> e_fun f_epsilon [m]
        | _ -> 
            match is_true (e_leq z e) with
            | Yes -> e
            | No -> e_opp e
            | Maybe -> raise Not_found
      end
  | _ -> raise Not_found

let builtin_sqrt = function
  | [e] ->
      let open Qed.Logic in
      begin match F.repr e with
        | Mul[a;b] when eval_eq a b -> e_fun f_rabs [a] (* a is smaller *)
        | _ when is_eq0 e_zero_real e -> e_zero_real
        | _ when is_eq0 e_zero e -> e_zero
        | _ -> raise Not_found
      end
  | _ -> raise Not_found


let () = 
  begin 
    F.set_builtin     f_iabs (builtin_abs f_iabs e_zero) ;
    F.set_builtin     f_rabs (builtin_abs f_rabs e_zero_real) ;
    F.set_builtin     f_sqrt  builtin_sqrt ;
    F.set_builtin_eq  f_iabs (builtin_positive_eq  f_iabs e_zero) ;
    F.set_builtin_eq  f_rabs (builtin_positive_eq  f_rabs e_zero_real) ;
    F.set_builtin_eq  f_sqrt (builtin_positive_eq  f_sqrt e_zero_real) ;
    F.set_builtin_leq f_iabs (builtin_positive_leq f_iabs e_zero) ;
    F.set_builtin_leq f_rabs (builtin_positive_leq f_rabs e_zero_real) ;
    F.set_builtin_leq f_sqrt (builtin_positive_leq f_sqrt e_zero_real) ;
  end

(* -------------------------------------------------------------------------- *)
(* --- Operators                                                          --- *)
(* -------------------------------------------------------------------------- *)

let flt_rnd  = Ctypes.fmemo (make_fun_float "to")
let flt_add  = Ctypes.fmemo (make_fun_float "add")
let flt_mul  = Ctypes.fmemo (make_fun_float "mul")
let flt_div  = Ctypes.fmemo (make_fun_float "div")
let flt_sqrt = Ctypes.fmemo (make_fun_float "sqrt")

let () = let open LogicBuiltins in
  begin
    add_builtin "\\model" [F Float32] f_model ;
    add_builtin "\\model" [F Float64] f_model ;
    add_builtin "\\delta" [F Float32] f_model ;
    add_builtin "\\delta" [F Float64] f_model ;
    add_builtin "\\epsilon" [F Float32] f_model ;
    add_builtin "\\epsilon" [F Float64] f_model ;
  end

(* -------------------------------------------------------------------------- *)
(* --- Precision                                                          --- *)
(* -------------------------------------------------------------------------- *)

module OP = FCMap.Make(Lang.Fun)

let models : (term list -> term) OP.t ref = ref OP.empty
let add_model f op = models := OP.add f op !models
let add_fmodel fop op =
  models := OP.add (fop Float32) op (OP.add (fop Float64) op !models)

let builtin_model = function
  | [e] ->
      let open Qed.Logic in
      begin match F.repr e with
        | Fun(f,_) when f = f_model -> e
        | Fun(f,_) when f = f_delta -> e_zero_real
        | Fun(f,_) when f = f_epsilon -> e_zero_real
        | Fun(op,xs) -> 
            let phi = OP.find op !models in
            (* find phi before computing arguments *)
            phi (List.map (fun e -> e_fun f_model [e]) xs)
        | Kreal _ -> e
        | _ -> raise Not_found
      end
  | _ -> raise Not_found

let builtin_round f = function
  | [e] ->
      let open Qed.Logic in
      begin match F.repr e with
        | Div(x,y) -> e_fun (flt_div f) [x;y]
        | Add ([_;_] as xs) -> e_fun (flt_add f) xs
        | Mul ([_;_] as xs) -> e_fun (flt_mul f) xs
        | Fun(s,([_] as xs)) when s = f_sqrt -> e_fun (flt_sqrt f) xs
        | Kreal r -> 
            begin match R.to_string r with
              | "0.0" | "1.0" | "-1.0" -> e
              | lit -> round_lit f lit
            end
        | _ -> raise Not_found
      end
  | _ -> raise Not_found

let builtin_error = function
  | [e] ->
      let open Qed.Logic in
      begin match F.repr e with
        | Fun(f,_) when f = f_model -> e_zero_real
        | _ -> raise Not_found
      end
  | _ -> raise Not_found

let () =
  begin
    add_fmodel flt_rnd (function  [x] -> x | _ -> raise Not_found) ;
    add_fmodel flt_add e_sum ;  (* only 2 params in flt_add *)
    add_fmodel flt_mul e_prod ; (* only 2 params in flt_mul *)
    add_fmodel flt_div (function [x;y] -> e_div x y | _ -> raise Not_found) ;
    add_fmodel flt_sqrt (e_fun f_sqrt) (* only 1 param *) ;
    F.set_builtin f_model builtin_model ;
    F.set_builtin f_delta builtin_error ;
    F.set_builtin f_epsilon builtin_error ;
    F.set_builtin (flt_rnd Float32) (builtin_round Float32) ;
    F.set_builtin (flt_rnd Float64) (builtin_round Float64) ;
  end

(* -------------------------------------------------------------------------- *)
(* --- Conversion Symbols                                                 --- *)
(* -------------------------------------------------------------------------- *)

let fconvert =
  fun f a ->
    match Context.get model with
    | Real -> a
    | Float -> e_fun (flt_rnd f) [a]

let real_of_int a = e_fun f_of_int [a]
let float_of_int f a = fconvert f (real_of_int a)
    
let frange =
  let is_float = Ctypes.fmemo (make_pred_float "is") in
  fun f a -> p_call (is_float f) [a]
               
(* -------------------------------------------------------------------------- *)
(* --- Float Arithmetics                                                  --- *)
(* -------------------------------------------------------------------------- *)
      
let runop op f x =
  match Context.get model with
    | Real -> op x
    | Float -> e_fun f [x]

let rbinop op f x y =
  match Context.get model with
    | Real -> op x y
    | Float -> e_fun f [x;y]

let funop op f x = fconvert f (op x)

let fbinop rop fop f x y =
  match Context.get model with
  | Real -> rop x y
  | Float -> e_fun (fop f) [x;y]

let fadd = fbinop e_add flt_add
let fmul = fbinop e_mul flt_mul
let fdiv = fbinop e_div flt_div

let fopp _ = e_opp (* sign change is exact in floats *)
let fsub f x y = fadd f x (e_opp y)

(* -------------------------------------------------------------------------- *)
(* --- Float Simplifiers                                                  --- *)
(* -------------------------------------------------------------------------- *)

let compute_f_of_int = function
  | [e] ->
      begin
	match F.repr e with
	  | Qed.Logic.Kint k -> 
	      let m = Z.to_string k in
	      let r = R.of_string (m ^ ".") in
	      F.e_real r
	  | _ -> raise Not_found
      end
  | _ -> raise Not_found

let () =
  F.set_builtin f_of_int compute_f_of_int ;

(* -------------------------------------------------------------------------- *)
