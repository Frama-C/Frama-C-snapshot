(**************************************************************************)
(*                                                                        *)
(*  This file is part of WP plug-in of Frama-C.                           *)
(*                                                                        *)
(*  Copyright (C) 2007-2018                                               *)
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

let f_model =
  extern_f ~library ~result ~params ~link:(link "model") "\\model"

let f_delta =
  extern_f ~library ~result ~params ~link:(link "delta") "\\delta"

let f_epsilon =
  extern_f ~library ~result ~params ~link:(link "epsilon") "\\epsilon"

(* -------------------------------------------------------------------------- *)
(* --- Model Setting                                                      --- *)
(* -------------------------------------------------------------------------- *)

type model = Real | Float

let model = Context.create ~default:Real "Cfloat.model"

(* -------------------------------------------------------------------------- *)
(* --- Literals                                                          --- *)
(* -------------------------------------------------------------------------- *)

let code_lit = F.e_float

let mantissa = "\\([-+]?[0-9]*\\)"
let comma = "\\(.\\(\\(0*[1-9]\\)*\\)0*\\)?"
let exponent = "\\([eE]\\([-+]?[0-9]*\\)\\)?"
let real = Str.regexp (mantissa ^ comma ^ exponent ^ "$")

let real_of_literal l =
  let open Cil_types in
  let r = l.r_literal in
  if Str.string_match real r 0 then
    let ma = Str.matched_group 1 r in
    let mb = try Str.matched_group 3 r with Not_found -> "" in
    let me = try Str.matched_group 6 r with Not_found -> "0" in
    let n = int_of_string me - String.length mb in
    let d n =
      let s = Bytes.make (succ n) '0' in
      Bytes.set s 0 '1' ; Q.of_string (Bytes.to_string s) in
    let m = Q.of_string (ma ^ mb) in
    if n < 0 then Q.div m (d (-n)) else
    if n > 0 then Q.mul m (d n) else m
  else Q.of_float l.r_nearest

let acsl_lit l = F.e_real (real_of_literal l)

(* -------------------------------------------------------------------------- *)
(* --- Operators                                                          --- *)
(* -------------------------------------------------------------------------- *)

let flt_rnd  = Ctypes.f_memo (make_fun_float "to")
let flt_add  = Ctypes.f_memo (make_fun_float "add")
let flt_mul  = Ctypes.f_memo (make_fun_float "mul")
let flt_div  = Ctypes.f_memo (make_fun_float "div")

let () =
  begin
    let open LogicBuiltins in
    add_builtin "\\model" [F Float32] f_model ;
    add_builtin "\\model" [F Float64] f_model ;
    add_builtin "\\delta" [F Float32] f_delta ;
    add_builtin "\\delta" [F Float64] f_delta ;
    add_builtin "\\epsilon" [F Float32] f_epsilon ;
    add_builtin "\\epsilon" [F Float64] f_epsilon ;
  end

(* -------------------------------------------------------------------------- *)
(* --- Floating Point Predicate                                           --- *)
(* -------------------------------------------------------------------------- *)

let fle   _ = F.p_leq
let flt   _ = F.p_lt
let feq   _ = F.p_equal
let fneq  _ = F.p_neq

(* -------------------------------------------------------------------------- *)
(* --- Precision                                                          --- *)
(* -------------------------------------------------------------------------- *)

module OP = Model.Static
    (struct
      type key = Lang.lfun
      type data = (term list -> term)
      let name = "Wp.Cfloat.OP"
      let compare = Lang.Fun.compare
      let pretty = Lang.Fun.pretty
    end)

let define_fmodel_of fop op =
  begin
    OP.define (fop Float32) op ;
    OP.define (fop Float64) op ;
  end

let builtin_model = function
  | [e] ->
      let open Qed.Logic in
      begin match F.repr e with
        | Fun(f,_) when f == f_model -> e
        | Fun(f,_) when f == f_delta -> e_zero_real
        | Fun(f,_) when f == f_epsilon -> e_zero_real
        | Fun(op,xs) ->
            let phi = OP.find op in
            (* find phi before computing arguments *)
            phi (List.map (fun e -> e_fun f_model [e]) xs)
        | Kreal _ -> e
        | _ -> raise Not_found
      end
  | _ -> raise Not_found

let builtin_round ulp = function
  | [e] ->
      let open Qed.Logic in
      begin match F.repr e with
        | Div(x,y) -> e_fun (flt_div ulp) [x;y]
        | Add ([_;_] as xs) -> e_fun (flt_add ulp) xs
        | Mul ([_;_] as xs) -> e_fun (flt_mul ulp) xs
        | Kreal r when Q.equal r Q.zero -> e
        | Kreal r when Q.equal r Q.one -> e
        | Kreal r ->
            let flt = Transitioning.Q.to_float r in
            let rnd =
              match ulp with
              | Float32 -> Floating_point.round_to_single_precision_float flt
              | Float64 -> flt
            in F.e_float rnd
        | _ -> raise Not_found
      end
  | _ -> raise Not_found

let builtin_error = function
  | [e] ->
      let open Qed.Logic in
      begin match F.repr e with
        | Fun(f,_) when f == f_model -> e_zero_real
        | _ -> raise Not_found
      end
  | _ -> raise Not_found

(* -------------------------------------------------------------------------- *)
(* --- Conversion Symbols                                                 --- *)
(* -------------------------------------------------------------------------- *)

let float_of_real f a =
    match Context.get model with
    | Real -> a
    | Float -> e_fun (flt_rnd f) [a]

let float_of_int f a = float_of_real f (Cmath.real_of_int a)
let real_of_float _f a = a

let range =
  let is_float = Ctypes.f_memo (make_pred_float "is") in
  fun f a -> p_call (is_float f) [a]

(* -------------------------------------------------------------------------- *)
(* --- Float Arithmetics                                                  --- *)
(* -------------------------------------------------------------------------- *)

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
(* --- Registry                                                           --- *)
(* -------------------------------------------------------------------------- *)

let () = Context.register
    begin fun () ->

      F.set_builtin f_model builtin_model ;
      F.set_builtin f_delta builtin_error ;
      F.set_builtin f_epsilon builtin_error ;
      F.set_builtin (flt_rnd Float32) (builtin_round Float32) ;
      F.set_builtin (flt_rnd Float64) (builtin_round Float64) ;

      define_fmodel_of flt_rnd (function  [x] -> x | _ -> raise Not_found) ;
      define_fmodel_of flt_add e_sum ;  (* only 2 params in flt_add *)
      define_fmodel_of flt_mul e_prod ; (* only 2 params in flt_mul *)
      define_fmodel_of flt_div (function [x;y] -> e_div x y | _ -> raise Not_found) ;
    end

let configure m = Context.set model m

(* -------------------------------------------------------------------------- *)
