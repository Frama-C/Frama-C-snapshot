(**************************************************************************)
(*                                                                        *)
(*  This file is part of WP plug-in of Frama-C.                           *)
(*                                                                        *)
(*  Copyright (C) 2007-2019                                               *)
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

let f32 = datatype ~library "f32"
let f64 = datatype ~library "f64"

let t32 = Lang.(t_datatype f32 [])
let t64 = Lang.(t_datatype f64 [])

let ftau = function
  | Float32 -> t32
  | Float64 -> t64

let ft_suffix = function Float32 -> "f32" | Float64 -> "f64"
let pp_suffix fmt ft = Format.pp_print_string fmt (ft_suffix ft)

let link phi = Lang.infoprover (Qed.Engine.F_call phi)

(* Qed exact representations, linked to f32/f64 *)
let fq32 = extern_f ~library ~result:t32 ~link:(link "to_f32") "q32"
let fq64 = extern_f ~library ~result:t64 ~link:(link "to_f64") "q64"

let f_model ft = extern_f ~library ~result:(ftau ft) "model_%a" pp_suffix ft
let f_delta ft = extern_f ~library ~result:(ftau ft) "delta_%a" pp_suffix ft
let f_epsilon ft = extern_f ~library ~result:(ftau ft) "epsilon_%a" pp_suffix ft

(* -------------------------------------------------------------------------- *)
(* --- Model Setting                                                      --- *)
(* -------------------------------------------------------------------------- *)

type model = Real | Float

let model = Context.create ~default:Float "Cfloat.model"

let tau_of_float f =
  match Context.get model with
  | Real -> Logic.Real
  | Float -> ftau f

(* -------------------------------------------------------------------------- *)
(* --- Operators                                                          --- *)
(* -------------------------------------------------------------------------- *)

type op =
  | LT
  | EQ
  | LE
  | NE
  | NEG
  | ADD
  | MUL
  | DIV
  | REAL
  | ROUND
  | EXACT

[@@@ warning "-32"]
let op_name = function
  | LT -> "flt"
  | EQ -> "feq"
  | LE -> "fle"
  | NE -> "fne"
  | NEG -> "fneg"
  | ADD -> "fadd"
  | MUL -> "fmul"
  | DIV -> "fdiv"
  | REAL -> "freal"
  | ROUND -> "fround"
  | EXACT -> "fexact"
[@@@ warning "+32"]

(* -------------------------------------------------------------------------- *)
(* --- Registry                                                           --- *)
(* -------------------------------------------------------------------------- *)

module REGISTRY = WpContext.Static
    (struct
      type key = lfun
      type data = op * c_float
      let name = "Wp.Cfloat.REGISTRY"
      include Lang.Fun
    end)

let find = REGISTRY.find

let () = Context.register
    begin fun () ->
      REGISTRY.define fq32 (EXACT,Float32) ;
      REGISTRY.define fq64 (EXACT,Float64) ;
    end

(* -------------------------------------------------------------------------- *)
(* --- Literals                                                           --- *)
(* -------------------------------------------------------------------------- *)

let rfloat = Floating_point.round_to_single_precision_float

let fmake ulp value = match ulp with
  | Float32 -> F.e_fun fq32 [F.e_float (rfloat value)]
  | Float64 -> F.e_fun fq64 [F.e_float value]

let qmake ulp q = fmake ulp (Transitioning.Q.to_float q)
let re_mantissa = "\\([-+]?[0-9]*\\)"
let re_comma = "\\(.\\(\\(0*[1-9]\\)*\\)0*\\)?"
let re_exponent = "\\([eE]\\([-+]?[0-9]*\\)\\)?"
let re_suffix = "\\([flFL]\\)?"
let re_real =
  Str.regexp (re_mantissa ^ re_comma ^ re_exponent ^ re_suffix ^ "$")

let parse_literal ~model v r =
  try
    if Str.string_match re_real r 0 then
      let has_suffix =
        try ignore (Str.matched_group 7 r) ; true
        with Not_found -> false in
      if has_suffix && model = Float then
        Q.of_float v
      else
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
    else Q.of_float v
  with Failure _ ->
    Warning.error "Unexpected constant literal %S" r

let acsl_lit l =
  let open Cil_types in
  F.e_real (parse_literal ~model:(Context.get model) l.r_nearest l.r_literal)

let code_lit ulp value original =
  match Context.get model , ulp , original with
  | Float , Float32 , _ -> F.e_fun fq32 [F.e_float value]
  | Float , Float64 , _ -> F.e_fun fq64 [F.e_float value]
  | Real , _ , None -> F.e_float value
  | Real , _ , Some r -> F.e_real (parse_literal ~model:Real value r)

(* -------------------------------------------------------------------------- *)
(* --- Literal Output                                                     --- *)
(* -------------------------------------------------------------------------- *)

let printers = [
  Printf.sprintf "%.0g" ;
  Printf.sprintf "%.1g" ;
  Printf.sprintf "%.2g" ;
  Printf.sprintf "%.3g" ;
  Printf.sprintf "%.4g" ;
  Printf.sprintf "%.5g" ;
  Printf.sprintf "%.6g" ;
  Printf.sprintf "%.9g" ;
  Printf.sprintf "%.12g" ;
  Printf.sprintf "%.15g" ;
  Printf.sprintf "%.18g" ;
  Printf.sprintf "%.21g" ;
  Printf.sprintf "%.32g" ;
  Printf.sprintf "%.64g" ;
]

let re_int = Str.regexp "[0-9]+"

let force_float r =
  if Str.string_match re_int r 0 &&
     Str.match_end () = String.length r
  then (r ^ ".0") else r

let float_lit ulp (q : Q.t) =
  let v = match ulp with
    | Float32 -> rfloat @@ Transitioning.Q.to_float q
    | Float64 -> Transitioning.Q.to_float q in
  let reparse ulp r =
    match ulp with
    | Float32 -> rfloat @@ float_of_string r
    | Float64 -> float_of_string r
  in
  let rec lookup ulp v = function
    | [] -> Pretty_utils.to_string Floating_point.pretty v
    | pp::pps ->
        let r = force_float @@ pp v in
        if reparse ulp r = v then r else
          lookup ulp v pps
  in lookup ulp v printers

(* -------------------------------------------------------------------------- *)
(* --- Finites                                                            --- *)
(* -------------------------------------------------------------------------- *)

let fclass value _args =
  match Context.get model with
  | Real -> F.e_bool value
  | Float -> raise Not_found

let () = Context.register
    begin fun () ->
      LogicBuiltins.hack "\\is_finite"         (fclass true) ;
      LogicBuiltins.hack "\\is_NaN"            (fclass false) ;
      LogicBuiltins.hack "\\is_infinite"       (fclass false) ;
      LogicBuiltins.hack "\\is_plus_infinity"  (fclass false) ;
      LogicBuiltins.hack "\\is_minus_infinity" (fclass false) ;
    end

(* -------------------------------------------------------------------------- *)
(* --- Computations                                                       --- *)
(* -------------------------------------------------------------------------- *)

let rec exact e =
  match F.repr e with
  | Qed.Logic.Kreal r -> r
  | Qed.Logic.Kint z -> Q.of_bigint z
  | Qed.Logic.Fun( f , [ q ] ) when f == fq32 || f == fq64 -> exact q
  | _ -> raise Not_found

let round ulp e =
  match F.repr e with
  | Qed.Logic.Fun( f , [ b ] ) ->
      begin
        match find f with
        | REAL , ulp2 when ulp2 = ulp -> b
        | _ -> qmake ulp (exact e )
      end
  | _ -> qmake ulp (exact e)

let compute_float op ulp xs =
  match op , xs with
  | NEG , [ x ] -> qmake ulp (Q.neg (exact x))
  | ADD , [ x ; y ] -> qmake ulp (Q.add (exact x) (exact y))
  | MUL , [ x ; y ] -> qmake ulp (Q.mul (exact x) (exact y))
  | DIV , [ x ; y ] -> qmake ulp (Q.div (exact x) (exact y))
  | ROUND , [ x ] -> round ulp x
  | REAL , [ x ] -> F.e_real (exact x)
  | LE , [ x ; y ] -> F.e_bool (Q.leq (exact x) (exact y))
  | LT , [ x ; y ] -> F.e_bool (Q.lt (exact x) (exact y))
  | EQ , [ x ; y ] -> F.e_bool (Q.equal (exact x) (exact y))
  | NE , [ x ; y ] -> F.e_bool (not (Q.equal (exact x) (exact y)))
  | _ -> raise Not_found

let compute_real op xs =
  match op , xs with
  | NEG , [ x ] -> F.e_opp x
  | ADD , [ x ; y ] -> F.e_add x y
  | MUL , [ x ; y ] -> F.e_mul x y
  | DIV , [ x ; y ] -> F.e_div x y
  | (ROUND|REAL) , [ x ] -> x
  | LE , [ x ; y ] -> F.e_leq x y
  | LT , [ x ; y ] -> F.e_lt x y
  | EQ , [ x ; y ] -> F.e_eq x y
  | NE , [ x ; y ] -> F.e_neq x y
  | _ -> raise Not_found

let compute op ulp xs =
  match Context.get model with
  | Real -> compute_real op xs
  | Float -> compute_float op ulp xs

(* -------------------------------------------------------------------------- *)
(* --- Operations                                                         --- *)
(* -------------------------------------------------------------------------- *)

let make_fun_float ?result name op ft =
  let result = match result with None -> ftau ft | Some r -> r in
  let phi = extern_f ~library ~result "%s_%a" name pp_suffix ft in
  Lang.F.set_builtin phi (compute op ft) ;
  REGISTRY.define phi (op,ft) ; phi

let make_pred_float name op ft =
  let prop = Pretty_utils.sfprintf "%s_%a" name pp_suffix ft in
  let bool = Pretty_utils.sfprintf "%s_%ab" name pp_suffix ft in
  let phi = extern_p ~library ~bool ~prop () in
  Lang.F.set_builtin phi (compute op ft) ;
  REGISTRY.define phi (op,ft) ; phi

let f_memo = Ctypes.f_memo

let real_of_flt  = f_memo (make_fun_float ~result:Logic.Real "of" REAL)
let flt_of_real  = f_memo (make_fun_float "to" ROUND)
let flt_add  = f_memo (make_fun_float "add" ADD)
let flt_mul  = f_memo (make_fun_float "mul" MUL)
let flt_div  = f_memo (make_fun_float "div" DIV)
let flt_neg  = f_memo (make_fun_float "neg" NEG)

let flt_lt = f_memo (make_pred_float "lt" LT)
let flt_eq = f_memo (make_pred_float "eq" EQ)
let flt_le = f_memo (make_pred_float "le" LE)
let flt_neq = f_memo (make_pred_float "ne" NE)

(* -------------------------------------------------------------------------- *)
(* --- Builtins                                                           --- *)
(* -------------------------------------------------------------------------- *)

let register_builtin_comparison suffix ft =
  begin
    let open Qed.Logic in
    let params = [Sdata;Sdata] in
    let sort = Sprop in
    let gt = generated_f ~params ~sort "\\gt_%s" suffix in
    let ge = generated_f ~params ~sort "\\ge_%s" suffix in
    let open LogicBuiltins in
    let signature = [F ft;F ft] in
    add_builtin ("\\eq_" ^ suffix) signature (flt_eq ft) ;
    add_builtin ("\\ne_" ^ suffix) signature (flt_neq ft) ;
    add_builtin ("\\lt_" ^ suffix) signature (flt_lt ft) ;
    add_builtin ("\\le_" ^ suffix) signature (flt_le ft) ;
    add_builtin ("\\gt_" ^ suffix) signature gt ;
    add_builtin ("\\ge_" ^ suffix) signature ge ;
    let converse phi x y = e_fun phi [y;x] in
    Lang.F.set_builtin_2 gt (converse (flt_lt ft)) ;
    Lang.F.set_builtin_2 ge (converse (flt_le ft)) ;
  end

let () =
  Context.register
    begin fun () ->
      register_builtin_comparison "float" Float32 ;
      register_builtin_comparison "double" Float64 ;
    end

(* -------------------------------------------------------------------------- *)
(* --- Models                                                             --- *)
(* -------------------------------------------------------------------------- *)

let () =
  Context.register
    begin fun () ->
      let open LogicBuiltins in
      let register_builtin ft =
        add_builtin "\\model" [F ft] (f_model ft) ;
        add_builtin "\\delta" [F ft] (f_delta ft) ;
        add_builtin "\\epsilon" [F ft] (f_epsilon ft) ;
      in
      register_builtin Float32 ;
      register_builtin Float64 ;
    end

(* -------------------------------------------------------------------------- *)
(* --- Conversion Symbols                                                 --- *)
(* -------------------------------------------------------------------------- *)

let real_of_float f a =
  match Context.get model with
  | Real -> a
  | Float -> e_fun (real_of_flt f) [a]

let float_of_real f a =
  match Context.get model with
  | Real -> a
  | Float -> e_fun (flt_of_real f) [a]

let float_of_int f a = float_of_real f (Cmath.real_of_int a)

(* -------------------------------------------------------------------------- *)
(* --- Float Arithmetics                                                  --- *)
(* -------------------------------------------------------------------------- *)

let fbinop rop fop f x y =
  match Context.get model with
  | Real -> rop x y
  | Float -> e_fun (fop f) [x;y]

let fcmp rop fop f x y =
  match Context.get model with
  | Real -> rop x y
  | Float -> p_call (fop f) [x;y]

let fadd = fbinop e_add flt_add
let fmul = fbinop e_mul flt_mul
let fdiv = fbinop e_div flt_div

let fopp f x =
  match Context.get model with
  | Real -> e_opp x
  | Float -> e_fun (flt_neg f) [x]

let fsub f x y = fadd f x (fopp f y)

let flt = fcmp p_lt flt_lt
let fle = fcmp p_leq flt_le
let feq = fcmp p_equal flt_eq
let fneq = fcmp p_neq flt_neq

(* -------------------------------------------------------------------------- *)
(* --- Registry                                                           --- *)
(* -------------------------------------------------------------------------- *)

let configure m =
  begin
    Context.set model m ;
    Context.set Lang.floats tau_of_float ;
  end

(* -------------------------------------------------------------------------- *)
