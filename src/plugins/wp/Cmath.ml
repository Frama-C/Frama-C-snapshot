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

open Qed
open Logic
open Lang
open Lang.F

let f_builtin ~library ?(injective=false) ?(result=Real) ?(params=[Real]) ?ext name =
  assert (name.[0] == '\\') ;
  let call =
    match ext with Some call -> call | None ->
      String.sub name 1 (String.length name - 1) in
  let link = Lang.infoprover (Engine.F_call call) in
  let category =
    let open Qed.Logic in
    if injective then Injection else Function
  in
  let signature = List.map LogicBuiltins.kind_of_tau params in
  let params = List.map Kind.of_tau params in
  let lfun = extern_s ~library ~category ~result ~params ~link name in
  LogicBuiltins.(add_builtin name signature lfun) ; lfun

(* -------------------------------------------------------------------------- *)
(* --- Real Of Int                                                        --- *)
(* -------------------------------------------------------------------------- *)

let f_real_of_int =
  extern_f ~library:"qed"
    ~category:Qed.Logic.Injection
    ~result:Logic.Real ~params:[Logic.Sint] "real_of_int"

let builtin_real_of_int e =
  match F.repr e with
  | Qed.Logic.Kint k -> F.e_real (Q.of_bigint k)
  | _ -> raise Not_found

(* -------------------------------------------------------------------------- *)
(* --- Truncate                                                           --- *)
(* -------------------------------------------------------------------------- *)

let f_truncate = f_builtin ~library:"truncate" ~result:Int "\\truncate"
let f_ceil = f_builtin ~library:"truncate" ~result:Int "\\ceil"
let f_floor = f_builtin ~library:"truncate" ~result:Int "\\floor"

let builtin_truncate f e =
  let open Qed.Logic in
  match F.repr e with
  | Kint _ -> e
  | Kreal r when Q.(equal r zero) -> e_zero
  | Kreal r ->
      begin
        try
          (* Waiting for Z-Arith to have truncation to big-int *)
          let truncated = int_of_float (Transitioning.Q.to_float r) in
          let reversed = Q.of_int truncated in
          let base = F.e_int truncated in
          if Q.equal r reversed then base else
          if f == f_ceil && Q.(lt zero r) then F.(e_add base e_one) else
          if f == f_floor && Q.(lt r zero) then F.(e_sub base e_one) else
            base
        with _ -> raise Not_found
      end
  | Fun( f , [e] ) when f == f_real_of_int -> e
  | _ -> raise Not_found

(* -------------------------------------------------------------------------- *)
(* --- Conversions                                                        --- *)
(* -------------------------------------------------------------------------- *)

let int_of_real x = e_fun f_truncate [x]
let real_of_int x = e_fun f_real_of_int [x]

let int_of_bool a = e_neq a F.e_zero (* if a != 0 then true else false *)
let bool_of_int a = e_if a F.e_one F.e_zero (* if a then 1 else 0 *)

(* -------------------------------------------------------------------------- *)
(* --- Sign                                                               --- *)
(* -------------------------------------------------------------------------- *)

(* rewrite a=b when a or b is f(x)
   for functions f such as 0 <= f(x) && ( f(x) = 0 <-> x = 0 ) *)
let builtin_positive_eq lfun ~domain ~zero ~injective a b =
  let open Qed.Logic in
  begin match F.repr a , F.repr b with
    | Fun(f,[a]) , Fun(f',[b])
      when injective && f == lfun && f' == lfun && domain a && domain b ->
        (* injective a in domain && b in domain -> ( f(a) = f(b) <-> a = b ) *)
        e_eq a b
    | Fun(f,[a]) , _ when f == lfun && domain a ->
        if QED.eval_lt b zero then
          (* a in domain && b < 0 -> ( f(a) = b <-> false ) *)
          e_false
        else
        if QED.eval_eq zero b then
          (* a in domain && b = 0 -> ( f(a) = 0 <-> a = 0 ) *)
          e_eq a zero
        else raise Not_found
    | _ -> raise Not_found
  end

(* rewrite a<=b when a or b is f(x)
   for functions f such as 0 <= f(x) && f(x) = 0 <-> x = 0 *)
let builtin_positive_leq lfun ~domain ~zero ~monotonic a b =
  let open Qed.Logic in
  begin match F.repr a , F.repr b with
    | Fun(f,[a]) , Fun(f',[b])
      when monotonic && f == lfun && f' == lfun && domain a && domain b ->
        (* increasing && a in domain && b in domain -> ( f(a) <= f(b) <-> a <= b) *)
        e_leq a b
    | Fun(f,[a]) , _ when f == lfun && domain a ->
        if QED.eval_lt b zero then
          (* a in domain && b < 0 -> ( f(a) <= b <-> false ) *)
          e_false
        else
        if QED.eval_eq zero b then
          (* a in domain && b = 0 -> ( f(a) <= b <-> a = 0 )*)
          e_eq a zero
        else raise Not_found
    | _ , Fun(f,[b]) when f == lfun && domain b && QED.eval_leq a zero ->
        (* b in domain && a <= 0 -> ( a <= f(b) <-> true *)
        e_true
    | _ -> raise Not_found
  end

(* rewrite a=b when a or b is f(x)
   for functions f such as 0 < f(x) *)
let builtin_strict_eq lfun ~domain ~zero ~injective a b =
  let open Qed.Logic in
  begin match F.repr a , F.repr b with
    | Fun(f,[a]) , Fun(f',[b])
      when injective && f == lfun && f' == lfun && domain a && domain b ->
        (* injective && a in domain && b in domain -> ( f(a) = f(b) <-> a = b ) *)
        e_eq a b
    | Fun(f,[a]) , _ when f == lfun && domain a && QED.eval_leq b zero ->
        (* a in domain && b <= 0 -> ( f(a) = b <-> false ) *)
        e_false
    | _ -> raise Not_found
  end

(* rewrite a<=b when a or b is f(x)
   for functions f such as 0 < f(x) *)
let builtin_strict_leq lfun ~domain ~zero ~monotonic a b =
  let open Qed.Logic in
  begin match F.repr a , F.repr b with
    | Fun(f,[a]) , Fun(f',[b])
      when monotonic && f == lfun && f' == lfun && domain a && domain b ->
        (* increasing && a in domain && b in domain -> ( f(a) <= f(b) <-> a <= b ) *)
        e_leq a b
    | Fun(f,[a]) , _ when f == lfun && domain a && QED.eval_leq b zero ->
        (* a in domain && b <= 0 -> ( f(a) <= b <-> false ) *)
        e_false
    | _ , Fun(f,[b]) when f == lfun && domain b && QED.eval_leq a zero ->
        (* b in domain && a <= 0 -> ( a <= f(b) <-> true ) *)
        e_true
    | _ -> raise Not_found
  end

(* -------------------------------------------------------------------------- *)
(* --- Absolute                                                           --- *)
(* -------------------------------------------------------------------------- *)

let f_iabs =
  extern_f ~library:"cmath"
    ~link:{
      altergo = Qed.Engine.F_call "abs_int";
      why3     = Qed.Engine.F_call "IAbs.abs";
      coq      = Qed.Engine.F_call "Z.abs";
    } "\\iabs"

let f_rabs =
  extern_f ~library:"cmath"
    ~result:Real ~params:[Sreal]
    ~link:{
      altergo = Qed.Engine.F_call "abs_real";
      why3     = Qed.Engine.F_call "RAbs.abs";
      coq      = Qed.Engine.F_call "R.abs";
    } "\\rabs"

let () =
  begin
    LogicBuiltins.(add_builtin "\\abs" [Z] f_iabs) ;
    LogicBuiltins.(add_builtin "\\abs" [R] f_rabs) ;
  end

let domain_abs _x = true

let builtin_abs f z e =
  let open Qed.Logic in
  match F.repr e with
  | Times(k,a) -> e_times (Integer.abs k) (e_fun f [a])
  | Kint k -> e_zint (Integer.abs k)
  | Kreal r when Q.lt r Q.zero -> e_real (Q.neg r)
  | _ ->
      match is_true (e_leq z e) with
      | Yes -> e
      | No -> e_opp e
      | Maybe -> raise Not_found

let builtin_iabs_eq = builtin_positive_eq f_iabs
    ~domain:domain_abs ~zero:e_zero ~injective:false

let builtin_iabs_leq = builtin_positive_leq f_iabs
    ~domain:domain_abs ~zero:e_zero ~monotonic:false

let builtin_rabs_eq = builtin_positive_eq f_rabs
    ~domain:domain_abs ~zero:e_zero_real ~injective:false

let builtin_rabs_leq = builtin_positive_leq f_rabs
    ~domain:domain_abs ~zero:e_zero_real ~monotonic:false

(* -------------------------------------------------------------------------- *)
(* --- Square Root                                                        --- *)
(* -------------------------------------------------------------------------- *)

let f_sqrt = f_builtin ~library:"sqrt" "\\sqrt"

let domain_sqrt x = QED.eval_leq e_zero_real x

let builtin_sqrt e =
  let open Qed.Logic in
  match F.repr e with
  | Kreal r when r == Q.zero -> F.e_zero_real (* srqt(0)==0 *)
  | Kreal r when r == Q.one -> F.e_one_real (* srqt(1)==1 *)
  | Mul[a;b] when eval_eq a b -> (* a==b ==> sqrt(a*b)==|a| *)
      e_fun f_rabs [a] (* a is smaller *)
  | _ -> raise Not_found

let builtin_sqrt_eq = builtin_positive_eq f_sqrt
    ~domain:domain_sqrt ~zero:e_zero_real ~injective:true

let builtin_sqrt_leq = builtin_positive_leq f_sqrt
    ~domain:domain_sqrt ~zero:e_zero_real ~monotonic:true

(* -------------------------------------------------------------------------- *)
(* --- Exponential                                                        --- *)
(* -------------------------------------------------------------------------- *)

let f_exp = f_builtin ~library:"exponential" ~injective:true "\\exp"
let f_log = f_builtin ~library:"exponential" "\\log"
let f_log10 = f_builtin ~library:"exponential" "\\log10"
let f_pow = f_builtin ~library:"power" ~params:[Real;Real] "\\pow"

let () = ignore f_log10

let domain_exp _x = true
let domain_log x = QED.eval_lt e_zero_real x

let builtin_exp e =
  let open Qed.Logic in
  match F.repr e with
  | Kreal r when r == Q.zero -> F.e_one_real (* exp(0)==1 *)
  | Times(n,r) when n == Z.minus_one -> (* exp(-r) = 1/exp(r) *)
      F.e_div F.e_one_real (F.e_fun f_exp [r])
  | Fun( f , [x] ) when f == f_log && domain_log x ->
      (* 0<x ==> exp(log(x)) = x *) x
  | _ -> raise Not_found

let builtin_log e =
  let open Qed.Logic in
  match F.repr e with
  | Kreal r when r == Q.one -> F.e_zero_real (* log(1) == 0 *)
  | Fun( f , [x] ) when f == f_exp -> x (* log(exp(x)) == x *)
  | Fun( f , [x;n] ) when f == f_pow && domain_log x ->
      (* 0<x ==> log(x^n) == n*log(x) *)
      F.e_mul n (F.e_fun f_log [x])
  | _ -> raise Not_found

(* a^n = e^(n.log a) *)
let builtin_pow a n =
  let open Qed.Logic in
  match F.repr n with
  | Kreal r when Q.(equal r zero) -> F.e_one_real (* a^0 == 1 *)
  | Kreal r when Q.(equal r one) -> a             (* a^1 == a *)
  | _ -> raise Not_found

let builtin_exp_eq = builtin_strict_eq f_exp
    ~domain:domain_exp ~zero:e_zero_real ~injective:true

let builtin_exp_leq = builtin_strict_leq f_exp
    ~domain:domain_exp ~zero:e_zero_real ~monotonic:true

(* -------------------------------------------------------------------------- *)
(* --- Trigonometry                                                       --- *)
(* -------------------------------------------------------------------------- *)

let f_sin = f_builtin ~library:"trigonometry" "\\sin"
let f_cos = f_builtin ~library:"trigonometry" "\\cos"
let f_tan = f_builtin ~library:"trigonometry" "\\tan"

let f_asin = f_builtin ~library:"arctrigo" "\\asin"
let f_acos = f_builtin ~library:"arctrigo" "\\acos"
let f_atan = f_builtin ~library:"arctrigo" ~injective:true "\\atan"

let domain_asin_acos x =
  QED.eval_leq x e_one_real &&
  QED.eval_leq e_minus_one_real x

let domain_atan _x = true

let builtin_trigo f_arc ~domain e =
  match F.repr e with
  | Fun(f,[x]) when f == f_arc && domain x -> x
  | _ -> raise Not_found

(* -------------------------------------------------------------------------- *)
(* --- Hyperbolic                                                         --- *)
(* -------------------------------------------------------------------------- *)

let () =
  begin
    ignore (f_builtin ~library:"hyperbolic" "\\sinh") ;
    ignore (f_builtin ~library:"hyperbolic" "\\cosh") ;
    ignore (f_builtin ~library:"hyperbolic" "\\tanh") ;
  end

(* -------------------------------------------------------------------------- *)
(* --- Polar Coordinates                                                  --- *)
(* -------------------------------------------------------------------------- *)

let () =
  begin
    ignore (f_builtin ~library:"polar" ~params:[Real;Real] "\\atan2") ;
    ignore (f_builtin ~library:"polar" ~params:[Real;Real] "\\hypot") ;
  end

(* -------------------------------------------------------------------------- *)
(* --- Registry                                                           --- *)
(* -------------------------------------------------------------------------- *)

let () = Context.register
    begin fun () ->

      F.set_builtin_1 f_real_of_int builtin_real_of_int ;
      F.set_builtin_1 f_truncate (builtin_truncate f_truncate) ;
      F.set_builtin_1 f_ceil (builtin_truncate f_ceil) ;
      F.set_builtin_1 f_floor (builtin_truncate f_floor) ;

      F.set_builtin_1   f_iabs (builtin_abs f_iabs e_zero) ;
      F.set_builtin_1   f_rabs (builtin_abs f_rabs e_zero_real) ;
      F.set_builtin_eq  f_iabs builtin_iabs_eq ;
      F.set_builtin_eq  f_rabs builtin_rabs_eq ;
      F.set_builtin_leq f_iabs builtin_iabs_leq ;
      F.set_builtin_leq f_rabs builtin_rabs_leq ;

      F.set_builtin_1   f_sqrt builtin_sqrt ;
      F.set_builtin_eq  f_sqrt builtin_sqrt_eq ;
      F.set_builtin_leq f_sqrt builtin_sqrt_leq ;

      F.set_builtin_1   f_log builtin_log ;
      F.set_builtin_1   f_exp builtin_exp ;
      F.set_builtin_eq  f_exp builtin_exp_eq ;
      F.set_builtin_leq f_exp builtin_exp_leq ;

      F.set_builtin_2   f_pow builtin_pow ;

      F.set_builtin_1 f_sin (builtin_trigo f_asin ~domain:domain_asin_acos) ;
      F.set_builtin_1 f_cos (builtin_trigo f_acos ~domain:domain_asin_acos) ;
      F.set_builtin_1 f_tan (builtin_trigo f_atan ~domain:domain_atan) ;
    end

(* -------------------------------------------------------------------------- *)
