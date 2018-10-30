(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2018                                               *)
(*    CEA (Commissariat à l'énergie atomique et aux énergies              *)
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

open Numerors_utils

module P = Precisions

(* Type declaration *)
type t = P.t * Mpfrf.t

(* Pretty printer *)
let pretty fmt (_, f) = Mpfrf.print fmt f

(* Get back the MPFR rounding mode *)
let rounding = function
  | Rounding.Near -> Mpfr.Near
  | Rounding.Down -> Mpfr.Down
  | Rounding.Up   -> Mpfr.Up

(* Apply an Mpfr function to an Mpfrf object *)
let convert f = fun x rnd ->
  let x' = Mpfrf.to_mpfr x in
  let r = Mpfr.init () in
  let _ = f r x' rnd in
  Mpfrf.of_mpfr r


(*-----------------------------------------------------------------------------
 *    Internal functions to handle the precisions of MPFR numbers
 *---------------------------------------------------------------------------*)

(* Set the default precision *)
let set_precision =
  Mpfr.set_default_prec (P.get P.Real) ;
  let actual_precision = ref P.Real in
  fun prec ->
    if not (P.eq prec !actual_precision) then
      (Mpfr.set_default_prec (P.get prec) ; actual_precision := prec)

(* Monad which sets the default precision before calling the given function f.
   Returns the tuple composed of the precision and the return of f. *)
let ( >>- ) prec f = set_precision prec ; prec, f ()
[@@inline]

(* Internal : change the precision *)
let change_prec ?(rnd = Mpfr.Near) prec (p, x) =
  if not (P.eq p prec) then
    let r = Mpfr.init () in
    let _ = Mpfr.set r (Mpfrf.to_mpfr x) rnd in
    Mpfrf.of_mpfr r
  else x
[@@inline]

(* Returns a function which apply the rounding of its optionnal parameter rnd
   and change the precision according to its optionnal parameter prec before
   calling the unary function f on an input of type t *)
let unary_mpfrf f =
  fun ?(rnd = Rounding.Near) ?(prec = P.Real) x ->
    prec >>- fun () ->
    f (change_prec prec x) (rounding rnd)

(* Returns a function which apply the rounding of its optionnal parameter rnd
   and change the precision according to its optionnal parameter prec before
   calling the binary function f on two inputs of type t *)
let binary_mpfrf f =
  fun ?(rnd = Rounding.Near) ?(prec = P.Real) x y ->
    prec >>- fun () ->
    f (change_prec prec x) (change_prec prec y) (rounding rnd)


(*-----------------------------------------------------------------------------
 *    Constructors
 *---------------------------------------------------------------------------*)
let of_mpfr p f = p, f

let of_int    ?(rnd = Rounding.Near) ?(prec = P.Real) i =
  prec >>- fun () -> Mpfrf.of_int   i (rounding rnd)

let of_float  ?(rnd = Rounding.Near) ?(prec = P.Real) f =
  prec >>- fun () -> Mpfrf.of_float f (rounding rnd)

let of_string ?(rnd = Rounding.Near) ?(prec = P.Real) str =
  prec >>- fun () ->
  let l = String.length str - 1 in
  let last = Transitioning.Char.lowercase_ascii str.[l] in
  let str =
    if last = 'f' || last = 'd' || last = 'l'
    then String.sub str 0 l
    else str
  in
  (* base=0 to let Mpfr infer the base, depending of the encoding of s. *)
  Mpfrf.of_mpfr (Mpfr.init_set_str str ~base:0 (rounding rnd))

let pos_zero prec = of_float ~prec 0.0
let neg_zero prec = of_float ~prec (~-. 0.0)

let pos_inf prec = of_mpfr prec @@ Mpfrf.of_float infinity      Mpfr.Near
let neg_inf prec = of_mpfr prec @@ Mpfrf.of_float neg_infinity  Mpfr.Near


(*-----------------------------------------------------------------------------
 *    Comparison methods
 *---------------------------------------------------------------------------*)
let compare (px, nx) (py, ny) =
  if not (Precisions.eq px py) then
    Value_parameters.fatal
      "Numerors: impossible to compare two numbers with different precisions"
  else Mpfrf.cmp nx ny
let eq a b = compare a b =  0
let le a b = compare a b <= 0
let lt a b = compare a b <  0
let ge a b = compare a b >= 0
let gt a b = compare a b >  0

let min x y = if compare x y <= 0 then x else y
let max x y = if compare x y <= 0 then y else x


(*-----------------------------------------------------------------------------
 *    Getters on floats
 *---------------------------------------------------------------------------*)
let sign (_, x) =
  let s = Mpfrf.sgn x in
  if s = 0 then
    (* Ugly fix because the sign of a MPFR zero is zero ! FUCK IT *)
    let fx = Mpfrf.to_float x in
    Sign.of_int @@ int_of_float @@ copysign 1.0 fx
  else Sign.of_int s

let prec (p, _) = p

(* The minus 1 is mandatory because MPFR represents the float numbers
   with a significand between 0 and 1 in place of the standard in the
   IEEE-754 norm. This difference implies that the exponent of the
   MPFR representation is greater than the one of the standard
   representation by one. *)
let exponent (prec, x as f) =
  if eq f (pos_zero prec) then min_int
  else (Mpfr.get_exp (Mpfrf.to_mpfr x)) - 1

let significand (prec, x) = prec >>- fun () ->
  let significand = Mpfrf.to_mpfr x in
  let _ = Mpfr.set_exp significand 1 in
  Mpfrf.abs (Mpfrf.of_mpfr significand) Mpfr.Near


(*-----------------------------------------------------------------------------
 *    Methods to check properties on floats
 *---------------------------------------------------------------------------*)
let is_nan (_, x) = Mpfrf.nan_p x
let is_inf (_, x) = Mpfrf.inf_p x

let is_pos f = Sign.is_pos (sign f)
let is_neg f = Sign.is_neg (sign f)

let is_a_zero (prec, _ as f) =  eq f @@ pos_zero prec
let is_pos_zero f = is_pos f && is_a_zero f
let is_neg_zero f = is_neg f && is_a_zero f
let is_strictly_pos f = is_pos f && not (is_a_zero f)
let is_strictly_neg f = is_neg f && not (is_a_zero f)


(*-----------------------------------------------------------------------------
 *    Functions without rounding errors
 *---------------------------------------------------------------------------*)
let neg (p, x) = p >>- fun () -> Mpfrf.neg x Mpfr.Near
let abs (p, x) = p >>- fun () -> Mpfrf.abs x Mpfr.Near


(*-----------------------------------------------------------------------------
 *    Operators
 *---------------------------------------------------------------------------*)
let add = binary_mpfrf Mpfrf.add
let sub = binary_mpfrf Mpfrf.sub
let mul = binary_mpfrf Mpfrf.mul
let div = binary_mpfrf Mpfrf.div
let pow = binary_mpfrf Mpfrf.pow
let pow_int =
  fun ?(rnd = Rounding.Near) ?(prec = P.Real) x n ->
    prec >>- fun () -> Mpfrf.pow_int (change_prec prec x) n (rounding rnd)


(*-----------------------------------------------------------------------------
 *    Functions with rounding errors
 *---------------------------------------------------------------------------*)
let square = unary_mpfrf (fun x -> Mpfrf.mul x x)
let sqrt = unary_mpfrf Mpfrf.sqrt
let log = unary_mpfrf @@ convert Mpfr.log
let exp = unary_mpfrf @@ convert Mpfr.exp
let sin = unary_mpfrf @@ convert Mpfr.sin
let cos = unary_mpfrf @@ convert Mpfr.cos
let tan = unary_mpfrf @@ convert Mpfr.tan


(*-----------------------------------------------------------------------------
 *    Apply the sign of <src> on <dst>
 *---------------------------------------------------------------------------*)
let apply_sign ~src ~dst =
  if not (Sign.eq (sign src) (sign dst)) then neg dst else dst


(*-----------------------------------------------------------------------------
 *    Next and prev float
 *---------------------------------------------------------------------------*)
let next_float (p, x) =
  let x' = Mpfrf.to_mpfr x in
  Mpfr.nextabove x' ;
  p, Mpfrf.of_mpfr x'

let prev_float (p, x) =
  let x' = Mpfrf.to_mpfr x in
  Mpfr.nextbelow x' ;
  p, Mpfrf.of_mpfr x'


(*-----------------------------------------------------------------------------
 *    Machine constants
 *---------------------------------------------------------------------------*)
let machine_epsilon ?(prec = P.Real) p =
  pow_int ~rnd:Rounding.Up ~prec (of_int ~prec 2) (- P.get p)

let machine_delta ?(prec = P.Real) p =
  pow_int ~rnd:Rounding.Up ~prec (of_int ~prec 2) @@ (P.denormalized p) - 1

let maximal_pos_float ~prec = prev_float @@ pos_inf prec

let maximal_neg_float ~prec = next_float @@ neg_inf prec

let change_prec ~rnd ~prec t = prec, change_prec ~rnd:(rounding rnd) prec t
