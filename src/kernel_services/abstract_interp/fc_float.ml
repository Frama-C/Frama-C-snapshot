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

open Float_sig

let set_rounding_mode = function
  | Down -> Floating_point.set_round_downward ()
  | Up -> Floating_point.set_round_upward ()
  | Zero -> Floating_point.set_round_toward_zero ()
  | Near -> ()

let (>>%) round f =
  set_rounding_mode round;
  let result = f () in
  if round <> Near then Floating_point.set_round_nearest_even ();
  result

let is_single = function Single -> true | _ -> false

let round_to_precision prec f =
  if is_single prec then Floating_point.round_to_single_precision_float f else f

type t = float

let packed_descr = Structural_descr.p_float

let hash = Hashtbl.hash
let pretty = Floating_point.pretty

let is_exact = function
  | Single | Double -> true
  | Long_Double | Real -> false

let cmp_ieee = (compare: float -> float -> int)

(** NOTE: all floating-point comparisons using OCaml's standard operators
    do NOT distinguish between -0.0 and 0.0.
    Whenever floats are compared using them, it implies that negative zeroes
    are also considered, e.g. "if x < 0.0" is equivalent to "if x < -0.0",
    which is also equivalent to "F.compare x (-0.0) < 0".
    This 'compare' operator distinguishes -0. and 0. *)
(* replace "noalloc" with [@@noalloc] for OCaml version >= 4.03.0 *)
[@@@ warning "-3"]
external compare : float -> float -> int = "float_compare_total" "noalloc"
[@@@ warning "+3"]

let of_float round prec f = round >>% fun () -> round_to_precision prec f

let to_float f = f

let is_nan a = classify_float a = FP_nan
let is_infinite f = classify_float f = FP_infinite
let is_finite f = match classify_float f with
  | FP_nan | FP_infinite -> false
  | FP_normal | FP_subnormal | FP_zero -> true

(* replace "noalloc" with [@@noalloc] for OCaml version >= 4.03.0 *)
[@@@ warning "-3"]
external is_negative : float -> bool = "float_is_negative" "noalloc"
[@@@ warning "+3"]

let round_to_precision round prec t =
  if is_single prec
  then round >>% fun () -> Floating_point.round_to_single_precision_float t
  else t

(* Wrong for [next -0.] and [prev 0.], as this function uses the binary
   representation of floating-point values, which is not continuous at 0. *)
let next_previous int64fup int64fdown float =
  let r = Int64.bits_of_float float in
  let f = if r >= 0L then int64fup else int64fdown in
  let f = Int64.float_of_bits (f r) in
  match classify_float f with
  | FP_nan -> float (* can only be produced from an infinity or a nan *)
  | FP_infinite | FP_normal | FP_subnormal | FP_zero -> f

let next_float prec f =
  if not (is_exact prec) then f else
  if compare f (-0.) = 0 then 0. else
    let f = next_previous Int64.succ Int64.pred f in
    if is_single prec
    then Up >>% fun () -> Floating_point.round_to_single_precision_float f
    else f

let prev_float prec f =
  if not (is_exact prec) then f else
  if compare f 0. = 0 then -0. else
    let f = next_previous Int64.pred Int64.succ f in
    if is_single prec
    then Down >>% fun () -> Floating_point.round_to_single_precision_float f
    else f

let m_pi = 3.1415929794311523 (* single-precision *)
let m_pi_2 = 1.5707964897155761 (* single-precision *)
let max_single_precision_float = Floating_point.max_single_precision_float

let le f1 f2 = compare f1 f2 <= 0

let widen_up f =
  if le f (-0.) then -0.
  else if le f 0. then 0.
  else if le f 1. then 1.
  else if le f m_pi_2 then m_pi_2
  else if le f m_pi then m_pi
  else if le f 10. then 10.
  else if le f 1e10 then 1e10
  else if le f max_single_precision_float then max_single_precision_float
  else if le f 1e80 then 1e80
  else if le f max_float then max_float
  else infinity

let neg = (~-.)
let abs = abs_float

let floor = floor
let ceil = ceil
let trunc = Floating_point.trunc
let fround = Floating_point.fround

let binary op = fun round prec x y ->
  round >>% fun () ->
  if is_single prec
  then Floating_point.round_to_single_precision_float (op x y)
  else op x y

let add round = binary ( +. ) round
let sub round = binary ( -. ) round
let mul round = binary ( *. ) round
let div round = binary ( /. ) round
let fmod round = binary mod_float round

let generate single double =
  fun round prec ->
    round >>% fun () -> if is_single prec then single else double

let exp round = generate Floating_point.expf exp round
let log round = generate Floating_point.logf log round
let log10 round = generate Floating_point.log10f log10 round
let sqrt round = generate Floating_point.sqrtf sqrt round
let pow round = generate Floating_point.powf ( ** ) round

let cos round = generate Floating_point.cosf cos round
let sin round = generate Floating_point.sinf sin round
let atan2 round = generate Floating_point.atan2f atan2 round
