(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2019                                               *)
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
let total_compare = compare
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

let le f1 f2 = compare f1 f2 <= 0


(* --------------------------------------------------------------------------
                                 Widen hints
   -------------------------------------------------------------------------- *)

module Widen_Hints = struct

  include Cil_datatype.Logic_real.Set

  let pretty fmt s =
    if not (is_empty s) then
      Pretty_utils.pp_iter
        ~pre:"@[<hov 1>{"
        ~suf:"}@]"
        ~sep:";@ "
        iter
        (fun fmt r -> Format.pp_print_string fmt r.Cil_types.r_literal) fmt s

  let logic_real_of_float f =
    { Cil_types.r_literal = Format.asprintf "%10.7g" f;
      r_nearest = f;
      r_lower = f;
      r_upper = f; }

  let of_float_list l =
    match l with
    | [] -> empty
    | [e] -> singleton (logic_real_of_float e)
    | e :: q ->
      List.fold_left
        (fun acc x -> add (logic_real_of_float x) acc)
        (singleton (logic_real_of_float e)) q

  let default_widen_hints =
    let l = [0.0;1.0;10.0;1e10;Floating_point.max_single_precision_float;1e80] in
    union (of_float_list l) (of_float_list (List.map (fun x -> -. x) l))

  exception Found of float

  let nearest_float_ge f s =
    try
      iter (fun e ->
          if total_compare e.Cil_types.r_upper f >= 0
          then raise (Found e.Cil_types.r_upper))
        s;
      raise Not_found
    with Found r -> r

  let nearest_float_le f s =
    try
      let els_desc = List.rev (elements s) in
      List.iter (fun e ->
          if total_compare e.Cil_types.r_lower f <= 0
          then raise (Found e.Cil_types.r_lower))
        els_desc;
      raise Not_found
    with Found r -> r

end

type widen_hints = Widen_Hints.t

let widen_up wh prec f =
  let r = try Widen_Hints.nearest_float_ge f wh
    with Not_found ->
      if le f max_float then max_float
      else infinity
  in
  round_to_precision Up prec r

let widen_down wh prec f =
  let r = try Widen_Hints.nearest_float_le f wh
    with Not_found ->
      if le (-. max_float) f then (-. max_float)
      else neg_infinity
  in
  round_to_precision Down prec r

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
