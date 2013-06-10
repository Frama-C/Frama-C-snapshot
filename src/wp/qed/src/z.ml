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

open Big_int

(* Nb of significant digits in a "word" of Big_int. *)
let nb_digits_of_big_int =
  let r =
    let rec nb_digits y =
      if 1 = num_digits_big_int (power_int_positive_int 2 y)
      then nb_digits (y + 1)
      else y
    in nb_digits 1
  in r

let base = power_int_positive_int 2 nb_digits_of_big_int
let base16bits = power_int_positive_int 2 16

(* If X is such that x
   = let f a x   =(a * base) + x        in List.fold_left f 0 X,
   and Y such that y
   = let f a y   =(a * base) + y        in List.fold_left f 0 Y,
   we have map2_base base op x y =
       let f a x y =(a * base) + (op x y) in List.fold_left f 0 X Y
*)
let map2_base b op x y =
  let rec map2_base_rec a x y =
    let (qx, mx) = quomod_big_int x b
    and (qy, my) = quomod_big_int y b
    in let res_m = op mx my
       and res_q =
        if (eq_big_int zero_big_int qx)
          && (eq_big_int zero_big_int qy)
        then a
        else map2_base_rec a qx qy
    in add_big_int (mult_big_int res_q b) res_m
  in map2_base_rec zero_big_int x y

let bitwise_op_positive_big_int op x y =
  assert (ge_big_int x zero_big_int);
  assert (ge_big_int y zero_big_int);
  let g =
    let f u v = assert(is_int_big_int u) ;
      assert(is_int_big_int v) ;
      let r = op (int_of_big_int u) (int_of_big_int v)
      in big_int_of_int (r)
    in map2_base base16bits f
  in let r = map2_base base g x y
  in assert (ge_big_int r zero_big_int);
    r

let lnot_big_int w = minus_big_int (succ_big_int w)

let shift_left_big_int x y = (* idem multiplication *)
  mult_big_int x (power_int_positive_big_int 2 y)

let shift_right_big_int x y = (* idem division rounding to -oo *)
  div_big_int x (power_int_positive_big_int 2 y)

let power_two =
  let h = Hashtbl.create 7 in
  fun k ->
      try
        Hashtbl.find h k
      with Not_found ->
        let p = power_int_positive_int 2 k in
        Hashtbl.add h k p;
        p

let two_power y =
    try
      let k = int_of_big_int y in
      power_two k
    with Failure _ -> assert false

let bitwise_op_big_int op x y =
  let (positive_x, op_sx) =
    if gt_big_int zero_big_int x
    then (lnot_big_int x, (fun u v -> op (lnot u) v))
    else (x, op)
  in let (positive_y, op_sx_sy) =
      if gt_big_int zero_big_int y
      then (lnot_big_int y, (fun u v -> op_sx u (lnot v)))
      else (y, op_sx)
  in let (positive_op_map, op_map) =
      if 0 = (op_sx_sy 0 0)
      then (op_sx_sy, (fun w -> w))
      else ((fun u v -> lnot (op_sx_sy u v)), lnot_big_int)
  in op_map (bitwise_op_positive_big_int positive_op_map positive_x positive_y)

let bitwise_not w = minus_big_int (succ_big_int w)
let bitwise_and = bitwise_op_big_int (land)
let bitwise_or  = bitwise_op_big_int (lor)
let bitwise_xor = bitwise_op_big_int (lxor)

let bitwise_shift_right = shift_right_big_int
let bitwise_shift_left = shift_left_big_int

let cast_max ~max ~signed ~value =
  if (not signed)
  then 
     mod_big_int value max
  else
    if eq_big_int (bitwise_and max value) zero_big_int
    then bitwise_and value (pred_big_int max)
    else bitwise_or (bitwise_not (pred_big_int max)) value

let cast_size ~size ~signed ~value =
  let max = if (not signed)
    then  two_power size
    else two_power (pred_big_int size) 
  in cast_max ~max ~signed ~value

(* end of bitwise operations *)

type t = big_int

let zero = zero_big_int
let one = unit_big_int
let minus_one = minus_big_int one
let int = big_int_of_int

let succ = succ_big_int
let pred = pred_big_int

let add = add_big_int
let sub = sub_big_int
let mul = mult_big_int
let opp = minus_big_int
let div a b = 
  let sb = sign_big_int b in
  if sb = 0 then failwith "Division by zero" ;
  let sa = sign_big_int a in
  if sa = 0 then zero else
    let a = abs_big_int a in
    let b = abs_big_int b in
    let q = div_big_int a b in
    if sa * sb > 0 then q else minus_big_int q

let remainder a b =
  let sb = sign_big_int b in
  if sb = 0 then failwith "Division by zero" ;
  let sa = sign_big_int a in
  if sa = 0 then zero else
    let a = abs_big_int a in
    let b = abs_big_int b in
    let r = mod_big_int a b in
    if sa > 0 then r else minus_big_int r

let euclidian a b =
  let sb = sign_big_int b in
  if sb = 0 then failwith "Division by zero" ;
  let sa = sign_big_int a in
  if sa = 0 then zero , zero else
    let a = abs_big_int a in
    let b = abs_big_int b in
    let q,r = quomod_big_int a b in
    ( (if sa * sb > 0 then q else minus_big_int q) ,
      (if sa > 0 then r else minus_big_int r) )

let to_string = string_of_big_int
let of_string = big_int_of_string

let pretty fmt x = Format.pp_print_string fmt (string_of_big_int x)

let of_int = big_int_of_int
let to_int x = try Some(int_of_big_int x) with _ -> None
let to_big_int x = x
let of_big_int x = x

let hash x = Hashtbl.hash (to_string x)
let compare = compare_big_int
let equal = eq_big_int
let not_equal x y = compare x y <> 0
let leq = le_big_int
let lt = lt_big_int
let max x y = if leq x y then y else x
let min x y = if leq x y then x else y
let positive x = sign_big_int x >= 0
let negative x = sign_big_int x <= 0
let null x = sign_big_int x = 0
let lt_zero x = sign_big_int x < 0
let gt_zero x = sign_big_int x > 0

type sign = Null | Positive | Negative

let sign x = 
  let s = sign_big_int x in
  if s < 0 then Negative else
    if s > 0 then Positive else
      Null
