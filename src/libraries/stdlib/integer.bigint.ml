(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2016                                               *)
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

type t = Big_int.big_int

exception Too_big

include Big_int

let equal = eq_big_int

let compare = compare_big_int

(** Computes [2^n] for [n] up to 1024 (arbitrarily chosen).
    Raises [Too_big] for [n] > 1024, to avoid memory explosion. *)
let safe_two_power_int n =
  if n > 1024 then raise Too_big
  else power_int_positive_int 2 n

(* Nb of significant digits in a "word" of Big_int. *)
let nb_digits_of_big_int =
  let r =
    let rec nb_digits y =
      if 1 = num_digits_big_int (safe_two_power_int y)
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

let two_power_of_int =
  let h = Hashtbl.create 7 in
  fun k ->
      try
        Hashtbl.find h k
      with Not_found ->
        let p = safe_two_power_int k in
        Hashtbl.add h k p;
        p

let two_power y =
    try
      let k = int_of_big_int y in
      two_power_of_int k
    with Failure _ -> raise Too_big

let log_shift_right_big_int x y = (* no meaning for negative value of x *)
  if (lt_big_int x zero_big_int)
  then raise (Invalid_argument "log_shift_right_big_int")
  else shift_right_big_int x y

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


let land_big_int = bitwise_op_big_int (land)
let lor_big_int  = bitwise_op_big_int (lor)
let lxor_big_int = bitwise_op_big_int (lxor)

(* Get the value encoded from the 'first' to 'last' bit of 'x' :
   Shift right 'x' and apply a mask on it.
   The result is:  div (mod x (2**(last+1))) (2**first) *)
let bitwise_extraction first_bit last_bit x =
  assert (first_bit <= last_bit);(* first_bit <= last_bit *)
  assert (first_bit >= 0);       (* first_bit >= 0        *)
  let q = div_big_int x (power_int_positive_int 2 first_bit) in
  let r = mod_big_int q (power_int_positive_int 2 (1 + last_bit - first_bit)) in
  r

(* To export *)

  let small_nums = Array.init 33 (fun i -> big_int_of_int i)

  let zero = zero_big_int
  let one = unit_big_int
  let two = small_nums.(2)
  let four = small_nums.(4)
  let eight = small_nums.(8)
  let sixteen = small_nums.(16)
  let thirtytwo = small_nums.(32)
  let onethousand = big_int_of_int 1000
  let billion_one = big_int_of_int 1_000_000_001

  let is_zero v = (sign_big_int v) = 0

  let rem = mod_big_int
  let div = div_big_int
  let divexact = div_big_int
  let div_rem = quomod_big_int

  let mul = mult_big_int

  let sub = sub_big_int

  let abs = abs_big_int
  let succ = succ_big_int
  let pred = pred_big_int
  let neg = minus_big_int

  let add = add_big_int

  let hash c =
    let i =
      try
        int_of_big_int c
      with Failure _ -> int_of_big_int (rem c billion_one)
    in
    197 + i

  let shift_right_logical = log_shift_right_big_int
  let shift_right = shift_right_big_int
  let shift_left = shift_left_big_int

  let logand = land_big_int
  let lognot = lnot_big_int
  let logor = lor_big_int
  let logxor = lxor_big_int

  let le = le_big_int
  let lt = lt_big_int
  let ge = ge_big_int
  let gt = gt_big_int

  let to_int v =
    try int_of_big_int v
    with Failure _ -> failwith "to_int"

  let of_int i =
    if 0 <= i && i <= 32
    then small_nums.(i)
    else big_int_of_int i

  let of_int64 i = big_int_of_int64 i
  let to_int64 i = int64_of_big_int i
  let of_int32 i = big_int_of_string (Int32.to_string i)
  let max_int64 = of_int64 Int64.max_int
  let min_int64 = of_int64 Int64.min_int

  let of_string = big_int_of_string (* may raise Failure "sys_big_int_of_string"
     or Failure "invalid digit". Let's leave the exact string unspecified *)
  let to_string = string_of_big_int
  let to_float = float_of_big_int

  (* Taken from Zarith, equal to 0x3ffffffffffff000. All positive double below
     this one can be translated as 63 bits OCaml integers, AND this double
     is exact. *)
  let max_precise_double_as_64_bits = 4611686018427383808.

  (* A better implementation is available through Zarith *)
  let of_float f =
    if (Sys.word_size = 32 &&
        Pervasives.abs_float f <= Pervasives.float_of_int max_int) ||
       (Sys.word_size = 64 &&
        Pervasives.abs_float f <= max_precise_double_as_64_bits)
    then of_int (int_of_float f)
    else raise Too_big

  let minus_one = pred zero

  let two_power_32 = two_power_of_int 32
  let two_power_60 = two_power_of_int 60
  let two_power_64 = two_power_of_int 64

  let add_2_64 x = add two_power_64 x
  let add_2_32 x = add two_power_32 x

  let pretty ?(hexa=false) fmt v =
    let rec aux v =
      if gt v two_power_60 then
        let quo, rem = quomod_big_int v two_power_60 in
        aux quo;
        Format.fprintf fmt "%015LX" (to_int64 rem)
      else
        Format.fprintf fmt "%LX" (to_int64 v)
    in
    if hexa then
      if equal v zero then Format.pp_print_string fmt "0"
      else if gt v zero then (Format.pp_print_string fmt "0x"; aux v)
      else (Format.pp_print_string fmt "-0x"; aux (minus_big_int v))
      else
        Format.pp_print_string fmt (to_string v)

  let is_one v = equal one v
  let pos_div  = div

  let pos_rem = rem
  let native_div = div

  let c_div u v =
    let bad_div = div u v in
    if (lt u zero) && not (is_zero (rem u v))
    then
      if lt v zero
      then pred bad_div
      else succ bad_div
    else bad_div


  let c_rem u v =
    sub u (mul v (c_div u v))

  let cast ~size ~signed ~value =
    let factor = two_power size in
    let mask = two_power (sub size one) in

    if (not signed) then pos_rem value factor
    else
      if equal (logand mask value) zero
    then logand value (pred mask)
    else
      logor (lognot (pred mask)) value

  let extract_bits ~start ~stop v =
    assert (ge start zero && ge stop start);
    (*Format.printf "%a[%a..%a]@\n" pretty v pretty start pretty stop;*)
    let r = bitwise_extraction (to_int start) (to_int stop) v in
      (*Format.printf "%a[%a..%a]=%a@\n" pretty v pretty start pretty stop pretty r;*)
      r

  let is_even v = is_zero (logand one v)

  (** [pgcd u 0] is allowed and returns [u] *)
  let pgcd u v =
    let r =
      if is_zero v
      then u
      else gcd_big_int u v in
      r

  let ppcm u v =
    if u = zero || v = zero
    then zero
    else native_div (mul u v) (pgcd u v)

  let length u v = succ (sub v u)

  let min = min_big_int
  let max = max_big_int

  let round_down_to_zero v modu =
    mul (pos_div v modu) modu

  (** [round_up_to_r m r modu] is the smallest number [n] such that
         [n]>=[m] and [n] = [r] modulo [modu] *)
  let round_up_to_r ~min:m ~r ~modu =
    add (add (round_down_to_zero (pred (sub m r)) modu) r) modu

  (** [round_down_to_r m r modu] is the largest number [n] such that
     [n]<=[m] and [n] = [r] modulo [modu] *)
  let round_down_to_r ~max:m ~r ~modu =
    add (round_down_to_zero (sub m r) modu) r

  let to_num = Num.num_of_big_int

  (* only for x >= 0 *)
  let popcount x = 
    let rec aux x acc =
      if is_zero x
      then acc
      else
	let acc = acc + (to_int (logand x one)) in
	aux (shift_right x one) acc
    in
    aux x 0
