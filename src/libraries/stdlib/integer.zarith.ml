(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2015                                               *)
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

type t = Z.t

exception Too_big

let equal = Z.equal

let compare = Z.compare


let two_power_of_int k =
  Z.shift_left Z.one k

let two_power y =
  try
    let k = Z.to_int y in
    if k > 1024 then
      (* avoid memory explosion *)
      raise Too_big
    else
      two_power_of_int k
  with Z.Overflow -> raise Too_big

let popcount = Z.popcount

(* To export *)

  let small_nums = Array.init 33 (fun i -> Z.of_int i)

  let zero = Z.zero
  let one = Z.one
  let minus_one = Z.minus_one
  let two = Z.of_int 2
  let four = Z.of_int 4
  let eight = Z.of_int 8
  let sixteen = Z.of_int 16
  let thirtytwo = Z.of_int 32
  let onethousand = Z.of_int 1000
  let billion_one = Z.of_int 1_000_000_001
  let two_power_32 = two_power_of_int 32
  let two_power_60 = two_power_of_int 60
  let two_power_64 = two_power_of_int 64

  let is_zero v = Z.equal v Z.zero


  let add = Z.add
  let sub = Z.sub
  let succ = Z.succ
  let pred = Z.pred
  let neg = Z.neg

  let rem = Z.erem
  let div = Z.ediv
  let mul = Z.mul

  let abs = Z.abs

  let hash = Z.hash

  let shift_left x y = Z.shift_left x (Z.to_int y)
  let shift_right x y = Z.shift_right x (Z.to_int y)
  let shift_right_logical x y = (* no meaning for negative value of x *)
    if (Z.lt x Z.zero)      
    then failwith "log_shift_right_big_int"
    else Z.shift_right x (Z.to_int y)

  let logand = Z.logand
  let lognot = Z.lognot
  let logor = Z.logor
  let logxor = Z.logxor

  let le a b = Z.compare a b <= 0
  let ge a b = Z.compare a b >= 0
  let lt a b = Z.compare a b < 0
  let gt a b = Z.compare a b > 0


  let of_int = Z.of_int

  let of_int64 = Z.of_int64
  let of_int32 = Z.of_int32

  (* Return the same exceptions as [Big_int] *)
  let to_int = Big_int_Z.int_of_big_int
  let to_int64 = Big_int_Z.int64_of_big_int
  let of_string s =
    try Z.of_string s
    with Invalid_argument _ ->
    (* We intentionally do NOT specify a string in the .mli, as Big_int
       raises multiple [Failure _] exceptions *)
      failwith "Integer.of_string"


  let max_int64 = of_int64 Int64.max_int
  let min_int64 = of_int64 Int64.min_int


  let to_string = Z.to_string
  let to_float = Z.to_float

  let add_2_64 x = add two_power_64 x
  let add_2_32 x = add two_power_32 x

  let pretty ?(hexa=false) fmt v =
    let rec aux v =
      if gt v two_power_60 then
        let quo, rem = Z.ediv_rem v two_power_60 in
        aux quo;
        Format.fprintf fmt "%015LX" (to_int64 rem)
      else
        Format.fprintf fmt "%LX" (to_int64 v)
    in
    if hexa then
      if equal v zero then Format.pp_print_string fmt "0"
      else if gt v zero then (Format.pp_print_string fmt "0x"; aux v)
      else (Format.pp_print_string fmt "-0x"; aux (Z.neg v))
      else
        Format.pp_print_string fmt (to_string v)

  let is_one v = equal one v
  let pos_div  = div

  let pos_rem = rem
  let native_div = div
  let divexact = Z.divexact
  let div_rem = Z.div_rem

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
    if (not signed) 
    then 
      let factor = two_power size in logand value (pred factor)
    else
      let mask = two_power (sub size one) in
      let p_mask = pred mask in
      if equal (logand mask value) zero
      then logand value p_mask
      else
	logor (lognot p_mask) value

  let length u v = succ (sub v u)

  let extract_bits ~start ~stop v =
    assert (ge start zero && ge stop start);
    (*Format.printf "%a[%a..%a]@\n" pretty v pretty start pretty stop;*)
    let r = Z.extract v (to_int start) (to_int (length start stop)) in
      (*Format.printf "%a[%a..%a]=%a@\n" pretty v pretty start pretty stop pretty r;*)
      r

  let is_even v = is_zero (logand one v)

  (** [pgcd u 0] is allowed and returns [u] *)
  let pgcd u v =
    let r =
      if is_zero v
      then u
      else Z.gcd u v in
      r

  let ppcm u v =
    if u = zero || v = zero
    then zero
    else native_div (mul u v) (pgcd u v)

  let min = Z.min
  let max = Z.max

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

  let to_num b = 
    Num.num_of_big_int
      (Big_int.big_int_of_string (Big_int_Z.string_of_big_int b))

  let power_int_positive_int = Big_int_Z.power_int_positive_int
