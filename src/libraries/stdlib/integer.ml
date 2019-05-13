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

type t = Z.t

let equal = Z.equal

let compare = Z.compare

let two_power_of_int k =
  Z.shift_left Z.one k

let two_power n =
  let k = Z.to_int n in
  if k > 1024 then
    raise Z.Overflow
  else
    two_power_of_int k

let power_int_positive_int = Big_int_Z.power_int_positive_int

let popcount = Z.popcount

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

let mul = Z.mul

let e_div = Z.ediv
let e_rem = Z.erem
let e_div_rem = Z.ediv_rem

let c_div = Z.div
let c_rem = Z.rem
let c_div_rem = Z.div_rem

let abs = Z.abs

let hash = Z.hash

let shift_left x y = Z.shift_left x (Z.to_int y)
let shift_right x y = Z.shift_right x (Z.to_int y)
let shift_right_logical x y = (* no meaning for negative value of x *)
  if (Z.lt x Z.zero)
  then raise (Invalid_argument "Integer.shift_right_logical")
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

let to_int = Z.to_int
let to_int64 = Z.to_int64
let to_int32 = Z.to_int32

let of_string = Z.of_string
let to_string = Z.to_string

let of_float = Z.of_float
let to_float = Z.to_float
let max_int64 = of_int64 Int64.max_int
let min_int64 = of_int64 Int64.min_int


let bdigits = [|
  "0000" ; (* 0 *)
  "0001" ; (* 1 *)
  "0010" ; (* 2 *)
  "0011" ; (* 3 *)
  "0100" ; (* 4 *)
  "0101" ; (* 5 *)
  "0110" ; (* 6 *)
  "0111" ; (* 7 *)
  "1000" ; (* 8 *)
  "1001" ; (* 9 *)
  "1010" ; (* 10 *)
  "1011" ; (* 11 *)
  "1100" ; (* 12 *)
  "1101" ; (* 13 *)
  "1110" ; (* 14 *)
  "1111" ; (* 15 *)
|]

let pp_bin_pos fmt r = Format.pp_print_string fmt bdigits.(r)
let pp_bin_neg fmt r = Format.pp_print_string fmt bdigits.(15-r)

let pp_hex_pos fmt r = Format.fprintf fmt "%04X" r
let pp_hex_neg fmt r = Format.fprintf fmt "%04X" (0xFFFF-r)

let bmask_bin = Z.of_int 0xF     (* 4 bits mask *)
let bmask_hex = Z.of_int 0xFFFF (* 64 bits mask *)

type digits = {
  nbits : int ; (* max number of bits *)
  bsize : int ; (* bits in each bloc *)
  bmask : Z.t ; (* block mask, must be (1 << bsize) - 1 *)
  sep : string ;
  pp : Format.formatter -> int -> unit ; (* print one block *)
}

let rec pp_digits d fmt n v =
  if gt v zero || n < d.nbits then
    begin
      let r = Z.to_int (Z.logand v d.bmask) in
      let k = d.bsize in
      pp_digits d fmt (n + k) (Z.shift_right_trunc v k) ;
      if gt v d.bmask || (n + k) < d.nbits
      then Format.pp_print_string fmt d.sep ;
      d.pp fmt r ;
    end

let pp_bin ?(nbits=1) ?(sep="") fmt v =
  let nbits = if nbits <= 0 then 1 else nbits in
  if le zero v then
    ( Format.pp_print_string fmt "0b" ;
      pp_digits { nbits ; sep ; bsize=4 ;
                  bmask = bmask_bin ; pp = pp_bin_pos } fmt 0 v )
  else
    ( Format.pp_print_string fmt "1b" ;
      pp_digits { nbits ; sep ; bsize=4 ;
                  bmask = bmask_bin ; pp = pp_bin_neg } fmt 0 (Z.lognot v) )

let pp_hex ?(nbits=1) ?(sep="") fmt v =
  let nbits = if nbits <= 0 then 1 else nbits in
  if le zero v then
    ( Format.pp_print_string fmt "0x" ;
      pp_digits { nbits ; sep ; bsize=16 ;
                  bmask = bmask_hex ; pp = pp_hex_pos } fmt 0 v )

  else
    ( Format.pp_print_string fmt "1x" ;
      pp_digits { nbits ; sep ; bsize=16 ;
                  bmask = bmask_hex ; pp = pp_hex_neg } fmt 0 (Z.lognot v) )
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

let pgcd u v =
  if is_zero v then abs u (* Zarith raises an exception on zero arguments *)
  else if is_zero u then abs v
  else Z.gcd u v

let ppcm u v =
  if u = zero || v = zero
  then zero
  else Z.lcm u v

let min = Z.min
let max = Z.max

let round_down_to_zero v modu =
  mul (e_div v modu) modu

let round_up_to_r ~min:m ~r ~modu =
  add (add (round_down_to_zero (pred (sub m r)) modu) r) modu

let round_down_to_r ~max:m ~r ~modu =
  add (round_down_to_zero (sub m r) modu) r
