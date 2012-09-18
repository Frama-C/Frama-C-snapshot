(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2012                                               *)
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

external set_round_downward: unit -> unit = "set_round_downward"
external set_round_upward: unit -> unit = "set_round_upward"
external set_round_nearest_even: unit -> unit = "set_round_nearest_even"

external round_to_single_precision_float: float -> float = "round_to_float"

external sys_single_precision_of_string: string -> float = 
    "single_precision_of_string"

let max_single_precision_float = Int32.float_of_bits 0x7f7fffffl
let most_negative_single_precision_float = -. max_single_precision_float

exception Infinity
exception Zero

type parsed_float = Exact of float | Inexact of float

let make_float ~num ~den ~exp ~man_size ~min_exp = 
  assert (My_bigint.gt num My_bigint.zero);
  assert (My_bigint.gt den My_bigint.zero);

  let size_bi = My_bigint.of_int man_size in
  let ssize_bi = My_bigint.of_int (succ man_size) in
  let min_exp = min_exp - man_size in

  let den = ref den in
  let exp = ref exp in
  while 
    My_bigint.ge
      num 
      (My_bigint.shift_left !den ssize_bi)
  do
    den := My_bigint.shift_left !den My_bigint.one;
    incr exp
  done;
  let den = !den in
  let shifted_den = My_bigint.shift_left den size_bi in
  let num = ref num in
  while 
    My_bigint.lt !num shifted_den && !exp > min_exp
  do
    num := My_bigint.shift_left !num My_bigint.one;
    decr exp
  done;
  let num = !num in
  let exp = !exp in

  let man = My_bigint.native_div num den in
  let rem =    
    My_bigint.sub num (My_bigint.mul den man)
  in
  let rem2 = (* twice the remainder *)
    My_bigint.shift_left rem My_bigint.one
  in
  let man = My_bigint.to_int64 man in
(*  Format.printf "pre-rounding: num den man rem: %a %a %Ld %a@."
    (My_bigint.pretty ~hexa:false) num 
    (My_bigint.pretty ~hexa:false) den
    man
    (My_bigint.pretty ~hexa:false) rem; *)
  let man =
    if My_bigint.lt rem2 den
    then man
    else if My_bigint.gt rem2 den
    then Int64.succ man
    else Int64.add man (Int64.logand man Int64.one)
  in
  let fl = 
    ldexp (Int64.to_float man) exp
  in
  if My_bigint.is_zero rem2  then Exact(fl) else Inexact(fl)

let exp = "[eE][+]?\\(-?[0-9]+\\)"
let dot = "[.]"
let numopt = "\\([0-9]*\\)"
let num = "\\([0-9]+\\)"

let numdotfrac = Str.regexp (numopt ^ dot ^ numopt)
let numdotfracexp = Str.regexp (numopt ^ dot ^ numopt ^ exp)
let numexp = Str.regexp (num ^ exp)

let parse_float ~man_size ~min_exp s =
  (*  Format.printf "parse: %s@." s; *)
  let match_exp group =
    let s = Str.matched_group group s in
    try
      int_of_string s
    with Failure _ ->
(*      Format.printf "Error in exponent: %s@." s; *)
      if s.[0] = '-' then raise Zero else raise Infinity
  in
    try
      let num, den, exp =
	if Str.string_match numdotfracexp s 0
	then
	  let n = Str.matched_group 1 s in
	  let frac = Str.matched_group 2 s in
	  let len_frac = String.length frac in
	  let num = My_bigint.of_string (n ^ frac) in
	  let den = My_bigint.power_int_positive_int 5 len_frac in
	  let exp10 = match_exp 3
	  in
	  if exp10 >= 0
	  then
	    My_bigint.mul 
	      num 
	      (My_bigint.power_int_positive_int 5 exp10),
        den,
        exp10 - len_frac
	  else
	    num,
        My_bigint.mul 
	  den
          (My_bigint.power_int_positive_int 5 (~- exp10)),
        exp10 - len_frac
	else if Str.string_match numdotfrac s 0
	then 
	  let n = Str.matched_group 1 s in
	  let frac = Str.matched_group 2 s in
	  let len_frac = String.length frac in
	  My_bigint.of_string (n ^ frac),
        My_bigint.power_int_positive_int 5 len_frac,
        ~- len_frac
	else if Str.string_match numexp s 0
	then
	  let n = Str.matched_group 1 s in
	  let exp10 = match_exp 2 in
	  let num = My_bigint.of_string n in
	  if exp10 >= 0
	  then
	    My_bigint.mul num (My_bigint.power_int_positive_int 5 exp10),
        My_bigint.one,
        exp10
   	  else
	    num,
        (My_bigint.power_int_positive_int 5 (~- exp10)),
        exp10
	else assert false
      in
      if My_bigint.is_zero num 
      then Exact (0.0)
      else
	make_float ~num ~den ~exp ~man_size ~min_exp 
    with Zero -> Inexact (0.0)
    | Infinity -> Inexact (infinity)

let is_hex s =
  let l = String.length s in
  l >= 2 && s.[0] = '0' && (s.[1] = 'x' || s.[1] = 'X')

let single_precision_of_string s = 
  if is_hex s
  then Exact (sys_single_precision_of_string s)
  else (* decimal *)
    let f = parse_float ~man_size:23 ~min_exp:(-126) s in    
    match f with 
      Exact fl | Inexact fl ->
	if fl <= max_single_precision_float
	then f
	else Inexact(infinity)

let double_precision_of_string s = 
  if is_hex s
  then Exact(float_of_string s)
  else (* decimal *)
    parse_float ~man_size:52 ~min_exp:(-1022) s


let double_norm = Int64.shift_left 1L 52
let double_mask = Int64.pred double_norm

let pretty_normal ~use_hex fmt f =
  let i = Int64.bits_of_float f in
  let s = 0L <> (Int64.logand Int64.min_int i) in
  let i = Int64.logand Int64.max_int i in
  let exp = Int64.to_int (Int64.shift_right_logical i 52) in
  let man = Int64.logand i double_mask in
  let s = if s then "-" else "" in
  if exp = 2047
  then begin
      if man = 0L
      then 
	Format.fprintf fmt "%sinf" s
      else
	Format.fprintf fmt "NaN"
    end
  else
  let firstdigit, exp =
    if exp <> 0
    then 1, (exp - 1023)
    else 0, -1022
  in
  if not use_hex
  then begin
      let firstdigit, man, exp =
	if 0 < exp && exp <= 12
	then begin
	    Int64.to_int
	      (Int64.shift_right_logical
		  (Int64.logor man double_norm)
		  (52 - exp)),
  	    Int64.logand (Int64.shift_left man exp) double_mask,
	    0
	  end
	else firstdigit, man, exp
      in
      let d =
	Int64.float_of_bits
	  (Int64.logor 0x3ff0000000000000L man)
      in
      let d, re =
	if d >= 1.5
	then d -. 1.5, 5000000000000000L
	else d -. 1.0, 0L
      in
      let d = d *. 1e16 in
      let decdigits = Int64.add re (Int64.of_float d) in
      if exp = 0
      then
	Format.fprintf fmt "%s%d.%016Ld"
	  s
	  firstdigit
	  decdigits
      else
	Format.fprintf fmt "%s%d.%016Ld*2^%d"
	  s
	  firstdigit
	  decdigits
	  exp
    end
  else
    Format.fprintf fmt "%s0x%d.%013Lxp%d"
      s
      firstdigit
      man
      exp

let pretty fmt f =
  let use_hex = Kernel.FloatHex.get() in
  set_round_nearest_even();
  if use_hex || (Kernel.FloatNormal.get ())
  then
    pretty_normal ~use_hex fmt f
  else begin
      let r = Format.sprintf "%.*g" 12 f in
      if (String.contains r '.' || String.contains r 'e' ||
	     String.contains r 'E')
	|| (match classify_float f with
	| FP_normal | FP_subnormal | FP_zero -> false
	| FP_infinite | FP_nan -> true)
      then Format.pp_print_string fmt r
      else Format.fprintf fmt "%s." r
    end

(*
Local Variables:
compile-command: "make -C ../.. byte"
End:
*)
