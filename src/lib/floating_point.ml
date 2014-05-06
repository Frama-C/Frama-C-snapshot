(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2014                                               *)
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

external set_round_downward: unit -> unit = "set_round_downward" "noalloc"
external set_round_upward: unit -> unit = "set_round_upward" "noalloc"
external set_round_nearest_even: unit -> unit = "set_round_nearest_even" "noalloc"

external round_to_single_precision_float: float -> float = "round_to_float" 
external sys_single_precision_of_string: string -> float = 
    "single_precision_of_string"
(* TODO two functions above: declare "float", 
   must have separate version for bytecode, see OCaml manual *)

let max_single_precision_float = Int32.float_of_bits 0x7f7fffffl
let most_negative_single_precision_float = -. max_single_precision_float

type parsed_float = {
  f_nearest : float ;
  f_lower : float ;
  f_upper : float ;
}

let inf ~man_size ~max_exp = 
  let biggest_not_inf = ldexp (2.0 -. ldexp 1.0 (~- man_size)) max_exp in
  { 
    f_lower = biggest_not_inf ;
    f_nearest = infinity ;
    f_upper = infinity ;
  }

(* [s = num * 2^exp / den] hold *)
let make_float ~num ~den ~exp ~man_size ~min_exp ~max_exp = 
  assert (Integer.gt num Integer.zero);
  assert (Integer.gt den Integer.zero);
(*
  Format.printf "make_float: num den exp:@\n%a@\n@\n%a@\n@\n%d@.min_exp:%d max_exp:%d@."
    (Integer.pretty ~hexa:false) num 
    (Integer.pretty ~hexa:false) den
    exp
    min_exp
    max_exp;
*)
  let size_bi = Integer.of_int man_size in
  let ssize_bi = Integer.of_int (succ man_size) in
  let min_exp = min_exp - man_size in

  let den = ref den in
  let exp = ref exp in
  while 
    Integer.ge num (Integer.shift_left !den ssize_bi)
    || !exp < min_exp
  do
    den := Integer.shift_left !den Integer.one;
    incr exp
  done;
  let den = !den in
  let shifted_den = Integer.shift_left den size_bi in
  let num = ref num in
  while 
    Integer.lt !num shifted_den && !exp > min_exp
  do
    num := Integer.shift_left !num Integer.one;
    decr exp
  done;
  let num = !num in
  let exp = !exp in
(* 
  Format.printf "make_float2: num den exp:@\n%a@\n@\n%a@\n@\n%d@."
    (Integer.pretty ~hexa:false) num 
    (Integer.pretty ~hexa:false) den
    exp;
*)
  if exp > max_exp - man_size then inf ~man_size ~max_exp
  else
    let man = Integer.native_div num den in
    let rem =    
      Integer.sub num (Integer.mul den man)
    in
    let rem2 = (* twice the remainder *)
      Integer.shift_left rem Integer.one
    in
    let man = Integer.to_int64 man in
(* Format.printf "pre-round: num den man rem:@\n%a@\n@\n%a@\n@\n%Ld@\n@\n%a@."
        (Integer.pretty ~hexa:false) num 
        (Integer.pretty ~hexa:false) den
        man
        (Integer.pretty ~hexa:false) rem; *)
    let lowb = ldexp (Int64.to_float man) exp in
    if Integer.is_zero rem2 then {
      f_lower = lowb ;
      f_nearest = lowb ;
      f_upper = lowb ;
    } else
      let upb = ldexp (Int64.to_float (Int64.succ man)) exp in
      if Integer.lt rem2 den ||
        (Integer.equal rem2 den && (Int64.logand man Int64.one) = 0L)
      then {
	f_lower = lowb ;
	f_nearest = lowb ;
	f_upper = upb ;
      }
      else {
	f_lower = lowb ;
	f_nearest = upb ;
	f_upper = upb ;
      }

let exp = "[eE][+]?\\(-?[0-9]+\\)"
let dot = "[.]"
let numopt = "\\([0-9]*\\)"
let num = "\\([0-9]+\\)"

let numdotfrac = Str.regexp (numopt ^ dot ^ numopt)
let numdotfracexp = Str.regexp (numopt ^ dot ^ numopt ^ exp)
let numexp = Str.regexp (num ^ exp)

exception Shortcut of parsed_float

let zero = { f_lower = 0.0 ; f_nearest = 0.0 ; f_upper = 0.0 }

(* [man_size] is the size of the mantissa, [min_exp] the frontier exponent
   between normalized and denormalized numbers *)
let parse_float ~man_size ~min_exp ~max_exp s =
  (*  Format.printf "parse: %s@." s; *)
  let match_exp group =
    let s = Str.matched_group group s in
    try
      int_of_string s
    with Failure _ ->
      (* Format.printf "Error in exponent: %s@." s; *)
      if s.[0] = '-'
      then raise (Shortcut { 
		    f_lower = 0.0 ; 
		    f_nearest = 0.0 ;
		    f_upper = ldexp 1.0 (min_exp - man_size) ;
		  })
      else raise (Shortcut (inf ~man_size ~max_exp))
  in
    try
      (* At the end of the function, [s = num * 2^exp / den] *)
      let num, den, exp =
	if Str.string_match numdotfracexp s 0
	then
	  let n = Str.matched_group 1 s in
	  let frac = Str.matched_group 2 s in
	  let len_frac = String.length frac in
	  let num = Integer.of_string (n ^ frac) in
	  let den = Integer.power_int_positive_int 5 len_frac in
          if Integer.is_zero num then raise (Shortcut zero);
	  let exp10 = match_exp 3
	  in
	  if exp10 >= 0
	  then
	    Integer.mul num (Integer.power_int_positive_int 5 exp10),
            den,
            exp10 - len_frac
	  else
	    num,
            Integer.mul den (Integer.power_int_positive_int 5 (~- exp10)),
            exp10 - len_frac
	else if Str.string_match numdotfrac s 0
	then 
	  let n = Str.matched_group 1 s in
	  let frac = Str.matched_group 2 s in
	  let len_frac = String.length frac in
	  Integer.of_string (n ^ frac),
          Integer.power_int_positive_int 5 len_frac,
          ~- len_frac
	else if Str.string_match numexp s 0
	then
	  let n = Str.matched_group 1 s in
	  let num = Integer.of_string n in
          if Integer.is_zero num then raise (Shortcut zero);
	  let exp10 = match_exp 2 in
	  if exp10 >= 0
	  then
	    Integer.mul num (Integer.power_int_positive_int 5 exp10),
            Integer.one,
            exp10
   	  else
	    num,
           (Integer.power_int_positive_int 5 (~- exp10)),
           exp10
	else (Format.printf "Could not parse floating point number %S@." s;
	      assert false)
      in
      if Integer.is_zero num 
      then zero
      else
	make_float ~num ~den ~exp ~man_size ~min_exp ~max_exp
    with Shortcut r -> r

let is_hex s =
  let l = String.length s in
  l >= 2 && s.[0] = '0' && (s.[1] = 'x' || s.[1] = 'X')

let single_precision_of_string s = 
  if is_hex s
  then 
    let f = sys_single_precision_of_string s in 
    { f_lower = f ; f_nearest = f ; f_upper = f }
  else (* decimal *)
    parse_float ~man_size:23 ~min_exp:(-126) ~max_exp:127 s

let double_precision_of_string s = 
  if is_hex s
  then 
    let f = float_of_string s in
    { f_lower = f ; f_nearest = f ; f_upper = f }
  else (* decimal *)
    parse_float ~man_size:52 ~min_exp:(-1022) ~max_exp:1023 s

let parse_kind kind string =
  match kind with
  | Cil_types.FFloat -> single_precision_of_string string
  | Cil_types.FDouble | Cil_types.FLongDouble ->
    double_precision_of_string string

let pretty_normal ~use_hex fmt f =
  let double_norm = Int64.shift_left 1L 52 in
  let double_mask = Int64.pred double_norm in
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


exception Float_Non_representable_as_Int64

(* If the argument [x] is not in the range [min_64_float, 2*max_64_float],
   raise Float_Non_representable_as_Int64. This is the most reasonable as
   a floating-point number may represent an exponentially large integer. *)
let truncate_to_integer =
  let min_64_float = -9.22337203685477581e+18 
           (* Int64.to_float (-0x8000000000000000L) *) 
  in
  let max_64_float = 9.22337203685477478e+18 
(*    let open Int64 in
    float_of_bits (pred (bits_of_float (to_float max_int))) *)
  in
  let float_non_representable_as_int64 = Float_Non_representable_as_Int64 in
  fun x ->
    let max_64_float = Extlib.id max_64_float in
    if x < min_64_float || x > (max_64_float +. max_64_float)
    then raise float_non_representable_as_int64;
    if x <= max_64_float then
      Integer.of_int64 (Int64.of_float x)
    else 
      Integer.add 
	(Integer.of_int64 (Int64.of_float (x +. min_64_float)))
	(Integer.two_power_of_int 63)

(*
Local Variables:
compile-command: "make -C ../.. byte"
End:
*)
