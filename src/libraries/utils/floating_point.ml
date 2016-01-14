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

type c_rounding_mode =
    FE_ToNearest | FE_Upward | FE_Downward | FE_TowardZero

let string_of_c_rounding_mode = function
  | FE_ToNearest -> "FE_NEAREST"
  | FE_Upward -> "FE_UPWARD"
  | FE_Downward -> "FE_DOWNWARD"
  | FE_TowardZero -> "FE_TOWARDZERO"

external set_round_downward: unit -> unit = "set_round_downward" "noalloc"
external set_round_upward: unit -> unit = "set_round_upward" "noalloc"
external set_round_nearest_even: unit -> unit = "set_round_nearest_even" "noalloc"
external set_round_toward_zero : unit -> unit = "set_round_toward_zero" "noalloc"
external get_rounding_mode: unit -> c_rounding_mode = "get_rounding_mode" "noalloc"
external set_rounding_mode: c_rounding_mode -> unit = "set_rounding_mode" "noalloc"

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
    Datatype.Integer.pretty num Datatype.Integer.pretty den exp min_exp max_exp;
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
    Datatype.Integer.pretty num Datatype.Integer.pretty den exp;
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
        Datatype.Integer.pretty num Datatype.Integer.pretty den
        man Datatype.Integer.pretty rem; *)
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

let reg_exp = "[eE][+]?\\(-?[0-9]+\\)"
let reg_dot = "[.]"
let reg_numopt = "\\([0-9]*\\)"
let reg_num = "\\([0-9]+\\)"

let numdotfrac = Str.regexp (reg_numopt ^ reg_dot ^ reg_numopt)
let numdotfracexp = Str.regexp (reg_numopt ^ reg_dot ^ reg_numopt ^ reg_exp)
let numexp = Str.regexp (reg_num ^ reg_exp)

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
      if exp = 0 || (firstdigit = 0 && decdigits = 0L && exp = -1022)
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
  (* should always arrive here with nearest_even *)
  if get_rounding_mode () <> FE_ToNearest then begin
    Kernel.failure "pretty: rounding mode (%s) <> FE_TONEAREST"
      (string_of_c_rounding_mode (get_rounding_mode ()));
    set_round_nearest_even();
  end;
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


type sign = Neg | Pos

exception Float_Non_representable_as_Int64 of sign

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
  fun x ->
    let max_64_float = Extlib.id max_64_float in
    if x < min_64_float
    then raise (Float_Non_representable_as_Int64 Neg);
    if x > (max_64_float +. max_64_float)
    then raise (Float_Non_representable_as_Int64 Pos);
    if x <= max_64_float then
      Integer.of_int64 (Int64.of_float x)
    else 
      Integer.add 
	(Integer.of_int64 (Int64.of_float (x +. min_64_float)))
	(Integer.two_power_of_int 63)

let bits_of_max_double =
  Integer.of_int64 (Int64.bits_of_float max_float)
let bits_of_most_negative_double =
  Integer.of_int64 (Int64.bits_of_float (-. max_float))

(** See e.g. http://www.h-schmidt.net/FloatConverter/IEEE754.html *)
let bits_of_max_float = Integer.of_int64 0x7F7FFFFFL
let bits_of_most_negative_float =
  let v = Int64.of_int32 0xFF7FFFFFl in(* cast to int32 to get negative value *)
  Integer.of_int64 v

external fround: float -> float = "c_round"
external trunc: float -> float = "c_trunc"

(** Single-precision (32-bit) functions. We round the result computed
    as a double, since float32 functions are rarely precise. *)

external expf: float -> float = "c_expf"
external logf: float -> float = "c_logf"
external log10f: float -> float = "c_log10f"
external powf: float -> float -> float = "c_powf"
external sqrtf: float -> float = "c_sqrtf"


(** C math-like functions *)

let isnan f =
  match classify_float f with
  | FP_nan -> true
  | _ -> false

let isfinite f =
  match classify_float f with
  | FP_nan | FP_infinite -> false
  | _ -> true

let min_denormal = Int64.float_of_bits 1L
let neg_min_denormal = -. min_denormal
let min_single_precision_denormal = Int32.float_of_bits 1l
let neg_min_single_precision_denormal = -. min_single_precision_denormal

(* auxiliary functions for nextafter/nextafterf *)
let min_denormal_float ~is_f32 =
  if is_f32 then min_single_precision_denormal else min_denormal
let nextafter_aux ~is_f32 fincr fdecr x y =
  if x = y (* includes cases "(0.0, -0.0) => -0.0" and its symmetric *)
  then y
  else if isnan x || isnan y then nan
  else if x = 0.0 (* or -0.0 *) then
    if x < y then min_denormal_float is_f32 else -. (min_denormal_float is_f32)
    (* the following conditions might be simpler if we had unsigned ints
       (uint32/uint64) *)
  else if x = neg_infinity (* && y = neg_infinity *) then fdecr x
  else if (x < y && x > 0.0) || (x > y && x < 0.0) then fincr x else fdecr x
let incr_f64 f =
  Int64.float_of_bits (Int64.succ (Int64.bits_of_float f))
let decr_f64 f =
  if f = infinity then max_float
  else Int64.float_of_bits (Int64.pred (Int64.bits_of_float f))
let incr_f32 f =
  if f = neg_infinity then most_negative_single_precision_float
  else Int32.float_of_bits (Int32.succ (Int32.bits_of_float f))
let decr_f32 f =
  if f = infinity then max_single_precision_float
  else Int32.float_of_bits (Int32.pred (Int32.bits_of_float f))

let nextafter x y =
  nextafter_aux ~is_f32:false incr_f64 decr_f64 x y

let nextafterf x y =
  nextafter_aux ~is_f32:true incr_f32 decr_f32 x y


(*
Local Variables:
compile-command: "make -C ../../.. byte"
End:
*)
