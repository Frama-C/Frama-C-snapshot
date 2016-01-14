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

open Abstract_interp

module F = struct

  type t = float

  let packed_descr = Structural_descr.p_float

  (** NOTE: all floating-point comparisons using OCaml's standard operators
      do NOT distinguish between -0.0 and 0.0.
      Whenever floats are compared using them, it implies that negative zeroes
      are also considered, e.g. "if x < 0.0" is equivalent to "if x < -0.0",
      which is also equivalent to "F.compare x (-0.0) < 0".
      This 'compare' operator distinguishes -0. and 0. *)
  external compare : float -> float -> int = "float_compare_total" "noalloc"
  let equal f1 f2 = compare f1 f2 = 0

  (* The Caml version of compare below is fine but the C version above is
     faster and does not allocate—it would be possible for the Caml version
     to avoid allocation, but OCaml 4.00.1 allocates 80 bytes, for instance *)
  (*  let compare f1 f2 =
        let i1 = Int64.bits_of_float f1 in
        let i2 = Int64.bits_of_float f2 in
        let m1 = (Int64.logand i1 Int64.min_int) in
        let m2 = (Int64.logand i2 Int64.min_int) in
        if m1 = m2
        then compare f1 f2
        else compare m1 m2 *)

  let le f1 f2 = compare f1 f2 <= 0

  let min f1 f2 =
    if le f1 f2 then f1 else f2

  let max f1 f2 =
    if le f1 f2 then f2 else f1

  let equal_ieee = ((=) : float -> float -> bool)

  let hash = Hashtbl.hash


  let zero = 0.0
  let minus_zero = -0.0

  let max_single_precision_float = Floating_point.max_single_precision_float
  let most_negative_single_precision_float =
    Floating_point.most_negative_single_precision_float
  let max_float = max_float
  let infinity = infinity
  let neg_infinity = neg_infinity
  let most_negative_float = -. max_float

  (* Maximum integer value M such that all 0 < n < M are exactly
     representable as doubles. *)
  let max_precise_integer = 2. ** 53.

  (* works but allocates:
     let is_negative f = Int64.bits_of_float f < Int64.zero *)
  external is_negative : float -> bool = "float_is_negative" "noalloc"

  let zero_of_same_sign f =
    if is_negative f then minus_zero else zero

  let is_infinity = (=) infinity
  let is_neg_infinity = (=) neg_infinity

  (* Must *not* be exported. All functions of this module should check the
     arguments with which they call the functions labelled "may raise Nan
     exception" *)
  exception NaN

  (* May raise NaN exception *)
  let ensure_not_nan r =
    match classify_float r with
    | FP_nan -> raise NaN
    | FP_normal | FP_subnormal | FP_infinite | FP_zero -> r

  let ensure_not_nan_unary f x = ensure_not_nan (f x)

  let ensure_not_nan_binary f x y = ensure_not_nan (f x y)

  let add = ensure_not_nan_binary (+.)
  let sub = ensure_not_nan_binary (-.)
  let neg = ensure_not_nan_unary (~-.)
  let mult = ensure_not_nan_binary ( *.)

  (* May raise NaN exception on zero divisor *)
  let div = ensure_not_nan_binary (/.)

  let pretty_normal = Floating_point.pretty_normal

  let pretty = Floating_point.pretty

  let avg x y =
    let h = 0.5 in
    let xp = x >= 0. in
    let yp = y >= 0. in
    if xp = yp
    then
      let d = x -. y in y +. h *. d
    else
      (x +. y) *. h

  let le_ieee = ((<=) : float -> float -> bool)
  let lt_ieee = ((<) : float -> float -> bool)

  let sqrt = (* See bts #1396. We patch Pervasives function only when needed *)
    if compare (sqrt minus_zero) minus_zero <> 0 then
      fun v ->
        if v = minus_zero
        then v
        else sqrt v
    else
      sqrt

  (* May raise NaN exception on negative arguments *)
  let sqrt = ensure_not_nan_unary sqrt

  let cos = ensure_not_nan_unary cos
  let sin = ensure_not_nan_unary sin
  let exp = ensure_not_nan_unary exp

  (* May raise NaN exception on negative or zero arguments *)
  let log = ensure_not_nan_unary log
  let log10 = ensure_not_nan_unary log10

  let floor = ensure_not_nan_unary floor
  let ceil = ensure_not_nan_unary ceil
  let trunc = ensure_not_nan_unary Floating_point.trunc
  let fround = ensure_not_nan_unary Floating_point.fround

  let atan2 = ensure_not_nan_binary atan2

  (* May raise NaN exception *)
  let pow = ensure_not_nan_binary ( ** )

  (* May raise NaN exception on zero second argument *)
  let fmod = ensure_not_nan_binary mod_float

  (* single-precision *)
  let expf = ensure_not_nan_unary Floating_point.expf
  let logf = ensure_not_nan_unary Floating_point.logf
  let log10f = ensure_not_nan_unary Floating_point.log10f
  let powf = ensure_not_nan_binary Floating_point.powf
  let sqrtf = ensure_not_nan_unary Floating_point.sqrtf

  let minus_one = -1.0
  let one = 1.0
  let minus_one_half = -0.5
  let ten = 10.
  let m_pi = 3.1415929794311523 (* single-precision *)
  let m_minus_pi = -. m_pi
  let m_pi_2 = 1.5707964897155761 (* single-precision *)
  let m_minus_pi_2 = -. m_pi_2
  let ff = 4.5
  let minus_ff = -4.5

  let of_int = float_of_int

  let widen_up f =
    if f <= zero then zero
    else if f <= one then one
    else if f <= m_pi_2 then m_pi_2
    else if f <= m_pi then m_pi
    else if f <= ten then ten
    else if f <= 1e10 then 1e10
    else if f <= max_single_precision_float then max_single_precision_float
    else if f <= 1e80 then 1e80
    else max_float

  let widen_down f =
    if f >= zero then zero
    else if f >= minus_one_half then minus_one_half
    else if f >= minus_one then minus_one
    else if f >= m_minus_pi then m_minus_pi
    else if f >=  most_negative_single_precision_float
    then most_negative_single_precision_float
    else most_negative_float

  let next_previous_normal int64fup int64fdown float =
    let r = Int64.bits_of_float float in
    let f =
      if r >= 0L then
        int64fup
      else
        int64fdown
    in
    Int64.float_of_bits (f r)

  let next_previous int64fup int64fdown float =
    match classify_float float with
    | FP_nan -> raise NaN
    | FP_infinite -> float
    | FP_normal | FP_subnormal -> begin
      let f = next_previous_normal int64fup int64fdown float in
      match classify_float f with
      | FP_nan -> assert false (* can only be produced from an infinity *)
      | FP_infinite | FP_normal | FP_subnormal | FP_zero -> f
      end
    | FP_zero ->
      (next_previous_normal int64fup int64fdown (float +. min_float)) -. min_float

  let next_float = next_previous Int64.succ Int64.pred
  let prev_float = next_previous Int64.pred Int64.succ

  let id = fun x -> x
  let of_float = ensure_not_nan_unary id
  let to_float = id

  let classify_float = Pervasives.classify_float
end

module F_Set = Set.Make(F) (* Uses our really total compare function *)

type float_kind = Float32 | Float64

exception Non_finite

(* Alarms produced by built-ins. *)
type builtin_alarm = APosInf | ANegInf | ANaN of string | AAssume of string
module Builtin_alarms = Set.Make(struct
    type t = builtin_alarm
    let compare (x : t) (y : t) = Pervasives.compare x y
  end)

let no_alarm = Builtin_alarms.empty
let an_alarm a = Builtin_alarms.singleton a

let next_after fkind x y = match fkind with
  | Float32 -> Floating_point.nextafterf x y
  | Float64 -> Floating_point.nextafter x y

let max_representable_float = function
  | Float32 -> F.max_single_precision_float
  | Float64 -> F.max_float

let most_negative_representable_float = function
  | Float32 -> F.most_negative_single_precision_float
  | Float64 -> F.most_negative_float


type denormal_treatment = Denormals | FTZ | DenormalsandFTZ

let denormal_treatment = Denormals
let _ = DenormalsandFTZ (* VP: silence warning about unused DenormalsandFTZ *)

module FRange : sig
  type t = private I of F.t * F.t
  val inject : F.t -> F.t -> t
  val inject_r_f : float_kind -> F.t -> F.t -> (bool * bool * t)
end =
struct

  type t = I of F.t * F.t

  let inject b e =
    if not (F.le b e) then
      Kernel.fatal "Invalid bounds for float interval@\n%a .. %a@."
        (F.pretty_normal ~use_hex:true) b (F.pretty_normal ~use_hex:true) e;
    I(b, e)

  (* If [fkind] is [Float32], we check that [b] and [e] are valid
     32-bit representations: lower bits are 0, and the value fits inside
     a 32-bit float. *)
  let check_representability fkind b e =
    if fkind = Float32 &&
       (Floating_point.round_to_single_precision_float b <> b ||
        Floating_point.round_to_single_precision_float e <> e)
    then
      Kernel.fatal "Ival: invalid float32, b=%g (%a) e=%g (%a)"
        b (Floating_point.pretty_normal ~use_hex:true) b
        e (Floating_point.pretty_normal ~use_hex:true) e

  let inject_r_f fkind b e =
    if F.is_neg_infinity e || F.is_infinity b then raise Non_finite;
    let infinite_e, e =
      match F.classify_float e with
      | FP_infinite ->
        let pos = F.le_ieee F.zero e in
        if pos then
          true, max_representable_float fkind
        else
          raise Non_finite
      | FP_subnormal ->
        let pos = F.le_ieee F.zero e in begin
          match pos with
          | true when denormal_treatment = FTZ ->
            false, F.zero
          | false when denormal_treatment <> Denormals ->
            false, F.minus_zero
          | _ -> false, e
        end
      | FP_normal | FP_zero -> false, e
      | FP_nan -> assert false
    in
    let infinite_b, b =
      match F.classify_float b with
      | FP_infinite ->
        let pos = F.le_ieee F.zero b in
        if pos then
          raise Non_finite
        else
          true, most_negative_representable_float fkind
      | FP_subnormal ->
        let pos = F.le_ieee F.zero b in begin
          match pos with
          | false when denormal_treatment = FTZ ->
            false, F.minus_zero
          | true when denormal_treatment <> Denormals ->
            false, F.zero
          | _ -> false, b
        end
      | FP_normal | FP_zero -> false, b
      | FP_nan -> assert false
    in
    check_representability fkind b e;
    infinite_b, infinite_e, inject b e

end

type t = FRange.t
(* open Private_Couple *) (* Workaround for Ocaml bug 5718 *)

type builtin_res = Builtin_alarms.t * FRange.t Abstract_interp.Bot.or_bottom

let structural_descr =
  Structural_descr.t_sum [| [| F.packed_descr; F.packed_descr |] |]

let packed_descr = Structural_descr.pack structural_descr

let inject = FRange.inject

let inject_r_f = FRange.inject_r_f

let inject_r b e =
  let ib, ie, r = FRange.inject_r_f Float64 b e in
  ib || ie, r

let inject_f fkind b e = let _,_,r = (inject_r_f fkind b e) in r

let min_and_max (FRange.I(b,e)) = b, e

let top_f fkind = inject (most_negative_representable_float fkind)
    (max_representable_float fkind)

let top = top_f Float64

let compare (FRange.I(b1,e1)) (FRange.I(b2,e2)) =
  let r = F.compare b1 b2 in
  if r <> 0 then r else F.compare e1 e2

let equal (FRange.I(b1,e1)) (FRange.I(b2,e2)) =
  F.equal b1 b2 && F.equal e1 e2

let pretty_aux pp_min pp_max fmt (FRange.I(b,e)) =
  if F.equal b e then
    Format.fprintf fmt "{%a}" F.pretty b
  else begin
    if (Kernel.FloatRelative.get())
    then begin
      Floating_point.set_round_upward ();
      let d = F.sub e b in
      Floating_point.set_round_nearest_even ();
      Format.fprintf fmt "[%a ++ %a]"
        F.pretty b
        F.pretty d
    end
    else begin
      Format.fprintf fmt "[%a .. %a]" pp_min b pp_max e
    end
  end

let pretty = pretty_aux F.pretty F.pretty
let pretty_overflow =
  let pp_aux bound fmt f =
    if F.equal bound f
    then Format.pp_print_string fmt "--."
    else F.pretty fmt f
  in
  let pp_min = pp_aux F.most_negative_float in
  let pp_max = pp_aux F.max_float in
  pretty_aux pp_min pp_max


let hash (FRange.I(b,e)) =
  F.hash b + (5 * F.hash e)

(* True only iff the interval contains at least one finite number. *)
let has_finite (FRange.I(b, e)) =
  let is_finite f = match classify_float f with
    | FP_nan | FP_infinite -> false | _ -> true
  in
  match is_finite b, is_finite e with
  | false, false -> (* check if the interval is not [-oo,+oo] *)
    F.is_neg_infinity b && F.is_infinity e
  | _, _ -> true

let inject_singleton x = inject x x

let one = inject_singleton F.one

let zero = inject_singleton F.zero

let minus_zero = inject_singleton F.minus_zero

let compare_min (FRange.I(m1,_)) (FRange.I(m2,_)) =
  F.compare m1 m2

let compare_max (FRange.I(_, m1)) (FRange.I(_, m2)) =
  F.compare m2 m1

let is_included (FRange.I(b1, e1)) (FRange.I(b2, e2)) =
  F.le b2 b1 && F.le e1 e2

let join (FRange.I(b1, e1)) (FRange.I(b2, e2)) =
  inject (F.min b1 b2) (F.max e1 e2)

let join_or_bottom = Bot.join_or_bottom join

let meet (FRange.I(b1, e1)) (FRange.I(b2, e2)) =
  if F.le b2 e1 && F.le b1 e2
  then `Value (inject (F.max b1 b2) (F.min e1 e2))
  else `Bottom

let contains_zero = is_included zero
let contains_minus_zero = is_included minus_zero

(* Returns true if [f] is certainly a zero (positive, negative or both). *)
let is_a_zero f = is_included f (inject F.minus_zero F.zero)

let fold_split n f (FRange.I(b, e)) acc =
  let bound = ref b in
  let acc = ref acc in
  begin
    for i = n downto 2 do
      let new_bound = F.add !bound (F.div (F.sub e !bound) (F.of_int i)) in
      acc := f (inject !bound new_bound) !acc;
      bound := new_bound
    done;
  end;
  (*    Format.printf "float fold_split %a@."
        pretty (!bound, e); *)
  f (inject !bound e) !acc

let contains_a_zero (FRange.I(b, e)) =
  F.le_ieee b F.zero && F.le_ieee F.zero e

let is_zero f =
  0 = compare zero f

let is_singleton (FRange.I(b, e)) = F.equal b e

let neg (FRange.I(b, e)) =
  inject (F.neg e) (F.neg b) (* do not round because exact operation *)

let abs (FRange.I(b, e) as f) =
  if contains_zero f then
    inject F.zero (F.max (abs_float b) (abs_float e))
  else
  if F.compare e F.zero < 0 then neg f else f


type rounding_mode = Any | Nearest_Even


let top_single_precision_float =
  inject
    F.most_negative_single_precision_float
    F.max_single_precision_float

let round_to_single_precision_float ~rounding_mode (FRange.I(b, e)) =
  if rounding_mode = Any
  then Floating_point.set_round_downward ()
  else Floating_point.set_round_nearest_even ();
  let b = Floating_point.round_to_single_precision_float b in
  if rounding_mode = Any then Floating_point.set_round_upward ();
  let e = Floating_point.round_to_single_precision_float e in
  if rounding_mode = Any then Floating_point.set_round_nearest_even ();
  let infb, b =
    match classify_float b, denormal_treatment with
    | FP_infinite, _ ->
      if F.equal_ieee b F.infinity
      then raise Non_finite;
      true, F.most_negative_single_precision_float
    | FP_subnormal, FTZ -> false, F.zero_of_same_sign b
    | FP_subnormal, DenormalsandFTZ when not (F.is_negative b) ->
      false, F.zero
    | _ -> false, b
  in
  let infe, e =
    match classify_float e, denormal_treatment with
    | FP_infinite, _ ->
      if F.equal_ieee e F.neg_infinity
      then raise Non_finite;
      true, F.max_single_precision_float
    | FP_subnormal, FTZ -> false, F.zero_of_same_sign e
    | FP_subnormal, DenormalsandFTZ when F.is_negative e ->
      false, F.minus_zero
    | _ -> false, e
  in
  infb || infe, inject b e
(*  Format.printf "Casting double -> float %a -> %B %a@."
     pretty _arg fl pretty _res; fl, _res *)


(* Bitwise reinterpretation of a double to a 64-bit integer. signedness of the
   integer is defined by ~signed *)
let bits_of_float64 ~signed (FRange.I(l, u)) =
  if F.is_negative u
  then begin
    if signed then
      Int.of_int64 (Int64.bits_of_float u),
      Int.of_int64 (Int64.bits_of_float l)
    else
      Int.(add_2_64 (of_int64 (Int64.bits_of_float u))),
      Int.(add_2_64 (of_int64 (Int64.bits_of_float l)))
  end
  else if F.is_negative l
  then begin
    if signed then
      Int.of_int64 Int64.min_int,
      Int.of_int64 (Int64.bits_of_float u)
    else
      Int.zero,
      Int.(add_2_64 (of_int64 (Int64.bits_of_float l)))
  end
  else
    Int.of_int64 (Int64.bits_of_float l),
    Int.of_int64 (Int64.bits_of_float u)

(* Bitwise reinterpretation of a float to a 32-bit integer. signedness of the
   integer is defined by ~signed *)
let bits_of_float32 ~signed (FRange.I(l, u)) =
  assert (F.equal l (Floating_point.round_to_single_precision_float l));
  assert (F.equal u (Floating_point.round_to_single_precision_float u));
  if F.is_negative u
  then begin
    if signed then
      Int.of_int32 (Int32.bits_of_float u),
      Int.of_int32 (Int32.bits_of_float l)
    else
      Int.(add_2_32 (of_int32 (Int32.bits_of_float u))),
      Int.(add_2_32 (of_int32 (Int32.bits_of_float l)))
  end
  else
  if F.is_negative l
  then begin
    if signed then
      Int.of_int32 Int32.min_int,
      Int.of_int32 (Int32.bits_of_float u)
    else
      Int.zero,
      Int.(add_2_32 (of_int32 (Int32.bits_of_float l)))
  end
  else
    Int.of_int32 (Int32.bits_of_float l),
    Int.of_int32 (Int32.bits_of_float u)


let add rounding_mode (FRange.I(b1, e1)) (FRange.I(b2, e2)) =
  if rounding_mode = Any
  then Floating_point.set_round_downward ()
  else Floating_point.set_round_nearest_even ();
  let bs = F.add b1 b2 in
  if rounding_mode = Any then Floating_point.set_round_upward ();
  let es = F.add e1 e2 in
  if rounding_mode = Any then Floating_point.set_round_nearest_even ();
  inject_r bs es

let sub rounding_mode v1 v2 = add rounding_mode v1 (neg v2)

let mul rounding_mode (FRange.I(b1, e1)) (FRange.I(b2, e2)) =
  if rounding_mode = Any
  then Floating_point.set_round_downward ()
  else Floating_point.set_round_nearest_even ();
  let a = F.mult b1 b2 in
  let b = F.mult b1 e2 in
  let c = F.mult e1 b2 in
  let d = F.mult e1 e2 in
  let min = F.min (F.min a b) (F.min c d) in
  let max =
    if rounding_mode = Any then begin
      Floating_point.set_round_upward ();
      let a = F.mult b1 b2 in
      let b = F.mult b1 e2 in
      let c = F.mult e1 b2 in
      let d = F.mult e1 e2 in
      Floating_point.set_round_nearest_even ();
      F.max (F.max a b) (F.max c d);
    end
    else
      F.max (F.max a b) (F.max c d)
  in
  inject_r min max

(** Assumes that [v2] does not contain zero. No NaN can be created by
    F.div in this case. *)
let div_non_zero rounding_mode (FRange.I(b1, e1)) (FRange.I(b2, e2)) =
  if rounding_mode = Any
  then Floating_point.set_round_downward ()
  else Floating_point.set_round_nearest_even ();
  let c1 = F.div b1 b2 in
  let c2 = F.div b1 e2 in
  let c3 = F.div e1 b2 in
  let c4 = F.div e1 e2 in
  let min = F.min (F.min c1 c2) (F.min c3 c4) in
  let max =
    if rounding_mode = Any then begin
      Floating_point.set_round_upward ();
      let c1 = F.div b1 b2 in
      let c2 = F.div b1 e2 in
      let c3 = F.div e1 b2 in
      let c4 = F.div e1 e2 in
      Floating_point.set_round_nearest_even ();
      F.max (F.max c1 c2) (F.max c3 c4)
    end
    else F.max (F.max c1 c2) (F.max c3 c4)
  in
  inject_r min max

let div rounding_mode v1 v2 =
  if is_a_zero v2 then
    raise Non_finite
  else
  if contains_zero v2 then
    true, top (* could be improved when v2 is strictly positive or negative.
                 However, if it very difficult to produce +0. without
                 -0. or the converse. Thus, this is not worth the effort. *)
  else div_non_zero rounding_mode v1 v2

let nan_sqrt = an_alarm (ANaN "negative argument")
let nan_sqrt_assume =
  Builtin_alarms.add (AAssume "non-negative argument") nan_sqrt

(* [sqrt_f] is the actual function computing the (exact) square root,
   in single precision (sqrtf) or double precision (sqrt). *)
let sqrt' sqrt_f rounding_mode (FRange.I(b, e)) =
  if F.lt_ieee e F.zero then
    nan_sqrt, `Bottom
  else
    let alarm, min =
      if F.le_ieee F.zero b
      then begin
        if rounding_mode = Any
        then Floating_point.set_round_downward ()
        else Floating_point.set_round_nearest_even ();
        let min = sqrt_f b in
        if rounding_mode = Any then Floating_point.set_round_nearest_even ();
        no_alarm, min
      end
      else
        (* case e < 0 treated above, some values are positive or zero *)
        nan_sqrt_assume, F.minus_zero
    in
    if rounding_mode = Any then Floating_point.set_round_upward ();
    let max = sqrt_f e in
    if rounding_mode = Any then Floating_point.set_round_nearest_even ();
    alarm, `Value (inject min max)

let sqrt = sqrt' F.sqrt
let sqrtf = sqrt' F.sqrtf

let minus_one_one = inject F.minus_one F.one
let minus_pi_pi = inject F.m_minus_pi F.m_pi

let cos (FRange.I(b, e)) =
  if F.equal b e then begin
    Floating_point.set_round_nearest_even ();
    let c = F.cos b in
    inject c c
  end
  else minus_one_one

let sin (FRange.I(b, e)) =
  if F.equal b e then begin
    Floating_point.set_round_nearest_even ();
    let c = F.sin b in
    inject c c
  end
  else minus_one_one

let atan2 (FRange.I(b1, e1)) (FRange.I(b2, e2)) =
  Floating_point.set_round_nearest_even ();
  let res =
    if F.equal b1 e1 && F.equal b2 e2 then begin
      let c = F.atan2 b1 b2 in
      inject c c
    end
    else
      (* Unless y ([b1,e1]) crosses the x-axis, atan2 is continuous,
         and its minimum/maximum are at the ends of the intervals of x and y.
         Otherwise, the result is [-pi,+pi]. *)
    if not (F.compare b1 F.zero < 0 && F.compare e1 F.minus_zero > 0) then
      let a1, a2, a3, a4 = F.atan2 b1 b2, F.atan2 b1 e2,
                           F.atan2 e1 b2, F.atan2 e1 e2
      in
      let b = F.min a1 (F.min a2 (F.min a3 a4)) in
      let e = F.max a1 (F.max a2 (F.max a3 a4)) in
      inject b e
    else
      minus_pi_pi
  in (no_alarm(* never emits alarms *), `Value res)

(* Returns true iff [f] represents an integer. *)
let is_integer f = f = (F.trunc f)

(* Returns true iff [f] represents an odd integer. *)
let is_odd f = abs_float (mod_float f 2.0) = 1.0

(* Maximum odd integer representable in float64 (2^53-1). *)
let max_double_odd_integer = 9007199254740991.0
(* Maximum odd integer representable in float32 (2^24-1). *)
let max_single_odd_integer = 16777215.0
(* Most negative odd integer representable in float64. *)
let most_negative_double_odd_integer = -.max_double_odd_integer
(* Most negative odd integer representable in float32. *)
let most_negative_single_odd_integer = -.max_single_odd_integer

let max_odd_integer fkind =
  match fkind with
  | Float64 -> max_double_odd_integer
  | Float32 -> max_single_odd_integer

let most_negative_odd_integer fkind =
  match fkind with
  | Float64 -> most_negative_double_odd_integer
  | Float32 -> most_negative_single_odd_integer

(* Returns [Some (⌈b⌉, ⌊e⌋), if ⌈b⌉ <= ⌊e⌋, or None otherwise. *)
let enclosed_integer_range (FRange.I(b, e)) =
  let ib, ie = ceil b, floor e in
  if ib <= ie then Some (inject ib ie)
  else None

(* [fkind] is not used, but present for symmetry with odd function *)
let min_and_max_enclosed_even _fkind (FRange.I(b, e)) =
  assert (is_integer b);
  assert (is_integer e);
  let b_is_odd = is_odd b in
  if b_is_odd && b = e then (* odd singleton *) None
  else
    (* note: no rounding errors may happen below because odd numbers are bounded *)
    Some ((if b_is_odd then b +. 1.0 else b), if is_odd e then e -. 1.0 else e)

(* large floating-point numbers are not symmetrical w.r.t. evenness *)
let min_and_max_enclosed_odd fkind (FRange.I(b, e)) =
  assert (is_integer b);
  assert (is_integer e);
  let min_odd =
    if is_odd b then b else max (most_negative_odd_integer fkind) (b +. 1.0)
  in
  let max_odd =
    if is_odd e then e else min (max_odd_integer fkind) (e -. 1.0)
  in
  if max_odd < min_odd then None else Some (min_odd, max_odd)

(* Returns true iff [b..e] contains at least one odd positive integer. *)
let contains_odd_positive_integer fkind (FRange.I(b, e)) =
  if e < 1.0 || b > max_odd_integer fkind then false
  else
    let posb, pose = max 0.0 b, max 0.0 e in
    let ib, ie = ceil posb, floor pose in
    ib < ie || (ib = ie && is_odd ib)

(* Splits [b..e] in (Some [b..f[, Some [f..e]).
   An empty sub-interval is represented by None. *)
let split_interval fkind (FRange.I(b, e) as x) f =
  if F.compare b f > 0 || F.equal b f then (None, Some x)
  else if F.compare e f < 0 then (Some x, None)
  else
    let pred_f = next_after fkind f neg_infinity in
    (Some (inject b pred_f), Some (inject f e))

let nan_pow = an_alarm (ANaN "negative base and noninteger exponent")
let nan_pow_assume1 =
  Builtin_alarms.add (AAssume ("non-negative base")) nan_pow
let nan_pow_assume2 (FRange.I(b, e)) =
  assert (is_integer b && is_integer e);
  Builtin_alarms.add
    (AAssume (Printf.sprintf
                "integer exponent between %g and %g for negative bases" b e))
    nan_pow

(* Negative x => function is only defined for integer values of y. *)
let pow_negative_x pow_f fkind ox y : builtin_res =
  match ox with
  | None -> no_alarm, `Bottom
  | Some (FRange.I(x1, x2)) ->
    match enclosed_integer_range y with
    | None -> (* no integer values of y *)
      nan_pow_assume1, `Bottom
    | Some y_int ->
      (* alert if y may contain non-integer values *)
      let alarms =
        if not (is_a_zero y) && (y <> y_int || not (is_singleton y)) then
          nan_pow_assume2 y_int
        else no_alarm
      in
      let compute_for_integer_y ~even min_and_max_f fkind y_int =
        match min_and_max_f fkind y_int with
        | None -> no_alarm, `Bottom
        | Some (min_y, max_y) ->
          let fx1, fx1' = pow_f x1 min_y, pow_f x1 max_y in
          let fx2, fx2' = pow_f x2 min_y, pow_f x2 max_y in
          let min_f = min fx1 (min fx1' (min fx2 fx2')) in
          let min_f = if even && F.equal min_f F.zero &&
                         contains_odd_positive_integer fkind y then
              (* underflow: include minus zero *) F.minus_zero
            else min_f
          in
          let max_f = max fx1 (max fx1' (max fx2 fx2')) in
          let infb, infe, res = inject_r_f fkind min_f max_f in
          let inf_alarms = Builtin_alarms.union
              (if infb then an_alarm ANegInf else no_alarm)
              (if infe then an_alarm APosInf else no_alarm)
          in
          Builtin_alarms.union alarms inf_alarms, `Value res
      in
      (* positive interval: even y *)
      let even_alarms, pos_itv =
        compute_for_integer_y ~even:true min_and_max_enclosed_even fkind y_int
      in
      let odd_alarms, neg_itv =
        compute_for_integer_y ~even:false min_and_max_enclosed_odd fkind y_int
      in
      Builtin_alarms.union even_alarms odd_alarms,
      join_or_bottom pos_itv neg_itv

(* We compute the "actual" values to generate alarms, but also the "precise"
   values (ignoring -inf/+inf). *)
let compute_for_zero_x has_neg_inf has_minus_zero has_zero has_one has_pos_inf =
  let min_f =
    match has_minus_zero, has_zero, has_one with
    | true, _, _ -> `Value minus_zero
    | _, true, _ -> `Value zero
    | _, _, true -> `Value one
    | false, false, false -> `Bottom
  in
  let max_f =
    match has_one, has_zero, has_minus_zero with
    | true, _, _ -> `Value one
    | _, true, _ -> `Value zero
    | _, _, true -> `Value minus_zero
    | false, false, false -> `Bottom
  in
  let alarms = Builtin_alarms.union
      (if has_neg_inf then an_alarm ANegInf else no_alarm)
      (if has_pos_inf then an_alarm APosInf else no_alarm)
  in
  alarms, join_or_bottom min_f max_f

(* x equal to -0.0 or 0.0 *)
let pow_zero_x fkind ox (FRange.I(y1, y2) as y) : builtin_res =
  match ox with
  | None -> no_alarm, `Bottom
  | Some x ->
    let has_zero = y2 > 0.0 in
    let has_one = contains_a_zero y in
    let has_pos_inf = y1 < -0.0 in
    match Extlib.opt_bind (min_and_max_enclosed_odd fkind) (enclosed_integer_range y) with
    | None -> (* no odd integers *)
      let has_neg_inf = false in
      let has_minus_zero = false in
      compute_for_zero_x has_neg_inf has_minus_zero has_zero has_one has_pos_inf
    | Some (min_odd_y, max_odd_y) ->
      let has_neg_inf = contains_minus_zero x && min_odd_y < 0.0 in
      let has_minus_zero = contains_minus_zero x && max_odd_y > 0.0 in
      compute_for_zero_x has_neg_inf has_minus_zero has_zero has_one has_pos_inf

(* x greater than 0.0 *)
let pow_positive_x pow_f fkind ox (FRange.I(y1, y2)) : builtin_res =
  match ox with
  | None -> no_alarm, `Bottom
  | Some (FRange.I(x1, x2)) ->
    let fx1, fx1' = pow_f x1 y1, pow_f x1 y2 in
    let fx2, fx2' = pow_f x2 y1, pow_f x2 y2 in
    let min_f = min fx1 (min fx1' (min fx2 fx2')) in
    let max_f = max fx1 (max fx1' (max fx2 fx2')) in
    let infb, infe, res = inject_r_f fkind min_f max_f in
    let alarms = Builtin_alarms.union
        (if infb then an_alarm ANegInf else no_alarm)
        (if infe then an_alarm APosInf else no_alarm)
    in
    alarms, `Value res

(* [pow_f] is the actual function computing the exact power, according to the
   desired precision: powf for single precision, pow for double precision. *)
let pow' pow_f fkind (FRange.I(b1, e1) as x) (FRange.I(b2, e2) as y) =
  Floating_point.set_round_nearest_even ();
  (* deterministic case *)
  if F.equal b1 e1 && F.equal b2 e2 then begin
    try
      let c = pow_f b1 b2 in
      match classify_float c with
      | FP_nan -> assert false
      | FP_infinite ->
        an_alarm (if c < 0. then ANegInf else APosInf), `Bottom
      | _ -> no_alarm, `Value (inject_f fkind c c)
    with F.NaN (* raised by pow_f *) ->
      nan_pow, `Bottom
  end
  else
    (* split analysis in 3 intervals for x:
       negatives (]-oo..-0.0[), zero ([-0.0..0.0]) and positives (]0.0..+oo[) *)
    let x_neg, x_pos_or_zero = split_interval fkind x (-0.0) in
    let x_zero, x_pos = match x_pos_or_zero with
      | None -> None, None
      | Some x_pos_or_zero ->
        let zero_succ = next_after fkind F.zero infinity in
        split_interval fkind x_pos_or_zero zero_succ in
    (* negative x is computed later because it may fail *)
    let alarms1, itv_for_zero_x = pow_zero_x fkind x_zero y in
    let alarms2, itv_for_pos_x = pow_positive_x pow_f fkind x_pos y in
    let alarms3, itv_for_neg_x = pow_negative_x pow_f fkind x_neg y in
    Builtin_alarms.union alarms1 (Builtin_alarms.union alarms2 alarms3),
    join_or_bottom itv_for_neg_x
      (join_or_bottom itv_for_zero_x itv_for_pos_x)

let pow = pow' F.pow Float64
let powf = pow' F.powf Float32

let cos_precise (FRange.I(b, e)) =
  Floating_point.set_round_nearest_even ();
  if F.equal b e
  then
    let c = F.cos b in
    inject c c
  else if F.le_ieee b F.minus_ff || F.le_ieee F.ff e
  then minus_one_one
  else begin
    let allpos = F.le_ieee F.zero b in
    let allneg = F.le_ieee e F.zero in
    if F.le_ieee F.m_minus_pi b && F.le_ieee e F.m_pi
    then begin
      if allpos
      then
        inject (F.cos e) (F.cos b)
      else if allneg
      then
        inject (F.cos b) (F.cos e)
      else
        inject (F.min (F.cos b) (F.cos e)) F.one
    end
    else if allpos || allneg
    then inject F.minus_one (F.max (F.cos b) (F.cos e))
    else minus_one_one
  end

let sin_precise (FRange.I(b, e)) =
  Floating_point.set_round_nearest_even ();
  if F.equal b e
  then let c = F.sin b in inject c c
  else if F.le_ieee b F.minus_ff || F.le_ieee F.ff e
  then minus_one_one
  else if F.le_ieee e F.m_pi_2
  then begin
    if F.le_ieee F.m_minus_pi_2 b
    then inject (F.sin b) (F.sin e)
    else if F.le_ieee e F.m_minus_pi_2
    then inject (F.sin e) (F.sin b)
    else inject F.minus_one (F.max (F.sin b) (F.sin e))
  end
  else if F.le_ieee F.m_pi_2 b
  then
    inject (F.sin e) (F.sin b)
  else if F.le_ieee F.m_minus_pi_2 b
  then
    inject (F.min (F.sin b) (F.sin e)) F.one
  else minus_one_one

(** See discussion in the .mli about [rounding_mode] *)
(* [exp_f] is the actual underlying function computing the exponential,
   according to the desired precision: expf for single precision,
   exp for double precision. *)
let exp' exp_f fkind rounding_mode (FRange.I(b, e)) =
  if rounding_mode = Any
  then Floating_point.set_round_downward ()
  else Floating_point.set_round_nearest_even ();
  let min = exp_f b in
  if rounding_mode = Any then Floating_point.set_round_upward ();
  let max = exp_f e in
  if rounding_mode = Any then Floating_point.set_round_nearest_even ();
  let infb, infe, r = inject_r_f fkind min max in
  let alarms =
    Builtin_alarms.union
      (if infb then an_alarm ANegInf else no_alarm)
      (if infe then an_alarm APosInf else no_alarm)
  in
  alarms, r

let exp = exp' F.exp Float64
let expf = exp' F.expf Float32

let widen (FRange.I(b1,e1)) (FRange.I(b2, e2)) =
  assert (F.le b2 b1);
  assert (F.le e1 e2);
  let b = if F.equal b2 b1 then b2 else F.widen_down b2 in
  let e = if F.equal e2 e1 then e2 else F.widen_up e2 in
  inject b e

let equal_float_ieee (FRange.I(b1, e1)) (FRange.I(b2, e2)) =
  let intersects =
    F.le_ieee b1 e2 && F.le_ieee b2 e1
  in
  if not intersects
  then true, false
  else if F.equal_ieee b1 e1 && F.equal_ieee b2 e2
  then false, true
  else true, true

let maybe_le_ieee_float (FRange.I(b1, _e1)) (FRange.I(_b2, e2)) =
  F.le_ieee b1 e2

let maybe_lt_ieee_float (FRange.I(b1, _e1)) (FRange.I(_b2, e2)) =
  F.lt_ieee b1 e2

let diff (FRange.I(b1, e1) as f1) (FRange.I(b2, e2)) =
  if F.le b2 b1 && F.le e1 e2
  then `Bottom
  else if F.le b2 e1 && F.le e1 e2
  then `Value (inject b1 b2)
  else if F.le b1 e2 && F.le b2 b1
  then `Value (inject e2 e1)
  else `Value f1

let filter_le_f allmodes fkind (FRange.I(b1, e1) as f1) e2 =
  let e2 =
    if F.equal_ieee F.zero e2
    then F.zero
    else
      match fkind with
      | Float32 ->
        (* Preserve the invariant that the returned interval has 32bits
           floating-point bounds *)
        if allmodes then
          Floating_point.set_round_upward () (* conservative direction *)
        else
          Floating_point.set_round_downward () (* precise direction *);
        let r = Floating_point.round_to_single_precision_float e2 in
        Floating_point.set_round_nearest_even ();
        r
      | Float64 -> e2
  in
  if not (F.le b1 e2)
  then `Bottom
  else if F.le e1 e2
  then `Value f1
  else `Value (inject b1 e2)

let filter_le allmodes fkind f1 (FRange.I(_b2, e2) as _f2) =
  filter_le_f allmodes fkind f1 e2

let filter_lt allmodes fkind (FRange.I(b1, _e1) as f1) (FRange.I(_b2, e2)) =
  if F.le_ieee e2 b1
  then `Bottom
  else
    let e2 =
      if allmodes
      then e2
      else if F.equal_ieee F.zero e2
      then Floating_point.neg_min_denormal
      else F.prev_float e2 (* non-infinite because >= b1 *)
    in
    filter_le_f allmodes fkind f1 e2

let filter_ge_f allmodes fkind (FRange.I(b1, e1) as f1) b2 =
  let b2 =
    if F.equal_ieee F.minus_zero b2
    then F.minus_zero
    else
      match fkind with
      | Float32 -> (* see comments in filter_le_f *)
        if allmodes then
          Floating_point.set_round_downward ()
        else
          Floating_point.set_round_upward ();
        let r = Floating_point.round_to_single_precision_float b2 in
        Floating_point.set_round_nearest_even ();
        r
      | Float64 -> b2
  in
  if not (F.le b2 e1)
  then `Bottom
  else if F.le b2 b1
  then `Value f1
  else `Value (inject b2 e1)

let filter_ge allmodes fkind f1 (FRange.I(b2, _e2)) =
  filter_ge_f allmodes fkind f1 b2

let filter_gt allmodes fkind (FRange.I(_b1, e1) as f1) (FRange.I(b2, _e2)) =
  if F.le_ieee e1 b2
  then `Bottom
  else
    let b2 =
      if allmodes
      then b2
      else if F.equal_ieee F.zero b2
      then Floating_point.min_denormal
      else F.next_float b2 (* non-infinite because <= e1 *)
    in
    filter_ge_f allmodes fkind f1 b2

let filter_le_ge_lt_gt op allmodes fkind f1 f2 = match op with
  | Cil_types.Le -> filter_le allmodes fkind f1 f2
  | Cil_types.Ge -> filter_ge allmodes fkind f1 f2
  | Cil_types.Lt -> filter_lt allmodes fkind f1 f2
  | Cil_types.Gt -> filter_gt allmodes fkind f1 f2
  | _ -> `Value f1

let nan_fmod = an_alarm (ANaN "division by zero")

(* Emits a warning if there may be a division by zero.
   Raises [Builtin_invalid_domain] if there must be a division by zero.
   Evalutes the function for y ∉ {+0.0,-0.0}. *)
let fmod (FRange.I(b1, e1) as x) (FRange.I(b2, e2) as y) =
  let alarms = if contains_a_zero y then nan_fmod else no_alarm in
  if is_a_zero y then
    alarms, `Bottom
  else begin
    Floating_point.set_round_nearest_even ();
    (* case analysis for extra precision *)
    (* 1. deterministic case: y is a singleton and x ≠ 0 (already tested) *)
    if F.equal b1 e1 && F.equal b2 e2 then
      let c = F.fmod b1 b2 in
      no_alarm, `Value (inject_f Float64 c c)
    else
      (* 2. [0 ∉ y] and [max_x < min_y] => [fmod(x,y) = x].
            (i.e., x is too small w.r.t. y and unaffected by fmod *)
    if not (contains_a_zero y) &&
       F.compare (F.max (abs_float b1) (abs_float e1))
         (F.min (abs_float b2) (abs_float e2)) < 0 then (alarms, `Value x)
    else
      (* 3. x and y are within the same continuos region.
         (i.e. do not contain zero, do not cross any modulo boundaries, etc.)
         Example: x=[6,7] and y=[4,5].
         Restriciton: [|x/y| < 2^53], otherwise truncation to an integer
         (to test the above property) may return an incorrect result.
         Note: to avoid issues with rounding, we conservatively set the
         limit to 2^51 instead of 2^53. *)
      let trunc x =
        if F.compare x F.zero < 0 then ceil x else floor x
      in
      let max_i = F.max_precise_integer /. 4. in
      let _ = Floating_point.set_round_toward_zero () in
      let f1 = trunc (abs_float (b1 /. b2)) in
      let f2 = trunc (abs_float (e1 /. e2)) in
      let f3 = trunc (abs_float (b1 /. e2)) in
      let f4 = trunc (abs_float (e1 /. b2)) in
      Floating_point.set_round_nearest_even ();
      if not (contains_zero x) && not (contains_zero y) &&
         F.compare f1 f2 = 0 && F.compare f2 f3 = 0 && F.compare f3 f4 = 0 &&
         F.compare f1 max_i < 0
      then
        (* normalize x and to positive intervals to minimize number of cases
           (x and y do not contain 0).
           The sign of x is forwarded to the result,
           and the sign of y is ignored. *)
        let x', y' = abs x, abs y in
        let res_is_positive = F.compare e1 F.minus_zero > 0 in
        match x', y' with
        | FRange.I(x1, x2), FRange.I(y1, y2) ->
          let r_min = mod_float x1 y2 in
          let r_max = mod_float x2 y1 in
          let res_abs = inject_f Float64 r_min r_max in
          let res = if res_is_positive then res_abs else neg res_abs in
          (alarms, `Value res)
      else
        (* General case: |fmod(x,y)| <= max(|b1|,|e1|) and
                         |fmod(x,y)| < max(|b2|,|e2|),
           e.g. (2.5 fmod 6) <= 2.5, and (6 fmod 2.5) < 2.5.
           Also, if x > 0, then 0 <= fmod(x,y), and symmetrically for x < 0. *)
        (* Auxiliary functions to filter interval extremities *)
        let filter_lower_bound (FRange.I(b,_) as i) =
          Abstract_interp.Bot.non_bottom
            (filter_gt false Float64 i (inject_f Float64 b b))
        in
        let filter_upper_bound (FRange.I(_,e) as i) =
          Abstract_interp.Bot.non_bottom
            (filter_lt false Float64 i (inject_f Float64 e e))
        in
        (* remove zeroes from y, by normalizing it to [ay1,ay2],
           where [ay1] and [ay2] are both positive.
           This is valid because fmod is an even function w.r.t. y
           (e.g. f(x,y) = f(x,-y)).
           Compute fmod on ]0, max(|b2|,|e2|)] if y crosses the x-axis,
           or on |[b2,e2]| otherwise. *)
        let max (FRange.I(_,e)) = e in
        let x', y' = abs x, abs y in
        let y' = if contains_zero y' then
            (* y crosses the x-axis, filter zero out (alarm already emitted) *)
            filter_lower_bound (inject_f Float64 F.zero (max y')) else y'
        in
        let r_mod = F.min (max x') (max y') in
        (* To know whether we can ignore the extremities of the interval,
           we check if the result modulus is due to the y interval. *)
        let strict_le = F.compare (max y') (max x') <= 0 in
        (* The final result is [0, r_mod] if x is always non-negative,
           [-r_mod, 0] if x is always negative, or [-r_mod, r_mod] otherwise.
           The interval is closed or open according to [strict_le]. *)
        let res =
          if F.compare e1 F.zero < 0 then
            (* x always negative *)
            let r = inject_f Float64 (-.r_mod) F.minus_zero in
            if strict_le then filter_lower_bound r else r
          else if F.compare b1 F.minus_zero > 0 then
            (* x always positive *)
            let r = inject_f Float64 F.zero r_mod in
            if strict_le then filter_upper_bound r else r
          else
            (* x may be negative or positive => intersect [-r_mod, r_mod]
               with the original interval *)
            let r = inject_f Float64 (-.r_mod) r_mod in
            let r =
              if strict_le then filter_lower_bound (filter_upper_bound r) else r
            in
            Abstract_interp.Bot.non_bottom (meet x r)
        in
        (alarms, `Value res)
  end


let nan_log = an_alarm (ANaN "negative argument")
let nan_log_assume =
  Builtin_alarms.add (AAssume "argument greater than zero") nan_log

(** See discussion in the .mli about [rounding_mode] *)
let log_float_aux fkind flog rounding_mode (FRange.I(_, e) as v) =
  (* we want to compute the smallest denormal bigger than zero -> use
     allroundingmodes=false. *)
  match filter_gt false fkind v zero with
  | `Bottom -> nan_log, `Bottom
  | `Value (FRange.I(b_reduced, _) as reduced) ->
    let alarm = if equal reduced v then no_alarm else nan_log_assume in
    if rounding_mode = Any
    then Floating_point.set_round_downward ()
    else Floating_point.set_round_nearest_even ();
    let min = flog b_reduced in
    if rounding_mode = Any then Floating_point.set_round_upward ();
    let max = flog e in
    if rounding_mode = Any then Floating_point.set_round_nearest_even ();
    let alm', alm'', r = inject_r_f fkind min max in
    assert (not (alm'||alm'')); (* alm' and alm'' should always be false *)
    alarm, `Value r

let log = log_float_aux Float64 F.log
let log10 = log_float_aux Float64 F.log10

let logf = log_float_aux Float32 F.logf
let log10f = log_float_aux Float32 F.log10f

(* The functions defined using [exact_aux] below are, among other
   properties, (1) exact (the result as a real can always be
   represented exactly, in the good type), and (2) total. In
   particular, given a float 'x', 'ff x == (float)(f (double)x)'.
   Thus, in this module, the 'f' functions are also the non-f
   (since float32 are represented using double) *)
let exact_aux fkind ff _rounding_mode (FRange.I(b, e)) =
  let fb, fe = ff b, ff e in
  no_alarm, inject_f fkind fb fe

let floor = exact_aux Float64 F.floor
let ceil = exact_aux Float64 F.ceil
let trunc = exact_aux Float64 F.trunc
let fround = exact_aux Float64 F.fround

let floorf = exact_aux Float32 F.floor
let ceilf = exact_aux Float32 F.ceil
let truncf = exact_aux Float32 F.trunc
let froundf = exact_aux Float32 F.fround


let subdiv_float_interval ~size (FRange.I(l, u) as i) =
  let midpoint = F.avg l u in
  let midpointl, midpointu =
    if size <> 32 && size <> 64
    then midpoint, midpoint
    else
      let smidpoint = F.next_float midpoint in
      if size = 64
      then
        if F.le smidpoint u
        then
          if F.next_float l = u
          then
            l, u
          else
            midpoint, smidpoint
        else midpoint, u
      else begin (* 32 *)
        let i1 = Int64.bits_of_float l in
        if i1 = Int64.min_int &&
           (Int64.bits_of_float u) = Int64.zero
        then
          l ,u
        else begin
          Floating_point.set_round_upward ();
          let midpointu =
            Floating_point.round_to_single_precision_float smidpoint
          in
          Floating_point.set_round_downward ();
          let midpointl =
            Floating_point.round_to_single_precision_float midpoint
          in
          Floating_point.set_round_nearest_even ();
          midpointl, midpointu
        end
      end
  in
  if F.le midpointu l || F.le u midpointl
  then raise Can_not_subdiv;
  (*    Format.printf "%a %a %a %a@."
        (F.pretty_normal ~use_hex:true) l
        (F.pretty_normal ~use_hex:true) midpointl
        (F.pretty_normal ~use_hex:true) midpointu
        (F.pretty_normal ~use_hex:true) u; *)
  let i1 = inject l midpointl in
  assert (is_included i1 i);
  let i2 = inject midpointu u in
  assert (is_included i2 i);
  i1, i2

(*
Local Variables:
compile-command: "make -C ../.. byte"
End:
*)
