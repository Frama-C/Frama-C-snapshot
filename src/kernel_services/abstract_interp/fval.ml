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

open Abstract_interp
open Bottom.Type

[@@@ ocaml.warning "-32"]
(* Those OCaml functions do not have the proper semantics w.r.t -0/+0. We
   make them inoperative for this entire file. *)

let min = ()
let max = ()
let compare = ()

[@@@ ocaml.warning "+32"]

type kind = Float32 | Float64 | Real (* Real with NaN *)

let kind = function
  | Cil_types.FFloat -> Float32
  | Cil_types.FDouble -> Float64
  | Cil_types.FLongDouble -> Real

let pretty_kind fmt kind =
  Format.pp_print_string fmt
    (match kind with
     | Float32 -> "Float32"
     | Float64 -> "Float64"
     | Real -> "Real")

module F = struct
  (** no NaN should be produced by function of this module
      An exception, that should not be caught is returned instead. *)

  type t = float

  let packed_descr = Structural_descr.p_float

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
  let lt f1 f2 = compare f1 f2 < 0
  let ge f1 f2 = compare f1 f2 >= 0
  let gt f1 f2 = compare f1 f2 > 0

  let min f1 f2 = if le f1 f2 then f1 else f2
  let max f1 f2 = if le f1 f2 then f2 else f1

  let equal_ieee = ((=) : float -> float -> bool)
  let le_ieee = ((<=) : float -> float -> bool)
  let lt_ieee = ((<) : float -> float -> bool)
  let ge_ieee = ((>=) : float -> float -> bool)
  let gt_ieee = ((>) : float -> float -> bool)

  let hash = Hashtbl.hash
  let pretty_normal = Floating_point.pretty_normal
  let pretty = Floating_point.pretty

  let plus_zero = 0.0
  let minus_zero = -0.0
  let is_a_zero x = equal_ieee x plus_zero

  let neg_infinity = neg_infinity
  let pos_infinity = Pervasives.infinity
  (** To minimize ambiguity, and to use terms closer to those of the IEEE-754
      standard, we use [pos_infinity] to denote OCaml's [Pervasives.infinity].
      Functions that deal with (unspecified) infinity (e.g. [is_infinity])
      consider both positive and negative infinities. *)

  let is_pos_infinity = (=) pos_infinity
  let is_neg_infinity = (=) neg_infinity

  (* works but allocates:
     let is_negative f = Int64.bits_of_float f < Int64.zero *)
  (* replace "noalloc" with [@@noalloc] for OCaml version >= 4.03.0 *)
  [@@@ warning "-3"]
  external is_negative : float -> bool = "float_is_negative" "noalloc"
  [@@@ warning "+3"]

  let is_finite f = match classify_float f with
    | FP_nan | FP_infinite -> false
    | FP_normal | FP_subnormal | FP_zero -> true

  let is_infinite f = classify_float f = FP_infinite

  let is_nan a = classify_float a = FP_nan

  (* Must *not* be exported. All functions of this module should check the
     arguments with which they call the functions labelled "may raise Nan
     exception" *)
  exception Invalid_NaN

  (* May raise NaN exception *)
  let ensure_not_nan r =
    match classify_float r with
    | FP_nan -> raise Invalid_NaN
    | FP_normal | FP_subnormal | FP_infinite | FP_zero -> r

  let ensure_not_nan_unary f x = ensure_not_nan (f x)
  let ensure_not_nan_binary f x y = ensure_not_nan (f x y)

  let id = fun x -> x
  let of_float = ensure_not_nan_unary id
  let to_float = id

  let sub = ensure_not_nan_binary (-.)
  let neg = ensure_not_nan_unary (~-.)

  (* See bts #1396. We patch Pervasives function only when needed *)
  let sqrt =
    if compare (sqrt minus_zero) minus_zero <> 0
    then fun v -> if v = minus_zero then v else sqrt v
    else sqrt

  (* May raise NaN exception on negative arguments *)
  let sqrt = ensure_not_nan_unary sqrt

  let cos = ensure_not_nan_unary cos
  let sin = ensure_not_nan_unary sin
  let atan2 = ensure_not_nan_binary atan2

  let exp = ensure_not_nan_unary exp

  (* May raise NaN exception on negative or zero arguments *)
  let log = ensure_not_nan_unary log
  let log10 = ensure_not_nan_unary log10

  let floor = ensure_not_nan_unary floor
  let ceil = ensure_not_nan_unary ceil
  let trunc = ensure_not_nan_unary Floating_point.trunc
  let fround = ensure_not_nan_unary Floating_point.fround

  (* May raise NaN exception *)
  let pow = ensure_not_nan_binary ( ** )

  (* May raise NaN exception on zero second argument *)
  let fmod = ensure_not_nan_binary mod_float
  let fmodf = ensure_not_nan_binary Floating_point.fmodf

  (* single-precision *)
  let expf = ensure_not_nan_unary Floating_point.expf
  let logf = ensure_not_nan_unary Floating_point.logf
  let log10f = ensure_not_nan_unary Floating_point.log10f
  let powf = ensure_not_nan_binary Floating_point.powf
  let sqrtf = ensure_not_nan_unary Floating_point.sqrtf
  let cosf = ensure_not_nan_unary Floating_point.cosf
  let sinf = ensure_not_nan_unary Floating_point.sinf
  let atan2f = ensure_not_nan_binary Floating_point.atan2f

  let minus_one = -1.0
  let one = 1.0
  let ten = 10.
  let m_pi = 3.1415929794311523 (* single-precision *)
  let m_minus_pi = -. m_pi
  let m_pi_2 = 1.5707964897155761 (* single-precision *)

  let max_single_precision_float = Floating_point.max_single_precision_float
  let most_negative_single_precision_float =
    Floating_point.most_negative_single_precision_float
  let max_float = max_float

  let most_negative_float = -. max_float

  (* Maximum integer value M such that all 0 < n < M are exactly
     representable as doubles. *)
  let max_precise_integer = 2. ** 53.
  let max_single_precise_integer = 2. ** 23.

  let max_representable_float = function
    | Float32 -> max_single_precision_float
    | Real | Float64 -> max_float

  let most_negative_representable_float = function
    | Float32 -> most_negative_single_precision_float
    | Real | Float64 -> most_negative_float

  (* Maximum odd integer representable in float64 (2^53-1). *)
  let max_double_odd_integer = 9007199254740991.0
  (* Maximum odd integer representable in float32 (2^24-1). *)
  let max_single_odd_integer = 16777215.0
  (* Most negative odd integer representable in float64. *)
  let most_negative_double_odd_integer = -.max_double_odd_integer
  (* Most negative odd integer representable in float32. *)
  let most_negative_single_odd_integer = -.max_single_odd_integer
  (* Successor of the maximum non-integer representable in float64 (2^52) *)
  let ceil_of_largest_representable_non_integer_double = 4503599627370496.0
  (* Successor of the maximum non-integer representable in float32 (2^23) *)
  let ceil_of_largest_representable_non_integer_single = 8388608.0
  (* Predecessor of the most-negative non-integer representable in float64 -(2^52) *)
  let floor_of_most_negative_representable_non_integer_double =
    -. ceil_of_largest_representable_non_integer_double
  (* Predecessor of the most-negative non-integer representable in float32 -(2^23) *)
  let floor_of_most_negative_representable_non_integer_single =
    -. ceil_of_largest_representable_non_integer_single

  let max_odd_integer fkind =
    match fkind with
    | Real | Float64 -> max_double_odd_integer
    | Float32 -> max_single_odd_integer

  let most_negative_odd_integer fkind =
    match fkind with
    | Real | Float64 -> most_negative_double_odd_integer
    | Float32 -> most_negative_single_odd_integer

  let ceil_of_largest_representable_non_integer fkind =
    match fkind with
    | Real | Float64 -> ceil_of_largest_representable_non_integer_double
    | Float32 -> ceil_of_largest_representable_non_integer_single

  let floor_of_most_negative_representable_non_integer fkind =
    match fkind with
    | Real | Float64 -> floor_of_most_negative_representable_non_integer_double
    | Float32 -> floor_of_most_negative_representable_non_integer_single

  let widen_up f =
    if le f minus_zero then minus_zero
    else if le f plus_zero then plus_zero
    else if le f one then one
    else if le f m_pi_2 then m_pi_2
    else if le f m_pi then m_pi
    else if le f ten then ten
    else if le f 1e10 then 1e10
    else if le f max_single_precision_float then max_single_precision_float
    else if le f 1e80 then 1e80
    else if le f max_float then max_float
    else pos_infinity

  let widen_down f = -. (widen_up (-. f))

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
    | FP_nan -> raise Invalid_NaN
    | FP_infinite -> float
    | FP_normal | FP_subnormal -> begin
        let f = next_previous_normal int64fup int64fdown float in
        match classify_float f with
        | FP_nan -> assert false (* can only be produced from an infinity *)
        | FP_infinite | FP_normal | FP_subnormal | FP_zero -> f
      end
    | FP_zero ->
      (next_previous_normal int64fup int64fdown (float +. min_float)) -. min_float

  let next_float fkind f' =
    let f = next_previous Int64.succ Int64.pred f' in
    match fkind with
    | Real -> f'
    | Float64 -> f
    | Float32 ->
      Floating_point.set_round_upward ();
      let f = Floating_point.round_to_single_precision_float f in
      Floating_point.set_round_nearest_even ();
      f

  let prev_float fkind f' =
    let f = next_previous Int64.pred Int64.succ f' in
    match fkind with
    | Real -> f'
    | Float64 -> f
    | Float32 ->
      Floating_point.set_round_downward ();
      let f = Floating_point.round_to_single_precision_float f in
      Floating_point.set_round_nearest_even ();
      f

  let pos_min_denormal fkind =
    match fkind with
    | Real -> plus_zero
    | Float64 -> Floating_point.min_denormal
    | Float32 -> Floating_point.min_single_precision_denormal

  let neg_min_denormal fkind =
    match fkind with
    | Real -> minus_zero
    | Float64 -> Floating_point.neg_min_denormal
    | Float32 -> Floating_point.neg_min_single_precision_denormal

  let next_float_ieee fkind b2 =
    if equal_ieee plus_zero b2
    then pos_min_denormal fkind
    else if is_pos_infinity b2
    then pos_infinity
    else next_float fkind b2

  let prev_float_ieee fkind b2 =
    if equal_ieee plus_zero b2
    then neg_min_denormal fkind
    else if is_neg_infinity b2
    then neg_infinity
    else prev_float fkind b2

  let round fkind f =
    match fkind with
    | Real | Float64 -> f
    | Float32 -> Floating_point.round_to_single_precision_float f

end

module FRange : sig
  (* invariants for intervals I(b,e,has_nan):
     - b and e are not NaN;
     - b <= e *)
  type t = private
    | I of F.t * F.t * bool
    | NaN
  val inject_raw : ?nan:bool -> F.t -> F.t -> t
  val inject : ?nan:bool -> kind -> F.t -> F.t -> t
  val inject_rounded: kind -> ?nan:bool -> F.t -> F.t -> t
  val nan: t
  val add_nan: t -> t
end =
struct

  type t =
    | I of F.t * F.t * bool
    | NaN

  let inject_raw ?(nan=false) b e =
    if F.is_nan b || F.is_nan e || not (F.le b e) then
      Kernel.fatal "Invalid bounds for float interval@\n%a .. %a@."
        (F.pretty_normal ~use_hex:true) b (F.pretty_normal ~use_hex:true) e;
    I(b, e, nan)

  (* If [fkind] is [Float32], we check that [b] and [e] are valid
     32-bit representations: lower bits are 0, and the value fits inside
     a 32-bit float. *)
  let check_representability fkind b e =
    if fkind = Float32 &&
       (Floating_point.round_to_single_precision_float b <> b ||
        Floating_point.round_to_single_precision_float e <> e)
    then
      let this_one fmt x =
        if Floating_point.round_to_single_precision_float x <> x
        then Format.pp_print_string fmt "->"
      in
      Kernel.fatal "Ival: invalid float32, %ab=%g (%a) %ae=%g (%a)"
        this_one b
        b (Floating_point.pretty_normal ~use_hex:true) b
        this_one e
        e (Floating_point.pretty_normal ~use_hex:true) e

  let inject ?nan fkind b e =
    check_representability fkind b e;
    inject_raw ?nan b e

  let inject_rounded fkind ?nan b e =
    let b, e =
      if fkind = Float32
      then Floating_point.round_to_single_precision_float b,
           Floating_point.round_to_single_precision_float e
      else b, e
    in
    inject_raw ?nan b e

  let nan = NaN

  let add_nan f =
    match f with
    | NaN -> f
    | I(_,_,true) -> f
    | I(b,e,false) -> I(b,e,true)

end

(* --------------------------------------------------------------------------
                                    Datatype
   -------------------------------------------------------------------------- *)

type t = FRange.t

let structural_descr =
  Structural_descr.t_sum [|
    [| F.packed_descr; F.packed_descr; Structural_descr.p_bool |];
    [| |]
  |]

let packed_descr = Structural_descr.pack structural_descr

let compare x y =
  match x, y with
  | FRange.I(b1,e1,n1), FRange.I(b2,e2,n2) ->
    let c = Pervasives.compare n1 n2 in
    if c <> 0 then c else
      let r = F.compare b1 b2 in
      if r <> 0 then r else F.compare e1 e2
  | FRange.I(_,_,_), FRange.NaN -> -1
  | FRange.NaN, FRange.I(_,_,_) -> 1
  | FRange.NaN, FRange.NaN -> 0

let equal x y =
  match x,y with
  | FRange.I(b1,e1,n1), FRange.I(b2,e2,n2) ->
    F.equal b1 b2 && F.equal e1 e2 && n1 = n2
  | FRange.NaN, FRange.NaN -> true
  | _ -> false

let pretty_aux pp_min pp_max fmt = function
  | FRange.I(b,e,nan) ->
    if F.equal b e then
      Format.fprintf fmt "{%a%t}" F.pretty b
        (fun fmt -> if nan then Format.pp_print_string fmt ";NaN")
    else begin
      let print_nan fmt nan =
        if nan then Format.fprintf fmt " %s {NaN}" (Unicode.union_string ())
      in
      if (Kernel.FloatRelative.get())
      then begin
        Floating_point.set_round_upward ();
        let d = F.sub e b in
        Floating_point.set_round_nearest_even ();
        Format.fprintf fmt "[%a ++ %a]%a"
          F.pretty b
          F.pretty d
          print_nan nan
      end
      else begin
        Format.fprintf fmt "[%a .. %a]%a" pp_min b pp_max e print_nan nan
      end
    end
  | FRange.NaN -> Format.fprintf fmt "NaN"

let pretty = pretty_aux F.pretty F.pretty

let hash = function
  | FRange.I(b,e,n) -> (2 * F.hash b) + (5 * F.hash e) + (7 * Hashtbl.hash n)
  | FRange.NaN -> 3

(* --------------------------------------------------------------------------
                       Constants, injections, projections
   -------------------------------------------------------------------------- *)

let top_f ~nan = FRange.inject_raw ~nan F.neg_infinity F.pos_infinity
let top = top_f ~nan:true

let top_finite = function
  | Real -> top_f ~nan:false
  | Float32 ->
    FRange.inject_raw
      F.most_negative_single_precision_float
      F.max_single_precision_float
  | Float64 ->
    FRange.inject_raw
      F.most_negative_float
      F.max_float

let inject = FRange.inject
let inject_singleton x = FRange.inject_raw x x

let one = inject_singleton F.one
let plus_zero = inject_singleton F.plus_zero
let minus_zero = inject_singleton F.minus_zero
let zeros = FRange.inject_raw F.minus_zero F.plus_zero
let pos_infinity = inject_singleton F.pos_infinity
let neg_infinity = inject_singleton F.neg_infinity
let nan = FRange.nan

let pi =
  (* [pi] is the nearest double to \pi, and is smaller than \pi. *)
  let pi = 3.14159265358979323846 in
  FRange.inject_raw ~nan:false pi (F.next_float Float64 pi)

let e =
  (* [e] is the nearest double to \e, and is smaller than \e. *)
  let e = 2.7182818284590452354 in
  FRange.inject_raw ~nan:false e (F.next_float Float64 e)

let minus_one_one = FRange.inject_raw ~nan:false F.minus_one F.one
let minus_pi_pi = FRange.inject_raw ~nan:false F.m_minus_pi F.m_pi

let round_to_single_precision_float = function
  | FRange.I(b, e, nan) ->
    let b = Floating_point.round_to_single_precision_float b in
    let e = Floating_point.round_to_single_precision_float e in
    FRange.inject_raw ~nan b e
  | FRange.NaN -> FRange.nan

let inject_after_tighten kind ~nan b e =
  match kind with
  | Real | Float64 -> `Value (FRange.inject ~nan kind b e)
  | Float32 ->
    Floating_point.set_round_upward ();
    let b = Floating_point.round_to_single_precision_float b in
    Floating_point.set_round_downward ();
    let e = Floating_point.round_to_single_precision_float e in
    Floating_point.set_round_nearest_even ();
    if F.le b e then `Value (FRange.inject_raw ~nan b e)
    else if nan then `Value FRange.nan else `Bottom

let tighten_bound_by_rounding = function
  | Real | Float64 -> fun f -> `Value f
  | Float32 -> function
    | FRange.I (b, e, nan) -> inject_after_tighten Float32 ~nan b e
    | FRange.NaN -> `Value FRange.nan

let min_and_max = function
  | FRange.I(b,e,nan) -> Some (b, e), nan
  | FRange.NaN -> None, true

let is_negative = function
  | FRange.I (b, e, false) ->
    if F.is_negative e then Comp.True
    else if not (F.is_negative b) then Comp.False
    else Comp.Unknown
  | FRange.I (_, _, true)
  | FRange.NaN -> Comp.Unknown

exception Not_Singleton_Float

let project_float = function
  | FRange.I(b, e,false) when F.equal b e -> b
  | FRange.I(_, _,_) | FRange.NaN -> raise Not_Singleton_Float

(* Splits the interval [b; e] into two intervals (neg, pos) such that [neg]
   (resp. [pos]) contains only negative (resp. positive) floats. *)
let split_by_sign ((b, e) as x) =
  if F.le e F.minus_zero then `Value x, `Bottom
  else if F.ge b F.plus_zero then `Bottom, `Value x
  else `Value (b, F.minus_zero), `Value (F.plus_zero, e)

(* Returns true iff [f] represents an odd integer. *)
let is_odd f = abs_float (mod_float f 2.0) = 1.0

(* [split_by_parity (b, e)] returns [even_bounds, odd_bounds], where:
   - [even_bounds] are the min and max even integers enclosed between b and e
     (or None if there is no such even integer).
   - [odd_bounds] are the min and max odd integers (representable as floating-
     point values) enclosed between b and e (or None if there is no such odd
     integer). These bounds cannot be infinite.  *)
let split_by_parity fkind (b, e) =
  let b = ceil b
  and e = floor e in
  if b > e then None, None
  else if b = e
  then
    let x = (b, e) in
    if is_odd b then None, Some x else Some x, None
  else
    let min_even, min_odd =
      if is_odd b
      (* No rounding errors may happen below because odd numbers are bounded. *)
      then b +. 1.0, b
      else b, F.max (F.most_negative_odd_integer fkind) (b +. 1.0)
    and max_even, max_odd =
      if is_odd e
      (* No rounding errors may happen below because odd numbers are bounded. *)
      then e -. 1.0, e
      else e, F.min (F.max_odd_integer fkind) (e -. 1.0)
    in
    let even = Some (min_even, max_even)
    and odd = if max_odd < min_odd then None else Some (min_odd, max_odd) in
    even, odd

(* --------------------------------------------------------------------------
                                    Lattice
   -------------------------------------------------------------------------- *)

let is_included x1 x2 =
  match x1, x2 with
  | FRange.I(b1, e1, n1), FRange.I(b2, e2, n2) ->
    F.le b2 b1 && F.le e1 e2 && (not n1 || n2)
  | FRange.NaN, FRange.I(_,_,true) -> true
  | FRange.NaN, FRange.NaN -> true
  | _ -> false

let join f1 f2 =
  match f1,f2 with
  | FRange.I(b1, e1, n1), FRange.I(b2, e2, n2) ->
    FRange.inject_raw ~nan:(n1 || n2) (F.min b1 b2) (F.max e1 e2)
  | (FRange.I(b1, e1, _), FRange.NaN)
  | (FRange.NaN, FRange.I(b1, e1, _)) -> FRange.inject_raw ~nan:true b1 e1
  | FRange.NaN, FRange.NaN -> FRange.nan

let widen f1 f2 =
  assert (is_included f1 f2);
  match f1, f2 with
  | FRange.I(b1,e1,_), FRange.I(b2, e2,nan) ->
    let b = if F.equal b2 b1 then b2 else F.widen_down b2 in
    let e = if F.equal e2 e1 then e2 else F.widen_up e2 in
    (** widen_up and down produce double only if the input is a double *)
    FRange.inject_raw ~nan b e
  | FRange.NaN, f2 -> f2
  | FRange.I(_,_,_), FRange.NaN -> assert false

let meet f1 f2 =
  match f1, f2 with
  | FRange.I(b1, e1, n1), FRange.I(b2, e2, n2) ->
    let is_finite = F.le b2 e1 && F.le b1 e2 in
    let is_nan = n1 && n2 in
    if is_finite || is_nan
    then
      let v =
        if is_finite then
          FRange.inject_raw ~nan:is_nan (F.max b1 b2) (F.min e1 e2)
        else FRange.nan
      in
      `Value v
    else `Bottom
  | (FRange.I(_,_,true) | FRange.NaN) , (FRange.I(_,_,true) | FRange.NaN) ->
    `Value FRange.nan
  | _ -> `Bottom

let narrow = meet

let has_greater_min_bound x1 x2 =
  match x1,x2 with
  | FRange.I (m1, _, _), FRange.I (m2, _, _) -> F.compare m1 m2
  | FRange.NaN, FRange.I _ -> 1
  | FRange.I _, FRange.NaN -> -1
  | FRange.NaN, FRange.NaN -> 0

let has_smaller_max_bound x1 x2 =
  match x1, x2 with
  | FRange.I (_, m1, _), FRange.I (_, m2, _) -> F.compare m2 m1
  | FRange.NaN, FRange.I _ -> 1
  | FRange.I _, FRange.NaN -> -1
  | FRange.NaN, FRange.NaN -> 0

(* --------------------------------------------------------------------------
                                   Tests
   -------------------------------------------------------------------------- *)

let contains_plus_zero = is_included plus_zero
let contains_minus_zero = is_included minus_zero

(* Returns true if [f] is certainly a zero (positive, negative or both). *)
let is_a_zero f = is_included f zeros

let contains_a_zero = function
  | FRange.I(b, e, _) -> F.le_ieee b F.plus_zero && F.ge_ieee e F.plus_zero
  | FRange.NaN -> false

let contains_non_zero = function
  | FRange.I(b, e, nan) ->
    nan || F.lt_ieee b F.plus_zero || F.gt_ieee e F.plus_zero
  | FRange.NaN -> true

let contains_strictly_pos = function
  | FRange.I(_, e, _) -> F.gt_ieee e F.plus_zero
  | FRange.NaN -> false

let contains_strictly_neg = function
  | FRange.I(b, _, _) -> F.lt_ieee b F.minus_zero
  | FRange.NaN -> false

let contains_strict_neg_finite (b, e) =
  F.gt_ieee e F.neg_infinity && F.lt b F.minus_zero

let contains_finite_noninteger fkind (b, e) =
  let ib = ceil b in
  not ((F.equal_ieee ib b && F.equal_ieee b e)
       || e <= F.floor_of_most_negative_representable_non_integer fkind
       || b >= F.ceil_of_largest_representable_non_integer fkind)

let contains_pos_infinity = function
  | FRange.I(_, e, _) -> F.equal_ieee F.pos_infinity e
  | FRange.NaN  -> false

let contains_neg_infinity = function
  | FRange.I(b, _, _) -> F.equal_ieee F.neg_infinity b
  | FRange.NaN  -> false

let contains_infinity f =
  contains_pos_infinity f || contains_neg_infinity f

let contains_nan = function
  | FRange.I(_, _, nan) -> nan
  | FRange.NaN  -> true

let is_singleton = function
  | FRange.I(b, e,false) -> F.equal b e
  | FRange.I(_, _,true) -> false
  | FRange.NaN -> true (* intentional, see .mli *)

let if_not_nan = function
  | FRange.NaN -> assert false
  | FRange.I(b,e,_) -> b,e

let finite_values fkind = function
  | FRange.NaN -> None
  | FRange.I(b,e,_) ->
    let min = F.max (F.most_negative_representable_float fkind) b in
    let max = F.min (F.max_representable_float fkind) e in
    if max < min then None else Some (min, max)

let is_not_nan = function
  | FRange.NaN -> Comp.False
  | FRange.I (_b, _e, nan) -> if nan then Comp.Unknown else Comp.True

let is_finite = function
  | FRange.NaN -> Comp.False
  | FRange.I (b, e, nan) ->
    if F.equal e F.neg_infinity || F.equal b F.pos_infinity
    then Comp.False
    else if nan || F.equal b F.neg_infinity || F.equal e F.pos_infinity
    then Comp.Unknown
    else Comp.True

let backward_is_not_nan = function
  | FRange.NaN -> `Bottom
  | FRange.I (b, e, _) -> `Value (FRange.inject_raw ~nan:false b e)

let backward_is_finite fkind = function
  | FRange.NaN -> `Bottom
  | FRange.I (b, e, _) as f ->
    if F.equal b e && F.is_infinite b
    then `Bottom (* [f] is exactly an infinite, we can return `Bottom even
                    in the [Real] case *)
    else narrow (top_finite fkind) f

(* --------------------------------------------------------------------------
                                Comparisons
   -------------------------------------------------------------------------- *)

let forward_eq (b1,e1) (b2,e2) =
  let not_intersects =
    F.lt_ieee e2 b1 || F.lt_ieee e1 b2
  in
  if not_intersects
  then Comp.False
  else if F.equal_ieee b1 e1 && F.equal_ieee b2 e2
  then Comp.True
  else Comp.Unknown

let forward_le (b1, e1) (b2, e2) =
  if F.le_ieee e1 b2 then Comp.True
  else if F.lt_ieee e2 b1 then Comp.False
  else Comp.Unknown

let forward_lt (b1, e1) (b2, e2) =
  if F.lt_ieee e1 b2 then Comp.True
  else if F.le_ieee e2 b1 then Comp.False
  else Comp.Unknown

let forward_comp op f1 f2 = match f1, f2 with
  | FRange.NaN, _ | _, FRange.NaN ->
    if op = Comp.Ne then Comp.True else Comp.False
  | FRange.I (b1, e1, nan1), FRange.I (b2, e2, nan2) ->
    let r = match op with
      | Comp.Le -> forward_le (b1, e1) (b2, e2)
      | Comp.Ge -> forward_le (b2, e2) (b1, e1)
      | Comp.Lt -> forward_lt (b1, e1) (b2, e2)
      | Comp.Gt -> forward_lt (b2, e2) (b1, e1)
      | Comp.Eq -> forward_eq (b1, e1) (b2, e2)
      | Comp.Ne -> inv_truth (forward_eq (b1, e1) (b2, e2))
    in
    if nan1 || nan2
    then
      if op = Comp.Ne
      then (match r with Comp.True -> Comp.True | _ -> Comp.Unknown)
      else (match r with Comp.False -> Comp.False | _ -> Comp.Unknown)
    else r

(* This function intentionally returns different results with
   [e2 = -0.] and [e2 = 0.] *)
let backward_le_aux fkind (b1, e1) e2 =
  if not (F.le b1 e2)
  then `Bottom
  else if F.le e1 e2
  then `Value (FRange.inject fkind b1 e1)
  else inject_after_tighten fkind ~nan:false b1 e2

(* This is the "real" backward transformer for [le], which does not distinguish
   [0.] and [-0.]. Thus we enlarge the bound in the "worst" direction. *)
let backward_le fkind (b1, e1) e2 =
  let e2 = if F.is_a_zero e2 then F.plus_zero else e2 in
  backward_le_aux fkind (b1, e1) e2

let backward_lt fkind ((b1, e1) as f1) e2 =
  if F.le_ieee e2 b1
  then `Bottom
  else
  if fkind = Real && not (F.equal b1 e1) then
    (* On real we cannot be more precise than [le], except on zeros: at
       least get rid of the "bad" zero *)
    let e2 = if F.is_a_zero e2 then F.minus_zero else e2 in
    backward_le_aux fkind f1 e2
  else
    backward_le fkind f1 (F.prev_float_ieee fkind e2)

(* see comments in {!backward_le_aux} *)
let backward_ge_aux fkind (b1, e1) b2 =
  if not (F.le b2 e1)
  then `Bottom
  else if F.le b2 b1
  then `Value (FRange.inject fkind b1 e1)
  else inject_after_tighten fkind ~nan:false b2 e1

(* see comments in {!backward_le} *)
let backward_ge fkind (b1, e1) b2 =
  let b2 = if F.is_a_zero b2 then F.minus_zero else b2 in
  backward_ge_aux fkind (b1, e1) b2

(* see comments in {!backward_gt} *)
let backward_gt fkind ((b1, e1) as f1) b2 =
  if F.le_ieee e1 b2
  then `Bottom
  else
  if fkind = Real && not (F.equal b1 e1) then
    let b2 = if F.is_a_zero b2 then F.plus_zero else b2 in
    backward_ge_aux fkind f1 b2
  else
    backward_ge fkind f1 (F.next_float_ieee fkind b2)

(** The operands cannot be {!Nan} *)
let backward_comp_left_true_finite op fkind f1' f2' =
  let f1 = if_not_nan f1' in
  let (b2,e2) = if_not_nan f2' in
  match op with
  | Comp.Le -> backward_le fkind f1 e2
  | Comp.Ge -> backward_ge fkind f1 b2
  | Comp.Lt -> backward_lt fkind f1 e2
  | Comp.Gt -> backward_gt fkind f1 b2
  | Comp.Eq ->
    (* -0 and +0 must not be distinguished here *)
    let f2 = if contains_a_zero f2' then join f2' zeros else f2' in
    narrow f1' f2
  | Comp.Ne ->
    (* compute (f1 ∩ [-infty,min[ ) ∪ (f1 ∩ ]max,infty]) *)
    let before_or_after min max =
      Bottom.join join
        (backward_lt fkind f1 min) (backward_gt fkind f1 max)
    in
    (* As usual, we cannot reduce if [f2] is not a singleton, except that
       the two zeros are a kind of singleton. Checking whether [f2] is on
       a frontier of [f1] is not obvious because of the multiple cases
       (and [allmodes]) so we use the transformers for [lt] instead. *)
    if is_a_zero f2' then before_or_after F.minus_zero F.plus_zero
    else if is_singleton f2' then before_or_after b2 b2
    else `Value f1'

(* Applies [backward f1 f2] and removes NaN from [f1] and [f2]. *)
let backward_comp_no_nan backward_finite f1 f2 =
  match f1, f2 with
  | FRange.NaN, _ | _, FRange.NaN -> `Bottom
  | FRange.I (b, e, nan), FRange.I _ ->
    let f1 = if nan then FRange.inject_raw ~nan:false b e else f1 in
    backward_finite f1 f2

(* Applies [backward f1 f2] but preserves NaN from [f1] and [f2]. *)
let backward_comp_with_nan backward_finite f1 f2 =
  if contains_nan f2
  then `Value f1
  else
    match f1 with
    | FRange.NaN -> `Value f1
    | FRange.I (_, _, nan) ->
      let nan = if nan then `Value FRange.nan else `Bottom in
      Bottom.join join (backward_finite f1 f2) nan

let backward_comp_left_true op kind =
  let backward_finite = backward_comp_left_true_finite op kind in
  if op = Comp.Ne
  then backward_comp_with_nan backward_finite
  else backward_comp_no_nan backward_finite

let backward_comp_left_false op kind =
  let backward_finite = backward_comp_left_true_finite (Comp.inv op) kind in
  if op = Comp.Ne
  then backward_comp_no_nan backward_finite
  else backward_comp_with_nan backward_finite

(* --------------------------------------------------------------------------
                        Simple arithmetic operations
   -------------------------------------------------------------------------- *)

(* The functions defined using [exact_aux] below are, among other
   properties, (1) exact (the result as a real can always be
   represented exactly, in the good type), and (2) total. In
   particular, given a float 'x', 'ff x == (float)(f (double)x)'.
   Thus, in this module, the 'f' functions are also the non-f
   (since float32 are represented using double) *)
let exact_aux ff fkind = function
  | FRange.NaN as f -> f
  | FRange.I (b, e, nan) ->
    (* [ff] returns NaN only if the argument is NaN. *)
    FRange.inject fkind ~nan (ff b) (ff e)

let floor = exact_aux F.floor
let ceil = exact_aux F.ceil
let trunc = exact_aux F.trunc
let fround = exact_aux F.fround

let neg = function
  | FRange.I(b, e, nan) ->
    (* do not round because exact operation *)
    FRange.inject_raw ~nan (F.neg e) (F.neg b)
  | FRange.NaN -> FRange.nan

let abs = function
  | (FRange.I(b, e,nan) as f) ->
    if contains_plus_zero f then
      FRange.inject_raw ~nan F.plus_zero (F.max (abs_float b) (abs_float e))
    else (* f is either strictly positive or strictly negative *)
    if F.compare e F.plus_zero < 0 then neg f else f
  | FRange.NaN as f -> f

(* This monad returns a NaN if one operand can only be NaN, and lets the second
   function perform the computation if both operands contain a non-empty
   floating-point interval. *)
let ( >>% ) = fun (x,y) f -> match x, y with
  | FRange.NaN, _ | _, FRange.NaN -> FRange.nan
  | FRange.I (b1, e1, nan1), FRange.I (b2, e2, nan2) ->
    let nan = nan1 || nan2 in
    f ~nan (b1, e1) (b2, e2)

(* Auxiliary function used for the forward semantics of add, mul and div.
   For a monotonic function [op], the bounds of [[b1..e1] op [b2..e2]] are the
   minimum and maximum of [b1 op b2], [b1 op e2], [e1 op b2] and [e1 op e2].
   NaN can be created from \infty - \infty, 0 * \infty, 0/0 and \infty / \infty,
   in which case the result contains NaN, and new operations are performed to
   take into account the results of values near \infty and 0.
   Beware that NaN and discontinuities occuring between the bounds of the
   arguments (i.e. on zeros, as an infinity is always a bound) should be checked
   and processed by the caller. *)
let monotonic op fkind x y =
  (x, y) >>% fun ~nan (b1, e1) (b2, e2) ->
  let nan = ref nan in
  (* Results of [op] applied to the bounds of the intervals, excluding NaN. *)
  let results = ref [] in
  (* When [a op b = NaN], performs new operations to take into account values
     near [a] (and near [b] with the same reasoning). For such a NaN from add,
     mul or div, [c op b] is constant for all values c <> a of the same sign.
     Thus, we can replace [a] by any of these values.
     - if [x] is a singleton or [-0 .. +0], there are no values other than the
       bounds to take into account;
     - otherwise, if [a] is infty, replace it by 1 with the sign of [a]
       (no risk of NaN with 1 on add, mul and div);
     - otherwise, if [a] is zero, the other bound [c] of the interval has the
       same sign as the values near [a] in the interval; as [c op b] is also
       computed, no need to perform a new operation. *)
  let treat_nan_result a b =
    nan := true;
    if classify_float a = FP_infinite && not (F.equal b1 e1)
    then results := op (copysign 1. a) b :: !results;
    if classify_float b = FP_infinite && not (F.equal b2 e2)
    then results := op a (copysign 1. b) :: !results;
  in
  let op x y =
    let r = op x y in
    if F.is_nan r
    then treat_nan_result x y
    else results := r :: !results
  in
  let compute () =
    results := [];
    op e1 e2; op e1 b2; op b1 e2; op b1 b2;
  in
  if fkind = Real
  then Floating_point.set_round_downward ()
  else Floating_point.set_round_nearest_even ();
  compute ();
  let min = List.fold_left F.min F.pos_infinity !results in
  if fkind = Real then begin
    Floating_point.set_round_upward ();
    compute ();
    Floating_point.set_round_nearest_even ();
  end;
  let max = List.fold_left F.max F.neg_infinity !results in
  if min > max
  then (assert !nan; FRange.nan)
  else FRange.inject_rounded fkind ~nan:!nan min max

let add = monotonic ( +. )
let sub = monotonic ( -. )

let mul fkind x y =
  let r = monotonic ( *. ) fkind x y in
  (* A NaN may occur between the bounds of the intervals, on 0 * \infty. *)
  if (contains_infinity x && contains_a_zero y) ||
     (contains_infinity y && contains_a_zero x)
  then FRange.add_nan r
  else r

let div fkind x y =
  let r = monotonic ( /. ) fkind x y in
  (* A NaN may occur between the bounds of the intervals, on 0/0. *)
  let nan = (contains_a_zero x && contains_a_zero y) in
  (* Treat the discontinuity around 0: divisions by 0 produce infinites. *)
  let pos_inf =
    contains_plus_zero y && contains_strictly_pos x ||
    contains_minus_zero y && contains_strictly_neg x
  and neg_inf =
    contains_plus_zero y && contains_strictly_neg x ||
    contains_minus_zero y && contains_strictly_pos x
  in
  let r = if pos_inf then join pos_infinity r else r in
  let r = if neg_inf then join neg_infinity r else r in
  if nan then FRange.add_nan r else r

(* Could be improved a lot, cf [Marre10]. *)
let backward_add_one fkind ~other ~result =
  (* No reduction when the result contains an infinity, and when the result and
     the other operand contain NaN (as x + NaN = NaN for any x). *)
  if contains_infinity result || (contains_nan other && contains_nan result)
  then `Value top
  else
    (* Values that can lead to NaN in the result. *)
    let reduce_for_nan () =
      match contains_pos_infinity other, contains_neg_infinity other with
      | true, true   -> FRange.add_nan (join neg_infinity pos_infinity)
      | true, false  -> FRange.add_nan neg_infinity
      | false, true  -> FRange.add_nan pos_infinity
      | false, false -> FRange.nan
    in
    let reduced_for_nan =
      if contains_nan result then `Value (reduce_for_nan ()) else `Bottom
    in
    (* Values that can lead to finite values in the result. *)
    let reduced_for_finite_values =
      match finite_values fkind result, finite_values fkind other with
      | None, _ | _, None  -> `Bottom
      | Some (bres, eres), Some (bother, eother) ->
        let bres = F.prev_float_ieee fkind bres in
        let eres = F.next_float_ieee fkind eres in
        if fkind <> Real then Floating_point.set_round_upward ();
        let b = F.sub bres eother in
        if fkind <> Real then Floating_point.set_round_downward ();
        let e = F.sub eres bother in
        if fkind <> Real then Floating_point.set_round_nearest_even ();
        inject_after_tighten fkind ~nan:false b e
    in
    Bottom.join join reduced_for_nan reduced_for_finite_values

let backward_add fkind ~left ~right ~result =
  backward_add_one fkind ~other:right ~result >>- fun left' ->
  backward_add_one fkind ~other:left ~result >>- fun right' ->
  `Value (left', right')

let backward_sub fk ~left ~right ~result =
  let right = neg right in
  backward_add fk ~left ~right ~result
  >>-: fun (left, right) -> (left, neg right)

(* --------------------------------------------------------------------------
                           Exp Log Sqrt Pow Fmod
   -------------------------------------------------------------------------- *)

(* Used to generate the proper forward semantics of [fgen] for each fkind by
   using the float [ff] and the double [df] functions. *)
let gen_two_version_function fgen df ff fkind f =
  match fkind with
  | Float64 | Real -> fgen df fkind f
  | Float32 -> fgen ff fkind f

(** See discussion in the .mli about [fkind] *)
(* [exp_f] is the actual underlying function computing the exponential,
   according to the desired precision: expf for single precision,
   exp for double precision. *)
let exp' exp_f fkind = function
  | FRange.NaN as f -> f
  | FRange.I (b, e, nan) ->
    if fkind = Real
    then Floating_point.set_round_downward ()
    else Floating_point.set_round_nearest_even ();
    let min = exp_f b in
    if fkind = Real then Floating_point.set_round_upward ();
    let max = exp_f e in
    if fkind = Real then Floating_point.set_round_nearest_even ();
    FRange.inject fkind ~nan min max

let exp = gen_two_version_function exp' F.exp F.expf

(** See discussion in the .mli about [fkind] *)
let log_float_aux flog fkind = function
  | FRange.NaN as f -> f
  | FRange.I (b, e, nan) ->
    if F.(lt e minus_zero)
    then FRange.nan
    else
      let nan = nan || F.(lt b minus_zero) in
      let b_reduced = F.max F.minus_zero b in
      if fkind = Real
      then Floating_point.set_round_downward ()
      else Floating_point.set_round_nearest_even ();
      let min = flog b_reduced in
      if fkind = Real then Floating_point.set_round_upward ();
      let max = flog e in
      if fkind = Real then Floating_point.set_round_nearest_even ();
      FRange.inject fkind ~nan min max

let log = gen_two_version_function log_float_aux F.log F.logf
let log10 = gen_two_version_function log_float_aux F.log10 F.log10f

(* [sqrt_f] is the actual function computing the (exact) square root,
   in single precision (sqrtf) or double precision (sqrt). *)
let sqrt' sqrt_f fkind = function
  | FRange.NaN -> FRange.nan
  | FRange.I(b,e,nan) ->
    if F.lt_ieee e F.minus_zero then
      FRange.nan
    else
      let nan, min =
        if F.ge_ieee b F.minus_zero
        then begin
          if fkind = Real
          then Floating_point.set_round_downward ()
          else Floating_point.set_round_nearest_even ();
          let min = sqrt_f b in
          if fkind = Real then Floating_point.set_round_nearest_even ();
          nan, min
        end
        else
          (* case e < 0 treated above, some values are positive or zero *)
          true, F.minus_zero
      in
      if fkind = Real then Floating_point.set_round_upward ();
      let max = sqrt_f e in
      if fkind = Real then Floating_point.set_round_nearest_even ();
      FRange.inject_raw ~nan min max

let sqrt = gen_two_version_function sqrt' F.sqrt F.sqrtf


let value_if condition value = if condition then `Value value else `Bottom

(* Returns the minimal or maximal (according to [min_or_max]) results of the
   binary operation [op] applied to the bounds of the intervals [b1..e1] and
   [b2..e2]. *)
let extremum min_or_max op (b1, e1) (b2, e2) =
  let extremum4 a b c d = min_or_max a (min_or_max b (min_or_max c d)) in
  extremum4 (op b1 b2) (op b1 e2) (op e1 b2) (op e1 e2)

(* Returns the minimum and maximum results of the binary operation [op] applied
   to the bounds of the intervals [b1..e1] and [b2..e2]. *)
let extrema op (b1, e1) (b2, e2) =
  let a = op b1 b2 and b = op b1 e2 and c = op e1 b2 and d = op e1 e2 in
  F.min a (F.min b (F.min c d)), F.max a (F.max b (F.max c d))

(* Computes [pow_f] on a negative [bx; ex] interval (including infinites).
   Processes by disjunction over even and odd integers enclosed within [by; ey].
   [pow] is then monotonic on even integers (including zeros and infinities),
   and on odd integers (except on infinities). *)
let pow_negative_x pow_f fkind (bx, ex as x) (by, ey as y) =
  let even, odd = split_by_parity fkind y in
  (* Even integers [y] lead to positive results, while odd ones lead to negative
     results.  When [y] contains both even and odd integers, the minimum result
     is in odd integers, and the maximum in even integers. *)
  let min, max = match even, odd with
    | None, None -> F.pos_infinity, F.neg_infinity
    | Some even, None -> extrema pow_f x even
    | None, Some odd  -> extrema pow_f x odd
    | Some even, Some odd ->
      extremum F.min pow_f x odd, extremum F.max pow_f x even
  in
  let nonint_y = contains_finite_noninteger fkind y in
  (* pow creates NaN when [x] is a negative non-zero finite value, and [y] a
     non integer value. *)
  let nan = contains_strict_neg_finite x && nonint_y in
  (* Special cases of neg_infinity and minus_zero for [x], that do not produce
     a NaN on non integer [y], unlike strictly negative finite values [x]. *)
  let neg_nonint_y = nonint_y && F.(lt by minus_zero) in
  let pos_nonint_y = nonint_y && F.(gt ey plus_zero) in
  let neg_infinity_x = F.(equal neg_infinity bx) in
  let zero_x = F.(equal minus_zero ex) in
  Bottom.join_list join
    [ if F.le min max then `Value (FRange.inject fkind min max) else `Bottom;
      value_if nan FRange.nan;
      value_if (neg_infinity_x && neg_nonint_y) plus_zero;
      value_if (neg_infinity_x && pos_nonint_y) pos_infinity;
      value_if (zero_x && neg_nonint_y) pos_infinity;
      value_if (zero_x && pos_nonint_y) plus_zero ]

(* Computes pow on a positive [bx; ex] interval (including infinites): the
   function is continuous and monotonic. *)
let pow_positive_x pow_f fkind x y =
  let min, max = extrema pow_f x y in
  FRange.inject fkind min max

let pow' pow_f fkind x y =
  Floating_point.set_round_nearest_even ();
  if equal one x || is_a_zero y then one
  else
    (x, y) >>% fun ~nan itv_x itv_y ->
    (* Split the x interval around zeros, as pow is discontinuous on zeros. *)
    let neg_x, pos_x = split_by_sign itv_x in
    let pos_x_res = pos_x >>-: fun x -> pow_positive_x pow_f fkind x itv_y in
    let neg_x_res = neg_x >>- fun x -> pow_negative_x pow_f fkind x itv_y in
    Bottom.non_bottom
      (Bottom.join_list join [ pos_x_res; neg_x_res; value_if nan FRange.nan ])

let pow = gen_two_version_function pow' F.pow F.powf

(* Is [fmod] continuous on positive intervals [b1..e1] and [b2..e2]?
   This is the case if for all x, y in these intervals, the rounded quotient
   [floor(x/y)] is constant, as [fmod x y = x - floor(x/y) * y] when [x] and [y]
   are positive.
   Also checks that [x/y < 2^53], otherwise truncation to an integer may return
   an incorrect result. Note: to avoid issues with rounding, we conservatively
   set the limit to 2^51 instead of 2^53 (and to 2^21 instead of 2^23 in single
   precision). *)
let is_fmod_continuous fkind (b1, e1) (b2, e2) =
  (* Discontinuity of [fmod x y] on infinite [x] and on zero [y]. *)
  F.is_finite e1 && F.gt_ieee b2 F.plus_zero &&
  let max_i = match fkind with
    | Float32 -> F.max_single_precise_integer /. 4.
    | Float64 | Real -> F.max_precise_integer /. 4.
  in
  Floating_point.set_round_toward_zero ();
  let f1 = F.floor (F.round fkind (b1 /. e2)) in
  let f2 = F.floor (F.round fkind (e1 /. b2)) in
  Floating_point.set_round_nearest_even ();
  F.equal f1 f2 && F.le f1 max_i

(* Forward semantics of fmod on positive intervals. [x] must contain finite
   values, and [y] must contain non-zero values, in which case finite values
   are produced. This function does not check the creation of NaN. *)
let positive_fmod fmod_f fkind (b1, e1 as x) (b2, e2 as y) =
  Floating_point.set_round_nearest_even ();
  (* Singleton case. [x] cannot be infinite, and [y] cannot be zero. *)
  if F.equal b1 e1 && F.equal b2 e2
  then let c = fmod_f b1 b2 in c, c
  (* If all values of x are smaller than all values of [y], [x] is unchanged. *)
  else if F.lt_ieee e1 b2
  then x
  (* If fmod is continuous on the intervals [x] and [y], it is also monotonic,
     and the bounds of the result are the remainders of the bounds. *)
  else if is_fmod_continuous fkind x y
  then fmod_f b1 e2, fmod_f e1 b2
  (* Otherwise, fmod always satisfies 0 <= [fmod x y] <= x and [fmod x y] < y. *)
  else let max = F.min e1 (F.prev_float fkind e2) in (F.plus_zero, max)

let fmod fmod_f fkind x y =
  (* [fmod x (-y)] = [fmod x y], so use only positive [y]. *)
  let y = abs y in
  (x, y) >>% fun ~nan (b1, e1) (b2, e2) ->
  (* If [x] is an infinite singleton, or if [y] contains only zero, only NaN
     can be created. *)
  if (F.equal b1 e1 && F.is_infinite b1) || (F.equal b2 e2 && F.is_a_zero b2)
  then FRange.nan
  else
    let nan = nan || contains_infinity x || contains_a_zero y in
    let positive_fmod x =
      let b, e = positive_fmod fmod_f fkind x (b2, e2) in
      FRange.inject_raw ~nan b e
    in
    (* Process by disjunction on the sign of [x], and join the results for
       negative x and for positive x. *)
    let neg_x, pos_x = split_by_sign (b1, e1) in
    let neg_itv (b, e) = -.e, -.b in
    let neg_r = neg_x >>-: neg_itv >>-: positive_fmod >>-: neg in
    let pos_r = pos_x >>-: positive_fmod in
    Bottom.non_bottom (Bottom.join join neg_r pos_r)

let fmod = gen_two_version_function fmod F.fmod F.fmodf

(* --------------------------------------------------------------------------
                                Trigonometry
   -------------------------------------------------------------------------- *)

(* It is wrong to use m_pi as the local minimum as was previously done,
   because:
   A = 3.14159265358979312 and A < m_pi, cos A = 1. and cos m_pi = 0.999
   Moreover it is not due to the imprecision in the value of pi:
   A < pred m_pi in simple.

   So we use a quarter of the interval [-pi:pi] to be safe. (But still,
   nothing proves that cos or sin are monotonic on those ranges.) *)
let cos_sin_security = F.m_pi /. 4.
let cos_sin run _fkind = function
  | FRange.NaN as f -> f
  | FRange.I (b, e, nan) ->
    Floating_point.set_round_nearest_even ();
    (* Special case when at least a bound is infinite. *)
    if F.is_infinite b || F.is_infinite e
    then if F.equal b e then FRange.nan else FRange.add_nan minus_one_one
    (* [b] and [e] are finite. Precise case for a singleton. *)
    else if F.equal b e
    then let c = run b in FRange.inject_raw ~nan c c
    else if F.le_ieee b (-. cos_sin_security) || F.le_ieee cos_sin_security e
    then FRange.inject_raw ~nan F.minus_one F.one
    else
      let xl = if F.lt b 0. && F.lt 0. e then [b; e; 0.] else [b; e] in
      let l = List.map run xl in
      let min_f = List.fold_left F.min F.pos_infinity l in
      let max_f = List.fold_left F.max F.neg_infinity l in
      FRange.inject_raw ~nan min_f max_f

let cos = gen_two_version_function cos_sin F.cos F.cosf
let sin = gen_two_version_function cos_sin F.sin F.sinf

let atan2 atan2_f fkind x y =
  match x,y with
  | (FRange.NaN, _) | (_, FRange.NaN) -> FRange.nan
  | FRange.I(b1,e1,nan1), FRange.I(b2,e2,nan2) ->
    let nan = nan1 || nan2 in
    Floating_point.set_round_nearest_even ();
    let res =
      if F.equal b1 e1 && F.equal b2 e2 then begin
        let c = atan2_f b1 b2 in
        FRange.inject_raw ~nan c c
      end
      else
        (* Unless y ([b1,e1]) crosses the x-axis, atan2 is continuous,
           and its minimum/maximum are at the ends of the intervals of x and y.
           Otherwise, the result is [-pi,+pi]. *)
      if not (F.compare b1 F.plus_zero < 0 && F.compare e1 F.minus_zero > 0) then
        let a1, a2, a3, a4 = atan2_f b1 b2, atan2_f b1 e2,
                             atan2_f e1 b2, atan2_f e1 e2
        in
        let b = F.min a1 (F.min a2 (F.min a3 a4)) in
        let e = F.max a1 (F.max a2 (F.max a3 a4)) in
        match fkind with
        | Real | Float64 -> FRange.inject_raw ~nan b e
        | Float32 ->
          (* Rounding of atan2f in single-precision may go against monotony and
             reach the next (previous) float after (before) the bounds. *)
          FRange.inject_raw ~nan (F.prev_float fkind b) (F.next_float fkind e)
      else
      if nan then FRange.add_nan minus_pi_pi else minus_pi_pi
    in res

let atan2 = gen_two_version_function atan2 F.atan2 F.atan2f

(* --------------------------------------------------------------------------
                      Bitwise reinterpretation and casts
   -------------------------------------------------------------------------- *)

(* Bitwise reinterpretation of a floating-point value into consecutive
   ranges of integer. (Thus, this operation is exact in terms of
   concretization.) 'Parametric' in the number of bits. *)
let bits_of_float_list ~bits_of_float ~succ ~minus_one ~max_int ~of_inti ?check () =
  let itvs_nan =
    let smallest_neg_nan = of_inti (succ (bits_of_float F.neg_infinity)) in
    let biggest_neg_nan  = of_inti minus_one in
    let smallest_pos_nan = of_inti (succ (bits_of_float F.pos_infinity)) in
    let biggest_pos_nan  = of_inti max_int in
    [(smallest_neg_nan, biggest_neg_nan);
     (smallest_pos_nan, biggest_pos_nan)]
  in
  function
  | FRange.NaN -> itvs_nan
  | FRange.I(b, e, nan) ->
    begin match check with
      | None -> ()
      | Some check ->
        assert (check b);
        assert (check e);
    end;
    let nan = if nan then itvs_nan else [] in
    let neg, pos = split_by_sign (b, e) in
    let neg =
      neg >>-: fun (b, e) ->
      of_inti (bits_of_float e), of_inti (bits_of_float b)
    in
    let pos =
      pos >>-: fun (b, e) ->
      of_inti (bits_of_float b), of_inti (bits_of_float e)
    in
    Bottom.add_to_list pos (Bottom.add_to_list neg nan)

let bits_of_float64_list =
  bits_of_float_list
    ~bits_of_float:Int64.bits_of_float
    ~succ:Int64.succ
    ~minus_one:Int64.minus_one
    ~max_int:Int64.max_int
    ~of_inti:Int.of_int64
    ?check:None
    ()

let bits_of_float32_list =
  let check l = F.equal l (Floating_point.round_to_single_precision_float l) in
  bits_of_float_list
    ~bits_of_float:Int32.bits_of_float
    ~succ:Int32.succ
    ~minus_one:Int32.minus_one
    ~max_int:Int32.max_int
    ~of_inti:Int.of_int32
    ~check
    ()

(* This function must make sure to return a result with float 32 bounds *)
let backward_cast_float_to_double = tighten_bound_by_rounding Float32

(** real are here not more precise than doubles *)
let backward_cast_double_to_real f = f

let cast_int_to_float fkind min max =
  let min = match min with
    | None -> F.neg_infinity
    | Some v ->
      if fkind = Real
      then Floating_point.set_round_downward ();
      F.round fkind (Int.to_float v)
  in
  let max = match max with
    | None -> F.pos_infinity
    | Some v ->
      if fkind = Real
      then Floating_point.set_round_upward ();
      F.round fkind (Int.to_float v)
  in
  if fkind = Real then Floating_point.set_round_nearest_even ();
  FRange.inject fkind min max

(* --------------------------------------------------------------------------
                                 Subdivision
   -------------------------------------------------------------------------- *)

let avg x y =
  let h = 0.5 in
  let xp = x >= 0. in
  let yp = y >= 0. in
  if xp = yp
  then
    let d = x -. y in y +. h *. d
  else
    (x +. y) *. h

(* assumption: [0. <= x <= y]. returns the median of the range [x..y]
   in number of double values. *)
let split_positive x y =
  let ix = Int64.bits_of_float x in
  let iy = Int64.bits_of_float y in
  Int64.(float_of_bits (add ix (div (sub iy ix) 2L)))

(* assumption: [x <= y] *)
let _split x y =
  match F.is_negative x, F.is_negative y with
  | false, false -> split_positive x y
  | true, true -> -. (split_positive (-.x) (-.y))
  | true, false -> F.minus_zero
  | false, true -> assert false


let subdiv_float_interval fkind = function
  | FRange.NaN -> raise Can_not_subdiv
  | FRange.I(l, u, true) -> FRange.inject_raw ~nan:false l u, FRange.nan
  | (FRange.I(l, u, false) as i) ->
    assert (fkind <> Real); (* See Value/Value#105 *)
    let midpointl, midpointu =
      (* infinities are interesting points to consider specially *)
      if F.is_neg_infinity l
      then F.neg_infinity, F.most_negative_representable_float fkind
      else if F.is_pos_infinity u
      then F.max_representable_float fkind, F.pos_infinity
      else
        let midpoint = avg l u in
        match fkind with
        | Real -> midpoint, midpoint
        | Float64 | Float32 ->
          let smidpoint = F.next_float Float64 midpoint in
          match fkind with
          | Real -> assert false
          | Float64 ->
            if F.le smidpoint u
            then
              if F.next_float Float64 l = u
              then
                l, u
              else
                midpoint, smidpoint
            else midpoint, u
          | Float32 ->
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
    in
    if ((not (F.is_neg_infinity l)) && F.le midpointu l)
    || ((not (F.is_pos_infinity u)) && F.le u midpointl)
    then raise Can_not_subdiv;
    (*    Format.printf "%a %a %a %a@."
          (F.pretty_normal ~use_hex:true) l
          (F.pretty_normal ~use_hex:true) midpointl
          (F.pretty_normal ~use_hex:true) midpointu
          (F.pretty_normal ~use_hex:true) u; *)
    let i1 = FRange.inject_raw l midpointl in
    assert (is_included i1 i);
    let i2 = FRange.inject_raw midpointu u in
    assert (is_included i2 i);
    i1, i2

(*
Local Variables:
compile-command: "make -C ../../.. byte"
End:
*)
