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

module Comp = Abstract_interp.Comp
open Bottom.Type

type round = Float_sig.round = Up | Down | Near | Zero
type prec = Float_sig.prec = Single | Double | Long_Double | Real

module Make (F: Float_sig.S) = struct

  (* Definitions of useful floating-point constants. *)
  module Cst = struct
    let pos_zero prec = F.of_float Near prec 0.
    let neg_zero prec = F.of_float Near prec (- 0.)
    let pos_infinity prec = F.of_float Near prec infinity
    let neg_infinity prec = F.of_float Near prec neg_infinity

    (* Maximum value M such that all integers 0 < n <= M are exactly
       representable in the given precision.
       It is 2^53 for double and 2^24 for single precision. *)
    let max_precise_integer prec =
      let f = match prec with
        | Single -> 2. ** 24.
        | Double | Long_Double -> 2. ** 53.
        | Real -> infinity
      in
      F.of_float Near prec f

    (* Maximum odd integer representable in the given precision.
       It is 2^53-1 for double and 2^24-1 for single precision. *)
    let max_odd_integer prec =
      let f = match prec with
        | Single -> 2. ** 24. -. 1.
        | Double -> 2. ** 53. -. 1.
        | Real | Long_Double -> infinity
      in
      F.of_float Near prec f

    (* Successor of the maximum non-integer representable in the given precision.
       It is 2^52 in double and 2^23 in single precision. *)
    let ceil_of_max_non_integer prec =
      let f = match prec with
        | Single -> 2. ** 23.
        | Double -> 2. ** 52.
        | Real | Long_Double -> infinity
      in
      F.of_float Near prec f

    (* Returns 1.0 if [f] is positive, and -1.0 if [t] is negative.
       [f] can be infinite or a (positive or negative) zero, but not NaN. *)
    let sign prec f =
      let s = if F.is_negative f then -1. else 1. in
      F.of_float Near prec s
  end

  (* IEEE and non-IEEE comparisons. *)
  module Cmp = struct
    (* Constants in single precision: should be used only for comparisons,
       and not to create intervals of arbitrary precisions. *)
    let pos_zero = Cst.pos_zero Float_sig.Single
    let neg_zero = Cst.neg_zero Float_sig.Single
    let one = F.of_float Near Float_sig.Single 1.
    let pos_infinity = Cst.pos_infinity Float_sig.Single
    let neg_infinity = Cst.neg_infinity Float_sig.Single

    let equal_ieee f1 f2 = F.cmp_ieee f1 f2 = 0
    let le_ieee f1 f2 = F.cmp_ieee f1 f2 <= 0
    let lt_ieee f1 f2 = F.cmp_ieee f1 f2 < 0
    let ge_ieee f1 f2 = F.cmp_ieee f1 f2 >= 0
    let gt_ieee f1 f2 = F.cmp_ieee f1 f2 > 0

    let is_a_zero f = equal_ieee pos_zero f
    let is_pos_infinity f = equal_ieee pos_infinity f
    let is_neg_infinity f = equal_ieee neg_infinity f

    let equal f1 f2 = F.compare f1 f2 = 0
    let le f1 f2 = F.compare f1 f2 <= 0
    let lt f1 f2 = F.compare f1 f2 < 0
    let ge f1 f2 = F.compare f1 f2 >= 0
    let gt f1 f2 = F.compare f1 f2 > 0

    let min f1 f2 = if le f1 f2 then f1 else f2
    let max f1 f2 = if le f1 f2 then f2 else f1

    (* next_float_ieee -0. = next_float_ieee 0. *)
    let next_float_ieee prec f =
      let f = if equal f neg_zero then Cst.pos_zero prec else f in
      F.next_float prec f

    (* prev_float_ieee 0. = next_float_ieee -0. *)
    let prev_float_ieee prec f =
      let f = if equal f pos_zero then Cst.neg_zero prec else f in
      F.prev_float prec f
  end


  module FRange = struct

    (* invariants for intervals Itv (b, e, nan):
       - b and e are not NaN;
       - b <= e *)
    type t =
      | Itv of F.t * F.t * bool
      | NaN

    let nan = NaN

    let inject ?(nan=false) b e =
      if F.is_nan b || F.is_nan e || F.compare b e > 0 then
        Kernel.fatal "Invalid bounds for float interval:@ %a .. %a@."
          F.pretty b F.pretty e;
      Itv (b, e, nan)

    let inject_after_tighten prec ~nan b e =
      let b = F.round_to_precision Up prec b
      and e = F.round_to_precision Down prec e in
      if Cmp.le b e then `Value (inject ~nan b e)
      else if nan then `Value NaN else `Bottom

    let add_nan = function
      | NaN as f -> f
      | Itv (_, _, true) as f -> f
      | Itv (b, e, false) -> Itv (b, e, true)

    let add_pos_infinity prec t =
      let pos_inf = Cst.pos_infinity prec in
      match t with
      | NaN -> Itv (pos_inf, pos_inf, true)
      | Itv (b, _e, nan) -> Itv (b, pos_inf, nan)

    let add_neg_infinity prec t =
      let neg_inf = Cst.neg_infinity prec in
      match t with
      | NaN -> Itv (neg_inf, neg_inf, true)
      | Itv (_b, e, nan) -> Itv (neg_inf, e, nan)
  end

  (* ------------------------------------------------------------------------
                                      Datatype
     ------------------------------------------------------------------------ *)

  type t = FRange.t

  let structural_descr =
    Structural_descr.t_sum [|
      [| F.packed_descr; F.packed_descr; Structural_descr.p_bool |];
      [| |]
    |]
  let packed_descr = Structural_descr.pack structural_descr

  let compare x y =
    match x, y with
    | FRange.Itv (b1, e1, n1), FRange.Itv (b2, e2, n2) ->
      let c = Transitioning.Stdlib.compare n1 n2 in
      if c <> 0 then c else
        let r = F.compare b1 b2 in
        if r <> 0 then r else F.compare e1 e2
    | FRange.Itv _, FRange.NaN -> -1
    | FRange.NaN, FRange.Itv _ -> 1
    | FRange.NaN, FRange.NaN -> 0

  let equal x y =
    match x,y with
    | FRange.Itv (b1, e1, n1), FRange.Itv (b2, e2, n2) ->
      Cmp.equal b1 b2 && Cmp.equal e1 e2 && n1 = n2
    | FRange.NaN, FRange.NaN -> true
    | _ -> false

  let pretty_or_zero fmt f =
    if Cmp.(equal f pos_zero) then Format.fprintf fmt "0" else F.pretty fmt f

  let pretty fmt = function
    | FRange.Itv (b, e, nan) ->
      if Cmp.equal b e then
        Format.fprintf fmt "{%a%t}" pretty_or_zero b
          (fun fmt -> if nan then Format.pp_print_string fmt ";NaN")
      else
        let print_nan fmt nan =
          if nan then Format.fprintf fmt " %s {NaN}" (Unicode.union_string ())
        in
        Format.fprintf fmt "[%a .. %a]%a" F.pretty b F.pretty e print_nan nan
    | FRange.NaN -> Format.fprintf fmt "NaN"

  let hash = function
    | FRange.Itv (b, e, n) ->
      (2 * F.hash b) + (5 * F.hash e) + (7 * Hashtbl.hash n)
    | FRange.NaN -> 3

  (* ------------------------------------------------------------------------
                         Constants, injections, projections
     ------------------------------------------------------------------------ *)

  let min_and_max = function
    | FRange.Itv (b, e, nan) -> Some (b, e), nan
    | FRange.NaN -> None, true

  let nan = FRange.nan

  let inject = FRange.inject

  let singleton x = FRange.inject x x

  let pos_zero prec = singleton (Cst.pos_zero prec)
  let one prec = singleton (F.of_float Near prec 1.)
  let pos_infinity prec = singleton (Cst.pos_infinity prec)
  let neg_infinity prec = singleton (Cst.neg_infinity prec)

  let minus_one_one prec ~nan =
    FRange.inject ~nan (F.of_float Near prec (-1.)) (F.of_float Near prec 1.)

  let m_pi = 3.1415929794311523 (* single-precision *)
  let m_minus_pi = -. m_pi

  let minus_pi_pi prec ~nan =
    FRange.inject ~nan (F.of_float Near prec m_minus_pi) (F.of_float Near prec m_pi)

  let top_f prec ~nan =
    FRange.inject ~nan (Cst.neg_infinity prec) (Cst.pos_infinity prec)

  let top prec = top_f prec ~nan:true

  let top_finite prec =
    if F.is_exact prec
    then
      let b = F.next_float prec (Cst.neg_infinity prec) in
      let e = F.prev_float prec (Cst.pos_infinity prec) in
      FRange.inject ~nan:false b e
    else top_f prec ~nan:false

  (* Splits the interval [b; e] into two intervals (neg, pos) such that [neg]
     (resp. [pos]) contains only negative (resp. positive) floats. *)
  let split_by_sign prec ((b, e) as x) =
    if Cmp.(le e neg_zero) then `Value x, `Bottom
    else if Cmp.(ge b pos_zero) then `Bottom, `Value x
    else `Value (b, Cst.neg_zero prec), `Value (Cst.pos_zero prec, e)

  (* Returns true iff [f] represents an odd integer. *)
  let is_odd prec f =
    let two = F.of_float Near prec 2. in
    Cmp.equal (F.abs (F.fmod Near prec f two)) Cmp.one

  (* [split_by_parity (b, e)] returns [even_bounds, odd_bounds], where:
     - [even_bounds] are the min and max even integers enclosed between b and e
       (or None if there is no such even integer).
     - [odd_bounds] are the min and max odd integers (representable as floating-
       point values) enclosed between b and e (or None if there is no such odd
       integer). These bounds cannot be infinite.  *)
  let split_by_parity prec (b, e) =
    let b = F.ceil b
    and e = F.floor e in
    if Cmp.gt_ieee b e then None, None
    else if Cmp.equal_ieee b e
    then
      let x = (b, e) in
      if is_odd prec b then None, Some x else Some x, None
    else
      let one = F.of_float Near prec 1. in
      let min_even, min_odd =
        if is_odd prec b
        (* No rounding errors may happen below because odd numbers are bounded. *)
        then F.add Near prec b one, b
        else b, Cmp.max (F.neg (Cst.max_odd_integer prec)) (F.add Near prec b one)
      and max_even, max_odd =
        if is_odd prec e
        (* No rounding errors may happen below because odd numbers are bounded. *)
        then F.sub Near prec e one, e
        else e, Cmp.min (Cst.max_odd_integer prec) (F.sub Near prec e one)
      in
      let even = Some (min_even, max_even)
      and odd =
        if Cmp.lt_ieee max_odd min_odd
        then None
        else Some (min_odd, max_odd)
      in
      even, odd

  (* -----------------------------------------------------------------------
                                      Lattice
     ----------------------------------------------------------------------- *)

  let is_included x1 x2 =
    match x1, x2 with
    | FRange.Itv (b1, e1, n1), FRange.Itv (b2, e2, n2) ->
      F.compare b2 b1 <= 0 && F.compare e1 e2 <= 0 && (not n1 || n2)
    | FRange.NaN, FRange.Itv (_, _, true) -> true
    | FRange.NaN, FRange.NaN -> true
    | _ -> false

  let join f1 f2 =
    match f1, f2 with
    | FRange.Itv (b1, e1, n1), FRange.Itv (b2, e2, n2) ->
      FRange.inject ~nan:(n1 || n2) (Cmp.min b1 b2) (Cmp.max e1 e2)
    | (FRange.Itv (b1, e1, _), FRange.NaN)
    | (FRange.NaN, FRange.Itv (b1, e1, _)) -> FRange.inject ~nan:true b1 e1
    | FRange.NaN, FRange.NaN -> FRange.nan

  let widen wh prec f1 f2 =
    assert (is_included f1 f2);
    match f1, f2 with
    | FRange.Itv (b1, e1, _), FRange.Itv (b2, e2, nan) ->
      let b = if Cmp.equal b2 b1 then b2 else F.widen_down wh prec b2 in
      let e = if Cmp.equal e2 e1 then e2 else F.widen_up wh prec e2 in
      (** widen_up and down produce double only if the input is a double *)
      FRange.inject ~nan b e
    | FRange.NaN, f2 -> f2
    | FRange.Itv _, FRange.NaN -> assert false

  let meet f1 f2 =
    match f1, f2 with
    | FRange.Itv (b1, e1, n1), FRange.Itv (b2, e2, n2) ->
      let is_finite = F.compare b2 e1 <= 0 && F.compare b1 e2 <= 0 in
      let is_nan = n1 && n2 in
      if is_finite || is_nan
      then
        let v =
          if is_finite then
            FRange.inject ~nan:is_nan (Cmp.max b1 b2) (Cmp.min e1 e2)
          else FRange.nan
        in
        `Value v
      else `Bottom
    | (FRange.Itv (_, _, true) | FRange.NaN),
      (FRange.Itv (_, _, true) | FRange.NaN) -> `Value FRange.nan
    | _ -> `Bottom

  let narrow = meet

  (* ------------------------------------------------------------------------
                                   Tests
     ------------------------------------------------------------------------ *)

  let contains cst = function
    | FRange.NaN -> false
    | FRange.Itv (b, e, _) -> Cmp.le b cst && Cmp.ge e cst

  let contains_pos_zero t = contains Cmp.pos_zero t
  let contains_neg_zero t = contains Cmp.neg_zero t

  let contains_a_zero = function
    | FRange.Itv (b, e, _) ->
      Cmp.(le_ieee b pos_zero) && Cmp.(ge_ieee e pos_zero)
    | FRange.NaN -> false

  let contains_non_zero = function
    | FRange.Itv (b, e, nan) ->
      nan || Cmp.(lt_ieee b pos_zero) || Cmp.(gt_ieee e pos_zero)
    | FRange.NaN -> true

  let contains_strictly_pos = function
    | FRange.Itv (_, e, _) -> Cmp.(gt_ieee e pos_zero)
    | FRange.NaN -> false

  let contains_strictly_neg = function
    | FRange.Itv (b, _, _) -> Cmp.(lt_ieee b neg_zero)
    | FRange.NaN -> false

  let contains_strict_neg_finite (b, e) =
    Cmp.(gt_ieee e neg_infinity) && Cmp.(lt b neg_zero)

  let contains_finite_noninteger prec (b, e) =
    let ib = F.ceil b in
    not ((Cmp.equal_ieee ib b && Cmp.equal_ieee b e)
         || Cmp.le_ieee e (F.neg (Cst.ceil_of_max_non_integer prec))
         || Cmp.ge_ieee b (Cst.ceil_of_max_non_integer prec))

  let contains_pos_infinity = function
    | FRange.Itv (_, e, _) -> Cmp.is_pos_infinity e
    | FRange.NaN  -> false

  let contains_neg_infinity = function
    | FRange.Itv (b, _, _) -> Cmp.is_neg_infinity b
    | FRange.NaN  -> false

  let contains_infinity f =
    contains_pos_infinity f || contains_neg_infinity f

  let contains_nan = function
    | FRange.NaN -> true
    | FRange.Itv (_, _, nan) -> nan

  let is_singleton = function
    | FRange.NaN -> true
    | FRange.Itv (b, e, nan) -> Cmp.equal b e && not nan

  let is_one = function
    | FRange.NaN -> false
    | FRange.Itv (b, e, nan) ->
      Cmp.(equal b one) && Cmp.(equal e one) && not nan

  let is_a_zero = function
    | FRange.NaN -> false
    | FRange.Itv (b, e, nan) ->
      not nan && Cmp.is_a_zero b && Cmp.is_a_zero e

  let if_not_nan = function
    | FRange.NaN -> assert false
    | FRange.Itv (b, e, _) -> b, e

  let is_not_nan = function
    | FRange.NaN -> Comp.False
    | FRange.Itv (_b, _e, nan) -> if nan then Comp.Unknown else Comp.True

  let is_finite = function
    | FRange.NaN -> Comp.False
    | FRange.Itv (b, e, nan) ->
      if Cmp.is_neg_infinity e || Cmp.is_pos_infinity b
      then Comp.False
      else if nan || Cmp.is_neg_infinity b || Cmp.is_pos_infinity e
      then Comp.Unknown
      else Comp.True

  let is_negative = function
    | FRange.Itv (b, e, false) ->
      if F.is_negative e then Comp.True
      else if not (F.is_negative b) then Comp.False
      else Comp.Unknown
    | FRange.Itv (_, _, true)
    | FRange.NaN -> Comp.Unknown

  let backward_is_nan ~positive = function
    | FRange.NaN as v -> if positive then `Value v else `Bottom
    | FRange.Itv (_, _, false) as v -> if positive then `Bottom else `Value v
    | FRange.Itv (b, e, true) ->
      if positive then `Value nan else `Value (FRange.inject ~nan:false b e)

  let backward_is_finite ~positive prec = function
    | FRange.NaN as v -> if positive then `Bottom else `Value v
    | FRange.Itv (b, e, nan) as f ->
      if positive
      then
        if Cmp.equal b e && F.is_infinite b
        then `Bottom (* [f] is exactly an infinite, we can return `Bottom
                        even in the [Real] case. *)
        else narrow (top_finite prec) f
      else
        match F.is_infinite b, F.is_infinite e with
        | true, true -> `Value f (* No possible reduction. *)
        | true, false -> `Value (FRange.inject ~nan b b)
        | false, true -> `Value (FRange.inject ~nan e e)
        | false, false -> if nan then `Value FRange.nan else `Bottom

  let has_greater_min_bound t1 t2 =
    match t1, t2 with
    | FRange.Itv (m1, _, _), FRange.Itv (m2, _, _) -> F.compare m1 m2
    | FRange.NaN, FRange.Itv _ -> 1
    | FRange.Itv _, FRange.NaN -> -1
    | FRange.NaN, FRange.NaN -> 0

  let has_smaller_max_bound t1 t2 =
    match t1, t2 with
    | FRange.Itv (_, m1, _), FRange.Itv (_, m2, _) -> F.compare m2 m1
    | FRange.NaN, FRange.Itv _ -> 1
    | FRange.Itv _, FRange.NaN -> -1
    | FRange.NaN, FRange.NaN -> 0

  (* ------------------------------------------------------------------------
                                Comparisons
     ------------------------------------------------------------------------ *)

  let forward_eq (b1,e1) (b2,e2) =
    let not_intersects = Cmp.lt_ieee e2 b1 || Cmp.lt_ieee e1 b2 in
    if not_intersects
    then Comp.False
    else if Cmp.equal_ieee b1 e1 && Cmp.equal_ieee b2 e2
    then Comp.True
    else Comp.Unknown

  let forward_le (b1, e1) (b2, e2) =
    if Cmp.le_ieee e1 b2 then Comp.True
    else if Cmp.lt_ieee e2 b1 then Comp.False
    else Comp.Unknown

  let forward_lt (b1, e1) (b2, e2) =
    if Cmp.lt_ieee e1 b2 then Comp.True
    else if Cmp.le_ieee e2 b1 then Comp.False
    else Comp.Unknown

  let forward_comp op f1 f2 = match f1, f2 with
    | FRange.NaN, _ | _, FRange.NaN ->
      if op = Comp.Ne then Comp.True else Comp.False
    | FRange.Itv (b1, e1, nan1), FRange.Itv (b2, e2, nan2) ->
      let r = match op with
        | Comp.Le -> forward_le (b1, e1) (b2, e2)
        | Comp.Ge -> forward_le (b2, e2) (b1, e1)
        | Comp.Lt -> forward_lt (b1, e1) (b2, e2)
        | Comp.Gt -> forward_lt (b2, e2) (b1, e1)
        | Comp.Eq -> forward_eq (b1, e1) (b2, e2)
        | Comp.Ne -> Abstract_interp.inv_truth (forward_eq (b1, e1) (b2, e2))
      in
      if nan1 || nan2
      then
        if op = Comp.Ne
        then (match r with Comp.True -> Comp.True | _ -> Comp.Unknown)
        else (match r with Comp.False -> Comp.False | _ -> Comp.Unknown)
      else r

  (* This function intentionally returns different results with
     [e2 = -0.] and [e2 = 0.] *)
  let backward_le_aux prec (b1, e1) e2 =
    if not (Cmp.le b1 e2)
    then `Bottom
    else if Cmp.le e1 e2
    then `Value (FRange.inject b1 e1)
    else FRange.inject_after_tighten prec ~nan:false b1 e2

  (* This is the "real" backward transformer for [le], which does not distinguish
     [0.] and [-0.]. Thus we enlarge the bound in the "worst" direction. *)
  let backward_le prec (b1, e1) e2 =
    let e2 = if Cmp.is_a_zero e2 then Cst.pos_zero prec else e2 in
    backward_le_aux prec (b1, e1) e2

  let backward_lt prec ((b1, e1) as f1) e2 =
    if Cmp.le_ieee e2 b1
    then `Bottom
    else if F.is_exact prec || Cmp.equal b1 e1
    then backward_le prec f1 (Cmp.prev_float_ieee prec e2)
    else
      (* On real we cannot be more precise than [le], except on zeros: at
         least get rid of the "bad" zero *)
      let e2 = if Cmp.is_a_zero e2 then Cst.neg_zero prec else e2 in
      backward_le_aux prec f1 e2

  (* see comments in {!backward_le_aux} *)
  let backward_ge_aux prec (b1, e1) b2 =
    if not (Cmp.le b2 e1)
    then `Bottom
    else if Cmp.le b2 b1
    then `Value (FRange.inject b1 e1)
    else FRange.inject_after_tighten prec ~nan:false b2 e1

  (* see comments in {!backward_le} *)
  let backward_ge prec (b1, e1) b2 =
    let b2 = if Cmp.is_a_zero b2 then Cst.neg_zero prec else b2 in
    backward_ge_aux prec (b1, e1) b2

  (* see comments in {!backward_gt} *)
  let backward_gt prec ((b1, e1) as f1) b2 =
    if Cmp.le_ieee e1 b2
    then `Bottom
    else if F.is_exact prec || Cmp.equal b1 e1
    then backward_ge prec f1 (Cmp.next_float_ieee prec b2)
    else
      let b2 = if Cmp.is_a_zero b2 then Cst.pos_zero prec else b2 in
      backward_ge_aux prec f1 b2

  (** The operands cannot be {!Nan} *)
  let backward_comp_left_true_finite op prec f1' f2' =
    let f1 = if_not_nan f1' in
    let (b2, e2) = if_not_nan f2' in
    match op with
    | Comp.Le -> backward_le prec f1 e2
    | Comp.Ge -> backward_ge prec f1 b2
    | Comp.Lt -> backward_lt prec f1 e2
    | Comp.Gt -> backward_gt prec f1 b2
    | Comp.Eq ->
      (* -0 and +0 must not be distinguished here *)
      let f2 =
        if contains_a_zero f2'
        then join f2' (FRange.inject (Cst.neg_zero prec) (Cst.pos_zero prec))
        else f2'
      in
      narrow f1' f2
    | Comp.Ne ->
      (* compute (f1 ∩ [-infty,min[ ) ∪ (f1 ∩ ]max,infty]) *)
      let before_or_after min max =
        Bottom.join join
          (backward_lt prec f1 min) (backward_gt prec f1 max)
      in
      (* As usual, we cannot reduce if [f2] is not a singleton, except that
         the two zeros are a kind of singleton. Checking whether [f2] is on
         a frontier of [f1] is not obvious because of the multiple cases
         (and [allmodes]) so we use the transformers for [lt] instead. *)
      if is_a_zero f2' then before_or_after (Cst.neg_zero prec) (Cst.pos_zero prec)
      else if is_singleton f2' then before_or_after b2 b2
      else `Value f1'

  (* Applies [backward f1 f2] and removes NaN from [f1] and [f2]. *)
  let backward_comp_no_nan backward_finite f1 f2 =
    match f1, f2 with
    | FRange.NaN, _ | _, FRange.NaN -> `Bottom
    | FRange.Itv (b, e, nan), FRange.Itv _ ->
      let f1 = if nan then FRange.inject ~nan:false b e else f1 in
      backward_finite f1 f2

  (* Applies [backward f1 f2] but preserves NaN from [f1] and [f2]. *)
  let backward_comp_with_nan backward_finite f1 f2 =
    if contains_nan f2
    then `Value f1
    else
      match f1 with
      | FRange.NaN -> `Value f1
      | FRange.Itv (_, _, nan) ->
        let nan = if nan then `Value FRange.nan else `Bottom in
        Bottom.join join (backward_finite f1 f2) nan

  let backward_comp_left_true op prec =
    let backward_finite = backward_comp_left_true_finite op prec in
    if op = Comp.Ne
    then backward_comp_with_nan backward_finite
    else backward_comp_no_nan backward_finite

  let backward_comp_left_false op prec =
    let backward_finite = backward_comp_left_true_finite (Comp.inv op) prec in
    if op = Comp.Ne
    then backward_comp_no_nan backward_finite
    else backward_comp_with_nan backward_finite


  (* ------------------------------------------------------------------------
                        Simple arithmetic operations
     ------------------------------------------------------------------------ *)

  (* The functions defined using [exact_aux] below are, among other
     properties, (1) exact (the result as a real can always be
     represented exactly, in the good type), and (2) total. In
     particular, given a float 'x', 'ff x == (float)(f (double)x)'.
     Thus, in this module, the 'f' functions are also the non-f
     (since float32 are represented using double) *)
  let (>>) t f = match t with
    | FRange.NaN -> t
    | FRange.Itv (b, e, nan) -> FRange.inject ~nan (f b) (f e)

  let floor t = t >> F.floor
  let ceil t = t >> F.ceil
  let trunc t = t >> F.trunc
  let fround t = t >> F.fround

  let neg = function
    | FRange.Itv (b, e, nan) ->
      (* do not round because exact operation *)
      FRange.inject ~nan (F.neg e) (F.neg b)
    | FRange.NaN -> FRange.nan

  let abs prec = function
    | FRange.Itv (b, e, nan) as f ->
      if contains Cmp.pos_zero f then
        let zero = Cst.pos_zero prec in
        FRange.inject ~nan zero (Cmp.max (F.abs b) (F.abs e))
      else (* f is either strictly positive or strictly negative *)
      if F.compare e Cmp.pos_zero < 0 then neg f else f
    | FRange.NaN as f -> f

  (* This monad returns a NaN if one operand can only be NaN, and lets the
     second function perform the computation if both operands contain a
     non-empty floating-point interval. *)
  let ( >>% ) = fun (x,y) f -> match x, y with
    | FRange.NaN, _ | _, FRange.NaN -> FRange.nan
    | FRange.Itv (b1, e1, nan1), FRange.Itv (b2, e2, nan2) ->
      let nan = nan1 || nan2 in
      f ~nan (b1, e1) (b2, e2)

  (* Auxiliary function used for the forward semantics of add, mul and div.
     For a monotonic function [op], the bounds of [[b1..e1] op [b2..e2]] are the
     minimum and maximum of [b1 op b2], [b1 op e2], [e1 op b2] and [e1 op e2].
     NaN can be created from \infty - \infty, 0 * \infty, 0/0 and \infty /
     \infty, in which case the result contains NaN, and new operations are
     performed to take into account the results of values near \infty and 0.
     Beware that NaN and discontinuities occuring between the bounds of the
     arguments (i.e. on zeros, as an infinity is always a bound) should be
     checked and processed by the caller. *)
  let monotonic op prec x y =
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
    let treat_nan_result rnd a b =
      nan := true;
      if F.is_infinite a && not (Cmp.equal b1 e1)
      then results := op rnd prec (Cst.sign prec a) b :: !results;
      if F.is_infinite b && not (Cmp.equal b2 e2)
      then results := op rnd prec a (Cst.sign prec b) :: !results;
    in
    let op rnd x y =
      let r = op rnd prec x y in
      if F.is_nan r
      then treat_nan_result rnd x y
      else results := r :: !results
    in
    let compute rnd =
      results := [];
      op rnd e1 e2; op rnd e1 b2; op rnd b1 e2; op rnd b1 b2;
    in
    let rounding_mode = if F.is_exact prec then Near else Down in
    compute rounding_mode;
    let pos_inf = Cst.pos_infinity prec in
    let min = List.fold_left Cmp.min pos_inf !results in
    if not (F.is_exact prec)
    then compute Up;
    let neg_inf = Cst.neg_infinity prec in
    let max = List.fold_left Cmp.max neg_inf !results in
    if min > max
    then (assert !nan; FRange.nan)
    else FRange.inject ~nan:!nan min max

  let add prec = monotonic F.add prec
  let sub prec = monotonic F.sub prec

  let mul prec x y =
    let r = monotonic F.mul prec x y in
    (* A NaN may occur between the bounds of the intervals, on 0 * \infty. *)
    if (contains_infinity x && contains_a_zero y) ||
       (contains_infinity y && contains_a_zero x)
    then FRange.add_nan r
    else r

  let div prec x y =
    let r = monotonic F.div prec x y in
    (* A NaN may occur between the bounds of the intervals, on 0/0. *)
    let nan = (contains_a_zero x && contains_a_zero y) in
    (* Treat the discontinuity around 0: divisions by 0 produce infinites. *)
    let pos_inf =
      contains Cmp.pos_zero y && contains_strictly_pos x ||
      contains Cmp.neg_zero y && contains_strictly_neg x
    and neg_inf =
      contains Cmp.pos_zero y && contains_strictly_neg x ||
      contains Cmp.neg_zero y && contains_strictly_pos x
    in
    let r = if pos_inf then FRange.add_pos_infinity prec r else r in
    let r = if neg_inf then FRange.add_neg_infinity prec r else r in
    if nan then FRange.add_nan r else r

  (* Could be improved a lot, cf [Marre10]. *)
  let backward_add_one prec ~other ~result =
    (* No reduction when the result contains an infinity, and when the result and
       the other operand contain NaN (as x + NaN = NaN for any x). *)
    if contains_infinity result || (contains_nan other && contains_nan result)
    then `Value (top prec)
    else
      (* Values that can lead to NaN in the result. *)
      let reduce_for_nan t =
        let t =
          if contains_pos_infinity other
          then FRange.add_neg_infinity prec t
          else t
        in
        if contains_neg_infinity other
        then FRange.add_pos_infinity prec t
        else t
      in
      let reduced_for_nan =
        if contains_nan result then `Value (reduce_for_nan FRange.nan) else `Bottom
      in
      (* Values that can lead to finite values in the result. *)
      let reduced_for_finite_values =
        match result, other with
        | FRange.NaN, _ | _, FRange.NaN  -> `Bottom
        | FRange.Itv (bres, eres, _), FRange.Itv (bother, eother, _) ->
          let bres = Cmp.prev_float_ieee prec bres in
          let eres = Cmp.next_float_ieee prec eres in
          let round = if F.is_exact prec then Up else Near in
          let b = F.sub round prec bres eother in
          let round = if F.is_exact prec then Down else Near in
          let e = F.sub round prec eres bother in
          if Cmp.le b e then `Value (FRange.inject ~nan:false b e) else `Bottom
      in
      Bottom.join join reduced_for_finite_values reduced_for_nan

  let backward_add fkind ~left ~right ~result =
    backward_add_one fkind ~other:right ~result >>- fun left' ->
    backward_add_one fkind ~other:left ~result >>- fun right' ->
    `Value (left', right')

  let backward_sub fk ~left ~right ~result =
    let right = neg right in
    backward_add fk ~left ~right ~result
    >>-: fun (left, right) -> (left, neg right)

  (* ------------------------------------------------------------------------
                             Exp Log Sqrt Pow Fmod
     ------------------------------------------------------------------------ *)

  let (>>:) t f = match t with
    | FRange.NaN -> t
    | FRange.Itv (b, e, nan) -> f ~nan b e

  let approx f prec ~nan b e =
    let round = if F.is_exact prec then Near else Down in
    let min = f round prec b in
    let round = if F.is_exact prec then Near else Up in
    let max = f round prec e in
    FRange.inject ~nan min max

  let exp prec t = t >>: approx F.exp prec

  let log_aux log prec t =
    t >>: fun ~nan b e ->
    if Cmp.(lt e neg_zero)
    then FRange.nan
    else
      let nan = nan || Cmp.(lt b neg_zero) in
      let b = Cmp.max (Cst.neg_zero prec) b in
      approx log prec ~nan b e

  let log prec = log_aux F.log prec
  let log10 prec = log_aux F.log10 prec

  (* [sqrt_f] is the actual function computing the (exact) square root,
     in single precision (sqrtf) or double precision (sqrt). *)
  let sqrt prec t =
    t >>: fun ~nan b e ->
    if Cmp.(lt_ieee e neg_zero)
    then FRange.nan
    else
      let nan, b =
        if Cmp.(ge_ieee b neg_zero)
        then nan, b
        else true, Cst.neg_zero prec
      in
      approx F.sqrt prec ~nan b e


  let value_if prec condition f = if condition then `Value (f prec) else `Bottom

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
    Cmp.min a (Cmp.min b (Cmp.min c d)), Cmp.max a (Cmp.max b (Cmp.max c d))

  (* Computes [pow_f] on a negative [bx; ex] interval (including infinites).
     Processes by disjunction over even and odd integers enclosed within [by; ey].
     [pow] is then monotonic on even integers (including zeros and infinities),
     and on odd integers (except on infinities). *)
  let pow_negative_x pow_f prec (bx, ex as x) (by, ey as y) =
    let even, odd = split_by_parity prec y in
    (* Even integers [y] lead to positive results, while odd ones lead to negative
       results.  When [y] contains both even and odd integers, the minimum result
       is in odd integers, and the maximum in even integers. *)
    let min, max = match even, odd with
      | None, None -> Cst.pos_infinity prec, Cst.neg_infinity prec
      | Some even, None -> extrema pow_f x even
      | None, Some odd  -> extrema pow_f x odd
      | Some even, Some odd ->
        extremum Cmp.min pow_f x odd, extremum Cmp.max pow_f x even
    in
    let nonint_y = contains_finite_noninteger prec y in
    (* pow creates NaN when [x] is a negative non-zero finite value, and [y] a
       non integer value. *)
    let nan = contains_strict_neg_finite x && nonint_y in
    (* Special cases of neg_infinity and neg_zero for [x], that do not produce
       a NaN on non integer [y], unlike strictly negative finite values [x]. *)
    let neg_nonint_y = nonint_y && Cmp.(lt by neg_zero) in
    let pos_nonint_y = nonint_y && Cmp.(gt ey pos_zero) in
    let neg_infinity_x = Cmp.is_neg_infinity bx in
    let zero_x = Cmp.(equal ex neg_zero) in
    Bottom.join_list join
      [ if Cmp.le min max then `Value (FRange.inject min max) else `Bottom;
        value_if prec nan (fun _ -> FRange.nan);
        value_if prec (neg_infinity_x && neg_nonint_y) pos_zero;
        value_if prec (neg_infinity_x && pos_nonint_y) pos_infinity;
        value_if prec (zero_x && neg_nonint_y) pos_infinity;
        value_if prec (zero_x && pos_nonint_y) pos_zero ]

  (* Computes pow on a positive [bx; ex] interval (including infinites): the
     function is continuous and monotonic. *)
  let pow_positive_x pow_f _prec x y =
    let min, max = extrema pow_f x y in
    FRange.inject min max

  let pow prec x y =
    if is_one x || is_a_zero y then one prec
    else
      (x, y) >>% fun ~nan itv_x itv_y ->
      let pow_f = F.pow Near prec in
      (* Split the x interval around zeros, as pow is discontinuous on zeros. *)
      let neg_x, pos_x = split_by_sign prec itv_x in
      let pos_x_res = pos_x >>-: fun x -> pow_positive_x pow_f prec x itv_y in
      let neg_x_res = neg_x >>- fun x -> pow_negative_x pow_f prec x itv_y in
      let nan_res = if nan then `Value FRange.nan else `Bottom in
      Bottom.non_bottom
        (Bottom.join_list join [ pos_x_res; neg_x_res; nan_res ])

  (* Is [fmod] continuous on positive intervals [b1..e1] and [b2..e2]?
     This is the case if for all x, y in these intervals, the rounded quotient
     [floor(x/y)] is constant, as [fmod x y = x - floor(x/y) * y] when [x] and [y]
     are positive.
     Also checks that [x/y < 2^53], otherwise truncation to an integer may return
     an incorrect result. Note: to avoid issues with rounding, we conservatively
     set the limit to 2^51 instead of 2^53 (and to 2^21 instead of 2^23 in single
     precision). *)
  let is_fmod_continuous prec (b1, e1) (b2, e2) =
    (* Discontinuity of [fmod x y] on infinite [x] and on zero [y]. *)
    F.is_finite e1 && Cmp.(gt_ieee b2 pos_zero) &&
    let four = F.of_float Near prec 4. in
    let max_i = F.div Near prec (Cst.max_precise_integer prec) four in
    let f1 = F.floor (F.div Zero prec b1 e2) in
    let f2 = F.floor (F.div Zero prec e1 b2) in
    Cmp.equal f1 f2 && Cmp.le f1 max_i && F.is_exact prec

  (* Forward semantics of fmod on positive intervals. [x] must contain finite
     values, and [y] must contain non-zero values, in which case finite values
     are produced. This function does not check the creation of NaN. *)
  let positive_fmod prec (b1, e1 as x) (b2, e2 as y) =
    let fmod = F.fmod Near prec in
    (* Singleton case. [x] cannot be infinite, and [y] cannot be zero. *)
    if Cmp.equal b1 e1 && Cmp.equal b2 e2
    then let c = fmod b1 b2 in c, c
    (* If all values of x are smaller than all values of [y], [x] is unchanged. *)
    else if Cmp.lt_ieee e1 b2
    then x
    (* If fmod is continuous on the intervals [x] and [y], it is also monotonic,
       and the bounds of the result are the remainders of the bounds. *)
    else if is_fmod_continuous prec x y
    then fmod b1 e2, fmod e1 b2
    (* Otherwise, fmod always satisfies 0 <= [fmod x y] <= x and [fmod x y] < y. *)
    else let max = Cmp.min e1 (F.prev_float prec e2) in (Cst.pos_zero prec, max)

  let fmod prec x y =
    (* [fmod x (-y)] = [fmod x y], so use only positive [y]. *)
    let y = abs prec y in
    (x, y) >>% fun ~nan (b1, e1) (b2, e2) ->
    (* If [x] is an infinite singleton, or if [y] contains only zero, only NaN
       can be created. *)
    if (Cmp.equal b1 e1 && F.is_infinite b1)
    || (Cmp.equal b2 e2 && Cmp.is_a_zero b2)
    then FRange.nan
    else
      let nan = nan || contains_infinity x || contains_a_zero y in
      let positive_fmod x =
        let b, e = positive_fmod prec x (b2, e2) in
        FRange.inject ~nan b e
      in
      (* Process by disjunction on the sign of [x], and join the results for
         negative x and for positive x. *)
      let neg_x, pos_x = split_by_sign prec (b1, e1) in
      let neg_itv (b, e) = F.neg e, F.neg b in
      let neg_r = neg_x >>-: neg_itv >>-: positive_fmod >>-: neg in
      let pos_r = pos_x >>-: positive_fmod in
      Bottom.non_bottom (Bottom.join join neg_r pos_r)

  (* ------------------------------------------------------------------------
                                Trigonometry
     ------------------------------------------------------------------------ *)

  (* It is wrong to use m_pi as the local minimum as was previously done,
     because:
     A = 3.14159265358979312 and A < m_pi, cos A = 1. and cos m_pi = 0.999
     Moreover it is not due to the imprecision in the value of pi:
     A < pred m_pi in simple.

     So we use a quarter of the interval [-pi:pi] to be safe. (But still,
     nothing proves that cos or sin are monotonic on those ranges.) *)
  let m_pi = 3.1415929794311523 (* single-precision *)
  let cos_sin_security = F.of_float Near Float_sig.Single (m_pi /. 4.)
  let cos_sin op prec t =
    t >>: fun ~nan b e ->
    (* Special case when at least a bound is infinite. *)
    if F.is_infinite b || F.is_infinite e
    then if Cmp.equal b e then FRange.nan else minus_one_one prec ~nan:true
    (* [b] and [e] are finite. Precise case for a singleton. *)
    else if Cmp.equal b e
    then let c = op Near prec b in FRange.inject ~nan c c
    else if Cmp.le_ieee b (F.neg cos_sin_security)
         || Cmp.ge_ieee e cos_sin_security
    then minus_one_one prec ~nan
    else
      let xl =
        if Cmp.(lt b pos_zero) && Cmp.(lt pos_zero e)
        then [b; e; Cst.pos_zero prec]
        else [b; e]
      in
      let l = List.map (op Near prec) xl in
      let min_f = List.fold_left Cmp.min (Cst.pos_infinity prec) l in
      let max_f = List.fold_left Cmp.max (Cst.neg_infinity prec) l in
      FRange.inject ~nan min_f max_f

  let cos prec = cos_sin F.cos prec
  let sin prec = cos_sin F.sin prec

  let atan2 prec x y =
    (x, y) >>% fun ~nan (b1, e1) (b2, e2) ->
    let op = F.atan2 Near prec in
    if Cmp.equal b1 e1 && Cmp.equal b2 e2
    then let c = op b1 b2 in FRange.inject ~nan c c
    else
      (* Unless y ([b1,e1]) crosses the x-axis, atan2 is continuous,
         and its minimum/maximum are at the ends of the intervals of x and y.
         Otherwise, the result is [-pi,+pi]. *)
    if Cmp.(lt b1 pos_zero) && Cmp.(gt e1 neg_zero)
    then minus_pi_pi prec ~nan
    else
      let a1, a2, a3, a4 = op b1 b2, op b1 e2, op e1 b2, op e1 e2 in
      let b = Cmp.min a1 (Cmp.min a2 (Cmp.min a3 a4)) in
      let e = Cmp.max a1 (Cmp.max a2 (Cmp.max a3 a4)) in
      match prec with
      | Float_sig.Single ->
        (* Rounding of atan2f in single-precision may go against monotony and
           reach the next (previous) float after (before) the bounds. *)
        FRange.inject ~nan
          (Cmp.prev_float_ieee prec b)
          (Cmp.next_float_ieee prec e)
      | _ -> FRange.inject ~nan b e

  (* ------------------------------------------------------------------------
                                   Casts
     ------------------------------------------------------------------------ *)

  let forward_cast ~dst = function
    | FRange.NaN -> nan
    | FRange.Itv (b, e, nan) ->
      let round = F.round_to_precision Near dst in
      inject ~nan (round b) (round e)

  (* This function must make sure to return a result with float 32 bounds *)
  let backward_cast ~src = function
    | FRange.NaN -> `Value nan
    | FRange.Itv (b, e, nan) -> FRange.inject_after_tighten src ~nan b e

  let cast_int_to_float prec min max =
    let min = match min with
      | None -> Cst.neg_infinity prec
      | Some v ->
        let round = if F.is_exact prec then Near else Down in
        F.of_float round prec (Integer.to_float v)
    in
    let max = match max with
      | None -> Cst.neg_infinity prec
      | Some v ->
        let round = if F.is_exact prec then Near else Up in
        F.of_float round prec (Integer.to_float v)
    in
    FRange.inject min max

  (* Bitwise reinterpretation of a floating-point value into consecutive
     ranges of integer. This operation is exact in terms of
     concretization. 'Parametric' in the number of bits. *)
  let bits_of_float_list ~prec ~bits_of_float ~max_int =
    let neg_infinity = Cst.neg_infinity prec
    and pos_infinity = Cst.pos_infinity prec in
    let itvs_nan =
      let smallest_neg_nan = Integer.succ (bits_of_float neg_infinity) in
      let biggest_neg_nan  = Integer.minus_one in
      let smallest_pos_nan = Integer.succ (bits_of_float pos_infinity) in
      let biggest_pos_nan  = max_int in
      [(smallest_neg_nan, biggest_neg_nan);
       (smallest_pos_nan, biggest_pos_nan)]
    in
    function
    | FRange.NaN -> itvs_nan
    | FRange.Itv (b, e, nan) ->
      let nan = if nan then itvs_nan else [] in
      let neg, pos = split_by_sign prec (b, e) in
      let neg = neg >>-: fun (b, e) -> bits_of_float e, bits_of_float b in
      let pos = pos >>-: fun (b, e) -> bits_of_float b, bits_of_float e in
      Bottom.add_to_list pos (Bottom.add_to_list neg nan)

  let bits_of_float64_list =
    let bits_of_float f =
      Integer.of_int64 (Int64.bits_of_float (F.to_float f))
    in
    let max_int = Integer.of_int64 Int64.max_int in
    bits_of_float_list ~prec:Double ~bits_of_float ~max_int

  let bits_of_float32_list =
    let bits_of_float f =
      Integer.of_int32 (Int32.bits_of_float (F.to_float f))
    in
    let max_int = Integer.of_int32 Int32.max_int in
    bits_of_float_list ~prec:Single ~bits_of_float ~max_int

  (* ------------------------------------------------------------------------
                                   Subdivision
     ------------------------------------------------------------------------ *)

  (* [avg] and [split] implement two different strategies for cutting a
     floating-point interval in half: [avg] computes the mathematical average of
     the two bounds, while [split] balances the number of representable values
     of the given precision in each resulting intervals. *)

  (* Computes the average between two ocaml doubles. *)
  let avg x y =
    let fx = F.to_float x and fy = F.to_float y in
    if F.is_negative x && F.is_negative y
    then fy +. (fx -. fy) /. 2.
    else (fx +. fy) /. 2.

  (* assumption: [0. <= x <= y]. returns the median of the range [x..y]
     in number of double values. *)
  let split_positive prec x y =
    let ix = Int64.bits_of_float (F.to_float x) in
    let iy = Int64.bits_of_float (F.to_float y) in
    let f = Int64.(float_of_bits (add ix (div (sub iy ix) 2L))) in
    F.of_float Near prec f

  (* assumption: [x <= y] *)
  let _split prec x y =
    match F.is_negative x, F.is_negative y with
    | false, false -> split_positive prec x y
    | true, true -> F.neg (split_positive prec (F.neg x) (F.neg y))
    | true, false -> Cst.neg_zero prec
    | false, true -> assert false

  exception Can_not_subdiv = Abstract_interp.Can_not_subdiv

  let subdivide prec t =
    assert (prec = Single || prec = Double); (* See Value/Value#105 *)
    match t with
    | FRange.NaN -> raise Can_not_subdiv
    | FRange.Itv (b, e, nan) ->
      if nan
      then FRange.inject ~nan:false b e, FRange.nan
      else if Cmp.equal b e
      then raise Can_not_subdiv
      else
        let midpoint, smidpoint =
          (* Infinities are interesting points to consider specially. *)
          if Cmp.is_neg_infinity b
          then Cst.neg_infinity prec, F.next_float prec (Cst.neg_infinity prec)
          else if Cmp.is_pos_infinity e
          then F.prev_float prec (Cst.pos_infinity prec), Cst.pos_infinity prec
          else if Cmp.equal (F.next_float prec b) e
          then b, e
          else
            let midpoint = avg b e in
            let midpoint = F.of_float Down prec midpoint in
            let smidpoint =
              if F.is_exact prec then F.next_float prec midpoint else midpoint
            in
            if Cmp.le smidpoint e
            then midpoint, smidpoint
            else midpoint, e
        in
        if Cmp.le smidpoint b || Cmp.le e midpoint then raise Can_not_subdiv;
        let i1 = FRange.inject ~nan b midpoint in
        assert (is_included i1 t);
        let i2 = FRange.inject ~nan smidpoint e in
        assert (is_included i2 t);
        i1, i2

end
