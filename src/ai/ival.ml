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

open Abstract_interp

(* Make sure all this is synchronized with the default value of -ilevel *)
let small_cardinal = ref 8
let small_cardinal_Int = ref (Int.of_int !small_cardinal)
let small_cardinal_log = ref 3

let debug_cardinal = false

let set_small_cardinal i =
  assert (2 <= i && i <= 1024);
  let rec log j p =
    if i <= p then j
    else log (j+1) (2*p)
  in
  small_cardinal := i;
  small_cardinal_Int := Int.of_int i;
  small_cardinal_log := log 1 2

let get_small_cardinal () = !small_cardinal

exception Can_not_subdiv
let can_not_subdiv = Can_not_subdiv

module F = struct

  type t = float

  let packed_descr = Structural_descr.p_float

  external compare : float -> float -> int = "float_compare_total" "noalloc"

(* The Caml version below is fine but the C version is faster and
   does not allocate—it would be possible for the Caml version
   to avoid allocation, but OCaml 4.00.1 allocates 80 bytes,
   for instance *)
(*  let compare f1 f2 =
    let i1 = Int64.bits_of_float f1 in
    let i2 = Int64.bits_of_float f2 in
    let m1 = (Int64.logand i1 Int64.min_int) in
    let m2 = (Int64.logand i2 Int64.min_int) in    
    if m1 = m2 
    then compare f1 f2
    else compare m1 m2 *)

  let equal f1 f2 = compare f1 f2 = 0

  let zero = 0.0
  let minus_zero = -0.0

  exception Nan_or_infinite

  let max_single_precision_float = Floating_point.max_single_precision_float
  let most_negative_single_precision_float = 
    Floating_point.most_negative_single_precision_float
  (* VP: unused function *)
  (* let min_single_precision_float = Int32.float_of_bits 0x800000l *)
  (* let neg_min_single_precision_float = -. min_single_precision_float *)
  let max_float = max_float
  let infinity = infinity
  let neg_infinity = neg_infinity
  let most_negative_float = -. max_float
  let min_denormal = Int64.float_of_bits 1L
  let neg_min_denormal = -. min_denormal

(* works but allocates:
  let is_negative f = Int64.bits_of_float f < Int64.zero *)
  external is_negative : float -> bool = "float_is_negative" "noalloc"

  let zero_of_same_sign f = 
    if is_negative f then minus_zero else zero

  let is_infinity = (=) infinity
  let is_neg_infinity = (=) neg_infinity

  let wrap r =
    match classify_float r with
      FP_nan -> raise Nan_or_infinite
    | FP_normal | FP_subnormal | FP_infinite | FP_zero -> r

  let wrap_un f x = wrap (f x)

  let wrap_bin f x y = wrap (f x y)

  let add = wrap_bin (+.)
  let sub = wrap_bin (-.)
  let neg = wrap_un (~-.)
  let mult = wrap_bin ( *.)
  let div = wrap_bin (/.)

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

  let sqrt = wrap_un sqrt

  let cos = wrap_un cos
  let sin = wrap_un sin
  let exp = wrap_un exp

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

  let round_normal int64fup int64fdown float =
    let r = Int64.bits_of_float float in
    let f =
      if r >= 0L then
        int64fup
      else
        int64fdown
    in
    Int64.float_of_bits (f r)

  let round int64fup int64fdown float =
    match classify_float float with
      FP_nan | FP_infinite -> raise Nan_or_infinite
    | FP_normal | FP_subnormal ->
        let f = round_normal int64fup int64fdown float in
        ( match classify_float f with
          FP_nan | FP_infinite -> raise Nan_or_infinite
        | FP_normal | FP_subnormal | FP_zero -> f )
    | FP_zero ->
        (round_normal int64fup int64fdown (float +. min_float)) -. min_float


  let round_up = round Int64.succ Int64.pred
  let round_down = round Int64.pred Int64.succ

  let le f1 f2 = compare f1 f2 <= 0

  let min f1 f2 =
    if le f1 f2 then f1 else f2

  let max f1 f2 =
    if le f1 f2 then f2 else f1

  let equal_ieee = ((=) : float -> float -> bool)

  let hash = Hashtbl.hash

  let id = fun x -> x
  let of_float = wrap_un id
  let to_float = id

  let classify_float = Pervasives.classify_float
end

module Float_abstract = struct

  exception Bottom

  type denormal_treatment = Denormals | FTZ | DenormalsandFTZ

  let denormal_treatment = Denormals
  let _ = DenormalsandFTZ (* VP: silence warning about unused DenormalsandFTZ *)

  module Private_Couple : sig
    type t = private I of F.t * F.t
    val inject : F.t -> F.t -> t
    val inject_r : F.t -> F.t -> (bool * t)
  end =
  struct

    type t = I of F.t * F.t

    let inject b e =
      assert
        ( if not (F.le b e)
          then begin
              Format.printf "assertion 0936 failed.@\n%a .. %a@."
                (F.pretty_normal ~use_hex:true) b
                (F.pretty_normal ~use_hex:true) e;
              false
            end
          else true);
      I(b, e)

    let inject_r b e =
      if F.is_neg_infinity e || F.is_infinity b
      then raise Bottom;

      let c = F.classify_float e in
      let overflow_alarm, e =
        match c with
          FP_infinite | FP_subnormal ->
            let pos = F.le_ieee F.zero e in
            ( match c, pos with
              FP_infinite, true -> true, F.max_float
            | FP_infinite, false -> raise Bottom
	    | _, true when denormal_treatment = FTZ ->
		false, F.zero
            | _, false when denormal_treatment <> Denormals ->
		false, F.minus_zero 
            | _ -> false, e)

        | _ -> false, e
      in
      let c = F.classify_float b in
      let overflow_alarm, b =
        match c with
          FP_infinite | FP_subnormal ->
            let pos = F.le_ieee F.zero b in
            ( match c, pos with
              FP_infinite, true -> raise Bottom
            | FP_infinite, false -> true, F.most_negative_float
	    | _, false when denormal_treatment = FTZ ->
		overflow_alarm, F.minus_zero
            | _, true when denormal_treatment <> Denormals ->
		overflow_alarm, F.zero 
            | _ -> overflow_alarm, b)
        | _ -> overflow_alarm, b
      in
      overflow_alarm, inject b e

  end

  type t = Private_Couple.t
  (* open Private_Couple *) (* Workaround for Ocaml bug 5718 *)

  let structural_descr =
    Structural_descr.t_sum [| [| F.packed_descr; F.packed_descr |] |]

  let packed_descr = Structural_descr.pack structural_descr

  let inject = Private_Couple.inject

  let inject_r = Private_Couple.inject_r

  let min_and_max_float (Private_Couple.I(b,e)) = b, e

  let top = inject F.most_negative_float F.max_float

  exception Nan_or_infinite = F.Nan_or_infinite

  let compare (Private_Couple.I(b1,e1)) (Private_Couple.I(b2,e2)) =
    let r = F.compare b1 b2 in
    if r <> 0 then r else F.compare e1 e2

  let pretty fmt (Private_Couple.I(b,e)) =
    if F.equal b e then
      Format.fprintf fmt "%a" F.pretty b
    else begin
      if (Kernel.FloatRelative.get())
      then begin
        Floating_point.set_round_upward ();
        let d = F.sub e b in
          Format.fprintf fmt "[%a ++ %a]"
            F.pretty b
            F.pretty d
      end
      else
        Format.fprintf fmt "[%a .. %a]"
          F.pretty b
          F.pretty e
    end

  let hash (Private_Couple.I(b,e)) =
    F.hash b + (5 * F.hash e)

  let inject_singleton x = inject x x

  let zero = inject_singleton F.zero

  let compare_min (Private_Couple.I(m1,_)) (Private_Couple.I(m2,_)) =
    F.compare m1 m2

  let compare_max (Private_Couple.I(_, m1)) (Private_Couple.I(_, m2)) =
    F.compare m2 m1

  let is_included (Private_Couple.I(b1, e1)) (Private_Couple.I(b2, e2)) =
    F.le b2 b1 && F.le e1 e2

  let join (Private_Couple.I(b1, e1)) (Private_Couple.I(b2, e2)) =
    inject (F.min b1 b2) (F.max e1 e2)

      (*@ raises [Bottom] *)
  let meet (Private_Couple.I(b1, e1)) (Private_Couple.I(b2, e2)) =
    if F.le b2 e1 && F.le b1 e2
    then
      inject (F.max b1 b2) (F.min e1 e2)
    else raise Bottom

  let contains_zero = is_included zero

  let fold_split n f (Private_Couple.I(b, e)) acc =
    let bound = ref b in
    let acc = ref acc in
    begin try
        for i = n downto 2 do
          let new_bound = F.add !bound (F.div (F.sub e !bound) (F.of_int i)) in
          acc := f (inject !bound new_bound) !acc;
          (*    Format.printf "float fold_split %a@."
                pretty (!bound, new_bound); *)
          bound := new_bound
        done;
      with Nan_or_infinite -> ()
    end;
    (*    Format.printf "float fold_split %a@."
          pretty (!bound, e); *)
    f (inject !bound e) !acc

  let contains_a_zero (Private_Couple.I(b, e)) =
    F.le_ieee b F.zero && F.le_ieee F.zero e

  let is_zero f =
    0 = compare zero f

  let is_singleton (Private_Couple.I(b, e)) = F.equal b e

  let neg_float v =
    let Private_Couple.I(b, e) = v in
    inject (F.neg e) (F.neg b) (* do not round because exact operation *)

  type rounding_mode = Any | Nearest_Even

  let top_single_precision_float =
    inject
      F.most_negative_single_precision_float
      F.max_single_precision_float

  let round_to_single_precision_float 
      ~rounding_mode (Private_Couple.I(b, e) as _arg) =
    if rounding_mode = Any
    then Floating_point.set_round_downward ()
    else Floating_point.set_round_nearest_even ();
    let b = Floating_point.round_to_single_precision_float b in
    if rounding_mode = Any
    then Floating_point.set_round_upward ();
    let e = Floating_point.round_to_single_precision_float e in
    let infb, b = 
      match classify_float b, denormal_treatment with
      | FP_infinite, _ ->
	if F.equal_ieee b F.infinity 
	then raise Bottom;
        true, F.most_negative_single_precision_float
      | FP_subnormal, FTZ -> false,
	F.zero_of_same_sign b
      | FP_subnormal, DenormalsandFTZ when not (F.is_negative b) ->
        false, F.zero
      | _ -> false, b
    in
    let infe, e = 
      match classify_float e, denormal_treatment with
      | FP_infinite, _ ->
	if F.equal_ieee e F.neg_infinity 
	then raise Bottom;
        true, F.max_single_precision_float
      | FP_subnormal, FTZ -> false,
	F.zero_of_same_sign e
      | FP_subnormal, DenormalsandFTZ when F.is_negative e ->
        false, F.minus_zero
      | _ -> false, e
    in
    infb || infe, inject b e
 (*   in
    Format.printf "Casting double -> float %a -> %B %a@."
      pretty _arg
      fl
      pretty _res;
    fl, _res
 *)


  (* Bitwise reinterpretation of a double to a 64-bit integer. signedness of the
     integer is defined by ~signed *)
  let bits_of_float64 ~signed (Private_Couple.I(l, u)) =
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
 let bits_of_float32 ~signed (Private_Couple.I(l, u)) =
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



  let add_float rounding_mode v1 v2 =
    let Private_Couple.I(b1, e1) =  v1 in
    let Private_Couple.I(b2, e2) =  v2 in
    if rounding_mode = Any
    then Floating_point.set_round_downward ()
    else Floating_point.set_round_nearest_even ();
    let bs = F.add b1 b2 in
    if rounding_mode = Any
    then Floating_point.set_round_upward ();
    let es = F.add e1 e2 in
    inject_r bs es

  let sub_float rounding_mode v1 v2 = add_float rounding_mode v1 (neg_float v2)

  let mult_float rounding_mode v1 v2 =
    let Private_Couple.I(b1, e1) =  v1 in
    let Private_Couple.I(b2, e2) =  v2 in
    if rounding_mode = Any
    then Floating_point.set_round_downward ()
    else Floating_point.set_round_nearest_even ();
    let a = F.mult b1 b2 in
    let b = F.mult b1 e2 in
    let c = F.mult e1 b2 in
    let d = F.mult e1 e2 in
    let min = F.min (F.min a b) (F.min c d) in
    let max =
      if rounding_mode = Any
      then begin
          Floating_point.set_round_upward ();
          let a = F.mult b1 b2 in
          let b = F.mult b1 e2 in
          let c = F.mult e1 b2 in
          let d = F.mult e1 e2 in
          F.max (F.max a b) (F.max c d)
        end
      else
        F.max (F.max a b) (F.max c d)
    in
    inject_r min max

  let div_float rounding_mode 
      (Private_Couple.I(b1, e1)) (Private_Couple.I(b2, e2) as v2) =
    if contains_a_zero v2
    then raise Nan_or_infinite;
    if rounding_mode = Any
    then Floating_point.set_round_downward ()
    else Floating_point.set_round_nearest_even ();
    let c1 = F.div b1 b2 in
    let c2 = F.div b1 e2 in
    let c3 = F.div e1 b2 in
    let c4 = F.div e1 e2 in
    let min = F.min (F.min c1 c2) (F.min c3 c4) in
    let max =
      if rounding_mode = Any
      then begin
          Floating_point.set_round_upward ();
          let c1 = F.div b1 b2 in
          let c2 = F.div b1 e2 in
          let c3 = F.div e1 b2 in
          let c4 = F.div e1 e2 in
          F.max (F.max c1 c2) (F.max c3 c4)
        end
      else F.max (F.max c1 c2) (F.max c3 c4)
    in
    inject_r min max

  let sqrt_float rounding_mode (Private_Couple.I(b, e)) =
    if rounding_mode = Any
    then Floating_point.set_round_downward ()
    else Floating_point.set_round_nearest_even ();
    let alarm, min =
      if F.le_ieee F.zero b
      then false, F.sqrt b
      else begin
          if not (F.le_ieee F.zero e)
          then raise Bottom;
          true, F.minus_zero
        end
    in
    if rounding_mode = Any
    then Floating_point.set_round_upward ();
    let max = F.sqrt e in
    alarm, inject min max

  let minus_one_one = inject F.minus_one F.one

  let cos_float v =
    Floating_point.set_round_nearest_even ();
      match v with
        Private_Couple.I(b, e) when F.equal b e ->
          let c = F.cos b in
          inject c c
      | _ ->
          minus_one_one

  let sin_float v =
    Floating_point.set_round_nearest_even ();
    match v with
      | Private_Couple.I(b, e) when F.equal b e -> let c = F.sin b in inject c c
      | _ -> minus_one_one

  let cos_float_precise v =
    Floating_point.set_round_nearest_even ();
    match v with
    | Private_Couple.I(b, e) ->
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

  let sin_float_precise v =
    Floating_point.set_round_nearest_even ();
    match v with
      | Private_Couple.I(b, e) ->
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

  let exp_float v =
      match v with
        Private_Couple.I(b, e) ->
          inject (F.exp b) (F.exp e)

  let widen (Private_Couple.I(b1,e1)) (Private_Couple.I(b2, e2)) =
    assert (F.le b2 b1);
    assert (F.le e1 e2);
    let b = if F.equal b2 b1 then b2 else F.widen_down b2 in
    let e = if F.equal e2 e1 then e2 else F.widen_up e2 in
    inject b e

  let equal_float_ieee f1 f2 =
      let Private_Couple.I(b1, e1) =  f1 in
      let Private_Couple.I(b2, e2) =  f2 in
      let intersects =
        F.le_ieee b1 e2 && F.le_ieee b2 e1
      in
      if not intersects
      then true, false
      else if F.equal_ieee b1 e1 && F.equal_ieee b2 e2
      then false, true
      else true, true

  let maybe_le_ieee_float f1 f2 =
      let Private_Couple.I(b1, _e1) =  f1 in
      let Private_Couple.I(_b2, e2) =  f2 in
      F.le_ieee b1 e2

  let maybe_lt_ieee_float f1 f2 =
      let Private_Couple.I(b1, _e1) =  f1 in
      let Private_Couple.I(_b2, e2) =  f2 in
      F.lt_ieee b1 e2

  let diff (Private_Couple.I(b1, e1) as f1) (Private_Couple.I(b2, e2)) =
    if F.le b2 b1 && F.le e1 e2
    then raise Bottom
    else if F.le b2 e1 && F.le e1 e2
    then inject b1 b2
    else if F.le b1 e2 && F.le b2 b1
    then inject e2 e1
    else f1

  let filter_le_f allmodes ~typ_loc (Private_Couple.I(b1, e1) as f1) e2 =
      let e2 = 
	if F.equal_ieee F.zero e2 
	then F.zero 
	else 
	  ( match allmodes, typ_loc with
            false, Cil_types.TFloat (Cil_types.FFloat, _) -> 
	      Floating_point.set_round_downward ();
	      Floating_point.round_to_single_precision_float e2
          | _ -> e2 ) 
      in
      if not (F.le b1 e2)
      then raise Bottom
      else if F.le e1 e2
      then f1
      else inject b1 e2

  let filter_le allmodes ~typ_loc f1 (Private_Couple.I(_b2, e2) as _f2) =
    filter_le_f allmodes ~typ_loc f1 e2

  let filter_lt allmodes ~typ_loc (Private_Couple.I(b1, _e1) as f1) (Private_Couple.I(_b2, e2)) =
    if F.le_ieee e2 b1
      then raise Bottom
    else
    let e2 = 
      if allmodes 
      then e2	
      else if F.equal_ieee F.zero e2 
      then F.neg_min_denormal
      else F.round_down e2
    in 
    filter_le_f allmodes ~typ_loc f1 e2

  let filter_ge_f allmodes ~typ_loc (Private_Couple.I(b1, e1) as f1) b2 =
    let b2 = 
      if F.equal_ieee F.minus_zero b2 
      then F.minus_zero
      else 
	( match allmodes, typ_loc with
          false, Cil_types.TFloat (Cil_types.FFloat, _) -> 
	    Floating_point.set_round_upward ();
	    Floating_point.round_to_single_precision_float b2
        | _ -> b2 )	
    in
    if not (F.le b2 e1)
    then raise Bottom
    else if F.le b2 b1
    then f1
    else inject b2 e1

  let filter_ge allmodes ~typ_loc f1 (Private_Couple.I(b2, _e2)) =
    filter_ge_f allmodes ~typ_loc f1 b2 

  let filter_gt allmodes ~typ_loc (Private_Couple.I(_b1, e1) as f1) (Private_Couple.I(b2, _e2)) =
    if F.le_ieee e1 b2
      then raise Bottom
    else
    let b2 = 
      if allmodes 
      then b2	
      else if F.equal_ieee F.zero b2 
      then F.min_denormal
      else F.round_up b2
    in 
    filter_ge_f allmodes ~typ_loc f1 b2 

  let subdiv_float_interval ~size (Private_Couple.I(l, u) as i) =
    let midpoint = F.avg l u in
    let midpointl, midpointu =
      if size <> 32 && size <> 64
      then midpoint, midpoint
      else
        let smidpoint = F.round_up midpoint in
        if size = 64
        then
          if F.le smidpoint u 
	  then 
	    if F.round_up l = u
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
		assert (F.equal l (Floating_point.round_to_single_precision_float l));
		assert (F.equal u (Floating_point.round_to_single_precision_float u));
		let midpointu = 
		  Floating_point.round_to_single_precision_float smidpoint 
		in
		Floating_point.set_round_downward ();
		let midpointl =
		  Floating_point.round_to_single_precision_float midpoint 
		in
		midpointl, midpointu
	      end
          end
    in
    if F.le midpointu l || F.le u midpointl
    then raise can_not_subdiv;
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

end

module Widen_Arithmetic_Value_Set = struct

  include Datatype.Big_int.Set

  let pretty fmt s =
    if is_empty s then Format.fprintf fmt "{}"
    else
      Pretty_utils.pp_iter
        ~pre:"@[<hov 1>{"
        ~suf:"}@]"
        ~sep:";@ "
        iter Int.pretty fmt s

  let of_list l =
    match l with
    | [] -> empty
    | [e] -> singleton e
    | e :: q ->
      List.fold_left (fun acc x -> add x acc) (singleton e) q

  let default_widen_hints =
    of_list (List.map Int.of_int [-128;-1;0;1;3;15;127;512;32767])

  let _default_widen_hints = of_list [Int.of_int (-1); Int.zero; Int.one]

  (* Bounds for all the signed types greater than int. No need to add smaller
     types: the computations are done as int, and then cast back in the smaller
     type. Thus they "overflow", but through a downcast. We do not add unsigned
     types either, given the fact that we automatically transform [0..2^n-1]
     into top_int when the value is stored. *)
  let hints_for_signed_int_types () =
    let size_int = Cil.bitsSizeOfInt Cil_types.IInt in
    let size_long = Cil.bitsSizeOfInt Cil_types.ILong in
    let size_long_long = Cil.bitsSizeOfInt Cil_types.ILongLong in
    let signed size = Int.pred (Int.two_power_of_int (size-1)) in
    of_list [signed size_int; signed size_long; signed size_long_long]

end

exception Infinity

let opt2 f m1 m2 =
  match m1, m2 with
    None, _ | _, None -> raise Infinity
  | Some m1, Some m2 -> f m1 m2

let opt1 f m =
  match m with
    None -> None
  | Some m -> Some (f m)

exception Error_Top
exception Error_Bottom

module O = FCSet.Make(Integer)

type pre_set = 
    Pre_set of O.t * int
    | Pre_top of Int.t * Int.t * Int.t

type tt =
  | Set of Int.t array
  | Float of Float_abstract.t
  | Top of Int.t option * Int.t option * Int.t * Int.t
(* Binary abstract operations do not model precisely float/integer operations.
   It is the responsability of the callers to have two operands of the same
   implicit type. The only exception is for [singleton_zero], which is the
   correct representation of [0.] *)


module Widen_Hints = Widen_Arithmetic_Value_Set
type widen_hint = Widen_Hints.t

let some_zero = Some Int.zero

let bottom = Set (Array.make 0 Int.zero)
let top = Top(None, None, Int.zero, Int.one)

let hash_v_option v =
  match v with None -> 97 | Some v -> Int.hash v

let hash v =
  match v with
    Set s -> Array.fold_left (fun acc v -> 1031 * acc + (Int.hash v)) 17 s
  | Top(mn,mx,r,m) ->
      hash_v_option mn + 5501 * (hash_v_option mx) +
        59 * (Int.hash r) + 13031 * (Int.hash m)
  | Float(f) ->
      3 + 17 * Float_abstract.hash f

let bound_compare x y =
  match x,y with
    None, None -> 0
  | None, Some _ -> 1
  | Some _, None -> -1
  | Some x, Some y -> Int.compare x y

exception Unequal of int

let compare e1 e2 =
  if e1==e2 then 0 else
  match e1,e2 with
  | Set e1,Set e2 -> 
      let l1 = Array.length e1 in
      let l2 = Array.length e2 in
      if l1 <> l2
      then l1 - l2 (* no overflow here *)
      else
        (try
           for i=0 to l1 -1 do
	     let r = Int.compare e1.(i) e2.(i) in
	     if r <> 0 then raise (Unequal r)
           done;
           0
         with Unequal v -> v )

  | _, Set _ -> 1
  | Set _, _ -> -1
  | Top(mn,mx,r,m), Top(mn',mx',r',m') ->
      let r1 = bound_compare mn mn' in
      if r1 <> 0 then r1
      else let r2 = bound_compare mx mx' in
      if r2 <> 0 then r2
      else let r3 = Int.compare r r' in
      if r3 <> 0 then r3
      else Int.compare m m'
  | _, Top _ -> 1
  | Top _, _ -> -1
  | Float(f1), Float(f2) ->
      Float_abstract.compare f1 f2
        (*| _, Float _ -> 1
          | Float _, _ -> -1 *)

let equal e1 e2 = compare e1 e2 = 0

let pretty fmt t =
  match t with
  | Top(mn,mx,r,m) ->
      let print_bound fmt =
        function 
	    None -> Format.fprintf fmt "--"
          | Some v -> Int.pretty fmt v
      in
        Format.fprintf fmt "[%a..%a]%t"
	  print_bound mn
          print_bound mx
          (fun fmt ->
            if Int.is_zero r && Int.is_one m then
              Format.fprintf fmt ""
            else Format.fprintf fmt ",%a%%%a"
                Int.pretty r
                Int.pretty m)
  | Float (f) ->
      Float_abstract.pretty fmt f
  | Set s ->
      if Array.length s = 0 then Format.fprintf fmt "BottomMod"
      else begin
        Pretty_utils.pp_iter
          ~pre:"@[<hov 1>{"
          ~suf:"}@]"
          ~sep:";@ "
          Array.iter Int.pretty fmt s
      end

let compare_elt_min elt min =
  match min with
  | None -> true
  | Some m -> Int.le m elt

let compare_elt_max elt max =
  match max with
  | None -> true
  | Some m -> Int.ge m elt

let all_positives min =
  match min with
  | None -> false
  | Some m -> Int.ge m Int.zero

let all_negatives max =
  match max with
  | None -> false
  | Some m -> Int.le m Int.zero

(* Sanity check for Top's arguments *)
let check min max r modu =
      if not (
       (Int.ge r Int.zero )
    && (Int.ge modu Int.one )
    && (Int.lt r modu)
    && (match min with
      | None -> true
      | Some m -> (Int.equal (Int.pos_rem m modu) r))
    && (match max with
      | None -> true
      | Some m -> (Int.equal (Int.pos_rem m modu) r)))
      then begin
	  let bound fmt = 
	    function 
		None -> Format.fprintf fmt "--" 
	      | Some(x) -> Int.pretty fmt x 
	  in 
	  Kernel.fatal "broken with min=%a max=%a r=%a modu=%a" 
	    bound min bound max Int.pretty r Int.pretty modu;
	end;
	    true


let cardinal_zero_or_one v =
  match v with
  | Top _ -> false
  | Set s -> Array.length s <= 1
  | Float (f) -> Float_abstract.is_singleton f

let is_singleton_int v = match v with
| Float _ | Top _ -> false
| Set s -> Array.length s = 1

let is_bottom x = x == bottom

let o_zero = O.singleton Int.zero
let o_one = O.singleton Int.one
let o_zero_or_one = O.union o_zero o_one

let small_nums = Array.map (fun i -> Set [| i |]) Int.small_nums

let zero = small_nums.(0)
let one = small_nums.(1)
let zero_or_one = Set [| Int.zero ; Int.one |]

let is_zero x = x == zero

let inject_singleton e =
  if Int.le Int.zero e && Int.le e Int.thirtytwo
  then small_nums.(Int.to_int e)
  else Set [| e |]

let share_set o s =
  if s = 0 then bottom
  else if s = 1
  then begin
      let e = O.min_elt o in
      inject_singleton e
    end
  else if O.equal o o_zero_or_one
  then zero_or_one
  else 
    let a = Array.make s Int.zero in
    let i = ref 0 in
    O.iter (fun e -> a.(!i) <- e; incr i) o;
    assert (!i = s);
    Set a

let share_array a s =
  if s = 0 then bottom
  else
    let e = a.(0) in
    if s = 1 && Int.le Int.zero e && Int.le e Int.thirtytwo
    then small_nums.(Int.to_int e)
    else if s = 2 && Int.is_zero e && Int.is_one a.(1)
    then zero_or_one
    else Set a

let inject_float f =
  if Float_abstract.is_zero f
  then zero
  else Float f

let inject_float_interval flow fup =
  let flow = F.of_float flow in
  let fup = F.of_float fup in
  if F.equal F.zero flow && F.equal F.zero fup
  then zero
  else Float (Float_abstract.inject (F.of_float flow) (F.of_float fup))

let subdiv_float_interval ~size v =
  match v with
  | Float f ->
      let f1, f2 = Float_abstract.subdiv_float_interval ~size f in
      inject_float f1, inject_float f2
  | Top _ | Set _ ->
      assert (is_zero v);
      raise can_not_subdiv

(*  let minus_zero = Float (Float_abstract.minus_zero, Float_abstract.minus_zero) *)

let is_one = equal one

let project_float v =
  if is_zero v
  then Float_abstract.zero
  else
    match v with
      Float f -> f
    | Top _ | Set _ -> raise Float_abstract.Nan_or_infinite

let in_interval x min max r modu =
  (Int.equal (Int.pos_rem x modu) r) &&
  (compare_elt_min x min) &&
  (compare_elt_max x max)

let array_mem v a =
  let l = Array.length a in
  let rec c i =
    if i = l then (-1)
    else 
      let ae = a.(i) in
      if Int.equal ae v
      then i
      else if Int.gt ae v
      then (-1)
      else c (succ i)
  in
  c 0

let contains_zero s =
  match s with
  | Top(mn,mx,r,m) -> in_interval Int.zero mn mx r m
  | Set s -> (array_mem Int.zero s)>=0
  | Float f -> Float_abstract.contains_zero f

exception Not_Singleton_Int

let project_int v = match v with
| Set [| e |] -> e
| _ -> raise Not_Singleton_Int

let cardinal v =
  match v with
    | Top (None,_,_,_) | Top (_,None,_,_) -> None
    | Top (Some mn, Some mx,_,m) ->
        Some (Int.succ ((Int.native_div (Int.sub mx mn) m)))
    | Set s -> Some (Int.of_int (Array.length s))
    | Float f -> if Float_abstract.is_singleton f then Some Int.one else None

let cardinal_less_than v n =
  let c =
    match v with
    | Top (None,_,_,_) | Top (_,None,_,_) -> raise Not_less_than
    | Top (Some mn, Some mx,_,m) ->
        Int.succ ((Int.native_div (Int.sub mx mn) m))
    | Set s -> Int.of_int (Array.length s)
    | Float f -> 
	if Float_abstract.is_singleton f then Int.one else raise Not_less_than
  in
  if Int.le c (Int.of_int n)
  then Int.to_int c (* This is smaller than the original [n] *)
  else raise Not_less_than

let share_top min max r modu =
  let r = Top (min, max, r, modu) in
  if equal r top then top else r

let inject_top min max r modu =
  assert (check min max r modu);
  match min, max with
  | Some mn, Some mx ->
      if Int.gt mx mn then
	let l = Int.succ (Int.div (Int.sub mx mn) modu) in
        if Int.le l !small_cardinal_Int
        then
	  let l = Int.to_int l in
          let s = Array.make l Int.zero in
          let v = ref mn in
	  let i = ref 0 in
          while (!i < l)
          do
	    s.(!i) <- !v;
	    v := Int.add modu !v;
	    incr i
          done;
	  assert (Int.equal !v (Int.add modu mx));
          share_array s l
        else Top (min, max, r, modu)
      else if Int.equal mx mn 
      then inject_singleton mn
      else bottom
  | _ ->
      share_top min max r modu


let subdiv ~size v =
  match v with
  | Float _ -> subdiv_float_interval ~size v
  | Set arr ->
      let len = Array.length arr in
      assert (len > 0 );
      if len <= 1 then raise can_not_subdiv;      
      let m = len lsr 1 in
      let lenhi = len - m in
      let lo = Array.sub arr 0 m in
      let hi = Array.sub arr m lenhi in
      share_array lo m,
      share_array hi lenhi
  | Top (Some lo, Some hi, r, modu) ->
      let mean = Int.native_div (Int.add lo hi) Abstract_interp.Int.two in
      let succmean = Abstract_interp.Int.succ mean in
      let hilo = Integer.round_down_to_r ~max:mean ~r ~modu in
      let lohi = Integer.round_up_to_r ~min:succmean ~r ~modu in
      inject_top (Some lo) (Some hilo) r modu,
      inject_top (Some lohi) (Some hi) r modu
  | Top _ -> raise can_not_subdiv

let inject_range min max = inject_top min max Int.zero Int.one

let top_float = Float Float_abstract.top

let unsafe_make_top_from_set_4 s =
  if debug_cardinal then assert (O.cardinal s >= 2);
  let m = O.min_elt s in
  let modu = O.fold
      (fun x acc ->
        if Int.equal x m
        then acc
        else Int.pgcd (Int.sub x m) acc)
      s
      Int.zero
  in
  let r = Int.pos_rem m modu in
  let max = O.max_elt s in
  let min = m in
  (min,max,r,modu)

let unsafe_make_top_from_array_4 s =
  let l = Array.length s in
  assert (l >= 2);
  let m = s.(0) in
  let modu = 
    Array.fold_left
      (fun acc x ->
	if Int.equal x m
        then acc
        else Int.pgcd (Int.sub x m) acc)
      Int.zero
      s
  in
  let r = Int.pos_rem m modu in
  let max = Some s.(pred l) in
  let min = Some m in
  assert (check min max r modu);
  (min,max,r,modu)

let unsafe_make_top_from_array s =
  let min, max, r, modu = unsafe_make_top_from_array_4 s in
  share_top min max r modu

let empty_ps = Pre_set (O.empty, 0)

let add_ps ps x =
  match ps with
  | Pre_set(o,s) ->
      if debug_cardinal then assert (O.cardinal o = s);
      if (O.mem x o) (* TODO: improve *)
      then ps
      else 
	let no = O.add x o in
	if s < !small_cardinal
	then begin	    
	    if debug_cardinal then assert (O.cardinal no = succ s);
	    Pre_set (no, succ s)
	  end
	else 
	  let min, max, _r, modu = unsafe_make_top_from_set_4 no in
	  Pre_top (min, max, modu)
  | Pre_top (min, max, modu) ->
      let new_modu =
	if Int.equal x min
        then modu
        else Int.pgcd (Int.sub x min) modu
      in
      let new_min = Int.min min x
      in
      let new_max = Int.max max x
      in
      Pre_top (new_min, new_max, new_modu)

let inject_ps ps =
  match ps with
    Pre_set(o, s) -> share_set o s
  | Pre_top (min, max, modu) -> 
      Top(Some min, Some max, Int.pos_rem min modu, modu)

let min_max_r_mod t =
  match t with
  | Set s ->
      assert (Array.length s >= 2);
      unsafe_make_top_from_array_4 s
  | Top (a,b,c,d) -> a,b,c,d
  | Float _ -> None, None, Int.zero, Int.one

let min_and_max t =
  match t with
  | Set s ->
      let l = Array.length s in
      assert (l >= 1);
      Some s.(0), Some s.(pred l)
  | Top (a,b,_,_) -> a, b
  | Float _ -> None, None

let min_and_max_float t =
  match t with
    Set _ when is_zero t -> F.zero, F.zero
  | Float f -> Float_abstract.min_and_max_float f
  | _ -> assert false

exception Unforceable

let force_float kind i =
  match i with
    Float _ ->  false, i
  | Set _ when is_zero i -> false, i
  | Top _ | Set _ ->
      ( match kind with
        Cil_types.FDouble ->
          ( try
              ( match min_and_max i with
                Some mn, Some mx ->
                  let mn, mx =
                    if Int.le Int.zero mn && Int.le mx Int.bits_of_max_float
                    then mn, mx
                    else if Int.le Int.min_int64 mn &&
                        Int.le mx Int.bits_of_most_negative_float
                    then mx, mn
                    else raise Unforceable
                  in
                  let red, fa =
                    Float_abstract.inject_r
                      (Int64.float_of_bits (Int.to_int64 mn))
                      (Int64.float_of_bits (Int.to_int64 mx))
                  in
                  assert (not red);
                  let f = inject_float fa in
                  (* Format.printf "cv: %a -> %a@."  pretty i pretty f; *)
                  false, f
          | _, _ -> true, top_float)
            with Unforceable ->
              true, top_float )
      | _ -> false, i)

let compare_min_int t1 t2 =
  let m1, _ = min_and_max t1 in
  let m2, _ = min_and_max t2 in
  match m1, m2 with
    None, None -> 0
  | None, Some _ -> -1
  | Some _, None -> 1
  | Some m1, Some m2 ->
      Int.compare m1 m2

let compare_max_int t1 t2 =
  let _, m1 = min_and_max t1 in
  let _, m2 = min_and_max t2 in
  match m1, m2 with
      None, None -> 0
    | None, Some _ -> 1
    | Some _, None -> -1
    | Some m1, Some m2 ->
        Int.compare m2 m1

let compare_min_float t1 t2 =
  let f1 = project_float t1 in
  let f2 = project_float t2 in
  Float_abstract.compare_min f1 f2

let compare_max_float t1 t2 =
  let f1 = project_float t1 in
  let f2 = project_float t2 in
  Float_abstract.compare_max f1 f2

let widen wh t1 t2 =
  if equal t1 t2 || cardinal_zero_or_one t1 then t2
  else
    match t2 with
        Float f2 ->
          ( try
              let f1 = project_float t1 in
              if not (Float_abstract.is_included f1 f2)
              then assert false;
              Float (Float_abstract.widen f1 f2)
            with Float_abstract.Nan_or_infinite -> assert false)
      | Top _ | Set _ ->
          let (mn2,mx2,r2,m2) = min_max_r_mod t2 in
          let (mn1,mx1,r1,m1) = min_max_r_mod t1 in
          let new_mod = Int.pgcd (Int.pgcd m1 m2) (Int.abs (Int.sub r1 r2)) in
          let new_rem = Int.rem r1 new_mod in
          
          let new_min = if bound_compare mn1 mn2 = 0 then mn2 else
            match mn2 with
              | None -> None
              | Some mn2 ->
                  try
                    let v = Widen_Hints.nearest_elt_le mn2 wh
                    in Some (Int.round_up_to_r ~r:new_rem ~modu:new_mod ~min:v)
                  with Not_found -> None
          in
          let new_max = if bound_compare mx1 mx2 = 0 then mx2 else
            match mx2 with None -> None
              | Some mx2 ->
                  try
                    let v = Widen_Hints.nearest_elt_ge mx2 wh
                    in Some (Int.round_down_to_r ~r:new_rem ~modu:new_mod ~max:v)
                  with Not_found -> None
          in
          let result = inject_top new_min new_max new_rem new_mod in
          (* Format.printf "%a -- %a --> %a (thx to %a)@."
            pretty t1 pretty t2 pretty result
            Widen_Hints.pretty wh; *)
          result

let compute_first_common mn1 mn2 r modu =
  if mn1 == None && mn2 == None
  then None
  else
    let m =
      match (mn1, mn2) with
      | Some m, None | None, Some m -> m
      | Some m1, Some m2 ->
          Int.max m1 m2
      | None, None -> assert false (* already tested above *)
    in
    Some (Int.round_up_to_r m r modu)

let compute_last_common mx1 mx2 r modu =
  if mx1 == None && mx2 == None
  then None
  else
    let m =
      match (mx1, mx2) with
      | Some m, None | None, Some m -> m
      | Some m1, Some m2 ->
          Int.min m1 m2
      | None, None -> assert false (* already tested above *)
    in
    Some (Int.round_down_to_r m r modu)


let min_min x y =
  match x,y with
  | None,_ | _,None -> None
  | Some x, Some y -> Some (Int.min x y)

let max_max x y =
  match x,y with
  | None,_ | _,None -> None
  | Some x, Some y -> Some (Int.max x y)

(* [extended_euclidian_algorithm a b] returns x,y,gcd such that a*x+b*y=gcd(x,y). *)
let extended_euclidian_algorithm a b =
  assert (Int.gt a Int.zero);
  assert (Int.gt b Int.zero);
  let a = ref a and b = ref b in
  let x = ref Int.zero and lastx = ref Int.one in
  let y = ref Int.one and lasty = ref Int.zero in
  while not (Int.is_zero !b) do
    let (q,r) = Int.div_rem !a !b in
    a := !b;
    b := r;
    let tmpx = !x in
    (x:= Int.sub !lastx (Int.mul q !x); lastx := tmpx);
    let tmpy = !y in
    (y:= Int.sub !lasty (Int.mul q !y); lasty := tmpy);
  done;
  (!lastx,!lasty,!a)

(* [JS 2013/05/23] unused right now 
   [modular_inverse a m] returns [x] such that a*x is congruent to 1 mod m. *)
let _modular_inverse a m =
  let (x,_,gcd) = extended_euclidian_algorithm a m in
  assert (Int.equal Int.one gcd);
  x

(* This function provides solutions to the chinese remainder theorem,
   i.e. it finds the solutions x such that:
   x == r1 mod m1 && x == r2 mod m2.

   If no such solution exists, it raises Error_Bottom; else it returns
   (r,m) such that all solutions x are such that x == r mod m. *)
let compute_r_common r1 m1 r2 m2 =

  (* (E1) x == r1 mod m1 && x == r2 mod m2
     <=> \E k1,k2: x = r1 + k1*m1 && x = r2 + k2*m2
     <=> \E k1,k2: x = r1 + k1*m1 && k1*m1 - k2*m2 = r2 - r1

     Let c = r2 - r1. The equation (E2): k1*m1 - k2*m2 = c is
     diophantine; there are solutions x to (E1) iff there are
     solutions (k1,k2) to (E2).

     Let d = pgcd(m1,m2). There are solutions to (E2) only if d
     divides c (because d divides k1*m1 - k2*m2). Else we raise
     [Error_Bottom]. *)
  let (x1,_,pgcd) = extended_euclidian_algorithm m1 m2 in
  let c = Int.sub r2 r1 in
  let (c_div_d,c_rem) = Int.div_rem c pgcd in
  if not (Int.equal c_rem Int.zero)
  then raise Error_Bottom

  (* The extended euclidian algorithm has provided solutions x1,x2 to
     the Bezout identity x1*m1 + x2*m2 = d.

     x1*m1 + x2*m2 = d ==> x1*(c/d)*m1 + x2*(c/d)*m2 = d*(c/d).

     Thus, k1 = x1*(c/d), k2=-x2*(c/d) are solutions to (E2)
     Thus, x = r1 + x1*(c/d)*m1 is a particular solution to (E1). *)
  else let k1 = Int.mul x1 c_div_d in
       let x = Int.add r1 (Int.mul k1 m1) in

       (* If two solutions x and y exist, they are equal modulo ppcm(m1,m2).
	  We have x == r1 mod m1 && y == r1 mod m1 ==> \E k1: x - y = k1*m1
	          x == r2 mod m2 && y == r2 mod m2 ==> \E k2: x - y = k2*m2

	  Thus k1*m1 = k2*m2 is a multiple of m1 and m2, i.e. is a multiple
	  of ppcm(m1,m2). Thus x = y mod ppcm(m1,m2). *)
       let ppcm = Int.divexact (Int.mul m1 m2) pgcd in

       (* x may be bigger than the ppcm, we normalize it. *)
       (Int.rem x ppcm, ppcm)
;;

let array_truncate r i =
  if i = 0 
  then bottom
  else if i = 1
  then inject_singleton r.(0)
  else begin
      (Obj.truncate (Obj.repr r) i);
      assert (Array.length r = i);
      Set r
    end

let array_inter a1 a2 =
  let l1 = Array.length a1 in
  let l2 = Array.length a2 in
  let lr_max = min l1 l2 in
  let r = Array.make lr_max Int.zero in
  let rec c i i1 i2 =
    if i1 = l1 || i2 = l2
    then array_truncate r i
    else 
      let e1 = a1.(i1) in
      let e2 = a2.(i2) in
      if Int.equal e1 e2
      then begin
	  r.(i) <- e1;
	  c (succ i) (succ i1) (succ i2)
	end
      else if Int.lt e1 e2
      then c i (succ i1) i2
      else c i i1 (succ i2)
  in
  c 0 0 0

let meet v1 v2 =
  if v1 == v2 then v1 else
  let result =
    match v1,v2 with
    | Top(min1,max1,r1,modu1), Top(min2,max2,r2,modu2) ->
        begin
          try
            let r,modu = compute_r_common r1 modu1 r2 modu2 in
            inject_top
              (compute_first_common min1 min2 r modu)
              (compute_last_common max1 max2 r modu)
              r
              modu
          with Error_Bottom ->
            (*Format.printf "meet to bottom: %a /\\ %a@\n"
              pretty v1 pretty v2;*)
            bottom
        end
    | Set s1 , Set s2 -> array_inter s1 s2
    | Set s, Top(min, max, rm, modu)
    | Top(min, max, rm, modu), Set s ->
	let l = Array.length s in
	let r = Array.create l Int.zero in
	let rec c i j =
	  if i = l 
	  then 
	    array_truncate r j 
	  else
	    let si = succ i in
	    let x = s.(i) in
	    if in_interval x min max rm modu
	    then begin
		r.(j) <- x;
		c si (succ j)
	      end
	    else
	      c si j
	in
	c 0 0
    | Float(f1), Float(f2) ->
        ( try
          inject_float (Float_abstract.meet f1 f2)
        with Float_abstract.Bottom -> bottom )
    | (Float f) as ff, other | other, ((Float f) as ff) ->
        if equal top other
        then ff
        else if (Float_abstract.contains_zero f) && contains_zero other
        then zero
        else bottom
  in
  (*      Format.printf "meet: %a /\\ %a -> %a@\n"
          pretty v1 pretty v2 pretty result;*)
  result

let narrow v1 v2 =
  match v1, v2 with
  | Float _, Float _ | (Top _| Set _), (Top _ | Set _) ->
      meet v1 v2 (* meet is exact *)
  | v, (Top _ as t) | (Top _ as t), v when equal t top -> v
  | Float f, (Set _ as s) | (Set _ as s), Float f when is_zero s ->
      ( try
        inject_float (Float_abstract.meet f Float_abstract.zero)
      with Float_abstract.Bottom -> bottom )
  | Float _, (Set _ | Top _) | (Set _ | Top _), Float _ ->
      (* ill-typed case. It is better to keep the operation symmetric *)
      top

let link _ = assert false

let join v1 v2 =
  let result =
    if v1 == v2 then v1 else
      match v1,v2 with
      | Top(mn1,mx1,r1,m1), Top(mn2,mx2,r2,m2) ->
          assert (check mn1 mx1 r1 m1);
          assert (check mn2 mx2 r2 m2);
          let modu = Int.pgcd (Int.pgcd m1 m2) (Int.abs(Int.sub r1 r2)) in
          let r = Int.rem r1 modu in
          let min = min_min mn1 mn2 in
          let max = max_max mx1 mx2 in
          let r  = inject_top min max r modu in
          r
      | Set s, (Top(min, max, r, modu) as t)
      | (Top(min, max, r, modu) as t), Set s ->
	  let l = Array.length s in
          if l = 0 then t
          else
            let f modu elt = Int.pgcd modu (Int.abs(Int.sub r elt)) in
            let new_modu = Array.fold_left f modu s in
            let new_r = Int.rem r new_modu in
            let new_min = match min with
              None -> None
            | Some m -> Some (Int.min m s.(0))
            in
            let new_max = match max with
              None -> None
            | Some m -> Some (Int.max m s.(pred l))
            in
            assert (check new_min new_max new_r new_modu);
            share_top new_min new_max new_r new_modu
      | Set s1 , Set s2 ->
	  let l1 = Array.length s1 in
	  if l1 = 0 
	  then v2
	  else
	    let l2 = Array.length s2 in
	    if l2 = 0 
	    then v1
	    else
	      (* second pass: make a set or make a top *)
	      let second uniq =
		if uniq <= !small_cardinal
		then 
		  let r = Array.create uniq Int.zero in
		  let rec c i i1 i2 =
		    if i1 = l1
		    then begin
			Array.blit s2 i2 r i (l2 - i2);
			share_array r uniq
		      end
		    else if i2 = l2
		    then begin
			Array.blit s1 i1 r i (l1 - i1);
			share_array r uniq
		      end
		    else
		      let si = succ i in
		      let e1 = s1.(i1) in
		      let e2 = s2.(i2) in
		      if Int.lt e2 e1
		      then begin
			  r.(i) <- e2;
			  c si i1 (succ i2)
			end
		      else begin
			  r.(i) <- e1;
			  let si1 = succ i1 in
			  if Int.equal e1 e2
			  then begin
			      c si si1 (succ i2)
			    end
			  else begin
			      c si si1 i2
			    end
			end
		  in
		  c 0 0 0
		else begin
	            let m = Int.min s1.(0) s2.(0) in
		    let accum acc x =
		      if Int.equal x m
		      then acc
		      else Int.pgcd (Int.sub x m) acc
		    in
		    let modu = ref Int.zero in
		    for j = 0 to pred l1 do
		      modu := accum !modu s1.(j)
		    done;
		    for j = 0 to pred l2 do
		      modu := accum !modu s2.(j)
		    done;		  
		    inject_ps 
		      (Pre_top (m, Int.max s1.(pred l1) s2.(pred l2), !modu))
		  end		
	      in
	      (* first pass: count unique elements and detect inclusions *)
	      let rec first i1 i2 uniq inc1 inc2 =
		let finished1 = i1 = l1 in 
		if finished1 
		then begin
		  if inc2
		  then v2
		  else second (uniq + l2 - i2)
		  end
		else
		let finished2 = i2 = l2 in 
		if finished2
		then begin
		  if inc1
		  then v1
		  else second (uniq + l1 - i1)
		  end
		else
		  let e1 = s1.(i1) in
		  let e2 = s2.(i2) in
		  if Int.lt e2 e1
		  then begin
		      first i1 (succ i2) (succ uniq) false inc2
		    end
		  else if Int.gt e2 e1
		  then begin
		      first (succ i1) i2 (succ uniq) inc1 false
		    end
		  else first (succ i1) (succ i2) (succ uniq) inc1 inc2
	      in
	      first 0 0 0 true true

      | Float(f1), Float(f2) ->
          inject_float (Float_abstract.join f1 f2)
      | Float (f) as ff, other | other, (Float (f) as ff) ->
          if is_zero other
          then inject_float (Float_abstract.join Float_abstract.zero f)
          else if is_bottom other then ff
          else top
  in
(*  Format.printf "mod_join %a %a -> %a@."
    pretty v1 pretty v2 pretty result; *)
  result

let fold_int f v acc =
  match v with
    Top(None,_,_,_) | Top(_,None,_,_) | Float _ ->
      raise Error_Top
  | Top(Some inf, Some sup, _, step) ->
      Int.fold f ~inf ~sup ~step acc
  | Set s ->
      Array.fold_left (fun acc x -> f x acc) acc s

let fold_enum f v acc =
  match v with
  | Float fl when Float_abstract.is_singleton fl -> f v acc
  | Float _ -> raise Error_Top
  | Set _ | Top _ -> fold_int (fun x acc -> f (inject_singleton x) acc) v acc

let fold_split ~split f v acc =
  match v with
  | Float (fl) when Float_abstract.is_singleton fl ->
      f v acc
  | Float (fl) ->
      Float_abstract.fold_split
        split
        (fun fl acc -> f (inject_float fl) acc)
        fl
        acc
  | Top(_,_,_,_) | Set _ ->
      fold_int (fun x acc -> f (inject_singleton x) acc) v acc


(** [min_is_lower mn1 mn2] is true iff mn1 is a lower min than mn2 *)
let min_is_lower mn1 mn2 =
  match mn1, mn2 with
    None, _ -> true
  | _, None -> false
  | Some m1, Some m2 ->
      Int.le m1 m2

(** [max_is_greater mx1 mx2] is true iff mx1 is a greater max than mx2 *)
let max_is_greater mx1 mx2 =
  match mx1, mx2 with
    None, _ -> true
  | _, None -> false
  | Some m1, Some m2 ->
      Int.ge m1 m2

let rem_is_included r1 m1 r2 m2 =
  (Int.is_zero (Int.rem m1 m2)) && (Int.equal (Int.rem r1 m2) r2)

let array_for_all f (a : Integer.t array) =
  let l = Array.length a in
  let rec c i =
    i = l ||
    ((f a.(i)) && c (succ i))
  in
  c 0

let array_subset a1 a2 =
  let l1 = Array.length a1 in
  let l2 = Array.length a2 in
  if l1 > l2 then false
  else
    let rec c i1 i2 =
      if i1 = l1 then true
      else if i2 = l2 then false
      else 
	let e1 = a1.(i1) in
	let e2 = a2.(i2) in
	let si2 = succ i2 in
	if Int.equal e1 e2
	then c (succ i1) si2
	else if Int.lt e1 e2
	then false
	else c i1 si2 (* TODO: improve by not reading a1.(i1) all the time *)
    in
    c 0 0

let is_included t1 t2 =
  (t1 == t2) ||
  match t1,t2 with
  | Top(mn1,mx1,r1,m1), Top(mn2,mx2,r2,m2) ->
      (min_is_lower mn2 mn1) &&
      (max_is_greater mx2 mx1) &&
      rem_is_included r1 m1 r2 m2
  | Top _, Set _ -> false (* Top _ represents more elements
                             than can be represented by Set _ *)
  | Set s, Top(min, max, r, modu) ->
      array_for_all (fun x -> in_interval x min max r modu) s
  | Set s1, Set s2 -> array_subset s1 s2
  | Float(f1), Float(f2) ->
      Float_abstract.is_included f1 f2
  | Float _, _ -> equal t2 top
  | _, Float (f) -> is_zero t1 && (Float_abstract.contains_zero f)

let join_and_is_included a b =
    let ab = join a b in (ab, equal a b)
(* In this lattice, [meet t1 t2=bottom] iff the
   intersection of [t1] and [t2] is empty. *)
let intersects t1 t2 =
  not (equal bottom (meet t1 t2))

let partially_overlaps ~size t1 t2 =
  match t1, t2 with
    Set s1, Set s2 ->
      not 
	(array_for_all
	    (fun e1 ->
	      array_for_all 
		(fun e2 ->
		  Int.equal e1 e2 ||
		    Int.le e1 (Int.sub e2 size) ||
		    Int.ge e1 (Int.add e2 size))
		s2)
	    s1)
  | Set s, Top(mi, ma, r, modu) | Top(mi, ma, r, modu), Set s ->
      not
	(array_for_all
	    (fun e ->
	      let psize = Int.pred size in
	      (not (compare_elt_min (Int.add e psize) mi)) ||
		(not (compare_elt_max (Int.sub e psize) ma)) ||
		( Int.ge modu size &&
		    let re = Int.pos_rem (Int.sub e r) modu in
		    Int.is_zero re ||
		      (Int.ge re size &&
			  Int.le re (Int.sub modu size)) ))
	    s)
  | _ -> false (* TODO *)

let map_set_exnsafe_acc f acc (s : Integer.t array) =
  Array.fold_left
    (fun acc v -> add_ps acc (f v))
    acc
    s

let map_set_exnsafe f (s : Integer.t array) = 
  inject_ps (map_set_exnsafe_acc f empty_ps s)

let apply2_notzero f (s1 : Integer.t array) s2 =
  inject_ps
    (Array.fold_left
	(fun acc v1 -> 
	  Array.fold_left
	    (fun acc v2 ->
	      if Int.is_zero v2
	      then acc
	      else add_ps acc (f v1 v2))
	    acc 
	    s2)
	empty_ps
	s1)

let apply2_n f (s1 : Integer.t array) (s2 : Integer.t array) =
  let ps = ref empty_ps in
  let l1 = Array.length s1 in
  let l2 = Array.length s2 in
  for i1 = 0 to pred l1 do
    let e1 = s1.(i1) in
    for i2 = 0 to pred l2 do
      ps := add_ps !ps (f e1 s2.(i2))
    done
  done;
  inject_ps !ps

let apply2_v f s1 s2 =
  match s1, s2 with
    [| x1 |], [| x2 |] -> 
      inject_singleton (f x1 x2)
  | _ -> apply2_n f s1 s2


let apply_set f v1 v2 =
  match v1,v2 with
  | Set s1, Set s2 ->
      apply2_n f s1 s2
  | _ ->
      (*ignore (CilE.warn_once "unsupported case for binary operator '%s'" info);*)
      top

let apply_set_unary _info f v = (* TODO: improve by allocating array*)
  match v with
  | Set s -> map_set_exnsafe f s
  | _ ->
      (*ignore (CilE.warn_once "unsupported case for unary operator '%s'" info);*)
      top

let apply_bin_1_strict_incr f x (s : Integer.t array) =
  let l = Array.length s in
  let r = Array.create l Int.zero in
  let rec c i =
    if i = l
    then share_array r l
    else 
      let v = f x s.(i) in
      r.(i) <- v;
      c (succ i)
  in
  c 0

let apply_bin_1_strict_decr f x (s : Integer.t array) =
  let l = Array.length s in
  let r = Array.create l Int.zero in
  let rec c i =
    if i = l
    then share_array r l
    else 
      let v = f x s.(i) in
      r.(l - i - 1) <- v;
      c (succ i)
  in
  c 0

let map_set_strict_decr f (s : Integer.t array) =
  let l = Array.length s in
  let r = Array.create l Int.zero in
  let rec c i =
    if i = l
    then share_array r l
    else 
      let v = f s.(i) in
      r.(l - i - 1) <- v;
      c (succ i)
  in
  c 0

let map_set_decr f (s : Integer.t array) =
  let l = Array.length s in
  if l = 0 
  then bottom
  else
    let r = Array.create l Int.zero in
    let rec c srcindex dstindex last =
      if srcindex < 0
      then begin
	  r.(dstindex) <- last;
	  array_truncate r (succ dstindex)
	end
      else 
	let v = f s.(srcindex) in
	if Int.equal v last
	then
	  c (pred srcindex) dstindex last
	else begin
	    r.(dstindex) <- last;
	    c (pred srcindex) (succ dstindex) v
	  end
    in
    c (l-2) 0 (f s.(pred l))

let map_set_incr f (s : Integer.t array) =
  let l = Array.length s in
  if l = 0 
  then bottom
  else
    let r = Array.create l Int.zero in
    let rec c srcindex dstindex last =
      if srcindex = l
      then begin
	  r.(dstindex) <- last;
	  array_truncate r (succ dstindex)
	end
      else 
	let v = f s.(srcindex) in
	if Int.equal v last
	then
	  c (succ srcindex) dstindex last
	else begin
	    r.(dstindex) <- last;
	    c (succ srcindex) (succ dstindex) v
	  end
    in
    c 1 0 (f s.(0))


let add_singleton_int i v = match v with
  | Float _ -> top
  | Set s -> apply_bin_1_strict_incr Int.add i s
  | Top (mn, mx, r, m) ->
    let incr v = Int.add i v in
    let new_mn = opt1 incr mn in
    let new_mx = opt1 incr mx in
    let new_r = Int.pos_rem (incr r) m in
    share_top new_mn new_mx new_r m


let rec add_int v1 v2 =
  match v1,v2 with
    Float _, _ | _, Float _ -> top
  | Set [| x |], Set s | Set s, Set [| x |]->
      apply_bin_1_strict_incr Int.add x s 
  | Set s1, Set s2 ->
      apply2_n Int.add s1 s2
  | Top(mn1,mx1,r1,m1), Top(mn2,mx2,r2,m2) ->
      let m = Int.pgcd m1 m2 in
      let r = Int.rem (Int.add r1 r2) m in
      let mn =
        try
          Some (Int.round_up_to_r (opt2 Int.add mn1 mn2) r m)
        with Infinity -> None
      in
      let mx =
        try
          Some (Int.round_down_to_r (opt2 Int.add mx1 mx2) r m)
        with Infinity -> None
      in
      inject_top mn mx r m
  | Set s, (Top _ as t) | (Top _ as t), Set s ->
      let l = Array.length s in
      if l = 0
      then bottom
      else if l = 1
      then (* only one element *)
        add_singleton_int s.(0) t
      else
        add_int t (unsafe_make_top_from_array s)

          (* TODO rename to neg_int *)
let neg v =
  match v with
  | Float _ -> top
  | Set s -> map_set_strict_decr Int.neg s
  | Top(mn,mx,r,m) ->
      share_top
        (opt1 Int.neg mx)
        (opt1 Int.neg mn)
        (Int.pos_rem (Int.neg r) m)
        m

let sub v1 v2 = add_int v1 (neg v2)

type ext_value = Ninf | Pinf | Val of Int.t
let inject_min = function None -> Ninf | Some m -> Val m
let inject_max = function None -> Pinf | Some m -> Val m
let ext_neg = function Ninf -> Pinf | Pinf -> Ninf | Val v -> Val (Int.neg v)
let ext_mul x y =
  match x, y with
  | Ninf, Ninf | Pinf, Pinf -> Pinf
  | Ninf, Pinf | Pinf, Ninf -> Ninf
  | Val v1, Val v2 -> Val (Int.mul v1 v2)
  | (x, Val v | Val v, x) when (Int.gt v Int.zero) -> x
  | (x, Val v | Val v, x) when (Int.lt v Int.zero) -> ext_neg x
  | _ -> Val Int.zero

let ext_min x y =
  match x,y with
    Ninf, _ | _, Ninf -> Ninf
  | Pinf, x | x, Pinf -> x
  | Val x, Val y -> Val(Int.min x y)

let ext_max x y =
  match x,y with
    Pinf, _ | _, Pinf -> Pinf
  | Ninf, x | x, Ninf -> x
  | Val x, Val y -> Val(Int.max x y)

let ext_proj = function Val x -> Some x | _ -> None

let singleton_zero = zero
let singleton_one = one
let zero_or_one = join singleton_one singleton_zero

let negative = Top(None, Some Int.minus_one,Int.zero,Int.one)

let min_int s =
  match s with
  | Top (min,_,_,_) -> min
  | Set s -> 
      if Array.length s = 0
      then raise Error_Bottom
      else
	Some s.(0)
  | Float _ -> None


let max_int s =
  match s with
  | Top (_,max,_,_) -> max
  | Set s -> 
      let l = Array.length s in
      if l = 0
      then raise Error_Bottom
      else
	Some s.(pred l)
  | Float _ -> None


exception No_such_element

let smallest_above min x = (* TODO: improve for Set *)
  match x with
  | Set s ->
      let r = ref None in
      Array.iter
        (fun e ->
          if Int.ge e min
          then match !r with
          | Some rr when Int.lt e rr -> r := Some e
          | None -> r := Some e
          | _ -> ())
        s;
      begin match !r with
        None -> raise No_such_element
      | Some r -> r
      end
  | Top(mn,mx,r,modu) ->
      let some_min = Some min in
      if not (max_is_greater mx some_min)
      then raise No_such_element;
      if min_is_lower some_min mn
      then Extlib.the mn
      else Int.round_up_to_r ~min ~r ~modu
  | Float _ -> raise No_such_element

let largest_below max x = (* TODO: improve for Set *)
  match x with
  | Float _ -> raise No_such_element
  | Set s ->
      let r = ref None in
      Array.iter
        (fun e ->
          if Int.le e max
          then match !r with
          | Some rr when Int.gt e rr -> r := Some e
          | None -> r := Some e
          | _ -> ())
        s;
      begin match !r with
        None -> raise No_such_element
      | Some r -> r
      end
  | Top(mn,mx,r,modu) ->
      let some_max = Some max in
      if not (min_is_lower mn some_max)
      then raise No_such_element;
      if max_is_greater some_max mx
      then Extlib.the mx
      else Int.round_down_to_r ~max ~r ~modu

(* Rounds up (x+1) to the next power of two, then substracts one; optimized. *)
let next_pred_power_of_two x =
  (* Unroll the first iterations, and skip the tests. *)
  let x = Int.logor x (Int.shift_right x Int.one) in
  let x = Int.logor x (Int.shift_right x Int.two) in
  let x = Int.logor x (Int.shift_right x Int.four) in
  let x = Int.logor x (Int.shift_right x Int.eight) in
  let x = Int.logor x (Int.shift_right x Int.sixteen) in
  let shift = Int.thirtytwo in
  let rec loop old shift =
    let x = Int.logor old (Int.shift_right old shift) in
    if Int.equal old x then x
    else loop x (Int.shift_left shift Int.one) in
  loop x shift

(* [different_bits min max] returns an overapproximation of the mask
   of the bits that can be different for different numbers
   in the interval [min]..[max] *)
let different_bits min max =
  let x = Int.logxor min max in
  next_pred_power_of_two x

(* [pos_max_land min1 max1 min2 max2] computes an upper bound for
   [x1 land x2] where [x1] is in [min1]..[max1] and [x2] is in [min2]..[max2].
   Precondition : [min1], [max1], [min2], [max2] must all have the
   same sign.
   Note: the algorithm below is optimal for the problem as stated.
   It is possible to compute this optimal solution faster but it does not
   seem worth the time necessary to think about it as long as integers
   are at most 64-bit. *)
let pos_max_land min1 max1 min2 max2 =
  let x1 = different_bits min1 max1 in
  let x2 = different_bits min2 max2 in
(*      Format.printf "pos_max_land %a %a -> %a |  %a %a -> %a@."
        Int.pretty min1 Int.pretty max1 Int.pretty x1
        Int.pretty min2 Int.pretty max2 Int.pretty x2; *)
  let fold_maxs max1 p f acc =
    let rec aux p acc =
      let p = Int.shift_right p Int.one in
      if Int.is_zero p
      then f max1 acc
      else if Int.is_zero (Int.logand p max1) 
      then aux p acc
      else 
	let c = Int.logor (Int.sub max1 p) (Int.pred p) in
	aux p (f c acc)
    in aux p acc
  in
  let sx1 = Int.succ x1 in
  let n1 = fold_maxs max1 sx1 (fun _ y -> succ y) 0 in
  let maxs1 = Array.make n1 sx1 in
  let _ = fold_maxs max1 sx1 (fun x i -> Array.set maxs1 i x; succ i) 0 in
  fold_maxs max2 (Int.succ x2)
    (fun max2 acc -> 
      Array.fold_left
	(fun acc max1 -> Int.max (Int.logand max1 max2) acc)
	acc
	maxs1)
    (Int.logand max1 max2)

let bitwise_or v1 v2 =
  if is_bottom v1 || is_bottom v2
  then bottom
  else
    match v1, v2 with
      Float _, _ | _, Float _ -> top
     | Set s1, Set s2 ->
        apply2_v Int.logor s1 s2
     | Set s, v | v, Set s when Array.length s = 1 && Int.is_zero s.(0) -> v
     | Top _, _ | _, Top _ ->
         ( match min_and_max v1 with
           Some mn1, Some mx1 when Int.ge mn1 Int.zero ->
             ( match min_and_max v2 with
               Some mn2, Some mx2 when Int.ge mn2 Int.zero ->
                 let new_max = next_pred_power_of_two (Int.logor mx1 mx2) in
		 let new_min = Int.max mn1 mn2 in (* Or can only add bits *)
                 inject_range (Some new_min) (Some new_max)
             | _ -> top )
         | _ -> top )

let bitwise_xor v1 v2 =
  if is_bottom v1 || is_bottom v2
  then bottom
  else
    match v1, v2 with
     | Float _, _ | _, Float _ -> top
     | Set s1, Set s2 -> apply2_v Int.logxor s1 s2
     | Top _, _ | _, Top _ ->
       (match min_and_max v1 with
         | Some mn1, Some mx1 when Int.ge mn1 Int.zero ->
           (match min_and_max v2 with
             | Some mn2, Some mx2 when Int.ge mn2 Int.zero ->
               let new_max = next_pred_power_of_two (Int.logor mx1 mx2) in
	       let new_min = Int.zero in
               inject_range (Some new_min) (Some new_max)
             | _ -> top )
         | _ -> top )


let contains_non_zero v =
  not (is_zero v || is_bottom v)

        (* TODO: rename this function to scale_int *)
let scale f v =
  if Int.is_zero f
  then zero
  else
    match v with
    | Float _ -> top
    | Top(mn1,mx1,r1,m1) ->
        let incr = Int.mul f in
        if Int.is_zero f
        then singleton_zero
        else if Int.gt f Int.zero
        then
          let modu = incr m1 in
          share_top
            (opt1 incr mn1) (opt1 incr mx1)
            (Int.pos_rem (incr r1) modu) modu
        else
          let modu = Int.neg (incr m1) in
          share_top
            (opt1 incr mx1) (opt1 incr mn1)
            (Int.pos_rem (incr r1) modu) modu
    | Set s ->
	if Int.ge f Int.zero 
	then apply_bin_1_strict_incr Int.mul f s
	else apply_bin_1_strict_decr Int.mul f s

let scale_div ~pos f v =
  assert (not (Int.is_zero f));
  let div_f =
    if pos
    then fun a -> Int.pos_div a f
    else fun a -> Int.c_div a f
  in
  match v with
  | Top(mn1,mx1,r1,m1) ->
      let r, modu =
	let negative = max_is_greater (some_zero) mx1 in
        if (negative (* all negative *) ||
	     pos (* good div *) ||
       	     (min_is_lower (some_zero) mn1)  (* all positive *) ||
             (Int.is_zero (Int.rem r1 f)) (* exact *) )	 
	  && (Int.is_zero (Int.rem m1 f))
        then
          let modu = Int.abs (div_f m1) in
	  let r = if negative then Int.sub r1 m1 else r1 in
          (Int.pos_rem (div_f r) modu), modu
        else (* degeneration*)
          Int.zero, Int.one
      in
      let divf_mn1 = opt1 div_f mn1 in
      let divf_mx1 = opt1 div_f mx1 in
      let mn, mx =
        if Int.gt f Int.zero
        then divf_mn1, divf_mx1
        else divf_mx1, divf_mn1
      in
      inject_top mn mx r modu
  | Set s -> 
      if Int.lt f Int.zero
      then
	map_set_decr div_f s
      else
	map_set_incr div_f s
  | Float _ -> top

let div_set x sy =
  Array.fold_left
    (fun acc elt ->
      if Int.is_zero elt
      then acc
      else join acc (scale_div ~pos:false elt x))
    bottom
    sy

(* ymin and ymax must be the same sign *)
let div_range x ymn ymx =
  match min_and_max x with
  | Some xmn, Some xmx ->
      let c1 = Int.c_div xmn ymn in
      let c2 = Int.c_div xmx ymn in
      let c3 = Int.c_div xmn ymx in
      let c4 = Int.c_div xmx ymx in
      let min = Int.min (Int.min c1 c2) (Int.min c3 c4) in
      let max = Int.max (Int.max c1 c2) (Int.max c3 c4) in

          (*     Format.printf "div: %a %a %a %a@."
                 Int.pretty mn Int.pretty mx Int.pretty xmn Int.pretty xmx; *)
          inject_range (Some min) (Some max)

  | _ ->
    Kernel.warning ~once:true ~current:true
      "approximating result of division. Please report if it matters.";
      top

let div x y =
  (*if (intersects y negative || intersects x negative) then ignore
    (CilE.warn_once "using 'round towards zero' semantics for '/',
    which only became specified in C99."); *)
  match y with
    Set sy ->
      div_set x sy
  | Top (Some mn,Some mx, r, modu) ->
      let result_pos =
        if Int.gt mx Int.zero
        then
          let lpos =
            if Int.gt mn Int.zero
            then mn
            else
              Int.round_up_to_r ~min:Int.one ~r ~modu
          in
          div_range x lpos mx
        else
          bottom
      in
      let result_neg =
        if Int.lt mn Int.zero
        then
          let gneg =
            if Int.lt mx Int.zero
            then mx
            else
              Int.round_down_to_r ~max:Int.minus_one ~r ~modu
          in
          div_range x mn gneg
        else
          bottom
      in
      join result_neg result_pos
  | Top _ | Float _->
      Kernel.warning ~once:true ~current:true
        "approximating result of division. Please report if it matters.";
      top


(* [scale_rem ~pos:false f v] is an over-approximation of the set of
   elements [x mod f] for [x] in [v].

   [scale_rem ~pos:true f v] is an over-approximation of the set of
   elements [x pos_rem f] for [x] in [v].
*)
let scale_rem ~pos f v =
(*     Format.printf "scale_rem %b %a %a@."
    pos
    Int.pretty f
    pretty v; *)
  if Int.is_zero f then bottom
  else
    let f = if Int.lt f Int.zero then Int.neg f else f in
    let rem_f a =
      if pos then Int.pos_rem a f else Int.c_rem a f
    in
    match v with
    | Top(mn,mx,r,m) ->
        let modu = Int.pgcd f m in
        let rr = Int.pos_rem r modu in
        let binf,bsup =
          if pos
          then (Int.round_up_to_r ~min:Int.zero ~r:rr ~modu),
          (Int.round_down_to_r ~max:(Int.pred f) ~r:rr ~modu)
          else
            let min =
              if all_positives mn then Int.zero else Int.neg (Int.pred f)
            in
            let max =
              if all_negatives mx then Int.zero else Int.pred f
            in
            (Int.round_up_to_r ~min ~r:rr ~modu,
            Int.round_down_to_r ~max ~r:rr ~modu)
        in
        let mn_rem,mx_rem =
          match mn,mx with
          | Some mn,Some mx ->
            let div_f a =
              if pos then Int.pos_div a f else Int.c_div a f
            in
            (* See if [mn..mx] is included in [k*f..(k+1)*f] for some [k]. In
               this case, [%] is monotonic and [mn%f .. mx%f] is a more precise
               result. *)
            if Int.equal (div_f mn) (div_f mx) then
              rem_f mn, rem_f mx
            else binf,bsup
          | _ -> binf,bsup
        in
        inject_top (Some mn_rem) (Some mx_rem) rr modu
    | Set s -> map_set_exnsafe rem_f s
    | Float _ -> top


let c_rem x y =
  match y with
  | Top (None, _, _, _) | Top (_, None, _, _)
  | Float _ -> top

  | Top (Some mn, Some mx, _, _) ->
      if Int.equal mx Int.zero then
        bottom (* completely undefined. *)
      else
        (* Result is of the sign of x. Also, compute |x| to bound the result *)
        let neg, pos, max_x = match x with
          | Float _ -> true, true, None
          | Set set ->
              let s = Array.length set in
              if s = 0 then (* Bottom *) false, false, None
              else
                Int.le set.(0) Int.minus_one,
                Int.ge set.(s-1) Int.one,
                Some (Int.max (Int.abs set.(0)) (Int.abs set.(s-1)))
          | Top (mn, mx, _, _) ->
              compare_elt_min Int.minus_one mn,
              compare_elt_max Int.one mx,
              (match mn, mx with
                | Some mn, Some mx -> Some (Int.max (Int.abs mn) (Int.abs mx))
                | _ -> None)
        in
        (* Bound the result: no more than |x|, and no more than |y|-1 *)
        let pos_rem = Integer.max (Int.abs mn) (Int.abs mx) in
        let bound = Int.pred pos_rem in
        let bound = Extlib.may_map (Int.min bound) ~dft:bound max_x in
        (* Compute result bounds using sign information *)
        let mn = if neg then Some (Int.neg bound) else Some Int.zero in
        let mx = if pos then Some bound else Some Int.zero in
        inject_top  mn mx Int.zero Int.one

  | Set yy ->
      ( match x with
        Set xx -> apply2_notzero Int.c_rem xx yy
      | Float _ -> top
      | Top _ ->
          let f acc y =
            join (scale_rem ~pos:false y x) acc
          in
          Array.fold_left f bottom yy)

module AllValueHashtbl =
  Hashtbl.Make
    (struct
      type t = Int.t * bool * int
      let equal (a,b,c:t) (d,e,f:t) = b=e && c=f && Int.equal a d
      let hash (a,b,c:t) = 
	257 * (Hashtbl.hash b) + 17 * (Hashtbl.hash c) + Int.hash a
    end)

let all_values_table = AllValueHashtbl.create 7

let create_all_values ~modu ~signed ~size =
  let t = modu, signed, size in
  try
     AllValueHashtbl.find all_values_table t
  with Not_found ->
    let mn, mx =
      if signed then
        let b = Int.two_power_of_int (size-1) in
        (Int.round_up_to_r ~min:(Int.neg b) ~modu ~r:Int.zero,
        Int.round_down_to_r ~max:(Int.pred b) ~modu ~r:Int.zero)
      else
        let b = Int.two_power_of_int size in
        Int.zero,
        Int.round_down_to_r ~max:(Int.pred b) ~modu ~r:Int.zero
    in
    let r = inject_top (Some mn) (Some mx) Int.zero modu in
    AllValueHashtbl.add all_values_table t r;
    r

let big_int_64 = Int.of_int 64
let big_int_32 = Int.thirtytwo

let cast ~size ~signed ~value =
  if equal top value
  then create_all_values ~size:(Int.to_int size) ~signed ~modu:Int.one
  else
  let result =
    let factor = Int.two_power size in
    let mask = Int.two_power (Int.pred size) in
    let rem_f value = Int.cast ~size ~signed ~value
    in
    let not_p_factor = Int.neg factor in
    let best_effort r m =
      let modu = Int.pgcd factor m in
      let rr = Int.pos_rem r modu in
      let min_val = Some (if signed then
        Int.round_up_to_r ~min:(Int.neg mask) ~r:rr ~modu
      else
        Int.round_up_to_r ~min:Int.zero ~r:rr ~modu)
      in
      let max_val = Some (if signed then
        Int.round_down_to_r ~max:(Int.pred mask) ~r:rr ~modu
      else
        Int.round_down_to_r ~max:(Int.pred factor)
          ~r:rr
          ~modu)
      in
      inject_top min_val max_val rr modu
    in
    match value with
    | Top(Some mn,Some mx,r,m) ->
        let highbits_mn,highbits_mx =
          if signed then
            Int.logand (Int.add mn mask) not_p_factor,
            Int.logand (Int.add mx mask) not_p_factor
          else
            Int.logand mn not_p_factor, Int.logand mx not_p_factor
        in
        if Int.equal highbits_mn highbits_mx
        then
          if Int.is_zero highbits_mn
          then value
          else
            let new_min = rem_f mn in
            let new_r = Int.pos_rem new_min m in
            inject_top (Some new_min) (Some (rem_f mx)) new_r m
        else best_effort r m
    | Top (_,_,r,m) ->
        best_effort r m
    | Set s -> map_set_exnsafe rem_f s
    | Float f ->
      let low, high = 
	if Int.equal size big_int_64
	then
          let l, h = Float_abstract.bits_of_float64 ~signed f in
          Some l, Some h
	else
          if Int.equal size big_int_32
          then
            let l, h = Float_abstract.bits_of_float32 ~signed f in
            Some l, Some h
          else None, None
      in
      inject_range low high
  in
(*  Format.printf "Cast with size:%d signed:%b to %a@\n"
    size
    signed
    pretty result; *)
  if equal result value then value else result

let top_single_precision_float = Float Float_abstract.top_single_precision_float

let cast_float ~rounding_mode v =
  match v with
  | Float f ->
      ( try
	  let b, f = 
	    Float_abstract.round_to_single_precision_float ~rounding_mode f
	  in
	  b, inject_float f
	with Float_abstract.Bottom -> true, bottom)
  | Set _ when is_zero v -> false, zero
  | Set _ | Top _ ->
      true, top_single_precision_float

let cast_double v =
  match v with
  | Float _ -> false, v
  | Set _ when is_zero v -> false, v
  | Set _ | Top _ -> true, top_float


    (* TODO rename to mul_int *)
let rec mul v1 v2 =
  (*    Format.printf "mul. Args: '%a' '%a'@\n" pretty v1 pretty v2; *)
  let result =
    if is_one v1 then v2 
    else if is_zero v2 || is_zero v1 then zero
    else if is_one v2 then v1 
    else
      match v1,v2 with
      | Float _, _ | _, Float _ ->
          top
      | Set s1, Set [| x |] | Set [| x |], Set s1 ->
	    if Int.ge x Int.zero 
	    then apply_bin_1_strict_incr Int.mul x s1
	    else apply_bin_1_strict_decr Int.mul x s1
      | Set s1, Set s2 ->
          apply2_n Int.mul s1 s2
      | Top(mn1,mx1,r1,m1), Top(mn2,mx2,r2,m2) ->
          assert (check mn1 mx1 r1 m1);
          assert (check mn2 mx2 r2 m2);
          let mn1 = inject_min mn1 in
          let mx1 = inject_max mx1 in
          let mn2 = inject_min mn2 in
          let mx2 = inject_max mx2 in
          let a = ext_mul mn1 mn2 in
          let b = ext_mul mn1 mx2 in
          let c = ext_mul mx1 mn2 in
          let d = ext_mul mx1 mx2 in

          let min = ext_min (ext_min a b) (ext_min c d) in
          let max = ext_max (ext_max a b) (ext_max c d) in

          (*      let multipl1 = Int.pgcd m1 r1 in
		  let multipl2 = Int.pgcd m2 r2 in
		  let modu1 = Int.pgcd m1 m2 in
		  let modu2 = Int.mul multipl1 multipl2 in
		  let modu = Int.ppcm modu1 modu2 in    *)
          let modu = Int.(pgcd (pgcd (mul m1 m2) (mul r1 m2)) (mul r2 m1))
          in
          let r = Int.rem (Int.mul r1 r2) modu in
          (*      let t = Top (ext_proj min, ext_proj max, r, modu) in
		  Format.printf "mul. Result: '%a'@\n" pretty t; *)
          inject_top (ext_proj min) (ext_proj max) r modu
      | Set s, (Top(_,_,_,_) as t) | (Top(_,_,_,_) as t), Set s ->
	  let l = Array.length s in
	  if l = 0 
          then bottom
          else if l = 1
          then (* only one element *)
            scale s.(0) t
          else mul t (unsafe_make_top_from_array s)
  in
  (* Format.printf "mul. result : %a@\n" pretty result;*)
  result

let shift_left ~size x y =
  try
    let min = smallest_above Int.zero y in
    let min = Int.two_power min in
    let max = match size with
      | None ->
          (match max_int y with Some v -> v | None -> raise No_such_element)
      | Some size -> largest_below (Int.pred size) y
    in
    let max = Int.two_power max in
    let factor = inject_top (Some min) (Some max) Int.zero min in
    (*      Format.printf "shift_left %a factor:%a@." pretty y pretty factor; *)
    mul factor x
  with No_such_element ->
    bottom

let shift_right ~size x y =
  let result =
    try
      let min = smallest_above Int.zero y in
      let max = match size with
        | None ->
            (match max_int y with Some v -> v | None -> raise No_such_element)
        | Some size -> largest_below (Int.pred size) y
      in
      Int.fold
        (fun n acc -> join acc (scale_div ~pos:true (Int.two_power n) x))
        ~inf:min ~sup:max ~step:Int.one
        bottom
    with No_such_element ->
      bottom
  in
  (*      Format.printf "shift_right %a >> %a -> %a@."
          pretty x pretty y pretty result; *)
  result

let interp_boolean ~contains_zero ~contains_non_zero =
  match contains_zero, contains_non_zero with
  | true, true -> zero_or_one
  | true, false -> singleton_zero
  | false, true -> singleton_one
  | false, false -> bottom

let filter_le_int max v =
  match v with
  | Float _ -> v
  | Set _ | Top _ ->
      narrow v (Top(None,max,Int.zero,Int.one))

let filter_ge_int min v =
  match v with
  | Float _ -> v
  | Set _ | Top _ ->
      narrow v (Top(min,None,Int.zero,Int.one))

let filter_lt_int max v = filter_le_int (opt1 Int.pred max) v
let filter_gt_int min v = filter_ge_int (opt1 Int.succ min) v

let filter_le v1 v2 = filter_le_int (max_int v2) v1
let filter_ge v1 v2 = filter_ge_int (min_int v2) v1
let filter_lt v1 v2 = filter_lt_int (max_int v2) v1
let filter_gt v1 v2 = filter_gt_int (min_int v2) v1

let filter_float filter v1 v2 =
  try
    let f1 = project_float v1 in
    let f2 = project_float v2 in
    inject_float (filter f1 f2)
  with
    Float_abstract.Nan_or_infinite -> v1
  | Float_abstract.Bottom -> bottom

let filter_le_float allmodes ~typ_loc =
  filter_float (Float_abstract.filter_le allmodes ~typ_loc)
let filter_ge_float allmodes ~typ_loc = 
  filter_float (Float_abstract.filter_ge allmodes ~typ_loc)
let filter_lt_float allmodes ~typ_loc =
  filter_float (Float_abstract.filter_lt allmodes ~typ_loc)
let filter_gt_float allmodes ~typ_loc =
  filter_float (Float_abstract.filter_gt allmodes ~typ_loc)

let diff _ _ = assert false

let diff_if_one value rem = 
  match rem, value with
    Set [| v |], Set a ->
      let index = array_mem v a in
      if index >= 0
      then
	let l = Array.length a in
	let pl = pred l in
	let r = Array.make pl Int.zero in
	Array.blit a 0 r 0 index;
	Array.blit a (succ index) r index (pl-index);
	share_array r pl
      else value
  | Set [| v |], Top (Some mn, mx, r, m) when Int.equal v mn ->
      inject_top (Some (Int.add mn m)) mx r m
  | Set [| v |], Top (mn, Some mx, r, m) when Int.equal v mx ->
      inject_top mn (Some (Int.sub mx m)) r m
  | Set [| v |], Top ((Some mn as min), (Some mx as max), r, m) when 
	Int.equal (Int.sub mx mn) (Int.mul m !small_cardinal_Int) &&
	  in_interval v min max r m ->
      let r = ref mn in
      Set 
	(Array.init
	    !small_cardinal
	    (fun _ -> 
	      let c = !r in
	      let corrected_c = 
		if Int.equal c v then Int.add c m else c
	      in
	      r := Int.add corrected_c m;
	      corrected_c))
	      
  | _ -> value (* TODO: more cases: Float *)

let rec extract_bits ~start ~stop ~size v =
  match v with
  | Set s ->
      inject_ps
        (Array.fold_left
           (fun acc elt -> add_ps acc (Int.extract_bits ~start ~stop elt))
	   empty_ps
	   s)
  | Float f ->
      let l, u =
	if Int.equal size big_int_64
	then 
	  Float_abstract.bits_of_float64 ~signed:true f 
	else 
	  Float_abstract.bits_of_float32 ~signed:true f 
      in
      extract_bits ~start ~stop ~size (inject_range (Some l) (Some u))	    
  | Top(_,_,_,_) as d ->
      let dived = scale_div ~pos:true (Int.two_power start) d in
      let rem =
	scale_rem ~pos:true (Int.two_power (Int.length start stop)) dived in
      (* Kernel.feedback "initial: %a results: %a " pretty d pretty rem; *)
      rem
;;

let all_values ~size v =
  if Int.lt big_int_64 size then false
      (* values of this size cannot be enumerated anyway in C.
         They may occur while initializing large blocks of arrays.
       *)
  else
    match v with
    | Float _ -> false
    | Top (None,_,_,modu) | Top (_,None,_,modu) ->
        Int.is_one modu
    | Top (Some mn, Some mx,_,modu) ->
        Int.is_one modu &&
          Int.le
          (Int.two_power size)
          (Int.length mn mx)
    | Set s ->
	let siz = Int.to_int size in
	Array.length s >= 1 lsl siz &&
          equal
          (cast ~size ~signed:false ~value:v)
	  (create_all_values ~size:siz ~signed:false ~modu:Int.one)


let compare_C f v1 v2 =
  let min1 = min_int v1 in
  let max1 = max_int v1 in
  let min2 = min_int v2 in
  let max2 = max_int v2 in
  f min1 max1 min2 max2

include Datatype.Make_with_collections
    (struct
      type t = tt
      let name = Int.name ^ " lattice_mod"
      open Structural_descr
      let structural_descr =
        let s_int = Descr.str Int.descr in
        t_sum
          [|
            [| pack (t_array s_int) |];
            [| Float_abstract.packed_descr |];
            [| pack (t_option s_int);
               pack (t_option s_int);
               Int.packed_descr;
               Int.packed_descr |]
          |]
      let reprs = [ top ; bottom ]
      let equal = equal
      let compare = compare
      let hash = hash
      let pretty = pretty
      let rehash x = 
	match x with
	| Set a -> share_array a (Array.length a)
	| _ -> x
      let internal_pretty_code = Datatype.pp_fail
      let mem_project = Datatype.never_any_project
      let copy = Datatype.undefined
      let varname = Datatype.undefined
    end)

let scale_int_base factor v = match factor with
  | Int_Base.Top -> top
  | Int_Base.Value f -> scale f v

let cast_float_to_int ~signed ~size iv =
  let all = create_all_values ~size ~signed ~modu:Int.one in
  try
    let f = project_float iv in
    let Float_abstract.Private_Couple.I(min,max) = f in
    let min_int = Floating_point.truncate_to_integer min in
    let max_int = Floating_point.truncate_to_integer max in
    assert (Int.compare min_int max_int <= 0);
    let r = inject_range (Some min_int) (Some max_int) in
    if is_included r all
    then false, false, r
    else false, true, (narrow r all)
  with
  | Floating_point.Float_Non_representable_as_Int64 ->
      (* raised by Floating_point.truncate_to_integer *)
      false, true, all
  | Float_abstract.Nan_or_infinite -> (* raised by project_float *)
      true, true, all

let cast_float_to_int_inverse ~single_precision i =
  match min_and_max i with
  | Some min, Some max when Int.le (Int.of_int (-16777215)) min &&
                            Int.le max (Int.of_int 16777215) ->
      let minf = 
	if Int.le min Int.zero
	then
	    let r = F.round_up (Int.to_float (Int.pred min)) in
	    if single_precision
	    then begin
		Floating_point.set_round_upward ();
		Floating_point.round_to_single_precision_float r
	      end
	    else r
	else Int.to_float min
      in      
      let maxf = 
	if Int.le Int.zero max 
	then 
	  let r = F.round_down (Int.to_float (Int.succ max)) in
	  if single_precision
	  then begin
	      Floating_point.set_round_downward ();
	      Floating_point.round_to_single_precision_float r
	    end
	  else r
	else Int.to_float max
      in
      Float (Float_abstract.inject minf maxf)
  | _ -> top_float

let of_int i = inject_singleton (Int.of_int i)

let of_int64 i = inject_singleton (Int.of_int64 i)

let negbil = Int.neg Int.billion_one

let cast_int_to_float rounding_mode v =
  match min_and_max v with
    None, _ | _, None -> false (* not ok *), top_float
  | Some min, Some max ->
      ( try
          Floating_point.set_round_nearest_even (); (* PC: Do not even ask *)
          let b = F.of_float (Int.to_float min) in
          let e = F.of_float (Int.to_float max) in
          if rounding_mode = Float_abstract.Nearest_Even
	    || (Int.le negbil min) && (Int.le max Int.billion_one)
	    (* PC: No, really, don't ask *)
          then true (* ok *), inject_float (Float_abstract.inject b e)
          else begin
              let b = F.round_down b
              in
              let e = F.round_up e
              in
              true, inject_float (Float_abstract.inject b e)
            end
        with
          F.Nan_or_infinite | Floating_point.Float_Non_representable_as_Int64 ->
            false, top_float)

let set_bits mn mx =
  match mn, mx with
    Some mn, Some mx ->
      Int.logand (Int.lognot (different_bits mn mx)) mn
  | _ -> Int.zero

let sub_bits x = (* TODO: can be improved *)
  let popcnt = Int.popcount x in
  let rec aux cursor acc =
    if Int.gt cursor x 
    then acc
    else 
      let acc = 
	if Int.is_zero (Int.logand cursor x)
	then acc
	else O.fold (fun e acc -> O.add (Int.logor cursor e) acc) acc acc
      in
      aux (Int.shift_left cursor Int.one) acc
  in
  let o = aux Int.one o_zero in
  let s = 1 lsl popcnt in
  (* assert (O.cardinal o = s); *)
  inject_ps (Pre_set (o, s))

let bitwise_and ~size ~signed v1 v2 =
  if is_bottom v1 || is_bottom v2
  then bottom
  else
    let v1 =
      match v1 with
        Float _ -> create_all_values ~size ~signed ~modu:Int.one
      | _ -> v1
    in
    let v2 =
      match v2 with
        Float _ -> create_all_values ~size ~signed ~modu:Int.one
      | _ -> v2
    in
    match v1, v2 with
      Float _, _ | _, Float _ -> assert false
    | Top _, other | other, Top _ ->
        let half_range = Int.two_power_of_int (pred size) in
        let minint = Int.neg half_range in
        let max_int_v1, max_int_v2 as max_int_v1_v2 = max_int v1, max_int v2 in
        let min_int_v1, min_int_v2 as min_int_v1_v2 = min_int v1, min_int v2 in
        let vmax =
          match max_int_v1_v2 with
            Some maxv1, Some maxv2 ->
              if Int.lt maxv1 Int.zero && Int.lt maxv2 Int.zero
              then begin
                Some (match min_int_v1_v2 with
                  Some minv1, Some minv2 ->
                    pos_max_land minv1 maxv1 minv2 maxv2
                | _ -> assert false)
              end
              else
                let max1 = (* improved min of maxv1 and maxv2*)
                  try
                    let bi1 = smallest_above Int.zero v1 in
                    let bi2 = smallest_above Int.zero v2 in
                    pos_max_land bi1 maxv1 bi2 maxv2
                  with No_such_element -> minint
                in
                let max2 = (* improved min of maxv1 and altmax2*)
                  try
                    let altmax2 =
                      Int.add half_range (largest_below Int.minus_one v2)
                    in
                    let bi1 = smallest_above Int.zero v1 in
                    let bi2 =
                      Int.add half_range (smallest_above minint v2)
                    in
                    pos_max_land bi1 maxv1 bi2 altmax2
                  with No_such_element -> minint
                in
                let max3 = (* improved min of maxv2 and altmax1*)
                  try
                    let altmax1 =
                      Int.add half_range (largest_below Int.minus_one v1)
                    in
                    let bi2 = smallest_above Int.zero v2 in
                    let bi1 =
                      Int.add half_range (smallest_above minint v1)
                    in
                    pos_max_land bi2 maxv2 bi1 altmax1
                  with No_such_element -> minint
                in
 (* Format.printf "bitwise_and v1 %a v2 %a maxv1 %a maxv2 %a \
    max1 max2 max3 %a %a %a@."
                  pretty v1 pretty v2
                  Int.pretty maxv1 Int.pretty maxv2
                  Int.pretty max1 Int.pretty max2 Int.pretty max3; *)
                Some (Int.max max1 (Int.max max2 max3))
          | _ -> None
        in
        let somenegativev1 = intersects v1 negative in
        let somenegativev2 = intersects v2 negative in
        let vmin =
          if somenegativev1 && somenegativev2
          then Some minint
          else if somenegativev1 || somenegativev2
          then some_zero
          else begin
              let bits1 = set_bits min_int_v1 max_int_v1 in
              let bits2 = set_bits min_int_v2 max_int_v2 in
              let min_a = Int.logand bits1 bits2 in
              let min_a =
                if not signed
                then
                  let rec find_mask x bit acc =
                    if Int.is_zero (Int.logand x bit)
                    then acc
                    else
                      find_mask
                        x
                        (Int.shift_right bit Int.one)
                        (Int.logor bit acc)
                  in
                  match min_int_v1_v2 with
                    Some m1, Some m2 ->
                      let mask1 = find_mask bits1 half_range Int.zero in
                      let min_b = Int.logand mask1 m2 in
                      let mask2 = find_mask bits2 half_range Int.zero in
                      let min_c = Int.logand mask2 m1 in
(*                    Format.printf
                        "bitwise_and v1 %a v2 %a min_b %a min_c %a@."
                        pretty v1 pretty v2
                        Int.pretty min_b Int.pretty min_c; *)
                      Int.max (Int.max min_a min_b) min_c
                  | _ -> assert false
                else min_a
              in
(*            Format.printf "bitwise_and v1 %a v2 %a bits1 %a bits2 %a@."
                pretty v1 pretty v2
                Int.pretty bits1 Int.pretty bits2; *)
              Some min_a
            end
        in
        let result = inject_top vmin vmax Int.zero Int.one in
	( match other with
	  Top _ | Float _ -> result
	| Set s ->
	    if 
	      array_for_all
		(fun elt -> 
		  Int.ge elt Int.zero && 
		    Int.popcount elt <= !small_cardinal_log) 
		s 
	    then
	      let result2 = 
		Array.fold_left
		  (fun acc elt ->
		    join 
		      (sub_bits elt)
		      acc)
		  bottom
		  s
	      in
	      narrow result result2
	    else result)
    | Set s1, Set s2 ->
        apply2_v Int.logand s1 s2

let pretty_debug = pretty
let name = "ival"

(*
Local Variables:
compile-command: "make -C ../.."
End:
*)
