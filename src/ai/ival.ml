(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2008                                               *)
(*    CEA (Commissariat à l'Énergie Atomique)                             *)
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

module N = struct type t = int let n = 7 end

module F = struct
  type t = float
  type integer = Int.t
  exception Nan_or_infinite

  let max_float = max_float

  let wrap f x =
    let r = f x in
    match classify_float r with
      FP_nan | FP_infinite -> raise Nan_or_infinite
    | FP_normal | FP_subnormal | FP_zero -> r

  let wrap_bin f x = wrap (f x)

  let add = wrap_bin (+.)
  let sub = wrap_bin (-.)
  let neg = wrap (~-.)
  let mult = wrap_bin ( *.)  
  let div = wrap_bin (/.)

  let le_ieee = ((<=) : float -> float -> bool)
  let lt_ieee = ((<) : float -> float -> bool)

  let zero = 0.0
  let is_zero_ieee = (=) zero

  let minus_zero = -0.0

  let sqrt = wrap sqrt

  let cos = wrap cos

  let minus_one = -1.0
  let one = 1.0
  let ten = 10.
  let m_pi = 3.14159265358979323846264338327950288
  let m_pi_2 = 1.57079632679489661923132169163975144

  let widen_up f =
    if f <= zero then zero
    else if f <= one then one
    else if f <= m_pi_2 then m_pi_2
    else if f <= m_pi then m_pi
    else if f <= ten then ten
    else if f <= 1e10 then 1e10
    else if f <= 1e80 then 1e80
    else max_float
      
  let equal f1 f2 = 
    if f1 = zero && f2 = zero
    then (1. /. f1) = (1. /. f2)
    else f1 = f2

  let is_zero f = f = zero && ((1. /. f) = infinity)

  let le f1 f2 =
    if f1 = zero && f2 = zero
    then (1. /. f1) <= (1. /. f2)
    else f1 <= f2

  let min f1 f2 =
    if le f1 f2 then f1 else f2

  let max f1 f2 =
    if le f1 f2 then f2 else f1

  let compare f1 f2 =
    let e1 = is_zero f1 in
    let e2 = is_zero f2 in
    match e1, e2 with
      true, true -> 0
    | true, false -> 1
    | false, true -> -1
    | false, false -> compare f1 f2

  let equal_ieee = ((=) : float -> float -> bool)
  let pretty fmt f = Format.fprintf fmt "%F"  f
(*  let compare = Pervasives.compare *)
  let hash = Hashtbl.hash

  let of_float = wrap (fun x -> x)

  let min_64_float = Int64.to_float (Int64.add Int64.min_int 512L)
  let max_64_float = Int64.to_float (Int64.sub Int64.max_int 512L)
  exception Non_representable_float
  let to_integer x = 
    if min_64_float <= x && x <= max_64_float then
      Int.of_int64 (Int64.of_float x)
    else
      raise Non_representable_float
end

module Float_abstract =
struct
  type t = F.t * F.t

  let min = ()
  let max = ()
  type integer = Int.t

  let most_negative_float = -. F.max_float
  let top = most_negative_float, F.max_float

  exception Bottom

  exception Nan_or_infinite = F.Nan_or_infinite

  let compare (b1,e1) (b2,e2) =
    let r = F.compare b1 b2 in
    if r <> 0 then r else F.compare e1 e2

  let pretty fmt (b,e) =
    if F.equal b e then
      Format.fprintf fmt "%a" F.pretty b
    else
      Format.fprintf fmt "[%a .. %a]"
	F.pretty b
	F.pretty e

  let hash (b, e) =
    Hashtbl.hash b + (5 * Hashtbl.hash e)

  let zero = F.zero, F.zero

  let inject b e =
    assert (F.le b e);
    b, e

  let is_included (b1, e1) (b2, e2) =
    F.le b2 b1 && F.le e1 e2

  let join (b1, e1) (b2, e2) =
    (F.min b1 b2, F.max e1 e2)

  let meet (b1, e1) (b2, e2) =
    (F.max b1 b2, F.min e1 e2)

  let contains_zero = is_included zero

  let is_zero x = compare x zero = 0

  let is_singleton (b, e) = F.equal b e

  let neg_float v =
    let b, e =  v in
    inject (F.neg e) (F.neg b)
      
  let add_float v1 v2 =
    let b1, e1 =  v1 in
    let b2, e2 =  v2 in
    inject (F.add b1 b2) (F.add e1 e2)
	
  let sub_float v1 v2 =
    let b1, e1 =  v1 in
    let b2, e2 =  v2 in
    inject (F.sub b1 e2) (F.sub e1 b2)
	
  let mult_float v1 v2 =
    let b1, e1 =  v1 in
    let b2, e2 =  v2 in
    let a = F.mult b1 b2 in
    let b = F.mult b1 e2 in
    let c = F.mult e1 b2 in
    let d = F.mult e1 e2 in
    let min = F.min (F.min a b) (F.min c d) in
    let max = F.max (F.max a b) (F.max c d) in
    inject min max
	  
  let div_float v1 v2 =
    if contains_zero v2
    then begin
	raise Nan_or_infinite
      end
    else begin
	if is_zero v1
	then zero
	else
	  match v1, v2 with
	    (b1, e1),  (b2, e2) ->
	      let c1 = F.div b1 b2 in
	      let c2 = F.div b1 e2 in
	      let c3 = F.div e1 b2 in
	      let c4 = F.div e1 e2 in
	      let min = F.min (F.min c1 c2) (F.min c3 c4) in
	      let max = F.max (F.max c1 c2) (F.max c3 c4) in
	      inject min max
      end
      
  let sqrt_float v =
    if is_zero v then zero else
      match v with
	 (b, e) ->
	  if not (F.le_ieee F.zero b)
	  then begin
	      ignore (CilE.warn_once "sqrt: TODO -- a proper alarm");
	      if F.le_ieee F.zero e
	      then F.minus_zero, (F.sqrt e) 
	    (* sqrt(-0.0) results in -0.0 on at least one implementation *)
	      else raise Bottom
	    end
	  else (F.sqrt b), (F.sqrt e)

  let minus_one_one =  (F.minus_one, F.one)
    
  let cos_float v =
    if is_zero v then zero else
      match v with
	 (b, e) when F.equal b e ->
	  let c = F.cos b in
	  c, c
      | _ ->
	  minus_one_one

  let widen (b1,e1) (b2, e2) =
    assert (F.le b2 b1);
    assert (F.le e1 e2);
    let b = if F.equal b2 b1 then b2 else most_negative_float in
    let e = if F.equal e2 e1 then e2 else F.widen_up e2 in
    b, e

  let equal_float_ieee f1 f2 =
      let b1, e1 =  f1 in
      let b2, e2 =  f2 in
      let intersects = 
	F.le_ieee b1 e2 && F.le_ieee b2 e1
      in 
      if not intersects
      then true, false
      else if F.equal_ieee b1 e1 && F.equal_ieee b2 e2
      then false, true
      else true, true
      
  let maybe_le_ieee_float f1 f2 =
      let b1, _e1 =  f1 in
      let _b2, e2 =  f2 in
      F.le_ieee b1 e2
      
  let maybe_lt_ieee_float f1 f2 =
      let b1, _e1 =  f1 in
      let _b2, e2 =  f2 in
      F.lt_ieee b1 e2

  let diff (b1, e1 as f1) (b2, e2) =
    if F.le b2 b1 && F.le e1 e2
    then raise Bottom
    else if F.le b2 e1 && F.le e1 e2
    then b1, b2
    else if F.le b1 e2 && F.le b2 b1
    then e2, e1
    else f1

  let filter_le (b1, e1 as f1) (_b2, e2) =
    let e2 = if F.equal_ieee F.zero e2 then F.zero else e2 in
    if not (F.le b1 e2)
    then raise Bottom
    else if F.le e1 e2 
    then f1
    else b1, e2

  let filter_ge (b1, e1 as f1) (b2, e2) =
    let b2 = if F.equal_ieee F.minus_zero e2 then F.minus_zero else b2 in
    if not (F.le b2 e1)
    then raise Bottom
    else if F.le b2 b1 
    then f1
    else b2, e1

  let filter_lt = filter_le (* some float domains may want to improve this *)
  let filter_gt = filter_ge

end

include Make_Lattice_Mod(Int)(N)(Float_abstract)

let scale_int64base factor v = 
  match factor with
  | Int_Base.Bottom -> bottom
  | Int_Base.Top -> top
  | Int_Base.Value f -> scale f v

let n = N.n

let cast_float_to_int f =
  try 
    let min,max = f in
    let min_int = F.to_integer min in
    let max_int = F.to_integer max in
    assert (Int.compare min_int max_int <= 0);
    inject_range (Some min_int) (Some max_int)
  with F.Non_representable_float -> top

let of_int i = inject_singleton (Int.of_int i)

let of_int64 i = inject_singleton (Int.of_int64 i)

let cast_int_to_float v =
  match min_and_max v with
    None, _ | _, None -> top
  | Some min, Some max ->
      inject_float (Float_abstract.inject (F.of_float (Int.to_float min)) (F.of_float (Int.to_float max)))

let cast ~size ~signed ~value = 
  if Cmdline.IgnoreOverflow.get () then value else cast ~size ~signed ~value

let tag = hash
let pretty_debug = pretty
let name = "ival" 

(*let pretty fmt x = 
  Format.fprintf fmt "%a ((%d))@." pretty x (tag x)*)
