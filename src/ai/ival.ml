(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2009                                               *)
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

(** Arithmetic lattices.
    @plugin development guide *)

open Abstract_interp

exception Can_not_subdiv

let small_cardinal = 7
let small_cardinal_Int = Int.of_int small_cardinal

module F = struct
  type t = float
  type integer = Int.t
  exception Nan_or_infinite
  exception Too_small

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

  let avg x y =
    let xp = x >= 0. in
    let yp = y >= 0. in
    if xp = yp
    then 
      let d = x -. y in y +. d /. 2.
    else
      (x +. y) /. 2.

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

  let of_int = float_of_int
    
  let widen_up f =
    if f <= zero then zero
    else if f <= one then one
    else if f <= m_pi_2 then m_pi_2
    else if f <= m_pi then m_pi
    else if f <= ten then ten
    else if f <= 1e10 then 1e10
    else if f <= 1e80 then 1e80
    else max_float

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

(*
  let round_up = round Int64.succ Int64.pred

  let round_down = round Int64.pred Int64.succ
*)
  let round_up f = f
  let round_down f = f

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
(*
  let compare f1 f2 =
    let e1 = is_zero f1 in
    let e2 = is_zero f2 in
    match e1, e2 with
      true, true -> 0
    | true, false -> 1
    | false, true -> -1
    | false, false -> compare f1 f2
*)

      let compare f1 f2 =Pervasives.compare f1 f2

  let equal_ieee = ((=) : float -> float -> bool)

  let pretty fmt f = 
    let r = Format.sprintf "%.*g" (Parameters.FloatDigits.get()) f in
    if (String.contains r '.' || String.contains r 'e' || String.contains r 'E')
      || (match classify_float f with 
          | FP_normal | FP_subnormal | FP_zero -> false 
          | FP_infinite | FP_nan -> true) 
    then Format.pp_print_string fmt r
    else Format.fprintf fmt "%s." r

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

  let most_negative_float = -. max_float
end

module Float_abstract =
struct
  type t = F.t * F.t

  let min = ()
  let max = ()
  type integer = Int.t

  let top = F.most_negative_float, F.max_float

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

  let compare_min (m1,_) (m2,_) = F.compare m1 m2

  let compare_max (_, m1) (_, m2) = F.compare m2 m1

  let rounding_inject b e = 
    assert (F.le b e);
    F.round_down b, F.round_up e

  let is_included (b1, e1) (b2, e2) =
    F.le b2 b1 && F.le e1 e2

  let join (b1, e1) (b2, e2) =
    (F.min b1 b2, F.max e1 e2)

  let meet (b1, e1) (b2, e2) =
    (F.max b1 b2, F.min e1 e2)

  let contains_zero = is_included zero

  let fold_split n f (b, e) acc =
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

  let contains_a_zero (b, e) = F.le_ieee b F.zero && F.le_ieee F.zero e

  let is_zero x = compare x zero = 0

  let is_singleton (b, e) = F.equal b e

  let neg_float v =
    let b, e =  v in
    inject (F.neg e) (F.neg b) (* do not round because exact operation *)
      
  let add_float v1 v2 =
    let b1, e1 =  v1 in
    let b2, e2 =  v2 in
    let bs = F.add b1 b2 in
    let b = 
      if F.le_ieee F.zero b1 && F.equal_ieee b2 bs
      then (if F.equal_ieee b2 F.zero then F.minus_zero else b2) 
      else if F.le_ieee F.zero b2 && F.equal_ieee b1 bs
      then b1
      else F.round_down bs
    in    
    let es = F.add e1 e2 in
    let e = 
      if F.le_ieee e1 F.zero  && F.equal_ieee e2 es
      then (if F.equal_ieee e2 F.zero then F.zero else e2) 
      else if F.le_ieee e2 F.zero && F.equal_ieee e1 es
      then e1
      else F.round_up es
    in    
    inject b e
 (*   the whole add_float function used to be only one call
      to rounding_inject, but the +-1ulp was causing problems when
      one term should have been absorbed by the other.
*)	

  let sub_float v1 v2 = add_float v1 (neg_float v2)
	
  let mult_float v1 v2 =
    let b1, e1 =  v1 in
    let b2, e2 =  v2 in
    let a = F.mult b1 b2 in
    let b = F.mult b1 e2 in
    let c = F.mult e1 b2 in
    let d = F.mult e1 e2 in
    let min = F.min (F.min a b) (F.min c d) in
    let max = F.max (F.max a b) (F.max c d) in
    rounding_inject min max
	  
  let div_float v1 v2 =
    if contains_a_zero v2
    then begin
	raise Nan_or_infinite
      end
    else begin
	  match v1, v2 with
	    (b1, e1),  (b2, e2) ->
	      let c1 = F.div b1 b2 in
	      let c2 = F.div b1 e2 in
	      let c3 = F.div e1 b2 in
	      let c4 = F.div e1 e2 in
	      let min = F.min (F.min c1 c2) (F.min c3 c4) in
	      let max = F.max (F.max c1 c2) (F.max c3 c4) in
	      rounding_inject min max
      end
      
  let sqrt_float v =
      match v with
	 (b, e) ->
	  if not (F.le_ieee F.zero b)
	  then begin
	      ignore (CilE.warn_once "sqrt: TODO -- a proper alarm");
	      if F.le_ieee F.zero e
	      then F.minus_zero, (F.sqrt e) 
	    (* sqrt(-0.0) results in -0.0 *)
	      else raise Bottom
	    end
	  else (F.sqrt b), (F.sqrt e)

  let minus_one_one =  (F.minus_one, F.one)
    
  let cos_float v =
      match v with
	 (b, e) when F.equal b e ->
	  let c = F.cos b in
	  c, c
      | _ ->
	  minus_one_one

  let widen (b1,e1) (b2, e2) =
    assert (F.le b2 b1);
    assert (F.le e1 e2);
    let b = if F.equal b2 b1 then b2 else F.most_negative_float in
    let e = if F.equal e2 e1 then e2 else F.widen_up e2 in
 (* Format.printf "F.widen %a %a -> %a@." 
  pretty (b1,e1)
  pretty (b2,e2) 
  pretty (b,e); *)
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

  let subdiv_float_interval (l, u) = 
    let midpoint = F.avg l u in
    if F.equal l midpoint || F.equal u midpoint
    then raise Can_not_subdiv;
    (l, midpoint), (midpoint, u)


end

module Widen_Arithmetic_Value_Set =
struct
  module V = Int
  module S = SetWithNearest.Make(V)
  include S

  module Datatype =
    Datatype.Make_Set(S)(struct include Int.Datatype let compare = Int.compare end)

  let pretty fmt s =
    if is_empty s then Format.fprintf fmt "{}"
    else begin
      Format.fprintf fmt "{%a}"
        (fun fmt s ->
	   iter
             (Format.fprintf fmt "%a; " Int.pretty) s) s
    end

  let default_widen_hints =
    List.fold_left
      (fun acc x ->
	add (Int.of_int x) acc)
      empty
      [-128;-1;0;1;3;15;127;512;32767;1 lsl 29]

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
module O = Set.Make(Int)
type tt =
    Set of O.t | Float of Float_abstract.t
  | Top of Int.t option * Int.t option * Int.t * Int.t
type t = tt
type y = O.t

module Widen_Hints = Widen_Arithmetic_Value_Set
type widen_hint = Widen_Hints.t

let some_zero = Some Int.zero

let bottom = Set O.empty
let top = Top(None, None, Int.zero, Int.one)

let hash_v_option v =
  match v with None -> 97 | Some v -> Int.hash v

let hash v =
  match v with
    Set s -> O.fold (fun v acc -> 1031 * acc + (Int.hash v)) s 0
  | Top(mn,mx,r,m) ->
      hash_v_option mn + 5501 * (hash_v_option mx) +
	59 * (Int.hash r) + 13031 * (Int.hash m)
  | Float(f) ->
      3 + 17 * Float_abstract.hash f

let tag = hash

let bound_compare x y =
  match x,y with
    None, None -> 0
  | None, Some _ -> 1
  | Some _, None -> -1
  | Some x, Some y -> Int.compare x y

let compare e1 e2 =
  if e1==e2 then 0 else
  match e1,e2 with
  | Set e1,Set e2 -> O.compare e1 e2
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

let is_singleton (b, e) = b == e

let pretty fmt t =
  match t with
  | Top(mn,mx,r,m) ->
      if equal t top  then
	Format.fprintf fmt "[--..--]"
      else
	Format.fprintf fmt "[%a..%a]%t"
	  (fun fmt ->
	    (function None -> Format.fprintf fmt "--"
	      | Some v -> Int.pretty fmt v))
	  mn
	  (fun fmt ->
	    (function None -> Format.fprintf fmt "--"
	      | Some v -> Int.pretty fmt v))
	  mx
	  (fun fmt ->
	    if Int.is_zero r && Int.is_one m then
	      Format.fprintf fmt ""
	    else Format.fprintf fmt ",%a%%%a"
		Int.pretty r
		Int.pretty m)
  | Float (f) ->
      Float_abstract.pretty fmt f
  | Set s ->
      if O.is_empty s then Format.fprintf fmt "BottomMod"
      else begin
	Format.fprintf fmt "{%a}"
	  (fun fmt s ->
	    O.iter
	      (Format.fprintf fmt "%a; " Int.pretty) s) s
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

let check doc min max r modu =
  assert(assert (Int.ge r Int.zero );
	 assert (Int.ge modu Int.one );
	 (match min with
	 | None -> ()
	 | Some m -> if not (Int.equal (Int.pos_rem m modu) r) then
	     begin
	       ignore (CilE.warn_once "Make_Lattice_Mod.check: '%s'\n" doc);
	       Format.printf "min=%a modu=%a r=%a@." Int.pretty m  Int.pretty modu  Int.pretty r;
	       assert false
	     end);
	 (match max with
	 | None -> ()
	 | Some m -> assert (Int.equal (Int.pos_rem m modu) r));
	 true)

let cardinal_zero_or_one v = match v with
| Top _ -> false
| Set s -> O.cardinal s <= 1
| Float (f) -> Float_abstract.is_singleton f

let is_singleton_int v = match v with
| Float _ | Top _ -> false
| Set s -> O.cardinal s = 1

let is_bottom = equal bottom

let inject_singleton e = Set (O.singleton e)

let zero = inject_singleton Int.zero

let inject_float f =
  if Float_abstract.is_zero f
  then zero
  else Float f

let subdiv_float_interval v =
  match v with
  | Float f ->
      let f1, f2 = Float_abstract.subdiv_float_interval f in
      inject_float f1, inject_float f2
  | Top _ | Set _ -> assert false


(*  let minus_zero = Float (Float_abstract.minus_zero, Float_abstract.minus_zero) *)

let one = inject_singleton Int.one

let is_zero = equal zero

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

let contains_zero s =
  match s with
  | Top(mn,mx,r,m) -> in_interval Int.zero mn mx r m
  | Set s -> O.mem Int.zero s
  | Float f -> Float_abstract.contains_zero f

exception Not_Singleton_Int

let project_int v = match v with
| Set s when O.cardinal s = 1 -> O.min_elt s
| _ -> raise Not_Singleton_Int

let cardinal_less_than v n =
  let c =
    match v with
    | Top (None,_,_,_) | Top (_,None,_,_) -> raise Not_less_than
    | Top (Some mn, Some mx,_,m) ->
	Int.succ ((Int.native_div (Int.sub mx mn) m))
    | Set s -> Int.of_int (O.cardinal s)
    | Float f -> if Float_abstract.is_singleton f then Int.one else raise Not_less_than
  in
  if Int.le c (Int.of_int n)
  then Int.to_int c (* This is smaller than the original [n] *)
  else raise Not_less_than

let splitting_cardinal_less_than ~split_non_enumerable v n =
  let c =
    match v with
    | Top (None,_,_,_) | Top (_,None,_,_) -> raise Not_less_than
    | Top (Some mn, Some mx,_,m) ->
	Int.succ ((Int.native_div (Int.sub mx mn) m))
    | Set s -> Int.of_int (O.cardinal s)
    | Float f -> 
	if Float_abstract.is_singleton f then Int.one 
	else Int.of_int split_non_enumerable
  in
  if Int.le c (Int.of_int n)
  then Int.to_int c
  else raise Not_less_than

let inject_top min max r modu =
  check "inject_top" min max r modu;
  match min, max with
  | Some mn, Some mx ->
      if Int.ge mx mn  then
	if (Int.le (Int.length mn mx) (Int.mul modu small_cardinal_Int)) 
	then
	  let s = ref O.empty in
	  let i = ref mn in
	  while (Int.le !i mx)
	  do
	    s := O.add !i !s;
	    i := Int.add modu !i
	  done;
	  Set (!s)
	else Top (min, max, r, modu)
      else bottom
  | _ -> Top (min, max, r, modu)

let inject_range min max = inject_top min max Int.zero Int.one

let top_float = Float Float_abstract.top

let unsafe_make_top_from_set_4 s =
  assert (O.cardinal s >= 2);
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
  let max = Some(O.max_elt s) in
  let min = Some m in
  check "unsafe_make_top_from_set_4" min max r modu;
  (min,max,r,modu)

let unsafe_make_top_from_set s =
  let (a,b,c,d) = unsafe_make_top_from_set_4 s in
  Top (a,b,c,d)

let min_max_r_mod t =
  match t with
  | Set s ->
      assert (O.cardinal s >= 2);
      unsafe_make_top_from_set_4 s
  | Top (a,b,c,d) -> a,b,c,d
  | Float _ -> None, None, Int.zero, Int.one

let min_and_max t =
  match t with
  | Set s ->
      assert (O.cardinal s >= 1);
      Some (O.min_elt s), Some (O.max_elt s)
  | Top (a,b,_,_) -> a, b
  | Float _ -> None, None

let compare_min_int t1 t2 =
  let m1, _ = min_and_max t1 in
  let m2, _ = min_and_max t2 in
  match m1, m2 with
    None, None -> 0
  | None, Some _ -> 1
  | Some _, None -> -1
  | Some m1, Some m2 ->
      Int.compare m1 m2

let compare_max_int t1 t2 =
  let m1, _ = min_and_max t1 in
  let m2, _ = min_and_max t2 in
  match m1, m2 with
    None, None -> 0
  | None, Some _ -> 1
  | Some _, None -> -1
  | Some m1, Some m2 ->
      Int.compare m1 m2

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
	(*Format.printf "%a -- %a --> %a (thx to %a)@."
	  pretty t1 pretty t2 pretty result
	  Widen_Hints.pretty wh;*)
	result


let inject_set s =
  if (O.cardinal s) <= small_cardinal
  then Set s
  else unsafe_make_top_from_set s

let compute_first_common mn1 mn2 r modu =
  if mn1 = None && mn2 = None
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
  if mx1 = None && mx2 = None
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


let min_max x y =
  match x,y with
  | None,z | z,None -> z
  | Some x, Some y -> Some (Int.min x y)

exception Found of Int.t


let compute_r_common r1 modu1 r2 modu2 =
  let modu = Int.ppcm modu1 modu2 in
  try
    let i = ref Int.zero in (* for i = 0 to modu - 1 *)
    while (Int.le !i (Int.pred modu)) 
    do
      if (Int.equal (Int.rem !i modu1) r1) && (Int.equal (Int.rem !i modu2) r2)
      then raise (Found !i);
      i := Int.succ !i
    done;
    raise Error_Bottom
  with Found i ->
    i, modu



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
    | Set s1 , Set s2 -> Set (O.inter s1 s2)
    | Set s, Top(min, max, r, modu)
    | Top(min, max, r, modu), Set s ->
	Set(O.filter
	      (fun x -> in_interval x min max r modu)
	      s)
    | Float(f1), Float(f2) ->
	inject_float (Float_abstract.meet f1 f2)
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

let narrow v1 v2 = meet v1 v2 (* meet is exact *)

let link _ = assert false

    (** This is NOT exact *)
let join v1 v2 =
  let result =
    if v1 == v2 then v1 else
    match v1,v2 with
    | Top(mn1,mx1,r1,m1), Top(mn2,mx2,r2,m2) ->
	check "join left" mn1 mx1 r1 m1;
	check "join right" mn2 mx2 r2 m2;
	let modu = Int.pgcd (Int.pgcd m1 m2) (Int.abs(Int.sub r1 r2)) in
	let r = Int.rem r1 modu in
	let min = min_min mn1 mn2 in
	let max = max_max mx1 mx2 in
	let r  = inject_top min max r modu in
	r
    | Set s, (Top(min, max, r, modu) as t)
    | (Top(min, max, r, modu) as t), Set s ->
	if O.is_empty s then t
	else
	  let f elt modu = Int.pgcd modu (Int.abs(Int.sub r elt)) in
	  let new_modu = O.fold f s modu in
	  let new_r = Int.rem r new_modu in
	  let new_min = match min with
	    None -> None
	  | Some m -> Some (Int.min m (O.min_elt s))
	  in
	  let new_max = match max with
	    None -> None
	  | Some m -> Some (Int.max m (O.max_elt s))
	  in
	  check "inside join" new_min new_max new_r new_modu;
	  Top(new_min, new_max, new_r, new_modu)
    | Set s1 , Set s2 ->
	let u = O.union s1 s2 in
	inject_set u
    | Float(f1), Float(f2) ->
	inject_float (Float_abstract.join f1 f2)
    | Float (f) as ff, other | other, (Float (f) as ff) ->
	if is_zero other
	then inject_float (Float_abstract.join Float_abstract.zero f)
	else if is_bottom other then ff
	else top
  in
  (*    Format.printf "mod_join %a %a -> %a@."
	pretty v1 pretty v2 pretty result; *)
  result

    (* TODO: rename this function in fold_int *)
let fold f v acc =
  match v with
    Top(None,_,_,_) | Top(_,None,_,_) | Float _ ->
      raise Error_Top
  | Top(Some inf, Some sup, _, step) ->
      Int.fold f ~inf ~sup ~step acc
  | Set s ->
      O.fold f s acc

let fold_enum ~split_non_enumerable f v acc =
  match v with
  | Float (fl) when Float_abstract.is_singleton fl ->
      f v acc
  | Float (fl) ->
      Float_abstract.fold_split 
	split_non_enumerable 
	(fun fl acc -> f (inject_float fl) acc)
	fl
	acc 
  | Top(_,_,_,_) | Set _ ->
      fold (fun x acc -> f (inject_singleton x) acc) v acc




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
  (Int.equal (Int.rem m1 m2) Int.zero) && (Int.equal (Int.rem r1 m2) r2)

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
      O.for_all (fun x -> in_interval x min max r modu) s
  | Set s1, Set s2 -> O.subset s1 s2
  | Float(f1), Float(f2) ->
      Float_abstract.is_included f1 f2
  | Float _, _ -> equal t2 top
  | _, Float (f) -> is_zero t1 && (Float_abstract.contains_zero f)

let is_included_exn v1 v2 =
  if not (is_included v1 v2) then raise Is_not_included

      (* In this lattice, [meet t1 t2=bottom] iff the
	 intersection of [t1] and [t2] is empty. *)
let intersects t1 t2 =
  not (equal bottom (meet t1 t2))

let map_set f s =
  O.fold
    (fun v -> O.add (f v))
    s
    O.empty

let apply2 f s1 s2 =
  O.fold
    (fun v -> O.union (map_set (f v) s2))
    s1
    O.empty

exception Apply_Set_Exn of exn

let apply_set info f v1 v2 =
  match v1,v2 with
  | Set s1, Set s2 ->
      begin try
	let result = try
	  apply2 f s1 s2
	with e -> raise (Apply_Set_Exn e)
	in
	inject_set result
      with
	Apply_Set_Exn(e) ->
	  ignore (CilE.warn_once
		    "binary operator '%s' raised an exception '%s' when applied"
		    info
		    (Printexc.to_string e));
	  top
      end
  | _ ->
      (*ignore (CilE.warn_once "unsupported case for binary operator '%s'" info);*)
      top

let rec apply_set_unary _info f v =
  match v with
  | Set s ->
      inject_set (map_set f s)
  | _ ->
      (*ignore (CilE.warn_once "unsupported case for unary operator '%s'" info);*)
      top

	(* TODO: rename in add_int *)
let rec add v1 v2 =
  if is_zero v1 then v2 else if is_zero v2 then v1 else
  match v1,v2 with
    Float _, _ | _, Float _ -> top
  | Set s1, Set s2 ->
      inject_set (apply2 Int.add s1 s2)
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
  | Set s, (Top(mn1,mx1,r1,m1) as t) | (Top(mn1,mx1,r1,m1) as t), Set s ->
      if O.is_empty s
      then bottom
      else let mn = O.min_elt s in
      let mx = O.max_elt s in
      if Int.equal mn mx 
      then (* only one element *)
	let incr = Int.add mn in
	let new_mn = opt1 incr mn1 in
	let new_mx = opt1 incr mx1 in
	let new_r = Int.pos_rem (incr r1) m1 in
	check "add" new_mn new_mx new_r m1 ;
	Top(new_mn, new_mx, new_r, m1)
      else
	add t (unsafe_make_top_from_set s)

	  (* TODO rename to neg_int *)
let neg v =
  match v with
  | Float _ -> top
  | Set s -> Set (map_set Int.neg s)
  | Top(mn,mx,r,m) ->
      Top(opt1 Int.neg mx, opt1 Int.neg mn, Int.pos_rem (Int.neg r) m, m)

let sub v1 v2 = add v1 (neg v2)

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
  try
    match s with
    | Top (min,_,_,_) -> min
    | Set s -> Some (O.min_elt s)
    | Float _ -> None
  with Not_found -> raise Error_Bottom

let max_int s =
  try
    match s with
    | Top (_,max,_,_) -> max
    | Set s -> Some (O.max_elt s)
    | Float _ -> None
  with Not_found -> raise Error_Bottom

exception No_such_element

let smallest_above min x =
  match x with
  | Set s ->
      let r = ref None in
      O.iter
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
      then Cilutil.out_some mn
      else Int.round_up_to_r ~min ~r ~modu
  | Float _ -> raise No_such_element

let largest_below max x =
  match x with
  | Float _ -> raise No_such_element
  | Set s ->
      let r = ref None in
      O.iter
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
      then Cilutil.out_some mx
      else Int.round_down_to_r ~max ~r ~modu

	  (* [different_bits min min] returns the mask of the bits that can be different
	     for different numbers in the interval [min]..[max] *)
let different_bits min max =
  let x = Int.logxor min max in
  let x = Int.logor x (Int.shift_right x Int.one) in
  let x = Int.logor x (Int.shift_right x Int.two) in
  let rec f old =
    let x = Int.logor old (Int.shift_right old Int.four) in
    if Int.equal old x then x
    else f x
  in
  f x

    (* [pos_max_land min1 max1 min2 max2] computes an upper bound for
       [x1 land x2] where [x1] is in [min1]..[max1] and [x2] is in [min2]..[max2].
       Precondition : [min1], [max1], [min2], [max2] must all have the
       same sign *)
let pos_max_land min1 max1 min2 max2 =
  let x1 = different_bits min1 max1 in
  let x2 = different_bits min2 max2 in
  (*    Format.printf "pos_max_land %a %a -> %a |  %a %a -> %a@."
	Int.pretty min1 Int.pretty max1 Int.pretty x1
	Int.pretty min2 Int.pretty max2 Int.pretty x2;*)
  if Int.lt x1 x2
  then
    (*let diff = Int.sub x2 x1 in*)
    let mask = Int.lognot x2 in
    let forced = Int.logand mask min1 in
    let part = Int.logand forced min2 in
    if Int.equal part forced
    then
      Int.min max1 max2
    else Int.logor part x2
  else
    (*let diff = Int.sub x1 x2 in*)
    let mask = Int.lognot x1 in
    let forced = Int.logand mask min2 in
    let part = Int.logand forced min1 in
    if Int.equal part forced
    then
      Int.min max1 max2
    else Int.logor part x1


let bitwise_and ~size v1 v2 =
  if is_bottom v1 || is_bottom v2
  then bottom
  else
    match v1, v2 with
      Float _, _ | _, Float _ -> top
    | Top _, _ | _, Top _ ->
	let half_range = Int.power_two (pred size) in
	let minint = Int.neg half_range in
	let vmax =
	  match max_int v1, max_int v2 with
	    Some maxv1, Some maxv2 ->
	      if Int.lt maxv1 Int.zero && Int.lt maxv2 Int.zero
	      then begin
		Some (match min_int v1, min_int v2 with
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
		(*		    Format.printf "bitwise and v1 %a v2 %a maxv1 %a maxv2 %a max1 max2 max3 %a %a %a@."
		  pretty v1 pretty v2
		  Int.pretty maxv1 Int.pretty maxv2
		  Int.pretty max1 Int.pretty max2 Int.pretty max3; *)
		Some (Int.max max1 (Int.max max2 max3))
	  | _ -> None
	in
	let vmin =
	  if intersects v1 negative && intersects v2 negative
	  then Some minint
	  else some_zero
	in
	inject_top vmin vmax Int.zero Int.one
    | Set _, Set _ ->
	(apply_set "&" Int.logand) v1 v2

let bitwise_or ~size:_ v1 v2 =
  if is_bottom v1 || is_bottom v2
  then bottom
  else
    match v1, v2 with
      Float _, _ | _, Float _ -> top
    | Top _, _ | _, Top _ -> top
    | Set _, Set _ ->
	(apply_set "|" Int.logor) v1 v2

let contains_non_zero v =
  match v with
  | Top _ | Float _ -> true
  | Set s -> O.exists (fun e -> not (Int.equal Int.zero e)) s

	(* TODO: rename this function to scale_int *)
let scale f v =
  let result =
    match v with
    | Float _ -> top
    | Top(mn1,mx1,r1,m1) ->
	let incr = Int.mul f in
	if Int.is_zero f
	then singleton_zero
	else if Int.gt f Int.zero 
	then
	  let modu = incr m1 in
	  Top(opt1 incr mn1, opt1 incr mx1, Int.pos_rem (incr r1) modu, modu)
	else
	  let modu = Int.neg (incr m1) in
	  Top(opt1 incr mx1, opt1 incr mn1, Int.pos_rem (incr r1) modu, modu)
    | Set s -> Set (map_set (Int.mul f) s)
  in
  (* Format.printf "scale: %a . %a -> %a@\n"
     Int.pretty f pretty v pretty result; *)
  result

let scale_div ~pos f v =
  assert (not (Int.equal f Int.zero));
  let div_f =
    if pos
    then fun a -> Int.pos_div a f
    else fun a -> Int.c_div a f
  in
  match v with
  | Top(mn1,mx1,r1,m1) ->
      let r, modu =
	if (Int.is_zero (Int.rem m1 f)) &&
	  ((Int.is_zero (Int.rem r1 f)) ||
	  (min_is_lower (some_zero) mn1) || (* all positive *)
	  (max_is_greater (some_zero) mx1) || (* all negative *)
	  pos                         (* good div *) )
	then
	  let modu = Int.abs (div_f m1) in
	  (Int.pos_rem (div_f r1) modu), modu
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
  | Set s -> Set (map_set div_f s)
  | Float _ -> top

let div_set x sy = 
  O.fold
    (fun elt acc -> 
      if Int.is_zero elt
      then acc
      else join acc (scale_div ~pos:false elt x))
    sy
    bottom  

(* ymin and ymax must be the same sign *)
let div_range x ymn ymx =
  match min_and_max x with
    Some xmn, Some xmx ->	
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
      CilE.warn_once "approximating result of division. Please report if it matters.";
      top

let div x y =
  let result =
    (*if (intersects y negative || intersects x negative)
      then ignore (CilE.warn_once "using 'round towards zero' semantics for '/', which only became specified in C99."); *)
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
	CilE.warn_once "approximating result of division. Please report if it matters.";
	top

  in
(*    Format.printf "div: %a / %a -> %a@\n"
      pretty x pretty y pretty result; *)
  result

    (* [scale_rem ~pos:false f v] is an over-approximation of the set of
       elements [x mod f] for [x] in [v].

       [scale_rem ~pos:true f v] is an over-approximation of the set of
       elements [x pos_rem f] for [x] in [v].
     *)


	(* TODO : rename to div_int *)
(*let div = 
  if contains_zero y
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
	      let mn_rem = rem_f mn in
	      let mx_rem = rem_f mx in
	      (*   Format.printf "scale_rem 1:%a %a %a %b %b %a %a@."
		   Int.pretty mn Int.pretty mx Int.pretty f
		   (Int.lt mx f) (Int.gt mn (Int.neg f))
		   Int.pretty mn_rem Int.pretty mx_rem;*)
	      if
		((Int.lt (Int.sub mx mn) f) || ((Int.lt mx f) && (Int.gt mn (Int.neg f))))  &&
		  (Int.le mn_rem mx_rem)
	      then
		( (*Format.printf "scale_rem 2:%a %a %a %a@."
		    Int.pretty mn Int.pretty mx Int.pretty mn_rem Int.pretty mx_rem; *)

		  mn_rem,mx_rem)
	      else binf,bsup
	  | _ -> binf,bsup
	in
	inject_top (Some mn_rem) (Some mx_rem) rr modu
    | Set s -> Set (map_set rem_f s)
    | Float _ -> top


let c_rem x y =
  match y with
    Top _ | Float _ -> top
  | Set yy ->
      ( match x with
	Set _ -> apply_set "%" Int.c_rem x y
      | Float _ -> top
      | Top _ ->
	  let f y acc =
	    join (scale_rem ~pos:false y x) acc	
	  in
	  O.fold f yy bottom)


let v32 = Int.of_int 32

let cast ~size ~signed ~value =
  let result =
    let factor = Int.two_power size in
    let mask = Int.two_power (Int.sub size Int.one) in
    let rem_f value = Int.cast ~size ~signed ~value 
    in
    let not_p_factor = Int.lognot (Int.pred factor) in
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
    | Set s -> inject_set (map_set rem_f s)
    | Float _ -> if Int.ge size v32 then value else top
  in  
(*  Format.printf "Cast with size:%d signed:%b to %a@\n"
    size
    signed
    pretty result; *)
  if equal result value then value else result

    (* TODO rename to mul_int *)
let rec mul v1 v2 =
  (*    Format.printf "mul. Args: '%a' '%a'@\n" pretty v1 pretty v2; *)
  let result =
    if is_one v1 then v2 else if is_one v2 then v1 else
    match v1,v2 with
    | Float _, _ | _, Float _ ->
	top
    | Set s1, Set s2 ->
	inject_set (apply2 Int.mul s1 s2)
    | Top(mn1,mx1,r1,m1), Top(mn2,mx2,r2,m2) ->
	check "mul left" mn1 mx1 r1 m1;
	check "mul right" mn2 mx2 r2 m2;
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

	(*	let multipl1 = Int.pgcd m1 r1 in
	  let multipl2 = Int.pgcd m2 r2 in
	  let modu1 = Int.pgcd m1 m2 in
	  let modu2 = Int.mul multipl1 multipl2 in
	  let modu = Int.ppcm modu1 modu2 in	*)
	let modu = Int.pgcd (Int.pgcd (Int.mul m1 m2) (Int.mul r1 m2)) (Int.mul r2 m1)
	in
	let r = Int.rem (Int.mul r1 r2) modu in
	(*	let t = Top (ext_proj min, ext_proj max, r, modu) in
	  Format.printf "mul. Result: '%a'@\n" pretty t; *)
	inject_top (ext_proj min) (ext_proj max) r modu
    | Set s, (Top(_,_,_,_) as t) | (Top(_,_,_,_) as t), Set s ->
	if O.is_empty s
	then bottom
	else let mn = O.min_elt s in
	let mx = O.max_elt s in
	if Int.equal mn mx 
	then (* only one element *)
	  scale mn t
	else mul t (unsafe_make_top_from_set s)
  in
  (* Format.printf "mul. result : %a@\n" pretty result;*)
  result

let shift_left ~size x y =
  try
    let min = smallest_above Int.zero y in
    let min = Int.shift_left Int.one min in
    let max = largest_below (Int.pred size) y in
    let max = Int.shift_left Int.one max in
    let factor = inject_top (Some min) (Some max) Int.zero min in
    (*      Format.printf "shift_left %a factor:%a@." pretty y pretty factor; *)
    mul factor x
  with No_such_element ->
    bottom

let shift_right ~size x y =
  let result =
    try
      let min = smallest_above Int.zero y in
      let max = largest_below (Int.pred size) y in
      Int.fold
	(fun n acc -> join acc (scale_div ~pos:true
				  (Int.shift_left Int.one n) x))
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

(** return the smallest lattice element that contains all elements of [s]
   that are in relation [f] ([<=],[>=],...) to [bound] *)
let filter_set f bound s =
  Set
    (O.fold
       (fun v acc ->
	 if f (Int.compare v bound)
	 then O.add v acc
	 else acc)
       s
       O.empty)

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

let filter_le_float = filter_float Float_abstract.filter_le
let filter_ge_float = filter_float Float_abstract.filter_ge
let filter_lt_float = filter_float Float_abstract.filter_lt
let filter_gt_float = filter_float Float_abstract.filter_gt

let rec diff value rem =
  match value,rem with
  | Set s1, Set s2 ->
      Set (O.diff s1 s2)
  | Set s, Top(min, max, r, modu) ->
      Set(O.filter
	    (fun x -> not (in_interval x min max r modu))
	    s)
  | Top(min, max, r, modu), Set s ->
      let changed = ref false in
      let new_min = match min with
      | Some min when O.mem min s ->
	  changed := true;
	  Some (Int.add min modu)
      | _ -> min
      in
      let new_max = match max with
      | Some max when O.mem max s ->
	  changed := true;
	  Some (Int.sub max modu)
      | _ -> max
      in
      if !changed then
	diff (inject_top new_min new_max r modu) rem
      else value
  | Top(_min1, _max1, _r1, _modu1), Top(_min2, _max2, _r2, _modu2) ->
      value  (* TODO : can do better *)
  | Float f1, Float f2 -> inject_float (Float_abstract.diff f1 f2)
  | Float _ , _ | _, Float _ -> value

let diff_if_one value rem =
  if not (cardinal_zero_or_one rem)  then
    value
  else diff value rem

let extract_bits ~with_alarms ~start ~stop v =
  match v with
  | Set s ->
      Set
	(O.fold
	   (fun elt acc -> O.add (Int.extract_bits ~with_alarms ~start ~stop elt) acc)
	   s
	   O.empty)
  | Top _ | Float _ ->
      inject_top
	some_zero
	(Some (Int.pred (Int.power_two (Int.to_int (Int.length start stop)))))
	Int.zero
	Int.one

let b64 = Int.of_int 64

let create_all_values ~modu ~signed ~size =
  let mn, mx =
    if signed then
      let b = Int.power_two (size-1) in
      (Int.round_up_to_r ~min:(Int.neg b) ~modu ~r:Int.zero,
      Int.round_down_to_r ~max:(Int.pred b) ~modu ~r:Int.zero)
    else
      let b = Int.power_two size in
      Int.zero,
      Int.round_down_to_r ~max:(Int.pred b) ~modu ~r:Int.zero
  in
  Top(Some mn, Some mx, Int.zero, modu)

let all_values ~size v =
  if Int.lt b64 size then false
      (* values of this size cannot be enumerated anyway in C.
	 They may occur while initializing large blocks of arrays.
       *)
  else
    let c =
      match v with
      | Float _ -> false
      | Top (None,_,_,modu) | Top (_,None,_,modu) ->
	  Int.eq Int.one modu
      | Top (Some mn, Some mx,_,modu) ->
	  Int.eq Int.one modu &&
	  Int.le
	    (Int.power_two (Int.to_int size))
	    (Int.succ (Int.sub mx mn))
      | Set _ ->
	  equal
	    (cast ~size ~signed:false ~value:v)
	    (cast ~size ~signed:false ~value:top)
    in
    c

let compare_C f v1 v2 =
  let min1 = min_int v1 in
  let max1 = max_int v1 in
  let min2 = min_int v2 in
  let max2 = max_int v2 in
  f min1 max1 min2 max2

module Datatype =
  Project.Datatype.Register
    (struct
      type t = tt
      let copy _ = assert false (* TODO *)
      let rehash x = match x with
      | Set set ->
	  inject_set
	    (O.fold (fun x -> O.add (Int.Datatype.rehash x)) set O.empty)
      | Float _f -> x (*TODO inject_float (Float_abstract.Datatype.rehash f) *)
      | Top(mn, mx, r, modu) ->
	  inject_top
	    (Extlib.opt_map Int.Datatype.rehash mn)
	    (Extlib.opt_map Int.Datatype.rehash mx)
	    (Int.Datatype.rehash r)
	    (Int.Datatype.rehash modu)
      let descr = Unmarshal.Abstract (* TODO: use Data.descr *)
      let name = Project.Datatype.extend_name "lattice_mod" Int.Datatype.name
    end)
let () = Datatype.register_comparable ~hash ~equal ()

let scale_int64base factor v = 
  match factor with
  | Int_Base.Bottom -> bottom
  | Int_Base.Top -> top
  | Int_Base.Value f -> scale f v

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
      inject_float 
	(Float_abstract.rounding_inject 
	   (F.of_float (Int.to_float min)) 
	   (F.of_float (Int.to_float max)))

let cast ~size ~signed ~value = 
  if Parameters.Overflow.get () then cast ~size ~signed ~value else value

let tag = hash
let pretty_debug = pretty
let name = "ival" 

(*let pretty fmt x = 
  Format.fprintf fmt "%a ((%d))@." pretty x (tag x)*)
