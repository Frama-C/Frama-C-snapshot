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

let emitter = Lattice_messages.register "Ival";;

let log_imprecision s =
  Lattice_messages.emit_imprecision emitter s
;;

module Widen_Arithmetic_Value_Set = struct

  include Datatype.Integer.Set

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
    of_list (List.map Int.of_int [-1;0;1])

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

module O = FCSet.Make(Integer)

type pre_set = 
    Pre_set of O.t * int
    | Pre_top of Int.t * Int.t * Int.t

type t =
  | Set of Int.t array
  | Float of Fval.t
  | Top of Int.t option * Int.t option * Int.t * Int.t
(* Binary abstract operations do not model precisely float/integer operations.
   It is the responsibility of the callers to have two operands of the same
   implicit type. The only exception is for [singleton_zero], which is the
   correct representation of [0.] *)


module Widen_Hints = Widen_Arithmetic_Value_Set
type size_widen_hint = Integer.t
type generic_widen_hint = Widen_Hints.t
type widen_hint = size_widen_hint * generic_widen_hint

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
      3 + 17 * Fval.hash f

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
      Fval.compare f1 f2
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
      Fval.pretty fmt f
  | Set s ->
      if Array.length s = 0 then Format.fprintf fmt "BottomMod"
      else begin
        Pretty_utils.pp_iter
          ~pre:"@[<hov 1>{"
          ~suf:"}@]"
          ~sep:";@ "
          Array.iter Int.pretty fmt s
      end

let min_le_elt min elt =
  match min with
  | None -> true
  | Some m -> Int.le m elt

let max_ge_elt max elt =
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


let fail min max r modu =
  let bound fmt = function
    | None -> Format.fprintf fmt "--"
    | Some(x) -> Int.pretty fmt x
  in
  Kernel.fatal "Ival: broken Top, min=%a max=%a r=%a modu=%a"
    bound min bound max Int.pretty r Int.pretty modu

let is_safe_modulo r modu =
  (Int.ge r Int.zero ) && (Int.ge modu Int.one) && (Int.lt r modu)

let is_safe_bound bound r modu = match bound with
  | None -> true
  | Some m -> Int.equal (Int.pos_rem m modu) r

(* Sanity check for Top's arguments *)
let check min max r modu =
  if not (is_safe_modulo r modu
          && is_safe_bound min r modu
          && is_safe_bound max r modu)
  then fail min max r modu


let cardinal_zero_or_one v =
  match v with
  | Top _ -> false
  | Set s -> Array.length s <= 1
  | Float f -> Fval.is_singleton f

let is_singleton_int v = match v with
| Float _ | Top _ -> false
| Set s -> Array.length s = 1

let is_bottom x = x == bottom

let o_zero = O.singleton Int.zero
let o_one = O.singleton Int.one
let o_zero_or_one = O.union o_zero o_one

let small_nums =
  Array.init 33 (fun i -> Set [| (Integer.of_int i) |])

let zero = small_nums.(0)
let one = small_nums.(1)
let minus_one = Set [| Int.minus_one |]
let zero_or_one = Set [| Int.zero ; Int.one |]
let float_zeros = Float Fval.zeros

let positive_integers = Top(Some Int.zero, None, Int.zero, Int.one)
let negative_integers =
  Top(None, Some Int.zero, Int.zero, Int.one)
let strictly_negative_integers =
  Top(None, Some Int.minus_one, Int.zero, Int.one)

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
  if Fval.(equal plus_zero f)
  then zero
  else Float f

let inject_float_interval flow fup =
  let flow = Fval.F.of_float flow in
  let fup = Fval.F.of_float fup in
  (* make sure that zero float is also zero int *)
  if Fval.F.equal Fval.F.plus_zero flow && Fval.F.equal Fval.F.plus_zero fup
  then zero
  else Float (Fval.inject Fval.Float64 flow fup)

(*  let minus_zero = Float (Fval.minus_zero, Fval.minus_zero) *)

let is_one = equal one

let project_float v =
  if is_zero v
  then Fval.plus_zero
  else
    match v with
    | Float f -> f
    | Top _ | Set _ -> assert false (* by hypothesis that it is a float *)

let in_interval x min max r modu =
  Int.equal (Int.pos_rem x modu) r && min_le_elt min x && max_ge_elt max x

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
  | Float f -> Fval.contains_a_zero f

let contains_non_zero s =
  match s with
  | Top _ -> true (* at least two values *)
  | Set _ -> not (is_zero s || is_bottom s)
  | Float f -> Fval.contains_non_zero f


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
    | Float f -> if Fval.is_singleton f then Some Int.one else None

let cardinal_estimate v ~size =
  match v with
  | Set s -> Int.of_int (Array.length s)
  | Top (None, _, _, _)
  | Top (_, None, _, _) -> Int.two_power size
  | Top (Some mn, Some mx, _, d) -> Int.(succ (div (sub mx mn) d))
  | Float f ->
    if Fval.is_singleton f
    then Int.one
    else
      let bits_of_float =
        if Integer.(equal size (of_int 32))
        then Fval.bits_of_float32_list
        else if Integer.(equal size (of_int 64))
        then Fval.bits_of_float64_list
        else (fun _ -> [Int.zero, Int.pred (Int.two_power size)])
      in
      let bits_list = bits_of_float f in
      let count acc (min, max) = Int.add acc (Int.length min max) in
      List.fold_left count Int.zero bits_list

let cardinal_less_than v n =
  let c =
    match v with
    | Top (None,_,_,_) | Top (_,None,_,_) -> raise Not_less_than
    | Top (Some mn, Some mx,_,m) ->
        Int.succ ((Int.native_div (Int.sub mx mn) m))
    | Set s -> Int.of_int (Array.length s)
    | Float f -> 
	if Fval.is_singleton f then Int.one else raise Not_less_than
  in
  if Int.le c (Int.of_int n)
  then Int.to_int c (* This is smaller than the original [n] *)
  else raise Not_less_than

let cardinal_is_less_than v n =
  match cardinal v with
  | None -> false
  | Some c -> Int.le c (Int.of_int n)

let share_top min max r modu =
  let r = Top (min, max, r, modu) in
  if equal r top then top else r

let make ~min ~max ~rem ~modu =
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
      else Top (min, max, rem, modu)
    else if Int.equal mx mn
    then inject_singleton mn
    else bottom
  | _ ->
    share_top min max rem modu

let inject_top min max rem modu =
  check min max rem modu;
  make ~min ~max ~rem ~modu

let inject_interval ~min ~max ~rem:r ~modu =
  assert (is_safe_modulo r modu);
  let fix_bound fix bound = match bound with
    | None -> None
    | Some b -> Some (if Int.equal b (Int.pos_rem r modu) then b else fix b)
  in
  let min = fix_bound (fun min -> Int.round_up_to_r ~min ~r ~modu) min
  and max = fix_bound (fun max -> Int.round_down_to_r ~max ~r ~modu) max in
  make ~min ~max ~rem:r ~modu


let subdiv_int v =
  match v with
  | Float _ -> raise Can_not_subdiv
  | Set arr ->
      let len = Array.length arr in
      assert (len > 0 );
      if len <= 1 then raise Can_not_subdiv;
      let m = len lsr 1 in
      let lenhi = len - m in
      let lo = Array.sub arr 0 m in
      let hi = Array.sub arr m lenhi in
      share_array lo m,
      share_array hi lenhi
  | Top (Some lo, Some hi, rem, modu) ->
      let mean = Int.native_div (Int.add lo hi) Int.two in
      let succmean = Int.succ mean in
      inject_interval ~min:(Some lo) ~max:(Some mean) ~rem ~modu,
      inject_interval ~min:(Some succmean) ~max:(Some hi) ~rem ~modu
  | Top _ -> raise Can_not_subdiv

let subdivide ~size = function
  | Float fval ->
    let fkind = match Integer.to_int size with
      | 32 -> Fval.Float32
      | 64 -> Fval.Float64
      | _ -> raise Can_not_subdiv (* see Value/Value#105 *)
    in
    let f1, f2 = Fval.subdiv_float_interval fkind fval in
    inject_float f1, inject_float f2
  | ival -> subdiv_int ival

let inject_range min max = inject_top min max Int.zero Int.one

let top_float = Float Fval.top
let top_single_precision_float = Float Fval.top

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
  check min max r modu;
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
  | Set _ when is_zero t -> Some (Fval.F.plus_zero, Fval.F.plus_zero), false
  | Float f -> Fval.min_and_max f
  | _ -> assert false

let is_float = function
  | Float _ -> true
  | Set _ | Top _ -> false

let has_greater_min_bound t1 t2 =
  if is_float t1 || is_float t2
  then Fval.has_greater_min_bound (project_float t1) (project_float t2)
  else
    let m1, _ = min_and_max t1 in
    let m2, _ = min_and_max t2 in
    match m1, m2 with
    | None, None -> 0
    | None, Some _ -> -1
    | Some _, None -> 1
    | Some m1, Some m2 -> Int.compare m1 m2

let has_smaller_max_bound t1 t2 =
  if is_float t1 || is_float t2
  then Fval.has_smaller_max_bound (project_float t1) (project_float t2)
  else
    let _, m1 = min_and_max t1 in
    let _, m2 = min_and_max t2 in
    match m1, m2 with
    | None, None -> 0
    | None, Some _ -> -1
    | Some _, None -> 1
    | Some m1, Some m2 -> Int.compare m2 m1

let widen (bitsize,wh) t1 t2 =
  if equal t1 t2 || cardinal_zero_or_one t1 then t2
  else
    match t2 with
      | Float f2 ->
        let f1 = project_float t1 in
        Float (Fval.widen f1 f2)
      | Top _ | Set _ ->
          (* Add possible interval limits deducted from the bitsize *)
          let wh = if Integer.is_zero bitsize 
          then wh
          else
            let limits = [
                Integer.neg (Integer.two_power (Integer.pred bitsize));
                Integer.pred (Integer.two_power (Integer.pred bitsize));
                Integer.pred (Integer.two_power bitsize);
              ] in
            let module ISet = Datatype.Integer.Set in
            ISet.union wh (ISet.of_list limits)
          in
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

(* This function provides solutions to the Chinese remainder theorem,
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
       let ppcm = Integer.ppcm m1 m2 in
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

(* Do the two arrays have an integer in common *)
let arrays_intersect a1 a2 =
  let l1 = Array.length a1 in
  let l2 = Array.length a2 in
  let rec aux i1 i2 =
    if i1 = l1 || i2 = l2 then false
    else
      let e1 = a1.(i1) in
      let e2 = a2.(i2) in
      if Int.equal e1 e2 then true
      else if Int.lt e1 e2 then aux (succ i1) i2
      else aux i1 (succ i2)
  in
  aux 0 0


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
	let r = Array.make l Int.zero in
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
    | Float(f1), Float(f2) -> begin
        match Fval.meet f1 f2 with
        | `Value f -> inject_float f
        | `Bottom -> bottom
      end
    | (Float f as ff), (Top _ | Set _ as o)
    | (Top _ | Set _ as o), (Float f as ff) ->
      if equal o top then ff
      else if contains_zero o && Fval.contains_plus_zero f then zero
      else bottom
  in
  (*      Format.printf "meet: %a /\\ %a -> %a@\n"
          pretty v1 pretty v2 pretty result;*)
  result

let intersects v1 v2 =
  v1 == v2 ||
  match v1, v2 with
  | Top _, Top _ -> not (is_bottom (meet v1 v2)) (* YYY: slightly inefficient *)
  | Set s1 , Set s2 -> arrays_intersect s1 s2
  | Set s, Top (min, max, rm, modu) | Top (min, max, rm, modu), Set s ->
    Extlib.array_exists (fun x -> in_interval x min max rm modu) s
  | Float f1, Float f2 -> begin
      match Fval.forward_comp Comp.Eq f1 f2 with
      | Comp.False -> false
      | Comp.True | Comp.Unknown -> true
    end
  | Float f, other | other, Float f ->
    equal top other || (Fval.contains_plus_zero f && contains_zero other)

let narrow v1 v2 =
  match v1, v2 with
  | _, Set [||] | Set [||], _ -> bottom
  | Float _, Float _ | (Top _| Set _), (Top _ | Set _) ->
      meet v1 v2 (* meet is exact *)
  | v, (Top _ as t) when equal t top -> v
  | (Top _ as t), v when equal t top -> v
  | Float f, (Set _ as s) | (Set _ as s), Float f when is_zero s -> begin
      match Fval.narrow f Fval.zeros with
      | `Value f -> inject_float f
      | `Bottom -> bottom
    end
  | Float _, (Set _ | Top _) | (Set _ | Top _), Float _ ->
      (* ill-typed case. It is better to keep the operation symmetric *)
      top

(* Given a set of elements that is an under-approximation, returns an
   ival (while maintaining the ival invariants that the "Set"
   constructor is used only for small sets of elements. *)
let set_to_ival_under set =
  let card = Int.Set.cardinal set in
  if card  <= !small_cardinal
  then
    (let a = Array.make card Int.zero in
     ignore(Int.Set.fold (fun elt i ->
       Array.set a i elt;
       i + 1) set 0);
     share_array a card)
  else
    (* If by chance the set is contiguous. *)
    if (Int.equal
	  (Int.sub (Int.Set.max_elt set)  (Int.Set.min_elt set))
	  (Int.of_int (card - 1)))
    then Top( Some(Int.Set.min_elt set),
	      Some(Int.Set.max_elt set),
	      Int.one,
	      Int.zero)
    (* Else: arbitrarily drop some elements of the under approximation. *)
    else
      let a = Array.make !small_cardinal Int.zero in
      log_imprecision "Ival.set_to_ival_under";
      try
	ignore(Int.Set.fold (fun elt i ->
	  if i = !small_cardinal then raise Exit;
	  Array.set a i elt;
	  i + 1) set 0);
	assert false
      with Exit -> Set a
;;

let link v1 v2 = match v1, v2 with
  | Set a1, Set a2 ->
    let s1 = Array.fold_right Int.Set.add a1 Int.Set.empty in
    let s2 = Array.fold_right Int.Set.add a2 s1 in
    set_to_ival_under s2
  | Top(mn1,mx1,r1,m1), Top(mn2,mx2,r2,m2) ->
     if Int.equal r1 r2 && Int.equal m1 m2
     then
       let min = match mn1,mn2 with
         | Some(a), Some(b) -> Some(Int.min a b)
         | _ -> None in
       let max = match mx1,mx2 with
         | Some(a), Some(b) -> Some(Int.max a b)
         | _ -> None in
       inject_top min max r1 m1
     else v1 (* No best abstraction anyway. *)
  | Top(mn,mx,r,m), Set s | Set s, Top(mn,mx,r,m) ->
     let max = match mx with
       | None -> None
       | Some(max) ->
          let curmax = ref max in
          for i = 0 to (Array.length s) - 1 do
            let elt = s.(i) in
            if Int.equal elt (Int.add !curmax m)
            then curmax := elt
          done;
          Some(!curmax) in
     let min = match mn with
       | None -> None
       | Some(min) ->
          let curmin = ref min in
          for i = (Array.length s) - 1 downto 0 do
            let elt = s.(i) in
            if Int.equal elt (Int.sub !curmin m)
            then curmin := elt
          done;
          Some(!curmin) in
     inject_top min max r m
  | _ -> bottom
;;


let join v1 v2 =
  let result =
    if v1 == v2 then v1 else
      match v1,v2 with
      | Top(mn1,mx1,r1,m1), Top(mn2,mx2,r2,m2) ->
          check mn1 mx1 r1 m1;
          check mn2 mx2 r2 m2;
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
            check new_min new_max new_r new_modu;
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
		  let r = Array.make uniq Int.zero in
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
          inject_float (Fval.join f1 f2)
      | Float (f) as ff, other | other, (Float (f) as ff) ->
          if is_zero other
          then inject_float (Fval.join Fval.plus_zero f)
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

let fold_int_decrease f v acc =
  match v with
    Top(None,_,_,_) | Top(_,None,_,_) | Float _ ->
      raise Error_Top
  | Top(Some inf, Some sup, _, step) ->
      Int.fold f ~inf ~sup ~step:(Int.neg step) acc
  | Set s ->
      Array.fold_right (fun x acc -> f x acc) s acc

let fold_enum f v acc =
  match v with
  | Float fl when Fval.is_singleton fl -> f v acc
  | Float _ -> raise Error_Top
  | Set _ | Top _ -> fold_int (fun x acc -> f (inject_singleton x) acc) v acc

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
  | Set [||], _ -> true
  | Top(mn1,mx1,r1,m1), Top(mn2,mx2,r2,m2) ->
      (min_is_lower mn2 mn1) &&
      (max_is_greater mx2 mx1) &&
      rem_is_included r1 m1 r2 m2
  | Top _, Set _ -> false (* Top _ represents more elements
                             than can be represented by Set _ *)
  | Set s, Top(min, max, r, modu) ->
    (* Inclusion of bounds is needed for the entire inclusion *)
    min_le_elt min s.(0) && max_ge_elt max s.(Array.length s-1)
    && (Int.equal Int.one modu || (*Top side contains all integers, we're done*)
          array_for_all (fun x -> Int.equal (Int.pos_rem x modu) r) s)
  | Set s1, Set s2 -> array_subset s1 s2
  | Float f1, Float f2 -> Fval.is_included f1 f2
  | Float _, _ -> equal t2 top
  | Set _, Float f -> is_zero t1 && Fval.contains_plus_zero f
  | Top _, Float _ -> false

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
	      (not (min_le_elt mi (Int.add e psize))) ||
		(not (max_ge_elt ma (Int.sub e psize))) ||
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
  | Set s1, Set s2 -> apply2_n f s1 s2
  | _ -> top

let apply_set_unary f v =
  match v with
  | Set s -> map_set_exnsafe f s
  | _ -> top

let apply_bin_1_strict_incr f x (s : Integer.t array) =
  let l = Array.length s in
  let r = Array.make l Int.zero in
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
  let r = Array.make l Int.zero in
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
  let r = Array.make l Int.zero in
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
    let r = Array.make l Int.zero in
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
    let r = Array.make l Int.zero in
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
  | Float _ -> assert false
  | Set s -> apply_bin_1_strict_incr Int.add i s
  | Top (mn, mx, r, m) ->
    let incr v = Int.add i v in
    let new_mn = opt1 incr mn in
    let new_mx = opt1 incr mx in
    let new_r = Int.pos_rem (incr r) m in
    share_top new_mn new_mx new_r m


let rec add_int v1 v2 =
  match v1,v2 with
  | Float _, _ | _, Float _ -> assert false
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

let add_int_under v1 v2 = match v1,v2 with
  | Float _, _ | _, Float _ -> assert false
  | Set [| x |], Set s | Set s, Set [| x |]->
      apply_bin_1_strict_incr Int.add x s
  | Set s1, Set s2 ->
    let set =
      Array.fold_left (fun acc i1 ->
	Array.fold_left (fun acc i2 ->
	  Int.Set.add (Int.add i1 i2) acc) acc s2)
	Int.Set.empty s1
    in set_to_ival_under set
  | Top(min1,max1,r1,modu1) , Top(min2,max2,r2,modu2)
    when Int.equal modu1 modu2 ->
    (* Note: min1+min2 % modu = max1 + max2 % modu = r1 + r2 % modu;
       no need to trim the bounds here.  *)
    let r = Int.rem (Int.add r1 r2) modu1 in
    let min = match min1, min2 with
      | Some min1, Some min2 -> Some (Int.add min1 min2)
      | _ -> None in
    let max = match max1, max2 with
      | Some max1, Some max2 -> Some (Int.add max1 max2)
      | _ -> None in
    inject_top min max r modu1

  (* In many cases, there is no best abstraction; for instance when
     modu1 divides modu2, a part of the resulting interval is
     congruent to modu1, and a larger part is congruent to modu2.  In
     general, one can take the intersection. In any case, this code
     should be rarely called. *)
  | Top _, Top _ -> bottom
  | Set s, (Top _ as t) | (Top _ as t), Set s ->
      let l = Array.length s in
      if l = 0
      then bottom
      else if l = 1
      then (* only one element: precise. *)
        add_singleton_int s.(0) t
      else begin
        log_imprecision "Ival.add_int_under";
	(* Not worse than another computation. *)
	add_singleton_int s.(0) t
      end
;;


let neg_int v =
  match v with
  | Float _ -> assert false
  | Set s -> map_set_strict_decr Int.neg s
  | Top(mn,mx,r,m) ->
      share_top
        (opt1 Int.neg mx)
        (opt1 Int.neg mn)
        (Int.pos_rem (Int.neg r) m)
        m

let sub_int v1 v2 = add_int v1 (neg_int v2)
let sub_int_under v1 v2 = add_int_under v1 (neg_int v2)

type ext_value = Ninf | Pinf | Val of Int.t
let inject_min = function None -> Ninf | Some m -> Val m
let inject_max = function None -> Pinf | Some m -> Val m
let ext_neg = function Ninf -> Pinf | Pinf -> Ninf | Val v -> Val (Int.neg v)
let ext_mul x y =
  match x, y with
  | Ninf, Ninf | Pinf, Pinf -> Pinf
  | Ninf, Pinf | Pinf, Ninf -> Ninf
  | Val v1, Val v2 -> Val (Int.mul v1 v2)
  | (Ninf | Pinf as x), Val v | Val v, (Ninf | Pinf as x) ->
     if Int.gt v Int.zero then x
     else if Int.lt v Int.zero then ext_neg x
     else Val Int.zero

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

(* Rounds up (x+1) to the next power of two, then subtracts one; optimized. *)
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
      | Float _, _ | _, Float _ -> top
      | Set s1, Set s2 -> apply2_v Int.logor s1 s2
      | Set [|s|],(Top _ as v) | (Top _ as v),Set [|s|] when Int.is_zero s -> v
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

(* TODO: rename this function to scale_int *)
let scale f v =
  if Int.is_zero f
  then zero
  else
    match v with
    | Float _ -> top
    | Top(mn1,mx1,r1,m1) ->
        let incr = Int.mul f in
	if Int.gt f Int.zero
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



let scale_div_common ~pos f v degenerate_ival degenerate_float =
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
	  degenerate_ival r1 m1
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
  | Float _ -> degenerate_float

let scale_div ~pos f v =
  scale_div_common ~pos f v (fun _ _ -> Int.zero, Int.one) top
;;

let scale_div_under ~pos f v =
  try
    (* TODO: a more precise result could be obtained by transforming
       Top(min,max,r,m) into Top(min,max,r/f,m/gcd(m,f)). But this is
       more complex to implement when pos or f is negative. *)
    scale_div_common ~pos f v (fun _r _m -> raise Exit) bottom
  with Exit -> bottom
;;

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
     log_imprecision "Ival.div_range";
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
  | Float _ -> assert false
  | Top (None, _, _, _) | Top (_, None, _, _) ->
      log_imprecision "Ival.div";
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
              min_le_elt mn Int.minus_one,
              max_ge_elt mx Int.one,
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

let create_all_values_modu ~modu ~signed ~size =
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

let create_all_values ~signed ~size =
  if size <= !small_cardinal_log then
    (* We may need to create a set. Use slow path *)
    create_all_values_modu ~signed ~size ~modu:Int.one
  else
  if signed then
    let b = Int.two_power_of_int (size-1) in
    Top (Some (Int.neg b), Some (Int.pred b), Int.zero, Int.one)
  else
    let b = Int.two_power_of_int size in
    Top (Some Int.zero, Some (Int.pred b), Int.zero, Int.one)

let big_int_64 = Int.of_int 64
let big_int_32 = Int.thirtytwo

let cast_int_to_int ~size ~signed value =
  if equal top value
  then create_all_values ~size:(Int.to_int size) ~signed
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
    | Set s -> begin
      let all =
        create_all_values ~size:(Int.to_int size) ~signed
      in
      if is_included value all then value else map_set_exnsafe rem_f s
    end
    | Float _ -> assert false
  in
  (* If sharing is no longer preserved, please change Cvalue.V.cast *)
  if equal result value then value else result

let reinterpret_float_as_int ~signed ~size f =
  let reinterpret_list l =
    let reinterpret_one (b, e) =
      let i = inject_range (Some b) (Some e) in
      cast_int_to_int ~size ~signed i
    in
    let l = List.map reinterpret_one l in
    List.fold_left join bottom l
  in
  if Int.equal size big_int_64
  then
    let itvs = Fval.bits_of_float64_list f in
    reinterpret_list itvs
  else
  if Int.equal size big_int_32
  then
    let itvs = Fval.bits_of_float32_list f in
    reinterpret_list itvs
  else top

let reinterpret_as_int ~size ~signed i =
  match i with
  | Set _ | Top _ ->
    (* On integers, cast and reinterpretation are the same operation *)
    cast_int_to_int ~signed ~size i
  | Float f -> reinterpret_float_as_int ~signed ~size f

let cast_float_to_float fkind v =
  match v with
  | Float f ->
    begin match fkind with
    | Fval.Real | Fval.Float64 -> v
    | Fval.Float32 ->
      inject_float (Fval.round_to_single_precision_float f)
    end
  | Set _ when is_zero v -> zero
  | Set _ | Top _ -> top_float


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
          check mn1 mx1 r1 m1;
          check mn2 mx2 r2 m2;
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

(** Computes [x (op) ({y >= 0} * 2^n)], as an auxiliary function for
    [shift_left] and [shift_right]. [op] and [scale] must verify
    [scale a b == op (inject_singleton a) b] *)
let shift_aux scale op (x: t) (y: t) =
  let y = narrow (inject_range (Some Int.zero) None) y in
  try
  match y with
  | Set s ->
    Array.fold_left (fun acc n -> join acc (scale (Int.two_power n) x)) bottom s
  | _ ->
    let min_factor = Extlib.opt_map Int.two_power (min_int y) in
    let max_factor = Extlib.opt_map Int.two_power (max_int y) in
    let modu = match min_factor with None -> Int.one | Some m -> m in
    let factor = inject_top min_factor max_factor Int.zero modu in
    op x factor
  with Integer.Too_big ->
    Lattice_messages.emit_imprecision emitter "Ival.shift_aux";
    (* We only preserve the sign of the result *)
    if is_included x positive_integers then positive_integers
    else
      if is_included x negative_integers then negative_integers
      else top

let shift_right x y = shift_aux (scale_div ~pos:true) div x y
let shift_left x y = shift_aux scale mul x y


let interp_boolean ~contains_zero ~contains_non_zero =
  match contains_zero, contains_non_zero with
  | true, true -> zero_or_one
  | true, false -> zero
  | false, true -> one
  | false, false -> bottom


module Infty = struct
  let lt0 = function
    | None -> true
    | Some a -> Int.lt a Int.zero

  let div a b = match a with
    | None -> None
    | Some a -> match b with
      | None -> Some Int.zero
      | Some b -> Some (Int.div a b)

  let neg = function
    | Some a -> Some (Int.neg a)
    | None -> None
end

let backward_mult_pos_left min_right max_right result =
  let min_res, max_res = min_and_max result in
  let min_left =
    Infty.div min_res (if Infty.lt0 min_res then Some min_right else max_right)
  and max_left =
    Infty.div max_res (if Infty.lt0 max_res then max_right else Some min_right)
  in
  inject_range min_left max_left

let backward_mult_neg_left min_right max_right result =
  backward_mult_pos_left (Integer.neg max_right) (Infty.neg min_right) (neg_int result)

let backward_mult_int_left ~right ~result =
  match min_and_max right with
  | None, None -> `Value None
  | Some a, Some b when a > b -> `Bottom

  | Some a, Some b when a = Int.zero && b = Int.zero ->
    if contains_zero result then `Value None else `Bottom

  | Some a, max when a > Int.zero ->
    `Value (Some (backward_mult_pos_left a max result))

  | Some a, max when a >= Int.zero ->
    if contains_zero result
    then `Value None
    else `Value (Some (backward_mult_pos_left Int.one max result))

  | min, Some b when b < Int.zero ->
    `Value (Some (backward_mult_neg_left min b result))

  | min, Some b when b = Int.zero ->
    if contains_zero result
    then `Value None
    else `Value (Some (backward_mult_neg_left min Int.minus_one result))

  | min, max ->
    if contains_zero result
    then `Value None
    else
      `Value (Some (join
                      (backward_mult_pos_left Int.one max result)
                      (backward_mult_neg_left min Int.one result)))


let backward_le_int max v =
  match v with
  | Float _ -> v
  | Set _ | Top _ ->
      narrow v (Top(None,max,Int.zero,Int.one))

let backward_ge_int min v =
  match v with
  | Float _ -> v
  | Set _ | Top _ ->
      narrow v (Top(min,None,Int.zero,Int.one))

let backward_lt_int max v = backward_le_int (opt1 Int.pred max) v
let backward_gt_int min v = backward_ge_int (opt1 Int.succ min) v

let diff_if_one value rem = 
  match rem, value with
  | Set [| v |], Set a ->
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

let diff value rem =
  log_imprecision "Ival.diff";
  diff_if_one value rem

(* This function is an iterator, but it needs [diff_if_one] just above. *)
let fold_int_bounds f v acc =
  match v with
  | Float _ -> f v acc
  | Set _ | Top _ ->
    if cardinal_zero_or_one v then f v acc
    else
      (* apply [f] to [b] and reduce [v] if [b] is finite,
         or return [v] and [acc] unchanged *)
      let on_bound b v acc = match b with
        | None -> v, acc
        | Some b ->
          let b = inject_singleton b in
          diff_if_one v b, f b acc
      in
      let min, max = min_and_max v in
      (* [v] has cardinal at least 2, so [min] and [max] are distinct *)
      let v, acc = on_bound min v acc in
      let v, acc = on_bound max v acc in
      (* but if the cardinal was 2, then this [v] may be bottom *)
      if equal v bottom then acc else f v acc


let backward_comp_int_left op l r =
  let open Comp in
  try
    match op with
    | Le -> backward_le_int (max_int r) l
    | Ge -> backward_ge_int (min_int r) l
    | Lt -> backward_lt_int (max_int r) l
    | Gt -> backward_gt_int (min_int r) l
    | Eq -> narrow l r
    | Ne -> diff_if_one l r
  with Error_Bottom (* raised by max_int *) -> bottom

let backward_comp_float_left_true op fkind f1 f2  =
  let f1 = project_float f1 in
  let f2 = project_float f2 in
  begin match Fval.backward_comp_left_true op fkind f1 f2 with
    | `Value f -> inject_float f
    | `Bottom -> bottom
  end

let backward_comp_float_left_false op fkind f1 f2  =
  let f1 = project_float f1 in
  let f2 = project_float f2 in
  begin match Fval.backward_comp_left_false op fkind f1 f2 with
    | `Value f -> inject_float f
    | `Bottom -> bottom
  end



let rec extract_bits ~start ~stop ~size v =
  match v with
  | Set s ->
      inject_ps
        (Array.fold_left
           (fun acc elt -> add_ps acc (Int.extract_bits ~start ~stop elt))
           empty_ps
           s)
  | Float f ->
    let inject (b, e) = inject_range (Some b) (Some e) in
    let itvs =
      if Int.equal size big_int_64 then
        List.map inject (Fval.bits_of_float64_list f)
      else if Int.equal size big_int_32 then
        List.map inject (Fval.bits_of_float32_list f)
      else (* long double *)
        [top]
    in
    let bits = List.map (extract_bits ~start ~stop ~size) itvs in
    List.fold_left join bottom bits
  | Top(_,_,_,_) as d ->
    try
      let dived = scale_div ~pos:true (Int.two_power start) d in
      scale_rem ~pos:true (Int.two_power (Int.length start stop)) dived
    with Integer.Too_big ->
      Lattice_messages.emit_imprecision emitter "Ival.extract_bits";
      top
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
          (cast_int_to_int ~size ~signed:false v)
	  (create_all_values ~size:siz ~signed:false)

let compare_min_max min max =
  match min, max with
  | None,_ -> -1
  | _,None -> -1
  | Some min, Some max -> Int.compare min max
    
let compare_max_min max min =
  match max, min with
  | None,_ -> 1
  | _,None -> 1
  | Some max, Some min -> Int.compare max min

let forward_le_int i1 i2 =
  if compare_max_min (max_int i1) (min_int i2) <= 0 then Comp.True
  else if compare_min_max (min_int i1) (max_int i2) > 0 then Comp.False
  else Comp.Unknown

let forward_lt_int i1 i2 =
  if compare_max_min (max_int i1) (min_int i2) < 0 then Comp.True
  else if compare_min_max (min_int i1) (max_int i2) >= 0 then Comp.False
  else Comp.Unknown

let forward_eq_int i1 i2 =
  if cardinal_zero_or_one i1 && equal i1 i2 then Comp.True
  else if intersects i2 i2 then Comp.Unknown
  else Comp.False

let forward_comp_int op i1 i2 =
  let open Abstract_interp.Comp in
  match op with
  | Le -> forward_le_int i1 i2
  | Ge -> forward_le_int i2 i1
  | Lt -> forward_lt_int i1 i2
  | Gt -> forward_lt_int i2 i1
  | Eq -> forward_eq_int i1 i2
  | Ne -> inv_truth (forward_eq_int i1 i2)

let rehash x = 
  match x with
  | Set a -> share_array a (Array.length a)
  | _ -> x

include (
  Datatype.Make_with_collections
    (struct
      type ival = t
      type t = ival
      let name = Int.name ^ " lattice_mod"
      open Structural_descr
      let structural_descr =
        let s_int = Descr.str Int.descr in
        t_sum
          [|
            [| pack (t_array s_int) |];
            [| Fval.packed_descr |];
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
      let rehash = rehash
      let internal_pretty_code = Datatype.pp_fail
      let mem_project = Datatype.never_any_project
      let copy = Datatype.undefined
      let varname = Datatype.undefined
    end):
 Datatype.S_with_collections with type t := t)

let scale_int_base factor v = match factor with
  | Int_Base.Top -> top
  | Int_Base.Value f -> scale f v

type overflow_float_to_int =
  | FtI_Ok of Int.t (* Value in range *)
  | FtI_Overflow of Floating_point.sign (* Overflow in the corresponding
                                           direction *)

let cast_float_to_int_non_nan ~signed ~size (min, max) =
  let all = create_all_values ~size ~signed in
  let min_all = Extlib.the (min_int all) in
  let max_all = Extlib.the (max_int all) in
  let conv f =
    try
      (* truncate_to_integer returns an integer that fits in a 64 bits
         integer, but might not fit in [size, sized] *)
      let i = Floating_point.truncate_to_integer f in
      if Int.ge i min_all then
        if Int.le i max_all then FtI_Ok i
        else FtI_Overflow Floating_point.Pos
      else FtI_Overflow Floating_point.Neg
    with Floating_point.Float_Non_representable_as_Int64 sign ->
      FtI_Overflow sign
  in    
  let min_int = conv (Fval.F.to_float min) in
  let max_int = conv (Fval.F.to_float max) in
  match min_int, max_int with
  | FtI_Ok min_int, FtI_Ok max_int -> (* no overflow *)
    (NoAlarm, NoAlarm), inject_range (Some min_int) (Some max_int)

  | FtI_Overflow Floating_point.Neg, FtI_Ok max_int -> (* one overflow *)
    (Alarm, NoAlarm), inject_range (Some min_all) (Some max_int)
  | FtI_Ok min_int, FtI_Overflow Floating_point.Pos -> (* one overflow *)
    (NoAlarm, Alarm), inject_range (Some min_int) (Some max_all)

  (* two overflows *)
  | FtI_Overflow Floating_point.Neg, FtI_Overflow Floating_point.Pos ->
    (Alarm, Alarm), inject_range (Some min_all) (Some max_all)

  (* Completely out of range *)
  | FtI_Overflow Floating_point.Pos, FtI_Overflow Floating_point.Pos ->
    (NoAlarm, SureAlarm), bottom
  | FtI_Overflow Floating_point.Neg, FtI_Overflow Floating_point.Neg ->
    (SureAlarm, NoAlarm), bottom

  | FtI_Overflow Floating_point.Pos, FtI_Overflow Floating_point.Neg
  | FtI_Overflow Floating_point.Pos, FtI_Ok _
  | FtI_Ok _, FtI_Overflow Floating_point.Neg ->
    assert false (* impossible if min-max are correct *)

let cast_float_to_int ~signed ~size iv =
  match Fval.min_and_max (project_float iv) with
  | Some (min, max), nan ->
    let ov, r = cast_float_to_int_non_nan ~signed ~size (min, max)in
    (if nan then Alarm else NoAlarm), ov, r
  | None, _ -> (* means NaN *)
    SureAlarm, (NoAlarm, NoAlarm), bottom


(* These are the bounds of the range of integers that can be represented
   exactly as 64 bits double values *)
let double_min_exact_integer = Int.neg (Int.two_power_of_int 53)
let double_max_exact_integer = Int.two_power_of_int 53

(* same with 32 bits single values *)
let single_min_exact_integer = Int.neg (Int.two_power_of_int 24)
let single_max_exact_integer = Int.two_power_of_int 24

(* Same values expressed as double *)
let double_min_exact_integer_d = -. (2. ** 53.)
let double_max_exact_integer_d =     2. ** 53.
let single_min_exact_integer_d = -. (2. ** 24.)
let single_max_exact_integer_d =     2. ** 24.


(* finds all floating-point values [f] such that casting [f] to an integer
   type returns [i]. *)
let cast_float_to_int_inverse ~single_precision i =
  let exact_min, exact_max =
    if single_precision
    then single_min_exact_integer, single_max_exact_integer
    else double_min_exact_integer, double_max_exact_integer
  in
  let fkind = if single_precision then Fval.Float32 else Fval.Float64 in
  match min_and_max i with
  | Some min, Some max when Int.lt exact_min min && Int.lt max exact_max ->
    let minf =
      if Int.le min Int.zero then
        (* min is negative. We want to return [(float)((real)(min-1)+epsilon)],
           as converting this number to int will truncate all the fractional
           part (C99 6.3.1.4). Given [exact_min] and [exact_max], 1ulp
           is at most 1 here, so adding 1ulp will at most cancel the -1.
           Hence, we can use [next_float]. *)
        (* This float is finite because min is small enough *)
        Fval.F.next_float fkind (Int.to_float (Int.pred min))
      else (* min is positive. Since casting truncates towards 0,
              [(int)((real)min-epsilon)] would return [min-1]. Hence, we can
              simply return the float corresponding to [min] -- which can be
              represented precisely given [exact_min] and [exact_max]. *)
        Int.to_float min 
    in
    (* All operations are dual w.r.t. the min bound. *)
    let maxf =
      if Int.le Int.zero max
      then
        (* This float is finite because max is big enough *)
        Fval.F.prev_float fkind (Int.to_float (Int.succ max))
      else Int.to_float max
    in
    assert (Fval.F.is_finite (Fval.F.of_float minf));
    assert (Fval.F.is_finite (Fval.F.of_float maxf));
    Float (Fval.inject fkind (Fval.F.of_float minf) (Fval.F.of_float maxf))
  | _ -> if single_precision then top_single_precision_float else top_float


let cast_int_to_float_inverse_not_nan ~single_precision (min, max) =
  (* We restrict ourselves to (min,max) \in [exact_min, exact_max]. Outside of
     this range, the conversion int -> float is not exact, and the operation
     is more involved. *)
  let exact_min, exact_max =
    if single_precision
    then single_min_exact_integer_d, single_max_exact_integer_d
    else double_min_exact_integer_d, double_max_exact_integer_d
  in
  (* We find the integer range included in [f] *)
  let min = Fval.F.to_float min in
  let max = Fval.F.to_float max in
  if exact_min <= min && max <= exact_max then
    (* Round to integers in the proper direction: discard the non-floating-point
       values on each extremity. *)
    let min = ceil min in
    let max = floor max in
    let conv f = try  Some (Integer.of_float f) with Integer.Too_big -> None in
    let r = inject_range (conv min) (conv max) in
    (* Kernel.result "Cast I->F inv:  %a -> %a@." pretty f pretty r; *)
    r
  else top (* Approximate *)

let cast_int_to_float_inverse ~single_precision f =
  match min_and_max_float f with
  | None, _ -> (* NaN *) bottom (* a cast of NaN to int is fully undefined *)
  | Some (min, max), _ (*we can disregard the NaN boolean for the same reason *)
    ->
    cast_int_to_float_inverse_not_nan ~single_precision (min, max)

let of_int i = inject_singleton (Int.of_int i)
let of_int64 i = inject_singleton (Int.of_int64 i)


(* This function always succeeds without alarms for C integers, because they
   always fit within a float32. *)
let cast_int_to_float fkind v =
  let min,max = min_and_max v in
  inject_float (Fval.cast_int_to_float fkind min max)

let reinterpret_as_float kind i =
  match i with
  | Float _ ->  i
  | Set _ when is_zero i || is_bottom i -> i
  | Top _ | Set _ ->
    (* Reinterpret a range of integers as a range of floats.
       Float are ordered this way :
       if [min_i], [max_i] are the bounds of the signed integer type that
       has the same number of bits as the floating point type, and [min_f]
       [max_f] are the integer representation of the most negative and most
       positive finite float of the type, and < is signed integer comparison,
       we have: min_i < min_f <  min_f+1  < -1 <  0 < max_f <  max_f+1  < max_i
                 |        |       |          |    |      |       |          |
                 --finite--       -not finite-    -finite-       -not finite-
                 |        |       |<--------->    |      |       |<--------->
                -0.     -max    -inf   NaNs      +0.    max     inf   NaNs
       The float are of the same sign as the integer they convert into.
       Furthermore, the conversion function is increasing on the positive
       interval, and decreasing on the negative one. *)
    let reinterpret size kind conv min_f max_f =
      let size = Integer.of_int size in
      let i = cast_int_to_int ~size ~signed:true i in
      (* Intersect [i'] with [i], and return the (finite) bounds directly. *)
      let bounds_narrow i' =
        let r = narrow i i' in
        if is_bottom r then `Bottom
        else
          match min_and_max r with
          | None, _ | _, None -> assert false (* i is finite thanks to cast *)
          | Some b, Some e -> `Value (b, e)
      in
      let s_max_f = Int.succ max_f (* neg inf *) in
      let s_min_f = Int.succ min_f (* pos inf *) in
      let s_s_max_f = Int.succ s_max_f (* first 'positive' NaN *) in
      let s_s_min_f = Int.succ s_min_f (* first 'negative' NaN  *) in
      (* positive floats *)
      let f_pos = inject_range (Some Integer.zero) (Some s_max_f) in
      (* negative floats *)
      let f_neg = inject_range None (Some s_min_f) in
      (* 'positive' NaNs *)
      let nan_pos = inject_range (Some s_s_max_f) None in
      (* 'negative' NaNs *)
      let nan_neg = inject_range (Some s_s_min_f) (Some Int.minus_one) in
      let nan = (* at least one NaN somewhere *)
        if intersects i nan_pos || intersects i nan_neg
        then [`Value Fval.nan]
        else []
      in
      let open Bottom in
      let range mn mx = Fval.inject kind (conv mn) (conv mx) in
      (* convert positive floats; increasing on positive range *)
      let pos = bounds_narrow f_pos >>-: (fun (b, e) -> range b e) in
      (* convert negative floats; decreasing on negative range *)
      let neg = bounds_narrow f_neg >>-: (fun (b, e) -> range e b) in
      let f = Bottom.join_list Fval.join (pos :: neg :: nan) in
      inject_float (Bottom.non_bottom f)
    in
    let open Floating_point in
    match kind with
    | Cil_types.FDouble ->
      let conv v = Fval.F.of_float (Int64.float_of_bits (Int.to_int64 v)) in
      reinterpret
        64 Fval.Float64 conv bits_of_most_negative_double bits_of_max_double
    | Cil_types.FFloat ->
      let conv v = Fval.F.of_float(Int32.float_of_bits (Int.to_int32 v)) in
      reinterpret
        32 Fval.Float32 conv bits_of_most_negative_float bits_of_max_float
    | Cil_types.FLongDouble ->
      (* currently always imprecise *)
      top_float

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

let bitwise_and_intervals ~size ~signed v1 v2 =
  let max_int_v1, max_int_v2 as max_int_v1_v2 = max_int v1, max_int v2 in
  let min_int_v1, min_int_v2 as min_int_v1_v2 = min_int v1, min_int v2 in
  let half_range = Int.two_power_of_int (pred size) in
  let minint = Int.neg half_range in
  let vmax =
    match max_int_v1_v2 with
    | Some maxv1, Some maxv2 ->
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
  let somenegativev1 = intersects v1 strictly_negative_integers in
  let somenegativev2 = intersects v2 strictly_negative_integers in
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
                      (* Format.printf
                         "bitwise_and v1 %a v2 %a min_b %a min_c %a@."
                         pretty v1 pretty v2
                         Int.pretty min_b Int.pretty min_c; *)
              Int.max (Int.max min_a min_b) min_c
          | _ -> assert false
        else min_a
      in
              (* Format.printf "bitwise_and v1 %a v2 %a bits1 %a bits2 %a@."
                 pretty v1 pretty v2
                 Int.pretty bits1 Int.pretty bits2; *)
      Some min_a
    end
  in
  vmin, vmax

(* [common_low_bits v] returns the common pattern between the 
   least-significant bits of all the elements of the Ival [v]. 
   The pattern is in the form [lower_bits, mask] where [mask]
   indicates the consecutive least significant bits that are
   common between all elements, and
   [lower_bits] indicates their values. *)
let common_low_bits v =
  match v with
  | Float _ -> assert false
  | Top(_,_,r,m) -> 
    if Int.is_zero (Int.logand m (Int.pred m))
    then (* m is a power of two *)
      r, Int.pred m
    else Int.zero, Int.zero (* TODO *)
  | Set [| v |] ->
    v, next_pred_power_of_two v
  | Set _ -> Int.zero, Int.zero (* TODO *)

let bitwise_and ~size ~signed v1 v2 =
  if is_bottom v1 || is_bottom v2
  then bottom
  else
    match v1, v2 with
    | Float _, _ | _, Float _ -> assert false
    | Set s1, Set s2 ->
      apply2_v Int.logand s1 s2
    | Top _, other | other, Top _ ->
      let min, max = bitwise_and_intervals ~signed ~size v1 v2 in
      let lower_bits1, mask1 = common_low_bits v1 in
      let lower_bits2, mask2 = common_low_bits v2 in
      let mask = Int.logand mask1 mask2 in
      let modu = Int.succ mask in
      let r = Int.logand lower_bits1 (Int.logand lower_bits2 mask) in
      let min = match min with
        | Some min -> Some (Int.round_up_to_r ~min ~r ~modu)
        | _ -> min 
      in
      let max = match max with
        | Some max -> Some (Int.round_down_to_r ~max ~r ~modu)
        | _ -> max 
      in
      let result = inject_top min max r modu in
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

let bitwise_not v =
  (* the result is [-v - 1] *)
  match v with
  | Float _ -> assert false
  | Top _ -> add_int (neg_int v) minus_one
  | Set s -> map_set_strict_decr Int.lognot s

let bitwise_not_size ~size ~signed v =
  let nv = bitwise_not v in
  if not signed then
    cast_int_to_int ~size:(Integer.of_int size) ~signed nv
  else nv (* always fits in the type if the argument fitted. *)


let pretty_debug = pretty
let name = "ival"

(*
Local Variables:
compile-command: "make -C ../../.."
End:
*)
