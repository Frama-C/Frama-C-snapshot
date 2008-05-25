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

open Cil_types

exception Not_less_than
exception Is_not_included

(** Generic lattice *)
module type Lattice = sig
  exception Error_Top
  exception Error_Bottom
  type t (** type of element of the lattice *)
  type widen_hint (** hints for the widening *)

  val equal: t -> t -> bool
  val join: t -> t -> t (** over-approximation of union *)
  val link: t -> t -> t (** under-approximation of union *)
  val meet: t -> t -> t (** under-approximation of intersection *)
  val narrow: t -> t -> t (** over-approximation of intersection *)
  val bottom: t (** the smallest *)
  val top: t  (** the largest *)

  val is_included: t -> t -> bool
  val is_included_exn: t -> t -> unit
  val intersects: t -> t -> bool
  val pretty: Format.formatter -> t -> unit

  val widen: widen_hint -> t -> t -> t
    (** [widen h t1 t2] is an over-approximation of [join t1 t2].
        Assumes [is_included t1 t2] *)

  val cardinal_zero_or_one: t -> bool

  (** [cardinal_less_than t v ]
      @raise Not_less_than whenever the cardinal of [t] is higher than [v]
  *)
  val cardinal_less_than: t -> int -> int

  val tag : t -> int

  module Datatype: Project.Datatype.OUTPUT with type t = t
end

module type Lattice_With_Diff = sig
  include Lattice

  val diff : t -> t -> t
    (** [diff t1 t2] is an over-approximation of [t1-t2]. *)

  val diff_if_one : t -> t -> t
    (** [diff t1 t2] is an over-approximation of [t1-t2].
       Returns [t1] if [t2] is not a singleton. *)

  val fold_enum : (t -> 'a -> 'a) -> t -> 'a -> 'a
  val hash : t -> int
  val pretty_debug : Format.formatter -> t -> unit
  val name : string
end

module type Lattice_Product = sig
  type t1
  type t2
  type tt = private Product of t1*t2 | Bottom
  include Lattice with type t = tt
  val inject : t1 -> t2 -> t
  val fst : t -> t1
  val snd : t -> t2
end

module type Lattice_Sum = sig
  type t1
  type t2
  type sum = private Top | Bottom | T1 of t1 | T2 of t2
  include Lattice with type t = sum
  val inject_t1 : t1 -> t
  val inject_t2 : t2 -> t
end

module type Lattice_Base = sig
  type l
  type tt = private Top | Bottom | Value of l
  include Lattice with type t = tt
  val project : t -> l
  val inject: l -> t
end

module type Lattice_Set = sig
  module O: Set.S
  type tt = private Set of O.t | Top
  include Lattice with type t = tt and type widen_hint = O.t
  val hash : t -> int
  val inject_singleton: O.elt -> t
  val inject: O.t -> t
  val empty: t
  val apply2: (O.elt -> O.elt -> O.elt) -> (t -> t -> t)
  val apply1: (O.elt -> O.elt) -> (t -> t)
  val fold: ( O.elt -> 'a -> 'a) -> t -> 'a -> 'a
  val iter: ( O.elt -> unit) -> t -> unit
  val project : t -> O.t
  val mem : O.elt -> t -> bool
end

module type Value = sig
  type t
  val pretty: Format.formatter -> t -> unit
  val compare : t -> t -> int
  val hash: t -> int
  module Datatype: Project.Datatype.OUTPUT with type t = t
end

module type Arithmetic_Value = sig
  include Value
  val gt : t -> t -> bool
  val le : t -> t -> bool
  val ge : t -> t -> bool
  val lt : t -> t -> bool
  val eq : t -> t -> bool
  val add : t -> t -> t
  val sub : t -> t -> t
  val mul : t -> t -> t
  val native_div : t -> t -> t
  val rem : t -> t -> t
  val pos_div : t -> t -> t
  val c_div : t -> t -> t
  val c_rem : t -> t -> t
  val abs : t -> t
  val zero : t
  val one : t
  val two : t
  val four : t
  val minus_one : t
  val is_zero : t -> bool
  val is_one : t -> bool
  val equal : t -> t -> bool
  val pgcd : t -> t -> t
  val ppcm : t -> t -> t
  val min : t -> t -> t
  val max : t -> t -> t
  val length : t -> t -> t (** b - a + 1 *)
  val of_int : int -> t
  val of_float : float -> t
  val of_int64 : Int64.t -> t
  val to_int : t -> int
  val to_float : t -> float
  val neg : t -> t
  val succ : t -> t
  val pred : t -> t
  val round_up_to_r : min:t -> r:t -> modu:t -> t
  val round_down_to_r : max:t -> r:t -> modu:t -> t
  val pos_rem : t -> t -> t
  val shift_left : t -> t -> t
  val shift_right : t -> t -> t
  val fold : (t -> 'a -> 'a) -> inf:t -> sup:t -> step:t -> 'a -> 'a
  val logand : t -> t -> t
  val logor : t -> t -> t
  val logxor : t -> t -> t
  val lognot : t -> t
  val power_two : int -> t
  val extract_bits : start:t -> stop:t -> t -> t
end

module Make_Lattice_Set(V:Value):(Lattice_Set with type O.elt=V.t)=
struct
  exception Error_Top
  exception Error_Bottom
  module O = Set.Make(V)
  type tt = Set of O.t | Top
  type t = tt = Set of O.t | Top
  type y = O.t
  type widen_hint = O.t

  let bottom = Set O.empty
  let top = Top

  let hash c =
    match c with
      Top -> 12373
    | Set s ->
	let f v acc =
	  67 * acc + (V.hash v)
	in
	O.fold f s 17

  let tag = hash

  let compare e1 e2 =
    if e1==e2 then 0 else
      match e1,e2 with
        | Top,_ -> 1
        | _, Top -> -1
        | Set e1,Set e2 -> O.compare e1 e2

  let equal v1 v2 = compare v1 v2 = 0

  let widen _wh _t1 t2 = (* [wh] isn't used *)
    t2

  (** This is exact *)
  let meet v1 v2 =
    if v1 == v2 then v1 else
      match v1,v2 with
      | Top, v | v, Top -> v
      | Set s1 , Set s2 -> Set (O.inter s1 s2)

  (** This is exact *)
  let narrow = meet

  (** This is exact *)
  let join v1 v2 =
    if v1 == v2 then v1 else
      match v1,v2 with
      | Top, _ | _, Top -> Top
      | Set s1 , Set s2 ->
          let u = O.union s1 s2 in
          Set u

  (** This is exact *)
  let link = join

  let cardinal_less_than s n =
    match s with
      Top -> raise Not_less_than
    | Set s ->
	let c = O.cardinal s in
	if  c > n
	then raise Not_less_than;
	c

  let cardinal_zero_or_one s =
    try
      ignore (cardinal_less_than s 1) ; true
    with Not_less_than -> false

  let inject s = Set s
  let inject_singleton e = inject (O.singleton e)
  let empty = inject O.empty

  let transform f = fun t1 t2 ->
    match t1,t2 with
      | Top, _ | _, Top -> Top
      | Set v1, Set v2 -> Set (f v1 v2)

  let map_set f s =
    O.fold
      (fun v -> O.add (f v))
      s
      O.empty

  let apply2 f s1 s2 =
    let distribute_on_elements f s1 s2 =
      O.fold
        (fun v -> O.union (map_set (f v) s2))
        s1
        O.empty
    in
    transform (distribute_on_elements f) s1 s2

  let apply1 f s = match s with
    | Top -> top
    | Set s -> Set(map_set f s)

  let pretty fmt t =
    match t with
      | Top -> Format.fprintf fmt "TopSet"
      | Set s ->
          if O.is_empty s then Format.fprintf fmt "BottomSet"
          else begin
            Format.fprintf fmt "@[{@[%a@]}@]"
              (fun fmt s ->
		 O.iter
                   (Format.fprintf fmt "@[%a;@]@ " V.pretty) s) s
          end

  let is_included t1 t2 =
    (t1 == t2) ||
      match t1,t2 with
      | _,Top -> true
      | Top,_ -> false
      | Set s1,Set s2 -> O.subset s1 s2

  let is_included_exn v1 v2 =
    if not (is_included v1 v2) then raise Is_not_included

  let intersects t1 t2 =
    let b = match t1,t2 with
      | _,Top | Top,_ -> true
      | Set s1,Set s2 ->
          O.exists (fun e -> O.mem e s2) s1
    in
    (* Format.printf
       "[Lattice_Set]%a intersects %a: %b @\n"
       pretty t1 pretty t2 b;*)
    b

  let fold f elt init =
    match elt with
      | Top -> raise Error_Top
      | Set v -> O.fold f v init


  let iter f elt =
    match elt with
      | Top -> raise Error_Top
      | Set v -> O.iter f v

  let project o = match o with
    | Top -> raise Error_Top
    | Set v -> v

  let mem v s = match s with
  | Top -> true
  | Set s -> O.mem v s

  module Datatype =
    Project.Datatype.Register
      (struct
	 type t = tt
	 let copy _ = assert false (* TODO *)
	 let rehash x = match x with
	   | Top -> x
	   | Set set ->
	       inject
		 (O.fold (fun x -> O.add (V.Datatype.rehash x)) set O.empty)
	 include Datatype.Nop
	 let name = Project.Datatype.Name.extend "lattice_set" V.Datatype.name
	 let dependencies = [ V.Datatype.self ]
       end)

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

module type Card = sig
  type t
  val n : t
end

module Widen_Arithmetic_Value_Set(V:Arithmetic_Value) =
struct
  module V = V
  module S = SetWithNearest.Make(V)
  include S

  module Datatype =
    Datatype.Make_Set(S)(struct include V.Datatype let compare = V.compare end)

  let pretty fmt s =
    if is_empty s then Format.fprintf fmt "{}"
    else begin
      Format.fprintf fmt "{%a}"
        (fun fmt s ->
	   iter
             (Format.fprintf fmt "%a; " V.pretty) s) s
    end

  let default_widen_hints =
    List.fold_left
      (fun acc x ->
	add (V.of_int x) acc)
      empty
      [-128;-1;0;1;3;15;127;512;32767;1 lsl 29]

end

module type Float_Abstract_Sig =
sig
  type t
  type integer
  exception Nan_or_infinite
  exception Bottom
  val compare : t -> t -> int
  val pretty : Format.formatter -> t -> unit
  val hash : t -> int
  val widen : t -> t -> t
  val is_singleton : t -> bool
  val is_zero : t -> bool
  val contains_zero : t -> bool
  val zero : t
  val meet : t -> t -> t
  val join : t -> t -> t
  val top : t
  val is_included : t -> t -> bool
  val diff : t -> t -> t
  val filter_le : t -> t -> t
  val filter_ge : t -> t -> t
  val filter_lt : t -> t -> t
  val filter_gt : t -> t -> t
(*  val inject_interval : float -> float -> bool *)
(*  val add: t -> t -> t
  val sub: t -> t -> t
  val neg: t -> t
  val mult: t -> t -> t
  val div: t -> t -> t
  val hash: t -> int
  val compare: t -> t -> int
  val equal: t -> t -> bool
  val equal_ieee: t -> t -> bool
  val pretty: Format.formatter -> t -> unit
  val zero: t
  val minus_zero : t
  val minus_one : t
  val one : t
  val sqrt : t -> t
  val cos : t -> t
  val is_zero_ieee: t -> bool
  val is_zero: t -> bool
  val max: t -> t -> t
  val min: t -> t -> t
  val le_ieee: t -> t -> bool
  val lt_ieee: t -> t -> bool
  val le: t -> t -> bool
  val of_float: float -> t
  exception Non_representable_float
  val to_integer: t -> integer*)
end

module Make_Lattice_Mod
    (V:Arithmetic_Value)
    (CARD:Card with type t = int)
    (F:Float_Abstract_Sig with type integer = V.t)  =
  struct
    let () = assert (compare CARD.n (2) >= 0)
    exception Error_Top
    exception Error_Bottom
    module O = Set.Make(V)
    type tt =
	Set of O.t | Float of F.t
      | Top of V.t option * V.t option * V.t * V.t
    type t = tt
    type y = O.t

    module Widen_Hints = Widen_Arithmetic_Value_Set(V)
    type widen_hint = Widen_Hints.t

    let some_zero = Some V.zero

    let bottom = Set O.empty
    let top = Top(None, None, V.zero, V.one)

    let hash_v_option v =
      match v with None -> 97 | Some v -> V.hash v

    let hash v =
      match v with
	Set s -> O.fold (fun v acc -> 1031 * acc + (V.hash v)) s 0
      | Top(mn,mx,r,m) ->
	  hash_v_option mn + 5501 * (hash_v_option mx) +
	    59 * (V.hash r) + 13031 * (V.hash m)
      | Float(f) ->
	  3 + 17 * F.hash f

    let tag = hash

    let bound_compare x y =
      match x,y with
	None, None -> 0
      | None, Some _ -> 1
      | Some _, None -> -1
      | Some x, Some y -> V.compare x y

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
	  else let r3 = V.compare r r' in
	  if r3 <> 0 then r3
	  else V.compare m m'
      | _, Top _ -> 1
      | Top _, _ -> -1
      | Float(f1), Float(f2) ->
	  F.compare f1 f2
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
		  | Some v -> V.pretty fmt v))
	      mn
	      (fun fmt ->
		(function None -> Format.fprintf fmt "--"
		  | Some v -> V.pretty fmt v))
	      mx
              (fun fmt ->
		if V.is_zero r && V.is_one m then
                  Format.fprintf fmt ""
		else Format.fprintf fmt ",%a%%%a"
                    V.pretty r
	            V.pretty m)
      | Float (f) ->
	  F.pretty fmt f
      | Set s ->
          if O.is_empty s then Format.fprintf fmt "BottomMod"
          else begin
            Format.fprintf fmt "{%a}"
              (fun fmt s ->
		O.iter
                  (Format.fprintf fmt "%a; " V.pretty) s) s
	      (*            Format.fprintf fmt "{%a (%x, %x)}"
			    (fun fmt s ->
			    O.iter
			    (Format.fprintf fmt "%a; " V.pretty) s) s
			    (Obj.magic s) (Obj.magic set)*)
          end

    let compare_elt_min elt min =
      match min with
      | None -> true
      | Some m -> V.le m elt 

    let compare_elt_max elt max =
      match max with
      | None -> true
      | Some m -> V.ge m elt 

    let all_positives min =
      match min with
      | None -> false
      | Some m -> V.ge m V.zero 

    let all_negatives max =
      match max with
      | None -> false
      | Some m -> V.le m V.zero 

    let check doc min max r modu =
      assert(assert (V.ge r V.zero );
	     assert (V.ge modu V.one );
	     (match min with
	     | None -> ()
	     | Some m -> if not (V.equal (V.pos_rem m modu) r) then
		 begin
		   ignore (CilE.warn_once "Make_Lattice_Mod.check: '%s'\n" doc);
		   Format.printf "min=%a modu=%a r=%a@." V.pretty m  V.pretty modu  V.pretty r;
		   assert false
		 end);
	     (match max with
	     | None -> ()
	     | Some m -> assert (V.equal (V.pos_rem m modu) r));
	     true)

    let cardinal_zero_or_one v = match v with
    | Top _ -> false
    | Set s -> O.cardinal s <= 1
    | Float (f) -> F.is_singleton f

    let is_singleton_int v = match v with
    | Float _ | Top _ -> false
    | Set s -> O.cardinal s = 1

    let is_bottom = equal bottom

    let inject_singleton e = Set (O.singleton e)

    let zero = inject_singleton V.zero

(*  let minus_zero = Float (F.minus_zero, F.minus_zero) *)

    let one = inject_singleton V.one

    let is_zero = equal zero

    let is_one = equal one

    let inject_float f =
      if F.is_zero f
      then zero
      else Float f

    let project_float v =
      if is_zero v
      then F.zero
      else
	match v with
	  Float f -> f
	| Top _ | Set _ -> raise F.Nan_or_infinite


    let in_interval x min max r modu =
      (V.equal (V.pos_rem x modu) r) &&
      (compare_elt_min x min) &&
      (compare_elt_max x max)

    let contains_zero s =
      match s with
      | Top(mn,mx,r,m) -> in_interval V.zero mn mx r m
      | Set s -> O.mem V.zero s
      | Float f -> F.contains_zero f

    exception Not_Singleton_Int

    let project_int v = match v with
    | Set s when O.cardinal s = 1 -> O.min_elt s
    | _ -> raise Not_Singleton_Int

    let cardinal_less_than v n =
      let c =
	match v with
	| Top (None,_,_,_) | Top (_,None,_,_) -> raise Not_less_than
	| Top (Some mn, Some mx,_,m) ->
            V.succ ((V.native_div (V.sub mx mn) m))
	| Set s -> V.of_int (O.cardinal s)
	| Float f -> if F.is_singleton f then V.one else raise Not_less_than
      in
      if V.le c (V.of_int n)
      then V.to_int c (* This is smaller than the original [n] *)
      else raise Not_less_than

    let inject_top min max r modu =
      check "inject_top" min max r modu;
      match min, max with
      | Some mn, Some mx ->
	  if V.ge mx mn  then
	    if (V.le (V.length mn mx) (V.mul modu (V.of_int CARD.n))) 
	    then
	      let s = ref O.empty in
	      let i = ref mn in
	      while (V.le !i mx)
	      do
		s := O.add !i !s;
		i := V.add modu !i
	      done;
	      Set (!s)
	    else Top (min, max, r, modu)
          else bottom
      | _ -> Top (min, max, r, modu)

    let inject_range min max = inject_top min max V.zero V.one

    let top_float = Float F.top

    let unsafe_make_top_from_set_4 s =
      assert (O.cardinal s >= 2);
      let m = O.min_elt s in
      let modu = O.fold
	  (fun x acc ->
	    if V.equal x m
	    then acc
	    else V.pgcd (V.sub x m) acc)
	  s
	  V.zero
      in
      let r = V.pos_rem m modu in
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
      | Float _ -> None, None, V.zero, V.one

    let min_and_max t =
      match t with
      | Set s ->
	  assert (O.cardinal s >= 1);
	  Some (O.min_elt s), Some (O.max_elt s)
      | Top (a,b,_,_) -> a, b
      | Float _ -> None, None

    let widen wh t1 t2 =
      if equal t1 t2 || cardinal_zero_or_one t1 then t2
      else
	match t2 with
	  Float f2 ->
	    ( try
	      let f1 = project_float t1 in
	      if not (F.is_included f1 f2)
	      then assert false;
	      Float (F.widen f1 f2)
	    with F.Nan_or_infinite -> assert false)
	| Top _ | Set _ ->
	    let (mn2,mx2,r2,m2) = min_max_r_mod t2 in
	    let (mn1,mx1,r1,m1) = min_max_r_mod t1 in
	    let new_mod = V.pgcd (V.pgcd m1 m2) (V.abs (V.sub r1 r2)) in
	    let new_rem = V.rem r1 new_mod in

	    let new_min = if bound_compare mn1 mn2 = 0 then mn2 else
            match mn2 with
	    | None -> None
            | Some mn2 ->
		try
		  let v = Widen_Hints.nearest_elt_le mn2 wh
		  in Some (V.round_up_to_r ~r:new_rem ~modu:new_mod ~min:v)
		with Not_found -> None
	    in
	    let new_max = if bound_compare mx1 mx2 = 0 then mx2 else
            match mx2 with None -> None
            | Some mx2 ->
		try
                  let v = Widen_Hints.nearest_elt_ge mx2 wh
                  in Some (V.round_down_to_r ~r:new_rem ~modu:new_mod ~max:v)
		with Not_found -> None
	    in
            let result = inject_top new_min new_max new_rem new_mod in
            (*Format.printf "%a -- %a --> %a (thx to %a)@."
              pretty t1 pretty t2 pretty result
              Widen_Hints.pretty wh;*)
            result


    let inject_set s =
      if (O.cardinal s) <= CARD.n
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
	      V.max m1 m2
          | None, None -> assert false (* already tested above *)
	in
	Some (V.round_up_to_r m r modu)

    let compute_last_common mx1 mx2 r modu =
      if mx1 = None && mx2 = None
      then None
      else
	let m =
	  match (mx1, mx2) with
	  | Some m, None | None, Some m -> m
	  | Some m1, Some m2 ->
	      V.min m1 m2
	  | None, None -> assert false (* already tested above *)
	in
	Some (V.round_down_to_r m r modu)


    let min_min x y =
      match x,y with
      | None,_ | _,None -> None
      | Some x, Some y -> Some (V.min x y)

    let max_max x y =
      match x,y with
      | None,_ | _,None -> None
      | Some x, Some y -> Some (V.max x y)


    let min_max x y =
      match x,y with
      | None,z | z,None -> z
      | Some x, Some y -> Some (V.min x y)

    exception Found of V.t


    let compute_r_common r1 modu1 r2 modu2 =
      let modu = V.ppcm modu1 modu2 in
      try
	let i = ref V.zero in (* for i = 0 to modu - 1 *)
	while (V.le !i (V.pred modu)) 
	do
	  if (V.equal (V.rem !i modu1) r1) && (V.equal (V.rem !i modu2) r2)
	  then raise (Found !i);
	  i := V.succ !i
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
	    inject_float (F.meet f1 f2)
	| (Float f) as ff, other | other, ((Float f) as ff) ->
	    if equal top other
	    then ff
	    else if (F.contains_zero f) && contains_zero other
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
	    let modu = V.pgcd (V.pgcd m1 m2) (V.abs(V.sub r1 r2)) in
	    let r = V.rem r1 modu in
	    let min = min_min mn1 mn2 in
	    let max = max_max mx1 mx2 in
	    let r  = inject_top min max r modu in
	    r
	| Set s, (Top(min, max, r, modu) as t)
	| (Top(min, max, r, modu) as t), Set s ->
            if O.is_empty s then t
            else
	      let f elt modu = V.pgcd modu (V.abs(V.sub r elt)) in
	      let new_modu = O.fold f s modu in
	      let new_r = V.rem r new_modu in
	      let new_min = match min with
		None -> None
	      | Some m -> Some (V.min m (O.min_elt s))
	      in
	      let new_max = match max with
		None -> None
	      | Some m -> Some (V.max m (O.max_elt s))
	      in
	      check "inside join" new_min new_max new_r new_modu;
	      Top(new_min, new_max, new_r, new_modu)
	| Set s1 , Set s2 ->
            let u = O.union s1 s2 in
            inject_set u
	| Float(f1), Float(f2) ->
	    inject_float (F.join f1 f2)
	| Float (f) as ff, other | other, (Float (f) as ff) ->
	    if is_zero other
	    then inject_float (F.join F.zero f)
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
          V.fold f ~inf ~sup ~step acc
      | Set s ->
          O.fold f s acc

    let fold_enum f v acc =
      match v with
      | Float (fl) when F.is_singleton fl ->
	  f v acc
      | Top(_,_,_,_) | Float _ | Set _ ->
	  fold (fun x acc -> f (inject_singleton x) acc) v acc

	    (** [min_is_lower mn1 mn2] is true iff mn1 is a lower min than mn2 *)
    let min_is_lower mn1 mn2 =
      match mn1, mn2 with
	None, _ -> true
      | _, None -> false
      | Some m1, Some m2 ->
	  V.le m1 m2 

	    (** [max_is_greater mx1 mx2] is true iff mx1 is a greater max than mx2 *)
    let max_is_greater mx1 mx2 =
      match mx1, mx2 with
	None, _ -> true
      | _, None -> false
      | Some m1, Some m2 ->
	  V.ge m1 m2 

    let rem_is_included r1 m1 r2 m2 =
      (V.equal (V.rem m1 m2) V.zero) && (V.equal (V.rem r1 m2) r2)

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
	  F.is_included f1 f2
      | Float _, _ -> equal t2 top
      | _, Float (f) -> is_zero t1 && (F.contains_zero f)

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
	  inject_set (apply2 V.add s1 s2)
      | Top(mn1,mx1,r1,m1), Top(mn2,mx2,r2,m2) ->
	  let m = V.pgcd m1 m2 in
	  let r = V.rem (V.add r1 r2) m in
	  let mn =
	    try
	      Some (V.round_up_to_r (opt2 V.add mn1 mn2) r m)
	    with Infinity -> None
	  in
	  let mx =
	    try
	      Some (V.round_down_to_r (opt2 V.add mx1 mx2) r m)
	    with Infinity -> None
	  in
	  inject_top mn mx r m
      | Set s, (Top(mn1,mx1,r1,m1) as t) | (Top(mn1,mx1,r1,m1) as t), Set s ->
	  if O.is_empty s
	  then bottom
	  else let mn = O.min_elt s in
	  let mx = O.max_elt s in
	  if V.equal mn mx 
	  then (* only one element *)
	    let incr = V.add mn in
	    let new_mn = opt1 incr mn1 in
	    let new_mx = opt1 incr mx1 in
	    let new_r = V.pos_rem (incr r1) m1 in
	    check "add" new_mn new_mx new_r m1 ;
	    Top(new_mn, new_mx, new_r, m1)
	  else
	    add t (unsafe_make_top_from_set s)

	      (* TODO rename to neg_int *)
    let neg v =
      match v with
      | Float _ -> top
      | Set s -> Set (map_set V.neg s)
      | Top(mn,mx,r,m) ->
	  Top(opt1 V.neg mx, opt1 V.neg mn, V.pos_rem (V.neg r) m, m)

    let sub v1 v2 = add v1 (neg v2)

    type ext_value = Ninf | Pinf | Val of V.t
    let inject_min = function None -> Ninf | Some m -> Val m
    let inject_max = function None -> Pinf | Some m -> Val m
    let ext_neg = function Ninf -> Pinf | Pinf -> Ninf | Val v -> Val (V.neg v)
    let ext_mul x y =
      match x, y with
      | Ninf, Ninf | Pinf, Pinf -> Pinf
      | Ninf, Pinf | Pinf, Ninf -> Ninf
      | Val v1, Val v2 -> Val (V.mul v1 v2)
      | (x, Val v | Val v, x) when (V.gt v V.zero) -> x
      | (x, Val v | Val v, x) when (V.lt v V.zero) -> ext_neg x
      | _ -> Val V.zero

    let ext_min x y =
      match x,y with
	Ninf, _ | _, Ninf -> Ninf
      | Pinf, x | x, Pinf -> x
      | Val x, Val y -> Val(V.min x y)

    let ext_max x y =
      match x,y with
	Pinf, _ | _, Pinf -> Pinf
      | Ninf, x | x, Ninf -> x
      | Val x, Val y -> Val(V.max x y)

    let ext_proj = function Val x -> Some x | _ -> None

    let singleton_zero = zero
    let singleton_one = one
    let zero_or_one = join singleton_one singleton_zero

    let negative = Top(None, Some V.minus_one,V.zero,V.one)

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
	      if V.ge e min
	      then match !r with
	      | Some rr when V.lt e rr -> r := Some e
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
	  else V.round_up_to_r ~min ~r ~modu
      | Float _ -> raise No_such_element

    let largest_below max x =
      match x with
      | Float _ -> raise No_such_element
      | Set s ->
	  let r = ref None in
	  O.iter
	    (fun e ->
	      if V.le e max
	      then match !r with
	      | Some rr when V.gt e rr -> r := Some e
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
	  else V.round_down_to_r ~max ~r ~modu

	      (* [different_bits min min] returns the mask of the bits that can be different
		 for different numbers in the interval [min]..[max] *)
    let different_bits min max =
      let x = V.logxor min max in
      let x = V.logor x (V.shift_right x V.one) in
      let x = V.logor x (V.shift_right x V.two) in
      let rec f old =
	let x = V.logor old (V.shift_right old V.four) in
	if V.equal old x then x
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
	    V.pretty min1 V.pretty max1 V.pretty x1
	    V.pretty min2 V.pretty max2 V.pretty x2;*)
      if V.lt x1 x2
      then
	(*let diff = V.sub x2 x1 in*)
	let mask = V.lognot x2 in
	let forced = V.logand mask min1 in
	let part = V.logand forced min2 in
	if V.equal part forced
	then
	  V.min max1 max2
	else V.logor part x2
      else
	(*let diff = V.sub x1 x2 in*)
	let mask = V.lognot x1 in
	let forced = V.logand mask min2 in
	let part = V.logand forced min1 in
	if V.equal part forced
	then
	  V.min max1 max2
	else V.logor part x1


    let bitwise_and ~size v1 v2 =
      if is_bottom v1 || is_bottom v2
      then bottom
      else
	match v1, v2 with
	  Float _, _ | _, Float _ -> top
	| Top _, _ | _, Top _ ->
	    let half_range = V.power_two (pred size) in
	    let minint = V.neg half_range in
	    let vmax =
	      match max_int v1, max_int v2 with
		Some maxv1, Some maxv2 ->
		  if V.lt maxv1 V.zero && V.lt maxv2 V.zero
		  then begin
		    Some (match min_int v1, min_int v2 with
		      Some minv1, Some minv2 ->
			pos_max_land minv1 maxv1 minv2 maxv2
		    | _ -> assert false)
		  end
		  else
		    let max1 = (* improved min of maxv1 and maxv2*)
		      try
			let bi1 = smallest_above V.zero v1 in
			let bi2 = smallest_above V.zero v2 in
			pos_max_land bi1 maxv1 bi2 maxv2
		      with No_such_element -> minint
		    in
		    let max2 = (* improved min of maxv1 and altmax2*)
		      try
			let altmax2 =
			  V.add half_range (largest_below V.minus_one v2)
			in
			let bi1 = smallest_above V.zero v1 in
			let bi2 =
			  V.add half_range (smallest_above minint v2)
			in
			pos_max_land bi1 maxv1 bi2 altmax2
		      with No_such_element -> minint
		    in
		    let max3 = (* improved min of maxv2 and altmax1*)
		      try
			let altmax1 =
			  V.add half_range (largest_below V.minus_one v1)
			in
			let bi2 = smallest_above V.zero v2 in
			let bi1 =
			  V.add half_range (smallest_above minint v1)
			in
			pos_max_land bi2 maxv2 bi1 altmax1
		      with No_such_element -> minint
		    in
		    (*		    Format.printf "bitwise and v1 %a v2 %a maxv1 %a maxv2 %a max1 max2 max3 %a %a %a@."
		      pretty v1 pretty v2
		      V.pretty maxv1 V.pretty maxv2
		      V.pretty max1 V.pretty max2 V.pretty max3; *)
		    Some (V.max max1 (V.max max2 max3))
	      | _ -> None
	    in
	    let vmin =
	      if intersects v1 negative && intersects v2 negative
	      then Some minint
	      else some_zero
	    in
	    inject_top vmin vmax V.zero V.one
	| Set _, Set _ ->
	    (apply_set "&" V.logand) v1 v2

    let bitwise_or ~size:_ v1 v2 =
      if is_bottom v1 || is_bottom v2
      then bottom
      else
	match v1, v2 with
	  Float _, _ | _, Float _ -> top
	| Top _, _ | _, Top _ -> top
	| Set _, Set _ ->
	    (apply_set "|" V.logor) v1 v2

    let contains_non_zero v =
      match v with
      | Top _ | Float _ -> true
      | Set s -> O.exists (fun e -> not (V.equal V.zero e)) s

	    (* TODO: rename this function to scale_int *)
    let scale f v =
      let result =
	match v with
	| Float _ -> top
	| Top(mn1,mx1,r1,m1) ->
	    let incr = V.mul f in
	    if V.is_zero f
	    then singleton_zero
	    else if V.gt f V.zero 
	    then
	      let modu = incr m1 in
	      Top(opt1 incr mn1, opt1 incr mx1, V.pos_rem (incr r1) modu, modu)
	    else
	      let modu = V.neg (incr m1) in
	      Top(opt1 incr mx1, opt1 incr mn1, V.pos_rem (incr r1) modu, modu)
	| Set s -> Set (map_set (V.mul f) s)
      in
      (* Format.printf "scale: %a . %a -> %a@\n"
	 V.pretty f pretty v pretty result; *)
      result

    let scale_div ~pos f v =
      assert (not (V.equal f V.zero));
      let div_f =
	if pos
	then fun a -> V.pos_div a f
	else fun a -> V.c_div a f
      in
      match v with
      | Top(mn1,mx1,r1,m1) ->
	  let r, modu =
            if (V.is_zero (V.rem m1 f)) &&
	      ((V.is_zero (V.rem r1 f)) ||
	      (min_is_lower (some_zero) mn1) || (* all positive *)
	      (max_is_greater (some_zero) mx1) || (* all negative *)
	      pos                         (* good div *) )
            then
	      let modu = V.abs (div_f m1) in
	      (V.pos_rem (div_f r1) modu), modu
            else (* degeneration*)
	      V.zero, V.one
	  in
	  let divf_mn1 = opt1 div_f mn1 in
	  let divf_mx1 = opt1 div_f mx1 in
	  let mn, mx =
	    if V.gt f V.zero
	    then divf_mn1, divf_mx1
	    else divf_mx1, divf_mn1
	  in
	  inject_top mn mx r modu
      | Set s -> Set (map_set div_f s)
      | Float _ -> top


	    (* TODO : rename to div_int *)
    let div x y =
      let result =
	if contains_zero y
	then top
	else begin
          (*if (intersects y negative || intersects x negative)
            then ignore (CilE.warn_once "using 'round towards zero' semantics for '/', which only became specified in C99."); *)
          match y with
            Set sy ->
	      O.fold
	        (fun elt acc -> join acc (scale_div ~pos:false elt x))
	        sy
	        bottom
	  | Top (Some mn,Some mx, r, modu) ->
	      let mn_pos = V.gt mn V.zero in
	      let mx_pos = V.gt mx V.zero in
	      begin match min_and_max x with
		Some xmn, Some xmx ->	
		  let c1 = V.c_div xmn mn in
		  let c2 = V.c_div xmx mn in
		  let c3 = V.c_div xmn mx in
		  let c4 = V.c_div xmx mx in
		  let min = V.min (V.min c1 c2) (V.min c3 c4) in
		  let max = V.max (V.max c1 c2) (V.max c3 c4) in
		  if (mn_pos = mx_pos)
		  then begin
		    (*	      Format.printf "div: %a %a %a %a@."
		      V.pretty mn V.pretty mx V.pretty xmn V.pretty xmx; *)
		    inject_range (Some min) (Some max)
		  end 
		  else begin
		    let gneg = V.round_down_to_r ~max:V.zero ~r ~modu in
		    let lpos = V.round_up_to_r ~min:V.zero ~r ~modu in
(*		    Format.printf "div gneg:%a lpos:%a xmn:%a xmx:%a@."
		      V.pretty gneg V.pretty lpos
		      V.pretty xmn V.pretty xmx; *)
		    let c5 = V.c_div xmn lpos in
		    let c6 = V.c_div xmn gneg in
		    let c7 = V.c_div xmx lpos in
		    let c8 = V.c_div xmx gneg in
		    let min = V.min min (V.min (V.min c5 c6) (V.min c7 c8)) in
		    let max = V.max max (V.max (V.max c5 c6) (V.max c7 c8)) in
		    inject_range (Some min) (Some max)
		  end
	      | _ ->
		  CilE.warn_once "approximating result of division. Please report if it matters.";
		  top
	      end
          | Top _ | Float _->
	      CilE.warn_once "approximating result of division. Please report if it matters.";
	      top
	end
      in
      (*  Format.printf "div: %a / %a -> %a@\n"
          pretty x pretty y pretty result;*)
      result

	(* [scale_rem ~pos:false f v] is an over-approximation of the set of
	   elements [x mod f] for [x] in [v].

	   [scale_rem ~pos:true f v] is an over-approximation of the set of
	   elements [x pos_rem f] for [x] in [v].
	 *)

    let scale_rem ~pos f v =
      let f = if V.lt f V.zero then V.neg f else f in
      let rem_f a =
	if pos then V.pos_rem a f else V.c_rem a f
      in
      match v with
      | Top(mn,mx,r,m) ->
	  let modu = V.pgcd f m in
          let rr = V.pos_rem r modu in
          let binf,bsup =
	    if pos
	    then (V.round_up_to_r ~min:V.zero ~r:rr ~modu),
	      (V.round_down_to_r ~max:(V.pred f) ~r:rr ~modu)
            else
	      let min =
		if all_positives mn then V.zero else V.neg (V.pred f)
	      in
	      let max =
		if all_negatives mx then V.zero else V.pred f
	      in
              (V.round_up_to_r ~min ~r:rr ~modu,
	       V.round_down_to_r ~max ~r:rr ~modu)
          in
          let mn_rem,mx_rem =
            match mn,mx with
            | Some mn,Some mx ->
		let mn_rem = rem_f mn in
		let mx_rem = rem_f mx in
		(*   Format.printf "scale_rem 1:%a %a %a %b %b %a %a@."
		     V.pretty mn V.pretty mx V.pretty f
		     (V.lt mx f) (V.gt mn (V.neg f))
		     V.pretty mn_rem V.pretty mx_rem;*)
		if
		  ((V.lt (V.sub mx mn) f) || ((V.lt mx f) && (V.gt mn (V.neg f))))  &&
                  (V.le mn_rem mx_rem)
		then
		  ( (*Format.printf "scale_rem 2:%a %a %a %a@."
		      V.pretty mn V.pretty mx V.pretty mn_rem V.pretty mx_rem; *)

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
            Set _ -> apply_set "%" V.c_rem x y
	  | Float _ -> top
	  | Top _ ->
              let f y acc =
		join (scale_rem ~pos:false y x) acc
		  (*             let result_m = V.pgcd y m in
				 let r1 =
				 if min_is_lower mn (some_zero)
				 then
				 let v_rem = V.c_rem (V.sub r m) result_m in
				 let add_elts =
				 if not (V.equal y result_m)
				 then
				 inject_top
				 (Some (V.add result_m (V.sub v_rem y)))
				 (Some v_rem)
				 (V.pos_rem v_rem result_m)
				 result_m
				 else
				 inject_singleton v_rem
				 in
				 join add_elts acc
				 else acc
				 in
				 let r2 =
				 if max_is_greater mx (some_zero)
				 then
				 let v_rem = V.c_rem r result_m in
				 let add_elts =
				 if not (V.equal y result_m)
				 then
				 inject_top
				 (Some v_rem)
				 (Some (V.add y (V.sub v_rem result_m)))
				 v_rem
				 result_m
				 else
				 inject_singleton v_rem
				 in
				 join add_elts r1
				 else r1
				 in r2*)
              in
              O.fold f yy bottom)


    let v32 = V.of_int 32

    let cast ~size ~signed ~value =
      let result =
	let factor = V.shift_left V.one size in
	let mask = V.shift_left V.one (V.sub size V.one) in

	let rem_f a =
          if (not signed) then V.pos_rem a factor
          else
            if V.equal (V.logand mask a) V.zero 
            then V.logand a (V.pred mask)
            else
              V.logor (V.lognot (V.pred mask)) a
	in
	let not_p_factor = V.lognot (V.pred factor) in
	let best_effort r m =
          let modu = V.pgcd factor m in
          let rr = V.pos_rem r modu in
          let min_val = Some (if signed then
            V.round_up_to_r ~min:(V.neg mask) ~r:rr ~modu
          else
            V.round_up_to_r ~min:V.zero ~r:rr ~modu)
          in
          let max_val = Some (if signed then
            V.round_down_to_r ~max:(V.pred mask) ~r:rr ~modu
          else
            V.round_down_to_r ~max:(V.pred factor)
              ~r:rr
              ~modu)
          in
          inject_top min_val max_val rr modu
	in
	match value with
	| Top(Some mn,Some mx,r,m) ->
	    let highbits_mn,highbits_mx =
	      if signed then
		V.logand (V.add mn mask) not_p_factor,
		V.logand (V.add mx mask) not_p_factor
	      else
		V.logand mn not_p_factor, V.logand mx not_p_factor
	    in
	    if V.equal highbits_mn highbits_mx 
	    then
	      if V.is_zero highbits_mn
	      then value
	      else
		let new_min = rem_f mn in
		let new_r = V.pos_rem new_min m in
		inject_top (Some new_min) (Some (rem_f mx)) new_r m
	    else best_effort r m
	| Top (_,_,r,m) ->
	    best_effort r m
	| Set s -> inject_set (map_set rem_f s)
	| Float _ -> if V.ge size v32 then value else top
      in
      (*
	Format.printf "Cast with size:%d signed:%b to %a@\n"
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
	    inject_set (apply2 V.mul s1 s2)
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

	    (*	let multipl1 = V.pgcd m1 r1 in
	      let multipl2 = V.pgcd m2 r2 in
	      let modu1 = V.pgcd m1 m2 in
	      let modu2 = V.mul multipl1 multipl2 in
	      let modu = V.ppcm modu1 modu2 in	*)
	    let modu = V.pgcd (V.pgcd (V.mul m1 m2) (V.mul r1 m2)) (V.mul r2 m1)
	    in
	    let r = V.rem (V.mul r1 r2) modu in
	    (*	let t = Top (ext_proj min, ext_proj max, r, modu) in
	      Format.printf "mul. Result: '%a'@\n" pretty t; *)
	    inject_top (ext_proj min) (ext_proj max) r modu
	| Set s, (Top(_,_,_,_) as t) | (Top(_,_,_,_) as t), Set s ->
	    if O.is_empty s
	    then bottom
	    else let mn = O.min_elt s in
	    let mx = O.max_elt s in
	    if V.equal mn mx 
	    then (* only one element *)
	      scale mn t
	    else mul t (unsafe_make_top_from_set s)
      in
      (* Format.printf "mul. result : %a@\n" pretty result;*)
      result

    let shift_left ~size x y =
      try
	let min = smallest_above V.zero y in
	let min = V.shift_left V.one min in
	let max = largest_below (V.pred size) y in
	let max = V.shift_left V.one max in
	let factor = inject_top (Some min) (Some max) V.zero min in
	(*      Format.printf "shift_left %a factor:%a@." pretty y pretty factor; *)
	mul factor x
      with No_such_element ->
	bottom

    let shift_right ~size x y =
      let result =
	try
	  let min = smallest_above V.zero y in
	  let max = largest_below (V.pred size) y in
	  V.fold
            (fun n acc -> join acc (scale_div ~pos:true
                                      (V.shift_left V.one n) x))
            ~inf:min ~sup:max ~step:V.one
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

	    (* keep all elements that are in relation [f] ([<=],[>=],...) to [bound] *)
    let filter_set f bound s =
      Set
	(O.fold
           (fun v acc ->
             if f (V.compare v bound)
             then O.add v acc
             else acc)
           s
           O.empty)

    let filter_le_int max v =
      match v with
      | Float _ -> v
      | Set _ | Top _ ->
          narrow v (Top(None,max,V.zero,V.one))
    let filter_ge_int min v =
      match v with
      | Float _ -> v
      | Set _ | Top _ ->
          narrow v (Top(min,None,V.zero,V.one))
    let filter_lt_int max v = filter_le_int (opt1 V.pred max) v
    let filter_gt_int min v = filter_ge_int (opt1 V.succ min) v

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
	F.Nan_or_infinite -> v1
      | F.Bottom -> bottom

    let filter_le_float = filter_float F.filter_le
    let filter_ge_float = filter_float F.filter_ge
    let filter_lt_float = filter_float F.filter_lt
    let filter_gt_float = filter_float F.filter_gt

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
              Some (V.add min modu)
          | _ -> min
          in
          let new_max = match max with
          | Some max when O.mem max s ->
              changed := true;
              Some (V.sub max modu)
          | _ -> max
          in
          if !changed then
            diff (inject_top new_min new_max r modu) rem
          else value
      | Top(_min1, _max1, _r1, _modu1), Top(_min2, _max2, _r2, _modu2) ->
          value  (* TODO : can do better *)
      | Float f1, Float f2 -> inject_float (F.diff f1 f2)
      | Float _ , _ | _, Float _ -> value

    let diff_if_one value rem =
      if not (cardinal_zero_or_one rem)  then
	value
      else diff value rem

    let extract_bits ~start ~stop v =
      match v with
      | Set s ->
          Set
            (O.fold
               (fun elt acc -> O.add (V.extract_bits ~start ~stop elt) acc)
               s
               O.empty)
      | Top _ | Float _ ->
          inject_top
            some_zero
            (Some (V.pred (V.power_two (V.to_int (V.length start stop)))))
            V.zero
            V.one

    let b64 = V.of_int 64

    let create_all_values ~modu ~size =
      let b = V.power_two (size-1) in
      Top(Some (V.round_up_to_r ~min:(V.neg b) ~modu ~r:V.zero),
	  Some (V.round_down_to_r ~max:(V.pred b) ~modu ~r:V.zero),
	  V.zero,
	  modu)

    let all_values ~size v =
      if V.le b64 size then false
	  (* values of this size cannot be enumerated anyway in C.
             They may occur while initializing large blocks of arrays.
	   *)
      else
	let c =
	  match v with
	  | Float _ -> false
	  | Top (None,_,_,modu) | Top (_,None,_,modu) ->
              V.eq V.one modu
	  | Top (Some mn, Some mx,_,modu) ->
              V.eq V.one modu &&
              V.le
		(V.power_two (V.to_int size))
		(V.succ (V.sub mx mn))
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
		(O.fold (fun x -> O.add (V.Datatype.rehash x)) set O.empty)
	  | Float _ as f -> f
		(*	       inject_float ((F.Datatype.rehash f1) (F.Datatype.rehash f2)*)
	  | Top(mn, mx, r, modu) ->
	      inject_top
		(Extlib.opt_map V.Datatype.rehash mn)
		(Extlib.opt_map V.Datatype.rehash mx)
		(V.Datatype.rehash r)
		(V.Datatype.rehash modu)
	  include Datatype.Nop
	  let name = Project.Datatype.Name.extend "lattice_mod" V.Datatype.name
	  let dependencies = [ V.Datatype.self ]
	end)

  end

module Make_Pair (V:Value) (W:Value)=
struct

  type t = V.t*W.t

  let compare (a,b as p) (a',b' as p') =
    if p==p' then 0 else
      let c = V.compare a a' in
      if c=0 then W.compare b b' else c

  let pretty fmt (a,b) =
    Format.fprintf fmt "(%a,%a)" V.pretty a W.pretty b

  module Datatype =
    Project.Datatype.Register
      (struct
	type tt = t
	type t = tt
	let copy _ = assert false (* TODO *)
	let rehash (v, w) = V.Datatype.rehash v, W.Datatype.rehash w
	include Datatype.Nop
	let name =
	  Project.Datatype.Name.extend2 "lattice_pair"
	    V.Datatype.name W.Datatype.name
	let dependencies = [ V.Datatype.self; W.Datatype.self ]
      end)

end

let rec compare_list compare_elt l1 l2 =
  if l1==l2 then 0 else
    match l1,l2 with
      | [], _ -> 1
      | _, [] -> -1
      | v1::r1,v2::r2 ->
          let c = compare_elt v1 v2 in
          if c=0 then compare_list compare_elt r1 r2 else c

module Make_Lattice_Interval_Set (V:Arithmetic_Value) =
struct
  exception Error_Top
  exception Error_Bottom
  module Interval = Make_Pair (V) (V)
  type elt = Interval.t

  type tt = Top | Set of elt list
  type t = tt

  type widen_hint = unit

  let bottom = Set []
  let top = Top

  let check t =
    assert (match t with
            | Top -> true
            | Set s ->
                let last_stop = ref None in
                List.for_all
                  (fun (a,b) -> V.compare a b <= 0 &&
                     match !last_stop with
                       None -> last_stop := Some b; true
                     | Some l -> last_stop := Some b; V.gt a l)
                  s) ;
    t

  let hash l = match l with
    Top -> 667
  | Set l ->
      List.fold_left
	(fun acc (b,e) -> 371 * acc + (V.hash b) + 1351 * (V.hash e))
	443
	l

  let tag = hash

  let cardinal_zero_or_one v =
    match v with
      Top -> false
    | Set [x,y] -> V.eq x y
    | Set _ -> false

  let cardinal_less_than v n =
    match v with
      Top -> raise Not_less_than
    | Set l ->
	let rec aux l card = match l with
	  [] -> card
	| (x,y)::t ->
	    let nn = V.of_int n in
	    let card = V.add card ((V.succ (V.sub y x))) in
	    if V.gt card nn
	    then raise Not_less_than
	    else aux t card
	in
	V.to_int (aux l V.zero)

  let compare e1 e2 =
    if e1==e2 then 0 else
      match e1,e2 with
        | Top,_ -> 1
        | _, Top -> -1
        | Set e1, Set e2 ->
            compare_list Interval.compare e1 e2

  let equal e1 e2 = compare e1 e2 = 0

  let pretty fmt t =
    match t with
      | Top -> Format.fprintf fmt "TopISet"
      | Set s ->
          if s=[] then Format.fprintf fmt "BottomISet"
          else begin
            Format.fprintf fmt "{%a}"
              (fun fmt s ->
		 List.iter
                   (fun (b,e) ->
                      Format.fprintf
			fmt
			"[%a..%a]; "
			V.pretty b
			V.pretty e
                   )
                   s)
              s
          end

  let widen _wh t1 t2  =  (if equal t1 t2 then t1 else top)

  let meet v1 v2 =
    if v1 == v2 then v1 else

	(match v1,v2 with
	 | Top, v | v, Top -> v
	 | Set s1 , Set s2 -> Set (
	     let rec aux acc (l1:elt list) (l2:elt list) = match l1,l2 with
	     | [],_|_,[] -> List.rev acc
             | (((b1,e1)) as i1)::r1,
		 (((b2,e2)) as i2)::r2 ->
		 let c = V.compare b1 b2 in
		 if c = 0 then (* intervals start at the same value *)
                   let ce = V.compare e1 e2 in
                   if ce=0 then
                     aux ((b1,e1)::acc) r1 r2 (* same intervals *)
                   else
                     (* one interval is included in the other *)
                     let min,not_min,min_tail,not_min_tail =
                       if ce > 0 then i2,i1,r2,r1 else
			 i1,i2,r1,r2
                     in
                     aux ((min)::acc) min_tail
                       (((
                           (snd (min),
                            snd (not_min))))::
                          not_min_tail)
		 else (* intervals start at different values *)
                   let _min,min_end,not_min_begin,min_tail,not_min_from =
                     if c > 0
                     then b2,e2,b1,r2,l1
                     else b1,e1,b2,r1,l2
                   in
                   let c_min = V.compare min_end not_min_begin in
                   if c_min >= 0 then
                     (* intersecting intervals *)
                     aux acc
		       ((
			  (not_min_begin,min_end))
			::min_tail)
                       not_min_from
                   else
                     (* disjoint intervals *)
                     aux acc min_tail not_min_from
             in aux [] s1 s2))

  let join v1 v2 =
    if v1 == v2 then v1 else
      (match v1,v2 with
       | Top, _ | _, Top -> Top
       | Set (s1:elt list) , Set (s2:elt list) ->
	   let rec aux (l1:elt list) (l2:elt list) = match l1,l2 with
           | [],l|l,[] -> l
           | (b1,e1)::r1,(b2,e2)::r2 ->
               let c = V.compare b1 b2 in
               let min_begin,min_end,min_tail,not_min_from =
                 if c >= 0 then b2,e2,r2,l1
                 else b1,e1,r1,l2
               in
               let rec enlarge_interval stop l1 look_in_me =
                 match look_in_me with
                 | [] -> stop,l1,[]
                 | ((b,e))::r ->
                     if V.compare stop (V.pred b) >= 0
                     then
                       if V.compare stop e >= 0
                       then enlarge_interval  stop l1 r
                       else enlarge_interval  e r l1
                     else stop,l1,look_in_me
               in
               let stop,new_l1,new_l2 =
                 enlarge_interval
                   min_end
                   min_tail
                   not_min_from
               in ((min_begin,stop))::
                    (aux new_l1 new_l2)
	   in Set (aux s1 s2))

  let inject l =  (Set l)

  let inject_one ~size ~value =
    (inject [value,V.add value (V.pred size)])

  let inject_bounds min max =
    if V.le min max
    then inject [min,max]
    else bottom

  let transform _f = (* f must be non-decreasing *)
    assert false

  let apply2 _f _s1 _s2 = assert false

  let apply1 _f _s = assert false

  let is_included t1 t2 =
    (t1 == t2) ||
      match t1,t2 with
      | _,Top -> true
      | Top,_ -> false
      | Set s1,Set s2 ->
          let rec aux l1 l2 = match l1 with
          | [] -> true
          | i::r ->
              let rec find (b,e as arg) l =
		match l with
		| [] -> raise Not_found
		| (b',e')::r ->
                    if V.compare b b' >= 0
                      && V.compare e' e >= 0
                    then  l
                    else if V.compare e' b >= 0 then
                      raise Not_found
                    else find arg r
              in
              try aux r (find i l2)
              with Not_found -> false
          in
          aux s1 s2

  let link t1 t2 = join t1 t2 (* join is in fact an exact union *)

  let is_included_exn v1 v2 =
    if not (is_included v1 v2) then raise Is_not_included

  let intersects t1 t2 =
    let m = meet t1 t2 in
    not (equal m bottom)

  let fold f v acc =
    match v with
      | Top -> raise Error_Top
      | Set s ->
          List.fold_right f s acc

  let narrow = meet

  module Datatype =
    Project.Datatype.Register
      (struct
	 type t = tt
	 let copy _ = assert false (* TODO *)
	 let rehash x = match x with
	   | Top -> x
	   | Set l -> inject (List.map Interval.Datatype.rehash l)
	 include Datatype.Nop
	 let name =
	   Project.Datatype.Name.extend "lattice_interval_set"
	     Interval.Datatype.name
	 let dependencies = [ Interval.Datatype.self ]
       end)

end

module Make_Lattice_Base (V:Value):(Lattice_Base with type l = V.t)=
struct
  type l = V.t
  type tt = Top | Bottom | Value of l
  type t = tt
  type y = l
  type widen_hint = V.t list

  let bottom = Bottom
  let top = Top

  exception Error_Top
  exception Error_Bottom
  let project v = match v with
    | Top  -> raise Error_Top
    | Bottom -> raise Error_Bottom
    | Value v -> v


  let cardinal_zero_or_one v = match v with
    | Top  -> false
    | _ -> true

  let cardinal_less_than v n = match v with
    | Top  -> raise Not_less_than
    | Value _ -> if n >= 1 then 1 else raise Not_less_than
    | Bottom -> 0

  let compare e1 e2 =
    if e1==e2 then 0 else
      match e1,e2 with
        | Top,_ -> 1
        | _, Top -> -1
        | Bottom, _ -> -1
        | _, Bottom -> 1
        | Value e1,Value e2 -> V.compare e1 e2
  let equal v1 v2 = compare v1 v2 = 0

  let tag = function
    | Top -> 3
    | Bottom -> 5
    | Value v -> V.hash v * 7

  let widen _wh t1 t2 = (* [wh] isn't used yet *)
    if equal t1 t2 then t1 else top

  (** This is exact *)
  let meet b1 b2 =
    if b1 == b2 then b1 else
    match b1,b2 with
    | Bottom, _ | _, Bottom -> Bottom
    | Top , v | v, Top -> v
    | Value v1, Value v2 -> if (V.compare v1 v2)=0 then b1 else Bottom

  (** This is exact *)
  let narrow = meet

  (** This is exact *)
  let join b1 b2 =
    if b1 == b2 then b1 else
      match b1,b2 with
      | Top, _ | _, Top -> Top
      | Bottom , v | v, Bottom -> v
      | Value v1, Value v2 -> if (V.compare v1 v2)=0 then b1 else Top

  (** This is exact *)
  let link = join

  let inject x = Value x

  let transform f = fun t1 t2 ->
    match t1,t2 with
      | Bottom, _ | _, Bottom -> Bottom
      | Top, _ | _, Top -> Top
      | Value v1, Value v2 -> Value (f v1 v2)

  let pretty fmt t =
    match t with
      | Top -> Format.fprintf fmt "Top"
      | Bottom ->  Format.fprintf fmt "Bottom"
      | Value v -> Format.fprintf fmt "<%a>" V.pretty v

  let is_included t1 t2 =
    let b = (t1 == t2) ||
      (equal (meet t1 t2) t1)
    in
    (* Format.printf
       "[Lattice]%a is included in %a: %b @\n"
       pretty t1 pretty t2 b;*)
    b

  let is_included_exn v1 v2 =
    if not (is_included v1 v2) then raise Is_not_included

  let intersects t1 t2 = not (equal (meet t1 t2) Bottom)

  module Datatype =
    Project.Datatype.Register
      (struct
	 type t = tt
	 let copy _ = assert false (* TODO *)
	 let rehash x = match x with
	   | Top | Bottom -> x
	   | Value v -> inject (V.Datatype.rehash v)
	 include Datatype.Nop
	 let name = Project.Datatype.Name.extend "lattice_base" V.Datatype.name
	 let dependencies = [ V.Datatype.self ]
       end)

end

module Int = struct
  open My_bigint
  type t = big_int

  module Datatype = Datatype.BigInt

  let zero = zero_big_int
  let one = unit_big_int
  let two = succ_big_int one
  let four = big_int_of_int 4
  let rem = mod_big_int
  let div = div_big_int
  let mul = mult_big_int
  let sub = sub_big_int
  let abs = abs_big_int
  let succ = succ_big_int
  let pred = pred_big_int
  let neg = minus_big_int
  let add = add_big_int

  let billion_one = big_int_of_int 1000000001
  let hash c =
    let i =
      try
	int_of_big_int c
      with Failure _ -> int_of_big_int (rem c billion_one)
    in
    197 + i

  let tag = hash
  let equal a b = eq_big_int a b

  let log_shift_right = log_shift_right_big_int
  let shift_right = shift_right_big_int
  let shift_left = shift_left_big_int

  let logand = land_big_int
  let lognot = lnot_big_int
  let logor = lor_big_int
  let logxor = lxor_big_int

  let le = le_big_int
  let lt = lt_big_int
  let ge = ge_big_int
  let gt = gt_big_int
  let eq = eq_big_int
  let neq x y = not (eq_big_int x y)

  let to_int v =
    try int_of_big_int v
    with Failure "int_of_big_int" -> assert false
  let of_int i = big_int_of_int i
  let of_int64 i = big_int_of_string (Int64.to_string i)
  let to_int64 i = Int64.of_string (string_of_big_int i)
  let of_string = big_int_of_string
  let to_string = string_of_big_int
  let to_float = float_of_big_int
  let of_float _ = assert false

  let minus_one = pred zero
  let pretty fmt i = Format.fprintf fmt "%s" (string_of_big_int i)
  let pretty_debug = pretty

  let pretty_s () = string_of_big_int
  let is_zero v = (sign_big_int v) = 0

  let compare = compare_big_int
  let is_one v = eq one v
  let pos_div  = div
  let pos_rem = rem
  let native_div = div
  let c_div u v =
    let bad_div = div u v in
    if (lt u zero) && neq zero (rem u v) then
      if lt v zero
      then pred bad_div
      else succ bad_div
    else bad_div
  let c_rem u v =
    sub u (mul v (c_div u v))

  let power_two n = power_int_positive_int 2 n

  let extract_bits ~start ~stop v =
    assert (ge start zero && ge stop start);
    (*Format.printf "%a[%a..%a]@\n" pretty v pretty start pretty stop;*)
    let r = bitwise_extraction (to_int start) (to_int stop) v in
      (*Format.printf "%a[%a..%a]=%a@\n" pretty v pretty start pretty stop pretty r;*)
      r

      (*

        include Int64
        let pretty fmt i = Format.fprintf fmt "%Ld" i
        let pretty_s () i = Format.sprintf  "%Ld" i
        let is_zero v = 0 = (compare zero v)
        let lt = (<)
        let le = (<=)
        let neq = (<>)
        let eq = (=)
        let gt = (>)
        let ge = (>=)

        let shift_left x y = shift_left x (to_int y)
        let shift_right x y = shift_right x (to_int y)
        let log_shift_right x y =  shift_right_logical x (to_int y)
        let of_int64 x = x
        let to_int64 x = x

        let pos_div u v =
        let bad_div = div u v in
        let bad_rem = rem u v in
        if compare bad_rem zero >= 0
        then bad_div
        else (sub bad_div one)

        let pos_rem x m =
        let r = rem x m in
        if lt r zero then add r m (* cannot overflow because r and m
        have different signs *)
        else r

        let c_div = div

      *)

  let is_even v = is_zero (logand one v)

  (** [pgcd u 0] is allowed and returns [u] *)
  let pgcd u v =
    let rec spec_pgcd u v = (* initial implementation viewed as the spec *)
      if is_zero v
      then u
      else spec_pgcd v (rem u v) in
  (*  let abs_max_min u v =
         if compare (abs u) (abs v) >= 0 then (u, v) else (v, u) in
    let impl_pgcd u v =
      let rec ordered_pgcd (u, v) =
        assert (compare (abs u) (abs v) >= 0); (* [pgcd (u, v)] such as (abs v) <= (abs u) *)
        if is_zero v
        then u
        else (* both differ from zero *)
          let rec none_zero_pcgd u v =
            assert (not (is_zero u));
            assert (not (is_zero v));
            let (u, v) = abs_max_min u v in
              ordered_pgcd (v, (rem u v)) in
            match (is_even u, is_even v) with
                (true, true) -> (* both are even: pgcd(2*a,2*b)=2*pgcd(a,b) *)
                  let u_div_2 = shift_right u one
                  and v_div_2 = shift_right v one
                  in shift_left (ordered_pgcd (v_div_2, (rem u_div_2 v_div_2))) one
              | (true, false) -> (* only u is even: pgcd(2*a,2*b+1)=pgcd(a,2*b+1) *)
                  let u_div_2 = shift_right u one
                  in none_zero_pcgd v u_div_2
              | (false, true) -> (* only v is even: pgcd(2*a,2*b+1)=pgcd(a,2*b+1) *)
                  let v_div_2 = shift_right v one
                  in none_zero_pcgd v_div_2 u
              | (false, false) -> (* none are even *)
                  ordered_pgcd (v, (rem u v)) in
        ordered_pgcd (abs_max_min u v) in
  *)
    (* let r = impl_pgcd u v in *)
    let r =
      if is_zero v
      then u
      else (* impl_pgcd *) gcd_big_int u v in
    assert (0 = (compare r (spec_pgcd v u))) ; (* compliance with the spec *)
      r


  let ppcm u v =
    if u = zero || v = zero
    then zero
    else native_div (mul u v) (pgcd u v)

  let length u v = succ (sub v u)

  let min u v     = if compare u v  >= 0 then v else u
  let max u v     = if compare u v  >= 0 then u else v

  let round_down_to_zero v modu =
    mul (pos_div v modu) modu

  (** [round_up_to_r m r modu] is the smallest number [n] such that
	 [n]>=[m] and [n] = [r] modulo [modu] *)
  let round_up_to_r ~min:m ~r ~modu =
    add (add (round_down_to_zero (pred (sub m r)) modu) r) modu

  (** [round_down_to_r m r modu] is the largest number [n] such that
     [n]<=[m] and [n] = [r] modulo [modu] *)
  let round_down_to_r ~max:m ~r ~modu =
    add (round_down_to_zero (sub m r) modu) r

  (** execute [f] on [inf], [inf + step], ... *)
  let fold f ~inf ~sup ~step acc =
(*    Format.printf "Int.fold: inf:%a sup:%a step:%a@\n"
       pretty inf pretty sup pretty step; *)
    let nb_loop = div (sub sup inf) step in
    let next = add step in
    let rec fold ~counter ~inf acc =
        if eq counter (of_int 1000) then
          CilE.warn_once "enumerating %s integers" (to_string nb_loop);
        if le inf sup
	then begin
(*          Format.printf "Int.fold: %a@\n" pretty inf; *)
          fold ~counter:(succ counter) ~inf:(next inf) (f inf acc)
	  end
        else acc
      in
    fold ~counter:zero ~inf acc

end

module type Key = sig
  include Map.OrderedType
  val pretty : Format.formatter -> t -> unit
  val is_null : t -> bool
  val null : t
  val hash : t -> int
  val id : t -> int
  module Datatype : Project.Datatype.OUTPUT with type t = t
end

module VarinfoSetLattice = Make_Lattice_Set
  (struct
     type t = Cil_types.varinfo
     module Datatype = Kernel_datatype.Varinfo
     let compare v1 v2 = compare v1.Cil_types.vid v2.Cil_types.vid
     let pretty fmt v = Format.fprintf fmt "@[%a@]"
       !Ast_printer.d_ident v.Cil_types.vname
     let hash v = v.Cil_types.vid
   end)

module LocationSetLattice = struct
  include Make_Lattice_Set
    (struct
       type t = Cil_types.location
       module Datatype = Kernel_datatype.Location
       let compare = Pervasives.compare
       let pretty fmt (b,e) =
         Format.fprintf fmt "@[%s:%d@]" b.Lexing.pos_fname b.Lexing.pos_lnum
       let hash (b,e) = Hashtbl.hash (b.Lexing.pos_fname,b.Lexing.pos_lnum)
     end)

  let currentloc_singleton () =
   inject_singleton !Cil.currentLoc
end

module type Collapse = sig
  val collapse : bool
end

(** If [C.collapse] then [L1.Bottom,_ = _,L2.Bottom = Bottom] *)
module Make_Lattice_Product (L1:Lattice) (L2:Lattice) (C:Collapse):
  (Lattice_Product with type t1 =  L1.t and type t2 = L2.t) =
struct
  exception Error_Top
  exception Error_Bottom
  type t1 = L1.t
  type t2 = L2.t
  type tt = Product of t1*t2 | Bottom
  type t = tt
  type widen_hint = L1.widen_hint * L2.widen_hint

  let tag = function
    | Bottom -> 3
    | Product(v1, v2) -> L1.tag v1 + 3 * L2.tag v2

  let cardinal_less_than _ = assert false

  let cardinal_zero_or_one v = match v with
    | Bottom -> true
    | Product (t1, t2) ->
	(L1.cardinal_zero_or_one t1) &&
	  (L2.cardinal_zero_or_one t2)

(*  let compare x x' =
    if x == x' then 0 else
      match x,x' with
      | Bottom, Bottom -> 0
      | Bottom, Product _ -> 1
      | Product _,Bottom -> -1
      | (Product (a,b)), (Product (a',b')) ->
	  let c = L1.compare a a' in
	  if c = 0 then L2.compare b b' else c
*)
  let equal x x' =
    if x == x' then true else
      match x,x' with
      | Bottom, Bottom -> true
      | Bottom, Product _ -> false
      | Product _,Bottom -> false
      | (Product (a,b)), (Product (a',b')) ->
	  L1.equal a a'  && L2.equal b b'

  let top = Product(L1.top,L2.top)

  let bottom = Bottom

  let fst x = match x with
    Bottom -> L1.bottom
  | Product(x1,_) -> x1

  let snd x = match x with
    Bottom -> L2.bottom
  | Product(_,x2) -> x2

  let condition_to_be_bottom x1 x2 =
    let c1 = (L1.equal x1 L1.bottom)  in
    let c2 = (L2.equal x2 L2.bottom)  in
    (C.collapse && (c1 || c2)) || (not C.collapse && c1 && c2)

  let inject x y =
    if condition_to_be_bottom x y
    then bottom
    else Product(x,y)

  let widen (wh1, wh2) t l =
    let t1 = fst t in
    let t2 = snd t in
    let l1 = fst l in
    let l2 = snd l in
    inject (L1.widen wh1 t1 l1) (L2.widen wh2 t2 l2)

  let join x1 x2 =
    if x1 == x2 then x1 else
      match x1,x2 with
      | Bottom, v | v, Bottom -> v
      | Product (l1,ll1), Product (l2,ll2) ->
	  Product(L1.join l1 l2, L2.join ll1 ll2)

  let link _ = assert false (** Not implemented yet. *)

  let narrow _ = assert false (** Not implemented yet. *)

  let meet x1 x2 =
    if x1 == x2 then x1 else
    match x1,x2 with
    | Bottom, _ | _, Bottom -> Bottom
    | Product (l1,ll1), Product (l2,ll2) ->
	let l1 = L1.meet l1 l2 in
	let l2 = L2.meet ll1 ll2 in
        inject l1 l2

  let pretty fmt x =
    match x with
      Bottom ->
	Format.fprintf fmt "BotProd"
    | Product(l1,l2) ->
	Format.fprintf fmt "(%a,%a)" L1.pretty l1 L2.pretty l2

  let intersects  x1 x2 =
    match x1,x2 with
    | Bottom, _ | _, Bottom -> false
    | Product (l1,ll1), Product (l2,ll2) ->
	(L1.intersects l1 l2) && (L2.intersects ll1 ll2)

  let is_included x1 x2 =
    (x1 == x2) ||
    match x1,x2 with
    | Bottom, _ -> true
    | _, Bottom -> false
    | Product (l1,ll1), Product (l2,ll2) ->
	(L1.is_included l1 l2) && (L2.is_included ll1 ll2)

  let is_included_exn x1 x2 =
    if x1 != x2
    then
      match x1,x2 with
      | Bottom, _ -> ()
      | _, Bottom -> raise Is_not_included
      | Product (l1,ll1), Product (l2,ll2) ->
	  L1.is_included_exn l1 l2;
	  L2.is_included_exn ll1 ll2

  let transform _f (_l1,_ll1) (_l2,_ll2) =
    raise (Invalid_argument "Abstract_interp.Make_Lattice_Product.transform")

  module Datatype =
    Project.Datatype.Register
      (struct
	 type t = tt
	 let copy _ = assert false (* TODO *)
	 let rehash x = match x with
	   | Bottom -> x
	   | Product(v1, v2) ->
	       inject (L1.Datatype.rehash v1) (L2.Datatype.rehash v2)
	 include Datatype.Nop
	 let name =
	   Project.Datatype.Name.extend2 "lattice_product"
	     L1.Datatype.name L2.Datatype.name
	 let dependencies = [ L1.Datatype.self; L2.Datatype.self ]
       end)

end

module Make_Lattice_Sum (L1:Lattice) (L2:Lattice):
  (Lattice_Sum with type t1 = L1.t and type t2 = L2.t)
  =
struct
  exception Error_Top
  exception Error_Bottom
  type t1 = L1.t
  type t2 = L2.t
  type sum = Top | Bottom | T1 of t1 | T2 of t2
  type t = sum
  type y = t
  type widen_hint = L1.widen_hint * L2.widen_hint

  let top = Top
  let bottom = Bottom

  let tag = function
    | Top -> 3
    | Bottom -> 5
    | T1 t -> 7 * L1.tag t
    | T2 t -> - 17 * L2.tag t

  let cardinal_less_than _ = assert false

  let cardinal_zero_or_one v = match v with
    | Top  -> false
    | Bottom -> true
    | T1 t1 -> L1.cardinal_zero_or_one t1
    | T2 t2 -> L2.cardinal_zero_or_one t2

  let widen (wh1, wh2) t1 t2 =
    match t1,t2 with
      | T1 x,T1 y ->
          T1 (L1.widen wh1 x y)
      | T2 x,T2 y ->
          T2 (L2.widen wh2 x y)
      | Top,Top | Bottom,Bottom -> t1
      | _,_ -> Top

(*  let compare u v =
    if u == v then 0 else
      match u,v with
      | Top,Top | Bottom,Bottom -> 0
      | Bottom,_ | _,Top -> 1
      | Top,_ |_,Bottom -> -1
      | T1 _ , T2 _ -> 1
      | T2 _ , T1 _ -> -1
      | T1 t1,T1 t1' -> L1.compare t1 t1'
      | T2 t1,T2 t1' -> L2.compare t1 t1'
*)

  let equal _x _y = assert false (* todo *)

  let inject _ = assert false

  (** Forbid [L1 Bottom] *)
  let inject_t1 x =
    if L1.equal x L1.bottom then Bottom
    else T1 x

  (** Forbid [L2 Bottom] *)
  let inject_t2 x =
    if L2.equal x L2.bottom then Bottom
    else T2 x

  let pretty fmt v =
    match v with
      | T1 x -> L1.pretty fmt x
      | T2 x -> L2.pretty fmt x
      | Top -> Format.fprintf fmt "<TopSum>"
      | Bottom -> Format.fprintf fmt "<BottomSum>"

  let join u v =
    if u == v then u else
      match u,v with
      | T1 t1,T1 t2 -> T1 (L1.join t1 t2)
      | T2 t1,T2 t2 -> T2 (L2.join t1 t2)
      | Bottom,x| x,Bottom -> x
      | _,_ ->
          (*Format.printf
            "Degenerating collision : %a <==> %a@\n" pretty u pretty v;*)
          top

  let link _ = assert false (** Not implemented yet. *)

  let narrow _ = assert false (** Not implemented yet. *)

  let meet u v =
    if u == v then u else
    match u,v with
      | T1 t1,T1 t2 -> inject_t1 (L1.meet t1 t2)
      | T2 t1,T2 t2 -> inject_t2 (L2.meet t1 t2)
      | (T1 _ | T2 _),Top -> u
      | Top,(T1 _ | T2 _) -> v
      | Top,Top -> top
      | _,_ -> bottom

  let intersects u v =
    match u,v with
      | Bottom,_ | _,Bottom -> false
      | Top,_ |_,Top -> true
      | T1 _,T1 _ -> true
      | T2 _,T2 _ -> true
      | _,_ -> false

  let is_included u v =
    (u == v) ||
    let b = match u,v with
    | Bottom,_ | _,Top -> true
    | Top,_ | _,Bottom -> false
    | T1 t1,T1 t2 -> L1.is_included t1 t2
    | T2 t1,T2 t2 -> L2.is_included t1 t2
    | _,_ -> false
    in
    (* Format.printf
      "[Lattice_Sum]%a is included in %a: %b @\n" pretty u pretty v b;*)
    b

  let is_included_exn v1 v2 =
    if not (is_included v1 v2) then raise Is_not_included

  let transform _f _u _v = assert false

  module Datatype =
    Project.Datatype.Register
      (struct
	 type t = sum
	 let copy _ = assert false (* TODO *)
	 let rehash x = match x with
	   | Top | Bottom -> x
	   | T1 v -> inject_t1 (L1.Datatype.rehash v)
	   | T2 v -> inject_t2 (L2.Datatype.rehash v)
	 include Datatype.Nop
	 let name =
	   Project.Datatype.Name.extend2 "lattice_sum"
	     L1.Datatype.name L2.Datatype.name
	 let dependencies = [ L1.Datatype.self; L2.Datatype.self ]
       end)

end
