(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2017                                               *)
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

open Cil_types
open Eval
open Cvalue
open Abstract_interp

let store_redundant = false

(** Auxiliary functions *)

let default size =
  let default_v = V_Or_Uninitialized.initialized V.singleton_zero in
  V_Offsetmap.create_isotropic ~size default_v

(* This function creates a dummy validity, sufficient to read [size]
   bits starting from [start] *)
let enough_validity ~start ~size =
  Base.Known (Int.zero, Int.(add start (pred size)))

(* copy [size] bits from [start] in [o]. If [o] has size [size_o],
   [size+start <= size_o] must hold. *)
let basic_copy ?(start=Int.zero) ~size o =
  let validity = enough_validity ~start ~size in
  let offsets = Ival.inject_singleton start in
  match V_Offsetmap.copy_slice ~validity ~offsets ~size o with
  | _, `Bottom -> assert false
  | _, `Value r -> r

(* paste [src] of size [size_src] starting at [start] in [r]. If [r] has size
   [size_r], [size+start <= size_r] must hold. *)
let basic_paste ?(start=Int.zero) ~src ~size_src dst =
  let size = size_src in
  let validity = enough_validity ~start ~size in
  let exact = true in
  let offsets = Ival.inject_singleton start in
  let from = src in
  match V_Offsetmap.paste_slice ~validity ~exact ~from ~size ~offsets dst with
  | _, `Bottom -> assert false
  | _, `Value r -> r

(* Reads [size] bits starting at [start] in [o], as a single value *)
let basic_find ?(start=Int.zero) ~size o =
  let validity = enough_validity ~start ~size in
  let offsets = Ival.inject_singleton start in
  let _, v = V_Offsetmap.find ~validity ~offsets ~size o in
  V_Or_Uninitialized.map (fun v -> fst (V.cast ~signed:false ~size v)) v

(* Paste [v] of size [size] at position [start] in [o] *)
let basic_add ?(start=Int.zero) ~size v o =
  let validity = enough_validity ~start ~size in
  let offsets = Ival.inject_singleton start in
  let v = V_Or_Uninitialized.initialized v in
  match V_Offsetmap.update ~validity ~exact:true ~offsets ~size v o with
  | _, `Value m -> m
  | _ -> assert false

let inject ~size v =
  V_Offsetmap.create ~size ~size_v:size (V_Or_Uninitialized.initialized v)


(** Reading a given bit *)

let read_bit o bit =
  let v = basic_find ~start:bit ~size:Integer.one o in
  let v = V_Or_Uninitialized.get_v v in
  try
    let i = V.project_ival_bottom v in
    match Ival.contains_zero i, Ival.contains_non_zero i with
    | true, true ->  `ZeroOne
    | true, false -> `Zero
    | false, true -> `One
    | false, false (* bottom *) -> `Zero
  with V.Not_based_on_null -> `ZeroOne

(** Decompose the range [b..e] (inclusive) of [o] into ranges of consecutive
    equal bits. *)
let explode_range o (b, e) =
  (* result. only [b..e] will be modified *)
  let r = ref o in
  let bit i = read_bit o (Integer.of_int i) in
  (* value + start of the constant interval *)
  let vstart = ref (bit b, b)  in
  (* Write the current value between [snd !vstart] and [i] inclusive *)
  let write_current i =
    let v, start = !vstart in
    let start = Integer.of_int start in
    let size = Integer.length start i in
    match v with
    | `Zero ->
      r := basic_add ~start ~size V.singleton_zero !r
    | `One ->
      let v = V.inject_int (Integer.(pred (two_power size))) in
      r := basic_add ~start ~size v !r
    | `ZeroOne -> () (* keep the underlying value unchanged *)
  in
  for i = b+1 to e do
    let v = bit i in
    if v <> fst !vstart then begin (* previous interval finished, write it *)
      write_current (Integer.of_int (i-1));
      vstart :=  (v, i);
    end
  done;
  write_current (Integer.of_int e);
  !r

(** Decompose an offsetmap into ranges of consecutive equal bits. Non-constant
    subparts are left unchanged. *)
let explode o =
  let r = ref o in
  let aux (e, b) _ =
    r := explode_range !r (Integer.to_int e, Integer.to_int b)
  in
  V_Offsetmap.iter aux o;
  List.rev (V_Offsetmap.fold (fun r v acc -> (r, v) :: acc) !r [])

(** Subpart of an offsetmap (as understood by advanced iterators) *)
type offsm_range = V_Or_Uninitialized.t * Int.t * Rel.t

module V_OffsetmapSentinel = struct
  include Cvalue.V_Offsetmap
  let sentinel = Cvalue.V_Offsetmap.empty
end
module ExplodeRes = struct
  type t = (Int_Intervals_sig.itv  * offsm_range) list
  let sentinel = []
end

(* Cached version of {!explode} *)
module CacheExplode =
  Binary_cache.Arity_One(V_OffsetmapSentinel)(ExplodeRes)
let explode = CacheExplode.merge explode


(** Read the contents of a value with repetition (i.e. a range of
    an offsetmap) as a single value. *)
let extract size vv =
  let d = default size in
  let o = V_Offsetmap.add ~exact:true (Int.zero, Int.pred size) vv d in
  basic_find ~size o


(** Offsetmap operations on ranges *)

let equal_offsm_range (v1, s1, r1: offsm_range) (v2, s2, r2: offsm_range) =
  V_Or_Uninitialized.equal v1 v2 && Int.equal s1 s2 && Rel.equal r1 r2

(** Offsetmap as decomposed into a list of ranges *)
type offsm_as_list = ((Int.t * Int.t) * offsm_range) list

let rec merge_list (f: _ -> offsm_range -> offsm_range -> offsm_range) (l1: offsm_as_list) (l2: offsm_as_list) : offsm_as_list =
  match l1, l2 with
  | [], [] -> []
  | [], _ :: _ | _ :: _, [] -> assert false
  | ((e1, b1 as i1), (v1, s1, r1 as vv1)) :: q1,
    ((e2, b2 as i2), (v2, s2, r2 as vv2)) :: q2 ->
    assert (Int.equal e1 e2);
    if b1 = b2 then
      (i1, f i1 vv1 vv2) :: merge_list f q1 q2
    else if Int.lt b1 b2 then
      (* vv1 is shorter, split vv2 in two. The value for the second part (vv2')
         starts at [e1] while the second part itself starts at [b1 + 1]:
         correct the offset appropriately. *)
      let d = Rel.sub_abs e1 (Int.succ b1) in
      let vv2' = (Int.succ b1, b2), (v2, s2, Rel.add d r2) in
      ((e1, b1), f i1 vv1 vv2) :: merge_list f q1 (vv2' :: q2)
    else
      (* Reverse case: vv2 is shorter, split vv1 *)
      let d = Rel.sub_abs e1 (Int.succ b2) in
      let vv1' = (Int.succ b2, b1), (v1, s1, Rel.add d r1) in
      ((e1, b2), f i2 vv1 vv2) :: merge_list f (vv1' :: q1) q2

let map2 f o1 o2 =
  let l1 = explode o1 in
  let l2 = explode o2 in
  let l' = merge_list f l1 l2 in
  let aux acc (i, vv) = V_Offsetmap.add ~exact:true i vv acc in
  List.fold_left aux o1 l'


(** Bitwise, pointwise operations *)

(* This function detects if the [size] first bits of [(v, _size_v, off)] are
   all set to zero.
   TODO: currently, we make no attempt to return a precise answer when
   [v] is not zero, but its restriction to [size] bits with [off] offset
   would be. *)
let is_zero =
  let zero = V_Or_Uninitialized.initialized V.singleton_zero in
  (fun _size (v, _size_v, _off) -> V_Or_Uninitialized.equal zero v)

(* This function detects if the [size] first bits of [(v, _size_v, off)] are
   all set to one.
   TODO: currently, we make no attempt to return a precise answer when
   [off] is not [zero]. Also, we could improve the function by not creating
   V_Or_Uninitialized values, and instead directly reasoning on Ival. *)
let is_all_ones size (v, _size_v, off) =
  Rel.equal Rel.zero off &&
  let n = Int.(pred (two_power size)) in
  let one = V_Or_Uninitialized.initialized (V.inject_int n) in
  V_Or_Uninitialized.equal one v

let lift f length (vv1: offsm_range) (vv2: offsm_range): offsm_range =
  let v1 = extract length vv1 in
  let v2 = extract length vv2 in
  (V_Or_Uninitialized.map2 f v1 v2, length, Rel.zero)

let same_concr (v1, _, _ as vv1: offsm_range) (vv2: offsm_range) =
  equal_offsm_range vv1 vv2 && V_Or_Uninitialized.cardinal_zero_or_one v1

let aux_or (b, e) (vv1: offsm_range) (vv2: offsm_range) =
  let size = Int.length b e in
  if is_zero size vv1 || is_all_ones size vv2 || same_concr vv1 vv2 then vv2
  else if is_zero size vv2 || is_all_ones size vv1 then vv1
  else
    lift V.bitwise_or size vv1 vv2

let aux_and (b, e) (vv1: offsm_range) (vv2: offsm_range) =
  let size = Int.length b e in
  if is_zero size vv1 || is_all_ones size vv2 || same_concr vv1 vv2 then vv1
  else if is_zero size vv2 || is_all_ones size vv1 then vv2
  else
    (*TODO: this ~signed may be dangerous if for some reason we get two values
      of inverse sign. extract_bits generate always positive integers, which
      is good. The good solution would be have V.bitwise_and to accept both
      signs simultaneously. *)
    let f = V.bitwise_and ~signed:false  ~size:(Integer.to_int size) in
    lift f size vv1 vv2

(* O is neutral for xor, and  v ^ v = 0 *)
let aux_xor (b, e) (vv1: offsm_range) (vv2: offsm_range) =
  let size = Int.length b e in
  if is_zero size vv1 then vv2
  else if is_zero size vv2 then vv1
  else if same_concr vv1 vv2 then
    (V_Or_Uninitialized.initialized V.singleton_zero, Int.one, Rel.zero)
  else lift V.bitwise_xor size vv1 vv2

let bitwise_or = map2 aux_or
let bitwise_and = map2 aux_and
let bitwise_xor = map2 aux_xor

module CacheOr =
  Binary_cache.Arity_Two
    (V_OffsetmapSentinel)(V_OffsetmapSentinel)(V_OffsetmapSentinel)
module CacheAnd =
  Binary_cache.Arity_Two
    (V_OffsetmapSentinel)(V_OffsetmapSentinel)(V_OffsetmapSentinel)
module CacheXor =
  Binary_cache.Arity_Two
    (V_OffsetmapSentinel)(V_OffsetmapSentinel)(V_OffsetmapSentinel)

let bitwise_or  = CacheOr.merge  bitwise_or
let bitwise_and = CacheAnd.merge bitwise_and
let bitwise_xor = CacheXor.merge bitwise_xor


(** Sign extension *)

let offsm_sign_extension sign_bit size =
  let i = match sign_bit with
    | `Zero -> Ival.zero
    | `One -> Ival.minus_one
    | `ZeroOne -> Ival.(join zero minus_one)
  in
  let v_extend = V_Or_Uninitialized.initialized (V.inject_ival i) in
  V_Offsetmap.create ~size v_extend ~size_v:size


(** Shifts *)

type shift = SLeft of Int.t | SRight of Int.t * bool

(* Shift implemented as an an offsetmap copy then paste. *)
let shift size o shift =
  let r = default size in
  match shift with
  | SLeft n ->
    if Int.lt n size then
      let size_copy = Int.sub size n in
      let data = basic_copy ~size:size_copy o in
      basic_paste ~start:n ~src:data ~size_src:size_copy r
    else r (* Guaranteed undefined behavior ; we don't care about the result. *)
  | SRight (n, signed) ->
    if Int.lt n size then
      let size_copy = Int.sub size n in
      let data = basic_copy ~start:n ~size:size_copy o in
      let o' = basic_paste ~src:data ~size_src:size_copy r in
      if signed then
        let sign_bit = read_bit o (Int.pred size) in
        if sign_bit <> `Zero then
          (* We need to preserve the sign. Add {-1} or {0; -1} on the leftmost
             bits *)
          let size_ext = Int.sub size size_copy in
          let ext = offsm_sign_extension sign_bit size_ext in
          basic_paste ~start:size_copy ~src:ext ~size_src:size_ext o'
        else
          o'
      else
        o'
    else r (* Guaranteed undefined behavior ; we don't care about the result. *)


(** Casts *)

let cast ~old_size ~new_size ~signed o =
  if Int.equal old_size new_size then o
  else if Int.lt new_size old_size then (* Truncation *)
    basic_copy ~size:new_size o
  else (* Extension *)
  if signed then (* need to check the sign and extend accordingly *)
    (* Original bits, extended with zeros *)
    let r = default new_size in
    let r_o = basic_paste ~src:o ~size_src:old_size r in
    (* Bits of sign extension *)
    let sign_bit = read_bit o (Int.pred old_size) in
    let size_ext = Int.sub new_size old_size in
    let ext = offsm_sign_extension sign_bit size_ext in
    basic_paste ~start:old_size ~src:ext ~size_src:size_ext r_o
  else
    let r = default new_size in
    basic_paste ~src:o ~size_src:old_size r


(** Binary not *)

let bnot o =
  let aux itv (v, s, rel) o =
    let v' = V_Or_Uninitialized.map V.bitwise_not v in
    V_Offsetmap.add ~exact:true itv (v', s, rel) o
  in
  V_Offsetmap.fold aux o o


(** Datatype *)

type offsm_or_top = O of V_Offsetmap.t | Top

module Datatype_Offsm_or_top = Datatype.Make_with_collections(struct
    type t = offsm_or_top
    let name = "Eval_offsm.offsm_or_top"
    let rehash = Datatype.identity
    let structural_descr =
      Structural_descr.t_sum [| [| V_Offsetmap.packed_descr |] |]
    let reprs = [Top; O (List.hd V_Offsetmap.reprs)]
    let compare o1 o2 = match o1, o2 with
      | Top, Top -> 0
      | O o1, O o2 -> V_Offsetmap.compare o1 o2
      | O _, Top -> -1
      | Top, O _ -> 1
    let equal = Datatype.from_compare
    let hash = function
      | Top -> 7895
      | O o -> V_Offsetmap.hash o
    let copy = Datatype.undefined
    let internal_pretty_code = Datatype.undefined
    let pretty fmt = function
      | Top -> Format.pp_print_string fmt "TopO"
      | O o -> Format.fprintf fmt "O @[%a@]" V_Offsetmap.pretty o
    let varname _ = "o"
    let mem_project = Datatype.never_any_project
  end)


let offsm_key = Structure.Key_Value.create_key "offsetmap_value"

module Offsm : Abstract_value.Internal with type t = offsm_or_top = struct
  include Datatype_Offsm_or_top

  let structure = Structure.Key_Value.Leaf offsm_key

  let pretty_typ typ fmt = function
    | Top as o -> pretty fmt o
    | O o ->
      Format.fprintf fmt "O @[%a@]"
        (V_Offsetmap.pretty_generic ?typ ()) o
  
  let top = Top

  let is_included o1 o2 = match o1, o2 with
    | _, Top -> true
    | O o1, O o2 -> V_Offsetmap.is_included o1 o2
    | Top, O _ -> false

  let join o1 o2 = match o1, o2 with
    | Top, _ | _, Top -> Top
    | O o1, O o2 -> O (V_Offsetmap.join o1 o2)

  let narrow o1 o2 = match o1, o2 with
    | Top, o | o, Top -> `Value o
    | O o1, O o2 ->
      V_Offsetmap.narrow_reinterpret o1 o2 >>-: (fun o -> O o)

  (* Simple values cannot be injected because we do not known their type
     (hence size in bits *)
  let zero = Top
  let float_zeros = Top
  let top_int = Top

  let inject_int typ i =
    try
      let size = Integer.of_int (Cil.bitsSizeOf typ) in
      O (inject ~size (V.inject_int i))
    with Cil.SizeOfError _ -> Top

  let inject_address _ = Top

  let constant e _c =
    let o =
      if store_redundant then
        match Cil.constFoldToInt e with
        | Some i -> inject_int (Cil.typeOf e) i
        | None -> Top
      else Top
    in
    `Value o, Alarmset.all

  let resolve_functions ~typ_pointer:_ _ = `Top, true (* TODO: extract value *)

  let forward_unop ~context:_ _typ op o =
    let o' = match o, op with
      | Top, _ | _, (Neg | LNot) -> Top
      | O o, BNot -> O (bnot o)
    in
    `Value o', Alarmset.all

  let ik_of_type typ = match Cil.unrollType typ with
    | TInt (ik, _)
    | TEnum ({ekind = ik}, _)  -> Some ik
    | TPtr _ -> Some Cil.theMachine.Cil.upointKind
    | _ -> None

  let forward_binop ~context:_ _typ op o1 o2 =
    let o' =
      match o1, o2, op with
      | O _o1, O _o2, (Shiftlt | Shiftrt) ->
        (* It is inconvenient to handle shift here, because we need a
           constant for o2 *)
        Top
      | O o1, O o2, BAnd -> O (bitwise_and o1 o2)
      | O o1, O o2, BOr -> O (bitwise_or o1 o2)
      | O o1, O o2, BXor -> O (bitwise_xor o1 o2)
      | _ -> Top
    in
    `Value o', Alarmset.all

  let backward_binop ~input_type:_ ~resulting_type:_ _op ~left:_ ~right:_ ~result:_ =
    `Value (None, None)

  let backward_unop ~typ_arg:_ _unop ~arg:_ ~res:_ = `Value None

  let backward_cast ~src_typ:_ ~dst_typ:_ ~src_val:_ ~dst_val:_ =
    `Value None

  let truncate_integer _e _range o = `Value o, Alarmset.all
  let rewrap_integer _range o = o
  let cast_float _e _fkind o = `Value o, Alarmset.all

  let do_promotion ~src_typ ~dst_typ _e o =
    let o' =
      match o, ik_of_type src_typ, ik_of_type dst_typ with
      | Top, _, _ | _, None, _ | _, _, None -> Top
      | O o, Some ik_src, Some ik_dst ->
        let old_size = Integer.of_int (Cil.bitsSizeOfInt ik_src) in
        let new_size = Integer.of_int (Cil.bitsSizeOfInt ik_dst) in
        let signed = Cil.isSigned ik_src in
        O (cast ~old_size ~new_size ~signed o)
    in
    `Value o', Alarmset.all

end


module CvalueOffsm : Abstract_value.Internal with type t = V.t * offsm_or_top
= struct
  include Value_product.Make (Main_values.CVal) (Offsm)

  let size typ = Integer.of_int (Cil.bitsSizeOf typ)

  (* Extract an offsetmap from a pair, by converting the value when needed. *)
  let to_offsm typ (v, o : t) =
    match o with
    | Top -> inject ~size:(size typ) v
    | O o -> o

  (* Ensure that the offsetmap component is not empty *)
  let strengthen_offsm typ (v, o as p : t) : t =
    if o = Top then
      (v, O (to_offsm typ p))
    else p

  (* Refine the value component according to the contents of the offsetmap *)
  let strengthen_v typ (v, o as p : t) : t =
    match o with
    | Top -> p
    | O o' ->
      let size = size typ in
      (* TODO: this should be done by the transfer function itself... *)
      let v = Cvalue_forward.unsafe_reinterpret typ v in
      let v_o = V_Or_Uninitialized.get_v (basic_find ~size o') in
      let v_o = Cvalue_forward.unsafe_reinterpret typ v_o in
      (V.narrow v v_o, o)

  let forward_unop ~context typ op p =
    match op with
    | BNot ->
      let p' = strengthen_offsm typ p in
      forward_unop ~context typ op p' >>=: fun p'' ->
      strengthen_v typ p''
    | _ -> forward_unop ~context typ op p

  let forward_binop ~context typ op l r =
    match op with
    | BAnd | BOr | BXor ->
      let l = strengthen_offsm typ l in
      let r = strengthen_offsm typ r in
      forward_binop ~context typ op l r >>=: fun p ->
      strengthen_v typ p
    | Shiftlt | Shiftrt ->
      let (v_r, _) = r in
      let (v_l, _) = l in
      begin
        try
          let i = V.project_ival v_r in
          let i = Ival.project_int i in
          let signed = Bit_utils.is_signed_int_enum_pointer typ in
          let shiftn = if op = Shiftlt then SLeft i else SRight (i, signed) in
          let o = shift (size typ) (to_offsm typ l) shiftn in
          Main_values.CVal.forward_binop ~context typ op v_l v_r >>=: fun v ->
          v, O o
        with V.Not_based_on_null | Ival.Not_Singleton_Int ->
          forward_binop ~context typ op l r
      end
    | _ -> forward_binop ~context typ op l r

end
