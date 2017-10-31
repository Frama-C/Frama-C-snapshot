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

open Abstract_interp
open Locations

type precise_offset =
  | POBottom (* No offset *)
  | POZero (* Offset zero *)
  | POSingleton of Int.t (* Single offset *)
  | POPrecise of Ival.t * (Int.t (* cardinal *))
       (* Offset exactly represented by an ival *)
  | POImprecise of Ival.t (* Offset that could not be represented precisely *)
  | POShift of (* Shifted offset *)
      Ival.t (* number of bits/bytes to shift *) *
      precise_offset *
      Int.t (* cardinal*)

(* Cardinals are over-approximated: the combination [{0, 1} + {0, 1}]
   is considered as having cardinal 4 instead of 3. POBottom is the
   only way to represent Bottom (ie [POImprecise Ival.bottom] is
   forbidden). Other invariants, ie. [POSingleton i] means that [i] is
   non-zero, are not required for correction -- only for performance. *)


let rec pretty_offset fmt = function
  | POBottom -> Format.fprintf fmt "<Bot>"
  | POZero -> Format.fprintf fmt "<0>"
  | POSingleton i -> Format.fprintf fmt "<%a>_0" Int.pretty i
  | POPrecise (po, _) -> Format.fprintf fmt "<%a>p" Ival.pretty po
  | POImprecise po -> Format.fprintf fmt "<%a>i" Ival.pretty po
  | POShift (i, po, _) ->
    Format.fprintf fmt "<%a+%a>" pretty_offset po Ival.pretty i

let rec equal_offset o1 o2 = match o1, o2 with
  | POBottom, POBottom -> true
  | POZero, POZero -> true
  | POSingleton i1, POSingleton i2 -> Int.equal i1 i2
  | POPrecise (i1, _), POPrecise (i2, _) -> Ival.equal i1 i2
  | POImprecise i1, POImprecise i2 -> Ival.equal i1 i2
  | POShift (shift1, o1, _), POShift (shift2, o2, _) ->
    Ival.equal shift1 shift2 && equal_offset o1 o2
  | _, _ -> false

let offset_zero = POZero
let offset_bottom = POBottom
let offset_top = POImprecise Ival.top

let is_bottom_offset off = off = POBottom

let cardinal_zero_or_one_offset = function
  | POBottom | POZero | POSingleton _ -> true
  | POPrecise (_, c) | POShift (_, _, c) -> Int.le c Int.one
  | POImprecise _ -> false


let small_cardinal c = Int.le c (Int.of_int (Offsetmap.get_plevel ()))

let _cardinal_offset = function
  | POBottom -> Some Int.zero
  | POZero | POSingleton _ -> Some Int.one
  | POPrecise (_, c) -> Some c
  | POImprecise _ -> None
  | POShift (_, _, c) -> Some c

let rec imprecise_offset = function
  | POBottom -> Ival.bottom
  | POZero -> Ival.zero
  | POSingleton i -> Ival.inject_singleton i
  | POPrecise (i, _) | POImprecise i -> i
  | POShift (shift, po, _) -> Ival.add_int shift (imprecise_offset po)

let rec _scale_offset scale po =
  assert (Int.gt scale Int.zero);
  match po with
    | POBottom -> POBottom
    | POZero -> POZero
    | POSingleton i -> POSingleton (Int.mul i scale)
    | POPrecise (i, c) -> POPrecise (Ival.scale scale i, c)
    | POImprecise i -> POImprecise (Ival.scale scale i)
    | POShift (shift, po, c) ->
      POShift (Ival.scale scale shift, _scale_offset scale po, c)

let shift_offset_by_singleton shift po =
  if Int.is_zero shift then
    po
  else
    match po with
      | POBottom -> POBottom
      | POZero -> POSingleton shift
      | POSingleton i -> POSingleton (Int.add i shift)
      | POPrecise (i, c) -> POPrecise (Ival.add_singleton_int shift i, c)
      | POImprecise i -> POImprecise (Ival.add_singleton_int shift i)
      | POShift (shift', po, c) ->
        POShift (Ival.add_singleton_int shift shift', po, c)

let inject_ival ival =
  if Ival.is_bottom ival then POBottom
  else
    match Ival.cardinal ival with
    | Some c when small_cardinal c ->
      if Int.equal c Int.one then
        let i = Ival.project_int ival in
        if Int.equal i Int.zero then POZero else POSingleton (Ival.project_int ival)
      else
        POPrecise (ival, c)
    | _ -> POImprecise ival

let shift_offset shift po =
  if Ival.is_bottom shift then
    POBottom
  else
    match po with
      | POBottom -> POBottom

      | POZero -> inject_ival shift

      | POImprecise i -> POImprecise (Ival.add_int shift i)

      | POSingleton i ->
        (match Ival.cardinal shift with
          | Some c when small_cardinal c ->
            if Int.equal c Int.one then
              POSingleton (Int.add (Ival.project_int shift) i)
            else
              POPrecise (Ival.add_singleton_int i shift, c)
          | _ -> POImprecise (Ival.add_int shift (imprecise_offset po)))

      | POPrecise (_i, cpo) ->
        (match Ival.cardinal shift with
          | Some cs ->
            let new_card = Int.mul cs cpo in
            if small_cardinal new_card then
              POShift (shift, po, new_card) (* may be a POPrecise depending
                                               on ilevel *)
            else
              POImprecise (Ival.add_int shift (imprecise_offset po))
          | None ->
            POImprecise (Ival.add_int shift (imprecise_offset po)))

      | POShift (_shift', _po', cpo) ->
        (match Ival.cardinal shift with
          | Some cs ->
            let new_card = Int.mul cs cpo in
            if small_cardinal new_card then
              POShift (shift, po, new_card) (* may be a single POShift depending
                                              on the cardinals of shift/shift'*)
            else
              POImprecise (Ival.add_int shift (imprecise_offset po))
          | None ->
            POImprecise (Ival.add_int shift (imprecise_offset po)))

type precise_location_bits =
  | PLBottom
  | PLLoc of Location_Bits.t
  | PLVarOffset of Base.t * precise_offset
  | PLLocOffset of Location_Bits.t * precise_offset

let pretty_loc_bits fmt = function
  | PLBottom -> Format.fprintf fmt "[Bot]"
  | PLLoc loc -> Format.fprintf fmt "[%a]" Location_Bits.pretty loc
  | PLVarOffset (b, po) ->
    Format.fprintf fmt "[%a+%a]" Base.pretty b pretty_offset po
  | PLLocOffset (loc, po) ->
    Format.fprintf fmt "[%a+%a]" Location_Bits.pretty loc pretty_offset po

let equal_loc_bits l1 l2 = match l1, l2 with
  | PLBottom, PLBottom -> true
  | PLLoc l1, PLLoc l2 -> Location_Bits.equal l1 l2
  | PLVarOffset (b1, o1), PLVarOffset (b2, o2) ->
    Base.equal b1 b2 && equal_offset o1 o2
  | PLLocOffset (l1, o1), PLLocOffset (l2, o2) ->
    Location_Bits.equal l1 l2 && equal_offset o1 o2
  | _, _ -> false

let bottom_location_bits = PLBottom

let cardinal_zero_or_one_location_bits = function
  | PLBottom -> true
  | PLLoc loc -> Location_Bits.cardinal_zero_or_one loc
  | PLVarOffset (_, po) -> cardinal_zero_or_one_offset po
  | PLLocOffset (loc, po) ->
    Location_Bits.cardinal_zero_or_one loc && cardinal_zero_or_one_offset po

let inject_location_bits loc =
  if Location_Bits.is_bottom loc then PLBottom else PLLoc loc

let combine_base_precise_offset base po =
  match po with
    | POBottom -> PLBottom
    | _ -> PLVarOffset (base, po)

let combine_loc_precise_offset loc po =
  try
    let base, ival = Location_Bits.find_lonely_key loc in
    begin match shift_offset ival po with
      | POBottom -> PLBottom
      | po -> PLVarOffset (base, po)
    end
  with Not_found ->
  match po with
  | POBottom      -> PLBottom
  | POZero        -> PLLoc loc
  | POImprecise i -> PLLoc (Location_Bits.shift i loc)
  | POSingleton i -> PLLoc (Location_Bits.shift (Ival.inject_singleton i) loc)
  | POPrecise (i, _c) when Location_Bits.cardinal_zero_or_one loc ->
    PLLoc (Location_Bits.shift i loc)
  | POPrecise (_, c) | POShift (_, _, c) ->
    match Location_Bits.cardinal loc with
    | Some card when small_cardinal (Int.mul card c) -> PLLocOffset (loc, po)
    | _ -> PLLoc (Location_Bits.shift (imprecise_offset po) loc)


let imprecise_location_bits = function
  | PLBottom -> Location_Bits.bottom
  | PLLoc l -> l
  | PLVarOffset (b, po) -> Location_Bits.inject b (imprecise_offset po)
  | PLLocOffset (loc, po) -> Location_Bits.shift (imprecise_offset po) loc

type precise_location = {
  loc: precise_location_bits;
  size: Int_Base.t
}

let equal_loc pl1 pl2 =
  equal_loc_bits pl1.loc pl2.loc && Int_Base.equal pl1.size pl2.size

let imprecise_location pl =
  make_loc (imprecise_location_bits pl.loc) pl.size

let make_precise_loc loc ~size = { loc; size }

let loc_size loc = loc.size

let loc_bottom = {
  loc = PLBottom;
  size = Int_Base.top;
}
let is_bottom_loc pl = pl.loc = PLBottom

let loc_top = {
  loc = PLLoc Location_Bits.top;
  size = Int_Base.top;
}
let is_top_loc pl = equal_loc loc_top pl

let rec fold_offset f po acc =
  match po with
    | POBottom -> f Ival.bottom acc
    | POZero -> f Ival.zero acc
    | POSingleton i -> f (Ival.inject_singleton i) acc
    | POPrecise (iv, _) | POImprecise iv -> f iv acc
    | POShift (shift, po', _) ->
      let aux_po ival acc =
        let aux_ival shift_i acc =
          let ival' = Ival.add_singleton_int shift_i ival in
          f ival' acc
        in
        Ival.fold_int aux_ival shift acc
      in
      fold_offset aux_po po' acc

let fold f pl acc =
  match pl.loc with
    | PLBottom -> acc
    | PLLoc l -> f (make_loc l pl.size) acc
    | PLVarOffset (b, po) ->
      let aux_po ival acc =
        let loc_b = Location_Bits.inject b ival in
        let loc = make_loc loc_b pl.size in
        f loc acc
      in
      fold_offset aux_po po acc
    | PLLocOffset (loc, po) ->
      let aux_po ival_po acc =
        let aux_loc b ival_loc acc =
          let aux_ival_loc i acc =
            let ival = Ival.add_singleton_int i ival_po in
            let loc_b = Location_Bits.inject b ival in
            let loc = make_loc loc_b pl.size in
            f loc acc
          in
          Ival.fold_int aux_ival_loc ival_loc acc
        in
        Location_Bits.fold_i aux_loc loc acc
      in
      fold_offset aux_po po acc

let enumerate_valid_bits ~for_writing loc =
  let aux loc z = Zone.join z (enumerate_valid_bits ~for_writing loc) in
  fold aux loc Zone.bottom


let cardinal_zero_or_one pl =
  not (Int_Base.is_top pl.size) && cardinal_zero_or_one_location_bits pl.loc

let valid_cardinal_zero_or_one ~for_writing pl =
    match pl.loc with
      | PLBottom -> true
      | PLLoc lb ->
        let loc = make_loc lb pl.size in
        Locations.valid_cardinal_zero_or_one ~for_writing loc
      | _ ->
        try
          ignore
            (fold (fun loc found_one ->
              let valid = Locations.valid_part ~for_writing loc in
              if Locations.is_bottom_loc loc then found_one
              else
                if Locations.cardinal_zero_or_one valid then
                  if found_one then raise Exit else true
                else raise Exit
             ) pl false);
          true
        with Exit -> false

let pretty_loc fmt loc =
  Format.fprintf fmt "%a (size:%a)"
    pretty_loc_bits loc.loc Int_Base.pretty loc.size


let rec reduce_offset_by_range range offset = match offset with
  | POBottom -> offset
  | POZero -> if Ival.contains_zero range then offset else POBottom
  | POSingleton i ->
    let i = Ival.inject_singleton i in
    if Ival.is_included i range then offset else POBottom
  | POPrecise (ival, card) ->
    let ival = Ival.narrow range ival in
    if Ival.is_bottom ival then POBottom else POPrecise (ival, card)
  | POImprecise ival ->
    let ival = Ival.narrow range ival in
    if Ival.is_bottom ival then POBottom else POImprecise ival
  | POShift (shift, offset, card) ->
    let range = Ival.sub_int range shift in
    let offset = reduce_offset_by_range range offset in
    if offset = POBottom then offset else POShift (shift, offset, card)

(* Maintain synchronized with Locations.reduce_offset_by_validity *)
let reduce_offset_by_validity ~for_writing ~bitfield base offset size =
  if for_writing && Base.is_read_only base then
    POBottom
  else
    match Base.validity base, size with
    | Base.Empty, _ ->
      if Int_Base.(compare size zero) > 0
      then POBottom
      else reduce_offset_by_range Ival.zero offset
    | Base.Invalid, _ -> POBottom
    | _, Int_Base.Top -> offset
    | (Base.Known (minv, maxv) | Base.Unknown (minv,_,maxv)),
      Int_Base.Value size ->
      (* The maximum offset is maxv - (size - 1), except if size = 0,
         in which case the maximum offset is exactly maxv. *)
      let pred_size = Int.max Int.zero (Int.pred size) in
      let maxv = Int.sub maxv pred_size in
      let range =
        if bitfield
        then Ival.inject_range (Some minv) (Some maxv)
        else Ival.inject_interval (Some minv) (Some maxv) Int.zero Int.eight
      in
      reduce_offset_by_range range offset
    | Base.Variable variable_v, Int_Base.Value size ->
      let pred_size = Int.max Int.zero (Int.pred size) in
      let maxv = Int.sub variable_v.Base.max_alloc pred_size in
      let range =
        if bitfield
        then Ival.inject_range (Some Int.zero) (Some maxv)
        else Ival.inject_interval (Some Int.zero) (Some maxv) Int.zero Int.eight
      in
      reduce_offset_by_range range offset


let reduce_by_valid_part ~for_writing ~bitfield precise_loc size =
  match precise_loc with
  | PLBottom -> precise_loc
  | PLLoc loc ->
    let loc = Locations.make_loc loc size in
    PLLoc Locations.((valid_part ~for_writing ~bitfield loc).Locations.loc)
  | PLVarOffset (base, offset) ->
    begin
      match reduce_offset_by_validity ~for_writing ~bitfield base offset size with
      | POBottom -> PLBottom
      | offset -> PLVarOffset (base, offset)
    end
  | PLLocOffset (_loc, _offset) ->
    (* Reduction is difficult in this case, because we must take into account
       simultaneously [loc] and [offset]. We do nothing for the time being. *)
    precise_loc

let valid_part ~for_writing ~bitfield {loc; size} =
  { loc = reduce_by_valid_part ~for_writing ~bitfield loc size;
    size = size }

(*
Local Variables:
compile-command: "make -C ../../.."
End:
*)
