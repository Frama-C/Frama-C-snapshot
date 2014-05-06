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

let offset_zero = POZero
let offset_bottom = POBottom
let offset_top = POImprecise Ival.top

let is_bottom_offset off = off = POBottom

let cardinal_zero_or_one_offset = function
  | POBottom | POZero | POSingleton _ -> true
  | POPrecise (_, c) | POShift (_, _, c) -> Int.le c Int.one
  | POImprecise _ -> false


let small_cardinal c = Int.le c (Int.of_int !Lattice_Interval_Set.plevel)

let _cardinal_offset = function
  | POBottom -> Some Int.zero
  | POZero | POSingleton _ -> Some Int.one
  | POPrecise (_, c) -> Some c
  | POImprecise _ -> None
  | POShift (_, _, c) -> Some c

let rec imprecise_offset = function
  | POBottom -> Ival.bottom
  | POZero -> Ival.singleton_zero
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

let shift_offset shift po =
  if Ival.is_bottom shift then
    POBottom
  else
    match po with
      | POBottom -> POBottom

      | POZero ->
        (match Ival.cardinal shift with
          | Some c when small_cardinal c ->
            if Int.equal c Int.one then
              POSingleton (Ival.project_int shift)
            else
              POPrecise (shift, c)
          | _ -> POImprecise shift)

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

let pretty_loc fmt = function
  | PLBottom -> Format.fprintf fmt "[Bot]"
  | PLLoc loc -> Format.fprintf fmt "[%a]" Location_Bits.pretty loc
  | PLVarOffset (b, po) ->
    Format.fprintf fmt "[%a+%a]" Base.pretty b pretty_offset po
  | PLLocOffset (loc, po) ->
    Format.fprintf fmt "[%a+%a]" Location_Bits.pretty loc pretty_offset po
 
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
    | POZero -> PLLoc (Location_Bits.inject base Ival.singleton_zero)
    | POSingleton i ->
      PLLoc (Location_Bits.inject base (Ival.inject_singleton i))
    | POImprecise i | POPrecise (i, _) -> PLLoc (Location_Bits.inject base i)
    | POShift _ -> PLVarOffset (base, po)

let combine_loc_precise_offset loc po =
  match po with
    | POBottom -> PLBottom
    | POZero -> PLLoc loc
    | POImprecise i ->
      PLLoc (Location_Bits.shift i loc)
    | POSingleton i ->
      PLLoc (Location_Bits.shift (Ival.inject_singleton i) loc)
    | POPrecise (i, _c) when Location_Bits.cardinal_zero_or_one loc ->
      PLLoc (Location_Bits.shift i loc)
    | POPrecise (_, c) | POShift (_, _, c) ->
      (match Location_Bits.cardinal loc with
        | Some card when small_cardinal (Int.mul card c) ->
          PLLocOffset (loc, po)
        | _ ->
          PLLoc (Location_Bits.shift (imprecise_offset po) loc)
      )

let imprecise_location_bits = function
  | PLBottom -> Location_Bits.bottom
  | PLLoc l -> l
  | PLVarOffset (b, po) -> Location_Bits.inject b (imprecise_offset po)
  | PLLocOffset (loc, po) -> Location_Bits.shift (imprecise_offset po) loc

type precise_location = {
  loc: precise_location_bits;
  size: Int_Base.t
}

let imprecise_location pl =
  make_loc (imprecise_location_bits pl.loc) pl.size

let make_precise_loc loc ~size = { loc; size }

let loc_size loc = loc.size

let loc_bottom = {
  loc = PLBottom;
  size = Int_Base.top;
}
let is_bottom_loc pl = pl.loc = PLBottom

let rec fold_offset f po acc =
  match po with
    | POBottom -> f Ival.bottom acc
    | POZero -> f Ival.singleton_zero acc
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
    | PLBottom -> f Locations.loc_bottom acc
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


let cardinal_zero_or_one ~for_writing pl =
  if true then
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
  else
    not (Int_Base.is_top pl.size) && cardinal_zero_or_one_location_bits pl.loc
 

(*
Local Variables:
compile-command: "make -C ../.."
End:
*)
