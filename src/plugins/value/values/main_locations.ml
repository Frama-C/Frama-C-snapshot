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

let ploc_key = Structure.Key_Location.create_key "precise_locs"

module PLoc = struct

  type value = Cvalue.V.t
  type location = Precise_locs.precise_location
  type offset =
    | Precise of Precise_locs.precise_offset
    | Imprecise of Cvalue.V.t (* when the offset contains addresses *)

  let structure = Structure.Key_Location.Leaf ploc_key

  let equal_loc = Precise_locs.equal_loc
  let equal_offset o1 o2 = match o1, o2 with
    | Precise o1, Precise o2 -> Precise_locs.equal_offset o1 o2
    | Imprecise v1, Imprecise v2 -> Cvalue.V.equal v1 v2
    | _, _ -> false

  let pretty_loc = Precise_locs.pretty_loc
  let pretty_offset fmt = function
    | Precise offset -> Precise_locs.pretty_offset fmt offset
    | Imprecise v -> Format.fprintf fmt "(Imprecise of %a)" Cvalue.V.pretty v

  let to_value t =
    let loc = Precise_locs.imprecise_location t in
    Locations.loc_to_loc_without_size loc

  let size loc = Precise_locs.loc_size loc

  let make loc =
    let ploc_bits = Precise_locs.inject_location_bits loc.Locations.loc in
    Precise_locs.make_precise_loc ploc_bits ~size:loc.Locations.size

  let top = make (Locations.make_loc Locations.Location_Bits.top Int_Base.Top)


  exception AlwaysOverlap of Alarms.alarm

  let check_non_overlapping lvs1 lvs2 =
    let conv (lval, loc) =
      let for_writing = false in
      let zone = Precise_locs.enumerate_valid_bits ~for_writing loc in
      let exact =
        lazy (Precise_locs.valid_cardinal_zero_or_one ~for_writing loc)
      in
      lval, zone, exact
    in
    let l1 = List.map conv lvs1
    and l2 = List.map conv lvs2 in
    let check (lval1, zone1, exact1) (lval2, zone2, exact2) acc =
      if Locations.Zone.intersects zone1 zone2
      then
        let alarm = Alarms.Not_separated (lval1, lval2) in
        if Lazy.force exact1 && Lazy.force exact2
        then raise (AlwaysOverlap alarm)
        else Alarmset.add alarm acc
      else acc
    in
    try
      let alarms =
        List.fold_left
          (fun acc x1 -> List.fold_left (fun acc x2 -> check x1 x2 acc) acc l2)
          Alarmset.none
          l1
      in `Value (), alarms
    with AlwaysOverlap alarm -> `Bottom, Alarmset.singleton alarm

  let partially_overlap loc1 loc2 =
    let loc1 = Precise_locs.imprecise_location loc1
    and loc2 = Precise_locs.imprecise_location loc2 in
    match loc1.Locations.size with
    | Int_Base.Value size ->
      Locations.(Location_Bits.partially_overlaps size loc1.loc loc2.loc)
    | _ -> false


  (* ------------------------------------------------------------------------ *)
  (*                              Offsets                                     *)
  (* ------------------------------------------------------------------------ *)

  let no_offset = Precise Precise_locs.offset_zero

  let forward_field typ field = function
    | Precise offset ->
      begin try
          let field = fst (Cil.bitsOffset typ (Field (field, NoOffset))) in
          let field_i = Integer.of_int field in
          Precise (Precise_locs.shift_offset_by_singleton field_i offset)
        with Cil.SizeOfError _ -> Precise (Precise_locs.offset_top)
      end
    | x -> x

  let forward_index typ_pointed index remaining =
    match remaining with
    | Imprecise offset ->
      let bases = Cvalue.V.topify_arith_origin index in
      Imprecise (Cvalue.V.join bases offset)
    | Precise offset ->
      try
        let index_i = Cvalue.V.project_ival index in
        let size = Bit_utils.sizeof typ_pointed in
        (* Index offsets expressed in terms of the array elements size *)
        let index_i = Ival.scale_int_base size index_i in
        (* Combine the two offsets *)
        Precise (Precise_locs.shift_offset index_i offset)
      with Cvalue.V.Not_based_on_null ->
        (* result will be a garbled mix: collect all the bases involved in
           the evaluation of [offset], and raise an exception *)
        Imprecise (Cvalue.V.topify_arith_origin index)

  (* We are accessing an array of size [size] at indexes [index].
     If index causes an out-of-bounds access, emit an informative
     alarm, and reduce [index]. *)
  let reduce_index_by_array_size ~size_expr ~index_expr size index =
    try
      let index_ival = Cvalue.V.project_ival index in
      let open Abstract_interp in
      let array_range =
        Ival.inject_range (Some Int.zero) (Some (Integer.pred size))
      in
      let new_index = Ival.narrow index_ival array_range in
      if Ival.equal new_index index_ival
      then `Value index, Alarmset.none
      else
        let positive = match Ival.min_int index_ival with
          | None -> false
          | Some min -> Int.ge min Int.zero
        in
        let alarms = Alarmset.singleton
            (Alarms.Index_out_of_bound (index_expr, Some size_expr)) in
        let alarms =
          if not positive
          then Alarmset.add (Alarms.Index_out_of_bound (index_expr, None)) alarms
          else alarms
        in
        if Ival.is_bottom new_index
        then `Bottom, alarms
        else `Value (Cvalue.V.inject_ival new_index), alarms
    with
    | Cvalue.V.Not_based_on_null -> `Value index, Alarmset.none
  (* TODO: reduce the numeric part, and emits the alarms. *)


  (* ------------------------------------------------------------------------ *)
  (*                             Locations                                    *)
  (* ------------------------------------------------------------------------ *)

  let make_precise_loc loc typ_offs =
    let size = Eval_typ.sizeof_lval_typ typ_offs in
    let loc = Precise_locs.make_precise_loc loc ~size in
    if Precise_locs.is_bottom_loc loc
    then `Bottom
    else `Value loc

  let join_loc value loc =
    let loc = Locations.(Location_Bits.join loc (loc_bytes_to_loc_bits value)) in
    Precise_locs.inject_location_bits loc

  let forward_variable typ_offset host offset =
    let base = Base.of_varinfo host in
    match offset with
    | Precise offset ->
      let loc_pr = Precise_locs.combine_base_precise_offset base offset in
      make_precise_loc loc_pr typ_offset
    | Imprecise value ->
      let loc_b = Locations.Location_Bits.inject base Ival.zero in
      let loc_pr = join_loc value loc_b in
      make_precise_loc loc_pr typ_offset

  let forward_pointer typ_offset loc_lv offset =
    let loc_bits = Locations.loc_bytes_to_loc_bits loc_lv in
    match offset with
    | Precise offset ->
      let loc_pr = Precise_locs.combine_loc_precise_offset loc_bits offset in
      make_precise_loc loc_pr typ_offset
    | Imprecise value ->
      let loc_pr = join_loc value loc_bits in
      make_precise_loc loc_pr typ_offset

  let eval_varinfo varinfo = make (Locations.loc_of_varinfo varinfo)

  let is_valid ~for_writing loc =
    Locations.is_valid ~for_writing (Precise_locs.imprecise_location loc)

  let memory_access_alarm ~for_writing lval =
    let access_kind =
      if for_writing then Alarms.For_writing else Alarms.For_reading
    in
    Alarmset.singleton (Alarms.Memory_access (lval, access_kind))

  let reduce_loc_by_validity ~for_writing ~bitfield lval loc =
    if not (is_valid ~for_writing loc)
    then
      let alarms = memory_access_alarm ~for_writing lval in
      let loc = Precise_locs.valid_part ~for_writing ~bitfield loc in
      if Precise_locs.is_bottom_loc loc
      then `Bottom, alarms
      else `Value loc, alarms
    else `Value loc, Alarmset.none


  (* ------------------------------------------------------------------------ *)
  (*                        Backward propagators                              *)
  (* ------------------------------------------------------------------------ *)

  (* No backward reduction when the offset is Imprecise.
     Backward reducers compute offsets as ival. As they are only used to reduce
     indexes expressions, more precise offsets will be pointless. *)

  let backward_variable varinfo location =
    let loc = Precise_locs.imprecise_location location in
    let base = Base.of_varinfo varinfo in
    let ival = Locations.(Location_Bits.find base loc.loc) in
    if Ival.is_bottom ival
    then `Bottom
    else `Value (Precise (Precise_locs.inject_ival ival))

  let backward_pointer mem offset location =
    match offset with
    | Imprecise value ->
      (* If the offset contains addresses, no reduction. *)
      `Value (value, offset)
    | Precise offset ->
      (* Offsets and locations are expressed in bits but values in bytes, so
         mem * 8 + offset == location *)
      let off_ival = Precise_locs.imprecise_offset offset in
      let loc = Precise_locs.imprecise_location location in
      let loc = loc.Locations.loc in
      (* new_off = location - (mem * 8)
         As the offset does not contain addresses, we can make the pointwise
         subtraction between the two locations. *)
      let value_bits = Locations.loc_bytes_to_loc_bits mem in
      let new_off = Locations.Location_Bits.sub_pointwise loc value_bits in
      let new_off = Ival.narrow new_off off_ival in
      let new_off = Precise_locs.inject_ival new_off in
      (* new_mem = (location - offset) * 8 *)
      let new_mem = Locations.Location_Bits.shift (Ival.neg_int off_ival) loc in
      let new_mem = Locations.loc_bits_to_loc_bytes new_mem in
      if Cvalue.V.is_bottom new_mem || Precise_locs.is_bottom_offset new_off
      then `Bottom
      else `Value (new_mem, Precise new_off)

  let backward_field typ field = function
    | Imprecise _ as x -> `Value x
    | Precise offset ->
      begin try
          let offset_ival = Precise_locs.imprecise_offset offset in
          let field = fst (Cil.bitsOffset typ (Field (field, NoOffset))) in
          let field_i = Integer.of_int (- field) in
          let ival = Ival.add_singleton_int field_i offset_ival in
          if Ival.is_bottom ival
          then `Bottom
          else `Value (Precise (Precise_locs.inject_ival ival))
        with Cil.SizeOfError _ -> `Value (Precise (Precise_locs.offset_top))
      end

  let backward_index typ_pointed ~index ~remaining offset =
    try
      match remaining, offset with
      | Imprecise _, _ | _, Imprecise _ -> `Value (index, remaining)
      | Precise remaining, Precise offset ->
        (* Index  offsets are expressed in terms of the array elements size, so
           index * size + remaining == offset *)
        let off_ival = Precise_locs.imprecise_offset offset in
        let rem_ival = Precise_locs.imprecise_offset remaining in
        let index_ival = Cvalue.V.project_ival index in
        let size = Bit_utils.sizeof typ_pointed in
        (* new_index = (offset - remaining) / size
           Beware of zero size. *)
        let new_index = Ival.sub_int off_ival rem_ival in
        let new_index = match size with
          | Int_Base.Top -> Ival.top
          | Int_Base.Value size ->
            if Integer.is_zero size
            then Ival.top
            else Ival.scale_div ~pos:true size new_index
        in
        (* new_remaining = offset - index * size *)
        let index_i = Ival.scale_int_base size index_ival in
        let new_rem = Ival.sub_int off_ival index_i in
        if Ival.is_bottom new_index || Ival.is_bottom new_rem
        then `Bottom
        else
          let index = Cvalue.V.inject_ival new_index
          and rem = Precise (Precise_locs.inject_ival new_rem) in
          `Value (index, rem)
    (* No reduction if the offsets are not arithmetics. *)
    with Cvalue.V.Not_based_on_null -> `Value (index, remaining)

end


(*
Local Variables:
compile-command: "make -C ../../../.."
End:
*)
