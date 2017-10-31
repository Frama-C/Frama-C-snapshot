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

open Cvalue

open Cil_types
open Abstract_interp

let offsetmap_of_v ~typ v =
  let size = Int.of_int (Cil.bitsSizeOf typ) in
  let v = V_Or_Uninitialized.initialized v in
  V_Offsetmap.create ~size v ~size_v:size

let offsetmap_of_loc location state =
  let aux loc offsm_res =
    let open Locations in
    let size = Int_Base.project loc.size in
    let copy = Cvalue.Model.copy_offsetmap loc.loc size state in
    Bottom.join Cvalue.V_Offsetmap.join copy offsm_res
  in
  Precise_locs.fold aux location `Bottom

let wrap_int i = Some (offsetmap_of_v ~typ:Cil.intType i)
let wrap_ptr p = Some (offsetmap_of_v ~typ:Cil.intPtrType p)
let wrap_double d = Some (offsetmap_of_v ~typ:Cil.doubleType d)
let wrap_float d = Some (offsetmap_of_v ~typ:Cil.floatType d)
let wrap_size_t i =
  Some (offsetmap_of_v ~typ:(Cil.theMachine.Cil.typeOfSizeOf) i)
let wrap_long_long i = Some (offsetmap_of_v ~typ:Cil.longLongType i)

let v_uninit_of_offsetmap ~typ offsm =
  let size = Eval_typ.sizeof_lval_typ typ in
  match size with
    | Int_Base.Top -> V_Offsetmap.find_imprecise_everywhere offsm
    | Int_Base.Value size ->
      let validity = Base.validity_from_size size in
      let offsets = Ival.zero in
      let _alarm, r =
        V_Offsetmap.find ~validity ~conflate_bottom:false ~offsets ~size offsm
      in
      r

let backward_comp_int_left positive comp l r =
  if (Value_parameters.UndefinedPointerComparisonPropagateAll.get())
    && not (Cvalue_forward.are_comparable comp l r)
  then l
  else
    let binop = if positive then comp else Comp.inv comp in
    V.backward_comp_int_left binop l r

let backward_comp_float_left round fkind positive comp l r =
  let binop = if positive then comp else Comp.inv comp in
  V.backward_comp_float_left binop round fkind l r

let backward_comp_left_from_type t =
  match Cil.unrollType t with
  | TInt _ | TEnum _ | TPtr _ -> backward_comp_int_left
  | TFloat (fk, _) ->
    backward_comp_float_left
      (Value_parameters.AllRoundingModes.get ()) (Value_util.float_kind fk)
  | _ -> (fun _ _ v _ -> v) (* should never occur anyway *)

exception Unchanged
exception Reduce_to_bottom

let reduce_by_initialized_defined f loc state =
  try
    let base, offset =
      Locations.Location_Bits.find_lonely_key loc.Locations.loc
    in
    if Base.is_weak base then raise Unchanged;
    let size = Int_Base.project loc.Locations.size in
    let ll = Ival.project_int offset in
    let lh = Int.pred (Int.add ll size) in
    let offsm = match Model.find_base_or_default base state with
      | `Bottom | `Top -> raise Unchanged
      | `Value offsm -> offsm
    in
    let aux (offl, offh) (v, modu, shift) acc =
      let v' = f v in
      if v' != v then begin
        if V_Or_Uninitialized.is_bottom v' then raise Reduce_to_bottom;
        let il = Int.max offl ll and ih = Int.min offh lh in
        let abs_shift = Integer.pos_rem (Rel.add_abs offl shift) modu in
        (* il and ih are the bounds of the interval to reduce.
           We change the initialized flags in the following cases:
           - either we overwrite entire values, or the partly overwritten
           value is at the beginning or at the end of the subrange
           - or we do not lose information on misaligned or partial values:
           the result is a singleton *)
        if V_Or_Uninitialized.(cardinal_zero_or_one v' || is_isotropic v') ||
	  ((Int.equal offl il || Int.equal (Int.pos_rem ll modu) abs_shift) &&
           (Int.equal offh ih || Int.equal
                                   (Int.pos_rem (Int.succ lh) modu) abs_shift))
        then
          let diff = Rel.sub_abs il offl in
          let shift_il = Rel.pos_rem (Rel.sub shift diff) modu in
          V_Offsetmap.add (il, ih) (v', modu, shift_il) acc
        else acc
      end
      else acc
     in
     let noffsm =
       V_Offsetmap.fold_between ~entire:true (ll, lh) aux offsm offsm
     in
     Model.add_base base noffsm state
   with
     | Reduce_to_bottom -> Model.bottom
     | Unchanged -> state
     | Abstract_interp.Error_Top (* from Int_Base.project *)
     | Not_found (* from find_lonely_key *)
     | Ival.Not_Singleton_Int (* from Ival.project_int *) ->
         state

let reduce_by_valid_loc ~positive ~for_writing loc typ state =
  try
    let value = Cvalue.Model.find state loc in
    if Cvalue.V.is_imprecise value then
      (* we won't reduce anything anyway, and we may lose information if loc
         contains misaligned data *)
      raise Exit;
    let loc_bits = Locations.loc_bytes_to_loc_bits value in
    let size = Bit_utils.sizeof_pointed typ in
    let value_as_loc = Locations.make_loc loc_bits size in
    let reduced_value =
      Locations.loc_to_loc_without_size
        (if positive
          then Locations.valid_part ~for_writing value_as_loc
          else Locations.invalid_part value_as_loc )
    in
    if V.equal value reduced_value
    then state
    else begin
      if V.equal V.bottom reduced_value
      then Cvalue.Model.bottom
      else
        Cvalue.Model.reduce_previous_binding state loc reduced_value
    end
  with Exit -> state

let make_loc_contiguous loc =
  try
    let base, offset =
      Locations.Location_Bits.find_lonely_key loc.Locations.loc
    in
    match offset, loc.Locations.size with
    | Ival.Top (Some min, Some max, _rem, modu), Int_Base.Value size
         when Int.equal modu size ->
       let size' = Int.add (Int.sub max min) modu in
       let i = Ival.inject_singleton min in
       let loc_bits = Locations.Location_Bits.inject base i in
       Locations.make_loc loc_bits (Int_Base.inject size')
    | _ -> loc
  with Not_found -> loc

let apply_on_all_locs f loc state =
  match loc.Locations.size with
  | Int_Base.Top -> state
  | Int_Base.Value _ as size ->
    try
      let loc = Locations.valid_part ~for_writing:false loc in
      let loc = loc.Locations.loc in
      let plevel = Value_parameters.ArrayPrecisionLevel.get() in
      ignore (Locations.Location_Bits.cardinal_less_than loc plevel);
      Locations.Location_Bits.fold_enum
        (fun l acc -> f (Locations.make_loc l size) acc) loc state
    with Not_less_than | Abstract_interp.Error_Top -> state

(* Display [o] as a single value, when this is more readable and more precise
   than the standard display. *)
let pretty_stitched_offsetmap fmt typ o =
  if Cil.isArithmeticOrPointerType typ &&
     not (Cvalue.V_Offsetmap.is_single_interval o)
  then
    let v = v_uninit_of_offsetmap ~typ o in
    if not (Cvalue.V_Or_Uninitialized.is_isotropic v)
    then
      Format.fprintf fmt "@\nThis amounts to: %a"
        Cvalue.V_Or_Uninitialized.pretty v

let pretty_offsetmap typ fmt offsm =
  (* YYY: catch pointers to arrays, and print the contents of the array *)
  Format.fprintf fmt "@[";
  if Cvalue.V_Offsetmap.(equal empty offsm)
  then Format.fprintf fmt "%s" (Unicode.emptyset_string ())
  else begin
    match Cvalue.V_Offsetmap.single_interval_value offsm with
    | Some value -> Cvalue.V_Or_Uninitialized.pretty fmt value;
    | None ->
      Cvalue.V_Offsetmap.pretty_generic ~typ () fmt offsm;
      pretty_stitched_offsetmap fmt typ offsm
  end;
  Format.fprintf fmt "@]"

(*
Local Variables:
compile-command: "make -C ../../../.."
End:
*)
