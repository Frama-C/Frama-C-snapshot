(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2015                                               *)
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
open Abstract_interp
open Cvalue

let is_bitfield typlv =
  match Cil.unrollType typlv with
    | TInt (_, attrs) | TEnum (_, attrs) ->
        (match Cil.findAttribute Cil.bitfield_attribute_name attrs with
           | [AInt _] -> true
           | _ -> false)
    | _ -> false

let sizeof_lval_typ typlv =
  match Cil.unrollType typlv with
    | TInt (_, attrs) | TEnum (_, attrs) as t ->
        (match Cil.findAttribute Cil.bitfield_attribute_name attrs with
           | [AInt i] -> Int_Base.Value i
           | _ -> Bit_utils.sizeof t)
    | t -> Bit_utils.sizeof t

(* TODO: this should probably be also put directly in reinterpret_int *)
let cast_lval_if_bitfield typlv size v =
  match size with
    | Int_Base.Top -> v (* Bitfields have known sizes *)
    | Int_Base.Value size ->
      if is_bitfield typlv then begin
        try
          ignore (V.project_ival_bottom v);
          let signed = Bit_utils.is_signed_int_enum_pointer typlv in
          let v, _ok = Cvalue.V.cast ~size ~signed v in
          v (* TODO: handle not ok case as a downcast *)
        with
        | V.Not_based_on_null (* from [project_ival] *) ->
          (* [v] is a pointer: check there are enough bits in
             the bit-field to contain it. *)
          if Int.ge size (Int.of_int (Bit_utils.sizeofpointer ())) ||
            V.is_imprecise v
          then v
          else begin
            Value_parameters.result
              "casting address to a bit-field of %s bits: \
                this is smaller than sizeof(void*)" (Int.to_string size);
            V.topify_arith_origin v
          end
      end
      else v



let need_cast t1 t2 =
  match Cil.unrollType t1, Cil.unrollType t2 with
    | (TInt _| TEnum _| TPtr _), (TInt _| TEnum _| TPtr _) | TFloat _, TFloat _
      ->
      (try Cil.bitsSizeOf t1 <> Cil.bitsSizeOf t2
       with Cil.SizeOfError _ -> true)
    | TComp (c1, _, _), TComp (c2, _, _) -> c1 != c2
    | _ -> true


let offsetmap_matches_type typ_lv o =
  let aux typ_matches = match V_Offsetmap.single_interval_value o with
    | None -> true (* multiple bindings. Assume that type matches *)
    | Some v ->
      let v = V_Or_Uninitialized.get_v v in
      try typ_matches (V.project_ival_bottom v)
      with V.Not_based_on_null -> true (* Do not mess with pointers *)
  in
  let is_float = function
    | Ival.Float _ -> true
    | Ival.Top _ -> false
    | Ival.Set _ as i -> Ival.(equal zero i || equal bottom i)
  in
  let is_int = function
    | Ival.Top _ | Ival.Set _ -> true
    | Ival.Float _ -> false
  in
  match Cil.unrollType typ_lv with
  | TFloat _ -> aux is_float
  | TInt _ | TEnum _ | TPtr _ -> aux is_int
  | _ -> true
