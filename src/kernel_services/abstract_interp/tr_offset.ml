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

type t =
  | Invalid
  | Set of Int.t list
  | Interval of Int.t * Int.t * Int.t
  | Overlap of Int.t * Int.t * Origin.t

let pretty fmt = function
  | Invalid -> Format.fprintf fmt "Invalid"
  | Set l -> Format.fprintf fmt "Set [%a]"
               (Pretty_utils.pp_list ~sep:",@ " Int.pretty) l
  | Interval (mn, mx, modu) -> Format.fprintf fmt "Interval (%a,%a,%a)"
                                 Int.pretty mn Int.pretty mx Int.pretty modu
  | Overlap (mn, mx, o) -> Format.fprintf fmt "Overlap (%a,%a,%a)"
                             Int.pretty mn Int.pretty mx Origin.pretty o

(* Reduces [ival] for an access according to [validity]. *)
let reduce_offset_by_validity origin ival size validity =
  (* Reduces [ival] so that all accesses fit within [min] and [max]. *)
  let reduce_for_bounds min max =
    if Integer.is_zero size
    then Set []
    else
      let max_valid = Int.sub max (Int.pred size) in
      let valid_range = Ival.inject_range (Some min) (Some max_valid) in
      let reduced_ival = Ival.narrow ival valid_range in
      match reduced_ival with
      | Ival.Float _ -> assert false
      | Ival.Set s -> if s = [||] then Invalid else Set (Array.to_list s)
      | Ival.Top (min, max, _r, modu) ->
        (* The bounds are finite thanks to the narrow with the valid range. *)
        let min = Extlib.the min and max = Extlib.the max in
        if Int.lt modu size
        then Overlap (min, Int.add max (Int.pred size), origin)
        else Interval (min, max, modu)
  in
  match validity with
  | Base.Invalid -> Invalid
  | Base.Empty -> Set []
  | Base.Known (min, max)
  | Base.Unknown (min, _, max) -> reduce_for_bounds min max
  | Base.Variable v -> reduce_for_bounds Int.zero v.Base.max_alloc

let trim_by_validity ?(origin=Origin.Unknown) ival size validity =
  let alarm = not (Base.offset_is_in_validity size validity ival) in
  let offset = reduce_offset_by_validity origin ival size validity in
  alarm, offset

(*
Local Variables:
compile-command: "make -C ../../.."
End:
*)
