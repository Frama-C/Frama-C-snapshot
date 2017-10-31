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

(* Returns (alarm, reduced_ival)] *)
let trim_by_validity ?(origin=Origin.Unknown) ival size validity =
  let pred_size = Int.max Int.zero (Int.pred size) in
  (* reduce [ival] so that all accesses fit within [min_valid] and
     [max_maybe_valid]; uses [opt_max_sure_valid] to generate alarms *)
  let reduce_for_bounds min_valid opt_max_sure_valid max_maybe_valid =
    let max_in_bound = Int.sub max_maybe_valid pred_size in
    let is_in_bound mn mx r modu =
      let alarm_min, new_mn =
        match mn with
        | Some mn when (Int.ge mn min_valid) -> false, mn
        | _ -> true, Int.round_up_to_r ~r ~modu ~min:min_valid
      in
      let alarm_min_or_max, new_mx =
        match mx with
        | Some mx when (Int.le mx max_in_bound) ->
          let alarm_max = match opt_max_sure_valid with
            | None -> true (* all accesses are possibly invalid *)
            | Some max_sure_valid ->
              Int.gt mx (Int.sub max_sure_valid pred_size)
          in
          alarm_min || alarm_max, mx
        | _ -> true, Int.round_down_to_r ~r ~modu ~max:max_in_bound
      in
      let itv_or_set =
        if Int.le new_mn new_mx
        then begin
          if Int.equal new_mn new_mx then
            Set [new_mn] (* No need to compare [size] and [modu] in this case *)
          else
            if Int.lt modu size
            then Overlap(new_mn, Int.add new_mx pred_size, origin)
            else Interval(new_mn, new_mx, modu)
        end
        else Invalid
      in
      alarm_min_or_max, itv_or_set
    in
    begin match ival with
    | Ival.Float _ -> assert false
    | Ival.Top (mn,mx,r,m) -> is_in_bound mn mx r m
    | Ival.Set s ->
      let alarm, set =
        Array.fold_right
          (fun offset (alarm_acc, reduced_acc) ->
            let sOffset = Some offset in
            let alarm, reduced =
              is_in_bound sOffset sOffset Int.zero Int.one
            in
            alarm || alarm_acc,
            if reduced != Invalid
            then offset :: reduced_acc
            else reduced_acc)
          s
          (false, [])
      in
      if set = [] then (alarm, Invalid) else (alarm, Set set)
    end
  in
  match validity with
  | Base.Invalid ->
    true, Invalid
  | Base.Empty ->
    let valid_access = Int.(equal size zero) && Ival.(equal ival zero) in
    not valid_access, Set []
  | Base.Known (min, max) -> reduce_for_bounds min (Some max) max
  | Base.Unknown (min, sure, max) -> reduce_for_bounds min sure max
  | Base.Variable variable_v ->
    reduce_for_bounds Int.zero (Some variable_v.Base.min_alloc)
      variable_v.Base.max_alloc

(*
Local Variables:
compile-command: "make -C ../../.."
End:
*)
