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
open CilE

type t =
  | Invalid
  | Set of Int.t list
  | Interval of Int.t * Int.t * Int.t
  | Imprecise of Int.t * Int.t

exception OverlyLongForPeriodic

(* Returns [still_exact_flag, (alarm, reduce_ival)] *)
let reduce_ival_by_bound ival size validity =
  let pred_size = Int.pred size in
  match validity with
  | Base.Invalid -> true, (true, Invalid)
  | Base.Known (bound_min, bound_max) | Base.Unknown (bound_min, _, bound_max)
  | Base.Periodic (bound_min, bound_max, _) ->
      let max_in_bound = Int.sub bound_max pred_size in
      let is_in_bound mn mx r modu = 
          let out, new_mn =
            match mn with
            | Some mn when (Int.ge mn bound_min) -> false, mn
            | _ -> true, Int.round_up_to_r ~r ~modu ~min:bound_min
          in
          let out, new_mx =
            match mx with
            | Some mx when (Int.le mx max_in_bound) -> 
		let out =
		  match validity with
                    | Base.Unknown (_,Some valid_max, _)
		      when Int.gt mx (Int.sub valid_max pred_size) -> true
                    | Base.Unknown (_, None, _) -> true
		    | _ -> out
		in
		out, mx
            | _ -> true, Int.round_down_to_r ~r ~modu ~max:max_in_bound
          in
          let itv_or_set =
            if Int.le new_mn new_mx
            then begin
                if Int.lt modu size
                then Imprecise(new_mn, Int.add new_mx pred_size)
                else Interval(new_mn, new_mx, modu)
              end
            else Invalid
          in
          out, itv_or_set
      in
      let out, reduced_bounds as result =
        begin match ival with
        | Ival.Top (mn,mx,r,m) -> is_in_bound mn mx r m
        | Ival.Float _ -> is_in_bound None None Integer.zero Integer.one
        | Ival.Set s ->
            let out, set =
              Array.fold_right
                (fun offset (out_acc, reduced_acc) ->
		  let sOffset = Some offset in
                  let out, reduced =
		    is_in_bound 
		      sOffset
		      sOffset
		      Integer.zero
		      Integer.one
		  in
                  out || out_acc,
		  if reduced != Invalid
		  then offset :: reduced_acc
		  else reduced_acc)
		s
                (false, [])
            in
            if set = [] then (out, Invalid) else (out, Set set)
        end
      in
      match validity with
      | Base.Periodic(_, _, p) ->
          assert (Int.is_zero bound_min);
          let reduced_bounds =
            match reduced_bounds with
            | Invalid -> Invalid
            | Imprecise (mn, mx) ->
                if Int.equal (Int.pos_div mn p) (Int.pos_div mx p)
                then Imprecise (Int.pos_rem mn p, Int.pos_rem mx p)
                else Imprecise (bound_min, Int.pred p)
            | Set s ->
                let treat_offset acc offset =
                  let new_offset = Int.pos_rem offset p in
                  if Int.gt (Int.add new_offset size) p
                  then raise OverlyLongForPeriodic
                  else
                    (*Format.printf "old offset: %a mx: %a period: %a new: %a@."
                      Int.pretty offset Int.pretty bound_max
                      Int.pretty p Int.pretty new_offset;     *)
                    new_offset :: acc
                in
                begin
                  try
                    let trimmed = List.fold_left treat_offset [] s in
                    Set (List.sort Integer.compare trimmed)
                  with OverlyLongForPeriodic ->
                    Imprecise (bound_min, Int.pred p)
                end
            | Interval(lb, _ub, mo) ->
                if Int.is_zero (Int.pos_rem mo p)
                then Set [Int.pos_rem lb p]
                else begin
(*                    Format.printf "Interval %a %a %a / %a@."
                      Int.pretty lb
                      Int.pretty _ub
                      Int.pretty mo
                      Int.pretty p; *)
                    Imprecise (bound_min, Int.pred p)
                  end
          in
          false, (out, reduced_bounds)
      | _ -> true, result

  let filter_by_bound_for_reading ~with_alarms ival size validity =
    let _, (out, filtered_by_bound) = reduce_ival_by_bound ival size validity in
    if out then warn_mem_read with_alarms;
    filtered_by_bound

  let filter_by_bound_for_writing ~with_alarms ~exact ival size validity =
    let still_exact, (out, filtered_by_bound) =
      reduce_ival_by_bound ival size validity
    in
    if out then warn_mem_write with_alarms;
    (exact && still_exact), filtered_by_bound

(*
Local Variables:
compile-command: "make -C ../.."
End:
*)
