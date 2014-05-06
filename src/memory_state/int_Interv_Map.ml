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

(** Undocumented. 
    Do not use this module if you don't know what you are doing. *)

open Abstract_interp

module Make(Value: Rangemap.Value) = struct

  include Rangemap.Make(Int_Interv)(Value)

  let check (bi,ei) = assert (Int.le bi ei)

  let add x = check x; add x
  let find x = check x; find x

  exception No_binding_above

  let find_above i m =
    let o (b2, _e2) = Int.le i b2
    in
    lowest_binding_above o m

  let pretty pretty_v fmt m =
    Pretty_utils.pp_iter
      ~pre:"@[<hv 1>{"
      ~suf:"}@]"
      ~sep:" ;@ "
      (fun pp map -> iter (fun bi_ei v -> pp (bi_ei, v)) map)
      (fun fmt ((bi, ei), v) ->
         Format.fprintf fmt "[%a..%a] -> %a"
           Int.pretty bi Int.pretty ei
           pretty_v v)
      fmt
      m

  let enlarge_to_right ~extend_right same_values ei new_vv acc =
    if extend_right then
      (* look for an interval starting just after i *)
      let s_ei = Int.succ ei in
        match concerned_intervals Int_Interv.fuzzy_order (s_ei,s_ei) acc
        with [] -> acc,ei
          | [(ba,ea) as a,vva] ->
              assert (Int.equal ba s_ei);
              if same_values vva new_vv then
                (remove a acc),ea
              else acc,ei
          | _ -> assert false
    else acc,ei

  let handle_rightmost_itv
      ~extend_right
      same_values ei new_vv ((_,ei1),vv1) acc =
    if Int.gt ei1 ei
    then (* Part of the previous binding remains
            on the right-hand-side *)
      if extend_right && same_values vv1 new_vv
      then (* same value -> merge keys *)
        acc,ei1
      else add (Int.succ ei, ei1) vv1 acc,ei
    else enlarge_to_right ~extend_right same_values ei new_vv acc

  let enlarge_to_left ~extend_left same_values bi new_vv acc =
    if extend_left then
    (* look for an interval ending just before i *)
      let p_bi = Int.pred bi in
        match concerned_intervals Int_Interv.fuzzy_order (p_bi,p_bi) acc
        with [] -> acc,bi
          | [(ba,ea) as a,vva] -> assert (Int.equal ea p_bi);
              if same_values vva new_vv then
                (remove a acc),ba
              else acc,bi
          | _ -> assert false
    else acc, bi

  let handle_leftmost_itv ~extend_left same_values bi new_vv
      ((bi1,_),vv1) acc =
    if Int.lt bi1 bi
    then   (* Part of the previous binding remains
              on the left-hand-side *)
      if extend_left && same_values vv1 new_vv
      then (* same value -> merge keys *)
        acc,bi1
      else add (bi1, Int.pred bi) vv1 acc,bi
    else enlarge_to_left ~extend_left same_values bi new_vv acc


  let cleanup_overwritten_bindings
      ?(extend_left=true) ?(extend_right=true)
      same_values (bi,ei as i) new_vv m =
    (* if not (extend_right && extend_left) then
      Format.printf "left:%b right:%b@\n" extend_left extend_right; *)
    let concerned_intervals =
      concerned_intervals Int_Interv.fuzzy_order i m
    in
    let result = match concerned_intervals with
    | [] ->
        let acc,new_bi =
          enlarge_to_left ~extend_left same_values bi new_vv m in
        let acc,new_ei =
          enlarge_to_right ~extend_right same_values ei new_vv acc in
        Some(new_bi, new_ei, acc)
    | [((bi1, ei1) as i1, vv1) as binding1] ->
        let cond_start = Int.le bi1 bi in
        let cond_end = Int.ge ei1 ei in
        let cond_same = same_values vv1 new_vv in
        if (cond_start && cond_end && cond_same && extend_right && extend_left)
        then None   (* nothing to do, the new interval is included in the
                    previous one and the old and new values are the same*)
        else begin
          let result1 = remove i1 m in
          let result2,new_bi =
            handle_leftmost_itv
              same_values ~extend_left bi new_vv binding1 result1
          in
          let result3,new_ei =
            handle_rightmost_itv
              ~extend_right
              same_values ei new_vv binding1 result2
          in
          Some(new_bi, new_ei, result3)
        end
    | ((_bi1, _ei1), _vv1 as binding1)::tail ->
        let result1 =
          List.fold_right
            (fun (i1,_) acc -> remove i1 acc)
            concerned_intervals
            m
        in
        (* part of the last interval might remain on the right *)
        let result2,new_ei =
          handle_rightmost_itv
            ~extend_right
            same_values ei new_vv binding1 result1
        in
        let rec f l acc =
          match l with
          | [] -> assert false
              (* at least 2 elements in [concerned_intervals] *)
          | [(_bi1, _ei1), _vv1 as binding1] ->
              (* part of the first interval might remain on the left *)
              handle_leftmost_itv ~extend_left
                same_values bi new_vv binding1 acc
          | ((_bi1, _ei1), _vv1)::tail ->
              (* the middle intervals are completely covered : ignore
                 former values *)
              f tail acc
        in
        let result3,new_bi = f tail result2 in
        Some(new_bi, new_ei, result3)
  in
  (* if not (extend_right && extend_left) then
    (match result with None -> Format.printf "Cleanup...NONE@\n"
     | Some (new_bi,new_ei,_) ->
         Format.printf "Cleanup...new_bi:%a new_ei:%a@\n" Int.pretty new_bi
           Int.pretty new_ei);*)
    result

  let remove_itv _fuzzy_order (start,stop as ss) to_ =
    let concerned_intervals =
      concerned_intervals Int_Interv.fuzzy_order ss to_ in
      List.fold_left
        (fun acc (bi,ei as i,vv) ->
           let r = remove i acc in
           let r = if Int.lt bi start then add (bi,Int.pred start) vv r
           else r
           in let r = if Int.gt ei stop then add (Int.succ stop,ei) vv r
             else r
           in r)
        to_ concerned_intervals

  let shift offs m =
    mapii (fun k v -> Int_Interv.shift offs k, v) m

end

(*
Local Variables:
compile-command: "make -C ../.."
End:
*)
