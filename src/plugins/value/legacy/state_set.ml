(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2016                                               *)
(*    CEA (Commissariat à l'énergie atomique et aux énergies              *)
(*         alternatives)                                                  *)
(*                                                                        *)
(*  All rights reserved.                                                  *)
(*  Contact CEA LIST for licensing.                                       *)
(*                                                                        *)
(**************************************************************************)

(* We store model -> trace information as a simple association
   list. This suffices because we do not need the find operation. *)
type t = (Cvalue.Model.t * Trace.t) list

let obviously_terminates = false

let fold = List.fold_left

let of_list_forget_history l = List.map (fun v -> (v, Trace.top)) l
let of_list l = l

let iter = List.iter
let map = List.map

let empty = []

let is_empty t = t = empty

let exists f = List.exists (fun (v,_) -> f v)

let length = List.length

exception Unchanged
let pretty fmt s =
  List.iter
    (fun (state,_trace) ->
      Format.fprintf fmt "set contains %a@\n"
        Cvalue.Model.pretty state)
    s

(* TODO: we forget about one trace when doing that. This is not a
   problem while traces are being used only for alarms. What should be
   done is merging traces, but this cannot be done in the stateset
   because of the Unchanged exception; changing this would make the
   dataflow propagate the stateset for no reason. So this requires
   proper implementation in the state_imp. *)
let add_to_list pair s =
  let (v,_) = pair in
  if (not (Cvalue.Model.is_reachable v))
    || ((not obviously_terminates) && 
	   (List.exists
	      (fun (e,_) -> Cvalue.Model.is_included v e)
	      s))
  then raise Unchanged;
  pair :: s

let add_exn p s = add_to_list p s

let merge_into sa ~into:sb = 
  let unchanged = ref true in
  let f acc e =
    try
      let r = add_exn  e acc in
      unchanged := false;
      r
    with Unchanged ->
      acc
  in
  let result = List.fold_left f sb sa in
  if !unchanged then raise Unchanged;
  result

let merge sa sb = 
  try merge_into sa ~into:sb
  with Unchanged -> sb
;;

let add p s =
  try
    add_exn p s
  with Unchanged -> s

let singleton p = add p empty ;;

let join s =
  List.fold_left
    (fun (accm,acct) (m,t) ->
      Cvalue.Model.join accm m,
      Trace.join acct t
    ) (Cvalue.Model.bottom, Trace.bottom)
    s

(* Computes a greatest lower bound of two disjoint unions of states.
   This computation is not as precise as computing each pairwise
   narrow between states, but it avoids a quadratic increase in the number of
   computations. *)
let narrow st1 st2 =
  let us1, ut1 = join st1 in
  let us2, ut2 = join st2 in
  let unmerged =
    List.map (fun (s1, t1) -> Cvalue.Model.narrow s1 us1, Trace.narrow t1 ut1) st2 @
    List.map (fun (s1, t1) -> Cvalue.Model.narrow s1 us2, Trace.narrow t1 ut2) st1
  in
  (* Remove eventual duplicates *)
  List.fold_right add unmerged []

(* Computes [narrow] with all the state sets in [stl].
   [stl] must not be empty.
   Note: defining this function inside State_set avoids list boxing/unboxing. *)
let narrow_list stl =
  let s = List.map join stl in
  let (snarrow, tnarrow) =
    List.fold_left
      (fun (acc_s, acc_t) (s, t) ->
         Cvalue.Model.narrow s acc_s, Trace.narrow t acc_t
      ) (Cvalue.Model.top, Trace.top) s
  in
  List.fold_left
    (fun acc st ->
       let narrowed_st =
         List.map
           (fun (s, t) -> Cvalue.Model.narrow s snarrow, Trace.narrow t tnarrow)
           st
       in
       merge narrowed_st acc) [] stl

let to_list l = List.map fst l

let reorder l = List.rev l

let add_statement states s =
  List.map (fun (state,trace) -> (state, Trace.add_statement s trace)) states
;;

(*
Local Variables:
compile-command: "make -C ../../../.."
End:
*)
