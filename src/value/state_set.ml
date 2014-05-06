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

let to_list l = List.map fst l

let reorder l = List.rev l

let add_statement states s =
  List.map (fun (state,trace) -> (state, Trace.add_statement s trace)) states
;;

(*
Local Variables:
compile-command: "make -C ../.."
End:
*)
