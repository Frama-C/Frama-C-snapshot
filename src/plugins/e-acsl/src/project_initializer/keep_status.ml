(**************************************************************************)
(*                                                                        *)
(*  This file is part of the Frama-C's E-ACSL plug-in.                    *)
(*                                                                        *)
(*  Copyright (C) 2012-2019                                               *)
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

(* E-ACSL needs to access to the property status of every property (in
   particular for the option -e-acsl-valid). However, the necessary elements for
   building a property are copied/modified several times from the original
   project to the final project and the property statuses are destroyed when
   creating the intermediate projects; so there is no easy way to access to
   property statuses from the final project.

   This module aims at solving this issue by providing an access to property
   statuses from the final project. To work properly, it requires to visit every
   property during the final visit in the very same order than during the AST
   preparation visit. Indeed, for each function, it associates to each
   property an unique integer corresponding to its visit ordering.  *)

(* the kind is only used for a few additional consistency checks between [push]
   and [must_translate]*)
type kind =
  | K_Assert
  | K_Invariant
  | K_Variant
  | K_StmtSpec
  | K_Allocation
  | K_Assigns
  | K_Decreases
  | K_Terminates
  | K_Complete
  | K_Disjoint
  | K_Requires
  | K_Ensures

let pretty_kind fmt k =
  Format.fprintf fmt "%s"
    (match k with
    | K_Assert -> "assert"
    | K_Invariant -> "invariant"
    | K_Variant -> "variant"
    | K_StmtSpec -> "stmtspec"
    | K_Allocation -> "allocation"
    | K_Assigns -> "assigns"
    | K_Decreases -> "decreases"
    | K_Terminates -> "terminates"
    | K_Complete -> "complete"
    | K_Disjoint -> "disjoint"
    | K_Requires -> "requires"
    | K_Ensures -> "ensures")

(* information attached to every kernel_function containing an annotation *)
type kf_info =
    { mutable cpt: int; (* counter building the relationship between [push] and
                           [must_translate *)
      mutable statuses: (kind * bool) Datatype.Int.Map.t
        (* map associating a property as an integer to its kind and status
           ([true] = proved) *) }

(* statuses for each function represented by its name (because the [kf] itself
   changes from a project to another). *)
let keep_status
    : kf_info Datatype.String.Hashtbl.t
    = Datatype.String.Hashtbl.create 17

(* will contain the value of a few options from the original project
   in order to safely use them from the final project. *)
let option_valid = ref false
let option_check = ref false

let clear () =
  Datatype.String.Hashtbl.clear keep_status;
  option_valid := Options.Valid.get ();
  option_check := Options.Check.get ()

let push kf kind ppt =
(*  Options.feedback "PUSHING %a for %a"
    pretty_kind kind
    Kernel_function.pretty kf;*)
  (* no registration when -e-acsl-check or -e-acsl-valid *)
  if not (!option_check || !option_valid) then
    let keep =
      let open Property_status in
      match get ppt with
      | Never_tried
      | Inconsistent _
      | Best ((False_if_reachable | False_and_reachable | Dont_know), _) ->
        true
      | Best (True, _) ->
        false
    in
    let status = kind, keep in
    let name = Kernel_function.get_name kf in
    try
      let info = Datatype.String.Hashtbl.find keep_status name in
      info.cpt <- info.cpt + 1;
      info.statuses <- Datatype.Int.Map.add info.cpt status info.statuses
    with Not_found ->
      let info = { cpt = 1; statuses = Datatype.Int.Map.singleton 1 status } in
      Datatype.String.Hashtbl.add keep_status name info

let before_translation () =
  (* reset all counters *)
  Datatype.String.Hashtbl.iter (fun _ info -> info.cpt <- 0) keep_status

let must_translate kf kind =
(*  Options.feedback "GETTING %a for %a"
    pretty_kind kind
    Kernel_function.pretty kf;*)
  !option_check
  ||
    !option_valid
  ||
    (* function contracts have been moved from the original function to its
       duplicate by [Dup_function] but they are still associated to the original
       function here *)
    let name = Functions.RTL.get_original_name kf in
    try
    let info =
      try Datatype.String.Hashtbl.find keep_status name
      with Not_found ->
        Options.fatal "[keep_status] unbound function" Datatype.String.pretty kf
    in
    info.cpt <- info.cpt + 1;
    let kind', keep =
      try Datatype.Int.Map.find info.cpt info.statuses
      with Not_found ->
        Options.fatal "[keep_status] unbound annotation (id %d)" info.cpt
    in
    (* check kind consistency in order to detect more abnormal behaviors *)
    if kind <> kind' then
      Options.fatal "[keep_status] incorrect kind '%a' (expected: '%a')"
        pretty_kind kind
        pretty_kind kind';
    keep
    with Not_found -> true
