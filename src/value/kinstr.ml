(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2008                                               *)
(*    CEA (Commissariat à l'Énergie Atomique)                             *)
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

(* $Id: kinstr.ml,v 1.17 2008/04/18 14:29:35 uid568 Exp $ *)

open Cil_types
open Db_types
open Eval
open Locations
open Db
open CilE

(* [TODO Julien 2007/07/04]:
   - is it signature really useful?
   - should the comments be somewhere in [Db.Value]? *)
module type Kinstr_S = sig
  (* Transform an lval into a location.
     The boolean argument forces the analysis to be on bit level.
     The result boolean is true iff the transformation is exact. *)

  (* Highlevel function : uses the alias analysis
     If [deps] is present, accumulates in deps the locations
     encountered while analyzing the [lval] *)
  val lval_to_loc_with_deps :
    skip_base_deps:bool
    -> kinstr
    -> deps:Locations.Zone.t
    -> lval
    -> Locations.Zone.t * Locations.location

  val lval_to_loc : kinstr -> lval ->  Locations.location

  val expr_to_kernel_function :
    kinstr
    -> deps:Locations.Zone.t option
    -> exp
    -> Locations.Zone.t * kernel_function list
end

let lval_to_loc_with_deps_state ~with_alarms ~skip_base_deps state ~deps lv =
  let deps, r =
    (if skip_base_deps then
       lval_to_loc_with_offset_deps_only
     else lval_to_loc_with_deps)
      ~with_alarms
      ~deps
      state
      lv
  in
  Zone.out_some_or_bottom deps, r

let lval_to_loc_with_deps ~with_alarms ~skip_base_deps kinstr ~deps lv =
  CilE.start_stmt kinstr;
  let state = Db.Value.noassert_get_state kinstr in
  let result =
    lval_to_loc_with_deps_state ~with_alarms ~skip_base_deps state ~deps lv in
  CilE.end_stmt ();
  result

let lval_to_loc_with_deps_state ~skip_base_deps state ~deps lv =
  lval_to_loc_with_deps_state ~with_alarms:warn_none_mode ~skip_base_deps state ~deps lv

let lval_to_loc_kinstr kinstr ~with_alarms lv =
  CilE.start_stmt kinstr;
  let state = Db.Value.noassert_get_state kinstr in
  (*    Format.printf "@\ngot state when lval_to_loc:%a@."
	Relations_type.Model.pretty state; *)
  let r = lval_to_loc ~with_alarms state lv in
  CilE.end_stmt ();
  r

let lval_to_zone kinstr ~with_alarms lv =
  Locations.valid_enumerate_bits (lval_to_loc_kinstr ~with_alarms kinstr lv)

let lval_to_zone_state state lv =
  Locations.valid_enumerate_bits (lval_to_loc ~with_alarms:warn_none_mode state lv)

let expr_to_kernel_function_state ~with_alarms state ~deps exp =
   try
      let deps, r = resolv_func_vinfo ~with_alarms deps state exp in
      Zone.out_some_or_bottom deps, r
    with Leaf -> Zone.out_some_or_bottom deps, []

let expr_to_kernel_function kinstr  ~with_alarms ~deps exp =
  CilE.start_stmt kinstr;
  let state = Db.Value.noassert_get_state kinstr in
  (* Format.printf "STATE IS %a@\n" Relations_type.Model.pretty state;*)
  let r =
    expr_to_kernel_function_state  ~with_alarms state ~deps exp
  in
  CilE.end_stmt ();
  r

let expr_to_kernel_function_state =
  expr_to_kernel_function_state ~with_alarms:warn_none_mode

exception Top_input

let assigns_to_zone_inputs_state state assigns =
  try
    let treat_one_zone acc (_,ins) =
      match ins with
          [] -> raise Top_input
        | ins ->
            (try
               List.fold_left
                 (fun acc loc ->
                    let clocs = match loc with
                        Location loc ->
                          !Db.Properties.Interp.tsets_to_lval loc.its_content
                      | Nothing -> []
                    in
                    List.fold_left
                      (fun acc cin ->
                         Zone.join acc
                           (!Value.lval_to_zone_state state cin))
                      acc clocs)
                 acc ins
             with Invalid_argument "not a lvalue" ->
               CilE.warn_once "Can not interpret assigns clause; Ignoring.";
               raise Top_input)
    in
    match assigns with
        [] -> Zone.bottom (*VP This corresponds to the old code (v1.9)
                              Not sure this is what we really want, though.
                             *)
      | [Nothing,_] -> Zone.bottom
      | _ -> List.fold_left treat_one_zone Zone.bottom assigns
  with Top_input -> Zone.top

let lval_to_offsetmap  kinstr lv ~with_alarms =
  CilE.start_stmt kinstr;
  let state = Db.Value.noassert_get_state kinstr in
  let loc = Locations.valid_part
    (lval_to_loc ~with_alarms state lv)
  in
  let offsetmap =
    Relations_type.Model.copy_offsetmap loc state
  in
  CilE.end_stmt ();
  offsetmap





let () =
  Value.lval_to_loc_with_deps :=
    (fun s ~with_alarms ~skip_base_deps ->
       lval_to_loc_with_deps ~with_alarms ~skip_base_deps s);
  Value.lval_to_loc_with_deps_state :=
    (fun s ~skip_base_deps ->
       lval_to_loc_with_deps_state ~skip_base_deps s);
  Value.expr_to_kernel_function := expr_to_kernel_function;
  Value.expr_to_kernel_function_state := expr_to_kernel_function_state;
  Value.lval_to_loc := lval_to_loc_kinstr;
  Value.lval_to_loc_state := lval_to_loc ~with_alarms:warn_none_mode ;
  Value.lval_to_zone_state := lval_to_zone_state;
  Value.lval_to_zone := lval_to_zone;
  Value.lval_to_offsetmap := lval_to_offsetmap;
  Value.assigns_to_zone_inputs_state := assigns_to_zone_inputs_state;
  Value.eval_expr := eval_expr;
  Value.eval_lval := eval_lval;

(*
Local Variables:
compile-command: "LC_ALL=C make -C ../.."
End:
*)
