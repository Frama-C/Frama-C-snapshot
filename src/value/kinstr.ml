(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2011                                               *)
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
open Eval
open Db_types
open Locations
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

let lval_to_loc_with_deps_state ~with_alarms state ~deps lv =
  let state, deps, r =
    Eval.lval_to_loc_with_deps
      ~with_alarms
      ~deps
      ~reduce_valid_index:(Parameters.SafeArrays.get ())
      state
      lv
  in
  state, Zone.out_some_or_bottom deps, r

let lval_to_loc_with_deps ~with_alarms  kinstr ~deps lv =
  CilE.start_stmt kinstr;
  let state = Db.Value.noassert_get_state kinstr in
  let result =
    lval_to_loc_with_deps_state ~with_alarms  state ~deps lv in
  CilE.end_stmt ();
  result

let lval_to_loc_with_deps_state  state ~deps lv =
  lval_to_loc_with_deps_state ~with_alarms:warn_none_mode  state ~deps lv

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
    with Leaf -> Zone.out_some_or_bottom deps, Kernel_function.Hptset.empty

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
          FromAny -> raise Top_input
        | From l ->
          List.fold_left
	    (fun acc term ->
	      let loc_ins =
	        !Db.Properties.Interp.identified_term_zone_to_loc ~result:None
	          state
	          term
	      in
	      Zone.join
	        acc
	        (Locations.valid_enumerate_bits loc_ins))
	    acc
	    l
    in
    match assigns with
      WritesAny -> Zone.bottom  (*VP This corresponds to the old code 
                                  (cvs rev 1.9)
                                  Not sure this is what we really want, though.
                                 *)
    | Writes [] -> Zone.bottom
    | Writes l  -> List.fold_left treat_one_zone Zone.bottom l
  with
    Top_input -> Zone.top
  | Invalid_argument "not an lvalue" ->
      CilE.warn_once "Failed to interpret assigns clause in inputs";
       Zone.top

let lval_to_offsetmap  kinstr lv ~with_alarms =
  CilE.start_stmt kinstr;
  let state = Db.Value.noassert_get_state kinstr in
  let loc = Locations.valid_part
    (lval_to_loc ~with_alarms state lv)
  in
  let offsetmap =
    Relations_type.Model.copy_offsetmap ~with_alarms loc state
  in
  CilE.end_stmt ();
  offsetmap

let lval_to_offsetmap_state state lv =
  let with_alarms = CilE.warn_none_mode in
  let loc = Locations.valid_part (lval_to_loc ~with_alarms state lv) in
  Relations_type.Model.copy_offsetmap ~with_alarms loc state


let () =
  Db.Value.lval_to_loc_with_deps :=
    (fun s ~with_alarms ~deps lval ->
      let _, deps, r = lval_to_loc_with_deps ~with_alarms s ~deps lval in
      deps, r);
  Db.Value.lval_to_loc_with_deps_state :=
    (fun s ~deps lval ->
      let _, deps, r = lval_to_loc_with_deps_state s ~deps lval in
      deps, r);
  Db.Value.expr_to_kernel_function := expr_to_kernel_function;
  Db.Value.expr_to_kernel_function_state := expr_to_kernel_function_state;
  Db.Value.lval_to_loc := lval_to_loc_kinstr;
  Db.Value.lval_to_loc_state := lval_to_loc ~with_alarms:warn_none_mode ;
  Db.Value.lval_to_zone_state := lval_to_zone_state;
  Db.Value.lval_to_zone := lval_to_zone;
  Db.Value.lval_to_offsetmap := lval_to_offsetmap;
  Db.Value.lval_to_offsetmap_state := lval_to_offsetmap_state;
  Db.Value.assigns_to_zone_inputs_state := assigns_to_zone_inputs_state;
  Db.Value.eval_expr := eval_expr;
  Db.Value.eval_lval :=
    (fun ~with_alarms deps state lval ->
      let _, deps, r = eval_lval ~conflate_bottom:true ~with_alarms deps state lval in
      deps, r)

(*
Local Variables:
compile-command: "make -C ../.."
End:
*)
