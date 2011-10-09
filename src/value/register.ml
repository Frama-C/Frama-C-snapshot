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
open Locations
open Eval_exprs

(** Main function of the value plugin for the kernel *)

let display_results () =
  if Db.Value.is_computed () && Value_parameters.verbose_atleast 1 then begin
    Value_parameters.result "====== VALUES COMPUTED ======";
    (* Val display and Inout compute/display *)
    !Db.Semantic_Callgraph.topologically_iter_on_functions
      (fun kf ->
         if Kernel_function.is_definition kf then
           begin
             Value_parameters.result "%a" Db.Value.display kf ;
           end)
  end

let () = Value_parameters.ForceValues.set_output_dependencies [Db.Value.self]

let main () =
  (* Value computations *)
  if Value_parameters.ForceValues.get () then begin
    !Db.Value.compute ();
    Value_parameters.ForceValues.output display_results;
  end

let () = Db.Main.extend main


(** Functions to register in Db.Value *)

let lval_to_loc_with_deps_state ~with_alarms state ~deps lv =
  let _state, deps, r =
    lval_to_loc_with_deps
      ~with_alarms
      ~deps
      ~reduce_valid_index:(Kernel.SafeArrays.get ())
      state
      lv
  in
  Zone.out_some_or_bottom deps, r

let lval_to_loc_with_deps kinstr ~with_alarms ~deps lv =
  CilE.start_stmt kinstr;
  let state = Db.Value.noassert_get_state kinstr in
  let result =
    lval_to_loc_with_deps_state ~with_alarms  state ~deps lv in
  CilE.end_stmt ();
  result

let lval_to_loc_kinstr kinstr ~with_alarms lv =
  CilE.start_stmt kinstr;
  let state = Db.Value.noassert_get_state kinstr in
  (*    Format.printf "@\ngot state when lval_to_loc:%a@."
        Cvalue.Model.pretty state; *)
  let r = lval_to_loc ~with_alarms state lv in
  CilE.end_stmt ();
  r

let lval_to_zone kinstr ~with_alarms lv =
  Locations.valid_enumerate_bits
    ~for_writing:false
    (lval_to_loc_kinstr ~with_alarms kinstr lv)

let lval_to_zone_state state lv =
  Locations.valid_enumerate_bits
    ~for_writing:false
    (lval_to_loc ~with_alarms:CilE.warn_none_mode state lv)

let expr_to_kernel_function_state ~with_alarms state ~deps exp =
   try
      let r, deps = resolv_func_vinfo ~with_alarms deps state exp in
      Zone.out_some_or_bottom deps, r
    with Leaf -> Zone.out_some_or_bottom deps, Kernel_function.Hptset.empty

let expr_to_kernel_function kinstr ~with_alarms ~deps exp =
  CilE.start_stmt kinstr;
  let state = Db.Value.noassert_get_state kinstr in
  (* Format.printf "STATE IS %a@\n" Cvalue.Model.pretty state;*)
  let r =
    expr_to_kernel_function_state  ~with_alarms state ~deps exp
  in
  CilE.end_stmt ();
  r

let expr_to_kernel_function_state =
  expr_to_kernel_function_state ~with_alarms:CilE.warn_none_mode

exception Top_input

let assigns_to_zone_inputs_state state assigns =
  try
    let treat_one_zone acc (_,ins) =
      match ins with
          FromAny -> raise Top_input
        | From l ->
          List.fold_left
            (fun acc { it_content = term } ->
              let loc_ins =
                !Db.Properties.Interp.loc_to_loc ~result:None state term
              in
              Zone.join
                acc
                (Locations.valid_enumerate_bits ~for_writing:false loc_ins))
            acc
            l
    in
    match assigns with
    | WritesAny -> Zone.top
    | Writes [] -> Zone.bottom
    | Writes l  -> List.fold_left treat_one_zone Zone.bottom l
  with
  | Top_input -> Zone.top
  | Invalid_argument "not an lvalue" ->
    Value_parameters.warning ~current:true ~once:true
      "Failed to interpret assigns clause in inputs";
    Zone.top

let lval_to_offsetmap kinstr lv ~with_alarms =
  CilE.start_stmt kinstr;
  let state = Db.Value.noassert_get_state kinstr in
  let loc = Locations.valid_part ~for_writing:false
    (lval_to_loc ~with_alarms state lv)
  in
  let offsetmap =
    Cvalue.Model.copy_offsetmap ~with_alarms loc state
  in
  CilE.end_stmt ();
  offsetmap

let lval_to_offsetmap_state state lv =
  let with_alarms = CilE.warn_none_mode in
  let loc =
    Locations.valid_part ~for_writing:false
      (lval_to_loc ~with_alarms state lv)
  in
  Cvalue.Model.copy_offsetmap ~with_alarms loc state


(* If the function is a builtin, or if the user has requested it, use
   \assigns and \from clauses, that give an approximation of the result *)
let use_spec_instead_of_definition kf =
  not (Kernel_function.is_definition kf) ||
    (let name = Kernel_function.get_name kf in
     Builtins.overridden_by_builtin name ||
     Datatype.String.Set.mem name (Value_parameters.UsePrototype.get ())
    )

let () =
  Db.Value.use_spec_instead_of_definition := use_spec_instead_of_definition;
  Db.Value.lval_to_loc_with_deps := lval_to_loc_with_deps;
  Db.Value.lval_to_loc_with_deps_state :=
    lval_to_loc_with_deps_state ~with_alarms:CilE.warn_none_mode;
  Db.Value.expr_to_kernel_function := expr_to_kernel_function;
  Db.Value.expr_to_kernel_function_state := expr_to_kernel_function_state;
  Db.Value.lval_to_loc := lval_to_loc_kinstr;
  Db.Value.lval_to_loc_state := lval_to_loc ~with_alarms:CilE.warn_none_mode ;
  Db.Value.lval_to_zone_state := lval_to_zone_state;
  Db.Value.lval_to_zone := lval_to_zone;
  Db.Value.lval_to_offsetmap := lval_to_offsetmap;
  Db.Value.lval_to_offsetmap_state := lval_to_offsetmap_state;
  Db.Value.assigns_to_zone_inputs_state := assigns_to_zone_inputs_state;
  Db.Value.eval_expr := eval_expr;
  Db.Value.eval_expr_with_state :=
    (fun ~with_alarms state expr ->
      let (s,_,v) = eval_expr_with_deps_state ~with_alarms None state expr in
      s,v);
  Db.Value.eval_lval :=
    (fun ~with_alarms deps state lval ->
      let _, deps, r = eval_lval ~conflate_bottom:true ~with_alarms deps state lval in
      deps, r);
  Db.Value.find_lv_plus := find_lv_plus;
;;


(*
Local Variables:
compile-command: "make -C ../.."
End:
*)
