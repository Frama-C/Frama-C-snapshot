(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2019                                               *)
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
open Cvalue

exception Invalid_nb_of_args of int

(* 'Always' means the builtin will always be used to replace a function
   with its name. 'OnAuto' means that the function will be replaced only
   if -val-builtins-auto is set. *)
type use_builtin = Always | OnAuto

let table = Hashtbl.create 17

let register_builtin name ?replace f =
  Hashtbl.replace table name (f, None, Always);
  match replace with
  | None -> ()
  | Some fname -> Hashtbl.replace table fname (f, Some name, OnAuto)

let () = Db.Value.register_builtin := register_builtin

(* The functions in _builtin must only return the 'Always' builtins *)

let registered_builtins () =
  let l =
    Hashtbl.fold
      (fun name (f, _, u) acc -> if u = Always then (name, f) :: acc else acc)
      table []
  in
  List.sort (fun (name1, _) (name2, _) -> String.compare name1 name2) l

let () = Db.Value.registered_builtins := registered_builtins

let builtin_names_and_replacements () =
  let stand_alone, replacements =
    Hashtbl.fold (fun name (_, replaced_by, _) (acc1, acc2) ->
        match replaced_by with
        | None -> name :: acc1, acc2
        | Some rep_by -> acc1, (name, rep_by) :: acc2
      ) table ([], [])
  in
  List.sort String.compare stand_alone,
  List.sort (fun (name1, _) (name2, _) -> String.compare name1 name2) replacements

let () =
  Cmdline.run_after_configuring_stage
    (fun () ->
       if Value_parameters.BuiltinsList.get () then begin
         let stand_alone, replacements = builtin_names_and_replacements () in
         Log.print_on_output
           (fun fmt ->
              Format.fprintf fmt "@[*** LIST OF EVA BUILTINS@\n@\n\
                                  ** Replacements set by -val-builtins-auto:\
                                  @\n   unless otherwise specified, \
                                  function <f> is replaced by builtin \
                                  Frama_C_<f>:@\n@\n   @[%a@]@]@\n"
                (Pretty_utils.pp_list ~sep:",@ "
                   (fun fmt (name, rep_by) ->
                      if rep_by = "Frama_C_" ^ name then
                        Format.fprintf fmt "%s" name
                      else
                        Format.fprintf fmt "%s (replaced by: %s)" name rep_by))
                replacements);
         Log.print_on_output
           (fun fmt ->
              Format.fprintf fmt "@\n@[** Full list of builtins \
                                  (configurable via -val-builtin):@\n\
                                  @\n   @[%a@]@]@\n"
                (Pretty_utils.pp_list ~sep:",@ "
                   Format.pp_print_string) stand_alone);
         raise Cmdline.Exit
       end)

let mem_builtin name =
  try
    let _, _, u = Hashtbl.find table name in
    u = Always
  with Not_found -> false

let () = Db.Value.mem_builtin := mem_builtin

(* Returns the builtin with its specification, used to evaluate preconditions
   and to transfer the states of other domains. *)
let find_builtin_specification kf =
  let spec = Annotations.funspec kf in
  (* The specification can be empty if [kf] has a body but no specification,
     in which case [Annotations.funspec] does not generate a specification.
     TODO: check that the specification is the frama-c libc specification? *)
  if spec.spec_behavior <> [] then Some spec else None

let find_builtin_override kf =
  let name =
    try Value_parameters.BuiltinsOverrides.find kf
    with Not_found -> Kernel_function.get_name kf
  in
  try
    let f, _, u = Hashtbl.find table name in
    if u = Always || Value_parameters.BuiltinsAuto.get ()
    then Extlib.opt_map (fun s -> name, f, s) (find_builtin_specification kf)
    else None
  with Not_found -> None

let warn_builtin_override bname kf =
  let source = fst (Kernel_function.get_location kf) in
  if find_builtin_specification kf = None
  then
    Value_parameters.warning ~source ~once:true
      ~wkey:Value_parameters.wkey_builtins_missing_spec
      "The builtin for function %a will not be used, as its frama-c libc \
       specification is not available."
      Kernel_function.pretty kf
  else
    let internal =
      let pos = fst (Kernel_function.get_location kf) in
      (*TODO: treat this 'internal'*)
      let file = pos.Filepath.pos_path in
      Filepath.is_relative ~base_name:Config.datadir (file :> string)
    in
    if Kernel_function.is_definition kf && not internal
    then
      let fname = Kernel_function.get_name kf in
      Value_parameters.warning ~source ~once:true
        ~wkey:Value_parameters.wkey_builtins_override
        "function %s: definition will be overridden by %s"
        fname (if fname = bname then "its builtin" else "builtin " ^ bname)

let warn_definitions_overridden_by_builtins () =
  Value_parameters.BuiltinsOverrides.iter
    (fun (kf, name) -> warn_builtin_override (Extlib.the name) kf);
  let autobuiltins = Value_parameters.BuiltinsAuto.get () in
  Hashtbl.iter
    (fun name (_, _, u) ->
       if autobuiltins || u = Always
       then
         try
           let kf = Globals.Functions.find_by_name name in
           warn_builtin_override name kf
         with Not_found -> ())
    table

(* -------------------------------------------------------------------------- *)
(* --- Returning a clobbered set                                          --- *)
(* -------------------------------------------------------------------------- *)

let clobbered_set_from_ret state ret =
  let aux b _ acc =
    match Model.find_base_or_default b state with
    | `Top -> Base.SetLattice.top
    | `Bottom -> acc
    | `Value m ->
      if Locals_scoping.offsetmap_contains_local m then
        Base.SetLattice.(join (inject_singleton b) acc)
      else acc
  in
  try V.fold_topset_ok aux ret Base.SetLattice.bottom
  with Abstract_interp.Error_Top -> Base.SetLattice.top

(* -------------------------------------------------------------------------- *)
(* --- Applying a builtin                                                 --- *)
(* -------------------------------------------------------------------------- *)

type call = (Precise_locs.precise_location, Cvalue.V.t) Eval.call
type result = Cvalue.Model.t * Locals_scoping.clobbered_set
type builtin = Db.Value.builtin_sig

open Eval

let unbottomize = function
  | `Bottom -> Cvalue.V.bottom
  | `Value v -> v

let offsetmap_of_formals state arguments rest =
  let compute expr assigned =
    let offsm = Cvalue_offsetmap.offsetmap_of_assignment state expr assigned in
    let value = unbottomize (Eval.value_assigned assigned) in
    expr, value, offsm
  in
  let treat_one_formal arg = compute arg.concrete arg.avalue in
  let treat_one_rest (exp, v) = compute exp v in
  let list = List.map treat_one_formal arguments in
  let rest = List.map treat_one_rest rest in
  list @ rest

let compute_builtin name builtin state actuals =
  try builtin state actuals
  with
  | Invalid_nb_of_args n ->
    Value_parameters.error ~current:true
      "Invalid number of arguments for builtin %s: %d expected, %d found"
      name n (List.length actuals);
    raise Db.Value.Aborted
  | Db.Value.Outside_builtin_possibilities ->
    Value_parameters.warning ~once:true ~current:true
      "Call to builtin %s failed, aborting." name;
    raise Db.Value.Aborted

let apply_builtin builtin call state =
  let name = Kernel_function.get_name call.kf in
  let actuals = offsetmap_of_formals state call.arguments call.rest in
  let res = compute_builtin name builtin state actuals in
  let call_stack = Value_util.call_stack () in
  Db.Value.Call_Type_Value_Callbacks.apply (`Builtin res, state, call_stack);
  let clob = Locals_scoping.bottom () in
  Locals_scoping.remember_bases_with_locals clob res.Value_types.c_clobbered;
  let process_one_return acc (ret, post_state) =
    if Cvalue.Model.is_reachable post_state then
      let state =
        match ret, call.return with
        | Some offsm_ret, Some vi_ret ->
          let b_ret = Base.of_varinfo vi_ret in
          Cvalue.Model.add_base b_ret offsm_ret post_state
        | _, _ -> post_state
      in
      (state, clob) :: acc
    else
      acc
  in
  let list = List.fold_left process_one_return [] res.Value_types.c_values in
  list, res.Value_types.c_cacheable

(*
Local Variables:
compile-command: "make -C ../../../../.."
End:
*)
