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

open Cvalue
open Cil_types

let dkey = Value_parameters.register_category "restart"

let base_cache : (int, Base.t) Hashtbl.t = Hashtbl.create 41
let v_cache = V.Hashtbl.create 53

(* Used to identify and remove escaping values from globals *)
exception Possibly_escaping_value

let import_varinfo (vi : varinfo) ~importing_value =
  try
    if Cil.isFunctionType vi.vtype then
      let kf = Globals.Functions.find_by_name vi.vname in
      Kernel_function.get_vi kf
    else begin
      let vi' = Globals.Vars.find_from_astinfo vi.vname VGlobal in
      if vi.vstorage = Static then
        Value_parameters.warning ~once:true
          "loaded state contains static variables;@ AST ordering@ \
           cannot be enforced and must be manually checked for soundness@ \
           (e.g. ensure that files are processed in the same order)";
      vi'
    end
  with Not_found ->
    (* search in the state *)
    (* alloced_return_* variables can have their kf restored via their name *)
    if Extlib.string_prefix "alloced_return_" vi.vname then begin
      let len = String.length "alloced_return_" in
      let kf_name = String.sub vi.vname len (String.length vi.vname - len) in
      try
        let kf = Globals.Functions.find_by_name kf_name in
        Value_parameters.feedback ~dkey "recreating alloced_return_%s" kf_name;
        let base = Library_functions.create_alloced_return vi.vtype kf in
        Base.to_varinfo base
      with Not_found ->
        Value_parameters.abort "alloced_return should have function: `%s'" kf_name
    end else
    if importing_value then begin
      (* Variable may be an escaping local value *)
      Value_parameters.warning "variable `%a' is not global, \
                                possibly an escaping value; ignoring"
        Printer.pp_varinfo vi;
      raise Possibly_escaping_value
    end else
      Value_parameters.abort "global not found: `%a'"
        Printer.pp_varinfo vi

let import_validity = function
  | Base.Empty | Base.Known _ | Base.Unknown _ | Base.Invalid as v -> v
  | Base.Variable { Base.weak; min_alloc; max_alloc; max_allocable } ->
    let var = Base.create_variable_validity ~weak ~min_alloc ~max_alloc in
    if Integer.equal max_allocable var.Base.max_allocable then
      Base.Variable var
    else Kernel.abort "Incompatible maximum size for variable %a vs. %a"
        Abstract_interp.Int.pretty max_allocable
        Abstract_interp.Int.pretty var.Base.max_allocable

let import_base (base : Base.t) ~importing_value =
  let make_base = function
    | Base.Var (vi, _validity) ->
      Base.of_varinfo (import_varinfo vi ~importing_value)
    | Base.CLogic_Var (lv, _ty, _validity) ->
      (* Value states do not contain logic variables anyway
         (except when evaluating ACSL clauses, which is not the case here *)
      Value_parameters.fatal "importing logic variables (%a) is unsupported"
        Printer.pp_logic_var lv
    | Base.Null -> Base.null
    | Base.String (_, s) ->
      (* TODO: currently, we recreate a new string unrelated to the original
         one. This is probably not the good solution *)
      let c = match s with
        | Base.CSString s -> Const (CStr s)
        | Base.CSWstring s -> Const (CWStr s)
      in
      let e = Cil.new_exp Cil_datatype.Location.unknown c in
      Base.of_string_exp e
    | Base.Allocated (vi, deallocation, validity) ->
      Value_parameters.feedback ~dkey "recreating allocated base for alloc: `%a'"
        Printer.pp_varinfo vi;
      let new_vi = Value_util.create_new_var vi.vname vi.vtype in
      let validity = import_validity validity in
      let new_base = Base.register_allocated_var new_vi deallocation validity in
      Builtins_malloc.register_malloced_base new_base;
      new_base
  in
  let id = Base.id base in
  try
    let res = Hashtbl.find base_cache id in
    res
  with Not_found ->
    let base' = make_base base in
    Hashtbl.replace base_cache id base';
    base'

let import_base_setlattice (sl : Base.SetLattice.t) ~importing_value =
  Base.SetLattice.fold (fun base acc ->
      let sl' = Base.SetLattice.inject_singleton (import_base base ~importing_value) in
      Base.SetLattice.join acc sl'
    ) sl Base.SetLattice.empty

let import_ival (ival : Ival.t) =
  match ival with
  | Ival.Set a ->
    Array.fold_left
      (fun acc i -> Ival.join (Ival.inject_singleton i) acc)
      Ival.bottom a
  | Ival.Float _ ->
    let mn, mx = Ival.min_and_max_float ival in
    Ival.inject_float_interval (Fval.F.to_float mn) (Fval.F.to_float mx)
  | Ival.Top (mn,mx,m,u) -> Ival.inject_interval mn mx m u

let import_map (m : Cvalue.V.M.t) =
  let add base ival m =
    let new_base = import_base base ~importing_value:true in
    let new_ival = import_ival ival in
    Cvalue.V.add new_base new_ival m
  in
  Cvalue.V.M.fold add m Cvalue.V.bottom

let import_v (v : Cvalue.V.t) =
  match v with
  | Cvalue.V.Top (sl, o) ->
    Value_parameters.warning ~once:true
      "importing garbled mix, locations may have changed";
    (*let o' = import_origin o in*)
    let sl' = import_base_setlattice sl ~importing_value:true in
    Cvalue.V.Top (sl', o)
  | Cvalue.V.Map m ->
    import_map m

let import_v_or_uninit (vu : Cvalue.V_Or_Uninitialized.t) =
  let find v =
    try
      let res = V.Hashtbl.find v_cache v in
      res
    with Not_found ->
      let v' = import_v v
      in
      V.Hashtbl.replace v_cache v v';
      v'
  in
  try
    V_Or_Uninitialized.map find vu
  with Possibly_escaping_value ->
    (* replace variable with ESCAPINGADDR *)
    Cvalue.V_Or_Uninitialized.C_init_esc V.bottom

let import_offsetmap (offsetmap : V_Offsetmap.t) =
  V_Offsetmap.map_on_values import_v_or_uninit offsetmap

let import_model (state : Model.t) =
  match state with
  | Model.Bottom -> Model.bottom
  | Model.Top -> Model.top
  | Model.Map map ->
    let add base offsetmap map =
      let new_offsetmap = import_offsetmap offsetmap in
      let new_base = import_base base ~importing_value:false in
      Model.add_base new_base new_offsetmap map
    in
    Model.fold add map Model.empty_map

(*and import_origin (o : Origin.t) =
  (* the "new" origin location is arbitrary, since no guarantees about the
     actual location can be given *)
  let loc = Origin.LocationSetLattice.currentloc_singleton () in
  match o with
  | Origin.Misalign_read _ -> Origin.Misalign_read loc
  | Origin.Leaf _ -> Origin.Leaf loc
  | Origin.Merge _ -> Origin.Merge loc
  | Origin.Arith _ -> Origin.Arith loc
  | Origin.Well | Origin.Unknown -> o*)

let load_globals_from_file filename : Model.t =
  let ic = open_in_bin filename in
  let (state : Model.t) = Marshal.from_channel ic in
  close_in ic;
  Value_parameters.feedback ~dkey "DE-MARSHALLED STATE (before import):@.%a"
    Cvalue.Model.pretty state;
  import_model state

let save_globals_to_file kf state_with_locals filename =
  Value_parameters.feedback "Saving globals state after call to function: %a"
    Kernel_function.pretty kf;
  let state = Model.filter_base Base.is_global state_with_locals in
  Value_parameters.feedback ~dkey "SAVED STATE:@.%a" Model.pretty state;
  let oc = open_out_bin filename in
  Marshal.to_channel oc state [];
  close_out oc

let load_and_merge_function_state state : Model.t =
  let (kf, filename) = Value_parameters.get_LoadFunctionState () in
  Value_parameters.feedback
    "Skipping call to %a,@ loading globals state from file:@ %s"
    Kernel_function.pretty kf filename;
  let saved_state = load_globals_from_file filename in
  Value_parameters.debug ~dkey "LOADED STATE:@.%a"
    Cvalue.Model.pretty saved_state;
  (* warn about missing globals in the new AST, and add new globals that were
     not present before *)
  let saved_map = match saved_state with
    | Model.Map m -> m
    | _ -> assert false
  in
  let locals =
    Model.filter_base (fun base -> not (Base.is_global base)) state
  in
  let state_without_locals =
    Model.filter_base (fun base -> Base.is_global base) state
  in
  Value_parameters.debug ~dkey "Merging state with locals: %a@."
    Model.pretty locals;
  let new_globals =
    Model.filter_base
      (fun base ->
         try
           let _ = Model.find_base base saved_state in
           false (* previously existing global *)
         with
         | Not_found ->
           Value_parameters.warning "found new global variable `%a'"
             Base.pretty base;
           true (* new global *)
      ) state_without_locals
  in
  let merged_globals_state =
    Model.fold (fun new_base offsm acc ->
        Model.add_base new_base offsm acc
      ) saved_map new_globals
  in
  let map_with_globals = match merged_globals_state with
    | Model.Map m -> m
    | _ -> Value_parameters.fatal "invalid saved state: %a"
             Model.pretty saved_state
  in
  let merged_globals_and_locals =
    Model.fold (fun new_base offsm acc ->
        Model.add_base new_base offsm acc
      ) map_with_globals locals
  in
  merged_globals_and_locals

let save_globals_state () : unit =
  let (kf, filename) = Value_parameters.get_SaveFunctionState () in
  let ret_stmt = Kernel_function.find_return kf in
  try
    let ret_state = Db.Value.get_stmt_state ret_stmt in
    match ret_state with
    | Model.Top ->
      Value_parameters.abort "cannot save state at return statement of %a \
                              (too imprecise)" Kernel_function.pretty kf
    | Model.Bottom ->
      Value_parameters.abort "cannot save state at return statement of %a \
                              (bottom)" Kernel_function.pretty kf
    | Model.Map _ -> save_globals_to_file kf ret_state filename
  with Not_found ->
    if Value_parameters.LoadFunctionState.is_set () then
      let (load_kf, _) = Value_parameters.get_LoadFunctionState () in
      Value_parameters.abort "could not find saved state for function `%a';@ \
                              this can happen if it is called from `%a'"
        Kernel_function.pretty kf Kernel_function.pretty load_kf;
    else
      Value_parameters.failure "could not find saved state for function `%a'"
        Kernel_function.pretty kf


exception Warn_local_addresses
(* visitor used by frama_c_load_state *)
class locals_visitor = object(_self) inherit Visitor.frama_c_inplace
  method! vlval (lhost, _) =
    match lhost with
    | Var vi ->
      if not vi.vglob then raise Warn_local_addresses;
      Cil.DoChildren
    | Mem _ -> Cil.DoChildren
end


(* Builtin to load a saved analysis state *)
let frama_c_load_state state actuals =
  if Value_parameters.ValShowProgress.get () then
    Value_parameters.feedback "Call to builtin Frama_C_load_state(%a)%t"
      Value_util.pretty_actuals actuals Value_util.pp_callstack;
  (* Warn if arguments contain pointers to local variables,
     in which case the loaded state may be unsound. *)
  begin
    try
      List.iter (fun (exp_arg, arg, _) ->
          let vis = new locals_visitor in
          if Cil.isPointerType (Cil.typeOf exp_arg) then
            ignore (Visitor.visitFramacExpr vis exp_arg);
          if Cvalue.V.contains_addresses_of_any_locals arg then
            raise Warn_local_addresses
        ) actuals;
    with Warn_local_addresses ->
      Value_parameters.warning ~current:true ~once:true
        "arguments to loaded function state contain local addresses,@ \
         possible unsoundness";
  end;
  let merged_loaded_state = load_and_merge_function_state state in
  {
    Value_types.c_values = [None, merged_loaded_state];
    c_clobbered = Base.SetLattice.empty;
    c_cacheable = Value_types.NoCacheCallers;
    c_from = None
  }

let () = Builtins.register_builtin "Frama_C_load_state" frama_c_load_state
