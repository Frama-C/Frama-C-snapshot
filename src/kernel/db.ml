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

(* $Id: db.ml,v 1.507 2008/11/20 13:13:10 uid530 Exp $ *)

open Format
open Cil_types
open Cilutil
open Cil
open Cilutil
open Db_types
open Extlib

type 'a p_ref = (*private*) 'a ref

let register ~journalize r f =
  match journalize with
  | None -> r := f
  | Some(name, ty) -> r := Journal.register ("!Db."^name) ty f

let register_compute name deps r f =
  let name = "!Db." ^ name in
  let f = Journal.register
    name
    (Type.func Type.unit Type.unit)
    f
  in
  let compute, _self = Computation.apply_once name deps f in
  r := compute

let register_guarded_compute name is_computed r f =
  let name = "!Db." ^ name in
  let f = Journal.register name
    (Type.func Type.unit Type.unit)
    f
  in
  let compute () = if not (is_computed ()) then f () in
  r := compute

module Main = Hook.Build(struct type t = Format.formatter end)

(* ************************************************************************* *)
(** {2 Inouts} *)
(* ************************************************************************* *)

module type INOUT = sig

  type t

  val self_internal: Project.Computation.t ref
  val self_external: Project.Computation.t ref

  val compute : (kernel_function -> unit) ref

  val get_internal : (kernel_function -> t) ref
    (** Inputs/Outputs with local variables *)

  val get_external : (kernel_function -> t) ref
    (** Inputs/Outputs without local variables *)

  val statement : (stmt -> t) ref
  val expr : (stmt -> exp -> t) ref

  val kinstr : kinstr -> t option
    (** Effects of the given statement or of the englobing statement *)

  val display : (Format.formatter -> kernel_function -> unit) ref
  val pretty : Format.formatter -> t -> unit

end

(** Computation of outputs
    - over-approximation of zones written by each function. *)
module Outputs = struct
  type t = Locations.Zone.t
  let self_internal = ref Project.Computation.dummy
  let self_external = ref Project.Computation.dummy
  let compute = mk_fun "Out.compute"
  let display = mk_fun "Out.display"
  let get_internal = mk_fun "Out.get_internal"
  let get_external = mk_fun "Out.get_external"
  let statement = mk_fun "Out.statement"
  let expr = mk_fun "Out.expr"
  let kinstr ki = match ki with
    | Kstmt s -> Some (!statement s)
    | Kglobal -> None

  let pretty = Locations.Zone.pretty
end

(** Computation of read inputs
    - over-approximation of locations read by each function. *)
module Inputs = struct
  (* TODO: use the result of [Db.From] to have best approximations on
     [Inputs.get_internal] and [Inputs.get_external]. *)
  (*       What about [Inputs.statement] ? *)
  type t = Locations.Zone.t
  let self_internal = ref Project.Computation.dummy
  let self_external = ref Project.Computation.dummy
  let self_with_formals = ref Project.Computation.dummy
  let compute = mk_fun "Inputs.compute"
  let display = mk_fun "Inputs.display"
  let display_with_formals = mk_fun "Inputs.display_with_formals"
  let get_internal = mk_fun "Inputs.get_internal"
  let get_external = mk_fun "Inputs.get_external"
  let get_with_formals = mk_fun "Inputs.get_with_formals"
  let statement = mk_fun "Inputs.statement"
  let expr = mk_fun "Inputs.expr"
  let kinstr ki = match ki with
    | Kstmt s -> Some (!statement s)
    | Kglobal -> None

  let pretty = Locations.Zone.pretty
end

(** Computation of operational inputs
    - over-approximation of zones whose input values are read by each function,
    Computation of sure outputs
    - under-approximation of zones written by each function. *)
module InOutContext = struct
  type t = Inout_type.t
  let self_internal = ref Project.Computation.dummy
  let self_external = ref Project.Computation.dummy
  let compute = mk_fun "InOutContext.compute"
  let display = mk_fun "InOutContext.display"
  let get_internal = mk_fun "InOutContext.get_internal"
  let get_external = mk_fun "InOutContext.get_external"
  let statement = mk_fun "InOutContext.statement"
  let expr = mk_fun "InOutContext.expr"
  let kinstr ki = match ki with
    | Kstmt s -> Some (!statement s)
    | Kglobal -> None

  type t' = Locations.Zone.t

  let pretty fmt x =
    Format.fprintf fmt "@[Over-approximated operational inputs: %a@]@."
      Locations.Zone.pretty (x.Inout_type.over_inputs);
    Format.fprintf fmt "@[Over-approximated operational inputs when termination: %a@]@."
      Locations.Zone.pretty (x.Inout_type.over_inputs_if_termination);
    Format.fprintf fmt "@[Under-approximated operational outputs when termination: %a@]"
      Locations.Zone.pretty (x.Inout_type.under_outputs_if_termination)

end

(** Derefs computations *)
module Derefs = struct
  type t = Locations.Zone.t
  let self_internal = ref Project.Computation.dummy
  let self_external = ref Project.Computation.dummy
  let compute = mk_fun "Derefs.compute"
  let display = mk_fun "Derefs.display"
  let get_internal = mk_fun "Derefs.get_internal"
  let get_external = mk_fun "Derefs.get_external"
  let statement = mk_fun "Derefs.statement"
  let expr = mk_fun "Derefs.expr"
  let kinstr ki = match ki with
    | Kstmt s -> Some (!statement s)
    | Kglobal -> None

  let pretty = Locations.Zone.pretty
end


(* ************************************************************************* *)
(** {2 Values} *)
(* ************************************************************************* *)

module Value = struct
  type state = Relations_type.Model.t
  type t = Cvalue_type.V.t

  let dependencies =
    [ Cil_state.self;
      Cmdline.MainFunction.self; Cmdline.LibEntry.self;
      Cmdline.MemFunctions.self; Cmdline.MemExecAll.self;
      Cmdline.PropagateTop.self;
      Cmdline.ArrayPrecisionLevel.self;
      Cmdline.SemanticUnrollingLevel.self;
      Cmdline.WideningLevel.self;
      Cmdline.MinValidAbsoluteAddress.self;
      Cmdline.MaxValidAbsoluteAddress.self;
      Cmdline.AutomaticContextMaxDepth.self;
      Cmdline.AutomaticContextMaxWidth.self;
      Cmdline.AllocatedContextValid.self;
      Cmdline.IgnoreOverflow.self;
      Cmdline.UnsafeArrays.self;
      Cmdline.KeepOnlyLastRun.self;
      Cmdline.UseRelations.self;
      Cmdline.WidenVariables.self;
    ]

  let size = 1789
  module Table =
    Cil_computation.InstrHashtbl
      (Relations_type.Model.Datatype)
      (struct
	 let name = "value_table"
	 let size = size
	 let dependencies = dependencies
       end)

  let self = Table.self
  let mark_as_computed () = Table.mark_as_computed ()
  let is_computed () = Table.is_computed ()

  module Called_Functions =
    Cil_computation.VarinfoHashtbl
      (Relations_type.Model.Datatype)
      (struct
	 let name = "called_functions"
	 let size = 9
	 let dependencies = [self]
       end)

  module VGlobals =
    Computation.OptionRef
      (Relations_type.Model.Datatype)
      (struct
	 let name = "vglobals"
	 let dependencies = [self]
       end)

  let pretty_table () =
   Table.iter
      (fun k v ->
         Format.printf "GLOBAL TABLE at %a: %a@\n"
           Instr.pretty k Relations_type.Model.pretty v)

  let pretty_table_raw () =
    InstrHashtbl.iter
      (fun k v ->
         Format.printf "GLOBAL TABLE at %a: %a@\n"
           Instr.pretty k Relations_type.Model.pretty v)

  module Record_Value_Callbacks =
    Hook.Build
      (struct
	 type t = (kernel_function * kinstr) list * state InstrHashtbl.t
       end)

  module Call_Value_Callbacks =
    Hook.Build
      (struct type t = state * (Db_types.kernel_function * kinstr) list end)

  let update_table k v =
(*    record_value_callbacks k v; : pascal : avant les hooks prenaient un kinstr
et un etat, maintenant ils prennent le callstack et toute la table d'un coup.
Du coup ceci ne peut plus etre fait ici *)
    let change old =
      if Cmdline.KeepOnlyLastRun.get () then v
      else Relations_type.Model.join old v
    in
    ignore (Table.memo ~change (fun _ -> v) k)

  let map2_while_possible f l1 l2 =
    let rec go l1 l2 acc =
      match l1,l2 with
      | [],_ | _, [] -> acc
      | x1::r1, x2::r2 ->
          go r1 r2 ((f x1 x2)::acc)
    in
    List.rev (go l1 l2 [])

  let merge_initial_state kf state =
    let vi = Kernel_function.get_vi kf in
    let change = Relations_type.Model.join state in
    ignore (Called_Functions.memo ~change (fun _ -> state) vi)

  let get_initial_state kf =
    try
      Called_Functions.find (Kernel_function.get_vi kf)
    with Not_found ->
      Relations_type.Model.bottom

  let valid_behaviors = mk_fun "Value.get_valid_behaviors"

  let get_state k =
    assert (is_computed ()); (* this assertion fails during value analysis *)
    try Table.find k with Not_found -> Relations_type.Model.bottom

  let noassert_get_state k =
    try Table.find k with Not_found -> Relations_type.Model.bottom

  let is_accessible stmt =
    let st = get_state stmt in
    Relations_type.Model.is_reachable st

  let is_reachable = Relations_type.Model.is_reachable

  let is_reachable_stmt stmt = is_reachable (get_state (Kstmt stmt))

  let is_called = mk_fun "Value.is_called"
  let callers = mk_fun "Value.callers"
  let never_terminates = mk_fun "Value.never_terminates"

  let initial_state_only_globals = mk_fun "Value.initial_state_only_globals"

  let globals_state () = VGlobals.memo !initial_state_only_globals

  let access_location = mk_fun "Value.access_location"

  let find = Relations_type.Model.find ~with_alarms:CilE.warn_none_mode

  let access =  mk_fun "Value.access"
  let access_expr =  mk_fun "Value.access_expr"
  let access_after = mk_fun "Value.access_after"
  let lval_to_offsetmap_after = mk_fun "Value.lval_to_offsetmap_after"
  let access_location_after = mk_fun "Value.access_location_after"
  let update = mk_fun "Value.update"

  let eval_lval =
    ref (fun ~with_alarms:_ _ -> not_yet_implemented "Value.eval_lval")
  let eval_expr =
    ref (fun ~with_alarms:_ _ -> not_yet_implemented "Value.eval_expr")

  let pretty_filter =  mk_fun "Value.pretty_filter"
  let pretty_state = Relations_type.Model.pretty
  let pretty_state_without_null = Relations_type.Model.pretty_without_null

  let pretty = Cvalue_type.V.pretty

  let display fmt kf =
    let values = get_state (Kstmt (Kernel_function.find_return kf)) in
    try
      let fst_values = get_state (Kstmt (Kernel_function.find_first_stmt kf)) in
      if Relations_type.Model.is_reachable fst_values then begin
	Format.fprintf fmt "@[<hov 2>Values for function %s:@\n"
          (Kernel_function.get_name kf);
        let try_to_filter =
          not (Cmdline.MemExecAll.get ()) &&
	    (Cilutil.StringSet.is_empty (Cmdline.MemFunctions.get ()))
        in
	if try_to_filter then
	  let outs = !Outputs.get_internal kf in
          Relations_type.Model.pretty_filter fmt values outs;
	else
	  Relations_type.Model.pretty fmt values;
	(*          (match kf.internal_out with
		    | Some _ when try_to_filter ->
		    let outs = !Outputs.get_internal kf in
		    Relations_type.Model.pretty_filter fmt values outs
		    | _ -> if try_to_filter then
		    warn "whacky situation: displaying without filtering. You may have interrupted the computations.";
		    Relations_type.Model.pretty fmt values);*)
        Format.fprintf fmt "@]@\n"
      end
    with Kernel_function.No_Statement -> ()

  let display_globals fmt () =
    let values = globals_state () in
    if Relations_type.Model.is_reachable values
    then begin
      Format.fprintf fmt "@[<hov 0>Values of globals at initialization @\n";
      Relations_type.Model.pretty_without_null fmt values;
      Format.fprintf fmt "@]@\n"
    end

  let compute = mk_fun "Value.compute"

  let memoize = mk_fun "Value.memoize"
  let expr_to_kernel_function = mk_fun "Value.expr_to_kernel_function"
  let expr_to_kernel_function_state =
    mk_fun "Value.expr_to_kernel_function_state"

  exception Not_a_call

  let call_to_kernel_function call_stmt = match call_stmt.skind with
    | Instr (Call (_, fexp, _, _)) ->
        let _, called_functions = !expr_to_kernel_function
                                    ~with_alarms:CilE.warn_none_mode ~deps:None
                                    (Kstmt call_stmt) fexp
        in called_functions
    | _ -> raise Not_a_call


  let lval_to_loc_with_deps = mk_fun "Value.lval_to_loc_with_deps"
  let lval_to_loc_with_deps_state = mk_fun "Value.lval_to_loc_with_deps_state"
  let lval_to_loc = mk_fun "Value.lval_to_loc"
  let lval_to_offsetmap = mk_fun "Value.lval_to_offsetmap"
  let lval_to_loc_state = mk_fun "Value.lval_to_loc_state"
  let lval_to_zone = mk_fun "Value.lval_to_zone"
  let lval_to_zone_state = mk_fun "Value.lval_to_zone_state"
  let assigns_to_zone_inputs_state = mk_fun "Value.assigns_to_zone_inputs_state"

  exception Void_Function

  let find_return_loc kf =
    let ki = Kernel_function.find_return kf in
    let lval = match ki with
      | { skind = Return (Some (Lval ((_ , offset) as lval)), _) } ->
	  assert (offset = NoOffset) ;
	  lval
      | { skind = Return (None, _) } -> raise Void_Function
      | _ -> assert false
    in !lval_to_loc (Kstmt ki) ~with_alarms:CilE.warn_none_mode lval

  exception Aborted

  let degeneration_occurred =
    ref (fun _kf _lv ->
       if not (Cmdline.PropagateTop.get ()) then raise Aborted)

end

module From = struct
  let access = mk_fun "From.access"
  let update = mk_fun "From.update"
  let find_deps_no_transitivity = mk_fun "From.find_deps_no_transitivity"
  let compute = mk_fun "From.compute"
  let compute_all = mk_fun "From.compute_all"
  let compute_all_calldeps = mk_fun "From.compute_all_calldeps"
  let is_computed = mk_fun "From.is_computed"
  let pretty = mk_fun "From.pretty"
  let get = mk_fun "From.get"
  let self = ref Project.Computation.dummy

  let display = mk_fun "From.display"

  module Callwise = struct
    let iter = mk_fun "From.Callwise.iter"
    let find = mk_fun "From.Callwise.find"
  end
end

module Memzone = struct
  type t = Memzone_type.Lmap_bitwise_with_empty_default.t
  let compute = mk_fun "Memzone.compute"
  let pretty = mk_fun "Memzone.pretty"
end

module Access_path = struct
  type t = (Locations.Zone.t * Locations.Location_Bits.t) BaseUtils.BaseMap.t
  let compute = mk_fun "Access_path.compute"
  let filter = mk_fun "Access_path.filter"
  let pretty = mk_fun "Access_path.pretty"
end

module Users = struct
  let get = mk_fun "Users.get"
end

(* ************************************************************************* *)
(** {2 PDG} *)
(* ************************************************************************* *)

module Pdg = struct
  type t = PdgTypes.Pdg.t
  type t_node = PdgTypes.Node.t
  type t_node_key = PdgIndex.Key.t
  type t_nodes_and_undef =
            (t_node * Locations.Zone.t option) list * Locations.Zone.t option

  exception Top = PdgTypes.Pdg.Top
  exception Bottom = PdgTypes.Pdg.Bottom
  exception NotFound = PdgIndex.NotFound

  let self = ref Project.Computation.dummy

  let get = mk_fun "Pdg.get"

  let from_same_fun pdg1 pdg2 =
    let kf1 =  PdgTypes.Pdg.get_kf pdg1 in
    let kf2 =  PdgTypes.Pdg.get_kf pdg2 in
      Kernel_function.equal kf1 kf2

  let node_key = mk_fun "Pdg.node_key"

  let synchronize_annotations = mk_fun "Pdg.synchronize_annotations"

  let find_decl_var_node = mk_fun "Pdg.find_decl_var_node"
  let find_input_node = mk_fun "Pdg.find_input_nodes"
  let find_ret_output_node = mk_fun "Pdg.find_ret_output_node"
  let find_output_nodes = mk_fun "Pdg.find_output_nodes"
  let find_all_outputs_nodes = mk_fun "Pdg.find_all_outputs_nodes"
  let find_all_inputs_nodes = mk_fun "Pdg.find_all_inputs_nodes"
  let find_stmt_and_blocks_nodes = mk_fun "Pdg.find_stmt_and_blocks_nodes"
  let find_simple_stmt_nodes = mk_fun "Pdg.find_simplestmt_nodes"
  let find_stmt_node = mk_fun "Pdg.find_stmt_node"
  let find_entry_point_node = mk_fun "Pdg.find_entry_point_node"
  let find_top_input_node = mk_fun "Pdg.find_top_input_node"
  let find_call_topin_node = mk_fun "Pdg.find_call_topin_node"
  let find_call_ctrl_node = mk_fun "Pdg.find_call_ctrl_node"
  let find_stmt_id_nodes = mk_fun "Pdg.find_stmt_id_nodes"
  let find_location_nodes_at_stmt = mk_fun "Pdg.find_location_nodes_at_stmt"
  let find_location_nodes_at_end = mk_fun "Pdg.find_location_nodes_at_end"
  let find_location_nodes_at_begin = mk_fun "Pdg.find_location_nodes_at_begin"
  let find_call_input_node = mk_fun "Pdg.find_call_input_node"
  let find_call_output_node = mk_fun "Pdg.find_call_output_node"
  let find_code_annot_nodes = mk_fun "Pdg.find_code_annot_nodes"
  let find_fun_precond_nodes = mk_fun "Pdg.find_fun_precond_nodes"
  let find_fun_postcond_nodes = mk_fun "Pdg.find_fun_postcond_nodes"
  let find_fun_variant_nodes = mk_fun "Pdg.find_fun_variant_nodes"

  let find_call_out_nodes_to_select = mk_fun "Pdg.find_call_out_nodes_to_select"
  let find_in_nodes_to_select_for_this_call =
    mk_fun "Pdg.find_in_nodes_to_select_for_this_call"

  let direct_dpds = mk_fun "Pdg.direct_dpds"
  let direct_ctrl_dpds = mk_fun "Pdg.direct_ctrl_dpds"
  let direct_data_dpds = mk_fun "Pdg.direct_data_dpds"
  let direct_addr_dpds = mk_fun "Pdg.direct_addr_dpds"

  let all_dpds = mk_fun "Pdg.all_dpds"
  let all_ctrl_dpds = mk_fun "Pdg.all_ctrl_dpds"
  let all_data_dpds = mk_fun "Pdg.all_data_dpds"
  let all_addr_dpds = mk_fun "Pdg.all_addr_dpds"

  let direct_uses = mk_fun "Pdg.direct_uses"
  let direct_ctrl_uses = mk_fun "Pdg.direct_ctrl_uses"
  let direct_data_uses = mk_fun "Pdg.direct_data_uses"
  let direct_addr_uses = mk_fun "Pdg.direct_addr_uses"

  let all_uses = mk_fun "Pdg.all_uses"

  let all_related_nodes = mk_fun "Pdg.all_related_nodes"
  let custom_related_nodes = mk_fun "Pdg.custom_related_nodes"

  let find_call_stmts = mk_fun "Pdg.find_call_stmts"

  let iter_nodes = mk_fun "Pdg.iter_nodes"

  let extract = mk_fun "Pdg.extract"
  let pretty = ref (fun ?(bw:_) _ _ ->
		      ignore(bw);
		      not_yet_implemented "Pdg.pretty")
  let pretty_node = mk_fun "Pdg.pretty_node"
  let pretty_key = mk_fun "Pdg.pretty_key"

  module F_FctMarks = PdgMarks.F_Fct
  (* module F_ProjMarks = PdgMarks.F_Proj *)

  type 't_mark t_info_caller_inputs = 't_mark PdgMarks.t_info_caller_inputs
  type 't_mark t_info_inter = 't_mark PdgMarks.t_info_inter

end

(* ************************************************************************* *)
(** {2 Scope} *)
(* ************************************************************************* *)

(** Interface for the Scope plugin *)
module Scope = struct
  let get_data_scope_at_stmt = mk_fun "Datascope.get_data_scope_at_stmt"
  let get_prop_scope_at_stmt = mk_fun "Datascope.get_prop_scope_at_stmt"
  let check_asserts = mk_fun "Datascope.check_asserts"
  let get_defs = mk_fun "Datascope.get_defs"

  type t_zones = Locations.Zone.t Inthash.t
  let build_zones = mk_fun "Pdg.build_zones"
  let pretty_zones = mk_fun "Pdg.pretty_zones"
  let get_zones = mk_fun "Pdg.get_zones"

end

(* ************************************************************************* *)
(** {2 Spare Code} *)
(* ************************************************************************* *)

(** Detection of the unused code of an application. *)
module Sparecode = struct
  let get =
    ref (fun ~select_annot:_  -> not_yet_implemented "Sparecode.run")
  let rm_unused_globals = 
    ref (fun ?project:_ -> not_yet_implemented "Sparecode.rm_unused_glob")
end

(* ************************************************************************* *)
(** {2 Slicing} *)
(* ************************************************************************* *)

(** Interface for the slicing tool. *)
module Slicing = struct

  exception No_Project
  exception Unable_to_process_annotations

  let self = ref Project.Computation.dummy

  (* TODO: merge with frama-c projects (?) *)
  module Project = struct
    type t = SlicingTypes.sl_project

    let extract = mk_fun "Slicing.Project.extract"
    let pretty = mk_fun "Slicing.Project.pretty"
    let print_dot =
      ref (fun ~filename:_ ~title:_ _ ->
	     not_yet_implemented "Slicing.Project.print_dot")

    let get_all = mk_fun "Slicing.Project.get_all"
    let get_project = mk_fun "Slicing.Project.get_project"
    let set_project = mk_fun "Slicing.Project.set_project"
    let mk_project = mk_fun "Slicing.Project.mk_project"
    let get_name = mk_fun "Slicing.Project.get_name"

    let is_directly_called_internal =
      mk_fun "Slicing.Project.is_directly_called_internal"
    let is_called = mk_fun "Slicing.Project.is_called"
    let has_persistent_selection =
      mk_fun "Slicing.Project.has_persistent_selection"
    let change_slicing_level =
      mk_fun "Slicing.Project.change_slicing_level"
  end

  module Mark = struct
    type t = SlicingTypes.sl_mark
    let compare = mk_fun "Slicing.Mark.compare"
    let pretty = mk_fun "Slicing.Mark.pretty"
    let make =
      ref (fun ~data:_ ~addr:_ ~ctrl:_ -> not_yet_implemented "Slicing.Mark.make")
    let is_bottom = mk_fun "Slicing.Mark.is_bottom"
    let is_spare = mk_fun "Slicing.Mark.is_spare"
    let is_ctrl = mk_fun "Slicing.Mark.is_ctrl"
    let is_data = mk_fun "Slicing.Mark.is_data"
    let is_addr = mk_fun "Slicing.Mark.is_addr"
    let get_from_src_func  = mk_fun "Slicing.Mark.get_from_src_func"
  end

  module Select = struct
    type t = SlicingTypes.sl_select
    type t_set = SlicingTypes.sl_selects

    let get_function = mk_fun "Slicing.Select.get_function"
    let select_stmt = mk_fun "Slicing.Select.select_stmt"
    let select_stmt_ctrl = mk_fun "Slicing.Select.select_stmt_ctrl"
    let select_stmt_lval_rw = mk_fun "Slicing.Select.select_stmt_lval_rw"
    let select_stmt_lval = mk_fun "Slicing.Select.select_stmt_lval"
    let select_stmt_zone = mk_fun "Slicing.Select.select_stmt_zone"
    let select_stmt_annots = mk_fun "Slicing.Select.select_stmt_annots"
    let select_stmt_annot = mk_fun "Slicing.Select.select_stmt_annot"
    let select_stmt_pred = mk_fun "Slicing.Select.select_stmt_pred"
    let select_stmt_term = mk_fun "Slicing.Select.select_stmt_term"
    let select_func_return = mk_fun "Slicing.Select.select_func_return"
    let select_func_calls_to = mk_fun "Slicing.Select.select_func_calls_to"
    let select_func_calls_into = mk_fun "Slicing.Select.select_func_calls_into"
    let select_func_lval_rw = mk_fun "Slicing.Select.select_func_lval_rw"
    let select_func_lval = mk_fun "Slicing.Select.select_func_lval"
    let select_func_zone = mk_fun "Slicing.Select.select_func_zone"
    let select_func_annots = mk_fun "Slicing.Select.select_func_annots"
    let select_stmt_internal = mk_fun "Slicing.Select.select_stmt_internal"
    let empty_selects = mk_fun "Slicing.Select.empty_selects"
    let add_to_selects_internal =
      mk_fun "Slicing.Select.add_to_selects_internal"
    let iter_selects_internal =
      mk_fun "Slicing.Select.iter_selects_internal"
        (* didn't manage to put this polymorphic function as a ref... *)
    let fold_selects f acc selections =
      let r = ref acc in
      let dof select = r := f !r select in
        !iter_selects_internal dof selections; !r
    let merge_internal =
      mk_fun "Slicing.Select.merge_internal"
    let select_min_call_internal =
      mk_fun "Slicing.Select.select_min_call_internal"
    let select_stmt_ctrl_internal =
      mk_fun "Slicing.Select.select_control_stmt_ctrl"
    let select_pdg_nodes =
      mk_fun "Slicing.Select.select_pdg_nodes"
    let select_entry_point_internal =
      mk_fun "Slicing.Select.select_entry_point_internal"
    let select_return_internal =
      mk_fun "Slicing.Select.select_return_node_internal"
    let select_pdg_nodes_internal =
      mk_fun "Slicing.Select.select_pdg_nodes_internal"
    let select_stmt_zone_internal =
      mk_fun "Slicing.Select.select_stmt_zone_internal"
    let select_zone_at_entry_point_internal =
      mk_fun "Slicing.Select.select_zone_at_entry_point_internal"
    let select_modified_output_zone_internal =
      mk_fun "Slicing.Select.select_modified_output_zone_internal"
    let select_zone_at_end_internal =
      mk_fun "Slicing.Select.select_zone_at_end_internal"
    let select_func_zone_internal =
      mk_fun "Slicing.Select.select_func_zone_internal"
    let pretty = mk_fun "Slicing.Select.pretty"
  end

  module Slice = struct
    type t = SlicingTypes.sl_fct_slice
    let create = mk_fun "Slicing.Slice.create"
    let remove = mk_fun "Slicing.Slice.remove"
    let remove_uncalled = mk_fun "Slicing.Slice.remove_uncalled"
    let get_all = mk_fun "Slicing.Slice.get_all"
    let get_callers = mk_fun "Slicing.Slice.get_callers"
    let get_called_slice = mk_fun "Slicing.Slice.get_called_slice"
    let get_called_funcs = mk_fun "Slicing.Slice.get_called_funcs"
    let get_function = mk_fun "Slicing.Slice.get_function"
    let pretty = mk_fun "Slicing.Slice.pretty"
    let get_mark_from_stmt = mk_fun "Slicing.Slice.get_mark_from_stmt"
    let get_mark_from_local_var =
      mk_fun "Slicing.Slice.get_mark_from_local_var"
    let get_mark_from_formal = mk_fun "Slicing.Slice.get_mark_from_formal"
    let get_mark_from_label = mk_fun "Slicing.Slice.get_from_label"
    let get_user_mark_from_inputs =
      mk_fun "Slicing.Slice.get_user_mark_from_inputs"
  end

  module Request = struct
    let add_selection = mk_fun "Slicing.Request.add_selection"
    let add_persistent_selection = mk_fun "Slicing.Request.add_persistent_selection"
    let add_persistent_cmdline = mk_fun "Slicing.Request.add_persistent_cmdline"
    let is_already_selected_internal =
      mk_fun "Slicing.Request.is_already_selected_internal"
    let add_slice_selection_internal =
      mk_fun "Slicing.Request.add_slice_selection_internal"
    let add_selection_internal =
      mk_fun "Slicing.Request.add_selection_internal"
    let add_call_slice = mk_fun "Slicing.Request.add_call_slice"
    let add_call_fun = mk_fun "Slicing.Request.add_call_fun"
    let add_call_min_fun = mk_fun "Slicing.Request.add_call_min_fun"
    let merge_slices = mk_fun "Slicing.Request.merge_slices"
    let copy_slice = mk_fun "Slicing.Request.copy_slice"
    let split_slice = mk_fun "Slicing.Request.split_slice"
    let propagate_user_marks = mk_fun "Slicing.Request.propagate_user_marks"
    let apply_all = mk_fun "Slicing.Request.apply_all"
    let apply_all_internal = mk_fun "Slicing.Request.apply_all_internal"
    let apply_next_internal = mk_fun "Slicing.Request.apply_next_internal"
    let pretty = mk_fun "Slicing.Request.pretty"
  end

end

(* ************************************************************************* *)
(** {2 Properties} *)
(* ************************************************************************* *)

module Properties = struct

  module Interp = struct
    let code_annot = mk_fun "Properties.Interp.code_annot"
    let lval = mk_fun "Properties.Interp.lval"
    let expr = mk_fun "Properties.Interp.expr"
    let term_lval_to_lval = mk_fun "Properties.Interp.term_lval_to_lval"
    let term_to_exp = mk_fun "Properties.Interp.term_to_exp"

    let force_term_to_exp = 
      ref (fun ~result_type:_ _ -> 
	     not_yet_implemented "Properties.Interp.force_term_to_exp")
    let force_back_exp_to_term = 
      mk_fun "Properties.Interp.force_back_exp_to_term"
    let force_term_lval_to_lval = 
      ref (fun ~result_type:_ _ -> 
	     not_yet_implemented "Properties.Interp.force_term_lval_to_lval")
    let force_back_lval_to_term_lval = 
      mk_fun "Properties.Interp.force_back_lval_to_term_lval"
    let force_term_offset_to_offset = 
      ref (fun ~result_type:_ _ -> 
	     not_yet_implemented
	       "Properties.Interp.force_term_offset_to_offset")
    let force_back_offset_to_term_offset = 
      mk_fun "Properties.Interp.force_back_offset_to_term_offset"

    let force_exp_to_term = 
      mk_fun "Properties.Interp.force_exp_to_term"
    let force_lval_to_term_lval = 
      mk_fun "Properties.Interp.force_lval_to_term_lval"
    let force_exp_to_predicate = 
      mk_fun "Properties.Interp.force_exp_to_predicate"
    let force_exp_to_assertion = 
      mk_fun "Properties.Interp.force_exp_to_assertion"

    let from_range_to_comprehension = 
      mk_fun "Properties.Interp.from_range_to_comprehension"
    let from_comprehension_to_range = 
      mk_fun "Properties.Interp.from_comprehension_to_range"

    let force_tsets_elem_to_exp = 
      ref (fun ~result_type:_ _ -> 
	     not_yet_implemented "Properties.Interp.force_tsets_elem_to_exp")
    let force_back_exp_to_tsets_elem = 
      mk_fun "Properties.Interp.force_back_exp_to_tsets_elem"
    let force_tsets_lval_to_lval = 
      ref (fun ~result_type:_ _ -> 
	     not_yet_implemented "Properties.Interp.force_tsets_lval_to_lval")
    let force_back_lval_to_tsets_lval = 
      mk_fun "Properties.Interp.force_back_lval_to_tsets_lval"
    let force_tsets_offset_to_offset = 
      ref (fun ~result_type:_ _ -> 
	     not_yet_implemented
	       "Properties.Interp.force_tsets_offset_to_offset")
    let force_back_offset_to_tsets_offset = 
      mk_fun "Properties.Interp.force_back_offset_to_tsets_offset"

    let force_tsets_elem_to_term =
      mk_fun "Properties.Interp.force_tsets_elem_to_term"
    let force_back_term_to_tsets_elem =
      mk_fun "Properties.Interp.force_back_term_to_tsets_elem"
    let force_tsets_lval_to_term_lval = 
      mk_fun "Properties.Interp.force_tsets_lval_to_term_lval"
    let force_back_term_lval_to_tsets_lval = 
      mk_fun "Properties.Interp.force_back_term_lval_to_tsets_lval"

    let term_to_lval = mk_fun "Properties.Interp.term_to_lval"
    let term_offset_to_offset =
      mk_fun "Properties.Interp.term_offset_to_offset"
    let tsets_to_exp = mk_fun "Properties.Interp.tsets_to_exp"
    let tsets_to_offset = mk_fun "Properties.Interp.tsets_to_offset"
    let tsets_to_lval = mk_fun "Properties.Interp.tsets_to_lval"
    let tsets_elem_to_lval = mk_fun "Properties.Interp.tsets_elem_to_lval"
    let tsets_elem_to_exp = mk_fun "Properties.Interp.tsets_elem_to_exp"

    module To_zone = struct
      type t_ctx = {state_opt:bool option; ki_opt:(stmt * bool) option; kf:Kernel_function.t}
      let mk_ctx_func_contrat = mk_fun "Interp.To_zone.mk_ctx_func_contrat"
      let mk_ctx_stmt_contrat = mk_fun "Interp.To_zone.mk_ctx_stmt_contrat"
      let mk_ctx_stmt_annot = mk_fun "Interp.To_zone.mk_ctx_stmt_annot"
      type t = {before:bool ; ki:stmt ; zone:Locations.Zone.t}
      type t_decl = VarinfoSet.t
      type t_pragmas = {ctrl: Cilutil.StmtSet.t ; stmt: Cilutil.StmtSet.t}
      let from_term = mk_fun "Interp.To_zone.from_term"
      let from_terms= mk_fun "Interp.To_zone.from_terms"
      let from_pred = mk_fun "Interp.To_zone.from_pred"
      let from_preds= mk_fun "Interp.To_zone.from_preds"
      let from_zone = mk_fun "Interp.To_zone.from_zone"
      let from_zones= mk_fun "Interp.To_zone.from_zones"
      let from_stmt_annot= mk_fun "Interp.To_zone.from_stmt_annot"
      let from_stmt_annots= mk_fun "Interp.To_zone.from_stmt_annots"
      let from_func_annots= mk_fun "Interp.To_zone.from_func_annots"
      let code_annot_filter= mk_fun "Interp.To_zone.code_annot_filter"
    end
  end


  let predicates_on_stmt s =
    let annots = Annotations.get s in
    let annotation_to_predicates a =
      let code_annotation_to_predicates ca =
        match ca.annot_content with
	  | AAssert (_,p,status) -> [p,(fun new_status -> status.status <- new_status)]
          | AAssume p -> [p,(fun _ -> ())]
	  | APragma _ -> []
          | AInvariant _ -> [] (*TODO: a more clever interpretation?*)
          | AVariant _ -> []
          | AAssigns _ -> []
          | AStmtSpec _ -> assert false
      in
      match a with
      | WP _ -> assert false
      | User ca | AI (_,ca) -> code_annotation_to_predicates ca
    in
    List.fold_left
      (fun (before,after,spec as acc) a -> match a with
       | Before (WP _) | After (WP _)  -> acc
       | Before (User { annot_content = AStmtSpec spec' } |
                 AI (_,{annot_content = AStmtSpec spec' }) )
         ->
           let spec =
             match spec with
                 None -> spec'
               | Some s -> Logic_const.merge_funspec s spec'; s
           in
           (before,after, Some spec)
       | After (User { annot_content = AStmtSpec _spec' } |
                 AI (_,{annot_content = AStmtSpec _spec' }) ) ->
           CilE.warn_once "Ignoring statement contract rooted after statement";
           acc
       | Before b ->annotation_to_predicates b@before,after, spec
       | After a -> before, annotation_to_predicates a@after, spec)
      ([],[],None)
      annots

  (** Insert a statement in a fundec.
      Modifies the fundec and preserves the Cfg. *)
  let insert_stmt fundec ~original_sid ~new_stmt ~before =
    let visitor = object
      inherit nopCilVisitor
      method vstmt s =
        if s.sid = original_sid then
          let new_lst = if before then [new_stmt;s] else [s;new_stmt] in
          ChangeTo (mkStmtCfgBlock new_lst)
        else DoChildren
    end
    in
    ignore (visitCilFunction visitor fundec)


  let insert_assert_in_file kf kinstr ~before annot =
    let sid = Ast_info.get_sid kinstr in
    let fundec = Kernel_function.get_definition kf in
    let ref_stmt = match kinstr with Kglobal -> assert false | Kstmt s -> s in
    let new_stmtkind = Instr (Code_annot (annot,locUnknown)) in
    let new_stmt = mkStmtCfg ~before ~new_stmtkind ~ref_stmt in
    insert_stmt
      fundec
      ~original_sid:sid
      ~new_stmt
      ~before;
    Kernel_function.register_stmt kf new_stmt;
    Value.Table.replace (Kstmt new_stmt) (Value.get_state kinstr)

  let add_assert kf kinstr ~before prop =
    let interp_prop = User (!Interp.code_annot kf kinstr ~before prop) in
    let localized = if before then Before interp_prop else After interp_prop in
    Annotations.add kinstr localized

  let get_user_assert _kf ki ~before =
    List.fold_left
      (fun acc a -> match a with
	 | Before (User a) when before -> a :: acc
	 | After (User a) when not before -> a :: acc
	 | _ -> acc)
      [] (Annotations.get ki)

  let add_alarm _kf ki alarm_type annot =
    (*if Debug.get () > 0 then Format.printf
      "First try at emitting alarm at %d@\n" (get_sid ki);*)
    let old_annots = Annotations.get ki in
    if List.for_all
      (function
       | Before (AI (a_t,old_annot)) when a_t = alarm_type ->
           Pervasives.compare
             old_annot.annot_content
             annot.annot_content
           <> 0
       | _ -> true)
      old_annots
    then
      ((*if Debug.get () > 0 then
        Format.printf "Emitting alarm at %d@\n" (get_sid ki);*)
        Annotations.add ki (Before (AI (alarm_type,annot))))

  let synchronize_alarms () =
    Alarms.fold
      (fun ki (at,annot) () ->
         match Globals.Functions.find_englobing_kf ki with
         | None ->
             CilE.warn_once "global alarm occured. Check the log above."
         | Some kf ->
             match ki with
             | Kglobal -> assert false
             | Kstmt s ->
	         add_alarm kf s at annot)
      ();
    Alarms.clear ()

end

module Wp = struct
  exception Failed of string

  let compute_with_cfg = mk_fun "compute_with_cfg"

  let compute_for_post =  mk_fun "Wp.compute_for_post"
end

(* ************************************************************************* *)
(** {2 Others plugins} *)
(* ************************************************************************* *)

module Miel = struct
  let extract_all = mk_fun "Miel.extract_all"
  let run_gui = mk_fun "Miel.run_gui"
  let gui_present = ref false
end

module Impact = struct
  let compute_pragmas = mk_fun "Impact.compute_pragmas"
  let from_stmt = mk_fun "Impact.from_stmt"
end

module Security = struct
  let run_whole_analysis = mk_fun "Security.run_whole_analysis"
  let run_ai_analysis = mk_fun "Security.run_ai_analysis"
  let run_slicing_analysis = mk_fun "Security.run_slicing_analysis"
  let get_component = mk_fun "Security.get_component"
  let get_direct_component = mk_fun "Security.get_direct_component"
  let get_indirect_backward_component =
    mk_fun "Security.get_indirect_backward_component"
  let get_forward_component = mk_fun "Security.get_forward_component"
  let impact_analysis = mk_fun "Security.impact_analysis"
  let self = ref Project.Computation.dummy
end

module Jessie = struct
  let run_analysis = mk_fun "Jessie.run_analysis"
end

module Occurrence = struct
  type t = (kinstr * lval) list
  let get = mk_fun "Occurrence.get"
  let get_last_result = mk_fun "Occurrence.get_last_result"
  let print_all = mk_fun "Occurrence.print_all"
  let self = ref Project.Computation.dummy
end

module Cxx = struct
  let suffixes = [ ".cc"; ".cxx"; ".cpp"; ".c++"]
  let mangle_cmdline_name = mk_fun "Cxx.mangle_entry_name"
  let mangle_cmdline_names = mk_fun "Cxx.mangle_entry_names"

  let get_original_files = mk_fun "Cxx.get_original_files"
  let set_original_files = mk_fun "Cxx.set_original_files"

  let print_cxx = mk_fun "Cxx.print_cxx"
  let loc_of_elsa_loc = mk_fun "Cxx.loc_of_elsa_loc"
end

module CxxSlicing = struct
  let slice_cxx = ref (fun (_: string option) -> ())
end

module Constant_Propagation = struct
  let get = mk_fun "Constant_Propagation.run_propagation"
end

module Syntactic_callgraph = struct
  let dump = mk_fun "Syntactic_callgraph.dump"
end

module Postdominators = struct
  let compute = mk_fun "Postdominators.compute"
  let is_postdominator
      : (kernel_function -> opening:stmt -> closing:stmt -> bool) ref
      = mk_fun "Postdominators.is_postdominator"
  exception Top
  let stmt_postdominators = mk_fun "Postdominators.stmt_postdominators"
  let display = mk_fun "Postdominators.display"
  let print_dot = mk_fun "Postdominators.print_dot"
end

module Dominators = struct
  let compute = mk_fun "Dominators.compute"
  let is_dominator
      : (kernel_function -> opening:stmt -> closing:stmt -> bool) ref
      = mk_fun "Dominators.is_dominator"
  exception Top
  let stmt_dominators = mk_fun "Dominators.stmt_dominators"
  let display = mk_fun "Dominators.display"
  let print_dot = mk_fun "Dominators.print_dot"
end



module Ltl_to_acsl = struct
  let run = mk_fun "Ltl_to_acsl.run"
end


module Metrics = struct
  type t =
      { sloc: int;
	call_statements: int;
	goto_statements: int;
	assign_statements: int;
	if_statements: int;
	loop_statements: int;
	mem_access: int;
	functions_without_source: int Cilutil.VarinfoHashtbl.t;
	functions_with_source: int Cilutil.VarinfoHashtbl.t;
      }
  let get = mk_fun "Metrics.get"
  let pretty = mk_fun "Metrics.pretty"
  let dump = mk_fun "Metrics.dump"
  let last_result = mk_fun "Metrics.last_result"
end



(* ************************************************************************* *)
(** {2 Graphs} *)
(* ************************************************************************* *)

module Semantic_Callgraph = struct
  let topologically_iter_on_functions =
    mk_fun "Semantic_Callgraph.topologically_iter_on_functions"
end

(* ************************************************************************* *)
(** {2 Toplevel} *)
(* ************************************************************************* *)

module Toplevel = struct
  let replay = mk_fun "Toplevel.replay"
  let run_all_plugins = mk_fun "Toplevel.run_all_plugins"
end

(* ************************************************************************* *)
(** {2 GUI} *)
(* ************************************************************************* *)

let progress = ref (fun () -> ())

(*
Local Variables:
compile-command: "LC_ALL=C make -C ../.. -j"
End:
*)
