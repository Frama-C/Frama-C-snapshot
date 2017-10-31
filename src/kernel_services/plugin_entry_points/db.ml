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

open Cil_types
open Cil_datatype
open Extlib

type 'a how_to_journalize =
  | Journalize of string * 'a Type.t
  | Journalization_not_required
  | Journalization_must_not_happen of string

let register how_to_journalize r f =
  match how_to_journalize with
  | Journalize (name, ty) -> r := Journal.register ("!Db." ^ name) ty f
  | Journalization_not_required -> r := f
  | Journalization_must_not_happen name ->
      r := Journal.never_write ("!Db." ^ name) f

let register_compute name deps r f =
  let name = "!Db." ^ name in
  let f = Journal.register name (Datatype.func Datatype.unit Datatype.unit) f in
  let compute, self = State_builder.apply_once name deps f in
  r := compute;
  self

let register_guarded_compute name is_computed r f =
  let name = "!Db." ^ name in
  let f = Journal.register name (Datatype.func Datatype.unit Datatype.unit) f in
  let compute () = if not (is_computed ()) then f () in
  r := compute

module Main = struct
  include Hook.Make(struct end)
  let play = mk_fun "Main.play"
end

module Toplevel = struct

  let run = ref (fun f -> f ())

end

(* ************************************************************************* *)
(** {2 Inouts} *)
(* ************************************************************************* *)

module type INOUTKF = sig
  type t
  val self_internal: State.t ref
  val self_external: State.t ref
  val compute : (kernel_function -> unit) ref

  val get_internal : (kernel_function -> t) ref
  val get_external : (kernel_function -> t) ref

  val display : (Format.formatter -> kernel_function -> unit) ref
  val pretty : Format.formatter -> t -> unit
end
module type INOUT = sig
  include INOUTKF
  val statement : (stmt -> t) ref
  val kinstr : kinstr -> t option
end

(** State_builder.of outputs
    - over-approximation of zones written by each function. *)
module Outputs = struct
  type t = Locations.Zone.t
  let self_internal = ref State.dummy
  let self_external = ref State.dummy
  let compute = mk_fun "Out.compute"
  let display = mk_fun "Out.display"
  let display_external = mk_fun "Out.display_external"
  let get_internal = mk_fun "Out.get_internal"
  let get_external = mk_fun "Out.get_external"
  let statement = mk_fun "Out.statement"
  let kinstr ki = match ki with
    | Kstmt s -> Some (!statement s)
    | Kglobal -> None

  let pretty = Locations.Zone.pretty
end

(** State_builder.of read inputs
    - over-approximation of locations read by each function. *)
module Inputs = struct
  (*       What about [Inputs.statement] ? *)
  type t = Locations.Zone.t
  let self_internal = ref State.dummy
  let self_external = ref State.dummy
  let self_with_formals = ref State.dummy
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

(** State_builder.of operational inputs
    - over-approximation of zones whose input values are read by each function,
    State_builder.of sure outputs
    - under-approximation of zones written by each function. *)
module Operational_inputs = struct
  type t = Inout_type.t
  let self_internal = ref State.dummy
  let self_external = ref State.dummy
  let compute = mk_fun "Operational_inputs.compute"
  let display = mk_fun "Operational_inputs.display"
  let get_internal = mk_fun "Operational_inputs.get_internal"
  let get_internal_precise = ref (fun ?stmt:_ _ ->
    failwith ("Db.Operational_inputs.get_internal_precise not implemented"))
  let get_external = mk_fun "Operational_inputs.get_external"

  module Record_Inout_Callbacks =
    Hook.Build (struct type t = Value_types.callstack * Inout_type.t end)

  let pretty fmt x =
    Format.fprintf fmt "@[<v>";
    Format.fprintf fmt "@[<v 2>Operational inputs:@ @[<hov>%a@]@]@ "
      Locations.Zone.pretty (x.Inout_type.over_inputs);
    Format.fprintf fmt "@[<v 2>Operational inputs on termination:@ @[<hov>%a@]@]@ "
      Locations.Zone.pretty (x.Inout_type.over_inputs_if_termination);
    Format.fprintf fmt "@[<v 2>Sure outputs:@ @[<hov>%a@]@]"
      Locations.Zone.pretty (x.Inout_type.under_outputs_if_termination);
    Format.fprintf fmt "@]";

end

(** Derefs computations *)
module Derefs = struct
  type t = Locations.Zone.t
  let self_internal = ref State.dummy
  let self_external = ref State.dummy
  let compute = mk_fun "Derefs.compute"
  let display = mk_fun "Derefs.display"
  let get_internal = mk_fun "Derefs.get_internal"
  let get_external = mk_fun "Derefs.get_external"
  let statement = mk_fun "Derefs.statement"
  let kinstr ki = match ki with
    | Kstmt s -> Some (!statement s)
    | Kglobal -> None

  let pretty = Locations.Zone.pretty
end


(* ************************************************************************* *)
(** {2 Values} *)
(* ************************************************************************* *)

module Value = struct
  type state = Cvalue.Model.t
  type t = Cvalue.V.t

  (* This function is responsible for clearing completely Value's state
     when the user-supplied initial state or main arguments are changed.
     It is set deep inside Value  for technical reasons *)
  let initial_state_changed = mk_fun "Value.initial_state_changed"

  (* Arguments of the root function of the value analysis *)
  module ListArgs = Datatype.List(Cvalue.V)
  module FunArgs =
    State_builder.Option_ref
      (ListArgs)
      (struct
        let name = "Db.Value.fun_args"
        let dependencies =
          [ Ast.self; Kernel.LibEntry.self; Kernel.MainFunction.self]
       end)
  let () = Ast.add_monotonic_state FunArgs.self


  exception Incorrect_number_of_arguments

  let fun_get_args () = FunArgs.get_option ()

  (* This function is *not* journalized *)
  let fun_set_args =
    let module L = Datatype.List(Cvalue.V) in
    Journal.register "(failwith \"Function cannot be journalized: \
        Db.Value.fun_set_args\" : _ -> unit)"
      (Datatype.func L.ty Datatype.unit)
      (fun l ->
         if
           not
             (Extlib.opt_equal ListArgs.equal (Some l) (FunArgs.get_option ()))
         then begin
           !initial_state_changed ();
           FunArgs.set l
         end)


  let fun_use_default_args =
    Journal.register "Db.Value.fun_use_default_args"
      (Datatype.func Datatype.unit Datatype.unit)
      (fun () ->
         if FunArgs.get_option () <> None then
           (!initial_state_changed (); FunArgs.clear ()))


  (* Initial memory state of the value analysis *)
  module VGlobals =
    State_builder.Option_ref
      (Cvalue.Model)
      (struct
         let name = "Db.Value.Vglobals"
         let dependencies = [Ast.self]
       end)

  (* This function is *not* journalized *)
  let globals_set_initial_state =
    Journal.register "(failwith \"Function cannot be journalized: \
        Db.Value.globals_set_initial_state\" : _ -> unit)"
      (Datatype.func Cvalue.Model.ty Datatype.unit)
      (fun state ->
         if not (Extlib.opt_equal Cvalue.Model.equal
                   (Some state)
                   (VGlobals.get_option ()))
         then begin
           !initial_state_changed ();
           VGlobals.set state
         end)


  let globals_use_default_initial_state =
    Journal.register
      "Db.Value.globals_use_default_initial_state"
      (Datatype.func Datatype.unit Datatype.unit)
      (fun () -> if VGlobals.get_option () <> None then
         (!initial_state_changed (); VGlobals.clear ()))

  let initial_state_only_globals = mk_fun "Value.initial_state_only_globals"

  let globals_state () =
    match VGlobals.get_option () with
      | Some v -> v
      | None -> !initial_state_only_globals ()

  let globals_use_supplied_state () = not (VGlobals.get_option () = None)

  (* Do NOT add dependencies to Kernel parameters here, but at the top of
     Value/Value_parameters *)
  let dependencies =
    [ Ast.self;
      Alarms.self;
      Annotations.code_annot_state;
      FunArgs.self;
      VGlobals.self ]

  let size = 256

  module States_by_callstack =
    Value_types.Callstack.Hashtbl.Make(Cvalue.Model)

  module Table_By_Callstack =
    Cil_state_builder.Stmt_hashtbl(States_by_callstack)
      (struct
        let name = "Db.Value.Table_By_Callstack"
        let size = size
        let dependencies = dependencies
       end)
  module Table =
    Cil_state_builder.Stmt_hashtbl(Cvalue.Model)
      (struct
        let name = "Db.Value.Table"
        let size = size
        let dependencies = [ Table_By_Callstack.self ]
       end)
  (* Clear Value's various caches each time [Db.Value.is_computed] is updated,
     including when it is set, reset, or during project change. Some operations
     of Value depend on -ilevel, -plevel, etc, so clearing those caches when
     Value ends ensures that those options will have an effect between two runs
     of Value. *)
  let () = Table_By_Callstack.add_hook_on_update
    (fun _ ->
       Cvalue.V_Offsetmap.clear_caches ();
       Cvalue.Model.clear_caches ();
       Locations.Location_Bytes.clear_caches ();
       Locations.Zone.clear_caches ();
       Function_Froms.Memory.clear_caches ();
    )


  module AfterTable_By_Callstack =
    Cil_state_builder.Stmt_hashtbl(States_by_callstack)
      (struct
        let name = "Db.Value.AfterTable_By_Callstack"
        let size = size
        let dependencies = [ Table_By_Callstack.self ]
       end)


  let self = Table_By_Callstack.self
  let only_self = [ self ]

  let mark_as_computed =
    Journal.register "Db.Value.mark_as_computed"
      (Datatype.func Datatype.unit Datatype.unit)
      Table_By_Callstack.mark_as_computed

  let is_computed () = Table_By_Callstack.is_computed ()

  module Conditions_table =
    Cil_state_builder.Stmt_hashtbl
      (Datatype.Int)
      (struct
         let name = "Db.Value.Conditions_table"
         let size = 101
         let dependencies = only_self
       end)

  let merge_conditions h =
    Cil_datatype.Stmt.Hashtbl.iter
      (fun stmt v ->
        try
          let old = Conditions_table.find stmt in
          Conditions_table.replace stmt (old lor v)
        with Not_found ->
          Conditions_table.add stmt v)
      h

  let mask_then = 1
  let mask_else = 2

  let condition_truth_value s =
    try
      let i = Conditions_table.find s in
      ((i land mask_then) <> 0, (i land mask_else) <> 0)
    with Not_found -> false, false

  module RecursiveCallsFound =
    State_builder.Set_ref
      (Kernel_function.Set)
      (struct
        let name = "Db.Value.RecursiveCallsFound"
        let dependencies = only_self
       end)

  let ignored_recursive_call kf =
    RecursiveCallsFound.mem kf

  let recursive_call_occurred kf =
    RecursiveCallsFound.add kf

  module Called_Functions_By_Callstack =
    State_builder.Hashtbl(Kernel_function.Hashtbl)
      (States_by_callstack)
      (struct
         let name = "Db.Value.Called_Functions_By_Callstack"
         let size = 11
         let dependencies = only_self
       end)

  module Called_Functions_Memo =
    State_builder.Hashtbl(Kernel_function.Hashtbl)
      (Cvalue.Model)
      (struct
         let name = "Db.Value.Called_Functions_Memo"
         let size = 11
         let dependencies = [ Called_Functions_By_Callstack.self ]
       end)

(*
  let pretty_table () =
   Table.iter
      (fun k v ->
         Kernel.log ~kind:Log.Debug
           "GLOBAL TABLE at %a: %a@\n"
           Kinstr.pretty k
           Cvalue.Model.pretty v)

  let pretty_table_raw () =
    Kinstr.Hashtbl.iter
      (fun k v ->
         Kernel.log ~kind:Log.Debug
           "GLOBAL TABLE at %a: %a@\n"
           Kinstr.pretty k
           Cvalue.Model.pretty v)
*)

  type callstack = (kernel_function * kinstr) list

  module Record_Value_Callbacks =
    Hook.Build
      (struct
         type t = (kernel_function * kinstr) list * (state Stmt.Hashtbl.t) Lazy.t
       end)

  module Record_Value_Callbacks_New =
    Hook.Build
      (struct
         type t =
           (kernel_function * kinstr) list *
           ((state Stmt.Hashtbl.t) Lazy.t  * (state Stmt.Hashtbl.t) Lazy.t)
             Value_types.callback_result
       end)

  module Record_Value_After_Callbacks =
    Hook.Build
      (struct
         type t = (kernel_function * kinstr) list * (state Stmt.Hashtbl.t) Lazy.t
       end)

  module Record_Value_Superposition_Callbacks =
    Hook.Build
      (struct
         type t = (kernel_function * kinstr) list * (state list Stmt.Hashtbl.t) Lazy.t
       end)

  module Call_Value_Callbacks =
    Hook.Build
      (struct type t = state * (kernel_function * kinstr) list end)

  module Call_Type_Value_Callbacks =
    Hook.Build(struct
      type t = [`Builtin of Value_types.call_result | `Spec of funspec
               | `Def | `Memexec]
        * state * (kernel_function * kinstr) list end)
  ;;


  module Compute_Statement_Callbacks =
    Hook.Build
      (struct type t = stmt * callstack * state list end)

  (* -remove-redundant-alarms feature, applied at the end of an Eva analysis,
     fulfilled by the Scope plugin that also depends on Eva. We thus use a
     reference here to avoid a cyclic dependency. *)
  let rm_asserts = mk_fun "Value.rm_asserts"

  let no_results = mk_fun "Value.no_results"

  let update_callstack_table ~after stmt callstack v =
    let open Value_types in
    let find,add =
      if after
      then AfterTable_By_Callstack.find, AfterTable_By_Callstack.add
      else Table_By_Callstack.find, Table_By_Callstack.add
    in
    try
      let by_callstack = find stmt in
      begin try
        let o = Callstack.Hashtbl.find by_callstack callstack in
        Callstack.Hashtbl.replace by_callstack callstack(Cvalue.Model.join o v)
        with Not_found ->
          Callstack.Hashtbl.add by_callstack callstack v
      end;
    with Not_found ->
      let r = Callstack.Hashtbl.create 7 in
      Callstack.Hashtbl.add r callstack v;
      add stmt r

  let merge_initial_state cs state =
    let open Value_types in
    let kf = match cs with (kf, _) :: _ -> kf | _ -> assert false in
    let by_callstack =
      try Called_Functions_By_Callstack.find kf
      with Not_found ->
        let h = Callstack.Hashtbl.create 7 in
        Called_Functions_By_Callstack.add kf h;
        h
    in
    try
      let old = Callstack.Hashtbl.find by_callstack cs in
      Callstack.Hashtbl.replace by_callstack cs (Cvalue.Model.join old state)
    with Not_found ->
      Callstack.Hashtbl.add by_callstack cs state

  let get_initial_state kf =
    assert (is_computed ()); (* this assertion fails during value analysis *)
    try Called_Functions_Memo.find kf
    with Not_found ->
      let state =
        try
          let open Value_types in
          let by_callstack = Called_Functions_By_Callstack.find kf in
          Callstack.Hashtbl.fold
            (fun _cs state acc -> Cvalue.Model.join acc state)
            by_callstack Cvalue.Model.bottom
        with Not_found -> Cvalue.Model.bottom
      in
      Called_Functions_Memo.add kf state;
      state

  let get_initial_state_callstack kf =
    assert (is_computed ()); (* this assertion fails during value analysis *)
    try Some (Called_Functions_By_Callstack.find kf)
    with Not_found -> None

  let valid_behaviors = mk_fun "Value.get_valid_behaviors"

  let add_formals_to_state = mk_fun "add_formals_to_state"

  let noassert_get_stmt_state s =
    if !no_results (Kernel_function.(get_definition (find_englobing_kf s)))
    then Cvalue.Model.top
    else
      try Table.find s
      with Not_found ->
        let ho = try Some (Table_By_Callstack.find s) with Not_found -> None in
        let state =
          match ho with
          | None -> Cvalue.Model.bottom
          | Some h ->
            Value_types.Callstack.Hashtbl.fold (fun _cs state acc ->
              Cvalue.Model.join acc state
            ) h Cvalue.Model.bottom
        in
        Table.add s state;
        state

  let noassert_get_state k =
    match k with
      | Kglobal -> globals_state ()
      | Kstmt s -> noassert_get_stmt_state s

  let get_stmt_state s =
    assert (is_computed ()); (* this assertion fails during value analysis *)
    noassert_get_stmt_state s

  let get_state k =
    assert (is_computed ()); (* this assertion fails during value analysis *)
    noassert_get_state k

  let get_stmt_state_callstack ~after stmt =
    assert (is_computed ()); (* this assertion fails during value analysis *)
    try
      Some (if after then AfterTable_By_Callstack.find stmt else
          Table_By_Callstack.find stmt)
    with Not_found -> None

  let fold_stmt_state_callstack f acc ~after stmt =
    assert (is_computed ()); (* this assertion fails during value analysis *)
    match get_stmt_state_callstack ~after stmt with
    | None -> acc
    | Some h -> Value_types.Callstack.Hashtbl.fold (fun _ -> f) h acc

  let fold_state_callstack f acc ~after ki =
    assert (is_computed ()); (* this assertion fails during value analysis *)
    match ki with
    | Kglobal -> f (globals_state ()) acc
    | Kstmt stmt -> fold_stmt_state_callstack f acc ~after stmt

  let is_reachable = Cvalue.Model.is_reachable

  exception Is_reachable
  let is_reachable_stmt stmt =
    if !no_results (Kernel_function.(get_definition (find_englobing_kf stmt)))
    then true
    else
      let ho = try Some (Table_By_Callstack.find stmt) with Not_found -> None in
      match ho with
      | None -> false
      | Some h ->
        try
          Value_types.Callstack.Hashtbl.iter
            (fun _cs state ->
              if Cvalue.Model.is_reachable state
              then raise Is_reachable) h;
          false
        with Is_reachable -> true

  let is_accessible ki =
    match ki with
    | Kglobal -> Cvalue.Model.is_reachable (globals_state ())
    | Kstmt stmt -> is_reachable_stmt stmt

  let is_called = mk_fun "Value.is_called"
  let callers = mk_fun "Value.callers"

  let access_location = mk_fun "Value.access_location"

  let find state loc = Cvalue.Model.find state loc

  let access =  mk_fun "Value.access"
  let access_expr =  mk_fun "Value.access_expr"

  (** Type for a Value builtin function *)

  type builtin_sig =
      state ->
      (Cil_types.exp * Cvalue.V.t * Cvalue.V_Offsetmap.t) list ->
      Value_types.call_result

  exception Outside_builtin_possibilities
  let register_builtin = mk_fun "Value.register_builtin"
  let registered_builtins = mk_fun "Value.registered_builtins"
  let mem_builtin = mk_fun "Value.mem_builtin"

  let use_spec_instead_of_definition =
    mk_fun "Value.use_spec_instead_of_definition"

  let eval_lval =
    ref (fun ?with_alarms:_ _ -> mk_labeled_fun "Value.eval_lval")
  let eval_expr =
    ref (fun ?with_alarms:_ _ -> mk_labeled_fun "Value.eval_expr")

  let eval_expr_with_state =
    ref (fun ?with_alarms:_ _ -> mk_labeled_fun "Value.eval_expr_with_state")

  let reduce_by_cond = mk_fun "Value.reduce_by_cond"

  let find_lv_plus = mk_fun "Value.find_lv_plus"

  let pretty_state = Cvalue.Model.pretty

  let pretty = Cvalue.V.pretty

  let compute = mk_fun "Value.compute"

  let memoize = mk_fun "Value.memoize"
  let expr_to_kernel_function = mk_fun "Value.expr_to_kernel_function"
  let expr_to_kernel_function_state =
    mk_fun "Value.expr_to_kernel_function_state"

  exception Not_a_call

  let call_to_kernel_function call_stmt = match call_stmt.skind with
    | Instr (Call (_, fexp, _, _)) ->
        let _, called_functions =
          !expr_to_kernel_function ?with_alarms:None ~deps:None
            (Kstmt call_stmt) fexp
        in called_functions
    | Instr(Local_init(_, ConsInit(f,_,_),_)) ->
      Kernel_function.Hptset.singleton (Globals.Functions.get f)
    | _ -> raise Not_a_call


  let lval_to_loc_with_deps = mk_fun "Value.lval_to_loc_with_deps"
  let lval_to_loc_with_deps_state = mk_fun "Value.lval_to_loc_with_deps_state"
  let lval_to_loc = mk_fun "Value.lval_to_loc"
  let lval_to_offsetmap = mk_fun "Value.lval_to_offsetmap"
  let lval_to_offsetmap_state = mk_fun "Value.lval_to_offsetmap_state"
  let lval_to_loc_state = mk_fun "Value.lval_to_loc_state"
  let lval_to_zone = mk_fun "Value.lval_to_zone"
  let lval_to_zone_state = mk_fun "Value.lval_to_zone_state"
  let lval_to_zone_with_deps_state = mk_fun "Value.lval_to_zone_with_deps_state"
  let lval_to_precise_loc_state =
    ref (fun ?with_alarms:_ _ -> mk_labeled_fun "Value.lval_to_precise_loc")
  let lval_to_precise_loc_with_deps_state =
    mk_fun "Value.lval_to_precise_loc_with_deps_state"
  let assigns_inputs_to_zone = mk_fun "Value.assigns_inputs_to_zone"
  let assigns_outputs_to_zone = mk_fun "Value.assigns_outputs_to_zone"
  let assigns_outputs_to_locations = mk_fun "Value.assigns_outputs_to_locations"
  let verify_assigns_froms = mk_fun "Value.verify_assigns_froms"

  module Logic = struct
    let eval_predicate =
      ref (fun ~pre:_ ~here:_ _ ->
        raise
          (Extlib.Unregistered_function
             "Function 'Value.Logic.eval_predicate' not registered yet"))

  end

  exception Void_Function

  let find_return_loc kf =
    try
      let ki = Kernel_function.find_return kf in
      let lval = match ki with
        | { skind = Return (Some ({enode = Lval ((_ , offset) as lval)}), _) }
          ->
          assert (offset = NoOffset) ;
          lval
        | { skind = Return (None, _) } -> raise Void_Function
        | _ -> assert false
      in
      !lval_to_loc (Kstmt ki) ?with_alarms:None lval
    with Kernel_function.No_Statement ->
      (* [JS 2011/05/17] should be better to have another name for this
         exception or another one since it is possible to have no return without
         returning void (the case when the kf corresponds to a declaration *)
      raise Void_Function

  exception Aborted

  let display = mk_fun "Value.display"

  let emitter = ref Emitter.dummy

end

module From = struct

  exception Not_lval

  let access = mk_fun "From.access"
  let find_deps_no_transitivity = mk_fun "From.find_deps_no_transitivity"
  let find_deps_no_transitivity_state =
    mk_fun "From.find_deps_no_transitivity_state"
  let find_deps_term_no_transitivity_state =
    mk_fun "From.find_deps_term_no_transitivity_state"
  let compute = mk_fun "From.compute"
  let compute_all = mk_fun "From.compute_all"
  let compute_all_calldeps = mk_fun "From.compute_all_calldeps"
  let is_computed = mk_fun "From.is_computed"
  let pretty = mk_fun "From.pretty"
  let get = mk_fun "From.get"
  let self = ref State.dummy
  let display = mk_fun "From.display"

  module Record_From_Callbacks =
    Hook.Build
      (struct
        type t =
            (Kernel_function.t Stack.t) *
              Function_Froms.Memory.t Stmt.Hashtbl.t *
              (Kernel_function.t * Function_Froms.Memory.t) list
              Stmt.Hashtbl.t
       end)

  module Callwise = struct
    let iter = mk_fun "From.Callwise.iter"
    let find = mk_fun "From.Callwise.find"
  end
end

module Users = struct
  let get = mk_fun "Users.get"
end

(* ************************************************************************* *)
(** {2 PDG} *)
(* ************************************************************************* *)

module Pdg = struct
  type t = PdgTypes.Pdg.t

  type t_nodes_and_undef =
      ((PdgTypes.Node.t * Locations.Zone.t option) list * Locations.Zone.t option)

  exception Top = PdgTypes.Pdg.Top
  exception Bottom = PdgTypes.Pdg.Bottom

  let self = ref State.dummy

  let get = mk_fun "Pdg.get"

  let from_same_fun pdg1 pdg2 =
    let kf1 =  PdgTypes.Pdg.get_kf pdg1 in
    let kf2 =  PdgTypes.Pdg.get_kf pdg2 in
      Kernel_function.equal kf1 kf2

  let node_key = mk_fun "Pdg.node_key"

  let find_decl_var_node = mk_fun "Pdg.find_decl_var_node"
  let find_input_node = mk_fun "Pdg.find_input_nodes"
  let find_ret_output_node = mk_fun "Pdg.find_ret_output_node"
  let find_output_nodes = mk_fun "Pdg.find_output_nodes"
  let find_all_inputs_nodes = mk_fun "Pdg.find_all_inputs_nodes"
  let find_stmt_and_blocks_nodes = mk_fun "Pdg.find_stmt_and_blocks_nodes"
  let find_simple_stmt_nodes = mk_fun "Pdg.find_simplestmt_nodes"
  let find_stmt_node = mk_fun "Pdg.find_stmt_node"
  let find_label_node = mk_fun "Pdg.find_label_node"
  let find_entry_point_node = mk_fun "Pdg.find_entry_point_node"
  let find_top_input_node = mk_fun "Pdg.find_top_input_node"
  let find_call_ctrl_node = mk_fun "Pdg.find_call_ctrl_node"
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

  let custom_related_nodes = mk_fun "Pdg.custom_related_nodes"

  let find_call_stmts = mk_fun "Pdg.find_call_stmts"

  let iter_nodes = mk_fun "Pdg.iter_nodes"

  let extract = mk_fun "Pdg.extract"
  let pretty = ref (fun ?bw:_ _ _ -> mk_labeled_fun "Pdg.pretty")
  let pretty_node = mk_fun "Pdg.pretty_node"
  let pretty_key = mk_fun "Pdg.pretty_key"

end

(* ************************************************************************* *)
(** {2 Spare Code} *)
(* ************************************************************************* *)

(** Detection of the unused code of an application. *)
module Sparecode = struct
  let get =
    ref (fun ~select_annot:_  -> mk_labeled_fun "Sparecode.run")
  let rm_unused_globals =
    ref (fun ?new_proj_name:_ -> mk_labeled_fun "Sparecode.rm_unused_globals")
end

(* ************************************************************************* *)
(** {2 Properties} *)
(* ************************************************************************* *)

module Properties = struct

  let mk_resultfun s =
    ref (fun ~result:_ -> failwith (Printf.sprintf "Function '%s' not registered yet" s))

  module Interp = struct

   exception No_conversion

  (** Interpretation and conversions of of formulas *)
    let code_annot = mk_fun "Properties.Interp.code_annot"
    let term_lval = mk_fun "Properties.Interp.term_lval"
    let term = mk_fun "Properties.Interp.term"
    let predicate = mk_fun "Properties.Interp.predicate"
    let term_lval_to_lval = mk_resultfun "Properties.Interp.term_lval_to_lval"
    let term_to_exp = mk_resultfun "Properties.Interp.term_to_exp"
    let term_to_lval = mk_resultfun "Properties.Interp.term_to_lval"
    let loc_to_lval = mk_resultfun "Properties.Interp.loc_to_lval"
    (* loc_to_loc and loc_to_locs are defined in Value/Eval_logic, not
       in Logic_interp *)
    let loc_to_loc = mk_resultfun "Properties.Interp.loc_to_loc"
    let loc_to_loc_under_over = mk_resultfun "Properties.Interp.loc_to_loc_with_deps"
    let loc_to_offset = mk_resultfun "Properties.Interp.loc_to_offset"
    let loc_to_exp = mk_resultfun "Properties.Interp.loc_to_exp"
    let term_offset_to_offset =
      mk_resultfun "Properties.Interp.term_offset_to_offset"

    module To_zone : sig
      (** The signature of the mli is copy pasted here because of
          http://caml.inria.fr/mantis/view.php?id=7313 *)
      type t_ctx =
          {state_opt:bool option;
           ki_opt:(stmt * bool) option;
           kf:Kernel_function.t}

      val mk_ctx_func_contrat:
        (kernel_function -> state_opt:bool option -> t_ctx) ref
      (** To build an interpretation context relative to function
          contracts. *)

      val mk_ctx_stmt_contrat:
        (kernel_function -> stmt -> state_opt:bool option -> t_ctx) ref
      (** To build an interpretation context relative to statement
          contracts. *)

      val mk_ctx_stmt_annot:
        (kernel_function -> stmt -> t_ctx) ref
      (** To build an interpretation context relative to statement
          annotations. *)

      type t = {before:bool ; ki:stmt ; zone:Locations.Zone.t}
      type t_zone_info = (t list) option
      (** list of zones at some program points.
       *   None means that the computation has failed. *)

      type t_decl = {var: Varinfo.Set.t ; (* related to vars of the annot *)
                     lbl: Logic_label.Set.t} (* related to labels of the annot *)
      type t_pragmas =
        {ctrl: Stmt.Set.t ; (* related to //@ slice pragma ctrl/expr *)
         stmt: Stmt.Set.t}  (* related to statement assign and
                               //@ slice pragma stmt *)

      val from_term: (term -> t_ctx -> t_zone_info * t_decl) ref
      (** Entry point to get zones needed to evaluate the [term] relative to
          the [ctx] of interpretation. *)

      val from_terms: (term list -> t_ctx -> t_zone_info * t_decl) ref
      (** Entry point to get zones needed to evaluate the list of [terms]
          relative to the [ctx] of interpretation. *)

      val from_pred: (predicate -> t_ctx -> t_zone_info * t_decl) ref
      (** Entry point to get zones needed to evaluate the [predicate]
          relative to the [ctx] of interpretation. *)

      val from_preds: (predicate list -> t_ctx -> t_zone_info * t_decl) ref
      (** Entry point to get zones needed to evaluate the list of
          [predicates] relative to the [ctx] of interpretation. *)

      val from_zone: (identified_term -> t_ctx -> t_zone_info * t_decl) ref
      (** Entry point to get zones needed to evaluate the [zone] relative to
          the [ctx] of interpretation. *)

      val from_stmt_annot:
        (code_annotation -> stmt * kernel_function
         -> (t_zone_info * t_decl) * t_pragmas) ref
      (** Entry point to get zones needed to evaluate an annotation on the
          given stmt. *)

      val from_stmt_annots:
        ((code_annotation -> bool) option ->
         stmt * kernel_function -> (t_zone_info * t_decl) * t_pragmas) ref
      (** Entry point to get zones needed to evaluate annotations of this
          [stmt]. *)

      val from_func_annots:
        (((stmt -> unit) -> kernel_function -> unit) ->
         (code_annotation -> bool) option ->
         kernel_function -> (t_zone_info * t_decl) * t_pragmas) ref
      (** Entry point to get zones
          needed to evaluate annotations of this [kf]. *)

      val code_annot_filter:
        (code_annotation ->
         threat:bool -> user_assert:bool -> slicing_pragma:bool ->
         loop_inv:bool -> loop_var:bool -> others:bool -> bool) ref
        (** To quickly build an annotation filter *)

    end
    = struct
      type t_ctx =
          { state_opt: bool option;
            ki_opt: (stmt * bool) option;
            kf:Kernel_function.t }
      let mk_ctx_func_contrat = mk_fun "Interp.To_zone.mk_ctx_func_contrat"
      let mk_ctx_stmt_contrat = mk_fun "Interp.To_zone.mk_ctx_stmt_contrat"
      let mk_ctx_stmt_annot = mk_fun "Interp.To_zone.mk_ctx_stmt_annot"
      type t = {before:bool ; ki:stmt ; zone:Locations.Zone.t}
      type t_zone_info = (t list) option
      type t_decl =
          { var: Varinfo.Set.t;
            lbl: Logic_label.Set.t }
      type t_pragmas =
          { ctrl: Stmt.Set.t;
            stmt: Stmt.Set.t }
      let from_term = mk_fun "Interp.To_zone.from_term"
      let from_terms= mk_fun "Interp.To_zone.from_terms"
      let from_pred = mk_fun "Interp.To_zone.from_pred"
      let from_preds= mk_fun "Interp.To_zone.from_preds"
      let from_zone = mk_fun "Interp.To_zone.from_zone"
      let from_stmt_annot= mk_fun "Interp.To_zone.from_stmt_annot"
      let from_stmt_annots= mk_fun "Interp.To_zone.from_stmt_annots"
      let from_func_annots= mk_fun "Interp.To_zone.from_func_annots"
      let code_annot_filter= mk_fun "Interp.To_zone.code_annot_filter"
    end

    let to_result_from_pred =
      mk_fun "Properties.Interp.to_result_from_pred"
  end

  let add_assert emitter kf kinstr prop =
    Kernel.deprecated "Db.Properties.add_assert" ~now:"ACSL_importer plug-in"
      (fun () ->
        let interp_prop = !Interp.code_annot kf kinstr prop in
        Annotations.add_code_annot emitter kinstr interp_prop)
      ()

end

(* ************************************************************************* *)
(** {2 Others plugins} *)
(* ************************************************************************* *)

module Impact = struct
  let compute_pragmas = mk_fun "Impact.compute_pragmas"
  let from_stmt = mk_fun "Impact.from_stmt"
  let from_nodes = mk_fun "Impact.from_nodes"
end

module Security = struct
  let run_whole_analysis = mk_fun "Security.run_whole_analysis"
  let run_ai_analysis = mk_fun "Security.run_ai_analysis"
  let run_slicing_analysis = mk_fun "Security.run_slicing_analysis"
  let self = ref State.dummy
end

module Occurrence = struct
  type t = (kernel_function option * kinstr * lval) list
  let get = mk_fun "Occurrence.get"
  let get_last_result = mk_fun "Occurrence.get_last_result"
  let print_all = mk_fun "Occurrence.print_all"
  let self = ref State.dummy
end

module RteGen = struct
  type status_accessor =
      string * (kernel_function -> bool -> unit) * (kernel_function -> bool)
  let compute = mk_fun "RteGen.compute"
  let annotate_kf = mk_fun "RteGen.annotate_kf"
  let self = ref State.dummy
  let do_precond = mk_fun "RteGen.do_precond"
  let do_all_rte = mk_fun "RteGen.do_all_rte"
  let do_rte = mk_fun "RteGen.do_rte"
  let get_all_status = mk_fun "RteGen.get_all_status"
  let get_precond_status = mk_fun "RteGen.get_precond_status"
  let get_signedOv_status = mk_fun "RteGen.get_signedOv_status"
  let get_divMod_status = mk_fun "RteGen.get_divMod_status"
  let get_initialized_status = mk_fun "RteGen.get_initialized_status"
  let get_signed_downCast_status = mk_fun "RteGen.get_signed_downCast_status"
  let get_memAccess_status = mk_fun "RteGen.get_memAccess_status"
  let get_pointerCall_status = mk_fun "RteGen.get_pointerCall_status"
  let get_unsignedOv_status = mk_fun "RteGen.get_unsignedOv_status"
  let get_unsignedDownCast_status = mk_fun "RteGen.get_unsignedDownCast_status"
  let get_float_to_int_status = mk_fun "RteGen.get_float_to_int_status"
  let get_finite_float_status = mk_fun "RteGen.get_finite_float_status"
end

module Constant_Propagation = struct
  let get = mk_fun "Constant_Propagation.get"
  let compute = mk_fun "Constant_Propagation.compute"
end

module PostdominatorsTypes = struct
  exception Top

  module type Sig = sig
    val compute: (kernel_function -> unit) ref
    val stmt_postdominators:
      (kernel_function -> stmt -> Stmt.Hptset.t) ref
    val is_postdominator:
      (kernel_function -> opening:stmt -> closing:stmt -> bool) ref
    val display: (unit -> unit) ref
    val print_dot : (string -> kernel_function -> unit) ref
  end
end


module Postdominators = struct
  let compute = mk_fun "Postdominators.compute"
  let is_postdominator
      : (kernel_function -> opening:stmt -> closing:stmt -> bool) ref
      = mk_fun "Postdominators.is_postdominator"
  let stmt_postdominators = mk_fun "Postdominators.stmt_postdominators"
  let display = mk_fun "Postdominators.display"
  let print_dot = mk_fun "Postdominators.print_dot"
end

module PostdominatorsValue = struct
  let compute = mk_fun "PostdominatorsValue.compute"
  let is_postdominator
      : (kernel_function -> opening:stmt -> closing:stmt -> bool) ref
      = mk_fun "PostdominatorsValue.is_postdominator"
  let stmt_postdominators = mk_fun "PostdominatorsValue.stmt_postdominators"
  let display = mk_fun "PostdominatorsValue.display"
  let print_dot = mk_fun "PostdominatorsValue.print_dot"
end

(* ************************************************************************* *)
(** {2 GUI} *)
(* ************************************************************************* *)

let progress = ref (fun () -> ())

exception Cancel

(*
Local Variables:
compile-command: "make -C ../../.."
End:
*)
