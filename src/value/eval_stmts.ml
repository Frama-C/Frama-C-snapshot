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

(** Value analysis of statements and functions bodies *)

open Cil_types
open Cil
open Cil_datatype
open Locations
open Abstract_interp
open Bit_utils
open Cvalue
open Ast_printer
open Value_util
open Eval_exprs
open Eval_logic
open Locals_scoping

exception Wrong_function_type (* in function call *)

let need_cast t1 t2 =
  match unrollType t1, unrollType t2 with
    | (TInt _| TEnum _| TPtr _), (TInt _| TEnum _| TPtr _)
    | TFloat _, TFloat _
    | TComp _, TComp _ ->
        (try bitsSizeOf t1 <> bitsSizeOf t2
         with SizeOfError _ -> true)
    | _ -> true

let offsetmap_of_lv ~with_alarms state lv =
  CilE.set_syntactic_context (CilE.SyMem lv);
  let loc_to_read =
    lval_to_loc ~with_alarms state lv
  in
  Cvalue.Model.copy_offsetmap
    ~with_alarms:CilE.warn_none_mode
    loc_to_read
    state

exception Got_bottom
  
let compute_actual ~with_alarms (one_library_fun, all_library_funs) state e =
  let interpreted_expr, o = match e with
    | { enode = Lval l }
        when (* make sure not a bit-field *) not (is_bitfield l ()) ->
        let _, _, interpreted_expr =
          eval_lval ~conflate_bottom:false ~with_alarms None state l
        in
        if one_library_fun then
          ignore (eval_lval ~conflate_bottom:true ~with_alarms None state l);
        if V.is_bottom interpreted_expr
        then begin
          if not one_library_fun then (* alarm *)
            ignore (eval_lval ~conflate_bottom:true ~with_alarms None state l);
          if all_library_funs
          then begin
            Value_parameters.result ~current:true
              "Non-termination@ in@ evaluation@ of@ library function@ call@ lvalue@ argument@ @[%a@]" (!d_lval) l;
          end;
          raise Got_bottom;
        end;
        let r = do_cast ~with_alarms (typeOf e) interpreted_expr in
        let o = offsetmap_of_lv ~with_alarms state l in
        (match o with
           | Some o -> r, o
           | None ->
               Format.printf "failure in evaluation of function arguments@\n\
                   lval %a -> %a@." !d_lval l V.pretty interpreted_expr;
               assert false)
    | _ ->
        let interpreted_expr = eval_expr ~with_alarms state e in
        if V.is_bottom interpreted_expr
        then begin
          Value_parameters.result ~current:true
            "Non-termination@ in@ evaluation@ of@ function@ call@ expression@ argument@ @[%a@]"
	    (!d_exp) e;
            raise Got_bottom
          end;
          let typ = Cil.typeOf e in
          interpreted_expr,
          Builtins.offsetmap_of_value ~typ interpreted_expr
    in
    e, interpreted_expr, o


(* Forward reference to {Eval_calls.compute_call} *)
let compute_call_ref = ref (fun _ -> assert false)

module Computer
  (AnalysisParam:sig
    val stmt_can_reach : stmt -> stmt -> bool
    val is_natural_loop : stmt -> bool
    val slevel: int
    val initial_state : State_set.t
    val active_behaviors: Eval_logic.ActiveBehaviors.t
    end) =

struct
  let debug = ref false
  let name = "Values analysis"

  let stmt_can_reach = AnalysisParam.stmt_can_reach

  let obviously_terminates = 
    Value_parameters.ObviouslyTerminatesAll.get() (* TODO: by function *)      

  let slevel = 
    if obviously_terminates
    then max_int
    else
      AnalysisParam.slevel

  let debug = ref false

  let fused_initial_state = lazy (State_set.join AnalysisParam.initial_state)

  let current_table = Current_table.create ()

  let states_after = Cil_datatype.Stmt.Hashtbl.create 5

  (* During the dataflow analysis, if required by a callback, we store the
     state after a statement, but only if the following conditions are met
     ([succ] being a successor of [s])
     - if [s] is an instr (which almost always change the state, unlike
       the other kind of statements)
     - if there is a control-flow join on [succ]
     - if [s] is the last instruction of a block that contains
       local variables
     For statements for which the function below returns false, we deduce
     the state after by the state before [succ] or another successor of [s].
     This avoids potentially useless computations
  *)
  let store_state_after_during_dataflow s succ =
    ((match s.skind with Instr _ -> true | _ -> false) &&
       (match succ.preds with [_] -> false | _ -> true))
    || (let b1 = Kernel_function.find_enclosing_block s
       and b2 = Kernel_function.find_enclosing_block succ in
       not (Cil_datatype.Block.equal b1 b2) && b1.blocals <> [])

  (* Computation of the per-function 'after statement' states *)
  let local_after_states superposed =
    lazy (
      let superposed = Lazy.force superposed in
      Stmt.Hashtbl.iter
        (fun stmt state ->
           List.iter
             (fun pred ->
                if not (store_state_after_during_dataflow pred stmt) then
                  try
                    let cur = Stmt.Hashtbl.find states_after pred in
                    Stmt.Hashtbl.replace states_after pred
                      (Cvalue.Model.join state cur)
                  with Not_found -> Stmt.Hashtbl.add states_after pred state
             ) stmt.preds;
        ) superposed;
      (* Since the return instruction has no successor, it is not visited
         by the iter above. We fill it manually *)
      (try
         let ret = Kernel_function.find_return (current_kf ()) in
         let s = Stmt.Hashtbl.find superposed ret in
         Stmt.Hashtbl.add states_after ret s
       with Kernel_function.No_Statement | Not_found -> ()
      );
      states_after
    )

  (* Merging of 'after statement' states in the global table *)
  let merge_after after_full =
    Cil_datatype.Stmt.Hashtbl.iter
      (fun stmt st ->
         try
           let prev = Db.Value.AfterTable.find stmt in
           Db.Value.AfterTable.replace stmt (Cvalue.Model.join prev st)
         with Not_found ->
           Db.Value.AfterTable.add stmt st
      ) (Lazy.force after_full)

  (* Table storing whether conditions on 'if' have been evaluated
     to true or false *)
  let conditions_table = Cil_datatype.Stmt.Hashtbl.create 5

  let merge_current ~degenerate =
    let superposed = lazy (Current_table.states current_table) in
    let after_full = local_after_states superposed in
    Current_table.merge_db_table superposed;
    Db.Value.merge_conditions conditions_table;
    if Value_parameters.ResultsAfter.get () then merge_after after_full;
    if not degenerate then begin
      let stack_for_callbacks = for_callbacks_stack () in

      if not (Db.Value.Record_Value_Superposition_Callbacks.is_empty ())
      then begin
        let current_superpositions =
          lazy (Current_table.superpositions current_table)
        in
        if Value_parameters.ValShowProgress.get() then
          Value_parameters.feedback
            "now calling Record_Value_Superposition callbacks";
        Db.Value.Record_Value_Superposition_Callbacks.apply
          (stack_for_callbacks, current_superpositions);
      end ;

      if not (Db.Value.Record_Value_Callbacks.is_empty ())
      then begin
        if Value_parameters.ValShowProgress.get() then
          Value_parameters.feedback "now calling Record_Value callbacks";
        Db.Value.Record_Value_Callbacks.apply
          (stack_for_callbacks, superposed)
      end;

      if not (Db.Value.Record_Value_After_Callbacks.is_empty ())
      then begin
        if Value_parameters.ValShowProgress.get() then
          Value_parameters.feedback "now calling Record_After_Value callbacks";
        Db.Value.Record_Value_After_Callbacks.apply
          (stack_for_callbacks, after_full);
      end;

    end;
    Current_table.clear current_table

  type u =
      { counter_unroll : int; (* how many times this state has been crossed *)
        mutable value : State_set.t ; }

  module StmtStartData =
    Dataflow.StartData(struct type t = u let size = 107 end)

  type t = u

  let copy (d: t) = d

  let display_one fmt v =
    State_set.iter (fun value ->
      if not (Cvalue.Model.is_reachable value) then
        Format.fprintf fmt "Statement (x%d): UNREACHABLE@\n" v.counter_unroll
      else
        Format.fprintf fmt "Statement (x%d)@\n%a"
          v.counter_unroll
          Cvalue.Model.pretty
          value)
      v.value

  let pretty fmt (d: t) = display_one fmt d

  let computeFirstPredecessor (_s: stmt) state =
    { counter_unroll = 0; value = state.value;}

  let getWidenHints (s: stmt) =
    Widen.getWidenHints (current_kf()) s

  let counter_unroll_target = ref 100

  let combinePredecessors (_s: stmt) ~old new_ =
    if State_set.is_empty (new_.value)
    then None
    else begin
        if old.counter_unroll >= slevel
        then
          let sum =
            Cvalue.Model.join
              (State_set.join new_.value)
              (State_set.join old.value)
          in
          Some {counter_unroll = old.counter_unroll ;
                value = State_set.singleton sum;}
        else begin try
          let merged = State_set.merge_into new_.value old.value in
          let length_new = State_set.length new_.value in
          let new_counter_unroll = old.counter_unroll + length_new in
          if new_counter_unroll >= !counter_unroll_target
          then begin
            Value_parameters.result ~once:true
              "Semantic level unrolling superposing up to %d states"
              !counter_unroll_target;
            counter_unroll_target := !counter_unroll_target + 100;
          end;
          let result =
            Some
              { value =  merged ;
                counter_unroll = new_counter_unroll }
          in
          result
        with State_set.Unchanged -> None
        end
    end

  (** Clobber list for bases containing addresses of local variables. *)
  let bases_containing_locals = ref Location_Bits.Top_Param.bottom
  let remember_bases_with_locals =
    remember_bases_with_locals bases_containing_locals

(** Precondition: the type of [exp] and the type [loc_lv] may be different
    only if the cast from [typeOf exp] and [typeOf loc_lv] is a truncation
    or an extension.
    This function will not perform any conversion (float->int, int->float, ...)
    [exp] should not be bottom for optimization purposes in the caller.
  *)
  let do_assign_abstract_value_to_loc ~with_alarms state lv loc_lv exp =
    assert (not (Cvalue.V.is_bottom exp));
    (* Or one may propagate bottoms uselessly for too long. *)
    let exp = (* truncate the value if the [lv] is too small: this may
                 happen when the [lv] is a bit-field. Otherwise, the
                 cast is explicit thanks to Cil and no truncation is
                 necessary. *)
      try
        (* if it is a bit-field, the size is statically known. *)
        let size = Int_Base.project loc_lv.size in
        try
          ignore (V.project_ival exp);
          cast_lval_bitfield lv size exp
        with
        | V.Not_based_on_null (* from [project_ival] *) ->
            (* The exp is a pointer: check there are enough bits in
               the bit-field to contain it. *)
            if Int.compare size (Int.of_int (sizeofpointer ())) >= 0
              || V.is_imprecise exp
            then exp
            else begin
              Value_parameters.result 
		"casting address to a bit-field of %s bits: this is smaller than sizeof(void*)" 
		(Int.to_string size);
              V.topify_arith_origin exp
            end
        | Neither_Int_Nor_Enum_Nor_Pointer
            (* from [signof_typeof_lval] *) -> exp
      with
      | Int_Base.Error_Top | Int_Base.Error_Bottom ->
          (* from [project]: size is not known  *)
          exp
    in
    let pretty_org fmt org = if not (Origin.is_top org) then
      Format.fprintf fmt " because of %a" Origin.pretty org
    in
    (match loc_lv.loc with
    | Location_Bits.Top (Location_Bits.Top_Param.Top, orig) ->
        Value_parameters.result
          "State before degeneration:@\n======%a@\n======="
          Cvalue.Model.pretty state;
	warning_once_current
          "writing at a completely unknown address@[%a@].@\nAborting."
          pretty_org orig;
        do_degenerate (Some lv)

    | Location_Bits.Top((Location_Bits.Top_Param.Set _) as param,orig) ->
        Value_parameters.result ~current:true ~once:true
          "writing somewhere in @[%a@]@[%a@]."
          Location_Bits.Top_Param.pretty param
          pretty_org orig
    | Location_Bits.Map _ -> (* everything is normal *) ());
    let exact = valid_cardinal_zero_or_one ~for_writing:true loc_lv in
    let value =
      Cvalue.Model.add_binding ~with_alarms ~exact
        state loc_lv exp
    in
   value

  (** Precondition: the type of [exp] and the type [loc_lv] may be different
      only if the cast from [typeOf exp]
      and [typeOfPointed lv] is a truncation or an extension.
      This function will not perform any conversion (float->int, int->float,..)
  *)
  let do_assign_abstract_value ~with_alarms state lv exp =
    let loc_lv = lval_to_loc ~with_alarms state lv in
    remember_bases_with_locals loc_lv exp;
    CilE.set_syntactic_context (CilE.SyMem lv);
    do_assign_abstract_value_to_loc ~with_alarms state lv loc_lv exp


  let offsetmap_top_addresses_of_locals is_local =
    let is_local_bytes = Location_Bytes.contains_addresses_of_locals is_local in
    fun offsetmap ->
      if Cvalue.V_Offsetmap.is_empty offsetmap
      then Location_Bytes.Top_Param.top, offsetmap
      else
        let loc_contains_addresses_of_locals t =
          let v = Cvalue.V_Or_Uninitialized.get_v t in
          is_local_bytes v
        in
        let locals, result =
          Cvalue.V_Offsetmap.top_stuff
            loc_contains_addresses_of_locals
            (fun v ->
               Cvalue.V_Or_Uninitialized.unspecify_escaping_locals
                 is_local v)
            Location_Bytes.Top_Param.join
            Location_Bytes.Top_Param.bottom
            offsetmap
        in
        locals, result

  let state_top_addresses_of_locals ~is_block offsetmap_top_addresses_of_locals fundec =
    let f k offsm =
      let locals, r = offsetmap_top_addresses_of_locals offsm in
      let found_locals = not (Cvalue.V_Offsetmap.equal r offsm) in
      if found_locals then
        warn_locals_escape is_block fundec k locals;
      r
    in
    (fun (state:Cvalue.Model.t) ->
       (* let's forget relations *)
       let simple_state = state in
       let f base acc =
         try
           let offset_to_clean = Cvalue.Model.find_base base simple_state
           in
           let cleaned_offsetmap = f base offset_to_clean in
           Cvalue.Model.add_offsetmap base cleaned_offsetmap acc
         with Not_found -> acc
       in
       try
           (Location_Bits.Top_Param.fold
              f
              !bases_containing_locals
              (f Base.null simple_state))
       with Location_Bits.Top_Param.Error_Top ->
         begin
           let f k offsm acc =
             let locals, r = offsetmap_top_addresses_of_locals offsm in
             let found_locals = not (Cvalue.V_Offsetmap.equal r offsm) in
             if found_locals then
               warn_locals_escape is_block fundec k locals;
             Cvalue.Model.add_offsetmap k r acc
           in
           let result =
             try
                 (Cvalue.Model.fold_base_offsetmap
                    f
                    state
                    Cvalue.Model.empty_map)
             with Cvalue.Model.Error_Bottom -> Cvalue.Model.bottom
           in
           result
         end)

  let top_addresses_of_locals fundec =
    let entry_point = Globals.entry_point () in
    if snd entry_point (* lib *) ||
      current_kf() != fst entry_point (* not entry point *)
    then
      let offsetmap_top_addresses_of_locals =
        offsetmap_top_addresses_of_locals
          (Cilutil.swap Base.is_formal_or_local fundec)
      in
      let state_top_addresses_of_locals =
        state_top_addresses_of_locals ~is_block:false
          offsetmap_top_addresses_of_locals fundec
      in
      offsetmap_top_addresses_of_locals, state_top_addresses_of_locals
    else (fun x -> Location_Bytes.Top_Param.bottom, x),(fun x -> x)

  let block_top_addresses_of_locals blocks =
    if List.for_all (fun b -> List.for_all (fun v -> v.vgenerated) b.blocals)
      blocks
    then
      fun x -> x (* no need to change the state if there is no local
                    variable or if all the variable have been generated
                    by Cil (in which case we know that they are correctly
                    initialized and used, don't we)
                 *)
    else
      let offsetmap_top_addresses_of_locals =
        offsetmap_top_addresses_of_locals
          (fun v -> List.exists (Base.is_block_local v) blocks)
      in
      let state_top_addresses_of_locals =
        state_top_addresses_of_locals ~is_block:true
          offsetmap_top_addresses_of_locals
          (Kernel_function.get_definition (current_kf()))
      in
      state_top_addresses_of_locals

 (* Assigns [exp] to [lv] in [state] *)
  let do_assign ~with_alarms old_state lv exp =
    assert (Cvalue.Model.is_reachable old_state);
    let fresh_flags () =
      let flag = ref false in
      (fun () -> flag := true),
      fun () -> !flag
    in
    let set_alarm, get_alarm = fresh_flags () in
    let logger v =
      if v <> CilE.Aignore
      then CilE.Acall set_alarm
      else CilE.Aignore
    in
    let warn_remember_mode =
      { CilE.imprecision_tracing = logger with_alarms.CilE.imprecision_tracing;
        others = with_alarms.CilE.others;
        unspecified = logger with_alarms.CilE.unspecified}
    in
    let reduced_state, _, evaled_exp =
      eval_expr_with_deps_state_subdiv ~with_alarms:warn_remember_mode None
        old_state
        exp
    in
(*    Value_parameters.debug ~level:2 "do_assign %a = (%a)(%a)"
      !d_lval lv
      !d_exp exp
      V.pretty evaled_exp; *)
    let left_loc = lval_to_loc ~with_alarms old_state lv in
    let is_bitfield = is_bitfield lv ~sizebf:left_loc.size () in
    remember_bases_with_locals left_loc evaled_exp;
    let warn_right_exp_imprecision () =
      (match with_alarms.CilE.imprecision_tracing with
       | CilE.Aignore -> ()
       | CilE.Acall f -> f ()
       | CilE.Alog _ ->
           match evaled_exp with
           | Cvalue.V.Top(_topparam,origin) ->
               Value_parameters.result ~once:true ~current:true
                 "@[<v>@[Assigning imprecise value to %a%t.@]%a%t@]"
                 !Ast_printer.d_lval lv
                 (fun fmt -> match lv with
                  | (Mem _, _) ->
                      Format.fprintf fmt "@ (i.e. %a)"
                        Locations.pretty left_loc
                  | (Var _, _) -> ())
                 (fun fmt org ->
                    if not (Origin.is_top origin) then
                      Format.fprintf fmt
                        "@ @[The imprecision@ originates@ from@ %a@]"
                        Origin.pretty org)
                 origin
                 pp_callstack
           | Cvalue.V.Map _ ->
               if not (Got_Imprecise_Value.get ()) &&
                 not (Cvalue.V.cardinal_zero_or_one evaled_exp)
               then begin
                   Got_Imprecise_Value.set true;
                   if (Value_parameters.ValShowProgress.get())
                   then
                     Value_parameters.result ~current:true
                       "assigning non deterministic value for the first time";
               end)
    in
    let reduced_state =
      match lv with
      | Mem mem_e,NoOffset ->
          let new_reduced_state =
            reduce_by_valid_expr
              ~positive:true ~for_writing:true
              mem_e
              reduced_state
          in
          if not (Cvalue.Model.is_reachable new_reduced_state)
          then begin
            CilE.set_syntactic_context (CilE.SyMem lv);
            CilE.warn_mem_write with_alarms ;
            Value_parameters.result ~current:true
              "all target addresses were invalid. This path is assumed to be dead.";
          end;
          new_reduced_state
            (*      | Var _ , Index _ -> assert false
                    TODO: do something for "TAB[i] = expr"
            *)
      | _ -> reduced_state
    in
      let default () =
        warn_right_exp_imprecision ();
        if get_alarm() then
          (* log alarms that have not been logged the first time *)
          ignore
            (eval_expr
               ~with_alarms:
               {CilE.imprecision_tracing=with_alarms.CilE.imprecision_tracing;
                others=CilE.Aignore;
                unspecified=with_alarms.CilE.unspecified}
               old_state
               exp);

        if Cvalue.V.is_bottom evaled_exp ||
          Location_Bits.equal left_loc.loc Location_Bits.bottom  ||
          not (Cvalue.Model.is_reachable reduced_state)
        then Cvalue.Model.bottom
        else begin
          CilE.set_syntactic_context (CilE.SyMem lv);
          do_assign_abstract_value_to_loc ~with_alarms
            reduced_state
            lv
            left_loc
            evaled_exp
          end
      in
      let default_lval exp_lv =
        (* directly copy the old value without trying to recompose it.
           Useful for structs assignment. *)
        let right_loc = lval_to_loc ~with_alarms old_state exp_lv in
	( match right_loc.size, left_loc.size with 
	  Int_Base.Value rsize, Int_Base.Value lsize when
	      Int.equal rsize lsize &&
		Int.to_int(rsize) > bitsSizeOf (TInt (IInt, [])) ->
		  if
		    Location_Bits.partially_overlaps 
		      rsize
		      right_loc.loc
		      left_loc.loc
		  then begin
		      warning_once_current "Partially overlapping lvalue assignment \"%a=%a;\". Left address in bits: %a. Right address in bits: %a. assert(separated or same)"
			!d_lval lv
			!d_lval exp_lv
			Location_Bits.pretty left_loc.loc
		      	Location_Bits.pretty right_loc.loc;
		      CilE.stop_if_stop_at_first_alarm_mode ()
		    end
	| _ -> () );


        CilE.set_syntactic_context (CilE.SyMem exp_lv);
        let full_val =
          Cvalue.Model.find_unspecified
            ~with_alarms:CilE.warn_none_mode
            old_state
            right_loc
        in
        if Location_Bits.equal left_loc.loc Location_Bits.bottom  ||
          not (Cvalue.Model.is_reachable reduced_state) ||
          V_Or_Uninitialized.equal full_val V_Or_Uninitialized.bottom
        then Cvalue.Model.bottom
        else begin
            match right_loc.size, left_loc.size with
          | Int_Base.Value size, Int_Base.Value other_size
              when Int.equal other_size size ->
              let offsetmap_relations =
                  Cvalue.V_Offsetmap.empty (* TODO: cleanup *)
              in
              CilE.set_syntactic_context (CilE.SyMem exp_lv);
              let offsetmap_memory =
                match Cvalue.Model.copy_offsetmap ~with_alarms right_loc old_state with
                  | Some v -> v
                  | None -> raise Lmap.Cannot_copy (* invalid copy paste *)
              in
              let offsetmap =
                Cvalue.V_Offsetmap.over_intersection
                  offsetmap_relations
                  offsetmap_memory
              in
              if not (Cvalue.V_Offsetmap.is_empty offsetmap)
              then begin
                CilE.set_syntactic_context (CilE.SyMem lv);
                let copy_paste_succeeded =
                  Cvalue.Model.paste_offsetmap with_alarms
                    offsetmap left_loc.loc Int.zero size true reduced_state
                in
                (* Shall we warn about imprecise contents just copied? *)
                let module L = struct exception Got_imprecise end in
                (try
                   Cvalue.V_Offsetmap.iter_contents
                     (fun v ->
                        match Cvalue.V_Or_Uninitialized.get_v v with
                        | Location_Bytes.Map _ -> ()
                        | _ -> raise L.Got_imprecise)
                     offsetmap
                     size
                 with L.Got_imprecise ->
                   warn_right_exp_imprecision ());
                copy_paste_succeeded
              end
              else raise Lmap.Cannot_copy
          | _ -> raise Lmap.Cannot_copy
        end
      in
      let new_main_memory_state =
        try
          if is_bitfield then default()
          else
            (* An lval assignement might be hidden by a dummy cast *)
            let lv = find_lv ~with_alarms old_state exp in
            default_lval lv
        with Cannot_find_lv | Lmap.Cannot_copy
            (* from Cvalue.Model.paste_offsetmap
               or directly default_lval *) ->
              default ()
      in
      new_main_memory_state


  let do_assign ~with_alarms old_state lv exp =
    if true then do_assign ~with_alarms old_state lv exp
    else
      let vars =
        get_influential_vars ~with_alarms:CilE.warn_none_mode old_state exp
      in
      let rec try_sub vars =
        match vars with
        | [] | [ _ ] -> do_assign ~with_alarms old_state lv exp
        | v :: tail ->
            try
              if not (List.exists (fun x -> Locations.loc_equal v x) tail)
              then raise Too_linear;
              let value =
                Cvalue.Model.find
                  ~conflate_bottom:true
                  ~with_alarms:CilE.warn_none_mode
                  old_state
                  v
              in

              if Location_Bytes.is_included value Location_Bytes.top_float
              then raise Too_linear;

              ignore (Cvalue.V.splitting_cardinal_less_than
                         ~split_non_enumerable:42 value 142);
(*              Value_parameters.debug
                "subdiv assignment: candidate %a value %a@."
                Locations.pretty v
                Cvalue.V.pretty value; *)
              let treat_subdiv subvalue acc =
                let sub_oldstate =
                  (* FIXME: should be relation-aware primitive *)
                  Cvalue.Model.add_binding
                    ~with_alarms:CilE.warn_none_mode
                    ~exact:true
                    old_state
                    v
                    subvalue
                in
                let sub_newstate =
                  do_assign ~with_alarms sub_oldstate lv exp
                in
                Cvalue.Model.join acc sub_newstate
              in
              Location_Bytes.fold_enum
                ~split_non_enumerable:42
                treat_subdiv
                value
                Cvalue.Model.bottom
            with
            | Not_less_than | Too_linear -> try_sub tail
            | Location_Bytes.Error_Top -> assert false;
      in
      try_sub vars

  let empty_interpretation_result =
    None, Cvalue.Model.bottom, Location_Bits.Top_Param.bottom

  let assign_return_to_lv ~with_alarms funcexp lv return new_state =
    let loc = lval_to_loc ~with_alarms new_state lv in
    let rtype = getReturnType (typeOf funcexp) in
    let lvtyp = typeOfLval lv in
    let is_bitfield = is_bitfield lv ~sizebf:loc.size ~sizelv:(sizeof lvtyp) () in
    let default () =
      let value_with_init =
        V_Offsetmap.find_ival
          ~conflate_bottom:false
          ~validity:Base.All
          ~with_alarms:CilE.warn_none_mode
          Ival.zero
          return
          (Int.of_int (bitsSizeOf rtype))
      in
      let flags = V_Or_Uninitialized.get_flags value_with_init in
      let init = V_Or_Uninitialized.is_initialized flags in
      let no_esc = V_Or_Uninitialized.is_noesc flags in
      let value = V_Or_Uninitialized.get_v value_with_init in
      if not init then CilE.warn_uninitialized with_alarms;
      if not no_esc then CilE.warn_escapingaddr with_alarms;
      if Cvalue.V.is_bottom value && not (init && no_esc)
      then
        Value_parameters.result ~current:true
          "Function call returned an unspecified value. \
              This path is assumed to be dead.";
      let exact = valid_cardinal_zero_or_one ~for_writing:true loc in
      let evaled_exp =
        (* fix http://bts.frama-c.com/view.php?id=798 *)
        do_cast ~with_alarms:CilE.warn_none_mode rtype value in
      let evaled_exp =
        if is_bitfield
        then cast_lval_bitfield lv (Int_Base.project loc.size) evaled_exp
        else do_cast ~with_alarms:CilE.warn_none_mode lvtyp evaled_exp
      in
      remember_bases_with_locals loc evaled_exp;
      Cvalue.Model.add_binding
        ~with_alarms:CilE.warn_none_mode
        ~exact
        new_state
        loc
        evaled_exp
    in
    if is_bitfield || (need_cast lvtyp rtype)
    then default ()
    else
      (try
         let result =
           Cvalue.Model.paste_offsetmap with_alarms
             return
             loc.loc
             Int.zero
             (Int_Base.project loc.size)
             true
             new_state
         in
         let evaled_exp=
           Cvalue.V_Or_Uninitialized.get_v
             (V_Offsetmap.find_ival
                ~conflate_bottom:false
                ~validity:Base.All
                ~with_alarms:CilE.warn_none_mode
                Ival.zero
                return
                (Int.of_int (bitsSizeOf rtype))
             )
         in
         remember_bases_with_locals loc evaled_exp;
         result
       with Lmap.Cannot_copy -> default ())

  let interp_call stmt lval_to_assign funcexp argl d_value =
    let call_site_loc = CurrentLoc.get () in
    let with_alarms = warn_all_quiet_mode () in
    let return_type_funcexp =
      match unrollType (typeOf funcexp) with
	TFun (t, _, _, _) -> t
      | _ -> assert false
    in
    let bitssizeofreturntypefuncexp =
      bitsSizeOf return_type_funcexp
    in
    let state_after_call state =
      try
        let functions, _ = resolv_func_vinfo ~with_alarms None state funcexp in
        let is_library_function kf = not (Kernel_function.is_definition kf) in
        let calling_at_least_one_library_function =
          Kernel_function.Hptset.exists is_library_function functions
        in
        let calling_only_library_functions =
          calling_at_least_one_library_function &&
            (Kernel_function.Hptset.for_all is_library_function functions)
        in
        let compute_actual = compute_actual ~with_alarms
          (calling_at_least_one_library_function,
           calling_only_library_functions)
        in
        let actuals = List.map (compute_actual state) argl in
        let treat_one_call f (acc_rt,acc_res,acc_clobbered_set as acc) =
	  try
	    let return_type = Kernel_function.get_return_type f in
	    if bitsSizeOf return_type <> bitssizeofreturntypefuncexp
	    then raise Wrong_function_type;

            let return, result, clobbered_set =
              !compute_call_ref f ~call_kinstr:(Kstmt stmt) state actuals
            in
            let caller = current_kf (), stmt in
            Kf_state.add_caller f ~caller;
            CurrentLoc.set call_site_loc;
            (match acc_rt,return with
            | None,_ -> return
            | Some _, None -> acc_rt
            | Some acc_rt, Some return ->
		Some (snd (V_Offsetmap.join acc_rt return))),
            Cvalue.Model.join acc_res result,
            Location_Bits.Top_Param.join acc_clobbered_set clobbered_set
	  with Wrong_function_type ->
	    warning_once_current
	      "Pointed function type must match function pointer type when dereferenced: assert(Ook)";
	    CilE.stop_if_stop_at_first_alarm_mode ();
	    acc
        in
        let return,new_state,clobbered_set =
          Kernel_function.Hptset.fold treat_one_call
            functions
            empty_interpretation_result
        in
        bases_containing_locals :=
          Location_Bits.Top_Param.join !bases_containing_locals clobbered_set;
        match lval_to_assign with
          | None -> new_state
          | Some lv ->
              match return with
                | Some return ->
                    assign_return_to_lv ~with_alarms funcexp lv return new_state
                | None ->
                  if Cvalue.Model.is_reachable new_state
                  then
                    warning_once_current
                      "In function %t: called function returns void but \
                        returned value is assigned; ignoring assignment"
                      pretty_current_cfunction_name;
                  new_state
      with
        | Got_bottom ->
            CurrentLoc.set call_site_loc;
            Cvalue.Model.bottom
        | Leaf ->
            CurrentLoc.set call_site_loc;
            (match lval_to_assign with
              | None ->  state
              | Some lv ->
                  let evaled_exp = V.top_leaf_origin () in
                  do_assign_abstract_value ~with_alarms state lv evaled_exp)
    in
    State_set.fold
      (fun acc state -> State_set.add (state_after_call state) acc)
      State_set.empty
      d_value

  let doInstr stmt (i: instr) (d: t) =
    !Db.progress ();
    CilE.start_stmt (Kstmt stmt);
    let d_states = d.value in
    let unreachable = State_set.is_empty d_states in
    let result =
      if unreachable then
        Dataflow.Done d
      else begin
          let apply_each_state f =
            let modified_states =
              State_set.fold
                (fun acc state_value -> State_set.add (f state_value) acc)
                State_set.empty
                d_states
            in
            Dataflow.Done { counter_unroll = 0; value =  modified_states }
          in
          (* update current statement *)
          match i with
          | Set (lv,exp,_loc) ->
              apply_each_state
                (fun state_value ->
                  do_assign
                    ~with_alarms:(warn_all_quiet_mode ())
                    state_value
                    lv
                    exp)
          | Call (lval_to_assign,
                 {enode = Lval (Var {vname=("__builtin_va_start"|"__builtin_va_arg"|"__builtin_va_end" as _builtin_name) },NoOffset)},
                 [{enode = Lval lv}],_loc) ->
(*            Format.printf "builtin: %s@." _builtin_name; *)
              apply_each_state
                (fun state ->
                  let state =
                    do_assign_abstract_value
                      ~with_alarms:(warn_all_quiet_mode ())
                      state
                      lv
                      Cvalue.V.top_int
                  in
                  ( match lval_to_assign with
                    None -> state
                  | Some lval_assign ->
                      do_assign_abstract_value
                        ~with_alarms:(warn_all_quiet_mode ())
                        state
                        lval_assign
                        Cvalue.V.top_int))
          | Call (lval_to_assign,funcexp,argl,_loc) ->
              Dataflow.Done
                { counter_unroll = 0;
                  value = interp_call stmt lval_to_assign funcexp argl d_states}
          | Asm _ ->
              warning_once_current
                "assuming assembly code has no effects in function %t"
                pretty_current_cfunction_name;
              Dataflow.Default
          | Skip _ ->
              Dataflow.Default
          | Code_annot (_,_) -> (* processed in dostmt from Db *)
              Dataflow.Default
        end
    in
    CilE.end_stmt ();
    result

  (* Reduce the given states according to the given code annotations.
     If [record] is true, update the proof state of the code annotation.
     DO NOT PASS record=false unless you known what your are doing *)
  let interp_annot state stmt ca record =
    let aux text behav p =
      let in_behavior =
        match behav with
          | [] -> True
          | behavs ->
              let ab = AnalysisParam.active_behaviors in
              let all_active = Extlib.filter_map'
                (ActiveBehaviors.behavior_from_name ab)
                (ActiveBehaviors.is_active ab)
                behavs
              in
              if all_active = [] then False
              else
                if List.exists (ActiveBehaviors.only_active ab) all_active
                then True
                else Unknown
      in
      if in_behavior = False
      then state
      else
        let result = fold_join_predicate State_set.fold
          (fun here ->
             let env = env_annot ~pre:!!fused_initial_state ~here in
             eval_predicate ~result:None env p)
          state
        in
        let ip = Property.ip_of_code_annot (current_kf()) stmt ca in
        let change_status st =
          if record then List.iter (fun p -> emit_status p st) ip 
	in
        let message, result =
          (match result, in_behavior with
             | Unknown, _ | False, Unknown ->
                 if State_set.is_empty state then begin
                   change_status Property_status.False_if_reachable;
                   "invalid (stopping propagation)", State_set.empty
                 end else begin
                   change_status Property_status.Dont_know;
                   "unknown", state
                 end
             | True, _ ->
                 change_status Property_status.True;
                 "valid", state
             | False, True ->
                 change_status Property_status.False_if_reachable;
                 "invalid (stopping propagation)", State_set.empty
             | _, False -> assert false)
        in
        if record then
	  Value_parameters.result ~once:true ~current:true
	    "%s got status %s.%t" text message pp_callstack;
        if in_behavior = True then
          let env = env_annot ~pre:!!fused_initial_state
            ~here:(State_set.join result) in
          reduce_by_disjunction ~result:None ~env
            result
            slevel
            p
        else
	  result
    in
    match ca.annot_content with
    | AAssert (behav,p) -> aux "Assertion" behav p
    | AInvariant (behav, true, p) -> aux "Loop invariant" behav p
    | APragma _
    | AInvariant (_, false, _)
    | AVariant _ | AAssigns _
    | AStmtSpec _ (*TODO*) -> state

  let check_non_overlapping state lvs1 lvs2 =
    List.iter
      (fun lv1 ->
         List.iter
           (fun lv2 ->
              let zone1 =
                Locations.valid_enumerate_bits ~for_writing:false
                  (lval_to_loc ~with_alarms:CilE.warn_none_mode state lv1)
              in
              let zone2 =
                Locations.valid_enumerate_bits ~for_writing:false
                  (lval_to_loc ~with_alarms:CilE.warn_none_mode state lv2)
              in
              if Locations.Zone.intersects zone1 zone2
              then begin
                CilE.set_syntactic_context
                  (CilE.SySep
                     (Cil.mkAddrOf ~loc:(CurrentLoc.get ()) lv1,
                      Cil.mkAddrOf ~loc:(CurrentLoc.get ()) lv2));
                CilE.warn_separated warn_all_mode
              end)
           lvs2)
      lvs1

(* TODO: Take advantage of calls information. *)
  let check_unspecified_sequence state seq =
    let rec check_one_stmt ((stmt1,_,writes1,_,_) as my_stmt) = function
        [] -> ()
      | (stmt2,_,_,_,_)::seq when stmt1 == stmt2 -> check_one_stmt my_stmt seq
      | (stmt2,modified2,writes2,reads2,_) :: seq ->
          let unauthorized_reads =
            (* TODO: try to have a more semantical interpretation of modified *)
            List.filter
              (fun x -> List.for_all
                (fun y -> not (Cil.compareLval x y)) modified2)
              writes1
          in
          check_non_overlapping state unauthorized_reads reads2;
          if stmt1.sid < stmt2.sid then
            check_non_overlapping state writes1 writes2;
          check_one_stmt my_stmt seq
    in
    List.iter (fun x -> check_one_stmt x seq) seq

  let doStmt (s: stmt) (d: t) =
    let states = d.value in
    d.value <- State_set.empty;
    let kinstr = Kstmt s in

    if State_set.is_empty states
    then
      Dataflow.SDefault
    else
      let annots_before =
        Annotations.single_fold_stmt
          (fun a acc ->
            match a with
            | User { annot_content = AStmtSpec _ }
            | AI (_,{annot_content = AStmtSpec _ }) -> acc
            | AI (_, b) | User b -> b :: acc)
          s
          []
      in
      CilE.start_stmt kinstr;
      let states =
        List.fold_left
          (fun states annot -> interp_annot states s annot true)
          states
          annots_before
      in
      CilE.end_stmt ();
      let not_already_states =
        if obviously_terminates
	then states
	else Current_table.update_and_tell_if_changed current_table s states
      in
      if State_set.is_empty not_already_states
      then Dataflow.SDefault
      else
	let is_return = match s.skind with Return _ -> true | _ -> false in
        let new_states =
          if d.counter_unroll >= slevel || (is_return && obviously_terminates)
          then
            let curr_wcounter, curr_wstate =
              Current_table.find_widening_info current_table s 
	    in
            let state = State_set.join states in
            let joined = Cvalue.Model.join curr_wstate state in
	    if obviously_terminates
	    then begin
	      Current_table.update_widening_info current_table s 0 joined;
	      states
	    end
	    else
              let r =
		if AnalysisParam.is_natural_loop s && curr_wcounter = 0 then
                  let wh_key_set, wh_hints = getWidenHints s in
                  let widen_hints =
                    true, wh_key_set(* no longer used thanks to 0/1 widening*),
                    wh_hints
                  in
                  snd (Cvalue.Model.widen widen_hints curr_wstate joined)
		else
                  joined
              in
              let new_widening =
		if curr_wcounter = 0
		then 1
		else pred curr_wcounter
              in
              let new_state = State_set.singleton r in
              if Cvalue.Model.equal r joined then (
		Current_table.update_widening_info current_table s new_widening r;
		new_state)
              else begin
              (* Try to correct over-widenings *)
		CilE.start_stmt kinstr;
		let new_states =
                (* Do *not* record the status after interpreting the annotation
                   here. Possible unproven assertions have already been
                   recorded when the assertion has been interpreted the first
                   time higher in this function. *)
                List.fold_left
                  (fun states annot -> interp_annot states s annot false)
                  new_state
                  annots_before
              in
              CilE.end_stmt ();
              let new_joined = State_set.join new_states in
              Current_table.update_widening_info
                current_table s new_widening new_joined;
              State_set.singleton new_joined
            end
          else states
        in
        let d = { d with value = new_states }
        in
         ( match s.skind with
         | Loop _ ->
             if d.counter_unroll >= slevel &&
              (Value_parameters.ValShowProgress.get())
            then
              Value_parameters.result ~level:1 ~once:true ~current:true
                "entering loop for the first time"
        | UnspecifiedSequence seq ->
	    if Kernel.UnspecifiedAccess.get () 
	    then begin
 	      CilE.start_stmt kinstr;
              State_set.iter
                (fun state -> check_unspecified_sequence state seq) states;
              CilE.end_stmt ()
	    end
        | _ -> ());
        Dataflow.SUse d

  let doEdge s succ d =
    let kinstr = Kstmt s in
    let states = d.value in
    CilE.start_stmt kinstr;
    (* We store the state after the execution of [s] for the callback
       {Value.Record_Value_After_Callbacks}. This is done here
       because we want to see the values of the variables local to the block *)
    if (Value_parameters.ResultsAfter.get () ||
        not (Db.Value.Record_Value_After_Callbacks.is_empty ()))
      && (store_state_after_during_dataflow s succ)
    then (
      let old =
        try Cil_datatype.Stmt.Hashtbl.find states_after s
        with Not_found -> Cvalue.Model.bottom
      in
      let updated = State_set.fold Cvalue.Model.join old states in
      Cil_datatype.Stmt.Hashtbl.replace states_after s updated
    );

    let states =
      match Kernel_function.blocks_closed_by_edge s succ with
      | [] -> states
      | closed_blocks ->
          let block_top_addresses_of_locals =
            block_top_addresses_of_locals closed_blocks
          in
            State_set.fold
              (fun set state ->
                let state =
                  Cvalue.Model.uninitialize_locals closed_blocks state
                in
                State_set.add (block_top_addresses_of_locals state) set)
              State_set.empty
              states;
    in
    CilE.end_stmt ();
    { d with value =  states }

  let filterStmt _stmt = true

  (* Remove all local variables and formals from table *)
  let externalize return kf =
    match kf.fundec with
    | Declaration _ -> assert false
    | Definition (fundec,_loc) ->
        assert
          (StmtStartData.iter
              (fun k v ->
                if State_set.is_empty (v.value)
                then ()
                else (Value_parameters.fatal "sid:%d@\n%a@\n"
                         k.sid
                         State_set.pretty (v.value)));
                true);
        let superpos =
          Current_table.find_superposition current_table return
        in
        let init_state =
          Current_table.find_superposition
            current_table
            (Kernel_function.find_first_stmt kf)
        in
        let superpos =
          let result =
            match return with
            | {skind = Return (Some ({enode = Lval (Var v,_)}),_)} ->
                Some v
            | _ -> None
          in
          check_fct_postconditions ~result
            kf
            ~init_state
            ~active_behaviors:AnalysisParam.active_behaviors
            ~post_state:superpos
            Normal
        in
        let state = State_set.join_dropping_relations superpos in

       if Value_parameters.ValShowProgress.get() then
        Value_parameters.feedback "Recording results for %a"
          Kernel_function.pretty kf;

        merge_current ~degenerate:false;
        let ret_val =
          (match return with
           | {skind = Return (Some ({enode = Lval lv}),_)} ->
               offsetmap_of_lv ~with_alarms:(warn_all_quiet_mode ()) state lv
           | {skind = Return (None,_)} -> None
           | _ -> assert false)
        in
        let state =
          Cvalue.Model.clear_state_from_locals fundec state
        in
        let offsetmap_top_addresses_of_locals, state_top_addresses_of_locals =
          top_addresses_of_locals fundec
        in
        let result =
          (match ret_val with
           | None -> ret_val
           | Some ret_val ->
               let locals, r = offsetmap_top_addresses_of_locals ret_val in
               let warn = not (Cvalue.V_Offsetmap.equal r ret_val)
               in
               if warn then warn_locals_escape_result fundec locals;
               Some r),
          state_top_addresses_of_locals state,
          !bases_containing_locals
        in
        result

  let doGuardOneCond stmt exp t =
    if State_set.is_empty (t.value)
    then Dataflow.GUnreachable
    else begin
        CilE.start_stmt (Kstmt stmt);
        let with_alarms = warn_all_quiet_mode () in
        let new_values =
          State_set.fold
            (fun acc state ->
              let test =
                eval_expr
                  ~with_alarms
                  state exp
              in
              CilE.set_syntactic_context
                (CilE.SyBinOp (Ne, Cil.zero ~loc:exp.eloc, exp));
              let warn, _, test =
                check_comparable Eq V.singleton_zero test
              in

              let do_it =
                (warn && Value_parameters.UndefinedPointerComparisonPropagateAll.get ()) ||
                  let t1 = unrollType (typeOf exp) in
                  if isIntegralType t1 || isPointerType t1
                  then V.contains_non_zero test
                  else true (* TODO: a float condition is true iff != 0.0 *)
              in
              if do_it then
                try
                  State_set.add
                    (reduce_by_cond ~with_alarms:CilE.warn_none_mode
                        state {positive = true; exp = exp})
                    acc
                with Reduce_to_bottom -> acc
              else acc)
            State_set.empty
            t.value
        in
        let result =
          if State_set.is_empty new_values then Dataflow.GUnreachable
          else Dataflow.GUse {t with value =  new_values}
        in
        CilE.end_stmt ();
        result

      end

  let mask_then = Db.Value.mask_then
  let mask_else = Db.Value.mask_else

  let doGuard stmt exp t =
    let not_exp = new_exp ~loc:exp.eloc (UnOp(LNot, exp, intType)) in
    let th, el as thel =
      doGuardOneCond stmt exp t, doGuardOneCond stmt not_exp t
    in
    let current_condition_status =
      try
        Cil_datatype.Stmt.Hashtbl.find conditions_table stmt
      with Not_found -> 0
    in
    let new_status =
      ( if (current_condition_status land mask_then) <> 0
        then mask_then
        else
          match th with
            Dataflow.GUse _ | Dataflow.GDefault -> mask_then
          | Dataflow.GUnreachable -> 0) lor
      ( if (current_condition_status land mask_else) <> 0
        then mask_else
        else
          match el with
            Dataflow.GUse _ | Dataflow.GDefault -> mask_else
          | Dataflow.GUnreachable -> 0)
    in
    if new_status <> 0
    then Cil_datatype.Stmt.Hashtbl.replace conditions_table stmt new_status;
    Separate.filter_if stmt thel
end


(*
Local Variables:
compile-command: "make -C ../.."
End:
*)
