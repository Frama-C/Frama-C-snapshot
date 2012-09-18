(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2012                                               *)
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
open Eval_terms
open Locals_scoping


exception Recursive_call

let need_cast t1 t2 =
  match unrollType t1, unrollType t2 with
    | (TInt _| TEnum _| TPtr _), (TInt _| TEnum _| TPtr _)
    | TFloat _, TFloat _
    | TComp _, TComp _ ->
        (try bitsSizeOf t1 <> bitsSizeOf t2
         with SizeOfError _ -> true)
    | _ -> true


 (* Forward reference to [Eval_funs.compute_call] *)
 let compute_call_ref = ref (fun _ -> assert false)

 module Computer
   (AnalysisParam:sig
     val kf: kernel_function
     val slevel: int
     val initial_states : State_set.t
     val active_behaviors: Eval_annots.ActiveBehaviors.t
     val local_slevel_info : Local_slevel_types.local_slevel_info
     end) =

 struct
   let debug = ref false
   let name = "Values analysis"

   let current_kf = AnalysisParam.kf
   let current_fundec = Kernel_function.get_definition current_kf
   let return = Kernel_function.find_return current_kf
   let return_lv =
     match return.skind with
       | Return (Some ({enode = Lval lv}),_) -> Some lv
       | Return (None,_) -> None
       | _ -> assert false (* Cil invariant *)


   let stmt_can_reach = Value_util.stmt_can_reach current_kf
   let is_natural_loop = Loop.is_natural current_kf

   let obviously_terminates = 
     Value_parameters.ObviouslyTerminatesAll.get() (* TODO: by function *)      

   let slevel = 
     if obviously_terminates
     then max_int
     else
       AnalysisParam.slevel

   let fused_initial_state = lazy (State_set.join AnalysisParam.initial_states)

   let current_table = Current_table.create ()

   let states_after = Cil_datatype.Stmt.Hashtbl.create 5

   (* During the dataflow analysis, if required by a callback, we store the
      state after a statement, but only if either the following conditions 
      is met ([succ] being a successor of [s])
      - [s] is an instr (the control flow statements such as [goto] and [if]
	do not change the state (union of the states in the case of if))
	AND there is a control-flow join on [succ]
      - [s] is the last instruction of a block that contains
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
	  let ret = Kernel_function.find_return current_kf in
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
	 if Value_parameters.ValShowProgress.get () then
	   Value_parameters.feedback
	     "now calling Record_Value_Superposition callbacks";
	 Db.Value.Record_Value_Superposition_Callbacks.apply
	   (stack_for_callbacks, current_superpositions);
       end ;

       if not (Db.Value.Record_Value_Callbacks.is_empty ())
       then begin
	 if Value_parameters.ValShowProgress.get () then
	   Value_parameters.feedback "now calling Record_Value callbacks";
	 Db.Value.Record_Value_Callbacks.apply
	   (stack_for_callbacks, superposed)
       end;

       if not (Db.Value.Record_Value_Callbacks_New.is_empty ())
       then begin
	 if Value_parameters.ValShowProgress.get () then
	   Value_parameters.debug ~dkey:"callbacks"
             "now calling Record_Value_New callbacks";
	 Db.Value.Record_Value_Callbacks_New.apply
          (stack_for_callbacks,
           Value_aux.NormalStore (superposed, (Mem_exec.new_counter ())))

       end;

       if not (Db.Value.Record_Value_After_Callbacks.is_empty ())
       then begin
	 if Value_parameters.ValShowProgress.get () then
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
	     && Value_parameters.ShowSlevel.get()
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
   let remember_bases_with_locals_offm =
     remember_bases_with_locals_offsetmap bases_containing_locals

   let top_addresses_of_locals fundec =
     let entry_point = Globals.entry_point () in
     if snd entry_point (* lib *) ||
       current_kf != fst entry_point (* not entry point *)
     then
       let offsetmap_top_addresses_of_locals =
	 offsetmap_top_addresses_of_locals
	   (Cilutil.swap Base.is_formal_or_local fundec)
       in
       let state_top_addresses_of_locals =
	 state_top_addresses_of_locals 
	   (warn_locals_escape false fundec)
	   offsetmap_top_addresses_of_locals !bases_containing_locals
       in
       (offsetmap_top_addresses_of_locals ~exact:true,
        state_top_addresses_of_locals ~exact:true)
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
	 state_top_addresses_of_locals
	   (warn_locals_escape true current_fundec)
	   offsetmap_top_addresses_of_locals
	   !bases_containing_locals
       in
       state_top_addresses_of_locals ~exact:true


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
    (match loc_lv.loc with
    | Location_Bits.Top (Location_Bits.Top_Param.Top, orig) ->
        Value_parameters.result
          "State before degeneration:@\n======%a@\n======="
          Cvalue.Model.pretty state;
	warning_once_current
          "writing at a completely unknown address@[%a@].@\nAborting."
          Origin.pretty_as_reason orig;
        do_degenerate (Some lv)

    | Location_Bits.Top((Location_Bits.Top_Param.Set _) as param,orig) ->
        Value_parameters.result ~current:true ~once:true
          "writing somewhere in @[%a@]@[%a@]."
          Location_Bits.Top_Param.pretty param
          Origin.pretty_as_reason orig
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
    let state, loc_lv = lval_to_loc_state ~with_alarms state lv in
    let exp =
      if hasAttribute  "volatile" (typeAttrs (typeOfLval lv))
      then V.top_int
      else exp
    in
    remember_bases_with_locals loc_lv exp;
    CilE.set_syntactic_context (CilE.SyMem lv);
    do_assign_abstract_value_to_loc ~with_alarms state lv loc_lv exp


  (* Auxiliary function for [do_assign] below. When computing the
     result of [lv = exp], warn if the evaluation of [exp] results in
     an imprecision. [loc_lv] is the location pointed to by [lv].
     [exp_val] is the part of the evaluation of [exp] that is imprecise. *)
  let warn_right_exp_imprecision ~with_alarms lv loc_lv exp_val =
    match with_alarms.CilE.imprecision_tracing with
      | CilE.Aignore -> ()
      | CilE.Acall f -> f ()
      | CilE.Alog _ ->
          match exp_val with
            | Cvalue.V.Top(_topparam,origin) ->
                Value_parameters.result ~once:true ~current:true
                  "@[<v>@[Assigning imprecise value to %a%t.@]%a%t@]"
                  !Ast_printer.d_lval lv
                  (fun fmt -> match lv with
                    | (Mem _, _) ->
                        Format.fprintf fmt "@ (i.e. %a)" Locations.pretty loc_lv
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
                  not (Cvalue.V.cardinal_zero_or_one exp_val)
                then begin
                  Got_Imprecise_Value.set true;
                  if (Value_parameters.ValShowProgress.get())
                  then
                    Value_parameters.result ~current:true
                      "assigning non deterministic value for the first time";
                end

  (* Auxiliary function for do_assign (currently), that warns when the
     left-hand side and the right-hand side of an assignment overlap *)
  let warn_overlap ~with_alarms (lv, left_loc) (exp_lv, right_loc) =
    if with_alarms.CilE.others != CilE.Aignore then
      match right_loc.size with
        | Int_Base.Value size when Int.to_int size > Cil.bitsSizeOf intType ->
    	    if Location_Bits.partially_overlaps size right_loc.loc left_loc.loc
	    then begin
              match with_alarms.CilE.others with
                | CilE.Aignore -> assert false
                | CilE.Acall f -> f ()
                | CilE.Alog(_, suffix) ->
  	            warning_once_current
                      "Partially overlapping lvalue assignment \"%a=%a;\". \
                         Left address in bits: %a. Right address in bits: %a. \
                         assert(separated or same)@ %t"
	              !d_lval lv !d_lval exp_lv
	              Location_Bits.pretty left_loc.loc
                      Location_Bits.pretty right_loc.loc
                      suffix;
            end
        | _ -> ()


  (* Returns an eventual imprecised part contained in an offsetmap *)
  let offsetmap_contains_imprecision offs size =
    let module Exn = struct exception Got_imprecise of Cvalue.V.t end in
    try
      Cvalue.V_Offsetmap.iter_contents
        (fun v ->
          match Cvalue.V_Or_Uninitialized.get_v v with
            | Location_Bytes.Map _ -> ()
            | Location_Bytes.Top _ as v -> raise (Exn.Got_imprecise v)
        ) offs size;
      None
    with Exn.Got_imprecise v -> Some v


  (* Assigns [exp] to [lv] in [state] *)
  let do_assign ~with_alarms state lv exp =
    assert (Cvalue.Model.is_reachable state);
    let module Exn = struct exception Do_assign_default end in
    let state, left_loc = lval_to_loc_state ~with_alarms state lv in
    let state = Eval_exprs.reduce_by_accessed_loc ~with_alarms
      ~for_writing:true state left_loc lv
    in
    if not (Cvalue.Model.is_reachable state) then state
    else
    (* First mode, used when [exp] is not a lval, when a conversion is
       needed between [exp] and [lv], or as backup *)
    let default () =
      let state, _, exp_v =
        eval_expr_with_deps_state_subdiv ~with_alarms None state exp
      in
      remember_bases_with_locals left_loc exp_v;
      warn_right_exp_imprecision ~with_alarms lv left_loc exp_v;
      if Cvalue.V.is_bottom exp_v ||
        Location_Bits.equal left_loc.loc Location_Bits.bottom  ||
        not (Cvalue.Model.is_reachable state)
      then Cvalue.Model.bottom
      else begin
        CilE.set_syntactic_context (CilE.SyMem lv);
	  let exp_v =
	    if hasAttribute  "volatile" (typeAttrs (typeOfLval lv))
	    then V.top_int
	    else exp_v
	  in
        do_assign_abstract_value_to_loc ~with_alarms state lv left_loc exp_v
      end
    in

    (* More precise copy, in case exp is in fact an lval. We copy the entire
       lval in one operation. This is typically useful for struct assignment *)
    let right_is_lval exp_lv =
      if Location_Bits.equal left_loc.loc Location_Bits.bottom ||
        not (Cvalue.Model.is_reachable state)
      then Model.bottom
      else
        let state, right_loc = lval_to_loc_state ~with_alarms state exp_lv in
        let state =
	  Eval_exprs.reduce_by_accessed_loc ~for_writing:false ~with_alarms
	    state right_loc exp_lv
        in

        (* Size mismatch between left and right size. This cannot be done by
           copies, but require a conversion *)
        if not (Int_Base.equal right_loc.size left_loc.size) then
          raise Exn.Do_assign_default;

        (* Warn if right_loc is imprecise *)
        Eval_exprs.warn_imprecise_lval_read ~with_alarms
          exp_lv right_loc (* Dummy value:*)V.bottom;
        (* Warn if both sides overlap *)
        warn_overlap ~with_alarms (lv, left_loc) (exp_lv, right_loc);

        if Location_Bits.equal left_loc.loc Location_Bits.bottom ||
          not (Cvalue.Model.is_reachable state)
        then Cvalue.Model.bottom
        else begin
          match right_loc.size with
            | Int_Base.Value size ->
                CilE.set_syntactic_context (CilE.SyMem exp_lv);
                let offsetmap = Cvalue.Model.copy_offsetmap ~with_alarms
                  right_loc state in
                begin match offsetmap with
                  | None -> Model.bottom
                  | Some offsetmap ->
                      assert (not (Cvalue.V_Offsetmap.is_empty offsetmap));
                      remember_bases_with_locals_offm left_loc offsetmap size;
                      (* TODO: message "assigning non deterministic value for
                         the first time" *)
                      (match offsetmap_contains_imprecision offsetmap size with
                        | Some v ->
                           warn_right_exp_imprecision ~with_alarms lv left_loc v
                        | _ -> ());
                      CilE.set_syntactic_context (CilE.SyMem lv);
                      Cvalue.Model.paste_offsetmap with_alarms
                        offsetmap left_loc.loc Int.zero size true state
                end
	    | Int_Base.Bottom | Int_Base.Top -> assert false
        end
    in
    try
      if is_bitfield lv ~sizebf:left_loc.size ()
	|| (hasAttribute  "volatile" (typeAttrs (typeOfLval lv)))
      then default ()
      else
        (* An lval assignement might be hidden by a dummy cast *)
        let lv = find_lv ~with_alarms state exp in
        right_is_lval lv
    with Cannot_find_lv | Exn.Do_assign_default -> default ()


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
    let new_state, loc = lval_to_loc_state ~with_alarms new_state lv in
    if Model.equal Model.bottom new_state then
      new_state
    else
      let rtype = getReturnType (typeOf funcexp) in
      let lvtyp = typeOfLval lv in
      let is_bitfield =
        is_bitfield lv ~sizebf:loc.size ~sizelv:(sizeof lvtyp) ()
      in
      if not (is_bitfield) && not (need_cast lvtyp rtype) then
        (* Direct paste *)
        let size = Int_Base.project loc.size in
        CilE.set_syntactic_context (CilE.SyMem lv);
        let result =
          Cvalue.Model.paste_offsetmap with_alarms
            return loc.loc Int.zero size true new_state
        in
        remember_bases_with_locals_offm loc return size;
        result
      else (* Size mismatch. We read then cast the returned value *)
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
        (* Cf. bts #997 and #1024 for the syntactic context below *)
        CilE.set_syntactic_context CilE.SyCallResult;
        if not init then CilE.warn_uninitialized with_alarms;
        if not no_esc then CilE.warn_escapingaddr with_alarms;
        if Cvalue.V.is_bottom value && not (init && no_esc)
        then
          Value_parameters.result ~current:true
            "Function call returned an unspecified value. \
                This path is assumed to be dead.";
        let exact = valid_cardinal_zero_or_one ~for_writing:true loc in
        let evaled_exp = do_cast ~with_alarms rtype value
          (* fix http://bts.frama-c.com/view.php?id=798 *) in
        let evaled_exp =
          if is_bitfield
          then cast_lval_bitfield lv (Int_Base.project loc.size) evaled_exp
          else do_cast ~with_alarms lvtyp evaled_exp
        in
        remember_bases_with_locals loc evaled_exp;
        CilE.set_syntactic_context (CilE.SyMem lv);
        Cvalue.Model.add_binding ~with_alarms ~exact new_state loc evaled_exp

  let interp_call stmt lval_to_assign funcexp argl d_value =
    let call_site_loc = CurrentLoc.get () in
    let with_alarms = warn_all_quiet_mode () in
    let state_after_call state =
      try
        let functions, _ = resolv_func_vinfo ~with_alarms None state funcexp in
        let is_library_function kf = 
	  not 
	    (Kernel_function.is_definition kf
	      || let name = Kernel_function.get_name kf in
		 (name >= "Frama_C" && name < "Frama_D")
		 || Builtins.mem_builtin name)	    
	in
        let calling_at_least_one_library_function =
          Kernel_function.Hptset.exists is_library_function functions
        in
	if calling_at_least_one_library_function && 
	  Value_parameters.InterpreterMode.get()
	then begin
	    warning_once_current "Library function call. Stopping.";
	    exit 0
	  end;
        let calling_only_library_functions =
          calling_at_least_one_library_function &&
            (Kernel_function.Hptset.for_all is_library_function functions)
        in
        let compute_actual = Function_args.compute_actual ~with_alarms
          (calling_at_least_one_library_function,
           calling_only_library_functions)
        in
        let actuals = List.map (compute_actual state) argl in

        let treat_one_call f acc_rt_res =
	  try
            let results, clobbered_set =
              !compute_call_ref f ~call_kinstr:(Kstmt stmt) state actuals
            in
            CurrentLoc.set call_site_loc; (* Changed by compute_call_ref *)
            let caller = current_kf, stmt in
            Kf_state.add_caller f ~caller;

            bases_containing_locals :=
              Location_Bits.Top_Param.join !bases_containing_locals
              clobbered_set;
            results @ acc_rt_res;

	  with
            | Recursive_call ->
                (match lval_to_assign with
                  | None ->  [None, state]
                  | Some lv ->
                      let v = V.top_leaf_origin () in
                      let v = Cvalue.V_Or_Uninitialized.initialized v in
                      let size = Int_Base.project (Bit_utils.sizeof_lval lv) in
                      let offsm = Cvalue.V_Offsetmap_ext.create_initial ~v
                        ~modu:size in
                      [Some offsm, state])
            | Function_args.WrongFunctionType ->
                warning_once_current
                  "Function type must match type at call site: \
                     assert(function type matches)";
                Value_util.stop_if_stop_at_first_alarm_mode ();
                acc_rt_res
        in

        let results = Kernel_function.Hptset.fold treat_one_call functions [] in

        let treat_one_result filtered (return, new_state) =
          if not (Cvalue.Model.is_reachable new_state)
          then filtered
          else
            match lval_to_assign with
              | None -> new_state :: filtered
              | Some lv ->
                let new_state =
                  match return with
                    | Some return ->
                        assign_return_to_lv ~with_alarms
                          funcexp lv return new_state
                    | None -> assert false
                in
                new_state :: filtered
        in
        List.fold_left treat_one_result [] results

      with
        | Function_args.Actual_is_bottom -> (* from compute_actual *)
            CurrentLoc.set call_site_loc;
            []
    in
    State_set.fold
      (fun acc state ->
        let results = state_after_call state in
        List.fold_left (fun acc state -> State_set.add state acc) acc results)
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
          let with_alarms = warn_all_quiet_mode () in
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
                (fun state_value -> do_assign ~with_alarms state_value lv exp)
          | Call (lval_to_assign,
                 {enode = Lval (Var {vname=("__builtin_va_start"|"__builtin_va_arg"|"__builtin_va_end" as _builtin_name) },NoOffset)},
                 [{enode = Lval lv}],_loc) ->
(*            Format.printf "builtin: %s@." _builtin_name; *)
              apply_each_state
                (fun state ->
                  let state = do_assign_abstract_value
                    ~with_alarms state lv V.top_int
                  in
                  ( match lval_to_assign with
                    None -> state
                  | Some lval_assign ->
                      do_assign_abstract_value
                        ~with_alarms state lval_assign V.top_int))
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

  (* This function is later used to insert a stmt to the worklist manually.
   * Needed for manual split/merge zones
   * Will be filled by Local_slevel_compute.compute_sub_function
   * after Dataflow.Forwards initialization
   *)
  let add_to_worklist = ((ref (fun _ -> assert false)) : (stmt -> unit) ref)

  let doStmt (s: stmt) (d: t) =

    let states = d.value in
    d.value <- State_set.empty;
    let kinstr = Kstmt s in

    (* This function handles local_slevel. *)
    let ret =
      match Local_slevel.determine_mode
        current_kf
        s
        AnalysisParam.local_slevel_info
      with
        | Local_slevel_types.Normal -> fun x -> x
        | Local_slevel_types.Merge ->
            fun _ -> Dataflow.SDone
        (* FIXME [SCM] strict mode only - will have to work in split mode as
         * well f.e. while(1) { foo() } as split and merge stmt *)
        | Local_slevel_types.MergeSplit _ -> assert false
        | Local_slevel_types.Split info ->
            let new_state, clobbered_set =
              Local_slevel.compute_sub_function
                current_kf
                s
                info
                states
            in
            bases_containing_locals := Locations.Location_Bits.Top_Param.join
              clobbered_set
              !bases_containing_locals;
            List.iter (fun stmt ->
              StmtStartData.add stmt { counter_unroll = 0
                                     ; value = State_set.singleton new_state };
              !add_to_worklist stmt)
            (* FIXME [SCM] strict mode *)
            (Cil_datatype.Stmt.Hptset.elements
            info.Local_slevel_types.merges);
            (*(List.hd (Cil_datatype.Stmt.Hptset.elements
            info.Local_slevel_types.merges)).succs;*)
            fun _ -> Dataflow.SDone
    in

    if State_set.is_empty states
    then
      ret Dataflow.SDefault
    else
      let annots_before =
        Annotations.fold_code_annot
          (fun _ a acc -> match a with
            | User { annot_content = AStmtSpec _ }
            | AI (_,{annot_content = AStmtSpec _ }) -> acc
            | AI (_, b) | User b -> b :: acc)
          s
          []
      in
      CilE.start_stmt kinstr;
      let interp_annot = Eval_annots.interp_annot
        current_kf !!fused_initial_state slevel AnalysisParam.active_behaviors
      in
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
      then ret Dataflow.SDefault
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
		if is_natural_loop s && curr_wcounter = 0 then
                  let wh_key_set, wh_hints = Widen.getWidenHints current_kf s in
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
              (Value_parameters.ShowSlevel.get())
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
        ret (Dataflow.SUse d)

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

  (* Get access to current_table in case of split/merge zone.
   * Without explicit merging, this is done via externalize
   *)
  let getStateSet stmt = Current_table.find_superposition current_table stmt

  let mergeResults () =
    if Value_parameters.ValShowProgress.get() then
      Value_parameters.feedback "Recording results for %a"
        Kernel_function.pretty AnalysisParam.kf;
    merge_current ~degenerate:false

  (* Check that the dataflow is indeed finished *)
  let checkConvergence () =
    StmtStartData.iter (fun k v ->
      if not (State_set.is_empty (v.value)) then
        Value_parameters.fatal "sid:%d@\n%a@\n" k.sid State_set.pretty v.value)

  (* Final states of the function, reduced by the post-condition *)
  let finalStates () =
    if !debug then checkConvergence ();
    let states = Current_table.find_superposition current_table return in
    (* Reduce final states according to the function postcondition *)
    let result = match return_lv with
      | Some (Var v, NoOffset) -> Some v
      | Some _ -> assert false
      | None -> None
    in
    Eval_annots.check_fct_postconditions ~result current_kf
      ~init_state:AnalysisParam.initial_states
      ~active_behaviors:AnalysisParam.active_behaviors
      ~post_state:states
      Normal (* termination kind*)

  (* Remove locals from the given, and extract the content of \result *)
  let externalizeOneState state =
    let state, ret_val =
      match return_lv with
        | Some lv ->
            Eval_exprs.offsetmap_of_lv
              ~with_alarms:(warn_all_quiet_mode ()) state lv
        | None -> state, None
    in
    let state = Cvalue.Model.clear_state_from_locals current_fundec state in
    let offsetmap_top_addresses_of_locals, state_top_addresses_of_locals =
      top_addresses_of_locals current_fundec
    in
    let ret_val = match ret_val with
      | None -> ret_val
      | Some ret_val ->
        let locals, r = offsetmap_top_addresses_of_locals ret_val in
        if not (Cvalue.V_Offsetmap.equal r ret_val) then
          warn_locals_escape_result current_fundec locals;
        Some r
    in
    ret_val,
    state_top_addresses_of_locals state

  let externalize states =
    let states =
      Split_return.join_final_states current_kf ~lv_return:return_lv states in
    let res = List.map externalizeOneState states in
    res, !bases_containing_locals


  let doGuardOneCond stmt exp t =
    if State_set.is_empty (t.value)
    then Dataflow.GUnreachable
    else begin
        CilE.start_stmt (Kstmt stmt);
        let with_alarms = warn_all_quiet_mode () in
        let new_values =
          State_set.fold
            (fun acc state ->
              let state, _, test =
                eval_expr_with_deps_state None ~with_alarms state exp
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
  let mask_both = mask_then lor mask_else

  let doGuard stmt exp t =
    let not_exp = new_exp ~loc:exp.eloc (UnOp(LNot, exp, intType)) in
    let th, el as thel =
      doGuardOneCond stmt exp t, doGuardOneCond stmt not_exp t
    in
    let th_reachable =
      match th with
        Dataflow.GUse _ | Dataflow.GDefault -> mask_then
      | Dataflow.GUnreachable -> 0
    in
    let el_reachable =
      match el with
        Dataflow.GUse _ | Dataflow.GDefault -> mask_else
      | Dataflow.GUnreachable -> 0
    in
    let reachable = th_reachable lor el_reachable in
    if Value_parameters.InterpreterMode.get() && (reachable = mask_both)
    then begin
	warning_once_current "Do not know which branch to take. Stopping.";
	exit 0
      end;
    let current_condition_status =
      try
        Cil_datatype.Stmt.Hashtbl.find conditions_table stmt
      with Not_found -> 0
    in
    let new_status = 
      current_condition_status lor reachable 
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
