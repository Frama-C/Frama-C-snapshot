(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2014                                               *)
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
open Locations
open Abstract_interp
open Bit_utils
open Cvalue
open Value_util
open Eval_exprs

(* Forward reference to [Eval_funs.compute_call] *)
let compute_call_ref = ref (fun _ -> assert false)


let need_cast t1 t2 =
  match unrollType t1, unrollType t2 with
    | (TInt _| TEnum _| TPtr _), (TInt _| TEnum _| TPtr _) | TFloat _, TFloat _
      ->
      (try bitsSizeOf t1 <> bitsSizeOf t2
       with SizeOfError _ -> true)
    | TComp (c1, _, _), TComp (c2, _, _) -> c1 != c2
    | _ -> true


(** Precondition: the type of [v] and the type of [loc_lv] may be different
    only through a truncation or an extension.
    This function will not perform any conversion (float->int, int->float, ...)
    [exp] should not be bottom (for optimization purposes in the caller). *)
  let do_assign_abstract_value ~with_alarms state typ_lv loc_lv v =
    assert (not (Cvalue.V.is_bottom v));
    (* Or one may propagate bottoms uselessly for too long. *)
    let exp = (* truncate the value if the [lv] is too small: this may
                 happen when the [lv] is a bit-field. Otherwise, the
                 cast is explicit thanks to Cil and no truncation is
                 necessary. *)
      try
        (* if it is a bit-field, the size is statically known. *)
        let size = Int_Base.project loc_lv.size in (* TODOBY: ignore this case*)
        try
          ignore (V.project_ival v);
          Eval_op.cast_lval_bitfield typ_lv loc_lv.size v
        with
        | V.Not_based_on_null (* from [project_ival] *) ->
            (* The exp is a pointer: check there are enough bits in
               the bit-field to contain it. *)
            if Int.ge size (Int.of_int (sizeofpointer ())) || V.is_imprecise v
            then v
            else begin
              Value_parameters.result 
		"casting address to a bit-field of %s bits: this is smaller than sizeof(void*)" 
		(Int.to_string size);
              V.topify_arith_origin v
            end
        | Neither_Int_Nor_Enum_Nor_Pointer
            (* from [signof_typeof_lval] *) -> v
      with
      | Int_Base.Error_Top (* from Int_Base.project *) ->
          (* Imprecise location, handled below *) v
    in
    (match loc_lv.loc with
    | Location_Bits.Top (Base.SetLattice.Top, orig) ->
        Value_parameters.result
          "State before degeneration:@\n======%a@\n======="
          Cvalue.Model.pretty state;
	warning_once_current
          "writing at a completely unknown address@[%a@].@\nAborting."
          Origin.pretty_as_reason orig;
        raise Db.Value.Aborted

    | Location_Bits.Top((Base.SetLattice.Set _) as param,orig) ->
        Value_parameters.result ~current:true ~once:true
          "writing somewhere in @[%a@]@[%a@]."
          Base.SetLattice.pretty param
          Origin.pretty_as_reason orig
    
    | Location_Bits.Map _ -> (* everything is normal *) ()
    );
    let exact = valid_cardinal_zero_or_one ~for_writing:true loc_lv in
    let value = Cvalue.Model.add_binding ~with_alarms ~exact state loc_lv exp in
    value


  exception Do_assign_imprecise_copy

  (* Assigns [exp] to [lv] in [state]. [lv_is_volatile] and [typ_lv] are
     information about [lv] that are computed by the caller. [left_loc] is
     one of the locations [lv] evaluates to. Returns [state] modified by
     the assignment, and whether [left_loc] was at least partially valid.
     If [warn_indeterminate] is [true], indetermine values inside [exp] are
     caught, signaled to the user, and removed. *)
  let do_assign_one_loc ~with_alarms clob ~warn_indeterminate state  lv lv_is_volatile typ_lv exp left_loc =
    let state, left_loc =
      if Locations.is_bottom_loc left_loc then
        Model.bottom, left_loc
      else
        Eval_exprs.warn_reduce_by_accessed_loc ~with_alarms
          ~for_writing:true state left_loc lv
    in
    if not (Cvalue.Model.is_reachable state) then (state, false)
    else
    (* First mode, used when [exp] is not a lval, when a conversion is
       needed between [exp] and [lv], or as backup *)
    let default () =
      let state, _, v =
        eval_expr_with_deps_state_subdiv ~with_alarms None state exp
      in
      Locals_scoping.remember_if_locals_in_value clob left_loc v;
      Warn.warn_right_exp_imprecision ~with_alarms lv left_loc v;
      if Cvalue.V.is_bottom v ||
        Locations.is_bottom_loc left_loc  ||
        not (Cvalue.Model.is_reachable state)
      then Cvalue.Model.bottom
      else begin
        CilE.set_syntactic_context (CilE.SyMem lv);
	let v = if lv_is_volatile then Eval_op.light_topify v else v in
        do_assign_abstract_value ~with_alarms state typ_lv left_loc v
      end
    in
    (* More precise copy, in case exp is in fact an lval (and has a known size).
       We copy the entire lval in one operation. This is typically useful for
       struct assignment *)
    let right_is_lval exp_lv =
      (* Copy one location to which [exp_lv] points to, in [state] *)
      let aux_one_loc right_loc state =
        let state, right_loc =
          Eval_exprs.warn_reduce_by_accessed_loc ~with_alarms
            ~for_writing:false state right_loc exp_lv
        in
        (* Warn if right_loc is imprecise *)
        Warn.warn_imprecise_lval_read ~with_alarms
          exp_lv right_loc (* Dummy value:*)V.bottom;
        (* Warn if both sides overlap *)
        Warn.warn_overlap ~with_alarms (lv, left_loc) (exp_lv, right_loc);
        if not (Cvalue.Model.is_reachable state)
        then Cvalue.Model.bottom
        else begin
          (* tested before this function is called, in which case the imprecise
             mode is used *)
          let size = Int_Base.project right_loc.size in
          CilE.set_syntactic_context (CilE.SyMem exp_lv);
          let offsetmap =
            Cvalue.Model.copy_offsetmap ~with_alarms right_loc state
          in
          let offsetmap =
            Extlib.opt_bind
              (fun o ->
                if warn_indeterminate
                then Warn.warn_indeterminate_offsetmap ~with_alarms typ_lv o
                else Some o)
              offsetmap
          in
          match offsetmap with
            | None -> Model.bottom
            | Some offsetmap ->
              assert (not (Cvalue.V_Offsetmap.is_empty offsetmap));
              Locals_scoping.remember_if_locals_in_offsetmap
                clob left_loc offsetmap;
              (* TODO: message "assigning non deterministic value for
                 the first time" *)
              (match Warn.offsetmap_contains_imprecision offsetmap with
                | Some v ->
                  Warn.warn_right_exp_imprecision ~with_alarms lv left_loc v
                | _ -> ());
              CilE.set_syntactic_context (CilE.SyMem lv);
              Cvalue.Model.paste_offsetmap with_alarms
                offsetmap left_loc.loc Int.zero size true state
        end
      in
      if Locations.is_bottom_loc left_loc
        || not (Cvalue.Model.is_reachable state)
      then Model.bottom
      else
        let state, p_right_loc, _ =
          lval_to_precise_loc_state ~with_alarms state exp_lv
        in
        if Model.is_reachable state then
          (* Size mismatch between left and right size, or imprecise size.
             This cannot be done by copies, but require a conversion *)
          let size = Precise_locs.loc_size p_right_loc in
          if not (Int_Base.equal size left_loc.size) || Int_Base.is_top size
          then raise Do_assign_imprecise_copy;
          let aux loc acc_state =
            Model.join acc_state (aux_one_loc loc state)
          in
          Precise_locs.fold aux p_right_loc Model.bottom
        else
          Model.bottom
    in
    let state_res =
      try
        if lv_is_volatile || Eval_op.is_bitfield typ_lv
        then default ()
        else
          (* An lval assignement might be hidden by a dummy cast *)
          let exp_lv = find_lv ~with_alarms state exp in
          right_is_lval exp_lv
      with Cannot_find_lv | Do_assign_imprecise_copy -> default ()
    in
    state_res, not (Locations.is_bottom_loc left_loc)

  (* Assigns [exp] to [lv] in [state] *)
  let do_assign ~with_alarms kf clob state lv exp =
    assert (Cvalue.Model.is_reachable state);
    let state, precise_left_loc, typ_lv =
      lval_to_precise_loc_state ~with_alarms state lv
    in
    let lv_is_volatile = hasAttribute  "volatile" (typeAttrs typ_lv) in
    let warn_indeterminate = Value_util.warn_indeterminate kf in
    let aux_loc loc (acc_state, acc_non_bottom_loc) =
      let state', non_bottom_loc =
        do_assign_one_loc ~with_alarms
          clob ~warn_indeterminate state lv lv_is_volatile typ_lv exp loc
      in
      Model.join acc_state state', non_bottom_loc || acc_non_bottom_loc
    in
    let res, non_bottom_loc =
      Precise_locs.fold aux_loc precise_left_loc (Model.bottom, false)
    in
    if not non_bottom_loc then
      CilE.do_warn with_alarms.CilE.imprecision_tracing
        (fun _ -> Kernel.warning ~current:true ~once:true
          "@[<v>@[all target addresses were invalid. This path is \
              assumed to be dead.@]%t@]" pp_callstack
        );
    res


  exception Too_linear

  let do_assign ~with_alarms kf clob state lv exp =
    if true then do_assign ~with_alarms kf clob state lv exp
    else (* Experimental code that performs automatic splitting when
            the expression is not linear *)
      let vars = get_influential_vars state exp in
      let rec try_sub vars =
        match vars with
        | [] | [ _ ] -> do_assign ~with_alarms kf clob state lv exp
        | v :: tail ->
            try
              if not (List.exists (fun x -> Locations.loc_equal v x) tail)
              then raise Too_linear;
              let value =
                Cvalue.Model.find
                  ~conflate_bottom:true
                  ~with_alarms:CilE.warn_none_mode
                  state
                  v
              in
              if Location_Bytes.is_included value Location_Bytes.top_float
              then raise Too_linear;
              (* any value is possible, provided it is not too hight *)
              let max_card = succ (Ival.get_small_cardinal()) in
              ignore (Cvalue.V.cardinal_less_than value max_card);
(*              Value_parameters.debug
                "subdiv assignment: candidate %a value %a@."
                Locations.pretty v Cvalue.V.pretty value; *)
              let treat_subdiv subvalue acc =
                let sub_state =
                  Cvalue.Model.reduce_previous_binding state v subvalue
                in
                let sub_newstate =
                  do_assign ~with_alarms kf clob sub_state lv exp
                in
                Cvalue.Model.join acc sub_newstate
              in
              Location_Bytes.fold_enum treat_subdiv value Cvalue.Model.bottom
            with
            | Not_less_than | Too_linear -> try_sub tail
            | Location_Bytes.Error_Top -> assert false;
      in
      try_sub vars

  (* This functions stores the result of call, represented by offsetmap
     [return], into [lv]. It is not trivial because we must handle the
     possibility of casts between the type of the result [rettyp] and the type
     of [lv]. With option [-no-collapse-call-cast], we only need the first part
     of the function. This function handles one possible location in [lv]. *)
  let assign_return_to_lv_one_loc ~with_alarms clob rettype (lv, loc, lvtyp) return state =
    let state, loc =
      Eval_exprs.warn_reduce_by_accessed_loc ~with_alarms
        ~for_writing:true state loc lv
    in
    if Locations.is_bottom_loc loc then
      state, false
    else
      let is_bitfield = Eval_op.is_bitfield lvtyp in
      if not (is_bitfield) && not (need_cast lvtyp rettype) then
        (* Direct paste *)
        let size = Int_Base.project loc.size in
        CilE.set_syntactic_context (CilE.SyMem lv);
        let result =
          Cvalue.Model.paste_offsetmap with_alarms
            return loc.loc Int.zero size true state
        in
        Locals_scoping.remember_if_locals_in_offsetmap clob loc return;
        result, false
      else (* Size mismatch. We read then cast the returned value *)
        let size = Int.of_int (bitsSizeOf rettype) in
        let validity = Base.Known (Int.zero, Int.pred size) in
        let value_with_init =
          V_Offsetmap.find
            ~conflate_bottom:false ~validity ~with_alarms:CilE.warn_none_mode
            ~offsets:Ival.zero ~size return
        in
        let flags = V_Or_Uninitialized.get_flags value_with_init in
        let init = V_Or_Uninitialized.is_initialized flags in
        let no_esc = V_Or_Uninitialized.is_noesc flags in
        let value = V_Or_Uninitialized.get_v value_with_init in
        (* Cf. bts #997 and #1024 for the syntactic context below *)
        CilE.set_syntactic_context CilE.SyCallResult;
        let evaled_exp = Eval_op.reinterpret ~with_alarms rettype value in
        if not init then CilE.warn_uninitialized with_alarms;
        if not no_esc then CilE.warn_escapingaddr with_alarms;
        let exact = valid_cardinal_zero_or_one ~for_writing:true loc in
        (* Type of [lv] and [return] might differ, perform a cast (bug #798) *)
        let evaled_exp =
          if is_bitfield
          then Eval_op.cast_lval_bitfield lvtyp loc.size evaled_exp
          else
            let msg fmt =
              Format.fprintf fmt "call result (%a)" V.pretty evaled_exp
            in
            Eval_op.do_promotion ~with_alarms (get_rounding_mode())
              ~src_typ:rettype ~dst_typ:lvtyp evaled_exp msg
        in
        Locals_scoping.remember_if_locals_in_value clob loc evaled_exp;
        CilE.set_syntactic_context (CilE.SyMem lv);
        let res = Model.add_binding ~with_alarms ~exact state loc evaled_exp in
        let failed = Cvalue.V.is_bottom value && not (init && no_esc) in
        res, failed

  (* Same as function above, but for multiple locations. *)
  let assign_return_to_lv ~with_alarms clob rettype (lv, ploc, lvtyp) return state =
    let aux loc (acc_state, acc_failed) =
      let state, failed =
        assign_return_to_lv_one_loc ~with_alarms
          clob rettype (lv, loc, lvtyp) return state
      in
      Model.join acc_state state, acc_failed || failed
    in
    let state, failed = Precise_locs.fold aux ploc (Model.bottom, false) in
    if failed then
      Value_parameters.result ~current:true
        "Function call returned an unspecified value. \
            This path is assumed to be dead.";
    state


  let interp_call ~with_alarms clob stmt lval_to_assign funcexp argl state =
    let cacheable = ref Value_types.Cacheable in
    let call_site_loc = CurrentLoc.get () in
    try
        let functions, _ = resolv_func_vinfo ~with_alarms None state funcexp in
        (* Warn for arguments that contain uninitialized/escaping if:
           - kf is a non-special leaf function (TODO: should we keep this?)
           - the user asked for this *)
        let warn_indeterminate kf = 
	  not 
	    (Kernel_function.is_definition kf (* Should we keep this? *)
	      || let name = Kernel_function.get_name kf in
		 (name >= "Frama_C" && name < "Frama_D")
		 || Builtins.mem_builtin name)
           || Value_util.warn_indeterminate kf
	in
        let warn_indeterminate =
          Kernel_function.Hptset.exists warn_indeterminate functions
        in
        let compute_actual =
          Function_args.compute_actual ~with_alarms ~warn_indeterminate
        in
        let actuals = List.map (compute_actual state) argl in
        (* TODO: check that lval_to_assign is not modified during the call:
           evaluate its dependencies here, and intersect them with the outs of
           the called function. The code below is not sound. *)
(*
        let to_assign, state = match lval_to_assign with
          | None -> None, state
          | Some lv ->
              let state', ploc, typlv =
                lval_to_precise_loc_state ~with_alarms state lv
              in
              let state', loc' =
                Eval_exprs.warn_reduce_by_accessed_loc ~with_alarms
                  ~for_writing:true state' loc lv
              in
              if Model.is_reachable state' then
                Some (lv, loc', typlv), state'
              else (* Ignore reduction, attempt the call, which might result
                      in a warning if the value of lv changes during the call *)
                Some (lv, loc, typlv), state
        in *)
        let caller = current_kf (), stmt in
        (* Remove bottom state from results, assigns result to retlv *)
        let treat_one_result res (return, state) =
          if not (Cvalue.Model.is_reachable state)
          then res
          else
            match lval_to_assign with
              | None -> state :: res
              | Some lv ->
                let state, ploc, typlv =
                  lval_to_precise_loc_state ~with_alarms state lv
                in
                (* See comments above.
                Warn.warn_modified_result_loc with_alarms kf locret state lvret;
*)
                let return = 
		  ( match return with
		    None ->
		      Value_parameters.abort ~current:true ~once:true 
			"Return value expected but none present. Did you misuse a builtin?"
		  | Some return -> return )
		in
                let rettype = getReturnType (typeOf funcexp) in  
                let state =
                  assign_return_to_lv ~with_alarms
                    clob rettype (lv, ploc, typlv) return state
                in
                state :: res
        in
        let treat_one_function f acc_rt_res =
	  try
            Kf_state.add_caller f ~caller;
            let call_kinstr = Kstmt stmt in
            let res = !compute_call_ref f ~call_kinstr state actuals in
            CurrentLoc.set call_site_loc; (* Changed by compute_call_ref *)
            if res.Value_types.c_cacheable = Value_types.NoCacheCallers then
              (* Propagate info that callers cannot be cached either *)
              cacheable := Value_types.NoCacheCallers;
            Locals_scoping.remember_bases_with_locals
              clob res.Value_types.c_clobbered;
            List.fold_left treat_one_result acc_rt_res res.Value_types.c_values
	  with
            | Function_args.WrongFunctionType ->
                warning_once_current
                  "Function type must match type at call site: \
                     assert(function type matches)";
                Value_util.stop_if_stop_at_first_alarm_mode ();
                acc_rt_res
        in
        let results =
          Kernel_function.Hptset.fold treat_one_function functions []
        in
        if results <> [] then Value_results.mark_call_terminating stmt;
        results, !cacheable
      with
        | Function_args.Actual_is_bottom -> (* from compute_actual *)
            CurrentLoc.set call_site_loc;
            [], !cacheable


  exception AlwaysOverlap

  let check_non_overlapping state lvs1 lvs2 =
    let conv lv =
      let loc = lval_to_precise_loc ~with_alarms:CilE.warn_none_mode state lv in
      let for_writing = false in
      let exact = lazy (Precise_locs.cardinal_zero_or_one ~for_writing loc) in
      let z = Precise_locs.enumerate_valid_bits ~for_writing loc in
      lv, exact, z
    in
    let l1 = List.map conv lvs1 in
    let l2 = List.map conv lvs2 in
    List.iter
      (fun (lv1, exact1, z1) ->
         List.iter
           (fun (lv2, exact2, z2) ->
              if Locations.Zone.intersects z1 z2 then begin
                CilE.set_syntactic_context (CilE.SySep(lv1, lv2));
                CilE.warn_separated warn_all_mode;
                if Lazy.force exact1 && Lazy.force exact2 then
                  raise AlwaysOverlap
              end;
           )
           l2)
      l1

  (* Not currently taking advantage of calls information. But see
     plugin Undefined Order by VP. *)
  let check_unspecified_sequence state seq =
    let rec check_one_stmt ((stmt1,_,writes1,_,_) as my_stmt) = function
        [] -> ()
      | (stmt2,_,_,_,_)::seq when stmt1 == stmt2 -> check_one_stmt my_stmt seq
      | (stmt2,modified2,writes2,reads2,_) :: seq ->
          (* Values that cannot be read, as they are modified in the statement
             (but not by the whole sequence itself) *)
          let unauthorized_reads =
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


  (* Remove locals from the given, and extract the content of \result *)
  let externalize ~with_alarms kf ~return_lv clob =
    let fundec = Kernel_function.get_definition kf in
    let offsetmap_top_addresses_of_locals, state_top_addresses_of_locals =
      Locals_scoping.top_addresses_of_locals fundec clob
    in
    fun state ->
      let state, ret_val =
        match return_lv with
          | None ->
            state, None
          | Some lv ->
            let typ_ret = Cil.typeOfLval lv in
            let _loc, state, oret = 
              Eval_exprs.offsetmap_of_lv ~with_alarms state lv
            in
            match oret with
              | None ->
                assert (Model.equal Model.bottom state);
                state, None
              | Some oret ->
                CilE.set_syntactic_context (CilE.SyMem lv);
                let o =
                  if Value_util.warn_indeterminate kf then
                    Warn.warn_indeterminate_offsetmap ~with_alarms typ_ret oret
                  else Some oret
                in
                match o with
                  | None -> (* Completely indeterminate return *)
                    Model.bottom, None
                  | Some ret_val ->
                    let locals, r = offsetmap_top_addresses_of_locals ret_val in
                    if not (Cvalue.V_Offsetmap.equal r ret_val) then
                      Warn.warn_locals_escape_result fundec locals;
                    state, Some r
      in
      let state = Cvalue.Model.uninitialize_formals_locals fundec state in
      let state = state_top_addresses_of_locals state in
      ret_val, state    


(*
Local Variables:
compile-command: "make -C ../.."
End:
*)
