(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2015                                               *)
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
open Cvalue
open Value_util
open Eval_exprs

(* Forward reference to [Eval_funs.compute_call] *)
let compute_call_ref = ref (fun _ -> assert false)


  exception Do_assign_imprecise_copy

  (* Assigns [exp] to [lv] in [state]. [typ_lv] is the type if [lv]. [left_loc]
     is one of the locations [lv] evaluates to. Returns [state] modified by
     the assignment, and whether [left_loc] was at least partially valid.
     If [warn_indeterminate] is [true], indetermine values inside [exp] are
     caught, signaled to the user, and removed. *)
  let do_assign_one_loc ~with_alarms clob ~warn_indeterminate state lv typ_lv exp left_loc =
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
        Eval_non_linear.eval_expr_with_deps_state ~with_alarms None state exp
      in
      Locals_scoping.remember_if_locals_in_value clob left_loc v;
      Warn.warn_right_exp_imprecision ~with_alarms lv left_loc v;
      if Cvalue.V.is_bottom v ||
        Locations.is_bottom_loc left_loc  ||
        not (Cvalue.Model.is_reachable state)
      then Cvalue.Model.bottom
      else Eval_op.write_abstract_value ~with_alarms state lv typ_lv left_loc v
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
          (* top size is tested before this function is called, in which case
             the imprecise copy mode is used *)
          let size = Int_Base.project right_loc.size in
          Valarms.set_syntactic_context (Valarms.SyMem exp_lv);
          let offsetmap =
            Eval_op.copy_offsetmap ~with_alarms right_loc.loc size state
          in
          let make_volatile = 
            typeHasQualifier "volatile" typ_lv  ||
            typeHasQualifier "volatile" (Cil.typeOfLval exp_lv)
          in
          let offsetmap_state = match offsetmap with
            | `Map o ->
              let o =
                (* TODO: this is the good place to handle partially volatile
                   struct, whether as source or destination *)
                if make_volatile then begin
                  V_Offsetmap.map_on_values
                    (V_Or_Uninitialized.map Eval_op.make_volatile) o
                end else o
              in
              if not (Eval_typ.offsetmap_matches_type typ_lv o) then
                raise Do_assign_imprecise_copy;
              (* Warn for unitialized/escaping addresses. May return bottom
                 when a part of the offsetmap contains no value. *)
              if warn_indeterminate then
                Warn.warn_reduce_indeterminate_offsetmap
                  ~with_alarms typ_lv o (`Loc right_loc) state
              else `Res (o, state)
            | `Top -> Warn.warn_top ();
            | `Bottom -> `Bottom
          in
          match offsetmap_state with
            | `Bottom -> Model.bottom
            | `Res (offsetmap, state) ->
              Locals_scoping.remember_if_locals_in_offsetmap
                clob left_loc offsetmap;
              (match Warn.offsetmap_contains_imprecision offsetmap with
                | Some v ->
                  Warn.warn_right_exp_imprecision ~with_alarms lv left_loc v
                | _ -> ());
              Valarms.set_syntactic_context (Valarms.SyMem lv);
              Eval_op.paste_offsetmap ~reducing:false ~with_alarms
                ~from:offsetmap ~dst_loc:left_loc.loc ~size ~exact:true state
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
        if Eval_typ.is_bitfield typ_lv
        then default ()
        else
          (* An lval assignement might be hidden by a dummy cast *)
          let exp_lv = find_lv state exp in
          right_is_lval exp_lv
      with Cannot_find_lv | Do_assign_imprecise_copy -> default ()
    in
    state_res, not (Locations.is_bottom_loc left_loc)

  (* Evaluate a location with the intent of writing in it. Signal an error
     if the lvalue is constant *)
  let lval_to_precise_loc_state_for_writing ~with_alarms state lv =
    let (_, _, typ as r) = lval_to_precise_loc_state ~with_alarms state lv in
    if Value_util.is_const_write_invalid typ then begin
      Valarms.set_syntactic_context (Valarms.SyMem lv);
      Valarms.warn_mem_write with_alarms;
      Model.bottom, Precise_locs.loc_bottom, typ
    end else
      r

  (* Assigns [exp] to [lv] in [state] *)
  let do_assign ~with_alarms kf clob state lv exp =
    assert (Cvalue.Model.is_reachable state);
    let state, precise_left_loc, typ_lv =
      lval_to_precise_loc_state_for_writing ~with_alarms state lv
    in
    let warn_indeterminate = Value_util.warn_indeterminate kf in
    let aux_loc loc (acc_state, acc_non_bottom_loc) =
      let state', non_bottom_loc =
        do_assign_one_loc ~with_alarms
          clob ~warn_indeterminate state lv typ_lv exp loc
      in
      Model.join acc_state state', non_bottom_loc || acc_non_bottom_loc
    in
    let res, non_bottom_loc =
      Precise_locs.fold aux_loc precise_left_loc (Model.bottom, false)
    in
    if not non_bottom_loc then
      Valarms.do_warn with_alarms.CilE.imprecision_tracing
        (fun () -> Kernel.warning ~current:true ~once:true
          "@[<v>@[all target addresses were invalid. This path is \
              assumed to be dead.@]%t@]" pp_callstack
        );
    res

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
      state
    else
      if not (Eval_typ.is_bitfield lvtyp) &&
         not (Eval_typ.need_cast lvtyp rettype)
      then
        (* Direct paste *)
        let size = Int_Base.project loc.size in
        Valarms.set_syntactic_context (Valarms.SyMem lv);
        let result =
          Eval_op.paste_offsetmap ~with_alarms ~reducing:false
            ~from:return ~dst_loc:loc.loc ~size ~exact:true state
        in
        Locals_scoping.remember_if_locals_in_offsetmap clob loc return;
        result
      else (* Size mismatch. We read then cast the returned value *)
        let size = Int.of_int (bitsSizeOf rettype) in
        let validity = Base.Known (Int.zero, Int.pred size) in
        let alarm, value_with_init =
          V_Offsetmap.find ~validity ~offsets:Ival.zero ~size return
        in
        if alarm then Valarms.warn_mem_read with_alarms;
        let value = V_Or_Uninitialized.get_v value_with_init in
        (* Cf. bts #997 and #1024 for the syntactic context below *)
        Valarms.set_syntactic_context Valarms.SyCallResult;
        let evaled_exp = Eval_op.reinterpret ~with_alarms rettype value in
        ignore (Warn.maybe_warn_indeterminate ~with_alarms value_with_init);
        (* Type of [lv] and [return] might differ, perform a cast (bug #798) *)
        let v_exp =
          let msg fmt =
            Format.fprintf fmt "call result (%a)" V.pretty evaled_exp
          in
          Eval_op.do_promotion ~with_alarms (get_rounding_mode())
            ~src_typ:rettype ~dst_typ:lvtyp evaled_exp msg
        in
        Locals_scoping.remember_if_locals_in_value clob loc v_exp;
        Eval_op.write_abstract_value ~with_alarms state lv lvtyp loc v_exp

  (* Same as function above, but for multiple locations. *)
  let assign_return_to_lv ~with_alarms clob rettype (lv, ploc, lvtyp) return state =
    let aux loc acc_state =
      let state =
        assign_return_to_lv_one_loc ~with_alarms
          clob rettype (lv, loc, lvtyp) return state
      in
      Model.join acc_state state
    in
    Precise_locs.fold aux ploc Model.bottom

  (** This function unbinds [formals] in [state]. Also, when possible, given
      a formal [f], it reduces the corresponding actual [act_f] to the value
      of [f] in [state]. It it is used after a call to clean up the state,
      and to gain some informations on the actuals.  *)
  let reduce_actuals_by_formals formals actuals state =
    let rec find_actual_varinfo e = match e.enode with
      | Lval (Var vi, NoOffset) ->
         if not vi.vaddrof && not (Cil.typeHasQualifier "volatile" vi.vtype)
         then Some vi else None
      | CastE (typ, e') -> begin
        match find_actual_varinfo e' with
        | None -> None
        | Some vi as ovi ->
           (* we can ignore casts, but only if they have no effect on the
              abstract value *)
           match Cil.unrollType typ, Cil.unrollType vi.vtype with
           | (TInt (ik, _) | TEnum ({ekind = ik}, _)),
             (TInt (ik', _) | TEnum ({ekind = ik'}, _)) ->
              if Cil.bytesSizeOfInt ik = Cil.bytesSizeOfInt ik' &&
                 Cil.isSigned ik = Cil.isSigned ik'
              then ovi else None
           | TPtr _, TPtr _ -> ovi
           | TFloat (fk, _), TFloat (fk', _) ->
              if fk = fk' then ovi else None
           | _ -> None
      end
      | _ -> None
    in
    let cleanup acc exp v =
      let b = Base.of_varinfo v in
      let reduced = match find_actual_varinfo exp with
        | Some vi -> begin
          (* Replace [vi] by [b] when the latter is is bound in [state]. This
             is sound because, had [b] been written during the call, it would
             have been removed. (see {!externalize} below). Thus, either [b]
             is equal to [vi], or it has been reduced during the call (in which
             case it is useful to reduce [vi]). *)
          try
            match Model.find_base b acc with
            | `Bottom | `Top -> acc
            | `Map offsm -> Model.add_base (Base.of_varinfo vi) offsm acc
          with Not_found -> acc
        end
        | None -> acc
      in
      Cvalue.Model.remove_base b reduced
    in
    Function_args.fold_left2_best_effort cleanup state actuals formals

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
        let aux_actual e (state, actuals) =
          let offsm, state =
            Function_args.compute_actual
              ~with_alarms ~warn_indeterminate state e
          in
          state, (e, offsm) :: actuals
        in
        let state, actuals = List.fold_right aux_actual argl (state, []) in
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
        let treat_one_result formals res (return, state) =
          if not (Cvalue.Model.is_reachable state)
          then res
          else
            let state = reduce_actuals_by_formals formals argl state in
            match lval_to_assign with
              | None -> state :: res
              | Some lv ->
                let state, ploc, typlv =
                  lval_to_precise_loc_state_for_writing ~with_alarms state lv
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
            Value_results.add_kf_caller f ~caller;
            let call_kinstr = Kstmt stmt in
            let recursive = not (Warn.check_no_recursive_call f) in
            let res =
              !compute_call_ref f ~recursive ~call_kinstr state actuals in
            CurrentLoc.set call_site_loc; (* Changed by compute_call_ref *)
            if res.Value_types.c_cacheable = Value_types.NoCacheCallers then
              (* Propagate info that callers cannot be cached either *)
              cacheable := Value_types.NoCacheCallers;
            Locals_scoping.remember_bases_with_locals
              clob res.Value_types.c_clobbered;
            (* If the call is recursive, we must not remove the formals: they
               have been restored to their values during the original call. *)
            let formals =
              if recursive then [] else Kernel_function.get_formals f in
            let treat = treat_one_result formals in
            List.fold_left treat acc_rt_res res.Value_types.c_values
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
      let exact =
        lazy (Precise_locs.valid_cardinal_zero_or_one ~for_writing loc)
      in
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
                Valarms.set_syntactic_context (Valarms.SySep(lv1, lv2));
                Valarms.warn_separated warn_all_mode;
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


  (* Remove locals and overwritten variables from the given state, and extract
     the content of \result. *)
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
              try
                Eval_exprs.offsetmap_of_lv ~with_alarms state lv
              with Int_Base.Error_Top ->
                Value_parameters.abort ~current:true
                  "Function %a returns a value of unknown size. Aborting"
                  Kernel_function.pretty kf
            in
            match oret with
              | `Bottom ->
                assert (Model.equal Model.bottom state);
                state, None
              | `Top -> Warn.warn_top ();
              | `Map oret ->
                Valarms.set_syntactic_context (Valarms.SyMem lv);
                let offsetmap_state =
                  if Value_util.warn_indeterminate kf then
                    Warn.warn_reduce_indeterminate_offsetmap
                      ~with_alarms typ_ret oret `NoLoc state
                  else `Res (oret, state)
                in
                match offsetmap_state with
                  | `Bottom -> (* Completely indeterminate return *)
                    Model.bottom, None
                  | `Res (ret_val, state) ->
                    let locals, r = offsetmap_top_addresses_of_locals ret_val in
                    if not (Cvalue.V_Offsetmap.equal r ret_val) then
                      Warn.warn_locals_escape_result fundec locals;
                    state, Some r
      in
      let state = Cvalue.Model.remove_variables fundec.slocals state in
      (* We only remove from [state] the formals that have been overwritten
         during the call. The other ones will be used by the caller. See
         {!reduce_actuals_by_formals} above. *)
      let written_formals = Value_util.written_formals kf in
      let state = Cvalue.Model.remove_variables written_formals state in
      let state = state_top_addresses_of_locals state in
      ret_val, state    


(*
Local Variables:
compile-command: "make -C ../../.."
End:
*)
