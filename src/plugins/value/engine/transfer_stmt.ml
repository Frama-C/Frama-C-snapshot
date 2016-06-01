(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2016                                               *)
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
open Eval

module type S = sig
  type state
  type value
  type summary
  val assign: with_alarms:CilE.warn_mode ->
    state -> kernel_function -> stmt -> lval -> exp -> state or_bottom
  val assume: with_alarms:CilE.warn_mode ->
    state -> stmt -> exp -> bool -> state or_bottom
  val call: with_alarms:CilE.warn_mode ->
    stmt -> lval option -> exp -> exp list -> state ->
    state list or_bottom * Value_types.cacheable
  val return: with_alarms:CilE.warn_mode ->
    kernel_function -> stmt -> lval option -> state ->
    (state, summary, value) return or_bottom
  val split_final_states:
    kernel_function -> exp -> Integer.t list -> state list -> state list list
  val check_unspecified_sequence:
    with_alarms:CilE.warn_mode ->
    state -> (stmt * lval list * lval list * lval list * stmt ref list) list ->
    unit or_bottom
  type res = (state, summary, value) call_result * Value_types.cacheable
  val compute_call_ref: (kinstr -> value call -> state -> res) ref
end

module Make
    (Domain: Abstract_domain.Transfer)
    (Eva: Evaluation.S with type state = Domain.state
                        and type value = Domain.value
                        and type loc = Domain.location
                        and type Valuation.t = Domain.valuation)
= struct

  type state = Domain.state
  type value = Domain.value
  type summary = Domain.summary

  (* Emits a warning if the evaluation of an argument leads to a bottom state. *)
  let warn_if_bottom v warn = match v with
    | `Value _ -> ()
    | `Bottom  -> warn ()

  (* ------------------------------------------------------------------------ *)
  (*                               Assignments                                *)
  (* ------------------------------------------------------------------------ *)

  (* Evaluates the left value of the assignment. *)
  let evaluate_left_lvalue ~with_alarms state lv =
    let eval, alarms = Eva.lvaluate ~for_writing:true state lv in
    Alarmset.emit with_alarms alarms;
    warn_if_bottom eval (fun () ->
        Valarms.do_warn with_alarms.CilE.imprecision_tracing
          (fun () -> Kernel.warning ~current:true ~once:true
              "@[<v>@[all target addresses were invalid. This path is \
               assumed to be dead.@]%t@]" Value_util.pp_callstack));
    eval

  (* Default assignment: evaluates the right expression, emits the alarms
     and call the transfer function of the abstract domain. *)
  let assign_generic state valuation expr =
    Eva.evaluate ~valuation state expr >>=: fun (valuation, value) ->
    Assign value, valuation

  (* Exact copy: when the right expression of an assignment is an lvalue
     [right_lval], copies the abstract value of the [right_lval] without
     determinizing it (the copied value can be uninitialized, or cantain
     escaping addresses). *)
  let copy_right_lval state valuation expr =
    let eval, alarms = Eva.evaluate ~valuation ~indeterminate:true state expr in
    let eval =
      match eval with
      | `Bottom ->
        let value = { v = `Bottom; initialized = false; escaping = false } in
        Exact value, valuation
      | `Value (valuation, _) ->
        match Eva.Valuation.find valuation expr with
        | `Top -> assert false
        | `Value record ->
          let value = { record.value with v = record.value.v} in
          Exact value, valuation
    in
    `Value eval, alarms

  (* Determinate copy: when the right expression of an assignment is an lvalue
     [right_lval], evaluates the value of [right_lval], determinizes it, and
     then assigns it. *)
  let affect_right_val state valuation expr =
    let eval, alarms = Eva.evaluate ~valuation state expr in
    (eval, alarms) >>=. fun (valuation, _value) ->
    match Eva.Valuation.find valuation expr with
    | `Top -> assert false
    | `Value record ->
      record.value.v >>-: fun v ->
      Determinate {record.value with v}, valuation

  (* When the right expression is an lvalue, evaluates its location, and
     do an exact copy or a determinate copy according to the parameter of
     the analysis, the type of the left value, the matching of the sizes of
     the left and right locations. *)
  let assign_right_lval kf state valuation left_lv expr right_lval =
    let {lval; lloc; ltyp} = left_lv in
    Eva.lvaluate ~for_writing:false ~valuation state right_lval
    >>= fun (valuation, right_loc, _right_typ) ->
    Eva.check_copy_lval (lval, lloc) (right_lval, right_loc)
    >>= fun compatible_locations ->
    let eval =
      if (Value_util.warn_indeterminate kf
          && Cil.isArithmeticOrPointerType (Cil.typeOfLval lval))
      || (Eval_typ.is_bitfield ltyp)
      || (not compatible_locations)
      then
        affect_right_val state valuation expr
      else
        copy_right_lval state valuation expr
    in
    eval >>=: fun (copied_value, valuation) ->
    let copy = Copy (right_lval, copied_value) in
    copy, valuation


  let pass_identity_cast typ expr =
    let typ_expr = Cil.typeOf expr in
    match Cil.unrollType typ, Cil.unrollType typ_expr with
    | (TInt (ik1, _) | TEnum ({ekind=ik1}, _)),
      (TInt (ik2, _) | TEnum ({ekind=ik2}, _)) ->
      Cil.isSigned ik1 = Cil.isSigned ik2 &&
      Cil.bytesSizeOfInt ik1 = Cil.bytesSizeOfInt ik2

    | TPtr _, TPtr _                         -> true

    | TPtr _, TInt (ik, _) | TInt (ik, _), TPtr _
      when Cil.(theMachine.upointKind) = ik -> true

    | TFloat (f1,_), TFloat (f2, _) -> Cil.frank f1 = Cil.frank f2
    | _ -> false (* Not a scalar type *)

  let rec find_lv expr = match expr.enode with
    | Lval lv -> Some lv
    | CastE (typ, e) ->
      if pass_identity_cast typ e
      then find_lv e
      else None
    | _ -> None

  (* Assignment. *)
  let assign ~with_alarms state kf stmt lval expr =
    evaluate_left_lvalue ~with_alarms state lval >>- fun (valuation, lloc, ltyp) ->
    let left_lv = { lval; ltyp; lloc } in
    let eval, alarms =
      match find_lv expr with
      | Some right_lval ->
        assign_right_lval kf state valuation left_lv expr right_lval
      | None ->
        assign_generic state valuation expr
    in
    Alarmset.emit with_alarms alarms;
    eval >>- fun (assigned, valuation) ->
    Domain.assign (Kstmt stmt) left_lv expr assigned valuation state


  (* ------------------------------------------------------------------------ *)
  (*                               Assumption                                 *)
  (* ------------------------------------------------------------------------ *)

  (* Assumption. *)
  let assume ~with_alarms state stmt expr positive =
    let eval, alarms = Eva.reduce state expr positive in
    (* TODO: check not comparable. *)
    Alarmset.emit with_alarms alarms;
    eval >>- fun valuation ->
    Domain.assume stmt expr positive valuation state


  (* ------------------------------------------------------------------------ *)
  (*                             Function Calls                               *)
  (* ------------------------------------------------------------------------ *)

  type res = (state, summary, value) call_result * Value_types.cacheable

  (* Forward reference to [Eval_funs.compute_call] *)
  let compute_call_ref
    : (kinstr -> value call -> Domain.state -> res) ref
    = ref (fun _ -> assert false)

  let process_call call_kinstr call = function
    | Compute (Continue state, _) -> !compute_call_ref call_kinstr call state
    | Result (res, cacheable) -> res, cacheable
    | _ -> assert false (* TODO! *)


  (* -------------------- Treat the results of a call ----------------------- *)

  let warn_infinite_return alarms =
    let process alarm _status = match alarm with
      | Alarms.Is_nan_or_infinite (_, fkind) ->
        let sfkind = match fkind with
          | FFloat -> "float"
          | FDouble -> "double"
          | FLongDouble -> "long double"
        in
        Value_messages.warning
          "@[non-finite@ %s@ value being@ returned:@ \
           assert(\\is_finite(\\returned_value))@]" sfkind;
      | _ -> assert false
    in
    Alarmset.iter process alarms

  let warn_undeterminate_return right =
    if not right.initialized then
      Value_messages.warning "@[returned value may be uninitialized:@ \
                              assert \\initialized(\\returned_value)@]";
    if right.escaping then
      Value_messages.warning "@[returned value may be contain escaping addresses:@ \
                              assert \\dangling(\\returned_value)@]"

  (* [assign_returned_value determinate kf assigned_lv returned_value state]
     builds the copy (see {Eval.mli} for the type) of the returned value
     of a function call [kf] into the assigned lvalue of the call.
     - The copy is exact or removes indeterminateness of the returned value
       depending on the boolean determinate (see {Eval.mli} for details).
     - [state] is the state at the end of the function [kf], in which is
       evaluated the location of [assigned_lv]. The function also returns the
       alarms and the valuation of this evaluation.
     - [assigned_lv] and [returned_value] are option type, depending on whether
       the called function returns a value and the result of the called is
       assigned to a lvalue. If there is an assignment but no returned value,
       the analysis aborts. *)
  let copy_returned_value determinate kf assigned_lv returned_value state =
    match assigned_lv with
    | None -> `Value (Eva.Valuation.empty, None), Alarmset.none
    | Some lval ->
      let return_value = match returned_value with
          None ->
          Format.printf "Function %a@." Kernel_function.pretty kf;
          Value_parameters.abort ~current:true ~once:true
            "Return value expected but none present."
        | Some right_value -> right_value
      in
      let rettype = Kernel_function.get_return_type kf in
      let exp = Cil.dummy_exp (Lval lval) in
      let rvalue =
        (* TODO: alarms of reinterpret? *)
        return_value.v >>- fun v ->
        let v, alarms = Eva.reinterpret exp rettype v in
        let ltyp = Cil.typeOfLval lval in
        if Eval_typ.is_bitfield ltyp || Eval_typ.need_cast ltyp rettype
        then warn_infinite_return alarms;
        v
      in
      Eva.lvaluate ~for_writing:true state lval
      >>=. fun (valuation, lloc, ltyp) ->
      let left_loc = {lval; ltyp; lloc} in
      let value_copied =
        if
          (determinate && Cil.isArithmeticOrPointerType ltyp)
          || Eval_typ.is_bitfield ltyp
          || Eval_typ.is_bitfield ltyp || Eval_typ.need_cast ltyp rettype
        then
          let () = warn_undeterminate_return return_value in
          rvalue >>- fun rvalue ->
          if Eval_typ.need_cast ltyp rettype
          then
            let rvalue, _alarms =
              Eva.do_promotion ~src_typ:rettype ~dst_typ:ltyp exp rvalue
            in
            (* TODO: change _alarms into warnings, and emit them. *)
            rvalue >>-: fun v ->
            Determinate { return_value with v }
          else
            `Value (Determinate { return_value with v = rvalue})
        else
          `Value (Exact { return_value with v = rvalue })
      in
      value_copied >>-: fun value_copied ->
      valuation, Some (left_loc, value_copied)

  (* Process one result of a function call: makes the copy of the returned
     value (if any) and uses the resolve_call of the domain. *)
  let process_result with_alarms determinate pre_state stmt call lv return =
    let eval, alarms =
      copy_returned_value determinate call.kf lv
        return.returned_value return.post_state
    in
    Alarmset.emit with_alarms alarms;
    eval >>- fun (valuation, assigned) ->
    let pre = pre_state
    and post = return.summary, return.post_state in
    Domain.resolve_call stmt call ~assigned valuation ~pre ~post


  (* ---------------------- Make a one function call ------------------------ *)

  (* Do the call to one function. *)
  let do_one_call with_alarms determinate valuation loc lv call state =
    let call_site_loc, stmt = loc in
    (* Choice of the action to performed by the domain. *)
    let call_action = Domain.call_action stmt call valuation state in
    (* Process the call according to the domain decision. *)
    let res, cacheable =
      try process_call (Kstmt stmt) call call_action
      with Db.Value.Aborted as e ->
        Value_util.pop_call_stack ();
        raise e
    in
    Value_util.pop_call_stack ();
    Cil.CurrentLoc.set call_site_loc; (* Changed by compute_call_ref *)
    cacheable,
    res >>- fun result ->
    (* Treat each result one by one. *)
    let proceed acc return =
      match process_result with_alarms determinate state stmt call lv return with
      | `Bottom -> acc
      | `Value s -> s :: acc
    in
    let states = List.fold_left proceed [] result in
    Bottom.bot_of_list states


  (* ------------------- Evaluation of the arguments ------------------------ *)

  (* [evaluate_argument ~with_alarms ~determinate valuation state expr]
     evaluates the call argument [expr] in the state [state] and the valuation
     [valuation]. Returns the value assigned, and the updated valuation.
     TODO: share more code with [assign]. *)
  let evaluate_argument ~with_alarms ~determinate valuation state expr =
    let warn_if_bot v kind =
      warn_if_bottom (fst v) (fun () ->
          if with_alarms.CilE.imprecision_tracing.CilE.a_log then
            Value_parameters.result ~current:true ~once:true
              "completely invalid@ %s in evaluation of@ argument %a"
              kind Printer.pp_exp expr)
    in
    match expr.enode with
    | Lval lv ->
      let eval = Eva.lvaluate ~for_writing:false ~valuation state lv in
      warn_if_bot eval "location";
      eval >>= fun (valuation, loc, _) ->
      if Int_Base.is_top (Eva.loc_size loc)
      then
        Value_parameters.abort ~current:true
          "Function argument %a has unknown size. Aborting"
          Printer.pp_exp expr;
      let eval =
        if
          (determinate && Cil.isArithmeticOrPointerType (Cil.typeOfLval lv))
          || Eval_typ.is_bitfield (Cil.typeOfLval lv)
        then
          let res = affect_right_val state valuation expr in
          warn_if_bot res "value";
          res
        else
          copy_right_lval state valuation expr
      in
      eval >>=: fun (copied_value, valuation) ->
      let copy = Copy (lv, copied_value) in
      copy, valuation
    | _ ->
      let res = assign_generic state valuation expr in
      warn_if_bot res "value";
      res

  (* Evaluates the list of the actual arguments of a call. Returns the list
     of each argument expression associated to its assigned value, and the
     valuation resulting of the evaluations. *)
  let compute_actuals with_alarms determinate valuation state arguments =
    let process expr acc =
      acc >>= fun (args, valuation) ->
      evaluate_argument ~with_alarms ~determinate valuation state expr >>=:
      fun (assigned, valuation) ->
      (expr, assigned) :: args, valuation
    in
    let eval, alarms =
      List.fold_right process arguments (`Value ([], valuation), Alarmset.none)
    in
    Alarmset.emit with_alarms alarms;
    eval


  (* ------------------------- Make an Eval.call ---------------------------- *)

  (* Warn for arguments that contain uninitialized/escaping if:
     - kf is a non-special leaf function (TODO: should we keep this?)
     - the user asked for this *)
  let is_determinate kf =
    let name = Kernel_function.get_name kf in
    Value_util.warn_indeterminate kf
    || not (Kernel_function.is_definition kf (* Should we keep this? *)
            || (name >= "Frama_C" && name < "Frama_D")
            || Builtins.mem_builtin name)

  (* Create an Eval.call *)
  let create_call kf args =
    let formals = Kernel_function.get_formals kf in
    let rec format_arguments acc args formals = match args, formals with
      | _, [] -> acc, args
      | [], _ -> raise InvalidCall
      | (concrete, avalue) :: args, formal :: formals ->
        let argument = { formal ; concrete; avalue } in
        format_arguments (argument :: acc)  args formals
    in
    let arguments, rest = format_arguments [] args formals in
    let arguments = List.rev arguments in
    {kf; arguments; rest}

  let make_call with_alarms kf arguments valuation state =
    (* Evaluate the arguments of the call. *)
    let determinate = is_determinate kf in
    compute_actuals with_alarms determinate valuation state arguments
    >>-: fun (args, valuation) ->
    let call = create_call kf args in
    call, valuation


  (* --------------------- Process the call statement ---------------------- *)

  (* We cannot statically check that a call through a function pointer is
     correct wrt the number of arguments and their types (see the examples at
     the end of tests/misc/fun_ptr.i). Thus, we make additional checks  here. *)
  let check_call call =
    let check_one_formal arg =
      let expr = arg.concrete and formal = arg.formal in
      try Cil.bitsSizeOf (Cil.typeOf expr) = Cil.bitsSizeOf (formal.vtype)
      with Cil.SizeOfError _ -> false
    in
    if Warn.check_no_recursive_call call.kf
    then
      if List.for_all check_one_formal call.arguments
      then ()
      else raise InvalidCall
    else
      (* TODO: recursive call. *)
      Value_parameters.abort ~current:true ~once:true
        "Recursive call to a function."

  let call ~with_alarms stmt lval_option funcexp arguments state =
    let cacheable = ref Value_types.Cacheable in
    let eval =
      (* Resolve [funcexp] into the called kernel functions. *)
      let functions, alarms = Eva.eval_function_exp funcexp state in
      Alarmset.emit with_alarms alarms;
      functions >>- fun functions ->
      let call_site_loc = Cil.CurrentLoc.get ()
      and current_kf = Value_util.current_kf () in
      let determinate = is_determinate current_kf in
      let process_one_function kf valuation =
        (* Create the call. *)
        make_call with_alarms kf arguments valuation state
        >>- fun (call, valuation) ->
        (* Check the call. *)
        check_call call;
        (* Register the call. *)
        Value_results.add_kf_caller call.kf ~caller:(current_kf, stmt);
        Value_util.push_call_stack call.kf (Kstmt stmt);
        (* Do the call. *)
        let c, states =
          do_one_call with_alarms determinate valuation (call_site_loc, stmt)
            lval_option call state
        in
        (* If needed, propagate that callers cannot be cached. *)
        if c = Value_types.NoCacheCallers then
          cacheable := Value_types.NoCacheCallers;
        states
      in
      (* Process each possible function apart, and append the result list. *)
      let process acc (kf, valuation) =
        try
          let res = process_one_function kf valuation in
          (Bottom.list_of_bot res) @ acc
        with
        | InvalidCall ->
          Value_util.warning_once_current
            "Function type must match type at call site: \
             assert(function type matches)";
          Value_util.stop_if_stop_at_first_alarm_mode ();
          acc
      in
      let states_list = List.fold_left process [] functions in
      Bottom.bot_of_list states_list
    in
    eval, !cacheable


  (* ------------------------------------------------------------------------ *)
  (*                            Function Return                               *)
  (* ------------------------------------------------------------------------ *)

  let return ~with_alarms kf return_stmt return_lval state =
    let returned =
      match return_lval with
      | None -> `Value None
      | Some lval ->
        let eval, alarms = Eva.lvaluate ~for_writing:false state lval in
        Alarmset.emit with_alarms alarms;
        eval >>- fun (valuation, lloc, ltyp) ->
        let returned_lv = {lval; ltyp; lloc} in
        let expr = match return_stmt.skind with
          | Return (Some expr, _) -> expr
          | _ -> assert false
        in
        let eval, alarms =
          if (Value_util.warn_indeterminate kf
              && Cil.isArithmeticOrPointerType (Cil.typeOfLval lval))
          || (Eval_typ.is_bitfield ltyp)
          then
            affect_right_val state valuation expr
          else
            copy_right_lval state valuation expr
        in
        Alarmset.emit with_alarms alarms;
        eval >>-: fun (copied, _valuation) ->
        Some (returned_lv, copied)
    in
    returned >>- fun returned ->
    Domain.summarize kf return_stmt ~returned state >>-: fun (summary, state) ->
    let extract_value returned = match snd returned with
      | Determinate v -> { v = `Value v.v; initialized = true; escaping = false }
      | Exact v -> v
    in
    { post_state = state;
      summary = summary;
      returned_value = Extlib.opt_map extract_value returned }


  let split_final_states kf return_expr expected_values states =
    let varinfo = match return_expr.enode with
      | Lval (Var varinfo, NoOffset) -> varinfo
      | _                            -> assert false (* Cil invariant *)
    in
    if Cil.isIntegralOrPointerType varinfo.vtype
    then
      let matched, tail =
        Eva.split_by_evaluation return_expr expected_values states
      in
      let process (i, states, mess) =
        if mess then
          Value_parameters.result ~once:true ~current:true
            "%a: cannot properly split on \\result == %a"
            Kernel_function.pretty kf Abstract_interp.Int.pretty i;
        states
      in
      tail :: List.map process matched
    else [states]

  (* ------------------------------------------------------------------------ *)
  (*                            Unspecified Sequence                          *)
  (* ------------------------------------------------------------------------ *)

  (* Not currently taking advantage of calls information. But see
     plugin Undefined Order by VP. *)
  let check_unspecified_sequence ~with_alarms state seq =
    let rec check_one_stmt ((stmt1, _, writes1, _, _) as stmt) = function
      | [] -> `Value (), Alarmset.none
      | (stmt2, _, _, _, _) :: seq when stmt1 == stmt2 -> check_one_stmt stmt seq
      | (stmt2, modified2, writes2, reads2, _) :: seq  ->
        (* Values that cannot be read, as they are modified in the statement
           (but not by the whole sequence itself) *)
        let unauthorized_reads =
          List.filter
            (fun x -> List.for_all
                (fun y -> not (LvalStructEq.equal x y)) modified2)
            writes1
        in
        Eva.check_non_overlapping state unauthorized_reads reads2 >>= fun () ->
        if stmt1.sid < stmt2.sid then
          Eva.check_non_overlapping state writes1 writes2 >>= fun () ->
          check_one_stmt stmt seq
        else
          check_one_stmt stmt seq
    in
    let res, alarms =
      List.fold_left
        (fun acc x -> acc >>= fun () -> check_one_stmt x seq)
        (`Value (), Alarmset.none)
        seq
    in
    Alarmset.emit with_alarms alarms;
    res

end


(*
Local Variables:
compile-command: "make -C ../../../.."
End:
*)
