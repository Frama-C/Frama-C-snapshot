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
open Eval

module type S = sig
  type state
  type value
  val assign: state -> kinstr -> lval -> exp -> state or_bottom
  val assume: state -> stmt -> exp -> bool -> state or_bottom
  val call:
    stmt -> lval option -> exp -> exp list -> state ->
    state list or_bottom * Value_types.cacheable
  val split_final_states:
    kernel_function -> exp -> Integer.t list -> state list -> state list list
  val check_unspecified_sequence:
    stmt ->
    state -> (stmt * lval list * lval list * lval list * stmt ref list) list ->
    unit or_bottom
  type call_result = {
    states: state list or_bottom;
    cacheable: Value_types.cacheable;
  }
  val compute_call_ref: (kinstr -> value call -> state -> call_result) ref
end

(* Reference filled in by the callwise-inout callback *)
module InOutCallback =
  State_builder.Option_ref (Inout_type)
    (struct
      let dependencies = [Db.Value.self]
      let name = "Transfer_stmt.InOutCallback"
    end)

let register_callback () =
  Db.Operational_inputs.Record_Inout_Callbacks.extend_once
    (fun (_stack, inout) -> InOutCallback.set inout)

let () = Cmdline.run_after_configuring_stage register_callback

let current_kf_inout = InOutCallback.get_option

(* Should we warn about indeterminate copies in the function [kf] ? *)
let warn_indeterminate kf =
  let params = Value_parameters.WarnCopyIndeterminate.get () in
  Kernel_function.Set.mem kf params

(* An assignment from a right scalar lvalue is interpreted as a copy when
   indeterminate copies are allowed. Otherwise, such assignments are interpreted
   through the evaluation of the right lvalue, possibly leading to alarms about
   non initialization and dangling pointers. *)
let do_copy_at = function
  | Kglobal -> false
  | Kstmt stmt ->
    try
      let kf = Kernel_function.find_englobing_kf stmt in
      not (warn_indeterminate kf)
    with Not_found -> assert false

(* Warn for arguments that contain uninitialized/escaping if:
   - kf is a non-special leaf function (TODO: should we keep this?)
   - the user asked for this *)
let is_determinate kf =
  let name = Kernel_function.get_name kf in
  warn_indeterminate kf
  || not (Kernel_function.is_definition kf (* Should we keep this? *)
          || (name >= "Frama_C" && name < "Frama_D")
          || Builtins.find_builtin_override kf <> None)

module Make
    (Value: Abstract_value.External)
    (Location: Abstract_location.External)
    (Domain: Abstract_domain.External with type value = Value.t
                                       and type location = Location.location)
    (Eva: Evaluation.S with type state = Domain.state
                        and type value = Domain.value
                        and type loc = Domain.location
                        and type origin = Domain.origin)
= struct

  type state = Domain.state
  type value = Domain.value

  (* Transfer functions. *)
  module TF = Domain.Transfer (Eva.Valuation)

  (* ------------------------------------------------------------------------ *)
  (*                               Assignments                                *)
  (* ------------------------------------------------------------------------ *)

  (* Default assignment: evaluates the right expression. *)
  let assign_by_eval state valuation expr =
    Eva.evaluate ~valuation state expr >>=: fun (valuation, value) ->
    Assign value, valuation

  (* Assignment by copying the value of a right lvalue. *)
  let assign_by_copy state valuation lval =
    (* This code about garbled mix is specific to the Cvalue domain.
       Unfortunately, the current API for abstract_domain does not permit
       distinguishing between an evaluation or a copy. *)
    Locations.Location_Bytes.do_track_garbled_mix false;
    let r = Eva.copy_lvalue ~valuation state lval in
    Locations.Location_Bytes.do_track_garbled_mix true;
    r >>=: fun (valuation, value) ->
    Copy (lval, value), valuation

  (* At a call site, if the return value of a function can be infinite or NaN,
     the corresponding alarm cannot be emitted. Instead, we replace the
     return value by top_int (that contains the infinite and NaN values).
     This also requires updating the valuation.
     TODO: this infamous hack will no longer be necessary when infinity and
     NaN are encoded in Ival. *)
  let update_valuation valuation expr value =
    match Eva.Valuation.find valuation expr with
    | `Top -> valuation
    | `Value record ->
      let record = {record with value = {record.value with v = `Value value}} in
      Eva.Valuation.add valuation expr record

  let update_assigned assigned value = match assigned with
    | Assign _ -> Assign value
    | Copy (lval, v) -> Copy (lval, {v with v = `Value value})

  (* At a call site, no ACSL assertion can involve the return value of the
     called function. The \is_finite alarms about \result coming from builtins
     or specifications, are thus reported as simple warnings. *)
  let check_infinite_return alarms expr eval =
    let is_infinite = ref false in
    let inspects alarm status =
      match alarm with
      | Alarms.Is_nan_or_infinite _ ->
        is_infinite := !is_infinite || status <> Alarmset.True
      | _ -> () (* There might be alarms coming from evaluation of the lhs
                   of the assignment. We don't treat them here. *)
    in
    Alarmset.iter inspects alarms;
    if !is_infinite
    then
      let top = Value.top_int in
      eval >>-: fun (assigned, valuation) ->
      update_assigned assigned top, update_valuation valuation expr top
    else
      eval

  (* For an initialization, use for_writing:false for the evaluation of
     the left location, as the written variable could be const.  This is only
     useful for local initializations through function calls, as other
     initializations are handled by initialization.ml. *)
  let for_writing kinstr = match kinstr with
    | Kglobal -> false
    | Kstmt stmt -> match stmt.skind with
      | Instr (Local_init _) -> false
      | _ -> true

  (* Find a lvalue hidden under identity casts. This function correctly detects
     bitfields (thanks to [need_cast]) and will never expose the underlying
     field. *)
  let rec find_lval expr = match expr.enode with
    | Lval lv -> Some lv
    | CastE (typ, e) ->
      if Eval_typ.need_cast typ (Cil.typeOf e) then None else find_lval e
    | _ -> None

  (* Emits an alarm if the left and right locations of a struct or union copy
     overlap. *)
  let check_overlap kinstr typ (lval, loc) (right_lval, right_loc) =
    if Cil.isStructOrUnionType typ && Location.partially_overlap loc right_loc
    then Alarmset.(emit kinstr (singleton (Alarms.Overlap (lval, right_lval))))

  (* Checks the compatibility between the left and right locations of a copy. *)
  let are_compatible loc right_loc =
    let size1 = Location.size loc
    and size2 = Location.size right_loc in
    Int_Base.equal size1 size2 && not (Int_Base.is_top size1)

  (* Assignment. *)
  let assign_lv_or_ret ~is_ret state kinstr lval expr =
    let for_writing = for_writing kinstr in
    let eval, alarms_loc = Eva.lvaluate ~for_writing state lval in
    Alarmset.emit kinstr alarms_loc;
    match eval with
    | `Bottom ->
      Kernel.warning ~current:true ~once:true
        "@[<v>@[all target addresses were invalid. This path is \
         assumed to be dead.@]%t@]" Value_util.pp_callstack;
      `Bottom
    | `Value (valuation, lloc, ltyp) ->
      (* Tries to interpret the assignment as a copy for the returned value
         of a function call, on struct and union types, and when
         -val-warn-copy-indeterminate is disabled. *)
      let lval_copy =
        if is_ret || Cil.isStructOrUnionType ltyp || do_copy_at kinstr
        then find_lval expr
        else None
      in
      let eval, alarms = match lval_copy with
        | None -> assign_by_eval state valuation expr
        | Some right_lval ->
          (* In case of a copy, checks that the left and right locations are
             compatible and that they do not overlap. *)
          Eva.lvaluate ~for_writing:false ~valuation state right_lval
          >>= fun (valuation, right_loc, _right_typ) ->
          check_overlap kinstr ltyp (lval, lloc) (right_lval, right_loc);
          if are_compatible lloc right_loc
          then assign_by_copy state valuation right_lval
          else assign_by_eval state valuation expr
      in
      let eval =
        (* At a call site, do not emit alarms about the fake variable used
           for the return of the called function. *)
        if is_ret
        then check_infinite_return alarms expr eval
        else (Alarmset.emit kinstr alarms; eval)
      in
      eval >>- fun (assigned, valuation) ->
      TF.assign kinstr {lval; ltyp; lloc} expr assigned valuation state

  let assign = assign_lv_or_ret ~is_ret:false
  let assign_ret = assign_lv_or_ret ~is_ret:true

  (* ------------------------------------------------------------------------ *)
  (*                               Assumption                                 *)
  (* ------------------------------------------------------------------------ *)

  (* Assumption. *)
  let assume state stmt expr positive =
    let eval, alarms = Eva.reduce state expr positive in
    (* TODO: check not comparable. *)
    Alarmset.emit (Kstmt stmt) alarms;
    eval >>- fun valuation ->
    TF.assume stmt expr positive valuation state


  (* ------------------------------------------------------------------------ *)
  (*                             Function Calls                               *)
  (* ------------------------------------------------------------------------ *)

  type call_result = {
    states: state list or_bottom;
    cacheable: Value_types.cacheable;
  }

  (* Forward reference to [Eval_funs.compute_call] *)
  let compute_call_ref
    : (kinstr -> value call -> Domain.state -> call_result) ref
    = ref (fun _ -> assert false)

  let process_call call_kinstr call = function
    | Compute state ->
      Domain.Store.register_initial_state (Value_util.call_stack ()) state;
      !compute_call_ref call_kinstr call state
    | Result (states, cacheable) -> { states; cacheable }

  (* ------------------- Retro propagation on formals ----------------------- *)


  let get_precise_location = Location.get Main_locations.ploc_key

  (* [is_safe_argument valuation expr] is true iff the expression [expr] could
     not have been written during the last call.
     If the Location module includes precise_locs, and if the inout plugins
     is run callwise, then the function uses the precise_locs of the [valuation]
     and the results of inout. An argument is safe if its dependencies (the
     locations on which its value depends) do not intersect with the zones
     written by the called function.
     If precise_locs or the callwise inout is not available, a syntactic
     criterion is used. See {!Backward_formals.safe_argument}. *)
  let is_safe_argument =
    let default _ expr = Backward_formals.safe_argument expr in
    match get_precise_location with
    | None -> default
    | Some get ->
      fun valuation expr ->
        match InOutCallback.get_option () with
        | None -> default valuation expr
        | Some inout ->
          let find_loc lval =
            match Eva.Valuation.find_loc valuation lval with
            | `Top -> Precise_locs.loc_top
            | `Value record -> get record.loc
          in
          let expr_zone = Value_util.zone_of_expr find_loc expr in
          let written_zone = inout.Inout_type.over_outputs_if_termination in
          not (Locations.Zone.intersects expr_zone written_zone)

  (* Removes from the list of arguments of a call the arguments whose concrete
     or formal argument could have been written during the call, as well as
     arguments of non arithmetic or non pointer type. *)
  let filter_safe_arguments valuation call =
    let written_formals = Backward_formals.written_formals call.kf in
    let is_safe argument =
      not (Varinfo.Set.mem argument.formal written_formals)
      && Cil.isArithmeticOrPointerType argument.formal.vtype
      && is_safe_argument valuation argument.concrete
    in
    List.filter is_safe call.arguments

  (* At the end of a call, this function gathers the arguments whose value can
     be reduced at the call site. These are the arguments such that:
     - the formal has not been written during the call, but its value has been
       reduced;
     - no variable of the concrete argument has been written during the call
     (thus the concrete argument is still equal to the formal).
     [state] is the state at the return statement of the called function;
     it is used to evaluate the formals; their values are then compared to the
     ones at the beginning of the call.
     The function returns an association list between the argument that can be
     reduced, and their new (more precise) value.  *)
  let gather_reduced_arguments action call valuation state =
    match action with
    | Result _ -> `Value []
    | _ ->
      let safe_arguments = filter_safe_arguments valuation call in
      let empty = Eva.Valuation.empty in
      let reduce_one_argument acc argument =
        acc >>- fun acc ->
        let pre_value = match argument.avalue with
          | Assign pre_value -> `Value pre_value
          | Copy (_lv, pre_value) -> pre_value.v
        in
        let lval = Cil.var argument.formal in
        (* We use copy_lvalue instead of evaluate to get the escaping flag:
           if a formal is escaping at the end of the called function, it may
           have been freed, which is not detected as a write. We prevent the
           backward propagation in that case.
           If the call has copied the argument, it may be uninitialized. Thus,
           we also avoid the backward propagation if the formal is uninitialized
           here. This should not happen in the Assign case above. *)
        fst (Eva.copy_lvalue ~valuation:empty state lval)
        >>- fun (_valuation, post_value) ->
        if
          Bottom.is_included Value.is_included pre_value post_value.v
          || post_value.escaping || not post_value.initialized
        then `Value acc
        else post_value.v >>-: fun post_value -> (argument, post_value) :: acc
      in
      List.fold_left reduce_one_argument (`Value []) safe_arguments

  (* [reductions] is an association list between expression and value.
     This function reduces the [state] by assuming [expr = value] for each pair
     (expr, value) of [reductions]. *)
  let reduce_arguments reductions state =
    let valuation = `Value Eva.Valuation.empty in
    let reduce_one_argument valuation (argument, post_value) =
      valuation >>- fun valuation ->
      Eva.assume ~valuation state argument.concrete post_value
    in
    List.fold_left reduce_one_argument valuation reductions >>-: fun valuation ->
    TF.update valuation state

  (* -------------------- Treat the results of a call ----------------------- *)

  (* Treat the assignment of the return value in the caller: if the function
     has a non-void type, perform the assignment if there is a lvalue at
     the callsite, and in all cases, remove the pseudo-variable from scope. *)
  let treat_return ~kf_callee lv return stmt state =
    match lv, return with
    | None, None -> `Value state
    | None, Some vi_ret -> `Value (Domain.leave_scope kf_callee [vi_ret] state)
    | Some _, None -> assert false
    | Some lval, Some vi_ret ->
      let exp_ret_caller = Value_util.lval_to_exp  (Var vi_ret, NoOffset) in
      assign_ret state (Kstmt stmt) lval exp_ret_caller
      >>-: fun state -> Domain.leave_scope kf_callee [vi_ret] state

  (* ---------------------- Make a one function call ------------------------ *)

  (* The variables leaving scope at the end of a call to [kf]:
     the formals, and the locals of the body of kf, if any. *)
  let leaving_vars kf =
    let locals =
      try
        let fundec = Kernel_function.get_definition kf in
        fundec.sbody.blocals
      with Kernel_function.No_Definition -> []
    in
    Kernel_function.get_formals kf @ locals

  (* Do the call to one function. *)
  let do_one_call valuation stmt lv call state =
    let kf_callee = call.kf in
    Value_util.push_call_stack kf_callee (Kstmt stmt);
    let cleanup () =
      Value_util.pop_call_stack ();
      (* Changed by compute_call_ref, called from process_call *)
      Cil.CurrentLoc.set (Cil_datatype.Stmt.loc stmt); 
    in
    (* Choice of the action to performed by the domain. *)
    let call_action = TF.start_call stmt call valuation state in
    (* Process the call according to the domain decision. *)
    let call_result =
      try
        let result = process_call (Kstmt stmt) call call_action in
        cleanup ();
        result
      with Db.Value.Aborted as e ->
        InOutCallback.clear ();
        cleanup ();
        raise e
    in
    call_result.cacheable,
    call_result.states >>- fun result ->
    let leaving_vars = leaving_vars kf_callee in
    let pre = state in
    (* Treat each result one by one. *)
    let process state =
      (* Gathers the possible reductions on the value of the concrete arguments
         at the call site, according to the value of the formals at the post
         state of the called function.
         This obviously requires the formals to still be in the post_state. *)
      gather_reduced_arguments call_action call valuation state
      >>- fun reductions ->
      (* The formals (and the locals) of the called function leave scope. *)
      let post = Domain.leave_scope kf_callee leaving_vars state in
      (* Computes the state after the call, from the post state at the end of
         the called function, and the pre state at the call site. *)
      TF.finalize_call stmt call ~pre ~post >>- fun state ->
      (* Backward propagates the [reductions] on the concrete arguments. *)
      reduce_arguments reductions state >>- fun state ->
      treat_return ~kf_callee lv call.return stmt state
    and process_recursive state =
      (* When the call is recursive, formals have not been added to the
         domains. Do not reduce them, and more importantly, do not remove
         them from the scope. (Because the instance from the initial,
         non-recursive, call are still present.) *)
      TF.finalize_call stmt call ~pre ~post:state >>- fun state ->
      treat_return ~kf_callee lv call.return stmt state
    in
    let states =
      let process = if call.recursive then process_recursive else process in
      List.fold_left
        (fun acc return -> Bottom.add_to_list (process return) acc)
        [] result
    in
    InOutCallback.clear ();
    Bottom.bot_of_list states


  (* ------------------- Evaluation of the arguments ------------------------ *)

  (* [evaluate_argument ~determinate valuation state expr]
     evaluates the call argument [expr] in the state [state] and the valuation
     [valuation]. Returns the value assigned, and the updated valuation.
     TODO: share more code with [assign]. *)
  let evaluate_actual ~determinate valuation state expr =
    match expr.enode with
    | Lval lv ->
      Eva.lvaluate ~for_writing:false ~valuation state lv
      >>= fun (valuation, loc, _) ->
      if Int_Base.is_top (Location.size loc)
      then
        Value_parameters.abort ~current:true
          "Function argument %a has unknown size. Aborting"
          Printer.pp_exp expr;
      if determinate && Cil.isArithmeticOrPointerType (Cil.typeOfLval lv)
      then assign_by_eval state valuation expr
      else assign_by_copy state valuation lv
    | _ ->
      assign_by_eval state valuation expr

  (* Evaluates the list of the actual arguments of a call. Returns the list
     of each argument expression associated to its assigned value, and the
     valuation resulting of the evaluations. *)
  let compute_actuals determinate valuation state arguments =
    let process expr acc =
      acc >>= fun (args, valuation) ->
      evaluate_actual ~determinate valuation state expr >>=:
      fun (assigned, valuation) ->
      (expr, assigned) :: args, valuation
    in
    List.fold_right process arguments (`Value ([], valuation), Alarmset.none)

  (* ------------------------- Make an Eval.call ---------------------------- *)

  (* Create an Eval.call *)
  let create_call kf args =
    let recursive = Recursion.is_recursive_call kf in
    let return = Library_functions.get_retres_vi kf in
    let arguments, rest =
      if recursive then
        (* For recursive calls, we evaluate 'assigns \result \from \nothing'
           using a specification. We generate a dummy [call] object in which
           formals are not present. This way, domains will not overwrite
           the formals of the recursive function (which would be present
           in scope twice). *)
        [], []
      else
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
        arguments, rest
    in
    {kf; arguments; rest; return; recursive}

  let make_call kf arguments valuation state =
    (* Evaluate the arguments of the call. *)
    let determinate = is_determinate kf in
    compute_actuals determinate valuation state arguments
    >>=: fun (args, valuation) ->
    let call = create_call kf args in
    call, valuation


  (* ----------------- show_each and dump_each directives ------------------- *)

  let extract_cvalue = match Domain.get Cvalue_domain.key with
    | None -> fun _ -> Cvalue.Model.top
    | Some get -> get

  (* The product of domains formats the printing of each leaf domains, by
     checking their log_category and adding their name before the dump. If the
     domain is not a product, this needs to be done here. *)
  let print_state =
    if Domain.log_category = Domain_product.product_category
    then Domain.pretty
    else if Value_parameters.is_debug_key_enabled Domain.log_category
    then
      fun fmt state ->
        Format.fprintf fmt "# %s:@ @[<hv>%a@]@ " Domain.name Domain.pretty state
    else fun _ _ -> ()

  (* Frama_C_dump_each functions. *)
  let dump_state name state =
    Value_parameters.result ~current:true
      "%s:@\n@[<v>%a@]==END OF DUMP==%t"
      name print_state state Value_util.pp_callstack

  (* Idem as for [print_state]. *)
  let show_expr =
    if Domain.log_category = Domain_product.product_category
    then TF.show_expr
    else if Value_parameters.is_debug_key_enabled Domain.log_category
    then
      fun valuation state fmt exp ->
        Format.fprintf fmt "# %s: @[<hov>%a@]"
          Domain.name (TF.show_expr valuation state) exp
    else fun _ _ _ _ -> ()

  (* Frama_C_domain_show_each functions. *)
  let domain_show_each name arguments state =
    let pretty fmt expr =
      let pp fmt  =
        match fst (Eva.evaluate state expr) with
        | `Bottom -> Format.fprintf fmt "%s" (Unicode.bottom_string ())
        | `Value (valuation, _value) -> show_expr valuation state fmt expr
      in
      Format.fprintf fmt "%a : @[<h>%t@]" Printer.pp_exp expr pp
    in
    let pp = Pretty_utils.pp_list ~pre:"@[<v>" ~sep:"@ " ~suf:"@]" pretty in
    Value_parameters.result ~current:true
      "@[<v>%s:@ %a@]%t"
      name pp arguments Value_util.pp_callstack

  (* For non scalar expressions, prints the offsetmap of the cvalue domain. *)
  let show_offsm =
    match Domain.get Cvalue_domain.key, Location.get Main_locations.ploc_key with
    | None, _ | _, None ->
      fun fmt _ _ -> Format.fprintf fmt "%s" (Unicode.top_string ())
    | Some get_cvalue, Some get_ploc ->
      fun fmt expr state ->
        match expr.enode with
        | Lval lval ->
          begin
            try
              let offsm =
                fst (Eva.lvaluate ~for_writing:false state lval)
                >>- fun (_, loc, _) ->
                let ploc = get_ploc loc
                and cvalue_state = get_cvalue state in
                Eval_op.offsetmap_of_loc ploc cvalue_state
              in
              let typ = Cil.typeOf expr in
              (Bottom.pretty (Eval_op.pretty_offsetmap typ)) fmt offsm
            with Abstract_interp.Error_Top ->
              Format.fprintf fmt "%s" (Unicode.top_string ())
          end
        | _ -> assert false

  (* For scalar expressions, prints the cvalue component of their values. *)
  let show_value =
    match Value.get Main_values.cvalue_key with
    | None -> fun fmt _ _ -> Format.fprintf fmt "%s" (Unicode.top_string ())
    | Some get_cval ->
      fun fmt expr state ->
        let value = fst (Eva.evaluate state expr) >>-: snd >>-: get_cval in
        (Bottom.pretty Cvalue.V.pretty) fmt value

  (* Frama_C_show_each functions. *)
  let show_each name arguments state =
    let pretty fmt expr =
      if Cil.isArithmeticOrPointerType (Cil.typeOf expr)
      then show_value fmt expr state
      else show_offsm fmt expr state
    in
    let pp = Pretty_utils.pp_list ~pre:"@[<hv>" ~sep:",@ " ~suf:"@]" pretty in
    Value_parameters.result ~current:true
      "@[<hv>%s:@ %a@]%t"
      name pp arguments Value_util.pp_callstack

  (** Applies the show_each or dump_each directives. *)
  let apply_special_directives kf arguments state =
    let name = Kernel_function.get_name kf in
    if Ast_info.can_be_cea_function name
    then
      if Ast_info.is_cea_dump_function name
      then (dump_state name state; true)
      else if Ast_info.is_cea_function name
      then (show_each name arguments state; true)
      else if Ast_info.is_cea_domain_function name
      then (domain_show_each name arguments state; true)
      else false
    else false

  (* Legacy callbacks for the cvalue domain, usually called by
     {Cvalue_transfer.start_call}. *)
  let apply_cvalue_callback kf ki_call state =
    let stack_with_call = (kf, ki_call) :: Value_util.call_stack () in
    let cvalue_state = extract_cvalue state in
    Db.Value.Call_Value_Callbacks.apply (cvalue_state, stack_with_call);
    Db.Value.merge_initial_state (Value_util.call_stack ()) cvalue_state;
    let result =
      { Value_types.c_values = [ None, cvalue_state] ;
        c_clobbered = Base.SetLattice.bottom;
        c_from = None;
        c_cacheable = Value_types.Cacheable;
      }
    in
    Db.Value.Call_Type_Value_Callbacks.apply
      (`Builtin result, cvalue_state, stack_with_call)


  (* --------------------- Process the call statement ---------------------- *)

  (* We cannot statically check that a call through a function pointer is
     correct wrt the number of arguments and their types (see the examples at
     the end of tests/misc/fun_ptr.i). Thus, we make additional checks  here. *)
  let check_formals_types call =
    let check_one_formal arg =
      let expr = arg.concrete and formal = arg.formal in
      try Cil.bitsSizeOf (Cil.typeOf expr) = Cil.bitsSizeOf (formal.vtype)
      with Cil.SizeOfError _ -> false
    in
    if not (List.for_all check_one_formal call.arguments) then
      raise InvalidCall

  let call stmt lval_option funcexp arguments state =
    let ki_call = Kstmt stmt in
    let cacheable = ref Value_types.Cacheable in
    let eval =
      (* Resolve [funcexp] into the called kernel functions. *)
      let functions, alarms = Eva.eval_function_exp funcexp state in
      Alarmset.emit ki_call alarms;
      functions >>- fun functions ->
      let current_kf = Value_util.current_kf () in
      let process_one_function kf valuation =
        (* The special Frama_C_ functions to print states are handled here. *)
        if apply_special_directives kf arguments state
        then
          let () = apply_cvalue_callback kf ki_call state in
          `Value ([state])
        else
          (* Create the call. *)
          let eval, alarms = make_call kf arguments valuation state in
          Alarmset.emit ki_call alarms;
          eval >>- fun (call, valuation) ->
          (* Check that formals are properly typed. raise [InvalidCall]
             otherwise. *)
          check_formals_types call;
          (* Register the call. *)
          Value_results.add_kf_caller call.kf ~caller:(current_kf, stmt);
          (* Do the call. *)
          let c, states = do_one_call valuation stmt lval_option call state in
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
          let alarm = Alarmset.singleton (Alarms.Function_pointer funcexp) in
          Alarmset.emit ki_call alarm;
          acc
      in
      let states_list = List.fold_left process [] functions in
      Bottom.bot_of_list states_list
    in
    eval, !cacheable


  (* ------------------------------------------------------------------------ *)
  (*                            Function Return                               *)
  (* ------------------------------------------------------------------------ *)

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
  let check_unspecified_sequence stmt state seq =
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
    Alarmset.emit (Kstmt stmt) alarms;
    res

end


(*
Local Variables:
compile-command: "make -C ../../../.."
End:
*)
