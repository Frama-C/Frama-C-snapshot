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

(** Value analysis of statements *)

open Cil_types
open Cil
open Cil_datatype
open Locations
open Abstract_interp
open Bit_utils
open Cvalue
open Value_util
open Eval_exprs
open Locals_scoping

module StmtCanReachCache =
  Kernel_function.Make_Table
    (Datatype.Function
       (struct include Cil_datatype.Stmt let label = None end)
       (Datatype.Function
          (struct include Cil_datatype.Stmt let label = None end)
          (Datatype.Bool)))
    (struct
      let name = "Eval_funs.StmtCanReachCache"
      let kind = `Internal
      let size = 17
      let dependencies = [ Ast.self ]
     end)

let stmt_can_reach_memo = StmtCanReachCache.memo Stmts_graph.stmt_can_reach

let compute_using_cfg kf ~call_kinstr initial_state =
  match kf.fundec with
  | Declaration _ -> assert false
  | Definition (f,_loc) ->
      begin
        let f_varinfo = f.svar in
        let module Computer =
          Eval_stmts.Computer
            (struct
              let current_kf = kf
              let stmt_can_reach =
                if Value_parameters.MemoryFootprint.get () >= 3
                then stmt_can_reach_memo kf
                else Stmts_graph.stmt_can_reach kf
              let is_natural_loop = Loop.is_natural kf
              let non_linear_assignments = Non_linear.find f
              let slevel = get_slevel kf
              let initial_state = initial_state (* for future reference *)
              let active_behaviors =
                Eval_logic.ActiveBehaviors.create initial_state kf
            end)
        in
        let module Compute = Dataflow.Forwards(Computer) in
        List.iter
          (function {called_kf = g} ->
             if kf == g
             then begin
                 if Value_parameters.IgnoreRecursiveCalls.get()
                 then begin
                     warning_once_current
                       "ignoring recursive call during value analysis of %a (%a)"
                       Varinfo.pretty f_varinfo
                       pretty_call_stack (call_stack ());
                     Db.Value.recursive_call_occurred kf;
                     raise Leaf
                   end
                 else begin
                   warning_once_current
                     "@[recursive call@ during@ value@ analysis@ (%a <- %a)@.Use %s@ to@ ignore@ (beware@ this@ will@ make@ the analysis@ unsound)@]"
                     Varinfo.pretty f_varinfo
                     pretty_call_stack (call_stack ())
                     Value_parameters.IgnoreRecursiveCalls.option_name;
                   raise (Extlib.NotYetImplemented "recursive calls in value analysis")
                 end
             end)
          (call_stack ());
        push_call_stack {called_kf = kf;
                         call_site = call_kinstr;
                         called_merge_current = Computer.merge_current};
        match f.sbody.bstmts with
          [] -> assert false
        | start :: _ ->
            let ret_id =
              try Kernel_function.find_return kf
              with Kernel_function.No_Statement -> assert false
            in
            (* We start with only the start block *)
            Computer.StmtStartData.add
              start
              (Computer.computeFirstPredecessor
                 start
                 {
                   Computer.counter_unroll = 0;
                   value =  initial_state});
            begin try
              Compute.compute [start]
            with Db.Value.Aborted as e ->
              (* State_builder.was aborted: pop the call stack and inform
                 the caller *)
              pop_call_stack ();
              raise e
            end;
            let last_ret,_,last_clob as last_state =
              try
                let _,state,_ as result =
                  try
                    Computer.externalize ret_id kf
                  with Not_found -> assert false
                in
                if Cvalue.Model.is_reachable state
                then begin
                  try
                    if hasAttribute "noreturn" f_varinfo.vattr
                    then
                      warning_once_current
                        "function %a may terminate but has the noreturn attribute"
                        Kernel_function.pretty kf;
                  with Not_found -> assert false
                end
                else raise Not_found;
                result
              with Not_found -> begin
                None,
                Cvalue.Model.bottom,
                Location_Bits.Top_Param.bottom
              end
            in
            Value_parameters.debug
              "@[RESULT FOR %a <-%a:@\n\\result -> %a@\nClobered set:%a@]"
                Kernel_function.pretty kf
              pretty_call_stack (call_stack ())
              (fun fmt v ->
                 match v with
                 | None -> ()
                 | Some v -> V_Offsetmap.pretty fmt v)
              last_ret
              Location_Bits.Top_Param.pretty
              last_clob;
            pop_call_stack ();
            last_state
      end

let compute_using_prototype kf ~active_behaviors ~state_with_formals =
(*    Format.printf "compute_using_prototype %s %a@."
      (Kernel_function.get_name kf)
      Cvalue.Model.pretty state_with_formals; *)
  let vi = Kernel_function.get_vi kf in
  if Cil.hasAttribute "noreturn" vi.vattr then
    None, Cvalue.Model.bottom, Location_Bits.Top_Param.bottom
  else
      let return_type,_formals_type,_inline,_attr =
        splitFunctionType (Kernel_function.get_type kf)
      in
      let behaviors =
        Eval_logic.ActiveBehaviors.active_behaviors active_behaviors in
      let assigns = Ast_info.merge_assigns behaviors in
      let returned_value, state_with_formals =
        Library_functions.returned_value kf return_type state_with_formals
      in
      let returned_value = ref returned_value in
      let clobbered_set = ref Location_Bits.Top_Param.bottom in
      let state =
        match assigns with
        | WritesAny ->
            warning_once_current "Cannot handle empty assigns clause. Assuming assigns \\nothing: be aware this is probably incorrect.";
            state_with_formals
        | Writes [] -> state_with_formals
        | Writes l ->
            let treat_assign acc (out, ins) =
              let input_contents =
                try
                  match ins with
                    | FromAny -> Cvalue.V.top_int
                    | From l ->
                      List.fold_left
                        (fun acc term ->
                          let input_loc =
                            !Db.Properties.Interp.loc_to_loc
                              ~result:None
                              state_with_formals
                              term.it_content
                          in
                          let r =
                            Cvalue.V.topify_arith_origin(
                              Cvalue.Model.find
                                ~conflate_bottom:true
                                ~with_alarms:CilE.warn_none_mode
                                state_with_formals
                                input_loc
                            ) in
                          Cvalue.V.join acc r)
                        Cvalue.V.top_int
                        l
                with Invalid_argument "not an lvalue" ->
		  warning_once_current
                    "cannot interpret@ assigns@ in function %a"
                    Kernel_function.pretty kf;
                  Cvalue.V.top
              in
              let treat_output_loc loc acc =
                remember_bases_with_locals
                  clobbered_set
                  loc
                  input_contents;
		let state =
                  Cvalue.Model.add_binding
                    ~with_alarms:CilE.warn_none_mode
                    ~exact:false acc loc input_contents
		in
		(* ugly; Fix ? Yes. *)
		if Cvalue.Model.is_reachable state
		then state
		else acc
              in
              try
                let loc = !Db.Properties.Interp.loc_to_loc
                  ~result:None acc out.it_content
                in
                let st = treat_output_loc loc acc in
                if Cvalue.Model.equal Cvalue.Model.top st then (
                  Value_parameters.error ~once:true ~current:true
                    "Cannot@ handle@ assigns@ for %a,@ location@ is@ too@ \
                     imprecise@ (%a).@ Assuming@ it@ is@ not@ assigned,@ but@ \
                     be@ aware@ this@ is@ incorrect."
                    d_term out.it_content Locations.pretty loc;
                  acc)
                else st
              with
                | Invalid_argument "not an lvalue" ->
                  if Logic_utils.is_result out.it_content then begin
                    returned_value :=
                      Cvalue.V.join input_contents !returned_value;
                    acc
                  end else begin
                    warning_once_current
                      "Cannot interpret assigns in function %a; \
                            effects will be ignored"
                      Kernel_function.pretty kf; acc
                  end
            in
            (List.fold_left treat_assign state_with_formals l)
      in
(*	Value_parameters.debug "compute_using_prototype suite %s %a@."
	  (Kernel_function.get_name kf)
	  Cvalue.Model.pretty state; *)
      let retres_vi, state =
        if isVoidType return_type
        then None, state
        else
          let offsetmap =
            V_Offsetmap.update_ival
              ~with_alarms:CilE.warn_none_mode
              ~validity:Base.All
              ~offsets:Ival.zero
              ~exact:true
              ~size:(Int.of_int (bitsSizeOf return_type))
              V_Offsetmap.empty
              (Cvalue.V_Or_Uninitialized.initialized !returned_value)
          in
          let rvi, state = Library_functions.add_retres_to_state
            ~with_alarms:CilE.warn_none_mode kf offsetmap state
          in
          Some rvi, state
      in
      retres_vi, state, !clobbered_set

let initial_state_formals kf (state:Cvalue.Model.t) =
  match kf.fundec with
    | Declaration (_, _, None, _) -> state
    | Declaration (_, _, Some l, _)
    | Definition ({ sformals = l }, _) ->
        List.fold_right
          Initial_state.initialize_var_using_type
          l
          state

let rec fold_left2_best_effort f acc l1 l2 =
  match l1,l2 with
  | _,[] -> acc
  | [],_ ->
      raise Eval_stmts.Wrong_function_type
  | (x1::r1),(x2::r2) -> fold_left2_best_effort f (f acc x1 x2) r1 r2

let actualize_formals kf state actuals check =
  let formals = Kernel_function.get_formals kf in
  let treat_one_formal acc (expr,_actual_val,actual_o) formal =
    check expr formal;
    let loc_without_size =
      Location_Bits.inject
        (Base.create_varinfo formal)
        (Ival.zero)
    in
    Cvalue.Model.paste_offsetmap CilE.warn_none_mode
      actual_o
      loc_without_size
      Int.zero
      (Int_Base.project (sizeof_vid formal))
      true
      acc
  in
  fold_left2_best_effort
    treat_one_formal
    state
    actuals
    formals

let () =
  Db.Value.add_formals_to_state :=
    (fun state kf exps ->
       try
         let compute_actual = Eval_stmts.compute_actual
           ~with_alarms:CilE.warn_none_mode (false, false) in
         let actuals = List.map (compute_actual state) exps in
         actualize_formals kf state actuals
	   (fun _ _ -> ())
       with Eval_stmts.Got_bottom -> Cvalue.Model.bottom)

let compute_using_declaration kf with_formals =
  Kf_state.mark_as_called kf;
  let stateset = Eval_logic.check_fct_preconditions kf with_formals in
  (* TODO: This is a hack. Use a function that checks preconditions without
   multiplying the states instead -- or compute_using_prototype several times *)
  let active_behaviors = Eval_logic.ActiveBehaviors.create stateset kf in
  let state_with_formals = State_set.join stateset in
  let retres_vi, result_state, thing =
    compute_using_prototype kf ~active_behaviors ~state_with_formals in
  let result_state =
    Eval_logic.check_fct_postconditions ~result:retres_vi kf ~active_behaviors
      ~init_state:(State_set.singleton state_with_formals)
      ~post_state:(State_set.singleton result_state)
      Normal
  in
  let result_state = State_set.join result_state in
  let result, result_state = match retres_vi with
    | None -> None, result_state
    | Some vi ->
      if not (Cvalue.Model.is_reachable result_state) then 
        (* This test prevents the call to Model.find_base that would
           raise Not_found in this case. *)
        None, result_state
      else
        let value_state = result_state in
        let retres_base = Base.create_varinfo vi in
        (Some (Cvalue.Model.find_base retres_base value_state)) ,
        Cvalue.Model.remove_base retres_base result_state
  in
  let formals = Kernel_function.get_formals kf in
  let result_state =
    List.fold_left
      (fun acc vi ->
        Cvalue.Model.remove_base
          (Base.create_varinfo vi)
          acc)
      result_state
      formals
  in
  result, result_state, thing

(* In the state [initial_state] globals and formals are present
   but locals of [kf] are not.*)
let compute_with_initial_state ~call_kinstr kf with_formals =
  match kf.fundec with
    | Declaration _ -> compute_using_declaration kf with_formals
    | Definition (f,_) ->
        let with_locals =
          List.fold_left
            (fun acc local ->
              Cvalue.Model.add_binding_not_initialized
                 acc
                 (Locations.loc_of_varinfo local))
            with_formals
            f.slocals
        in
        (* Remark: the pre-condition cannot talk about the locals. BUT
           check_fct_preconditions split the state into a stateset, hence
           it is simpler to apply it to the (unique) state with locals *)
        let initial_states= Eval_logic.check_fct_preconditions kf with_locals in
        compute_using_cfg ~call_kinstr kf initial_states

let compute_entry_point kf ~library =
  clear_call_stack ();
  Kf_state.mark_as_called kf;
  Value_parameters.feedback "Analyzing a%scomplete application starting at %a"
    (if library then "n in" else " ")
    Kernel_function.pretty kf;

  Separate.prologue();

  let initial_state_globals =
    if Db.Value.globals_use_supplied_state () then (
      let r = Db.Value.globals_state () in
      Value_parameters.feedback "Initial state supplied by user";
      Value_parameters.debug "@[<hov 0>Values of globals@\n%a@]"
        Db.Value.pretty_state_without_null r;
      r)
    else (
      Value_parameters.feedback "Computing initial state";
      let r = Db.Value.globals_state () in
      Value_parameters.feedback "Initial state computed";
      Value_parameters.result
        "@[<hov 0>Values of globals at initialization@\n%a@]"
        Db.Value.pretty_state_without_null r;
      r
    ) in
  if not (Db.Value.is_reachable initial_state_globals) then begin
    Value_parameters.result "Value analysis not started because globals \
                               initialization is not computable.";
    None, initial_state_globals, Locations.Location_Bits.Top_Param.empty
  end else begin
    Mark_noresults.run();

    let with_formals = match Db.Value.fun_get_args () with
      | None -> initial_state_formals kf initial_state_globals
      | Some actuals ->
          let formals = Kernel_function.get_formals kf in
          if (List.length formals) <> List.length actuals then
            raise Db.Value.Incorrect_number_of_arguments;
          let treat_one_formal f a =
            (), a, Builtins.offsetmap_of_value ~typ:f.vtype a
          in
          actualize_formals
            kf
            initial_state_globals
            (List.map2 treat_one_formal formals actuals)
	    (fun _ _ -> ())
    in
    Db.Value.Call_Value_Callbacks.apply (with_formals, [ kf, Kglobal ]);
    let result =
      compute_with_initial_state kf ~call_kinstr:Kglobal with_formals
    in
    Value_parameters.feedback "done for function %a"
      Kernel_function.pretty kf;
    Separate.epilogue();
    result
  end

let compute_call_to_cil_function kf _initial_state with_formals call_kinstr =
  let print_progress = Value_parameters.ValShowProgress.get() in
  if print_progress then
    Value_parameters.feedback
      "@[computing for function %a <- %a.@\nCalled from %a.@]"
      Kernel_function.pretty kf
      pretty_call_stack (call_stack ())
      pretty_loc_simply (CilE.current_stmt());
  let result = match kf.fundec with
    | Declaration (_,_,_,_) ->
        compute_using_declaration kf with_formals
    | Definition (def, _) ->
        Kf_state.mark_as_called kf;
        if Datatype.String.Set.mem
          def.svar.vname (Value_parameters.UsePrototype.get ())
        then
          compute_using_declaration kf with_formals
        else
          compute_with_initial_state kf ~call_kinstr with_formals
  in
  if print_progress then
    Value_parameters.feedback "Done for function %a"
      Kernel_function.pretty kf;
  result

(* Compute a call to a possible builtin. Returns [Some result], or [None]
   if the call must be computed using the Cil function *)
let compute_call_to_builtin kf initial_state actuals =
  let name = Kernel_function.get_name kf in
  try
    let name, override =
      (* Advanced builtins which override a Cil function with a Caml one, but
         use the Cil one as backup if the Caml one fails. (None by default) *)
      try
        let name = Value_parameters.BuiltinsOverrides.find name in
        Kf_state.mark_as_called kf;
        name, true
      with Not_found -> name, false
    in
    (* Standard builtins with constant names, e.g. Frama_C_cos *)
    let abstract_function = Builtins.find_builtin name in
    (try
       Some (abstract_function initial_state actuals)
     with Db.Value.Outside_builtin_possibilities ->
       if override then None
       else (
         do_degenerate None;
         raise Db.Value.Aborted
       )
    )
  with Not_found ->
    (* Special builtins, such as Frama_C_show_each_foo *)
    if Ast_info.can_be_cea_function name then
      (* A few special functions that are not registered in the builtin table *)
      if Ast_info.is_cea_dump_function name then
        Some (Builtins.dump_state initial_state)
      else if Ast_info.is_cea_alloc_with_validity name then
        Some (Builtins.alloc_with_validity initial_state actuals)
      else if Ast_info.is_cea_function name then
        Some (Builtins.dump_args name initial_state actuals)
      else if Ast_info.is_cea_dump_file_function name then
        Some (Builtins.dump_state_file name initial_state actuals)
      else
        None
    else None

let compute_call kf ~call_kinstr initial_state actuals =
  let with_formals = 
    actualize_formals kf initial_state actuals 
      (fun expr formal -> 
	if bitsSizeOf (typeOf expr) <> bitsSizeOf (formal.vtype)
	then raise Eval_stmts.Wrong_function_type)
  in
  Db.Value.merge_initial_state kf with_formals;
  let stack_without_call = for_callbacks_stack () in
  Db.Value.Call_Value_Callbacks.apply
    (with_formals, ((kf, call_kinstr) :: stack_without_call));
  match compute_call_to_builtin kf initial_state actuals with
    | Some r -> r
    | None ->
        compute_call_to_cil_function kf initial_state with_formals call_kinstr

let () = Eval_stmts.compute_call_ref := compute_call

let floats_ok () =
  let u = min_float /. 2. in
  let u = u /. 2. in
  0. < u && u < min_float

let cleanup () = 
  StmtCanReachCache.clear ()

let force_compute () =
  assert (floats_ok ());
  try
    let kf, library = Globals.entry_point () in
    ignore (compute_entry_point kf ~library);
    Db.Value.mark_as_computed ();
    cleanup ();
    (* Remove redundant alarms *)
    if Value_parameters.RmAssert.get() then !Db.Scope.rm_asserts ()
  with
  | Db.Value.Aborted ->
      cleanup ();
      (* This case is reached only if [do_degenerate] did not raise another
         exception to handle abortion properly. See the behavior of the GUI
         in case of degeneration to understand the machinery. *)
      Db.Value.mark_as_computed ();
      Value_parameters.abort
        "Degeneration occured:@\nresults are not correct for lines of code \
that can be reached from the degeneration point."
  | Globals.No_such_entry_point _ as exn -> raise exn
  | exn -> Db.Value.mark_as_computed (); raise exn

let _self =
  Db.register_compute "Value.compute"
    [ Db.Value.self ]
    Db.Value.compute
    (fun () -> if not (Db.Value.is_computed ()) then force_compute ())


(*
Local Variables:
compile-command: "make -C ../.."
End:
*)
