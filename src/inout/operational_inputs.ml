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

open Cil_types
open Locations

(* Computation of over-approximed operational inputs:
   An acurate computation of these inputs needs the computation of
   under-approximed outputs.
*)

type tt = Inout_type.tt = {
  over_inputs: Locations.Zone.t;
  over_inputs_if_termination: Locations.Zone.t;
  under_outputs_if_termination: Locations.Zone.t;
  over_outputs: Locations.Zone.t;
  over_outputs_if_termination: Locations.Zone.t;
}

let top = {
  over_inputs = Zone.top;
  over_inputs_if_termination = Zone.top;
  under_outputs_if_termination = Zone.bottom;
  over_outputs = Zone.top;
  over_outputs_if_termination = Zone.top;
}

(* [_if_termination] fields of the type above, which are the one propagated by
   the dataflow analysis of this module. It is meaningless to store the other
   ones, as they come from branches that are by construction not propagated
   until the end by the dataflow. *)
type compute_t = {
  over_inputs_d : Zone.t ;
  under_outputs_d : Zone.t;
  over_outputs_d: Zone.t;
}

(* Initial value for the computation *)
let empty = {
  over_inputs_d = Zone.bottom;
  under_outputs_d = Zone.bottom;
  over_outputs_d = Zone.bottom;
}

let bottom = {
  over_inputs_d = Zone.bottom;
  under_outputs_d = Zone.top;
  over_outputs_d = Zone.bottom;
}

let equal ct1 ct2 =
  Zone.equal ct1.over_inputs_d ct2.over_inputs_d &&
  Zone.equal ct1.under_outputs_d ct2.under_outputs_d &&
  Zone.equal ct1.over_outputs_d ct2.over_outputs_d

let join c1 c2 = {
  over_inputs_d = Zone.join c1.over_inputs_d c2.over_inputs_d;
  under_outputs_d = Zone.meet c1.under_outputs_d c2.under_outputs_d;
  over_outputs_d = Zone.join c1.over_outputs_d c2.over_outputs_d;
}

let is_included c1 c2 =
  Zone.is_included c1.over_inputs_d   c2.over_inputs_d &&
  Zone.is_included c2.under_outputs_d c1.under_outputs_d &&
  Zone.is_included c1.over_outputs_d  c2.over_outputs_d

let join_and_is_included smaller larger =
  let join = join smaller larger in
  join, equal join larger
;;

let catenate c1 c2 = {
  over_inputs_d =
    Zone.join c1.over_inputs_d (Zone.diff c2.over_inputs_d c1.under_outputs_d);
  under_outputs_d = Zone.link c1.under_outputs_d c2.under_outputs_d;
  over_outputs_d = Zone.join  c1.over_outputs_d c2.over_outputs_d;
}


let externalize_zone ~with_formals kf =
  Zone.filter_base
    (!Db.Semantic_Callgraph.accept_base ~with_formals ~with_locals:false kf)

(* This code evaluates an assigns, computing in particular a sound approximation
   of sure outputs. For an assigns [locs_out \from locs_from], the process
   is the following:
   - evaluate locs_out to locations; discard those that are not exact, as
     we cannot guarantee that they are always assigned
   - evaluate locs_from, as a zone (no need for locations)
   - compute the difference between the out and the froms, ie remove the
     zones that are such that [z \from z] holds

   (Note: large parts of this code are inspired/redundant with
   [assigns_to_zone_foobar_state] in Value/register.ml)
*)
let eval_assigns kf state assigns =
  let treat_one_zone acc (out, froms as asgn) = (* treat a single assign *)
    (* Return a list of independent output zones, plus a zone indicating
       that the zone has been overwritten in a sure way *)
    let clean_deps =
      Locations.Zone.filter_base
        (function
           | Base.Var (v, _) | Base.Initialized_Var (v, _) ->
               not (Kernel_function.is_formal v kf)
           | Base.CLogic_Var _ | Base.Null | Base.String _ -> true)
    in
    let outputs, deps =
      try
        if Logic_utils.is_result out.it_content
        then [], Zone.bottom
        else
          let locs_out, deps = !Db.Properties.Interp.loc_to_locs ~result:None
            state out.it_content
          in
          let conv loc =
            let z = enumerate_valid_bits ~for_writing:true loc in
            let sure = Locations.cardinal_zero_or_one loc in
            z, sure
          in
          List.map conv locs_out, clean_deps deps
      with Invalid_argument _ ->
        Inout_parameters.warning ~current:true ~once:true
          "Failed to interpret assigns clause '%a'" Printer.pp_term out.it_content;
        [Locations.Zone.top, false], Locations.Zone.top
    in
    (* Compute all inputs as a zone *)
    let inputs =
      try
        match froms with
          | FromAny -> Zone.top
          | From l ->
              let aux acc { it_content = from } =
                let locs, deps =
                  !Db.Properties.Interp.loc_to_locs None state from in
                let acc = Zone.join (clean_deps deps) acc in
                List.fold_left
                  (fun acc loc ->
                     let z = enumerate_valid_bits ~for_writing:false loc in
                     Zone.join z acc
                  ) acc locs
              in
              List.fold_left aux deps l
      with Invalid_argument _ ->
        Inout_parameters.warning ~current:true ~once:true
          "Failed to interpret inputs in assigns clause '%a'"
          Printer.pp_from asgn;
        Zone.top
    in
    (* Fuse all outputs. An output is sure if it was certainly overwritten,
       and if it is not amongst its from *)
    let extract_sure (sure_out, all_out) (out, exact) =
      let all_out' = Zone.join out all_out in
      if exact then
        let sure = Locations.Zone.diff out inputs in
        Zone.join sure sure_out, all_out'
      else
        sure_out, all_out'
    in
    let sure_out, all_out =
      List.fold_left extract_sure (Zone.bottom, Zone.bottom) outputs
    in (* Join all three kinds of locations. The use a join (not a meet) for
          under_outputs is correct here (and in fact required for precision) *)
    {
      under_outputs_d = Zone.join acc.under_outputs_d sure_out;
      over_inputs_d = Zone.join acc.over_inputs_d inputs;
      over_outputs_d = Zone.join acc.over_outputs_d all_out;
    }
  in
  match assigns with
    | WritesAny -> top
    | Writes l  ->
        let init = { bottom with under_outputs_d = Zone.bottom } in
        let r = List.fold_left treat_one_zone init l in {
          over_inputs = r.over_inputs_d;
          over_inputs_if_termination = r.over_inputs_d;
          under_outputs_if_termination = r.under_outputs_d;
          over_outputs = r.over_outputs_d;
          over_outputs_if_termination = r.over_outputs_d;
        }

let compute_using_prototype_state state kf =
  let behaviors = !Db.Value.valid_behaviors kf state in
  let assigns = Ast_info.merge_assigns behaviors in
  eval_assigns kf state assigns

let compute_using_prototype ?stmt kf =
  let state = Cumulative_analysis.specialize_state_on_call ?stmt kf in
  compute_using_prototype_state state kf

(* Results of this module, consolidated by functions. Formals and locals
   are stored *)
module Internals =
  Kernel_function.Make_Table(Inout_type)
    (struct
       let name = "Internal inouts full"
       let dependencies = [ Db.Value.self ]
       let size = 17
     end)

module CallsiteHash = Value_types.Callsite.Hashtbl

(* Results of an an entire call, represented by a pair (stmt, kernel_function]).
   This table is filled by the [-inout-callwise] option, or for functions for
   which only the specification is used. *)
module CallwiseResults =
  State_builder.Hashtbl
  (CallsiteHash)
  (Inout_type)
  (struct
    let size = 17
    let dependencies = [Internals.self;
                        Inout_parameters.ForceCallwiseInout.self]
    let name = "Operational_inputs.CallwiseResults"
   end)

module Computer(Fenv:Dataflows.FUNCTION_ENV)(X:sig
  val version: string (* Callwise or functionwise *)
  val kf: kernel_function (* Function being analyzed *)
  val stmt_state: stmt -> Db.Value.state (* Memory state at the given stmt *)
  val at_call: stmt -> kernel_function -> Inout_type.t (* Results of the
      analysis for the given call. Must not contain locals or formals *)
end) = struct

  let non_terminating_callees_inputs = ref Zone.bottom
  let non_terminating_callees_outputs = ref Zone.bottom

  type t = compute_t

  let pretty fmt x =
    Format.fprintf fmt
      "@[Over-approximated operational inputs: %a@]@\n\
       @[Under-approximated operational outputs: %a@]"
      Zone.pretty x.over_inputs_d
      Zone.pretty x.under_outputs_d

  let bottom = bottom
  let join_and_is_included = join_and_is_included
  let join = join
  let is_included = is_included


  (* Transfer function on expression. *)
  let transfer_exp s exp data =
    let state = X.stmt_state s in
    let inputs = !Db.From.find_deps_no_transitivity_state state exp in
    let new_inputs = Zone.diff inputs data.under_outputs_d in
    {data with over_inputs_d = Zone.join data.over_inputs_d new_inputs}
  ;;

  (* Transfer function on instructions. *)
  let transfer_instr stmt (i: instr) (data: t) =
    let state = X.stmt_state stmt in
    let add_out lv deps data =
      let deps, new_outs, exact =
        !Db.Value.lval_to_zone_with_deps_state state
          ~deps:(Some deps) ~for_writing:true lv
      in
      let new_inputs = Zone.diff deps data.under_outputs_d in
      let new_sure_outs =
        if exact then
          (* There is only one modified zone. So, this is an exact output.
             Add it into the under-approximed outputs. *)
          Zone.link data.under_outputs_d new_outs
        else data.under_outputs_d
      in {
        under_outputs_d = new_sure_outs;
        over_inputs_d = Zone.join data.over_inputs_d new_inputs;
        over_outputs_d = Zone.join data.over_outputs_d new_outs }
    in
    match i with
    | Set (lv, exp, _) ->
             let state = X.stmt_state stmt in
             let e_inputs = 
	       !Db.From.find_deps_no_transitivity_state state exp 
	     in
             add_out lv e_inputs data

    | Call (lvaloption,funcexp,argl,_) ->
        let state = X.stmt_state stmt in
             let funcexp_inputs, called =
               !Db.Value.expr_to_kernel_function_state
                 ~deps:(Some Zone.bottom)
                 state
                 funcexp
             in

             let acc_funcexp_arg_inputs =
               (* add the inputs of [argl] to the inputs of the
                  function expression *)
               List.fold_right
                 (fun arg inputs ->
                    let arg_inputs = !Db.From.find_deps_no_transitivity_state
                      state arg
                    in Zone.join inputs arg_inputs)
                 argl
                 funcexp_inputs
             in
             let data =
               catenate
                 data
                 { over_inputs_d = acc_funcexp_arg_inputs ;
                   under_outputs_d = Zone.bottom;
                   over_outputs_d = Zone.bottom; }
             in
             let for_functions =
               Kernel_function.Hptset.fold
                 (fun kf acc  ->
                    let res = X.at_call stmt kf in
                    non_terminating_callees_inputs :=
                      Zone.join
                        !non_terminating_callees_inputs
                        (Zone.diff res.Inout_type.over_inputs
                           data.under_outputs_d);
                    non_terminating_callees_outputs :=
                      Zone.join
                        !non_terminating_callees_outputs
                        res.over_outputs;
                    let for_function = {
                      over_inputs_d = res.over_inputs_if_termination;
                      under_outputs_d = res.under_outputs_if_termination;
                      over_outputs_d = res.over_outputs_if_termination;
                    } in
                    join for_function acc)
                 called
                 bottom
             in
             let result = catenate data for_functions in
             let result =
               (* Treatment for the possible assignment of the call result *)
               (match lvaloption with
                | None -> result
                | Some lv -> add_out lv Zone.bottom result)
             in result

    | _ -> data
  ;;

  (* transfer_guard: gets the state obtained after evaluating the
     condition, and split the state according to the truth value of
     the condition. In this case, we just make sure that dead
     edges get bottom, instead of the input state. *)
  let transfer_guard stmt e t =
    let state = X.stmt_state stmt in
    let v_e = !Db.Value.eval_expr ~with_alarms:CilE.warn_none_mode state e in
    let t1 = Cil.unrollType (Cil.typeOf e) in
    let do_then, do_else =
      if Cil.isIntegralType t1 || Cil.isPointerType t1
      then Cvalue.V.contains_non_zero v_e,
           Cvalue.V.contains_zero v_e
      else true, true (* TODO: a float condition is true iff != 0.0 *)
    in
    (if do_then then t else bottom),
    (if do_else then t else bottom)
  ;;

  let return_data = ref bottom;;

  let transfer_stmt s data =
    let map_on_all_succs new_data = List.map (fun x -> (x,new_data)) s.succs in
    match s.skind with
    | Instr i -> map_on_all_succs (transfer_instr s i data)

    | If(exp,_,_,_) ->
      let data = transfer_exp s exp data in
      Dataflows.transfer_if_from_guard transfer_guard s data
    | Switch(exp,_,_,_) ->
      let data = transfer_exp s exp data in
      Dataflows.transfer_switch_from_guard transfer_guard s data

    | Return(Some exp,_) -> return_data := transfer_exp s exp data;
      assert (s.succs == []); []
    | Return(None,_) -> return_data := data;
      assert (s.succs == []); []

    | UnspecifiedSequence _ | Loop _ | Block _
    | Goto _ | Break _ | Continue _
    | TryExcept _ | TryFinally _
      -> map_on_all_succs data
  ;;

  (* Note: Not sure this adds anything to the precision (or
     efficiency) once we have tested the guards. The difference does
     not show up in the tests. *)
  let transfer_stmt s data =
    if Db.Value.is_reachable (X.stmt_state s)
    then transfer_stmt s data
    else []
  ;;

  let init = [(Kernel_function.find_first_stmt Fenv.kf), empty];;

  (* We want to compute the in/out for all terminating and
     non-terminating points of the function. It is computed by joining
     the result over all statements, not all "end" statements (this is
     acceptable because if s is before s' in the order of evaluation,
     then the inouts of s will be included in those of s'; hence the
     joining "too many statements" does not degrade the result). This
     takes care of in-out of statements in infinite loops.

     However, the in-out done in functions that do not terminate is
     not taken into account by this. That is why they are stored
     separately during the analysis in non_terminating_callees_*. *)
  let end_dataflow before =
    let res_if_termination = !return_data in
    Array.iteri (fun _ data ->
      non_terminating_callees_inputs :=
        Zone.join data.over_inputs_d !non_terminating_callees_inputs;
      non_terminating_callees_outputs :=
        Zone.join data.over_outputs_d !non_terminating_callees_outputs;
    ) before;
    {
      over_inputs_if_termination = res_if_termination.over_inputs_d;
      under_outputs_if_termination = res_if_termination.under_outputs_d ;
      over_outputs_if_termination = res_if_termination.over_outputs_d;

      over_inputs = Zone.join !non_terminating_callees_inputs res_if_termination.over_inputs_d;
      over_outputs = Zone.join !non_terminating_callees_outputs res_if_termination.over_outputs_d;
    }

end


let externalize ~with_formals kf v =
  let filter = externalize_zone ~with_formals kf in
  Inout_type.map filter v

let compute_externals_using_prototype ?stmt kf =
  let internals = compute_using_prototype ?stmt kf in
  externalize ~with_formals:false kf internals

let get_internal_aux ?stmt kf =
  match stmt with
    | None -> !Db.Operational_inputs.get_internal kf
    | Some stmt ->
        try CallwiseResults.find (kf, Kstmt stmt)
        with Not_found ->
          if !Db.Value.use_spec_instead_of_definition kf then
            compute_using_prototype ~stmt kf
          else !Db.Operational_inputs.get_internal kf

let get_external_aux ?stmt kf =
  match stmt with
    | None -> !Db.Operational_inputs.get_external kf
    | Some stmt ->
        try
          let internals = CallwiseResults.find (kf, Kstmt stmt) in
          externalize ~with_formals:false kf internals
        with Not_found ->
          if !Db.Value.use_spec_instead_of_definition kf then
            let r = compute_externals_using_prototype ~stmt kf in
            CallwiseResults.add (kf, Kstmt stmt) r;
            r
          else !Db.Operational_inputs.get_external kf


module Callwise = struct

  let compute_callwise () =
    Inout_parameters.ForceCallwiseInout.get () ||
      Dynamic.Parameter.Bool.get "-memexec-all" ()

  let merge_call_in_local_table call local_table v =
    let prev =
      try CallsiteHash.find local_table call
      with Not_found -> Inout_type.bottom
    in
    let joined = Inout_type.join v prev in
    CallsiteHash.replace local_table call joined

  let merge_call_in_global_tables (kf, _ as call) v =
    (* Global callwise table *)
    let prev =
      try CallwiseResults.find call
      with Not_found -> Inout_type.bottom
    in
    CallwiseResults.replace call (Inout_type.join v prev);
    (* Global, kf-indexed, table *)
    let prev =
      try Internals.find kf
      with Not_found -> Inout_type.bottom
    in
    Internals.replace kf (Inout_type.join v prev);
  ;;

  let merge_local_table_in_global_ones =
    CallsiteHash.iter merge_call_in_global_tables
  ;;


  let call_inout_stack = ref []

  let call_for_callwise_inout (state, call_stack) =
    if compute_callwise () then begin
      let (current_function, ki as call_site) = List.hd call_stack in
      if not (!Db.Value.use_spec_instead_of_definition current_function) then
        let table_current_function = CallsiteHash.create 7 in
        call_inout_stack :=
          (current_function, table_current_function) :: !call_inout_stack
      else 
        let inout = compute_using_prototype_state state current_function in
        if ki = Kglobal 
        then merge_call_in_global_tables call_site inout
        else
          try
            let _above_function, table = 
              try List.hd !call_inout_stack 
              with Failure "hd" -> assert false
            in
            merge_call_in_local_table call_site table inout;
          with Failure "hd" ->
            Inout_parameters.fatal "inout: empty stack"
              Kernel_function.pretty current_function
    end

  module MemExec =
    State_builder.Hashtbl
      (Datatype.Int.Hashtbl)
      (Inout_type)
      (struct
         let size = 17
         let dependencies = [Internals.self]
         let name = "Operational_inputs.MemExec"
   end)


  let end_record call_stack inout =
    merge_local_table_in_global_ones (snd (List.hd !call_inout_stack));

    let (current_function, _ as call_site) = List.hd call_stack in
    (* pop + record in top of stack the inout of function that just finished*)
    match !call_inout_stack with
      | (current_function2, _) :: (((_caller, table) :: _) as tail) ->
          if current_function2 != current_function then
            Inout_parameters.fatal "callwise inout %a != %a@."
              Kernel_function.pretty current_function (* g *)
              Kernel_function.pretty current_function2 (* f *);
          call_inout_stack := tail;
          merge_call_in_local_table call_site table inout;

      | _ ->  (* the entry point, probably *)
          merge_call_in_global_tables call_site inout;
          call_inout_stack := [];
          CallwiseResults.mark_as_computed ()

  let compute_call_from_value_states kf states =
    let module Fenv = (val Dataflows.function_env kf: Dataflows.FUNCTION_ENV) in
    let module Computer = Computer(Fenv)(
      struct
        let version = "callwise"
        let kf = kf

        let stmt_state stmt =
          try Cil_datatype.Stmt.Hashtbl.find states stmt
          with Not_found -> Cvalue.Model.bottom

        let at_call stmt kf =
          let _cur_kf, table = List.hd !call_inout_stack in
          try
            let with_internals = CallsiteHash.find table (kf, Kstmt stmt) in
            match kf.fundec with
              | Definition (fundec, _) ->
                  let filter = Zone.filter_base
                    (fun b -> not (Base.is_formal_or_local b fundec))
                  in
                  Inout_type.map filter with_internals
              | _ -> with_internals
          with Not_found -> Inout_type.bottom
      end) in
    let module Compute = Dataflows.Simple_forward(Fenv)(Computer) in
    Computer.end_dataflow Compute.before

  let record_for_callwise_inout ((call_stack: Db.Value.callstack), value_res) =
    if compute_callwise () then
      let inout = match value_res with
        | Value_types.Normal states | Value_types.NormalStore (states, _) ->
            let kf = fst (List.hd call_stack) in
            let inout =
              if !Db.Value.no_results (Kernel_function.get_definition kf) then
                top
              else
                compute_call_from_value_states kf (Lazy.force states)
            in
            Db.Operational_inputs.Record_Inout_Callbacks.apply
              (call_stack, inout);
            (match value_res with
               | Value_types.NormalStore (_, memexec_counter) ->
                   MemExec.replace memexec_counter inout
               | _ -> ());
            inout

        | Value_types.Reuse counter ->
            MemExec.find counter
      in
      end_record call_stack inout


  (* Register our callbacks inside the value analysis *)

  let add_hooks () =
    Db.Value.Record_Value_Callbacks_New.extend_once record_for_callwise_inout;
    Db.Value.Call_Value_Callbacks.extend_once call_for_callwise_inout

  let () = Inout_parameters.ForceCallwiseInout.add_update_hook
    (fun _bold bnew -> if bnew then add_hooks ())

  let () = Inout_parameters.ForceCallwiseInout.add_set_hook
    (fun bold bnew ->
      if bold = false && bnew then
        Project.clear
          ~selection:(State_selection.with_dependencies Db.Value.self) ();
    )

end


(* Functionwise version of the computations. *)
module FunctionWise = struct

  (* Stack of function being processed *)
  let call_stack : kernel_function Stack.t = Stack.create ()

  let compute_internal_using_cfg kf =
    try
      let module Fenv =
            (val Dataflows.function_env kf: Dataflows.FUNCTION_ENV)
      in
      let module Computer = Computer(Fenv)(struct
        let version = "functionwise"
        let kf = kf
        let stmt_state = Db.Value.get_stmt_state
        let at_call stmt kf = get_external_aux ~stmt kf
      end) in
      Stack.iter
        (fun g -> if kf == g then begin
          if Db.Value.ignored_recursive_call kf then
            Inout_parameters.warning ~current:true
              "During inout context analysis of %a:@ \
                  ignoring probable recursive call."
              Kernel_function.pretty kf;
          raise Exit
        end)
        call_stack;
      Stack.push kf call_stack;

      let module Compute = Dataflows.Simple_forward(Fenv)(Computer) in
      let result = Computer.end_dataflow Compute.before in
      ignore (Stack.pop call_stack);
      result

    with Exit -> Inout_type.bottom (*TODO*) (*{
    Inout_type.over_inputs_if_termination = empty.over_inputs_d ;
    under_outputs_if_termination = empty.under_outputs_d;
    over_inputs = empty.over_inputs_d;
    over_outputs = empty.over_outputs_d;
    over_outputs_if_termination = empty.over_outputs_d;
  }*)

  let compute_internal_using_cfg kf =
    if !Db.Value.no_results (Kernel_function.get_definition kf) then
      top
    else begin
      Inout_parameters.feedback ~level:2 "computing for function %a%s"
        Kernel_function.pretty kf
        (let s = ref "" in
         Stack.iter
           (fun kf -> s := !s^" <-"^
             (Pretty_utils.sfprintf "%a" Kernel_function.pretty kf))
           call_stack;
         !s);
      let r = compute_internal_using_cfg kf in
      Inout_parameters.feedback ~level:2 "done for function %a"
        Kernel_function.pretty kf;
      r
    end
end


let get_internal =
  Internals.memo
    (fun kf ->
       !Db.Value.compute ();
       try Internals.find kf (* If [-inout-callwise] is set, the results may
                              have been computed by the call to Value.compute *)
       with
         | Not_found ->
             if!Db.Value.use_spec_instead_of_definition kf then
               compute_using_prototype kf
             else
               FunctionWise.compute_internal_using_cfg kf
    )

let raw_externals ~with_formals kf =
  let filter = externalize ~with_formals kf in
  filter (get_internal kf)

module Externals =
  Kernel_function.Make_Table(Inout_type)
    (struct
       let name = "External inouts full"
       let dependencies = [ Internals.self ]
       let size = 17
     end)
let get_external = Externals.memo (raw_externals ~with_formals:false)
let compute_external kf = ignore (get_external kf)



module Externals_With_Formals =
  Kernel_function.Make_Table(Inout_type)
    (struct
       let name = "External inouts with formals full"
       let dependencies = [ Internals.self ]
       let size = 17
     end)
let get_external_with_formals =
  Externals_With_Formals.memo (raw_externals ~with_formals:true)
let compute_external_with_formals kf = ignore (get_external_with_formals kf)


let pretty_operational_inputs_internal fmt kf =
  Format.fprintf fmt "@[InOut (internal) for function %a:@\n%a@]@\n"
    Kernel_function.pretty kf
    Inout_type.pretty_operational_inputs (get_internal kf)

let pretty_operational_inputs_external fmt kf =
  Format.fprintf fmt "@[InOut for function %a:@\n%a@]@\n"
    Kernel_function.pretty kf
    Inout_type.pretty_operational_inputs (get_external kf)

let pretty_operational_inputs_external_with_formals fmt kf =
  Format.fprintf fmt "@[InOut (with formals) for function %a:@\n%a@]@\n"
    Kernel_function.pretty kf
    Inout_type.pretty_operational_inputs (get_external_with_formals kf)



let () =
  Db.Operational_inputs.self_internal := Internals.self;
  Db.Operational_inputs.self_external := Externals.self;
  Db.Operational_inputs.get_internal := get_internal;
  Db.Operational_inputs.get_external := get_external;
  Db.Operational_inputs.get_internal_precise := get_internal_aux;
  Db.Operational_inputs.compute := compute_external;
  Db.Operational_inputs.display := pretty_operational_inputs_internal


(*
Local Variables:
compile-command: "make -C ../.."
End:
*)
