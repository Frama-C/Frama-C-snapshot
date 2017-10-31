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

module AB = Transfer_logic.ActiveBehaviors

(* Eval: under-approximation of the term.  Note that ACSL states
   that assigns clauses are evaluated in the pre-state.
   We skip [\result]: it is meaningless when evaluating the 'assigns' part,
   and a special treatment must be done in [from] clauses anyway. *)
let eval_assigns_from pre_state it =
  let term = it.it_content in
  if Logic_utils.is_result it.it_content then
    Locations.Zone.bottom
  else
    let eval_env = Eval_terms.env_assigns pre_state in
    let under, _ =
      Eval_terms.eval_tlval_as_zone_under_over
        ~alarm_mode:Eval_terms.Ignore ~for_writing:false eval_env term
    in
    under

(** Compute the validity status for [from] in [pre_state], assuming the
    entire clause is [assigns asgn \from from]. The inferred dependencies
    are [found_froms], while [asgn] evaluates to [assigns_zone]. *)
let check_from pre_state asgn assigns_zone from found_froms =
  let open Locations in
  let found_deps =
    let open Function_Froms in
    if Logic_utils.is_result asgn.it_content then
      found_froms.deps_return
    else
      Memory.find_precise found_froms.deps_table assigns_zone
  in
  let (indirect_deps,direct_deps) =
    let filter x = List.mem "indirect" x.it_content.term_name in
    List.partition filter from
  in
  (* Under-approximation of the union. *)
  let link zones = List.fold_left Zone.link Zone.bottom zones in
  let eval = eval_assigns_from pre_state in
  let stated_indirect_deps = link (List.map eval indirect_deps) in
  let stated_direct_deps = link (List.map eval direct_deps) in
  let found_direct_deps = found_deps.Function_Froms.Deps.data in
  let found_indirect_deps = found_deps.Function_Froms.Deps.indirect in
  let res_for_unknown txt =
    Value_parameters.debug "found_direct deps %a stated_direct_deps %a \
                            found_indirect_deps %a stated_indirect_deps %a"
      Zone.pretty found_direct_deps Zone.pretty stated_direct_deps
      Zone.pretty found_indirect_deps Zone.pretty stated_indirect_deps;
    "unknown (cannot validate "^txt^" dependencies)",
    Alarmset.Unknown
  in
  match (Zone.is_included found_direct_deps stated_direct_deps,
         Zone.is_included found_indirect_deps stated_indirect_deps) with
  | true,true -> "valid", Alarmset.True
  | false,true -> res_for_unknown "direct"
  | false,false -> res_for_unknown "direct and indirect"
  | true,false -> res_for_unknown "indirect"


(* Emits a status and a notification message. Returns the message callback. *)
let emit_status ppt status =
  Property_status.emit ~distinct:true Value_util.emitter ~hyps:[] ppt status


(* Display the message as result/warning depending on [status] *)
let msg_status status ?current ?once ?source fmt =
  if status = Alarmset.True then
    if Value_parameters.ValShowProgress.get ()
    then Value_parameters.result ?current ?once ?source fmt
    else Value_parameters.result ?current ?once ?source ~level:2 fmt
  else
  if Value_parameters.AlarmsWarnings.get () then
    Value_parameters.warning ?current ?once ?source fmt
  else
    Value_parameters.result
      ?current ?once ?source ~dkey:Value_parameters.dkey_alarm fmt


let pp_bhv fmt b =
  if not (Cil.is_default_behavior b)
  then Format.fprintf fmt ", behavior %s" b.b_name

let pp_header kf fmt b =
  Format.fprintf fmt "function %a%a"
    Kernel_function.pretty kf pp_bhv b


let conv_status = function
  | Alarmset.False -> Property_status.False_if_reachable;
  | Alarmset.True -> Property_status.True;
  | Alarmset.Unknown -> Property_status.Dont_know


let check_fct_assigns kf ab ~pre_state found_froms =
  let open Locations in
  let open Alarmset in
  let behaviors = Annotations.behaviors kf in
  (* Under-approximation of the union. *)
  let link zones = List.fold_left Zone.link Zone.bottom zones in
  let outputs = Function_Froms.outputs found_froms in
  let check_for_behavior b =
    let activity = AB.is_active ab b in
    match activity with
    | False -> ()
    | True | Unknown ->
      let pp_activity fmt activity = match activity with
        | False -> assert false
        | True -> ()
        (* If unknown, the error may be because we did not notice
           that the behavior is inactive.  *)
        | Unknown -> Format.fprintf fmt "(the behavior may be inactive)"
      in
      (match b.b_assigns with
       | WritesAny -> ()
       | Writes(assigns_deps) ->
         let bol = Property.Id_contract (Datatype.String.Set.empty,b) in
         let ip = Extlib.the (Property.ip_of_assigns kf Kglobal bol b.b_assigns)
         in
         let source = fst (Property.location ip) in
         (* First, check the assigns. *)
         let assigns = List.map fst assigns_deps in
         let assigns_zones = List.map (eval_assigns_from pre_state) assigns in
         let assigns_union = link assigns_zones in
         let status_txt, vstatus, status =
           if not (Zone.is_included outputs assigns_union)
           then (
             Value_parameters.debug "found_assigns %a stated_assigns %a"
               Zone.pretty outputs Zone.pretty assigns_union;
             "unknown", Unknown, Property_status.Dont_know)
           else "valid", True, Property_status.True
         in
         msg_status vstatus ~once:true ~source
           "%a: assigns got status %s.%a%t"
           (pp_header kf) b
           status_txt
           pp_activity activity
           Value_util.pp_callstack;
         emit_status ip status;
         (* Now, checks the individual froms. *)
         let check_from ((asgn,deps) as from) assigns_zone =
           match deps with
           | FromAny -> ()
           | From deps ->
             let status_txt, status =
               check_from pre_state asgn assigns_zone deps found_froms
             in
             let ip = Extlib.the (Property.ip_of_from kf Kglobal bol from) in
             let source = fst (asgn.it_content.term_loc) in
             msg_status status ~once:true ~source
               "%a: \\from ... part in assign clause got status %s.%a%t"
               (pp_header kf) b
               status_txt
               pp_activity activity
               Value_util.pp_callstack;
             emit_status ip (conv_status status)
         in
         List.iter2 check_from assigns_deps assigns_zones)
  in List.iter check_for_behavior behaviors

let verify_assigns_from kf ~pre froms =
  let funspec = Annotations.funspec kf in
  let env = Eval_terms.env_pre_f ~pre () in
  let eval_predicate pred =
    match Eval_terms.eval_predicate env pred with
    | Eval_terms.True -> Alarmset.True
    | Eval_terms.False -> Alarmset.False
    | Eval_terms.Unknown -> Alarmset.Unknown
  in
  let ab = AB.create eval_predicate funspec in
  check_fct_assigns kf ab ~pre_state:pre froms;;

Db.Value.verify_assigns_froms := verify_assigns_from;;
