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
open Eval_terms

(* Statuses for code annotations and function contracts *)

(* Emits a status, possibly multiple times. *)
let emit_status ppt status =
  Property_status.emit ~distinct:true Value_util.emitter ~hyps:[] ppt status

let has_requires spec =
  let behav_has_requires b = b.b_requires <> [] in
  List.exists behav_has_requires spec.spec_behavior

let code_annotation_text ca =
  match ca.annot_content with
  | AAssert _ ->  "assertion"
  | AInvariant _ ->  "loop invariant"
  | APragma _  | AVariant _ | AAssigns _ | AAllocation _ | AStmtSpec _ 
  | AExtended _  ->
    assert false (* currently not treated by Value *)

(* location of the given code annotation. If unknown, use the location of the
   statement instead. *)
let code_annotation_loc ca stmt =
  match Cil_datatype.Code_annotation.loc ca with
  | Some loc when not (Cil_datatype.Location.(equal loc unknown)) -> loc
  | _ -> Cil_datatype.Stmt.loc stmt


let mark_unreachable () =
  let mark ppt =
    if not (Property_status.automatically_proven ppt) then begin
      Value_parameters.debug "Marking property %a as dead"
        Description.pp_property ppt;
      let emit =
        Property_status.emit ~distinct:true Value_util.emitter ~hyps:[]
      in
      let reach_p = Property.ip_reachable_ppt ppt in
      emit ppt Property_status.True;
      emit_status reach_p Property_status.False_and_reachable
    end
  in
  (* Mark standard code annotations *)
  let do_code_annot stmt _emit ca =
    if not (Db.Value.is_reachable_stmt stmt) then begin
      let kf = Kernel_function.find_englobing_kf stmt in
      let ppts = Property.ip_of_code_annot kf stmt ca in
      List.iter mark ppts;
    end
  in
  (* Mark preconditions of dead calls *)
  let unreach = object
    inherit Visitor.frama_c_inplace

    method! vstmt_aux stmt =
      if not (Db.Value.is_reachable_stmt stmt) then begin
        let mark_status kf =
          (* Do not put "unreachable" statuses on preconditions of
             functions overridden by builtins. We do not evaluate those
             preconditions on reachable calls, and the consolidation
             gives very bad results when reachable and unreachable calls
             coexist (untried+dead -> unknown). *)
          if Builtins.find_builtin_override kf = None
          then begin
            (* Setup all precondition statuses for [kf]: maybe it has
               never been called anywhere. *)
            Statuses_by_call.setup_all_preconditions_proxies kf;
            (* Now mark the statuses at this particular statement as dead*)
            let preconds =
              Statuses_by_call.all_call_preconditions_at
                ~warn_missing:false kf stmt
            in
            List.iter (fun (_, p) -> mark p) preconds
          end
        in
        match stmt.skind with
        | Instr (Call (_, e, _, _)) ->
          Extlib.may mark_status (Kernel_function.get_called e)
        | Instr(Local_init(_, ConsInit(f,_,_),_)) ->
          mark_status (Globals.Functions.get f)
        | _ -> ()
      end;
      Cil.DoChildren

    method! vinst _ = Cil.SkipChildren
  end
  in
  Annotations.iter_all_code_annot do_code_annot;
  Visitor.visitFramacFile unreach (Ast.get ())

let mark_rte () =
  let _, mem, _ = !Db.RteGen.get_memAccess_status () in
  let _, arith, _ = !Db.RteGen.get_divMod_status () in
  let _, signed_ovf, _ = !Db.RteGen.get_signedOv_status () in
  let _, unsigned_ovf, _ = !Db.RteGen.get_unsignedOv_status () in
  let _, signed_downcast, _ = !Db.RteGen.get_signed_downCast_status () in
  let _, unsigned_downcast, _ = !Db.RteGen.get_unsignedDownCast_status () in
  let _, pointer_call, _ = !Db.RteGen.get_pointerCall_status () in
  let _, float_to_int, _ = !Db.RteGen.get_float_to_int_status () in
  let _, finite_float, _ = !Db.RteGen.get_finite_float_status () in
  let b_signed_ovf = Kernel.SignedOverflow.get () in
  let b_unsigned_ovf = Kernel.UnsignedOverflow.get () in
  let b_signed_downcast = Kernel.SignedDowncast.get () in
  let b_unsigned_downcast = Kernel.UnsignedDowncast.get () in
  Globals.Functions.iter
    (fun kf ->
      if !Db.Value.is_called kf then (
        mem kf true;
        arith kf true;
        pointer_call kf true;
        if b_signed_ovf then signed_ovf kf true;
        if b_unsigned_ovf then unsigned_ovf kf true;
        if b_signed_downcast then signed_downcast kf true;
        if b_unsigned_downcast then unsigned_downcast kf true;
        float_to_int kf true;
        finite_float kf true;
      )
    )

let c_labels kf cs =
  if !Db.Value.use_spec_instead_of_definition kf then
    Cil_datatype.Logic_label.Map.empty
  else
    let fdec = Kernel_function.get_definition kf in
    let aux acc stmt =
      if stmt.labels != [] then
        try
          let hstate = Db.Value.Table_By_Callstack.find stmt in
          let state = Value_types.Callstack.Hashtbl.find hstate cs in
          Cil_datatype.Logic_label.Map.add (StmtLabel (ref stmt)) state acc
        with Not_found -> acc
      else acc
    in
    List.fold_left aux Cil_datatype.Logic_label.Map.empty fdec.sallstmts

(* Evaluates [p] at [stmt], using per callstack states for maximum precision. *)
(* TODO: we can probably factor some code with the GUI *)
let eval_by_callstack kf stmt p =
  (* This is actually irrelevant for alarms: they never use \old *)
  let pre = Db.Value.get_initial_state kf in
  let aux_callstack callstack state acc_status =
    let c_labels = c_labels kf callstack in
    let env = Eval_terms.env_annot ~c_labels ~pre ~here:state () in
    let status = Eval_terms.eval_predicate env p in
    let join = Eval_terms.join_predicate_status in
    match Bottom.join join acc_status (`Value status) with
    | `Value Unknown -> raise Exit (* shortcut *)
    | _ as r -> r
  in
  match Db.Value.get_stmt_state_callstack ~after:false stmt with
  | None -> (* dead; ignore, those will be marked 'unreachable' elsewhere *)
    Unknown
  | Some states ->
    try
      match Value_types.Callstack.Hashtbl.fold aux_callstack states `Bottom with
      | `Bottom -> Eval_terms.Unknown (* probably never reached *)
      | `Value status -> status
    with Exit -> Eval_terms.Unknown

(* Detection of terms \at(_, L) where L is a C label *)
class contains_c_at = object
  inherit Visitor.frama_c_inplace

  method! vterm t = match t.term_node with
    | Tat (_, StmtLabel _) -> raise Exit
    | _ -> Cil.DoChildren
end

let contains_c_at ca =
  let vis = new contains_c_at in
  let loc = Cil.CurrentLoc.get () in
  let r =
    try
      ignore (Visitor.visitFramacCodeAnnotation vis ca);
      false
    with Exit -> true
  in
  Cil.CurrentLoc.set loc;
  r

(* Re-evaluate all alarms, and see if we can put a 'green' or 'red' status,
   which would be more precise than those we have emitted during the current
   analysis. *)
let mark_green_and_red () =
  let do_code_annot stmt _e ca  =
    (* We reevaluate only alarms, in the hope that we can emit an 'invalid'
       status, or user assertions that mention a C label. The latter are
       currently skipped during evaluation. *)
    if contains_c_at ca || (Alarms.find ca <> None) then
      match ca.annot_content with
      | AAssert (_, p) | AInvariant (_, true, p) ->
        let loc = code_annotation_loc ca stmt in
        Cil.CurrentLoc.set loc;
        let kf = Kernel_function.find_englobing_kf stmt in
        let ip = Property.ip_of_code_annot_single kf stmt ca in
        (* This status is exact: we are _not_ refining the statuses previously
           emitted, but writing a synthetic more precise status. *)
        let distinct = false in
        let emit status =
          let status, text_status = match status with
            | `True -> Property_status.True, "valid"
            | `False -> Property_status.False_if_reachable, "invalid"
          in
          Property_status.emit ~distinct Value_util.emitter ~hyps:[] ip status;
          let source = fst loc in
          let text_ca = code_annotation_text ca in
          Value_parameters.result ~once:true ~source "%s%a got final status %s."
            text_ca Description.pp_named p text_status;
        in
        begin
          match eval_by_callstack kf stmt p with
          | Eval_terms.False -> emit `False
          | Eval_terms.True -> (* should not happen for an alarm that has been
              emitted during this Value analysis. However, this is perfectly
              possible for an 'old' alarm. *)
            emit `True
          | Eval_terms.Unknown -> ()
        end
      | AInvariant (_, false, _) | AStmtSpec _ | AVariant _ | AAssigns _
      | AAllocation _ | APragma _ | AExtended _ -> ()
  in
  Annotations.iter_all_code_annot do_code_annot

(* Special evaluation for the alarms on the very first statement of the
   main function. We put 'Invalid' statuses on them using this function. *)
let mark_invalid_initializers () =
  let kf = fst (Globals.entry_point ()) in
  let first_stmt = Kernel_function.find_first_stmt kf in
  let do_code_annot _e ca  =
    match Alarms.find ca with (* We only check alarms *)
    | None -> ()
    | Some _ ->
      match ca.annot_content with
      | AAssert (_, p) ->
        let ip = Property.ip_of_code_annot_single kf first_stmt ca in
        (* Evaluate in a fully empty state. Only predicates that do not
           depend on the memory will result in 'False' *)
        let bot = Cvalue.Model.bottom in
        let env = Eval_terms.env_annot ~pre:bot ~here:bot () in
        begin match Eval_terms.eval_predicate env p with
        | True | Unknown -> ()
        | False ->
          let status = Property_status.False_and_reachable in
          let distinct = false (* see comment in mark_green_and_red above *) in
          Property_status.emit ~distinct Value_util.emitter ~hyps:[] ip status;
        end
      | _ -> ()
    in
    Annotations.iter_code_annot do_code_annot first_stmt

(*
Local Variables:
compile-command: "make -C ../../../.."
End:
*)
