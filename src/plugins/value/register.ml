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
open Locations

let dkey_card = Value_parameters.register_category "cardinal"

let compute () =
  (* Nothing to recompute when Value has already been computed. This boolean
     is automatically cleared when an option of Value changes, because they
     are registered as dependencies on [Db.Value.self] in {!Value_parameters}.*)
  if not (Db.Value.is_computed ()) then Analysis.force_compute ()

let _self =
  Db.register_compute "Value.compute" [ Db.Value.self ] Db.Value.compute compute


let display ?fmt kf =
  (* Do not pretty Cil-generated variables or out-of scope local variables *)
  let filter_generated_and_locals base =
    match base with
      | Base.Var (v, _) ->
        if v.vtemp then v.vname = "__retres"
        else
          ((not (Kernel_function.is_local v kf))
          (* only locals of outermost block *)
           || List.exists (fun x -> x.vid = v.vid)
             (Kernel_function.get_definition kf).sbody.blocals )
      | _ -> true
  in
  try
    let values = Db.Value.get_stmt_state (Kernel_function.find_return kf) in
    let fst_values =
      Db.Value.get_stmt_state (Kernel_function.find_first_stmt kf)
    in
    if Cvalue.Model.is_reachable fst_values
      && not (Cvalue.Model.is_top fst_values)
    then begin
      let print_cardinal = Value_parameters.is_debug_key_enabled dkey_card in
      let estimate =
        if print_cardinal
        then Cvalue.Model.cardinal_estimate values
        else Cvalue.CardinalEstimate.one
      in
      let outs = !Db.Outputs.get_internal kf in
      let outs = Zone.filter_base filter_generated_and_locals outs in
      let header fmt =
        Format.fprintf fmt "Values at end of function %a:%t"
          Kernel_function.pretty kf
          (fun fmt ->
            if print_cardinal then
              Format.fprintf fmt " (~%a states)"
                Cvalue.CardinalEstimate.pretty estimate)
      in
      let body fmt =
        Format.fprintf fmt "@[%t@]@[  %t@]"
          (fun fmt ->
            match outs with
            | Zone.Top (Base.SetLattice.Top, _) ->
              Format.fprintf fmt "@[Cannot filter: dumping raw memory \
                  (including unchanged variables)@]@\n"
            | _ -> ())
          (fun fmt -> Cvalue.Model.pretty_filter fmt values outs) in
      match fmt with
      | None -> Value_parameters.printf
                  ~dkey:Value_parameters.dkey_final_states ~header "%t" body
      | Some fmt -> Format.fprintf fmt "%t@.%t@," header body
    end
  with Kernel_function.No_Statement -> ()

let display_results () =
  if Db.Value.is_computed () && Value_parameters.verbose_atleast 1
 then begin
    Value_parameters.result "====== VALUES COMPUTED ======";
    Callgraph.Uses.iter_in_rev_order display;
    Value_parameters.result "%t" Value_perf.display
  end

let () = Value_parameters.ForceValues.set_output_dependencies [Db.Value.self]

let main () =
  (* Value computations *)
  if Value_parameters.ForceValues.get () then begin
    !Db.Value.compute ();
    Value_parameters.ForceValues.output display_results;
  end

let () = Db.Main.extend main


(* Functions to register in Db.Value *)

let eval_error_reason fmt e =
  if e <> Eval_terms.CAlarm
  then Eval_terms.pretty_logic_evaluation_error fmt e

let assigns_inputs_to_zone state assigns =
  let env = Eval_terms.env_pre_f ~pre:state () in
  let treat_asgn acc (_,ins as asgn) =
    match ins with
      | FromAny -> Zone.top
      | From l ->
        try
          List.fold_left
            (fun acc t ->
               let z =
                 Eval_terms.eval_tlval_as_zone ~alarm_mode:Eval_terms.Ignore
                   ~for_writing:false env t.it_content
               in
               Zone.join acc z)
            acc
            l
        with Eval_terms.LogicEvalError e ->
          Value_parameters.warning ~current:true ~once:true
            "Failed to interpret inputs in assigns clause '%a'%a"
            Printer.pp_from asgn eval_error_reason e;
          Zone.top
  in
  match assigns with
    | WritesAny -> Zone.top
    | Writes l  -> List.fold_left treat_asgn Zone.bottom l

let assigns_outputs_aux ~eval ~bot ~top ~join state ~result assigns =
  let env = Eval_terms.env_post_f state state result () in
  let treat_asgn acc ({it_content = out},_) =
    if Logic_utils.is_result out && result = None
    then acc
    else
      try
        let z = eval env out in
        join z acc
      with Eval_terms.LogicEvalError e ->
        Value_parameters.warning ~current:true ~once:true
          "Failed to interpret assigns clause '%a'%a"
          Printer.pp_term out eval_error_reason e;
        join top acc
  in
  match assigns with
    | WritesAny -> join top bot
    | Writes l  -> List.fold_left treat_asgn bot l

let assigns_outputs_to_zone =
  let eval env term =
    Eval_terms.eval_tlval_as_zone
      ~alarm_mode:Eval_terms.Ignore ~for_writing:true env term
  in
  assigns_outputs_aux ~eval
    ~bot:Locations.Zone.bottom ~top:Locations.Zone.top ~join:Locations.Zone.join

let assigns_outputs_to_locations =
  let eval env term =
    Eval_terms.eval_tlval_as_location
      ~alarm_mode:Eval_terms.Ignore env term
  in
  assigns_outputs_aux
    ~eval
    ~bot:[] ~top:(Locations.make_loc Locations.Location_Bits.top Int_Base.top)
    ~join:(fun v l -> v :: l)


(* "access" functions before evaluation, registered in Db.Value *)
let access_value_of_lval kinstr lv =
  let state = Db.Value.get_state kinstr in
  snd (!Db.Value.eval_lval None state lv)

let access_value_of_expr kinstr e =
  let state = Db.Value.get_state kinstr in
  !Db.Value.eval_expr state e

let access_value_of_location kinstr loc =
  let state = Db.Value.get_state kinstr in
  Db.Value.find state loc

let find_deps_term_no_transitivity_state state t =
  try
    let env = Eval_terms.env_only_here state in
    let r = Eval_terms.eval_term ~alarm_mode:Eval_terms.Ignore env t in
    r.Eval_terms.ldeps
  with Eval_terms.LogicEvalError _ -> raise Db.From.Not_lval

(* If the function is a builtin, or if the user has requested it, use
   \assigns and \from clauses, that give an approximation of the result *)
let use_spec_instead_of_definition kf =
  not (Kernel_function.is_definition kf) ||
    Ast_info.is_frama_c_builtin (Kernel_function.get_name kf) ||
    Builtins.find_builtin_override kf <> None ||
    Kernel_function.Set.mem kf (Value_parameters.UsePrototype.get ()) ||
    Value_parameters.LoadFunctionState.mem kf

let eval_predicate ~pre ~here p =
  let open Eval_terms in
  let env = env_annot ~pre ~here () in
  match eval_predicate env p with
  | True -> Property_status.True
  | False -> Property_status.False_if_reachable
  | Unknown -> Property_status.Dont_know

let () =
(* Pretty-printing *)
  Db.Value.display := (fun fmt kf -> display ~fmt kf);
  Db.Value.use_spec_instead_of_definition := use_spec_instead_of_definition;
  Db.Value.assigns_outputs_to_zone := assigns_outputs_to_zone;
  Db.Value.assigns_outputs_to_locations := assigns_outputs_to_locations;
  Db.Value.assigns_inputs_to_zone := assigns_inputs_to_zone;
  Db.Value.access := access_value_of_lval;
  Db.Value.access_location := access_value_of_location;
  Db.Value.access_expr := access_value_of_expr;
  Db.Value.Logic.eval_predicate := eval_predicate;
  Db.From.find_deps_term_no_transitivity_state :=
    find_deps_term_no_transitivity_state;


(* -------------------------------------------------------------------------- *)
(*                    Register Evaluation Functions                           *)
(* -------------------------------------------------------------------------- *)

open Eval

module Val = struct
  include Main_values.CVal
  include Structure.Open (Structure.Key_Value) (Main_values.CVal)
  let reduce t = t
end

module Eva =
  Evaluation.Make
    (Val)
    (Main_locations.PLoc)
    (Cvalue_domain.State)

module Transfer = Cvalue_domain.State.Transfer (Eva.Valuation)

let bot_value = function
  | `Bottom -> Cvalue.V.bottom
  | `Value v -> v

let bot_state = function
  | `Bottom -> Cvalue.Model.bottom
  | `Value s -> s

let rec eval_deps state e =
  match e.enode with
  | SizeOf _ | SizeOfE _ | SizeOfStr _ | AlignOf _ | AlignOfE _ | Const _ ->
    Locations.Zone.bottom
  | Lval lv -> eval_deps_lval state lv
  | BinOp (_,e1,e2,_) ->
    Locations.Zone.join (eval_deps state e1) (eval_deps state e2)
  | CastE (_,e) | UnOp (_,e,_) | Info (e,_) ->
    eval_deps state e
  | AddrOf lv | StartOf lv -> eval_deps_addr state lv
and eval_deps_lval state lv =
  let for_writing = false in
  let deps = eval_deps_addr state lv in
  let loc =
    fst (Eva.lvaluate ~for_writing state lv)
    >>-: fun (_valuation, loc, _typ) -> loc
  in
  match loc with
  | `Bottom -> deps
  | `Value loc ->
    let deps_lv = Precise_locs.enumerate_valid_bits ~for_writing loc in
    Locations.Zone.join deps deps_lv
and eval_deps_addr state (h, o:lval) =
  Locations.Zone.join (eval_deps_host state h) (eval_deps_offset state o)
and eval_deps_host state h = match h with
  | Var _ -> Locations.Zone.bottom
  | Mem e -> eval_deps state e
and eval_deps_offset state o = match o with
  | NoOffset -> Locations.Zone.bottom
  | Field (_, o) -> eval_deps_offset state o
  | Index (i, o) ->
    Locations.Zone.join (eval_deps state i) (eval_deps_offset state o)

let notify_opt with_alarms alarms =
  Extlib.may (fun mode -> Alarmset.notify mode alarms) with_alarms

let eval_expr_with_valuation ?with_alarms deps state expr=
  let state = Cvalue_domain.inject state in
  let deps = match deps with
    | None -> None
    | Some deps ->
      let deps' = eval_deps state expr in
      Some (Locations.Zone.join deps' deps)
  in
  let eval, alarms = Eva.evaluate state expr in
  notify_opt with_alarms alarms;
  match eval with
  | `Bottom -> (Cvalue.Model.bottom, deps, Cvalue.V.bottom), None
  | `Value (valuation, result) ->
    let state = Cvalue_domain.project (Transfer.update valuation state) in
    (state, deps, result), Some valuation

(* Compatibility layer between the old API of eval_exprs and the new evaluation
   scheme. *)
module Eval = struct

  let eval_expr ?with_alarms state expr =
    let state = Cvalue_domain.inject state in
    let eval, alarms = Eva.evaluate ~reduction:false state expr in
    notify_opt with_alarms alarms;
    bot_value (eval >>-: snd)

  let eval_lval ?with_alarms deps state lval =
    let expr = Value_util.lval_to_exp lval in
    let res, valuation = eval_expr_with_valuation ?with_alarms deps state expr in
    let typ = match valuation with
      | None -> Cil.typeOfLval lval
      | Some valuation -> match Eva.Valuation.find_loc valuation lval with
        | `Value record -> record.typ
        | `Top -> Cil.typeOfLval lval
    in
    let state, deps, v = res in
    state, deps, v, typ

  let eval_expr_with_deps_state ?with_alarms deps state expr =
    fst (eval_expr_with_valuation ?with_alarms deps state expr)


  let reduce_by_cond state expr positive =
    let state = Cvalue_domain.inject state in
    let eval, _alarms =
      Eva.reduce state expr positive
    in
    bot_state (eval >>-: fun valuation ->
               Cvalue_domain.project (Transfer.update valuation state))


  let lval_to_precise_loc_deps_state ?with_alarms ~deps state ~reduce_valid_index:(_:bool) lval =
    if not (Cvalue.Model.is_reachable state)
    then state, deps, Precise_locs.loc_bottom, (Cil.typeOfLval lval)
    else
    let state = Cvalue_domain.inject state in
    let deps = match deps with
      | None -> None
      | Some deps ->
          let deps' = eval_deps_addr state lval in
          Some (Locations.Zone.join deps' deps)
    in
    let eval, alarms =
      Eva.lvaluate ~for_writing:false state lval
    in
    notify_opt with_alarms alarms;
    match eval with
      | `Bottom -> Cvalue.Model.bottom, deps, Precise_locs.loc_bottom, (Cil.typeOfLval lval)
      | `Value (valuation, loc, typ) ->
          Cvalue_domain.project (Transfer.update valuation state), deps, loc, typ

  let lval_to_loc_deps_state ?with_alarms ~deps state ~reduce_valid_index lv =
    let state, deps, pl, typ =
      lval_to_precise_loc_deps_state
        ?with_alarms ~deps state ~reduce_valid_index lv
    in
    state, deps, Precise_locs.imprecise_location pl, typ

  let lval_to_precise_loc_state ?with_alarms state lv =
    let state, _, r, typ =
      lval_to_precise_loc_deps_state
        ?with_alarms ~deps:None ~reduce_valid_index:(Kernel.SafeArrays.get ())
        state lv
    in
    state, r, typ

  and lval_to_loc_state ?with_alarms state lv =
    let state, _, r, typ =
      lval_to_loc_deps_state
        ?with_alarms ~deps:None ~reduce_valid_index:(Kernel.SafeArrays.get ())
        state lv
    in
    state, r, typ

  let lval_to_precise_loc ?with_alarms state lv =
    let _, r, _typ = lval_to_precise_loc_state ?with_alarms state lv in
    r

  let lval_to_loc ?with_alarms state lv =
    let _, r, _typ = lval_to_loc_state ?with_alarms state lv in
    r


  let resolv_func_vinfo ?with_alarms deps state funcexp =
    let open Cil_types in
    let state = Cvalue_domain.inject state in
    let deps = match funcexp.enode with
      | Lval (Var _, NoOffset) -> deps
      | Lval (Mem v, _) ->
          begin match deps with
            | None -> None
            | Some deps ->
                let deps' = eval_deps state v in
                Some (Locations.Zone.join deps' deps)
          end
      | _ -> assert false
    in
    let kfs, alarms = Eva.eval_function_exp funcexp state in
    notify_opt with_alarms alarms;
    let kfs = match kfs with
      | `Bottom -> Kernel_function.Hptset.empty
      | `Value kfs ->
          List.fold_left
            (fun acc (kf, _) -> Kernel_function.Hptset.add kf acc)
            Kernel_function.Hptset.empty kfs
    in
    kfs, deps

end

module type Eval = module type of Eval

(* Functions to register in Db.Value that depend on evaluation functions. *)
module Export (Eval : Eval) = struct

  open Eval

  let lval_to_loc_with_deps_state ?with_alarms state ~deps lv =
    let _state, deps, r, _ =
      lval_to_loc_deps_state
        ?with_alarms
        ~deps:(Some deps)
        ~reduce_valid_index:(Kernel.SafeArrays.get ())
        state
        lv
    in
    Extlib.opt_conv Locations.Zone.bottom deps, r

  let lval_to_loc_with_deps kinstr ?with_alarms ~deps lv =
    let state = Db.Value.noassert_get_state kinstr in
    lval_to_loc_with_deps_state ?with_alarms  state ~deps lv

  let lval_to_loc_kinstr kinstr ?with_alarms lv =
    let state = Db.Value.noassert_get_state kinstr in
    lval_to_loc ?with_alarms state lv

  let lval_to_precise_loc_with_deps_state_alarm ?with_alarms state ~deps lv =
    let _state, deps, ploc, _ =
      lval_to_precise_loc_deps_state ?with_alarms
        ~deps ~reduce_valid_index:(Kernel.SafeArrays.get ()) state lv
    in
    let deps = Extlib.opt_conv Locations.Zone.bottom deps in
    deps, ploc

  let lval_to_precise_loc_with_deps_state =
    lval_to_precise_loc_with_deps_state_alarm ?with_alarms:None

  let lval_to_zone kinstr ?with_alarms lv =
    let state_to_joined_zone state acc =
      let _, r =
        lval_to_precise_loc_with_deps_state_alarm ?with_alarms state ~deps:None lv
      in
      let zone = Precise_locs.enumerate_valid_bits ~for_writing:false r in
      Locations.Zone.join acc zone
    in
    Db.Value.fold_state_callstack
      state_to_joined_zone Locations.Zone.bottom ~after:false kinstr

  let lval_to_zone_state state lv =
    let _, r = lval_to_precise_loc_with_deps_state state ~deps:None lv in
    Precise_locs.enumerate_valid_bits ~for_writing:false r

  let lval_to_zone_with_deps_state state ~for_writing ~deps lv =
    let deps, r = lval_to_precise_loc_with_deps_state state ~deps lv in
    let r = (* No write effect if [lv] is const *)
      if for_writing && (Value_util.is_const_write_invalid (Cil.typeOfLval lv))
      then Precise_locs.loc_bottom
      else r
    in
    let zone = Precise_locs.enumerate_valid_bits ~for_writing r in
    let exact = Precise_locs.valid_cardinal_zero_or_one ~for_writing r in
    deps, zone, exact


  let lval_to_offsetmap_aux ?with_alarms state lv =
    let loc =
      Locations.valid_part ~for_writing:false
        (lval_to_loc ?with_alarms state lv)
    in
    match loc.Locations.size with
      | Int_Base.Top -> None
      | Int_Base.Value size ->
        match Cvalue.Model.copy_offsetmap loc.Locations.loc size state with
        | `Bottom -> None
        | `Value m -> Some m

  let lval_to_offsetmap kinstr ?with_alarms lv =
    let state = Db.Value.noassert_get_state kinstr in
    lval_to_offsetmap_aux ?with_alarms state lv

  let lval_to_offsetmap_state state lv =
    lval_to_offsetmap_aux state lv


  let expr_to_kernel_function_state ?with_alarms state ~deps exp =
    let r, deps = resolv_func_vinfo ?with_alarms deps state exp in
    Extlib.opt_conv Locations.Zone.bottom deps, r

  let expr_to_kernel_function kinstr ?with_alarms ~deps exp =
    let state_to_joined_kernel_function state (z_acc, kf_acc) =
      let z, kf =
        expr_to_kernel_function_state ?with_alarms state ~deps exp
      in
      Locations.Zone.join z z_acc,
      Kernel_function.Hptset.union kf kf_acc
    in
    Db.Value.fold_state_callstack
      state_to_joined_kernel_function
      ((match deps with None -> Locations.Zone.bottom | Some z -> z),
       Kernel_function.Hptset.empty)
      ~after:false kinstr

  let expr_to_kernel_function_state =
    expr_to_kernel_function_state ?with_alarms:None
end


module type Export = module type of (Export (Eval))

let register (module Eval: Eval) (module Export: Export) =
  let open Export in
  Db.Value.eval_expr := Eval.eval_expr;
  Db.Value.eval_expr_with_state :=
    (fun ?with_alarms state expr ->
       let (s, _, v) =
         Eval.eval_expr_with_deps_state ?with_alarms None state expr
       in
       s, v);
  Db.Value.reduce_by_cond := Eval.reduce_by_cond;
  Db.Value.eval_lval :=
    (fun ?with_alarms deps state lval ->
      let _, deps, r, _ = Eval.eval_lval ?with_alarms deps state lval in
      deps, r);
  Db.Value.lval_to_loc_with_deps := lval_to_loc_with_deps;
  Db.Value.lval_to_loc_with_deps_state :=
    lval_to_loc_with_deps_state ?with_alarms:None;
  Db.Value.lval_to_loc := lval_to_loc_kinstr;
  Db.Value.lval_to_loc_state := Eval.lval_to_loc ?with_alarms:None;
  Db.Value.lval_to_zone_state := lval_to_zone_state;
  Db.Value.lval_to_zone := lval_to_zone;
  Db.Value.lval_to_zone_with_deps_state := lval_to_zone_with_deps_state;
  Db.Value.lval_to_precise_loc_state := Eval.lval_to_precise_loc_state;
  Db.Value.lval_to_precise_loc_with_deps_state :=
    lval_to_precise_loc_with_deps_state;
  Db.Value.lval_to_offsetmap := lval_to_offsetmap;
  Db.Value.lval_to_offsetmap_state := lval_to_offsetmap_state;
  Db.Value.expr_to_kernel_function := expr_to_kernel_function;
  Db.Value.expr_to_kernel_function_state := expr_to_kernel_function_state;
  ()


let () = Db.Value.initial_state_only_globals := Analysis.cvalue_initial_state

let () =
  let eval = (module Eval : Eval) in
  let export = (module Export ((val eval : Eval)) : Export) in
  register eval export;;


(*
Local Variables:
compile-command: "make -C ../../.."
End:
*)
