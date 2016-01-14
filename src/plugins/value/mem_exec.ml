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

exception TooImprecise

(* Extract all the bases from a zone *)
let bases = function
  | Locations.Zone.Top (Base.SetLattice.Top, _) -> raise TooImprecise
  | Locations.Zone.Top (Base.SetLattice.Set s, _) -> s
  | Locations.Zone.Map m -> Base.Hptset.from_shape (Locations.Zone.shape m)


(* Auxiliary function that keeps only some bases inside a memory state *)
let filter_state bases state =
  Cvalue.Model.filter_by_shape (Base.Hptset.shape bases) state


module ValueOutputs = Datatype.Pair
  (Datatype.List(
    Datatype.Pair
      (Datatype.Option(Cvalue.V_Offsetmap)) (* Return *)
      (Cvalue.Model) (* Memory state *)))
  (Base.SetLattice) (* cloberred set for local variables *)

(* let pretty fmt (((bin, stin), (_, stout, _), _i) : PreviousState.t) =
  Format.fprintf fmt
    "@[<v>@[Inputs contained in %a]@ \
       @[Values of inputs:@]@ %a\
       @[Values of outputs:@]@ %a@]"
    Base.Hptset.pretty bin Cvalue.Model.pretty stin Cvalue.Model.pretty stout
*)

module PreviousState =
  Datatype.Pair
    (ValueOutputs (* Outputs *))
    (Datatype.Int(* Call number, for plugins *))
 
module Actuals = struct
  include Datatype.Pair(Cil_datatype.Exp)(Cvalue.V_Offsetmap)
  let compare (_, o1 : t) (_, o2 : t) = Cvalue.V_Offsetmap.compare o1 o2
end

module ActualsList =
  Datatype.List_with_collections(Actuals)
    (struct let module_name = "Mem_exec.ActualsList" end)

(* Map input states filtered on relevant bases to the relevant data *)
module MapInputsPrevious =
  Cvalue.Model.Hashtbl.Make(PreviousState)

(* Map from useful inputs to stored previous results *)
module MapBasesInputsPrevious =
  Base.Hptset.Hashtbl.Make(MapInputsPrevious)

(* Map from actuals to useful inputs to stored previous results *)
module MapActualsBasesInputsPrevious =
  ActualsList.Map.Make(MapBasesInputsPrevious)

module PreviousStates =
  State_builder.Hashtbl(Kernel_function.Hashtbl)(MapActualsBasesInputsPrevious)
    (struct
      let size = 17
      let dependencies = [Db.Value.self]
      let name = "Mem_exec.PreviousStates"
     end)

(* Reference filled in by the callwise-inout callback *)
module ResultFromCallback =
  State_builder.Option_ref(Datatype.Pair(Value_types.Callstack)(Inout_type))
    (struct
      let dependencies = [Db.Value.self]
      let name = "Mem_exec.ResultFromCallback"
     end)


(* TODO: it would be great to clear also the tables within the plugins. Export
   self and add dependencies *)
let cleanup_results () = 
  PreviousStates.clear ();
  ResultFromCallback.clear ();
;;


let map_to_outputs f =
  List.map
    (fun ((res: Cvalue.V_Offsetmap.t option), (out: Cvalue.Model.t)) ->
      (res, f out))

let register_callback () =
  if Value_parameters.MemExecAll.get () then
    Db.Operational_inputs.Record_Inout_Callbacks.extend_once
      (fun (_stack, _inout as v) ->
         ResultFromCallback.set v)

let () = Cmdline.run_after_configuring_stage register_callback


module SaveCounter =
  State_builder.SharedCounter(struct let name = "Mem_exec.save_counter" end)

let new_counter, current_counter =
  let cur = ref (-1) in
  (fun () -> cur := SaveCounter.next (); !cur),
  (fun () -> !cur)

(** [diff_base_full_zone bases zones] remove from the set of bases [bases]
    those of which all bits are present in [zones] *)
let diff_base_full_zone =
  let cache = Hptmap_sig.PersistentCache "Mem_exec.diff_base_full_zone" in
  let empty_left _ = Base.Hptset.empty (* nothing left to clear *) in
  let empty_right v = v (* return all bases unchanged *) in
  (* Check whether [range] covers the validity of [b]. If so, remove [b]
     (hence, return an empty set). Otherwise, keep [b]. *)
  let both b range = begin
    match Base.valid_range (Base.validity b) with
    | None -> assert false
    | Some (min, max) ->
      match Int_Intervals.project_singleton range with
      | Some (min', max') ->
        if Integer.equal min min' && Integer.equal max max' then
          Base.Hptset.empty
        else
          Base.Hptset.singleton b
      | None -> Base.Hptset.singleton b
  end in
  let join = Base.Hptset.union in
  let empty = Base.Hptset.empty in
  let f = Base.Hptset.fold2_join_heterogeneous
    ~cache ~empty_left ~empty_right ~both ~join ~empty
  in
  fun bases z ->
    match z with
    | Locations.Zone.Map m -> f bases (Locations.Zone.shape m)
    | Locations.Zone.Top _ -> bases (* Never happens anyway *)

let store_computed_call (callsite: Value_types.call_site) input_state actuals callres =
  if callres.Value_types.c_cacheable = Value_types.Cacheable then
    match ResultFromCallback.get_option () with
    | None -> ()
    | Some (_stack, inout) ->
      try
        let kf, _ki = callsite in
        let input_bases = bases inout.Inout_type.over_inputs
        and output_bases = bases inout.Inout_type.over_outputs_if_termination in
        (* There are two strategies to compute the 'inputs' for a memexec
           function: either we take all inputs_bases+outputs_bases
           (outputs_bases are important because of weak updates), or we
           remove the sure outputs from the outputs, as sure outputs by
           definition strong updated. The latter will enable memexec to fire
           more often, but requires more computations. *)
        let remove_sure_outputs = true in
        let input_bases =
          if remove_sure_outputs then
            let uncertain_output_bases =
              (* Remove outputs whose base is completely overwritten *)
              diff_base_full_zone
                output_bases inout.Inout_type.under_outputs_if_termination
            in
            Base.Hptset.union input_bases uncertain_output_bases
          else
            Base.Hptset.union input_bases output_bases
        in
        let state_input = filter_state input_bases input_state in
        (* Outputs bases, that is bases that are copy-pasted, also include
           input bases. Indeed, those may get reduced during the call. *)
        let all_output_bases =
          if remove_sure_outputs
          then Base.Hptset.union input_bases output_bases
          else input_bases
        in
        let clear state = filter_state all_output_bases state in
        let outputs = map_to_outputs clear callres.Value_types.c_values in
        let call_number = current_counter () in
        let map_a =
          try PreviousStates.find kf
          with Not_found -> ActualsList.Map.empty
        in
        let hkf =
          try ActualsList.Map.find actuals map_a
          with Not_found ->
            let h = Base.Hptset.Hashtbl.create 11 in
            let map_a = ActualsList.Map.add actuals h map_a in
            PreviousStates.replace kf map_a;
            h
        in
        let hkb =
          try Base.Hptset.Hashtbl.find hkf input_bases
          with Not_found ->
            let h = Cvalue.Model.Hashtbl.create 11 in
            Base.Hptset.Hashtbl.add hkf input_bases h;
            h
        in
        Cvalue.Model.Hashtbl.add hkb state_input
          ((outputs, callres.Value_types.c_clobbered), call_number);
        ResultFromCallback.clear ()
      with
        | TooImprecise
        | Kernel_function.No_Statement
        | Not_found -> ResultFromCallback.clear ()


exception Result_found of ValueOutputs.t * int

(** Find a previous execution in [map_inputs] that matches [st].
    raise [Result_found] when this execution exists, or do nothing. *)
let find_match_in_previous (map_inputs: MapBasesInputsPrevious.t) state =
  let aux_previous_call binputs hstates =
    (* restrict [state] to the inputs of this call *)
    let st_filtered = filter_state binputs state in
    try
      let (outputs, clobbered), i =
        Cvalue.Model.Hashtbl.find hstates st_filtered
      in
      (* We have found a previous execution, in which the outputs are
         [outputs]. Copy them in [state] and return this result. *)
      let aux = function
        | Cvalue.Model.Bottom | Cvalue.Model.Top as state -> state
        | Cvalue.Model.Map outputs ->
          Cvalue.Model.fold Cvalue.Model.add_base outputs state(*=acc*)
      in
      let outputs = map_to_outputs aux outputs in
      raise (Result_found ((outputs, clobbered), i))
    with Not_found -> ()
  in
  Base.Hptset.Hashtbl.iter aux_previous_call map_inputs


let reuse_previous_call (kf, _ as _callsite: Value_types.call_site) state actuals =
  try
    let previous_kf = PreviousStates.find kf in
    let previous = ActualsList.Map.find actuals previous_kf in
    find_match_in_previous previous state;
    None
  with
    | Not_found -> None
    | Result_found ((out, clob), i) ->
        let res_call = {
          Value_types.c_values = out;
          c_clobbered = clob;
          c_cacheable = Value_types.Cacheable
            (* call can be cached since it was cached once *);
        } in
        Some (res_call, i)


(* TEST code, to be pasted in eval_funs, below the call to reuse_previous_state
  let res = match compute_call_to_builtin kf initial_state actuals with
    | Some r -> r
    | None ->
      let res = compute_call_to_cil_function kf with_formals call_kinstr in
      res
  in
  match Mem_exec.reuse_previous_state with_formals (kf, call_kinstr) with
    | None ->
      Mem_exec.store_previous_state (kf, call_kinstr) with_formals res;
      res
    | Some res' ->
      let _ret, st, _ = res and (_ret', st', _ as res'), _, out, in_ = res' in
      if not (Cvalue.Model.equal st st') then begin
        begin
          (match st with
            | Cvalue.Model.Top | Cvalue.Model.Bottom -> assert false
            | Cvalue.Model.Map lb ->
              Cvalue.Model.LBase.iter
                (fun b offsm ->
                  let offsm' = Cvalue.Model.find_base b st' in
                  if not (V_Offsetmap.equal offsm offsm') then (
                    Format.printf "Different offsm for %a@\n%a@\n%a@."
                      Base.pretty b
                      V_Offsetmap.pretty offsm
                      V_Offsetmap.pretty offsm'
                  )
                ) lb);
          (match st' with
            | Cvalue.Model.Top | Cvalue.Model.Bottom -> assert false
            | Cvalue.Model.Map lb' ->
              Cvalue.Model.LBase.iter
                (fun b offsm' ->
                  let offsm = Cvalue.Model.find_base b st in
                  if not (V_Offsetmap.equal offsm offsm') then
                    Format.printf "Different offsm2 for %a@." Base.pretty b
                ) lb');
        end;
        let fmti = Format.formatter_of_out_channel (open_out "stinit")
        and fmt1 = Format.formatter_of_out_channel (open_out "st1")
        and fmt2 = Format.formatter_of_out_channel (open_out "st2") in
        Format.fprintf fmti "###INITIAL STATE@.%a@." Cvalue.Model.pretty with_formals;
        Format.fprintf fmt1 "###RESULT NORMAL@.%a@." Cvalue.Model.pretty st;
        Format.fprintf fmt2 "###RESULT CACHED@.%a@." Cvalue.Model.pretty st';
        Value_parameters.result "Caching failed for function %a,@.out %a@.in %a"
          Kernel_function.pretty kf Cvalue.Model.pretty out Cvalue.Model.pretty in_;
        do_degenerate None
      end;
      res'
*)


(*
Local Variables:
compile-command: "make -C ../../.."
End:
*)
