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

let store_computed_call (callsite: Value_types.call_site) input_state actuals callres =
  if callres.Value_types.c_cacheable = Value_types.Cacheable then
  match ResultFromCallback.get_option () with
    | None -> ()
    | Some (_stack, inout) ->
      try
        let kf, _ki = callsite in
        let input_bases = bases inout.Inout_type.over_inputs
        and output_bases = bases inout.Inout_type.over_outputs_if_termination in
        (* TODO. add only outputs that are not completely overwritten *)
        let input_bases = Base.Hptset.union input_bases output_bases in
        let state_input =  filter_state input_bases input_state in
        let clear = filter_state output_bases in
        let outputs =
          Value_util.map_outputs clear callres.Value_types.c_values
        in
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

let previous_matches st (map_inputs: MapBasesInputsPrevious.t) =
  let aux binputs hstates =
    let st_filtered = filter_state binputs st in
    try
      let old = Cvalue.Model.Hashtbl.find hstates st_filtered in
      let (outputs, clobbered), i = old in
      let aux st_outputs =
        if Cvalue.Model.is_reachable st_outputs then
          Cvalue.Model.fold_base_offsetmap
            Cvalue.Model.add_base st_outputs st(*=acc*)
        else st_outputs
      in
      let outputs = Value_util.map_outputs aux outputs in
      raise (Result_found ((outputs, clobbered), i))
    with Not_found -> ()
  in
  Base.Hptset.Hashtbl.iter aux map_inputs


let reuse_previous_call (kf, _ as _callsite: Value_types.call_site) state actuals =
  try
    let previous_kf = PreviousStates.find kf in
    let previous = ActualsList.Map.find actuals previous_kf in
    previous_matches state previous;
    None
  with
    | Not_found -> None
    | Result_found ((out, clob), i) ->
      (* TODO: check this. Do we record the result too early? *)
        let st_without_formals = match kf.fundec with
          | Definition (fdec, _) ->
              Value_util.map_outputs
                (Value_util.remove_formals_from_state fdec.sformals) out
          | Declaration _ -> out
        in
        let res_call = {
          Value_types.c_values = st_without_formals;
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
compile-command: "make -C ../.."
End:
*)
