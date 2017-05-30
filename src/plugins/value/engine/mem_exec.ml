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


module type Domain = sig
  include Datatype.S_with_collections

  val filter_by_bases: Base.Hptset.t -> t -> t
  val reuse: current_input:t -> previous_output:t -> t
end


module SaveCounter =
  State_builder.SharedCounter(struct let name = "Mem_exec.save_counter" end)

let new_counter, current_counter =
  let cur = ref (-1) in
  (fun () -> cur := SaveCounter.next (); !cur),
  (fun () -> !cur)

let cleanup_ref = ref (fun () -> ())

(* TODO: it would be great to clear also the tables within the plugins. Export
   self and add dependencies *)
let cleanup_results () = !cleanup_ref ()

exception TooImprecise

(* Extract all the bases from a zone *)
let bases = function
  | Locations.Zone.Top (Base.SetLattice.Top, _) -> raise TooImprecise
  | Locations.Zone.Top (Base.SetLattice.Set s, _) -> s
  | Locations.Zone.Map m -> Base.Hptset.from_shape (Locations.Zone.shape m)


let counter = ref 0

module Make
    (Value : Datatype.S)
    (Domain : Domain)
= struct

  incr counter;

  module CallOutput =
    Datatype.List (Domain)

  module StoredResult =
    Datatype.Pair
      (CallOutput)
      (Datatype.Int) (* Call number, for plugins *)

  (* Map from input states to outputs (summary and state). *)
  module CallEffect = Domain.Hashtbl.Make (StoredResult)

  (* Map from useful input bases to call effects. *)
  module InputBasesToCallEffect = Base.Hptset.Hashtbl.Make (CallEffect)

  (* List of the arguments of a call. *)
  module ActualArgs =
    Datatype.List_with_collections (Datatype.Option (Value)) (* None is bottom *)
      (struct let module_name = "Mem_exec.ActualArgs("
                                ^ string_of_int !counter ^ ")"
      end)

  (* Map from the arguments of a call to stored results. *)
  module ArgsToStoredCalls = ActualArgs.Map.Make (InputBasesToCallEffect)

  module PreviousCalls =
    Kernel_function.Make_Table
      (ArgsToStoredCalls)
      (struct
        let size = 17
        let dependencies = [Db.Value.self]
        let name = "Mem_exec.PreviousCalls(" ^ string_of_int !counter ^ ")"
      end)

  let cleanup = !cleanup_ref
  let () = cleanup_ref := fun () -> cleanup (); PreviousCalls.clear ()

  (** [diff_base_full_zone bases zones] remove from the set of bases [bases]
      those of which all bits are present in [zones] *)
  let diff_base_full_zone =
    let cache = Hptmap_sig.PersistentCache "Mem_exec.diff_base_full_zone" in
    let empty_left _ = Base.Hptset.empty (* nothing left to clear *) in
    let empty_right v = v (* return all bases unchanged *) in
    (* Check whether [range] covers the validity of [b]. If so, remove [b]
       (hence, return an empty set). Otherwise, keep [b]. Variable bases are
       always kept, because they may be changed into weak variables later.
       This is specific to the way this function is used later in this file. *)
    let both b range = begin
      match Base.validity b with
      | Base.Invalid -> assert false
      | Base.Empty -> Base.Hptset.empty
      | Base.Variable _ -> Base.Hptset.singleton b
      | Base.Known (min, max) | Base.Unknown (min, _, max) ->
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

  let store_computed_call kf input_state args
      (call_result: Domain.t list Bottom.or_bottom) =
    match Transfer_stmt.current_kf_inout () with
    | None -> ()
    | Some inout ->
      try
        let output_bases = bases inout.Inout_type.over_outputs_if_termination
        and input_bases = bases inout.Inout_type.over_inputs in
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
        let state_input = Domain.filter_by_bases input_bases input_state in
        (* Outputs bases, that is bases that are copy-pasted, also include
           input bases. Indeed, those may get reduced during the call. *)
        let all_output_bases =
          if remove_sure_outputs
          then Base.Hptset.union input_bases output_bases
          else input_bases
        in
        (* Adds the fake varinfo used for the result of [kf] to the
           output_bases. *)
        let return_varinfo = Library_functions.get_retres_vi kf in
        let return_base = Extlib.opt_map Base.of_varinfo return_varinfo in
        let all_output_bases =
          Extlib.opt_fold Base.Hptset.add return_base all_output_bases
        in
        let clear state = Domain.filter_by_bases all_output_bases state in
        let call_result = match call_result with
          | `Bottom -> []
          | `Value list -> list
        in
        let outputs = List.map clear call_result in
        let call_number = current_counter () in
        let map_a =
          try PreviousCalls.find kf
          with Not_found -> ActualArgs.Map.empty
        in
        let hkf =
          let args =
            List.map (function `Bottom -> None | `Value v -> Some v) args in
          try ActualArgs.Map.find args map_a
          with Not_found ->
            let h = Base.Hptset.Hashtbl.create 11 in
            let map_a = ActualArgs.Map.add args h map_a in
            PreviousCalls.replace kf map_a;
            h
        in
        let hkb =
          try Base.Hptset.Hashtbl.find hkf input_bases
          with Not_found ->
            let h = Domain.Hashtbl.create 11 in
            Base.Hptset.Hashtbl.add hkf input_bases h;
            h
        in
        Domain.Hashtbl.add hkb state_input
          (outputs, call_number);
      with
      | TooImprecise
      | Kernel_function.No_Statement
      | Not_found -> ()


  exception Result_found of CallOutput.t * int

  (** Find a previous execution in [map_inputs] that matches [st].
      raise [Result_found] when this execution exists, or do nothing. *)
  let find_match_in_previous (map_inputs: InputBasesToCallEffect.t) state =
    let aux_previous_call binputs hstates =
      (* restrict [state] to the inputs of this call *)
      let st_filtered = Domain.filter_by_bases binputs state in
      try
        let outputs, i = Domain.Hashtbl.find hstates st_filtered in
        (* We have found a previous execution, in which the outputs are
           [outputs]. Copy them in [state] and return this result. *)
        let process output =
          Domain.reuse ~current_input:state ~previous_output:output in
        let outputs = List.map process outputs in
        raise (Result_found (outputs, i))
      with Not_found -> ()
    in
    Base.Hptset.Hashtbl.iter aux_previous_call map_inputs

  let reuse_previous_call kf state args =
    try
      let previous_kf = PreviousCalls.find kf in
      let args = List.map (function `Bottom -> None | `Value v -> Some v) args in
      let previous = ActualArgs.Map.find args previous_kf in
      find_match_in_previous previous state;
      None
    with
    | Not_found -> None
    | Result_found (outputs, i) ->
      let call_result = outputs in
      Some (Bottom.bot_of_list call_result, i)

end


(*
Local Variables:
compile-command: "make -C ../../../.."
End:
*)
