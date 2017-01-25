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
open Locations

let dkey = Value_parameters.register_category "d-inout"

type inout = {
  (* over-approximation of the memory locations written by the function *)
  over_outputs: Zone.t;
  (* over-approximation of the memory locations read by the function *)
  over_inputs: Zone.t;
  (* under-approximation of the memory locations written by the function *)
  under_outputs: Zone.t;
  (* over-approximation of the memory locations parts read by the function
     that are parts of its inputs (i.e. that the function has not written
     previously) *)
  operational_inputs: Zone.t;
}


(* Lattice structure for the abstract state above *)
module LatticeInout = struct

  (* Frama-C "datatype" for type [inout]. These are "boring" utility
     functions, that can be skipped. *)
  include Datatype.Make_with_collections(struct
      include Datatype.Serializable_undefined

      type t = inout
      let name = "Value.Inout.t"

      let reprs = [ {
          over_outputs = List.hd Zone.reprs;
          over_inputs = List.hd Zone.reprs;
          under_outputs = List.hd Zone.reprs;
          operational_inputs = List.hd Zone.reprs;
        } ]

      let structural_descr =
        Structural_descr.t_record [|
          Zone.packed_descr;
          Zone.packed_descr;
          Zone.packed_descr;
          Zone.packed_descr;
        |]

      let compare m1 m2 =
        let c = Zone.compare m1.over_outputs m2.over_outputs in
        if c <> 0 then c
        else
          let c = Zone.compare m1.over_inputs m2.over_inputs in
          if c <> 0 then c
          else
            let c = Zone.compare m1.under_outputs m2.under_outputs in
            if c <> 0 then c
            else
              let c = Zone.compare m1.operational_inputs m2.operational_inputs in
              c

      let equal = Datatype.from_compare

      let pretty fmt c =
        Format.fprintf fmt
          "@[<v 2>Over outputs:@ @[<hov>%a@]@]@.\
           @[<v 2>Over inputs:@ @[<hov>%a@]@]@.\
           @[<v 2>Under outputs:@ @[<hov>%a@]@]@.\
           @[<v 2>Operational inputs:@ @[<hov>%a@]@]"
          Zone.pretty c.over_outputs
          Zone.pretty c.over_inputs
          Zone.pretty c.under_outputs
          Zone.pretty c.operational_inputs

      let hash m =
        Hashtbl.hash (Zone.hash m.over_outputs,
                      Zone.hash m.over_inputs,
                      Zone.hash m.under_outputs,
                      Zone.hash m.operational_inputs
                     )

      let copy c = c

    end)

  (* Initial abstract at the beginning of the computation: nothing written
     or read so far. *)
  let empty = {
    over_outputs = Zone.bottom;
    over_inputs = Zone.bottom;
    under_outputs = Zone.bottom;
    operational_inputs = Zone.bottom;
  }

  (* Top state: everything read or written, nothing written in a sure way *)
  let top = {
    over_outputs = Zone.top;
    over_inputs = Zone.top;
    under_outputs = Zone.bottom;
    operational_inputs = Zone.top;
  }

  (* Join: over-approximation are joined, under-approximation are met. *)
  let join c1 c2 = {
    over_outputs = Zone.join c1.over_outputs c2.over_outputs;
    over_inputs = Zone.join c1.over_inputs c2.over_inputs;
    under_outputs = Zone.link c1.under_outputs c2.under_outputs;
    operational_inputs = Zone.join c1.operational_inputs c2.operational_inputs;
  }

  (* The memory locations are finite, so the ascending chain property is
     already verified. We simply use a join. *)
  let widen _ _ c1 c2 = join c1 c2

  (* Inclusion testing: pointwise for over-approximations, counter-pointwise
     for under-approximations *)
  let is_included c1 c2 =
    Zone.is_included c1.over_outputs c2.over_outputs &&
    Zone.is_included c1.over_inputs c2.over_inputs &&
    Zone.is_included c2.under_outputs c1.under_outputs &&
    Zone.is_included c1.operational_inputs c2.operational_inputs
    
  (* Simultaneous computation of join and inclusion, for optimisation purposes.
     Unoptimized in our case *)
  let join_and_is_included smaller larger =
    let join = join smaller larger in
    join, equal join larger
  ;;

end

(* The four functions below compute the memory locations read when
   evaluating respectively a lvalue, the "host" of lvalue, the offset
   of a lvalue, and an expression. The [to_z] functions return the
   "atomic" inputs of an lvalue when it is dereferenced. *)

(* Computations of the inputs of a lvalue : union of the "host" part and
   the offset. *)
let rec inputs_lv to_z (h, o: lval) =
  Zone.join (inputs_host to_z h) (inputs_offset to_z o)
(* Computation of the inputs of a host. Nothing for a variable, and the
   inputs of [e] for a dereference [*e]. *)
and inputs_host to_z = function
  | Var _ -> Zone.bottom
  | Mem e -> inputs_exp to_z e
(* Computation of the inputs of an offset. *)
and inputs_offset to_z = function
  | NoOffset -> Zone.bottom
  | Field (_, o) -> inputs_offset to_z o
  | Index (e, o) -> Zone.join (inputs_exp to_z e) (inputs_offset to_z o)
(* Computation of the inputs of an expression. *)
and inputs_exp to_z (e: exp) = match e.enode with
  | Const _ | SizeOf _ | AlignOf _ | SizeOfStr _ | SizeOfE _ | AlignOfE _ ->
    (* static constructs, nothing is read to evaluate them. *)
    Zone.bottom
  | AddrOf lv | StartOf lv ->
    (* computation of an address: the inputs of the lvalue whose address
       is computed are read to compute said address. *)
    inputs_lv to_z lv
  | Lval lv ->
    (* dereference of an lvalue: first, its address must be computed,
       then its contents themselves are read *)
    Zone.join (inputs_lv to_z lv) (to_z lv)
  | CastE (_,e) | UnOp (_,e,_) | Info (e,_) ->
    (* Unary operators *)
    inputs_exp to_z e
  | BinOp (_,e1,e2,_) ->
    (* Binary operators *)
    Zone.join (inputs_exp to_z e1) (inputs_exp to_z e2)


module Transfer = struct

  (* Approximations of two consecutive statements [s1; s2], respectively
     abstracted as [c1] and [c2]. The result is immediate, expect for
     operational inputs. For those, we subtract from the inputs of [c2]
     the memory locations that have been written in a sure way in [c1],
     the perform the join. *)
  let catenate c1 c2 =
    { over_outputs = Zone.join c1.over_outputs c2.over_outputs;
      over_inputs = Zone.join c1.over_inputs c2.over_inputs;
      under_outputs = Zone.link c1.under_outputs c2.under_outputs;
      operational_inputs =
        Zone.join c1.operational_inputs
          (Zone.diff c2.operational_inputs c1.under_outputs);
    }
  
  (* Abstraction of the ';' (sequence) operator. More precisely, this
     function must return an abstraction of the effects of [s1; s2]. *)
  let sequence = catenate

  (* Effects of a conditional [if (e)]. [to_z] converts the location present
     in into zones. Nothing is written, and the memory locations present in
     [e] are read. *)
  let effects_assume to_z e =
    let inputs = inputs_exp to_z e in
    {
      over_outputs = Zone.bottom;
      over_inputs = inputs;
      under_outputs = Zone.bottom;
      operational_inputs = inputs;
    }

  (* Effects of an assigment [lv = e]. [to_z] converts the location present
     in [lv] and [e] into zones. The locations involved in the computation
     of [lv] and [e], and the location correspond to [lv] is read. *)
  let effects_assign to_z lv e =
    let inputs_e = inputs_exp to_z e in
    let inputs_lv = inputs_lv to_z lv.Eval.lval in
    let inputs = Zone.join inputs_e inputs_lv in
    let outputs =
      Precise_locs.enumerate_valid_bits ~for_writing:true lv.Eval.lloc
    in
    let exact = Precise_locs.cardinal_zero_or_one lv.Eval.lloc in
    let under_outputs = if exact then outputs else Zone.bottom in
    {
      over_outputs = outputs;
      over_inputs = inputs;
      under_outputs = under_outputs;
      operational_inputs = inputs;
    }

  (* Removal of a list of variables from a set. Used to model exiting a
     scope. *)
  let remove_variables vars state =
    let bases =
      List.fold_left
        (fun acc v -> Base.Set.add (Base.of_varinfo v) acc)
        Base.Set.empty vars
    in
    let rm = Zone.filter_base (fun b -> not (Base.Set.mem b bases)) in {
      over_outputs = rm state.over_outputs;
      over_inputs = rm state.over_inputs;
      under_outputs = rm state.under_outputs;
      operational_inputs = rm state.operational_inputs;
    }

end


module Internal : Domain_builder.InputDomain
  with type state = inout
   and type value = Cvalue.V.t
   and type location = Precise_locs.precise_location
= struct
  type state = inout
  type value = Cvalue.V.t
  type location = Precise_locs.precise_location

  include (LatticeInout: sig
             include Datatype.S_with_collections with type t = state
             include Abstract_domain.Lattice with type state := state
           end)

  let key = Structure.Key_Domain.create_key "inout domain"
  let structure : t Abstract_domain.structure = Abstract_domain.Leaf key

  let empty _ = LatticeInout.empty

  let enter_scope _kf _vars state = state
  let leave_scope _kf vars state = Transfer.remove_variables vars state

  type origin = unit

  let approximate_call kf state =
    let state =
      if Ast_info.is_frama_c_builtin name ||
         Eval_typ.kf_assigns_only_result_or_volatile kf
      then state
      else LatticeInout.top
    in
    `Value [state]

  module Transfer (Valuation: Abstract_domain.Valuation
                   with type value = value
                    and type origin = origin
                    and type loc = Precise_locs.precise_location)
    : Abstract_domain.Transfer
      with type state = state
       and type value = Cvalue.V.t
       and type location = Precise_locs.precise_location
       and type valuation = Valuation.t
  = struct
    type value = Cvalue.V.t
    type state = inout
    type location = Precise_locs.precise_location
    type valuation = Valuation.t

    let to_z valuation lv =
      match Valuation.find_loc valuation lv with
      | `Value loc ->
        Precise_locs.enumerate_valid_bits ~for_writing:false loc.Eval.loc
      | `Top -> Zone.top (* should not occur *)

    let assign _ki lv e _v valuation state =
      let to_z = to_z valuation in
      let effects = Transfer.effects_assign to_z lv e in
      `Value (Transfer.sequence state effects)

    let assume _stmt e _pos valuation state =
      let to_z = to_z valuation in
      let effects = Transfer.effects_assume to_z e in
      `Value (Transfer.sequence state effects)

    let dump_current_state state =
      let l = fst (Cil.CurrentLoc.get ()) in
      Value_parameters.result ~dkey "DUMPING INOUT STATE \
                                     of file %s line %d@.%a"
        (Filepath.pretty l.Lexing.pos_fname) l.Lexing.pos_lnum
        LatticeInout.pretty state

    let start_call _stmt _call _valuation _state =
      Eval.(Compute (LatticeInout.empty, false))
    
    let finalize_call _stmt call ~pre ~post =
      let kf = call.Eval.kf in
      let name = Kernel_function.get_name kf in
      if Ast_info.is_cea_dump_function name &&
         Value_parameters.is_debug_key_enabled dkey
      then dump_current_state post;
      `Value (Transfer.catenate pre post)

    let default_call _stmt call state =
      approximate_call call.Eval.kf state

    let update _valuation state = state

    let enter_loop _ state = state
    let incr_loop_counter _ state = state
    let leave_loop _ state = state

  end

  let compute_using_specification _ki (kf, _spec) state =
    approximate_call kf state

  (* Memexec *)
  let filter_by_bases _bases state = state
  let reuse ~current_input:state ~previous_output:_ = state

  (* Initial state. Initializers are singletons, so we store nothing. *)
  let global_state () = None
  let initialize_var_using_type state _ = state
  let initialize_var state _ _ _ = state

  (* Logic *)
  type eval_env = state
  let env_current_state state = `Value state
  let env_annot ~pre:_ ~here () = here
  let env_pre_f ~pre () = pre
  let env_post_f ~pre:_ ~post ~result:_ () = post
  let eval_predicate _ _ = Alarmset.Unknown
  let reduce_by_predicate state _ _ = state

  let storage () = false

  let top_query = `Value (Cvalue.V.top, ()), Alarmset.all

  let extract_expr _oracle _state _expr = top_query
  let extract_lval _oracle _state _lv _typ _locs = top_query

  let backward_location _state _lval _typ loc value =
    `Value (loc, value)

  let reduce_further _state _expr _value = [] (*Nothing intelligent to suggest*)

end

module D = Domain_builder.Complete (Internal)



(*
Local Variables:
compile-command: "make -C ../../.."
End:
*)
