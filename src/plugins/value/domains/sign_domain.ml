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
open Eval

let dkey = Value_parameters.register_category "d-sign"

(* Memory abstraction for the 'signs' domain. Currently, we only track
   the sign of integer variables. *)
module MapLatticeSign = struct
  include BaseMapLattice.Make(struct
      include Sign_values

      (* In this domain, we only track non-volatile integer variables. *)
      let track_variable vi =
        Cil.isIntegralType vi.vtype
        && not (Cil.typeHasQualifier "volatile" vi.vtype)
    end)

  (* The base lattice is finite, we can use join to perform widening *)
  let widen _kf _stmt = join
end


module Internal : Domain_builder.InputDomain
  with type state = MapLatticeSign.t
   and type value = Sign_values.t
   and type location = Precise_locs.precise_location
= struct
  type state = MapLatticeSign.t
  type value = Sign_values.t
  type location = Precise_locs.precise_location

  include (MapLatticeSign: sig
             include Datatype.S_with_collections with type t = state
             include Abstract_domain.Lattice with type state := state
           end)

  let key = Structure.Key_Domain.create_key "sign domain"
  let structure : t Abstract_domain.structure = Abstract_domain.Leaf key

  let empty _ = MapLatticeSign.top

  let enter_scope _kf _vars state = state
  let leave_scope _kf vars state = MapLatticeSign.remove_variables vars state

  type origin = unit

  let approximate_call kf state =
    let state =
      if Ast_info.is_frama_c_builtin name ||
         Eval_typ.kf_assigns_only_result_or_volatile kf
      then state
      else MapLatticeSign.top
    in
    `Value [state]

  module Transfer (Valuation: Abstract_domain.Valuation
                   with type value = value
                    and type origin = origin
                    and type loc = Precise_locs.precise_location)
    : Abstract_domain.Transfer
      with type state = state
       and type value = Sign_values.t
       and type location = Precise_locs.precise_location
       and type valuation = Valuation.t
  = struct
    type value = Sign_values.t
    type state = MapLatticeSign.t
    type location = Precise_locs.precise_location
    type valuation = Valuation.t

    (* This function binds [loc] to [v], of type [typ], in [state].
       [v] can be [`Bottom], which means that its contents are guaranteed
       to be indeterminate (e.g. uninitialized data). *)
    let bind_loc loc typ v state =
      match v with
      | `Value v ->
        (* We are adding a "good" value. Store it in the state. *)
        MapLatticeSign.add loc typ v state
      | `Bottom ->
        (* Indeterminate value. Drop the information known for loc. *)
        MapLatticeSign.remove loc state

    (* This function updates [state] with information for [e]. For the signs
       domain, this is only possible when [exp] is an lvalue. In this case,
       we can update the corresponding location with the result of the
       evaluation of [exp]. Both the value and the location are found in
       [valuation]. *)
    let assume_exp valuation exp r state =
      match exp.enode with
      | Lval lv -> begin
          match Valuation.find_loc valuation lv with
          | `Top -> state
          | `Value {loc} ->
            bind_loc loc (Cil.typeOfLval lv) r.value.v state
        end
      | _ -> state

    (* This function fills [state] according to the information available
       in [valuation]. This information is computed by EVA's engine for
       all the expressions involved in the current statement. *)
    let update valuation state =
      Valuation.fold (assume_exp valuation) valuation state

    (* Abstraction of an assignment. *)
    let assign _ki lv _e v valuation state =
      (* Update the state with the information obtained from evaluating
         [lv] and [e] *)
      let state = update valuation state in
      let typ = Cil.typeOfLval lv.lval in
      (* Extract the abstract value *)
      let v = Eval.value_assigned v in
      (* Store the information [lv = e;] in the state *)
      let state = bind_loc lv.lloc typ v state in
      `Value state

    (* Abstraction of a conditional. All information inferred by the engine
       is present in the valuation, and must be stored in the memory
       abstraction of the domain itself. *)
    let assume _stmt _e _pos valuation state =
      let state = update valuation state in
      `Value state

    let start_call _stmt _call _valuation state =
      Eval.(Compute (Continue state, true))

    let dump_current_state state =
      let l = fst (Cil.CurrentLoc.get ()) in
      Value_parameters.result ~dkey "DUMPING SIGNS STATE \
                                     of file %s line %d@.%a"
        (Filepath.pretty l.Lexing.pos_fname) l.Lexing.pos_lnum
        MapLatticeSign.pretty state

    let finalize_call _stmt call ~pre:_ ~post =
      let kf = call.Eval.kf in
      let name = Kernel_function.get_name kf in
      if Ast_info.is_cea_dump_function name &&
         Value_parameters.is_debug_key_enabled dkey
      then dump_current_state post;
      `Value post
    
    let default_call _stmt call state =
      approximate_call call.Eval.kf state
    
    let enter_loop _ state = state
    let incr_loop_counter _ state = state
    let leave_loop _ state = state

  end

  let compute_using_specification _ki (kf, _spec) state =
    approximate_call kf state

  (* Memexec *)
  let filter_by_bases _bases state = state
  let reuse ~current_input:state ~previous_output:_ = state

  (* Initial state. *)
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

  let top_query = `Value (Sign_values.top, ()), Alarmset.all
  
  (* This function returns the information known about the location
     corresponding to [_lv], so that it may be used by the engine during
     evaluation. *)
  let extract_lval _oracle state _lv typ loc =
    let v = MapLatticeSign.find loc typ state in
    `Value (v, ()), Alarmset.all
  let extract_expr _oracle _state _expr = top_query
  
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
