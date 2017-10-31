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
open Eval

module type Value = sig
  include Datatype.S
  val top : t
  val join : t -> t -> t
  val widen : t -> t -> t
  val narrow : t -> t -> t or_bottom
  val is_included : t -> t -> bool
  val track_variable: Cil_types.varinfo -> bool
  val pretty_debug: t Pretty_utils.formatter
end

module type S = sig
  type t
  type value
  val add: Precise_locs.precise_location -> Cil_types.typ -> value -> t -> t
  val find: Precise_locs.precise_location -> Cil_types.typ -> t -> value
  val remove: Precise_locs.precise_location -> t -> t
  val remove_variables: Cil_types.varinfo list -> t -> t
  val fold: (Base.t -> value -> 'a -> 'a) -> t -> 'a -> 'a
end

module Make_Memory (Value: Value) = struct

  module Initial_Values = struct let v = [] end
  module Deps = struct let l = [Ast.self] end

  include Hptmap.Make (Base) (Value)(Hptmap.Comp_unused) (Initial_Values) (Deps)

  let cache_name s =
    Hptmap_sig.PersistentCache ("Value." ^ Value.name ^ "." ^ s)

  let narrow =
    let module E = struct exception Bottom end in
    let cache = cache_name "narrow" in
    let decide _ v1 v2 =
      match Value.narrow v1 v2 with
      | `Bottom -> raise E.Bottom
      | `Value v -> v
    in
    fun a b ->
      try `Value (join ~cache ~symmetric:true ~idempotent:true ~decide a b)
      with E.Bottom -> `Bottom

  let join =
    let cache = cache_name "join" in
    let decide _ v1 v2 =
      let r = Value.join v1 v2 in
      if Value.(equal top r) then None else Some r
    in
    inter ~cache ~symmetric:true ~idempotent:true ~decide

  let widen =
    let cache = cache_name "widen"  in
    let decide _ b1 b2 =
      let r = Value.widen b1 b2 in
      if Value.(equal top r) then None else Some r
    in
    inter ~cache ~symmetric:false ~idempotent:true ~decide

  let is_included =
    let cache = cache_name "is_included" in
    let decide_fst _b _v1 = true (* v2 is top *) in
    let decide_snd _b _v2 = false (* v1 is top, v2 is not *) in
    let decide_both _ v1 v2 = Value.is_included v1 v2 in
    let decide_fast s t = if s == t then PTrue else PUnknown in
    binary_predicate cache UniversalPredicate
      ~decide_fast ~decide_fst ~decide_snd ~decide_both

  let top = empty

  type loc_for_base = Precise | Imprecise

  (* Checks whether the offset [o] and the size [size] corresponds to the
     tracked location for [b].
     The conditions are as follow:
     - the variable corresponding to [b] is not volatile.
     - the variable corresponding to [b] must be tracked.
     - the location must assign the entire variable.
     - the type of the variable matches [typ]. *)
  let covers_base b o size typ =
    match b with
    | Base.Var (vi, Base.Known (_, max)) -> (* "standard" varinfos only *)
      if not (Cil.typeHasQualifier "volatile" vi.vtype) &&
         Value.track_variable vi &&
         Cil_datatype.Typ.equal typ vi.vtype &&
         Ival.is_zero o &&
         (match size with
          | Int_Base.Value size -> Integer.equal size (Integer.succ max)
          | Int_Base.Top -> false)
      then Precise
      else Imprecise
    | _ -> Imprecise

  let find_or_top b state = try find b state with Not_found -> Value.top

  let add loc typ v state =
    let open Locations in
    let {loc; size} = Precise_locs.imprecise_location loc in
    (* exact means that the location is precise and that we can perform
       a strong update. *)
    let exact = Location_Bits.cardinal_zero_or_one loc in
    let aux_base b o state =
      match covers_base b o size typ with
      | Precise ->
        (* The location exactly matches [b]: we are able to store the result.
           If the location is not exact, performs a weak update: join [v] with
           the current value for [b]. *)
        let v = if exact then v else Value.join v (find_or_top b state) in
        (* Store the new value unless it is top. In this case, drop it for
           canonicity. *)
        if Value.(equal v top)
        then remove b state
        else add b v state
      | Imprecise -> remove b state
    in
    Location_Bits.fold_topset_ok aux_base loc state

  let remove_variables vars state =
    let remove_variable state v = remove (Base.of_varinfo v) state in
    List.fold_left remove_variable state vars

  let remove loc state =
    let loc = Precise_locs.imprecise_location loc in
    Locations.(Location_Bits.fold_bases remove loc.loc state)

  let find loc typ state =
    let open Locations in
    let {loc; size} = Precise_locs.imprecise_location loc in
    let aux_base b o r =
      (* We degenerate to Top as soon as we find an imprecise location,
         or a base which is not bound in the map. *)
      match covers_base b o size typ with
      | Precise -> Bottom.join Value.join r (`Value (find_or_top b state))
      | Imprecise -> `Value Value.top
    in
    match Location_Bits.fold_topset_ok aux_base loc `Bottom with
    | `Bottom -> Value.top (* does not happen if the location is not empty *)
    | `Value v -> v

end

module Make_Internal (Info: sig val name: string end) (Value: Value) = struct

  include Make_Memory (Value)

  let name = Info.name

  type state = t
  type value = Value.t
  type location = Precise_locs.precise_location

  let key = Structure.Key_Domain.create_key (Info.name ^ " domain")
  let structure : t Abstract_domain.structure = Abstract_domain.Leaf key

  let log_category = Value_parameters.register_category ("d-" ^ Info.name)

  let widen _kf _stmt = widen

  (* This function returns the information known about the location
     corresponding to [_lv], so that it may be used by the engine during
     evaluation. *)
  let extract_lval _oracle state _lv typ loc =
    let v = find loc typ state in
    `Value (v, ()), Alarmset.all

  let extract_expr _oracle _state _expr = `Value (Value.top, ()), Alarmset.all

  let backward_location state _lval typ loc _value =
    let new_value = find loc typ state in
    `Value (loc, new_value)

  let reduce_further _state _expr _value = []

  type origin = unit

  module Transfer
      (Valuation: Abstract_domain.Valuation with type value := value
                                             and type origin := origin
                                             and type loc := location)
  = struct

    (* This function binds [loc] to [v], of type [typ], in [state].
       [v] can be [`Bottom], which means that its contents are guaranteed
       to be indeterminate (e.g. unitialized data). *)
    let bind_loc loc typ v state =
      match v with
        (* We are adding a "good" value. Store it in the state. *)
      | `Value v -> add loc typ v state
        (* Indeterminate value. Drop the information known for loc. *)
      | `Bottom -> remove loc state

    (* This function updates [state] with information for [expr], only possible
       when it is an lvalue. In this case, we can update the corresponding
       location with the result of the evaluation of [exp]. Both the value and
       the location are found in the [valuation]. *)
    let assume_exp valuation expr record state =
      match expr.enode with
      | Lval lv -> begin
          match Valuation.find_loc valuation lv with
          | `Top -> state
          | `Value {loc; typ} -> bind_loc loc typ record.value.v state
        end
      | _ -> state

    (* This function fills [state] according to the information available
       in [valuation]. This information is computed by EVA's engine for
       all the expressions involved in the current statement. *)
    let update valuation state =
      Valuation.fold (assume_exp valuation) valuation state

    (* Abstraction of an assignment. *)
    let assign _kinstr lv _expr value valuation state =
      (* Update the state with the information obtained from evaluating
         [lv] and [e] *)
      let state = update valuation state in
      (* Extract the abstract value *)
      let value = Eval.value_assigned value in
      (* Store the information [lv = e;] in the state *)
      let state = bind_loc lv.lloc lv.ltyp value state in
      `Value state

    (* Abstraction of a conditional. All information inferred by the engine
       is present in the valuation, and must be stored in the memory
       abstraction of the domain itself. *)
    let assume _stmt _expr _pos valuation state =
      `Value (update valuation state)

    let start_call _stmt _call _valuation state =
      Eval.Compute state

    let finalize_call _stmt _call ~pre:_ ~post = `Value post

    let approximate_call _stmt call state =
      let state =
        if Ast_info.is_frama_c_builtin (Kernel_function.get_name call.kf) ||
           (* Frama-C standard library uses volatile variables to model flow
              information about e.g. the filesystem, but this domain does not
              track them. So it is always correct to skip calls to functions that
              only influence such variables.  *)
           Eval_typ.kf_assigns_only_result_or_volatile call.kf
        then state
        else top
      in
      `Value [state]

    let show_expr valuation state fmt expr =
      match expr.enode with
      | Lval lval ->
        begin
          match Valuation.find_loc valuation lval with
          | `Top -> ()
          | `Value {loc; typ} -> Value.pretty fmt (find loc typ state)
        end
      | _ -> ()
  end

  let enter_scope _kf _vars state = state
  let leave_scope _kf vars state = remove_variables vars state

  let enter_loop _ state = state
  let incr_loop_counter _ state = state
  let leave_loop _ state = state

  let logic_assign _assign location ~pre:_ state = remove location state
  let evaluate_predicate _ _ _ = Alarmset.Unknown
  let reduce_by_predicate _ state _ _ = `Value state

  let empty () = top
  let introduce_globals _varinfos state = state
  let initialize_variable _lval _location ~initialized:_ _value state = state
  let initialize_variable_using_type _kind _varinfo state = state

  let filter_by_bases _bases state = state
  let reuse ~current_input:_ ~previous_output:state = state

  let storage () = true
end


module Make_Domain (Info: sig val name: string end) (Value: Value) =
struct
  module M = Make_Internal (Info) (Value)
  include Domain_builder.Complete (M)
  let add = M.add
  let find = M.find
  let remove = M.remove
  let remove_variables = M.remove_variables
  let fold = M.fold
end
