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
open Locations

let dkey = Value_parameters.register_category "d-symblocs"

module K = Hcexprs
module V = Cvalue.V (* TODO: functorize (with locations too ?) *)


(* Map from expressions/lvalues to abstract values *)
module K2V = struct
  module M = Hptmap.Make(K.HCE)(V)(Hptmap.Comp_unused)
      (struct let v = [] end)(struct let l = [Ast.self] end)

  include M

  let cache_prefix = "Value.Symbolic_locs.K2V"

  let join =
    (* Missing keys are bound to top -> use inter as base function *)
    let cache_name = cache_prefix ^ ".join" in
    let cache = Hptmap_sig.PersistentCache cache_name in
    let symmetric = true in
    let idempotent = true in
    let decide _ v1 v2 = Some (V.join v1 v2) in
    M.inter ~cache ~symmetric ~idempotent ~decide

  let widen =
    let cache = Hptmap_sig.NoCache in
    let symmetric = false in
    let idempotent = true in
    let wh = Integer.zero, fun _b -> Ival.Widen_Hints.empty in
    let decide _ v1 v2 = Some (V.widen wh v1 v2) in
    M.inter ~cache ~symmetric ~idempotent ~decide

  let _narrow =
    let module E = struct exception Bottom end in
    let cache_name = cache_prefix ^ ".narrow" in
    let cache = Hptmap_sig.PersistentCache cache_name in
    let symmetric = true in
    let idempotent = true in
    let decide _ v1 v2 =
      let v = V.narrow v1 v2 in
      if V.is_bottom v then raise E.Bottom else v
    in
    fun a b ->
      try `Value (M.join ~cache ~symmetric ~idempotent ~decide a b)
      with E.Bottom -> `Bottom

  let is_included =
    let cache_name = cache_prefix ^ ".is_included" in
    let decide_fst _b _v1 = true (* v2 is top *) in
    let decide_snd _b _v2 = false (* v1 is top, v2 should not be *) in 
    let decide_both _ v1 v2 = V.is_included v1 v2 in
    let decide_fast s t =
      if s == t || M.is_empty t (*all bases present in s but not in t
                  are implicitly bound to Top in t, hence the inclusion holds *)
      then M.PTrue
      else M.PUnknown
    in
    M.binary_predicate
      (Hptmap_sig.PersistentCache cache_name) M.UniversalPredicate
      ~decide_fast ~decide_fst ~decide_snd ~decide_both

  (* Return the subtrees of the left map whose keys are *not* present in the
     right map. Values are ignored *)
  let only_in_left =
    let cache_name = cache_prefix ^ ".only_left" in
    let cache = Hptmap_sig.PersistentCache cache_name in
    let symmetric = false in
    let idempotent = false in
    let decide_both _ _ _ = None in
    let decide_left = M.Neutral in
    let decide_right = M.Absorbing in
    M.merge ~cache
      ~symmetric ~idempotent ~decide_both ~decide_left ~decide_right

end

(* (* not used for now: too costly *)
let rec interesting_exp (e: exp) = match e.enode with
  | Const _ | SizeOf _ | SizeOfStr _ | SizeOfE _ | AlignOf _ | AlignOfE _
  | StartOf _ | AddrOf _ ->
    false
  | Lval lv -> true
  | CastE (_,e) | UnOp (_,e,_) | Info (e,_) ->
    interesting_exp e
  | BinOp (op,e1,e2,_) ->
    match op with
    | Eq | Ne | Le | Ge | Lt | Gt -> false
    | _ -> interesting_exp e1 || interesting_exp e2
*)

(* computes whether an expression depends on a location with an imprecise
   location *)
let rec multiple_loc_exp get_locs (e: exp) = match e.enode with
  | Const _ | SizeOf _ | SizeOfStr _ | SizeOfE _ | AlignOf _ | AlignOfE _
  | StartOf _ | AddrOf _ ->
    false
  | Lval lv ->
    not (Precise_locs.cardinal_zero_or_one (get_locs lv))
  | CastE (_,e) | UnOp (_,e,_) | Info (e,_) ->
    multiple_loc_exp get_locs e
  | BinOp (_,e1,e2,_) ->
    multiple_loc_exp get_locs e1 || multiple_loc_exp get_locs e2

let is_cond exp = match exp.enode with
  | BinOp ((Eq | Ne | Le | Ge | Lt | Gt), _, _, _) -> true
  | _ -> false

(* Locals and formals syntactically present in an expression or lvalue *)
let rec vars_lv (h, o) = Base.Set.union (vars_host h) (vars_offset o)
and vars_exp (e: exp) = match e.enode with
  | Const _ | SizeOf _ | AlignOf _ | SizeOfStr _ ->
    Base.Set.empty
  | AddrOf lv | StartOf lv | Lval lv ->
    vars_lv lv
  | SizeOfE e | AlignOfE e | CastE (_,e) | UnOp (_,e,_) | Info (e,_) ->
    vars_exp e
  | BinOp (_,e1,e2,_) -> Base.Set.union (vars_exp e1) (vars_exp e2)
and vars_host = function
  | Var vi ->
    (* Global variables never go out of scope, no need to track them *)
    if vi.vglob then Base.Set.empty else Base.(Set.singleton (of_varinfo vi))
  | Mem e -> vars_exp e
and vars_offset = function
  | NoOffset -> Base.Set.empty
  | Field (_, o) -> vars_offset o
  | Index (e, o) -> Base.Set.union (vars_exp e) (vars_offset o)

(* Legacy names *)
module B2K = K.BaseToHCESet
module K2Z = K.HCEToZone

module Memory = struct

  (* This is the abstract state for the 'Symbolic location' domains *)
  type memory = {
    values: K2V.t (* map from expressions/lvalues to their abstract value *);
    zones: K2Z.t (* map from expressions/lvalues to the memory location
                    they depend on *);
    deps: B2K.t (* map from bases to the expressions/lvalues that
                   depend on them according to [zones] *);
    syntactic_deps: B2K.t (* map from bases to the expressions/lvalues
                             that syntactically refer to them *);
  }
  (* Invariants: [values] and [zones] have exactly the same keys.
     [deps] and [syntactic_deps] are caches that can be rebuilt from [values]
     and [vars_exp/lv] for [syntactic_deps], and from [zones] for [deps]. *)

  include Datatype.Make_with_collections(struct
      include Datatype.Serializable_undefined
      
      type t = memory
      let name = "Value.Symbolic_locs.Memory.t"

      let reprs = [ { values = List.hd K2V.M.reprs;
                      zones = List.hd K2Z.reprs;
                      deps = List.hd B2K.reprs;
                      syntactic_deps = List.hd B2K.reprs;
                    } ]

      let structural_descr =
        Structural_descr.t_record [|
          K2V.packed_descr;
          K2Z.packed_descr;
          B2K.packed_descr;
          B2K.packed_descr;
        |]

      let compare m1 m2 =
        let c = K2V.compare m1.values m2.values in
        if c <> 0 then c
        else
          let c = K2Z.compare m1.zones m2.zones in
          if c <> 0 then c
          else
            let c = B2K.compare m1.deps m2.deps in
            if c <> 0 then c
            else B2K.compare m1.syntactic_deps m2.syntactic_deps

      let equal = Datatype.from_compare

      let pretty fmt m =
        Format.fprintf fmt "@[<v>V: @[%a@]@ Z: @[%a@]@ I: @[%a@]@ S: @[%a@]@]"
          K2V.M.pretty m.values K2Z.pretty m.zones
          B2K.pretty m.deps B2K.pretty m.syntactic_deps

      let hash m = Hashtbl.hash
          (K2V.hash m.values, K2Z.hash m.zones,
           B2K.hash m.deps, B2K.hash m.syntactic_deps)

      let copy c = c

    end)

  let top = {
    values = K2V.M.empty;
    zones = K2Z.empty;
    deps = B2K.empty;
    syntactic_deps = B2K.empty;
  }

  let empty_map = top

  let is_included m1 m2 =
    K2V.is_included m1.values m2.values &&
    K2Z.is_included m1.zones m2.zones
  (* No need to check the two other fields, that are only inverse mappings
     from the first two ones *)

  (* bases on which a Cvalue.V depends *)
  let v_deps v =
    let aux b acc =
      let add =
        match b with
        | Base.Var (vi, _) -> not vi.vglob
        | Base.Allocated _ -> true (* can be freed. TODO: handle free *)
        | Base.Null | Base.CLogic_Var _ -> false (* does not appear yet *)
        | Base.String _ -> false (* can be seen as a global*)
      in
      if add then Base.Set.add b acc else acc
    in
    V.fold_bases aux v Base.Set.empty

  let key_deps k =
    match K.HCE.get k with
    | K.E e -> vars_exp e
    | K.LV lv -> vars_lv lv

  (* Auxiliary function that add [k] to [state]. [v] is the value bound to
     [k], [z] the dependency information. *)
  let add_key k v z state =
    let values = K2V.add k v state.values in
    let zones = K2Z.add k z state.zones in
    let add_dep b deps =
      let s = B2K.find_default b deps in
      let s' = K.HCESet.add k s in
      B2K.add b s' deps
    in
    try
      let deps = Zone.fold_bases add_dep z state.deps in
      let bases = Base.Set.union (key_deps k) (v_deps v) in
      let syntactic_deps = Base.Set.fold add_dep bases state.syntactic_deps in
      { values; zones; deps; syntactic_deps }
    with Abstract_interp.Error_Top (* unknown dependencies *) -> state

  (* rebuild the state from scratch, especially [deps] and [syntactic_deps].
     For debugging purposes. *)
  let rebuild state =
    let aux k v acc =
      let z =
        try K2Z.find k state.zones
        with Not_found ->
          Value_parameters.abort "Missing zone for %a@.%a"
            K.HCE.pretty k pretty state
      in
      add_key k v z acc
    in
    K2V.fold aux state.values empty_map

  (* check that a state is correct w.r.t. the invariants on [deps] and
     [syntactic_deps]. *)
  let _check state =
    assert (equal state (rebuild state))

  (* inverse operation of [add_key] *)
  let remove_key k state =
    try
      let v = K2V.find k state.values in
      let values = K2V.remove k state.values in
      let zones = K2Z.remove k state.zones in
      let aux_deps b d =
        let set_b = try B2K.find b d with Not_found -> assert false in
        let set_b' = K.HCESet.remove k set_b in
        if K.HCESet.is_empty set_b'
        then B2K.remove b d
        else B2K.add b set_b' d
      in
      (* there exists a dependency associated to k because d(values)=d(zones) *)
      let z =
        try K2Z.find_default k state.zones with Not_found ->assert false
      in
      let deps = Zone.fold_bases aux_deps z state.deps in
      let syn_deps = Base.Set.union (key_deps k) (v_deps v) in
      let syntactic_deps =
        Base.Set.fold aux_deps syn_deps state.syntactic_deps
      in
      { values; zones; deps; syntactic_deps }
    with Not_found -> state

  let remove_keys keys state =
    K.HCESet.fold remove_key keys state

  let join m1 m2 =
    if K2V.equal m1.values m2.values && K2Z.equal m1.zones m2.zones then m1
    else
      let remove_m1 = K2V.only_in_left m1.values m2.values in
      let remove_m2 = K2V.only_in_left m2.values m1.values in
      let m1 = K2V.fold (fun k _ m -> remove_key k m) remove_m1 m1 in
      let m2 = K2V.fold (fun k _ m -> remove_key k m) remove_m2 m2 in
      { values = K2V.join m1.values m2.values;
        zones = K2Z.union m1.zones m2.zones;
        deps = B2K.union m1.deps m2.deps;
        syntactic_deps = B2K.union m1.syntactic_deps m2.syntactic_deps;
      }

  let widen _kf _wh m1 m2 =
    if K2V.equal m1.values m2.values && K2Z.equal m1.zones m2.zones
    then m1
    else { m2 with values = K2V.widen m1.values m2.values }

  (* TODO *)
  let narrow m1 _m2 = `Value m1

  (* ------------------------------------------------------------------------ *)
  (* --- High-level functions                                             --- *)
  (* ------------------------------------------------------------------------ *)

  (* fold on all the keys of [state] overwritten when [z] is written *)
  let fold_overwritten f state z acc =
    (* Check if [k] is overwritten *)
    let aux_key k acc =
      let z_k = K2Z.find k state.zones in
      if Zone.intersects z z_k then f k acc else acc
    in
    (* Check the keys overwritten among those depending on [b] *)
    let aux_base b acc =
      let keys = B2K.find_default b state.deps in
      K.HCESet.fold aux_key keys acc
    in
    try
      (* Check all the keys overwritten *)
      Zone.fold_bases aux_base z acc
    with Abstract_interp.Error_Top -> top

  (* remove the keys that depend on the variables in [l] *)
  let remove_variables l state =
    let aux_vi state vi =
      let b = Base.of_varinfo vi in
      let keys = B2K.find_default b state.syntactic_deps in
      remove_keys keys state
    in
    List.fold_left aux_vi state l

  let kill loc state =
    let z = Locations.enumerate_valid_bits ~for_writing:false loc in
    fold_overwritten remove_key state z state

  (* Add the the mapping [lv --> v] to [state] when possible.
     [get_z] is a function that computes dependencies. *)
  let add_lv state get_z lv v  =
    if Eval_typ.lval_contains_volatile lv then
      state
    else
      let k = K.HCE.of_lval lv in
      let for_writing = false in
      let z_lv = Precise_locs.enumerate_valid_bits ~for_writing (get_z lv) in
      let z_lv_indirect = Value_util.indirect_zone_of_lval get_z lv in
      if Locations.Zone.intersects z_lv z_lv_indirect then
        (* The location of [lv] intersects with the zones needed to compute
           itself, the equality would not hold. *)
        state
      else
        let z = Zone.join z_lv z_lv_indirect in
        add_key k v z state

  (* Add the mapping [e --> v] to [state] when possible and useful.
     [get_z] is a function that computes dependencies. *)
  let add_exp state get_z e v =
    if Eval_typ.expr_contains_volatile e then
      state
    else
      let k = K.HCE.of_exp e in
      let z = Value_util.zone_of_expr get_z e in
      add_key k v z state

  let find k state =
    try Some (K2V.find k state.values)
    with Not_found -> None

  let find_lval lv state =
    find (K.HCE.of_lval lv) state

  let find_expr expr state =
    find (K.HCE.of_exp expr) state

end

module Internal : Domain_builder.InputDomain
  with type state = Memory.t
   and type value = V.t
   and type location = Precise_locs.precise_location
= struct
  type state = Memory.t
  type value = V.t
  type location = Precise_locs.precise_location
  include (Memory: sig
             include Datatype.S_with_collections with type t = state
             include Abstract_domain.Lattice with type state := state
           end)

  let name = "Symbolic locations domain"
  let structure = Abstract_domain.Void
  let log_category = dkey

  let empty _ = Memory.empty_map

  let enter_scope _kf _vars state = state
  let leave_scope _kf vars state =
    (* removed variables revert implicitly to Top *)
    Memory.remove_variables vars state

  let enter_loop _ state = state
  let incr_loop_counter _ state = state
  let leave_loop _ state = state

  type origin = unit

  module Transfer (Valuation: Abstract_domain.Valuation
                   with type value = value
                    and type origin = origin
                    and type loc = Precise_locs.precise_location)
    : Abstract_domain.Transfer
      with type state := state
       and type value := V.t
       and type location := Precise_locs.precise_location
       and type valuation := Valuation.t
  = struct

    (* build a [get_locs] function from a valuation *)
    let get_locs valuation =
      fun lv ->
        let r =
          match Valuation.find_loc valuation lv with
          | `Top -> Precise_locs.loc_top
          | `Value loc -> loc.Eval.loc
        in
        if Precise_locs.(equal_loc loc_top r) then
          Value_parameters.fatal "Unknown location for %a" Printer.pp_lval lv
        else r

    (* update the state according to the information known in the valuation.
       Important, because on statements such as [if (t[i] + j <= 3)], the
       interesting information on [t[i]] is only in the valuation. *)
    let update valuation state =
      let aux e r state =
        let v = r.value in
        (* TODO: incorporate DB criterion: only expressions that are immediate
           lvalues, or that embed two non-singleton lvalues for the first
           time. *)
        match r.reductness, v.v, v.initialized, v.escaping with
        | (Created | Reduced), `Value v, true, false ->
          if not (is_cond e) && multiple_loc_exp (get_locs valuation) e then
            begin
              let k = K.HCE.of_exp e in
              (* remove the existing binding: the key may already be in
                 the state, and [add_exp] assumes it is not the case.
                 The new dependencies may not be the same (in rare cases
                 where one dependency has disappeared by reduction), so
                 we need to update the dependency inverse maps. *)
              (* TODO: it would be more efficient to use a function that
                 compares the previous and current dependencies, and update
                 the inverse maps accordingly. *)
              let state = Memory.remove_key k state in
              Memory.add_exp state (get_locs valuation) e v
            end
          else
            state
        | _ -> state
      in
      Valuation.fold aux valuation state

    let store_value valuation lv loc state v =
      let loc = Precise_locs.imprecise_location loc in
      (* Remove the keys that are overwritten because [loc] is written *)
      let state = Memory.kill loc state in
      if Locations.cardinal_zero_or_one loc then
        (* Stored by the standard domain. Skip *)
        `Value state
      else
        (* Add the new binding *)
        `Value (Memory.add_lv state (get_locs valuation) lv v)

    (* Assume we may be copying indeterminate bits. Kill existing information *)
    let store_indeterminate state loc =
      let loc = Precise_locs.imprecise_location loc in
      `Value (Memory.kill loc state)

    let store_copy valuation lv loc state fv =
      if Cil.isArithmeticOrPointerType lv.ltyp then
        match fv.v, fv.initialized, fv.escaping with
        | `Value v, true, false -> store_value valuation lv.lval loc state v
        | _ -> store_indeterminate state loc
      else
        store_indeterminate state loc

    (* perform [lv = e] in [state] *)
    let assign _kinstr lv _e v valuation state =
      let state = update valuation state in
      match v with
      | Copy (_, vc) -> store_copy valuation lv lv.lloc state vc
      | Assign v -> store_value valuation lv.lval lv.lloc state v

    let assume _stmt _exp _pos valuation state = `Value (update valuation state)

    let start_call _stmt _call valuation state =
      let state = update valuation state in
      Compute state

    let finalize_call _stmt _call ~pre:_ ~post = `Value post

    (* Call in which we do not use the body. Return Top, except for builtins
       and functions that do not significantly alter the memory. *)
    let approximate_call _stmt call state =
      let post_state =
        let name = Kernel_function.get_name call.kf in
        if Ast_info.is_frama_c_builtin name ||
           name <> "free" && Eval_typ.kf_assigns_only_result_or_volatile call.kf
        then state
        else top
      in
      `Value [post_state]

    let show_expr _valuation _state _fmt _expr = ()
  end

  let top_query = `Value (V.top, ()), Alarmset.all

  (* For extraction functions, if we have an information about the value,
     this means that the key has been evaluated in all the paths that reach
     this point. Hence, the alarms have already been emitted, and we can
     return [Alarmset.none]. *)

  let extract_expr _oracle state expr =
    match Memory.find_expr expr state with
    | None -> top_query
    | Some v -> `Value (v, ()), Alarmset.none

  let extract_lval _oracle state lv _typ _locs =
    match Memory.find_lval lv state with
    | None -> top_query
    | Some v -> `Value (v, ()), Alarmset.none

  let backward_location _state _lval _typ loc value =
    (* Nothing to do. We could check if [[lval]] intersects [value] and
       return [`Bottom] if it is not the case, but we have already supplied
       [[lval]] during the forward propagation, so the intersection is probably
       always non-empty. *)
    `Value (loc, value)

  let reduce_further _state _expr _value = [] (*Nothing intelligent to suggest*)

  (* Memexec *)
  let filter_by_bases _bases state = state (* TODO *)
  let reuse ~current_input:state ~previous_output:_ =
    state (* TODO *)

  (* Initial state. Initializers are singletons, so we store nothing. *)
  let introduce_globals _ state = state
  let initialize_variable_using_type _ _ state = state
  let initialize_variable _ _ ~initialized:_ _ state = state

  (* Logic *)
  let logic_assign _assigns location ~pre:_ state =
    let loc = Precise_locs.imprecise_location location in
    Memory.kill loc state

  let evaluate_predicate _ _ _ = Alarmset.Unknown
  let reduce_by_predicate _ state _ _ = `Value state

  let storage = Value_parameters.SymbolicLocsStorage.get

end

module D = Domain_builder.Complete (Internal)
