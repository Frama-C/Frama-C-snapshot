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

type call_init_state =
  | ISCaller
  | ISFormals
  | ISEmpty

let call_init_state () = ISEmpty

(* Handle calls to functions with only a specification as if nothing was
   written. Unsafe. *)
let unsafe_spec_calls = false

module type S = sig
  include Abstract_domain.Internal
  val key : t Abstract_domain.key

  val pretty_debug : Format.formatter -> t -> unit

  type equalities
  val project : t -> equalities
end

let dkey = Value_parameters.register_category "d-eqs"

module type InternalDatatype = sig
  include Datatype.S_with_collections
  include Abstract_domain.Lattice with type state = t
  module Store: Abstract_domain.Store with type state := state
  val structure : t Abstract_domain.structure
  val log_category : Log.category
  val key : t Abstract_domain.key
  type equalities
  val project : t -> equalities
end

let counter = ref 0

open Hcexprs

module Atom = struct

  include HCE

  module Deps = struct
    include Datatype.Pair(HCEToZone)(BaseToHCESet)
    (* Map from expression to its dependencies, and inverse map from the
       bases of the dependencies to the expressions *)

    let empty = HCEToZone.empty, BaseToHCESet.empty

    let join (m1, i1) (m2, i2) =
      HCEToZone.inter m1 m2, BaseToHCESet.inter i1 i2

    let is_included (m1, _) (m2, _) =
      HCEToZone.is_included m1 m2

    let concat (m1, i1) (m2, i2) =
      HCEToZone.union m1 m2, BaseToHCESet.union i1 i2

    let intersects (m, i: t) z =
      let aux_e e acc =
        let z_e = HCEToZone.find_default e m in
        if Locations.Zone.intersects z z_e then
          e :: acc
        else acc
      in
      let aux_base b _ acc =
        let set = BaseToHCESet.find_default b i in
        HCESet.fold aux_e set acc
      in
      (* TODO: a recursive descent would be much more effective *)
      Locations.Zone.fold_topset_ok aux_base z []

    let add e z (m, i : t) =
      let aux_base b _ acc =
        let set = BaseToHCESet.find_default b i in
        let set = HCESet.add e set in
        BaseToHCESet.add b set acc
      in
      let i = Locations.Zone.fold_topset_ok aux_base z i in
      let m = HCEToZone.add e z m in
      (m, i : t)

    let remove e (m, i as state : t) =
      try
        let z = HCEToZone.find e m in
        let aux_base b _ i =
          let s = BaseToHCESet.find_default b i in
          let s = HCESet.remove e s in
          if HCESet.is_empty s then
            BaseToHCESet.remove b i
          else
            BaseToHCESet.add b s i
        in
        let i = Locations.Zone.fold_topset_ok aux_base z i in
        let m = HCEToZone.remove e m in
        (m, i)
      with Not_found -> (* cannot find [e] in [m] *)
        state

  end
end

(* Make datatypes independent from the value abstraction. *)
module MakeDatatype
    (Equality : Equality_sig.S_with_collections with type elt = Atom.t)
= struct

  module Internal = struct

    incr counter;;
    let name = Equality.Set.name ^ "domain_(" ^ string_of_int !counter ^ ")"

    include Datatype.Triple_with_collections
        (Equality.Set)
        (Atom.Deps)
        (Locations.Zone) (* memory zones that have been overwritten since
                            the beginning of the function. Not used when the
                            state of the caller is used as initial state. *)
        (struct let module_name = name end)

    type state = t

    let name = "Equality domain"
    let key = Structure.Key_Domain.create_key name
    let structure : t Abstract_domain.structure = Abstract_domain.Leaf key
    let log_category = dkey

    type equalities = Equality.Set.t
    let project (t, _, _) = t

    let top = Equality.Set.empty, Atom.Deps.empty, Locations.Zone.top
    let is_included (a, _, y) (b, _, z) =
      Equality.Set.subset b a && Locations.Zone.is_included y z
    let join (e1, d1, z1) (e2, d2, z2) =
      Equality.Set.inter e1 e2, Atom.Deps.join d1 d2, Locations.Zone.join z1 z2

    (* TODO *)
    let widen _kf _stmt a b = join a b

    let narrow (e1, d1, z1) (e2, d2, z2) =
      if Atom.Deps.equal d1 d2
      then `Value (Equality.Set.union e1 e2, d1, Locations.Zone.narrow z1 z2)
      else `Value (e1, d1, z1)

    let storage = Value_parameters.EqualityStorage.get

  end

  include Internal
  module Store = Domain_store.Make (Internal)

end

module MakeDomain
    (Equality : Equality_sig.S_with_collections with type elt = Atom.t)
    (Internal: InternalDatatype with type t = Equality.Set.t *
                                            Atom.Deps.t *
                                            Locations.Zone.t)
    (Value : Abstract_value.External)
= struct

  include Internal

  let get_cvalue = Value.get Main_values.cvalue_key

  type value = Value.t
  type location = Precise_locs.precise_location
  type origin = unit

  let pretty fmt (eqs, _, _) = Equality.Set.pretty fmt eqs

  let pretty_debug fmt (eqs, deps, modified) =
    Format.fprintf fmt
      "@[<v>@[<hov 2>Eqs: %a@]@.@[<hov 2>Deps: %a@]@.@[<hov 2>Changed: %a@]@]"
      Equality.Set.pretty eqs Atom.Deps.pretty deps
      Locations.Zone.pretty modified

  (* let pretty = pretty_debug *)

  let rec fold_tree f t acc =
    match t with
    | Equality_sig.Empty -> acc
    | Equality_sig.Leaf v -> f v acc
    | Equality_sig.Node (t1, t2) -> fold_tree f t2 (fold_tree f t1 acc)

  let empty = Equality.Set.empty, Atom.Deps.empty, Locations.Zone.bottom
  let top = Equality.Set.empty, Atom.Deps.empty, Locations.Zone.top
  let is_included (a, m, y) (b, n, z) =
    Equality.Set.subset b a && Atom.Deps.is_included m n
    && Locations.Zone.is_included y z
  let join (e1, d1, z1) (e2, d2, z2) =
    let e' = Equality.Set.inter e1 e2 in
    let z' = Locations.Zone.join z1 z2 in
    let removed1 = Equality.Set.elements_only_left e1 e' in
    let removed2 = Equality.Set.elements_only_left e2 e' in
    let d1' = fold_tree Atom.Deps.remove removed1 d1 in
    let d2' = fold_tree Atom.Deps.remove removed2 d2 in
    let d' = Atom.Deps.join d1' d2' in
    e', d', z'

  let concat (e1, d1, z1) (e2, d2, z2) =
    Equality.Set.union e1 e2, Atom.Deps.concat d1 d2, Locations.Zone.join z1 z2

  (* TODO *)
  let widen _kf _stmt a b = join a b

  let reduce_further (equalities, _, _) expr value =
    let atom = Atom.of_exp expr in
    match Equality.Set.find_option atom equalities with
    | Some equality ->
      Equality.fold
        (fun atom acc -> (Atom.to_exp atom, value) :: acc)
        equality []
    | None -> []

  let backward_location _state _lv _typ loc value = `Value (loc, value)

  let alarms_inter x y = (* TODO *)
    if Alarmset.is_empty y then x else Alarmset.all

  (* Remove all 'origin' information from the Cvalue component of a value.
     Since we perform evaluations at the current statement, the origin
     information we compute is incompatible with the one obtained from e.g.
     the Cvalue domain. *)
  let imprecise_origin =
    match get_cvalue with
    | None -> fun v -> v
    | Some get ->
      fun v ->
        let c = get v in
        if Cvalue.V.is_imprecise c then
          let c' = Cvalue.V.topify_with_origin Origin.top c in
          Value.set Main_values.cvalue_key c' v
        else v

  let coop_eval oracle equalities atom_src =
    match Equality.Set.find_option atom_src equalities with
    | Some equality ->
      let aux_eq atom (accv, accalarms as acc) =
        if Atom.equal atom atom_src then acc (* avoid trivial recursion *)
        else
          let e = Atom.to_exp atom in
          let v', alarms = oracle e in
          (* Remove 'origin' information *)
          let v' = v' >>-: imprecise_origin in
          Bottom.narrow Value.narrow accv v', alarms_inter accalarms alarms
      in
      Equality.fold aux_eq equality (`Value Value.top, Alarmset.none)
      >>=: fun v -> (v, ())
    | None -> `Value (Value.top, ()), Alarmset.all

  let extract_expr (oracle: exp -> Value.t evaluated) (equalities, _, _) expr =
    let atom_e = Atom.of_exp expr in
    coop_eval oracle equalities atom_e

  let extract_lval oracle (equalities, _, _) lval _typ _location =
    let atom_lv = Atom.of_lval lval in
    coop_eval oracle equalities atom_lv

  (* Type of operation to perform on the 'modified field' when performing a
     kill operation. AddAsModified means that the location has been written,
     and should be added to 'modified'. RemovedFromModified means that
     the variable should be removed instead, for example because it goes
     out of scope. *)
  type kill_type = AddAsModified | RemoveFromModified

  let kill kt zone (equalities, deps, modified_zone) =
    if Locations.Zone.(equal zone top) then
      top
    else
      let atoms = Atom.Deps.intersects deps zone in
      let equalities' = List.fold_right Equality.Set.remove atoms equalities in
      let disappeared =
        Equality.Set.elements_only_left equalities equalities'
      in
      let deps' = fold_tree Atom.Deps.remove disappeared deps in
      (* In ISCaller mode, this field is useless. So we do not
         compute it at all. *)
      let modified_zone' =
        if call_init_state () = ISCaller then modified_zone
        else
          match kt with
          | AddAsModified -> Locations.Zone.join modified_zone zone
          | RemoveFromModified -> Locations.Zone.diff modified_zone zone
      in
      let s' = equalities', deps', modified_zone' in
      s'

  (* assume that [vars] go out of scope, and remove them from the list of
     equalities *)
  let unscope state vars =
    let aux_vi zones vi =
      let z = Locations.zone_of_varinfo vi in
      Locations.Zone.join z zones
    in
    let zone = List.fold_left aux_vi Locations.Zone.bottom vars in
    kill RemoveFromModified zone state

  let approximate_call kf state =
    let post_state =
      let name = Kernel_function.get_name kf in
      if Ast_info.is_frama_c_builtin name ||
         (name <> "free" && Eval_typ.kf_assigns_only_result_or_volatile kf)
      then state
      else if unsafe_spec_calls then state else top
    in
    `Value [post_state]

  module Transfer
      (Valuation: Abstract_domain.Valuation
       with type value = Value.t
        and type loc = Precise_locs.precise_location)
  = struct

    let find_loc valuation = fun lval ->
      match Valuation.find_loc valuation lval with
      | `Top -> assert false (* TODO *)
      | `Value record -> record.loc

    let update _valuation state = state

    let is_singleton = match get_cvalue with
      | None -> fun _ -> false
      | Some get ->
        function
        | `Bottom -> true
        | `Value v -> Cvalue.V.cardinal_zero_or_one (get v)

    let expr_cardinal_zero_or_one valuation e =
      match Valuation.find valuation e with
      | `Top -> false (* should not happen *)
      | `Value { value = { v } } -> is_singleton v

    let expr_is_cardinal_zero_or_one_loc valuation e =
      match e.enode with
      | Lval lv -> begin
          let loc = Valuation.find_loc valuation lv in
          match loc with
          | `Top -> false (* should not happen *)
          | `Value loc -> Precise_locs.cardinal_zero_or_one loc.loc
        end
      | _ -> false (* TODO: handle upcasts *)

    (* Auxiliary function for [assign]. The assignment takes place, unless
       some the of the expressions involved are volatile. [{left,right}_zone]
       are the dependencies of the corresponding lval/expr. *)
    let assign_eq left_lval left_zone right_expr right_zone state =
      if Eval_typ.lval_contains_volatile left_lval ||
         Eval_typ.expr_contains_volatile right_expr
      then state
      else
        let (equalities, deps, modified_zone: t) = state in
        let lterm = Atom.of_lval left_lval in
        let rterm = Atom.of_exp right_expr in
        let equalities = Equality.Set.unite lterm rterm equalities in
        (* Add the zone dependencies of the two atoms. *)
        let deps = Atom.Deps.add lterm left_zone deps in
        let deps = Atom.Deps.add rterm right_zone deps in
        (equalities, deps, modified_zone: t)

    let assign _stmt left_value right_expr value valuation state =
      let open Locations in
      let left_loc = Precise_locs.imprecise_location left_value.lloc in
      let direct_left_zone = Locations.enumerate_bits left_loc in
      let state = kill AddAsModified direct_left_zone state in
      let indirect_left_zone =
        Value_util.indirect_zone_of_lval (find_loc valuation) left_value.lval
      and right_zone = Value_util.zone_of_expr (find_loc valuation) right_expr in
      (* After an assignment lv = e, the equality [lv == eq] holds iff the value
         of [e] and the location of [lv] are not modified by the assignment,
         i.e. iff the dependencies of [e] and of the lhost and offset of [lv]
         do not intersect the assigned location.
         Moreover, the domain do not store the equality when the abstract
         location of [lv] and the abstract value of [e] are singleton, as in
         this case, the main cvalue domain is able to infer the equality. *)
      if (Zone.intersects direct_left_zone right_zone) ||
         (Zone.intersects direct_left_zone indirect_left_zone) ||
         (is_singleton (Eval.value_assigned value) &&
          Locations.cardinal_zero_or_one left_loc)
      then
        `Value state
      else
        (* left_zone contains all dependencies of [left_value] *)
        let left_zone = Zone.join direct_left_zone indirect_left_zone in
        `Value (assign_eq left_value.lval left_zone right_expr right_zone state)

    (* Add the equalities between the formals of a function and the actuals
       at the call. *)
    let assign_formals valuation call state =
      let assign_formal state arg =
        if is_singleton (Eval.value_assigned arg.avalue) then
          state
        else
          let left_value = Var arg.formal, NoOffset in
          let left_zone = Locations.zone_of_varinfo arg.formal in
          let right_zone =
            Value_util.zone_of_expr (find_loc valuation) arg.concrete
          in
          assign_eq left_value left_zone arg.concrete right_zone state
      in
      List.fold_left assign_formal state call.arguments

    let assume _stmt expr positive valuation (eqs, deps, modified_zone as state)  =
      match positive, expr.enode with
      | true,  BinOp (Eq, e1, e2, _)
      | false, BinOp (Ne, e1, e2, _) ->
        if Eval_typ.expr_contains_volatile e1
        || Eval_typ.expr_contains_volatile e2
        || (expr_is_cardinal_zero_or_one_loc valuation e1 &&
            expr_cardinal_zero_or_one valuation e2)
        || (expr_is_cardinal_zero_or_one_loc valuation e2 &&
            expr_cardinal_zero_or_one valuation e1)
        then `Value state
        else
          let a1 = Atom.of_exp e1 in
          let a2 = Atom.of_exp e2 in
          let eqs = Equality.Set.unite a1 a2 eqs in
          let z1 = Value_util.zone_of_expr (find_loc valuation) e1 in
          let z2 = Value_util.zone_of_expr (find_loc valuation) e2 in
          let deps = Atom.Deps.add a1 z1 deps in
          let deps = Atom.Deps.add a2 z2 deps in
          `Value (eqs, deps, modified_zone)
      | _ -> `Value state

    let start_call _stmt call valuation state =
      let state =
        match call_init_state () with
        | ISCaller  -> assign_formals valuation call state
        | ISFormals -> assign_formals valuation call empty
        | ISEmpty   -> empty
      in
      Compute state

    let finalize_call _stmt call ~pre ~post =
      let kf = call.kf in
      (* remove equalities involving formals, that will no longer be in scope.
         equalities on locals have already been removed. *)
      let post = unscope post (Kernel_function.get_formals kf) in
      (* now we compute the state which is sent back to the caller *)
      if call_init_state () = ISCaller then
        `Value post (* [pre] was the state inferred in the caller, and it
                       has been updated during the analysis of [kf] into
                       [post]. Send all the equalities back to the caller. *)
      else
        (* [pre] contains the equalities from the caller, but [post] was
           computed starting from an essentially empty state. We must
           restore the equalities of [pre]. *)
        let (_, _, modif) = post in
        (* Invalidate the equalities that are no longer true. *)
        let pre' = kill AddAsModified modif pre in
        (* then merge the two sets of equalities *)
        `Value (concat pre' post)

    let approximate_call _stmt call state =
      approximate_call call.kf state

    let show_expr _valuation (equalities, _, _) fmt expr =
      let atom = Atom.of_exp expr in
      match Equality.Set.find_option atom equalities with
      | Some equality -> Equality.pretty fmt equality
      | None -> ()
  end

  let logic_assign _assigns location ~pre:_ state =
    let loc = Precise_locs.imprecise_location location in
    let zone = Locations.enumerate_bits loc in
    kill AddAsModified zone state

  let evaluate_predicate _ _ _ = Alarmset.Unknown
  let reduce_by_predicate _ state _ _ = `Value state

  let enter_scope _kf _vars state = state
  let leave_scope _kf vars state = unscope state vars

  let enter_loop _ state = state
  let incr_loop_counter _ state = state
  let leave_loop _ state = state

  let empty () = empty
  let introduce_globals _vars state = state
  let initialize_variable _ _ ~initialized:_ _ state = state
  let initialize_variable_using_type _ _ state  = state

  let filter_by_bases _ state = state
  let reuse ~current_input:_ ~previous_output:state = state

end


module MakeInternal
    (Equality : Equality_sig.S_with_collections with type elt = Atom.t)
    (Value : Abstract_value.External)
= struct
  module Internal = MakeDatatype (Equality)
  include MakeDomain (Equality) (Internal) (Value)
end


(* Default Instantiation. *)
module Name = struct let module_name = "eq" end
module Equality = Equality.Make (Atom) (HCESet) (Name)
module Internal = MakeDatatype (Equality)

module Make (Value : Abstract_value.External) =
  MakeDomain (Equality) (Internal) (Value)
