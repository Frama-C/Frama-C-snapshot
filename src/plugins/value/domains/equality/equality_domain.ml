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

module type S = sig
  include Abstract_domain.Internal
  val key : t Abstract_domain.key

  val pretty_debug : Format.formatter -> t -> unit

  type equalities
  val project : t -> equalities
end

let dkey = Value_parameters.register_category "d-eq"

let counter = ref 0

module Make
    (Atom : Equality_term.Atom)
    (Equality : Equality_sig.S_with_collections with type elt = Atom.t)
    (Value : Abstract_value.External)
= struct

  incr counter;;
  let name = Equality.Set.name ^ "domain_(" ^ string_of_int !counter ^ ")"

  include Datatype.Triple_with_collections
      (Equality.Set)
      (Atom.Lmap_Bitwise)
      (Locations.Zone)
      (struct let module_name = name end)

  let key = Structure.Key_Domain.create_key "equality_domain"
  let structure : t Abstract_domain.structure = Abstract_domain.Leaf key

  let project (t, _, _) = t

  type state = Equality.Set.t * Atom.Lmap_Bitwise.t * Locations.Zone.t
  type value = Value.t
  type location = Precise_locs.precise_location
  type origin = unit
  type summary = unit
  module Summary = Datatype.Unit

  let pretty fmt (eqs, _, _) =
    Format.fprintf fmt "@[<v>Eqs: %a@]" Equality.Set.pretty eqs

  let pretty_debug fmt (eqs, deps, modified) =
    Format.fprintf fmt
      "@[<v>@[<hov 2>Eqs: %a@]@.@[<hov 2>Deps: %a@]@.@[<hov 2>Changed: %a@]@]"
      Equality.Set.pretty eqs Atom.Lmap_Bitwise.pretty deps
      Locations.Zone.pretty modified

  let empty = Equality.Set.empty, Atom.Lmap_Bitwise.empty, Locations.Zone.bottom
  let top = Equality.Set.empty, Atom.Lmap_Bitwise.empty, Locations.Zone.top
  let is_included (a, _, y) (b, _, z) =
    Equality.Set.subset b a && Locations.Zone.is_included y z
  let join (e1, d1, z1) (e2, d2, z2) =
    Equality.Set.inter e1 e2, Atom.Lmap_Bitwise.join d1 d2, Locations.Zone.join z1 z2
  let union (e1, d1, z1) (e2, d2, z2) =
    Equality.Set.union e1 e2, Atom.Lmap_Bitwise.join d1 d2, Locations.Zone.join z1 z2
  let join_and_is_included a b =
    join a b, is_included a b

  (* TODO *)
  let widen _kf _stmt a b = join a b

  let atom_to_expr atom = match Atom.get atom with
    | Equality_term.Exp e -> Some e
    | Equality_term.Lvalue lv -> Some (Cil.dummy_exp (Lval lv))
    | _ -> None

  let reduce_further (equalities, _, _) expr value =
    let atom = Atom.of_exp expr in
    match Equality.Set.find_option atom equalities with
    | Some equality ->
      Equality.fold
        (fun atom acc -> match atom_to_expr atom with
           | Some e -> (e, value) :: acc
           | None -> acc)
        equality []
    | None -> []

  let backward_location _state _lv _typ loc value = `Value (loc, value)

  let alarms_inter x y = (* TODO *)
    if Alarmset.is_empty y then x else Alarmset.all

  let coop_eval oracle equalities atom_src =
    match Equality.Set.find_option atom_src equalities with
    | Some equality ->
      let aux_eq atom (accv, accalarms as acc) =
        if Atom.equal atom atom_src then acc (* avoid trivial recursion *)
        else
          match atom_to_expr atom with
          | Some e ->
            let v', alarms = oracle e in
            Bottom.narrow Value.narrow accv v', alarms_inter accalarms alarms
          | None -> acc
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

  let kill zone (equalities, deps, modified_zone) =
    match Atom.Lmap_Bitwise.find deps zone with
    | Atom.Lattice_Set.Top -> top (* never happens in practice *)
    | Atom.Lattice_Set.Set atoms ->
      let equalities = Atom.Hptset.fold Equality.Set.remove atoms equalities in
      let deps =
        Atom.Lmap_Bitwise.add_binding ~reducing:true ~exact:true
          deps zone Atom.Lattice_Set.bottom
      in
      let modified_zone = Locations.Zone.join modified_zone zone in
      equalities, deps, modified_zone

  let approximate_call kf state =
    let post_state =
      let name = Kernel_function.get_name kf in
      if Ast_info.is_frama_c_builtin name
      then state
      else if name = "free"
      then top
      else
        try
          let spec = Annotations.funspec ~populate:false kf in
          let assigns behavior = match behavior.b_assigns with
            | Writes []            -> false
            | Writes [(l, _)]      -> not (Logic_utils.is_result l.it_content)
            | Writes _ | WritesAny -> true
          in
          if List.exists assigns spec.spec_behavior
          then top
          else state
        with
          Annotations.No_funspec _ -> top
    in
    `Value [{ post_state; summary = (); returned_value = None }]

  module Transfer
      (Valuation: Abstract_domain.Valuation
       with type loc = Precise_locs.precise_location)
  = struct

    type state = t
    type value = Value.t
    type location = Precise_locs.precise_location
    type summary = unit
    type valuation = Valuation.t


    let update _valuation state = state


    let add_atom atom zone deps =
      let dep = Atom.Lattice_Set.inject_singleton atom in
      Atom.Lmap_Bitwise.add_binding ~reducing:true ~exact:false deps zone dep


    let rec zone_of_expr valuation expr =
      let rec process expr = match expr.enode with
      | Lval lval -> zone_of_lval valuation lval
      | UnOp (_, e, _) | CastE (_, e) | Info (e, _) -> process e
      | BinOp (_, e1, e2, _) -> Locations.Zone.join (process e1) (process e2)
      | StartOf lv | AddrOf lv -> zone_of_lval valuation lv
      | _ -> Locations.Zone.bottom
      in
      process expr

    and zone_of_lval valuation (lhost, offset as lval) =
      let l = match Valuation.find_loc valuation lval with
        | `Top -> assert false (* TODO *)
        | `Value record -> record.loc
      in
      let z = Locations.enumerate_bits (Precise_locs.imprecise_location l) in
      Locations.Zone.join z
        (Locations.Zone.join
           (zone_of_lhost valuation lhost) (zone_of_offset valuation offset))

    and zone_of_lhost valuation = function
      | Var _ -> Locations.Zone.bottom
      | Mem e -> zone_of_expr valuation e

    and zone_of_offset valuation = function
      | NoOffset -> Locations.Zone.bottom
      | Field (_, o) -> zone_of_offset valuation o
      | Index (e, o) ->
        Locations.Zone.join
          (zone_of_expr valuation e) (zone_of_offset valuation o)

    let get_cvalue = Value.get Main_values.cvalue_key
    let extract_value = function
      | Assign v -> `Value v
      | Copy (_, c) -> match c with
        | Determinate v -> `Value v.v
        | Exact v -> v.v

    let is_singleton = match get_cvalue with
      | None -> fun _ -> false
      | Some get ->
        fun a -> match extract_value a with
          | `Bottom -> true
          | `Value v -> Cvalue.V.cardinal_zero_or_one (get v)

    let assign _stmt left_value right_expr value valuation state =
      let left_loc = Precise_locs.imprecise_location left_value.lloc in
      let left_zone = Locations.enumerate_bits left_loc in
      let state = kill left_zone state in
      let right_zone = zone_of_expr valuation right_expr in
      if
        Locations.Zone.intersects left_zone right_zone
        || Eval_typ.lval_contains_volatile left_value.lval
        || Eval_typ.expr_contains_volatile right_expr
        || Eval_typ.is_bitfield left_value.ltyp
        || (is_singleton value
            && Locations.cardinal_zero_or_one left_loc)
      then `Value state
      else
        let (equalities, deps, modified_zone) = state in
        let lterm = Atom.of_lval left_value.lval in
        let rterm = Atom.of_exp right_expr in
        let equalities = Equality.Set.unite lterm rterm equalities in
        (* Add the zone dependencies of the two atoms. *)
        let left_zone = zone_of_lval valuation left_value.lval in
        let deps = add_atom lterm left_zone deps in
        let deps = add_atom rterm right_zone deps in
        `Value (equalities, deps, modified_zone)

    let assume _stmt expr positive valuation (eqs, deps, modified_zone as state)  =
      match positive, expr.enode with
      | true,  BinOp (Eq, e1, e2, _)
      | false, BinOp (Ne, e1, e2, _) ->
        let a1 = Atom.of_exp e1 in
        let a2 = Atom.of_exp e2 in
        let eqs = Equality.Set.unite a1 a2 eqs in
        let z1 = zone_of_expr valuation e1 in
        let z2 = zone_of_expr valuation e2 in
        let deps = add_atom a1 z1 deps in
        let deps = add_atom a2 z2 deps in
        `Value (eqs, deps, modified_zone)
      | _ -> `Value state

    let call_action _stmt _call _valuation _state =
      Compute (Continue empty, true)

    let summarize _kf _stmt ~returned:_ state = `Value ((), state)

    let resolve_call _stmt call ~assigned _valuation ~pre ~post =
      let state = snd post in
      let kf = call.kf in
      let name = Kernel_function.get_name kf in
      if  Ast_info.is_frama_c_builtin name then begin
        if Ast_info.is_cea_dump_function name &&
           Value_parameters.is_debug_key_enabled dkey
        then begin
          let l = fst (Cil.CurrentLoc.get ()) in
          Value_parameters.result "DUMPING EQ STATE \
                                   of file %s line %d@.%a"
            (Filepath.pretty l.Lexing.pos_fname) l.Lexing.pos_lnum
            pretty state;
        end;
      end;
      let state = match assigned with
        | None -> state
        | Some (loc, _) ->
          let loc = Precise_locs.imprecise_location loc.lloc in
          kill (Locations.enumerate_bits loc) state
      in
      let (_, _, modif) = state in
      let pre' = kill modif pre in
      `Value (union pre' state)

    let default_call _stmt call state =
      approximate_call call.kf state

  end

  let compute_using_specification _ (kf, _) state =
    approximate_call kf state

  type eval_env = state
  let env_current_state state = `Value state
  let env_annot ~pre:_ ~here () = here
  let env_pre_f ~pre () = pre
  let env_post_f ~pre:_ ~post ~result:_ () = post
  let eval_predicate _ _ = Alarmset.Unknown
  let reduce_by_predicate state _ _ = state

  let close_block fundec block ~body state =
    let zones = List.map Locations.zone_of_varinfo block.blocals in
    let zone = List.fold_left Locations.Zone.join Locations.Zone.bottom zones in
    let zone =
      if body
      then
        let formals_zones = List.map Locations.zone_of_varinfo fundec.sformals in
        List.fold_left Locations.Zone.join zone formals_zones
      else zone
    in
    kill zone state

  let open_block _fundec _block ~body:_ state = state

  let empty () = empty
  let initialize_var state _ _ _ = state
  let initialize_var_using_type state _  = state
  let global_state () = None

  let filter_by_bases _ state = state
  let reuse ~current_input:_ ~previous_output:state = state

end
