(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2018                                               *)
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

open Eval

let key = Structure.Key_Domain.create_key "cvalue_domain"
let dkey_card = Value_parameters.register_category "cardinal"

let extract get = match get key with
  | None -> fun _ -> Cvalue.Model.top
  | Some get -> function
    | `Bottom -> Cvalue.Model.bottom
    | `Value state -> get state

module Model = struct

  include Cvalue.Model
  type value = Main_values.CVal.t
  type location = Main_locations.PLoc.location

  (* The origin is the value stored in the state for a lvalue, when this value
     has a type incompatible with the type of the lvalue. This may happen on
     union with fields of different types, or on code pattern such as
       int x = v; float f = *(float* )&x
     In this case, the value stored in the state and the value computed for the
     lvalue can be incomparable. The origin is then used to store the value from
     the state, to later choose which value to keep. This is done by the update
     function in cvalue_transfer. *)
  type origin = value option

  let extract_expr _ _ _ = `Value (Cvalue.V.top, None), Alarmset.all

  let indeterminate_alarms lval v =
    let open Cvalue.V_Or_Uninitialized in
    let status =
      if Cvalue.V.is_bottom (get_v v) then Alarmset.False else Alarmset.Unknown
    in
    match v with
    | C_uninit_noesc _ -> Alarmset.singleton ~status (Alarms.Uninitialized lval)
    | C_init_esc _     -> Alarmset.singleton ~status (Alarms.Dangling lval)
    | C_uninit_esc _   ->
      (* Unknown alarms: [v] can be either dangling or uninit *)
      Alarmset.(set (Alarms.Dangling lval) Unknown
                  (set (Alarms.Uninitialized lval) Unknown none))
    | C_init_noesc _   -> Alarmset.none


  let eval_one_loc state lval typ =
    let eval_one_loc single_loc =
      let v = Cvalue.Model.find_indeterminate state single_loc in
      Cvalue.V_Or_Uninitialized.get_v v, indeterminate_alarms lval v
    in
    (* We have no good neutral element for "no alarm emitted yet", so we use
       [None] instead. *)
    let join_alarms acc alarms =
      match acc with
      | None -> Some alarms
      | Some acc -> Some (Alarmset.union alarms acc)
    in
    fun loc (acc_result, acc_alarms) ->
      let result, alarms = eval_one_loc loc in
      let result = Cvalue_forward.make_volatile ~typ:typ result in
      Cvalue.V.join result acc_result, join_alarms acc_alarms alarms

  (* The zero singleton is shared between float and integer representations in
     ival, and is thus untyped. *)
  let is_float v =
    Cvalue.V.(is_included v top_float) && Cvalue.V.contains_non_zero v

  let extract_scalar_lval state lval typ loc =
    let process_one_loc = eval_one_loc state lval typ in
    let acc = Cvalue.V.bottom, None in
    let value, alarms = Precise_locs.fold process_one_loc loc acc in
    let alarms = match alarms with None -> Alarmset.none | Some a -> a in
    (* The origin is set to false when the value stored in the memory has not
       the same type as the read lvalue. In this case, we don't update the state
       with the new value stemming from the evaluation, even if it has been
       reduced, in order to not propagate incompatible type. *)
    let incompatible_type = is_float value <> Cil.isFloatingType typ in
    let origin = if incompatible_type then Some value else None in
    let value = Cvalue_forward.reinterpret typ value in
    if Cvalue.V.is_bottom value
    then `Bottom, alarms
    else `Value (value, origin), alarms

  (* Imprecise version for aggregate types that cvalues are unable to precisely
     represent. The initialization alarms must remain sound, though. *)
  let extract_aggregate_lval state lval _typ ploc =
    let loc = Precise_locs.imprecise_location ploc in
    match loc.Locations.size with
    | Int_Base.Top -> `Value (Cvalue.V.top, None), Alarmset.all
    | Int_Base.Value size ->
      let offsm = Cvalue.Model.copy_offsetmap loc.Locations.loc size state in
      match offsm with
      | `Bottom -> `Bottom, Alarmset.none
      | `Value offsm ->
        let value = Cvalue.V_Offsetmap.find_imprecise_everywhere offsm in
        let alarms = indeterminate_alarms lval value in
        let v = Cvalue.V_Or_Uninitialized.get_v value in
        let v = if Cvalue.V.is_bottom v then `Bottom else `Value (v, None) in
        v, alarms

  let extract_lval _oracle state lval typ loc =
    if Cil.isArithmeticOrPointerType typ
    then extract_scalar_lval state lval typ loc
    else extract_aggregate_lval state lval typ loc

  let backward_location state _lval typ precise_loc value =
    let size = Precise_locs.loc_size precise_loc in
    let upto = succ (Ival.get_small_cardinal()) in
    let loc = Precise_locs.imprecise_location precise_loc in
    let eval_one_loc single_loc =
      let v = Cvalue.Model.find state single_loc in
      let v = Cvalue_forward.make_volatile ~typ v in
      Cvalue_forward.reinterpret typ v
    in
    let process_ival base ival (acc_loc, acc_val as acc) =
      let loc_bits = Locations.Location_Bits.inject base ival in
      let single_loc = Locations.make_loc loc_bits size in
      let v = eval_one_loc single_loc in
      if Cvalue.V.intersects v value
      then Locations.Location_Bits.join loc_bits acc_loc, Cvalue.V.join v acc_val
      else acc
    in
    let fold_ival base ival acc =
      if Ival.cardinal_is_less_than ival upto
      then Ival.fold_enum (process_ival base) ival acc
      else process_ival base ival acc
    in
    let fold_location loc acc =
      try
        let loc = loc.Locations.loc in
        Locations.Location_Bits.fold_i fold_ival loc acc
      with
        Abstract_interp.Error_Top -> loc.Locations.loc, value
    in
    let acc = Locations.Location_Bits.bottom, Cvalue.V.bottom in
    let loc_bits, value = fold_location loc acc in
    if Locations.Location_Bits.is_bottom loc_bits
    then `Bottom
    else
      let loc = Precise_locs.inject_location_bits loc_bits in
      `Value (Precise_locs.make_precise_loc loc ~size, value)

end


module State = struct

  type state = Model.t * Locals_scoping.clobbered_set

  let structure =
    Abstract_domain.Node (Abstract_domain.Leaf key,
                          Abstract_domain.Leaf Locals_scoping.key)

  let log_category = Value_parameters.dkey_cvalue_domain

  include Datatype.Make_with_collections (
    struct
      include Datatype.Serializable_undefined
      type t = state
      let name = Model.name ^ "+clobbered_set"
      let reprs = List.map (fun s -> s, Locals_scoping.bottom ()) Model.reprs
      let structural_descr =
        Structural_descr.(
          t_tuple [| Model.packed_descr; pack Locals_scoping.structural_descr |])
      let pretty fmt (s, _) = Model.pretty fmt s
      let equal (a, _) (b, _) = Model.equal a b
      let compare (a, _) (b, _) = Model.compare a b
      let hash (s, _) = Model.hash s
      let rehash = Datatype.identity
      let copy = Datatype.undefined
      let mem_project = Datatype.never_any_project
    end )

  let name = "Cvalue domain"

  type value = Model.value
  type location = Model.location

  let top = Model.top, Locals_scoping.bottom ()
  let is_included (a, _) (b, _) = Model.is_included a b
  let join (a, clob) (b, _) = Model.join a b, clob

  let widen kf stmt (a, clob) (b, _) =
    let hint = Widen.getWidenHints kf stmt in
    Model.widen hint a b, clob

  let narrow (a, clob) (b, _) =
    let s = Model.narrow a b in
    if Model.(equal bottom s) then `Bottom else `Value (s, clob)

  type origin = Model.origin

  let extract_expr evaluate (s, _) expr = Model.extract_expr evaluate s expr
  let extract_lval oracle (s, _) lval typ loc =
    Model.extract_lval oracle s lval typ loc
  let backward_location (state, _) lval typ precise_loc value =
    Model.backward_location state lval typ precise_loc value
  let reduce_further _ _ _ = []


  module Transfer
      (Valuation: Abstract_domain.Valuation with type value = value
                                             and type origin = origin
                                             and type loc = location)
  = struct

    module T = Cvalue_transfer.Transfer (Valuation)

    let update valuation (s, clob) = T.update valuation s, clob

    let assign stmt lv expr assigned valuation (s, clob) =
      T.assign stmt lv expr assigned valuation s >>-: fun s ->
      (* TODO: use the value in assignment *)
      let _ =
        Eval.value_assigned assigned >>-: fun value ->
        let location = Precise_locs.imprecise_location lv.lloc in
        Locals_scoping.remember_if_locals_in_value clob location value
      in
      s, clob

    let assume stmt expr positive valuation (s, clob) =
      T.assume stmt expr positive valuation s >>-: fun s ->
      s, clob

    let start_call stmt call valuation (s, _clob) =
      T.start_call stmt call valuation s >>-: fun state ->
      state, Locals_scoping.bottom ()

    let finalize_call stmt call ~pre ~post =
      let (post_state, post_clob) = post
      and pre_state, clob = pre in
      Locals_scoping.(remember_bases_with_locals clob post_clob.clob);
      T.finalize_call stmt call ~pre:pre_state ~post:post_state
      >>-: fun state ->
      state, clob

    let show_expr valuation (state, _) = T.show_expr valuation state
  end

  (* ------------------------------------------------------------------------ *)
  (*                                 Mem Exec                                 *)
  (* ------------------------------------------------------------------------ *)

  let relate _kf _bases _state = Base.SetLattice.empty

  (* Auxiliary function that keeps only some bases inside a memory state *)
  let filter _kf _kind bases (state, clob) =
    Cvalue.Model.filter_by_shape (Base.Hptset.shape bases) state, clob

  let reuse _ _ ~current_input:(state, _) ~previous_output:(output, clob) =
    Cvalue.Model.merge ~into:state output, clob

  (* ------------------------------------------------------------------------ *)
  (*                                  Logic                                   *)
  (* ------------------------------------------------------------------------ *)

  let lift_env logic_env =
    Abstract_domain.{ states = (fun label -> fst (logic_env.states label));
                      result = logic_env.result; }

  let evaluate_predicate logic_env (state, _clob) pred =
    let eval_env = Eval_terms.make_env (lift_env logic_env) state in
    match Eval_terms.eval_predicate eval_env pred with
    | Eval_terms.True -> Alarmset.True
    | Eval_terms.False -> Alarmset.False
    | Eval_terms.Unknown -> Alarmset.Unknown

  let reduce_by_predicate logic_env (state, clob) pred b =
    let eval_env = Eval_terms.make_env (lift_env logic_env) state in
    let eval_env = Eval_terms.reduce_by_predicate eval_env b pred in
    let state = Eval_terms.env_current_state eval_env in
    if Cvalue.Model.is_reachable state
    then `Value (state, clob)
    else `Bottom

  let pp_eval_error fmt e =
    if e <> Eval_terms.CAlarm then
      Format.fprintf fmt "@ (%a)" Eval_terms.pretty_logic_evaluation_error e

  let evaluate_from_clause env (_, ins as assign) =
    let open Cil_types in
    match ins with
    | FromAny -> Cvalue.V.top_int
    | From l ->
      try
        (* Evaluates the contents of one element of the from clause, topify them,
           and add them to the current state of the evaluation in acc. *)
        let one_from_contents acc { it_content = t } =
          let loc =
            Eval_terms.(eval_tlval_as_location ~alarm_mode:Ignore env t)
          in
          let state = Eval_terms.env_current_state env in
          let v = Cvalue.Model.find ~conflate_bottom:false state loc in
          Cvalue.V.join acc (Cvalue.V.topify_leaf_origin v)
        in
        let filter x = not (List.mem "indirect" x.it_content.term_name) in
        let direct = List.filter filter l in
        List.fold_left one_from_contents Cvalue.V.top_int direct
      with Eval_terms.LogicEvalError e ->
        Value_util.warning_once_current
          "@[<hov 0>cannot interpret 'from'@ @[<hov 2>clause '%a'@]%a"
          Printer.pp_from assign pp_eval_error e;
        Cvalue.V.top

  let logic_assign logic_assign location ~pre:(pre_state, _) (state, sclob) =
    match logic_assign with
    | Assigns assign ->
      let location = Precise_locs.imprecise_location location in
      let env = Eval_terms.env_assigns pre_state in
      let value = evaluate_from_clause env assign in
      Locals_scoping.remember_if_locals_in_value sclob location value;
      Cvalue.Model.add_binding ~exact:false state location value, sclob
    | Frees _ | Allocates _ -> state, sclob

  (* ------------------------------------------------------------------------ *)
  (*                             Initialization                               *)
  (* ------------------------------------------------------------------------ *)

  let introduce_globals vars (state, clob) =
    let introduce state varinfo =
      let base = Base.of_varinfo varinfo in
      let loc = Locations.loc_of_base base in
      let value = Cvalue.V_Or_Uninitialized.uninitialized in
      Model.add_indeterminate_binding ~exact:true state loc value
    in
    List.fold_left introduce state vars, clob

  let initialize_variable  _lval loc ~initialized init_value (state, clob) =
    let value = match init_value with
      | Abstract_domain.Top  -> Cvalue.V.top_int
      | Abstract_domain.Zero -> Cvalue.V.singleton_zero
    in
    let cvalue =
      if initialized
      then Cvalue.V_Or_Uninitialized.C_init_noesc value
      else Cvalue.V_Or_Uninitialized.C_uninit_noesc value
    in
    let loc = Precise_locs.imprecise_location loc in
    Model.add_indeterminate_binding ~exact:true state loc cvalue, clob

  let empty () =
    let open Cvalue in
    let state = Model.empty_map in
    let min_valid = Base.min_valid_absolute_address () in
    let max_valid = Base.max_valid_absolute_address () in
    if Integer.le min_valid max_valid
    then begin
      (* Bind everything between [0..max] to bottom. Offsetmaps cannot
         contain holes, which can happen when min > 0 holds. *)
      let bot =
        V_Offsetmap.create_isotropic
          ~size:max_valid (V_Or_Uninitialized.initialized V.bottom)
      in
      let v = if true (* TODO: command line option *)
        then V_Or_Uninitialized.initialized V.top_int
        else V_Or_Uninitialized.uninitialized
      in
      let offsm =
        V_Offsetmap.add
          (min_valid, max_valid) (v, Integer.one, Abstract_interp.Rel.zero) bot
      in
      Cvalue.Model.add_base Base.null offsm state, Locals_scoping.bottom ()
    end
    else state, Locals_scoping.bottom ()

  let initialize_variable_using_type kind varinfo (state, clob) =
    match kind with
    | Abstract_domain.Main_Formal
    | Abstract_domain.Library_Global ->
      Cvalue_init.initialize_var_using_type varinfo state, clob
    | Abstract_domain.Spec_Return kf ->
      let value = Library_functions.returned_value kf in
      let loc = Locations.loc_of_varinfo varinfo in
      Model.add_binding ~exact:true state loc value, clob

  (* ------------------------------------------------------------------------ *)
  (*                                  Misc                                    *)
  (* ------------------------------------------------------------------------ *)

  let enter_scope _kf vars (state, clob) =
    let bind_local state vi =
      let b = Base.of_varinfo vi in
      let offsm =
        if Value_parameters.InitializedLocals.get () then
          let v = Cvalue.(V_Or_Uninitialized.initialized V.top_int) in
          match Cvalue.V_Offsetmap.size_from_validity (Base.validity b) with
          | `Bottom -> assert false
          | `Value size -> Cvalue.V_Offsetmap.create_isotropic ~size v
        else
          Bottom.non_bottom (Cvalue.Default_offsetmap.default_offsetmap b)
      in
      Model.add_base b offsm state
    in
    List.fold_left bind_local state vars, clob

  let leave_scope kf vars (state, clob) =
    let state = Model.remove_variables vars state in
    try
      let fdec = Kernel_function.get_definition kf in
      Locals_scoping.make_escaping_fundec fdec clob vars state, clob
    with Kernel_function.No_Definition -> state, clob

  let enter_loop _stmt (s, clob) = s, clob

  let leave_loop _stmt (s, clob) = s, clob

  let incr_loop_counter _stmt (s, clob) = s, clob


  (* ------------------------------------------------------------------------ *)
  (*                                Storage                                   *)
  (* ------------------------------------------------------------------------ *)

  module Store = struct
    module Storage =
      State_builder.Ref (Datatype.Bool)
        (struct
          let dependencies = [Db.Value.self]
          let name = name ^ ".Storage"
          let default () = false
        end)

    let register_global_state _ = Storage.set true
    let register_initial_state callstack (state, _clob) =
      Db.Value.merge_initial_state callstack state
    let register_state_before_stmt callstack stmt (state, _clob) =
      Db.Value.update_callstack_table ~after:false stmt callstack state
    let register_state_after_stmt callstack stmt (state, _clob) =
      Db.Value.update_callstack_table ~after:true stmt callstack state

    let return state =
      if Cvalue.Model.(equal state bottom)
      then `Bottom
      else `Value (state, Locals_scoping.top ())

    let lift_tbl tbl =
      let open Value_types in
      let h = Callstack.Hashtbl.create 7 in
      let process callstack state =
        Callstack.Hashtbl.replace h callstack (state, Locals_scoping.top ())
      in
      Callstack.Hashtbl.iter process tbl;
      h

    let get_global_state () = return (Db.Value.globals_state ())
    let get_initial_state kf = return (Db.Value.get_initial_state kf)
    let get_initial_state_by_callstack kf =
      if Storage.get ()
      then
        match Db.Value.get_initial_state_callstack kf with
        | Some tbl -> `Value (lift_tbl tbl)
        | None -> `Bottom
      else `Top

    let get_stmt_state stmt = return (Db.Value.get_stmt_state stmt)
    let get_stmt_state_by_callstack ~after stmt =
      if Storage.get ()
      then
        match Db.Value.get_stmt_state_callstack ~after stmt with
        | Some tbl -> `Value (lift_tbl tbl)
        | None -> `Bottom
      else `Top

  end

  let display ?fmt kf =
    let open Cil_types in
    (* Do not pretty Cil-generated variables or out-of-scope local variables *)
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
        let outs = Locations.Zone.filter_base filter_generated_and_locals outs in
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
               | Locations.Zone.Top (Base.SetLattice.Top, _) ->
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
    Value_parameters.result "====== VALUES COMPUTED ======";
    Callgraph.Uses.iter_in_rev_order display;
    Value_parameters.result "%t" Value_perf.display

  let post_analysis _state =
    if Value_parameters.ForceValues.get () && Value_parameters.verbose_atleast 1
    then Value_parameters.ForceValues.output display_results
end

let () = Db.Value.display := (fun fmt kf -> State.display ~fmt kf)

let inject cvalue_model = cvalue_model, Locals_scoping.bottom ()
let project (state, _) = state


type prefix = Hptmap.prefix
module Subpart = struct
  type t = Model.subtree
  let hash = Model.hash_subtree
  let equal = Model.equal_subtree
end
let distinct_subpart a b =
  if Model.equal a b then None
  else
    try Model.comp_prefixes a b; None
    with Model.Found_prefix (p, s1, s2) -> Some (p, s1, s2)
let find_subpart s prefix = Model.find_prefix s prefix

(*
Local Variables:
compile-command: "make -C ../../../../.."
End:
*)
