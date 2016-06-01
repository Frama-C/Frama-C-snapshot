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
open Offsm_value

let store_redundant = false
(** If [true], the offsetmap domain stores information that can probably be
    re-synthesized from the value domain. Otherwise, we try to avoid such
    redundancies. Setting this variable to [true] is helpful to find
    unsoundnesses in the domain through testing, because many more expressions
    end up being handled. *)

let dkey = Value_parameters.register_category "d-offsm"

module Default_offsetmap = struct
  open Cvalue

  let is_top m = V_Offsetmap.is_same_value m V_Or_Uninitialized.top

  let default_offsetmap b =
    match b with
    | Base.String _ ->
      Cvalue.Default_offsetmap.default_offsetmap b
    | Base.Var _ | Base.CLogic_Var _ | Base.Null | Base.Allocated _ ->
      let validity = Base.validity b in
      match V_Offsetmap.size_from_validity validity with
      | `Bottom -> `Bottom
      | `Value size ->
         `Map (V_Offsetmap.create_isotropic ~size V_Or_Uninitialized.top)

  let default_contents = `Top

  let name = "Eval_Offsm.Default_offsetmap"
end

(** This domain ignores initialization and danglingness alarms entirely.
    During pretty-printing, we skip them altogether.
    (In fact, it should be possible to prove inductively that everything
    is initialized except Top, because no computation introduces initialized
    bits, and nothing is initially unitialized. *)
module V_Or_Uninitialized = struct
  include Cvalue.V_Or_Uninitialized

  let pretty_typ typ fmt v =
    let v = get_v v in
    if Cvalue.V.is_bottom v then Format.pp_print_string fmt "INDET"
    else pretty_typ typ fmt (initialized v)

  let pretty fmt v = pretty_typ None fmt v
end

module V_Offsetmap = struct
  include Cvalue.V_Offsetmap

  let pretty_generic ?typ ?pretty_v ?skip_v ?sep () fmt t =
    let pretty_v = Extlib.opt_conv V_Or_Uninitialized.pretty_typ pretty_v in
    pretty_generic ?typ ~pretty_v ?skip_v ?sep () fmt t
end

module Memory = struct
  include
    Lmap.Make_LOffset(V_Or_Uninitialized)(V_Offsetmap)(Default_offsetmap)

  let join_and_is_included t1 t2 =
    let t12 = join t1 t2 in (t12, equal t12 t2)

  let widen kf stmt s1 s2 =
    let wh = Widen.getWidenHints kf stmt in
    widen wh s1 s2
end


module D  : Abstract_domain.Internal
  with type state = Memory.t
   and type value = offsm_or_top
   and type location = Precise_locs.precise_location
= struct
  type value = offsm_or_top
  type state = Memory.t
  type location = Precise_locs.precise_location
  include (Memory: sig
             include Datatype.S_with_collections with type t = state
             include Abstract_domain.Lattice with type state := state
           end)

  let structure = Abstract_domain.Void

  let empty _ = Memory.empty_map

  let open_block _fundec _block ~body:_ state =
    state (* default is Top, nothing to do *)
  let close_block _fundec block ~body:_ state =
    Memory.remove_variables block.blocals state (* reverts implicity to Top *)

  type origin = unit (* ???? *)

  type summary = unit
  module Summary = Datatype.Unit

  module Transfer (Valuation:
                     Abstract_domain.Valuation with type value = value
                                                and type origin = origin
                                                and type loc = Precise_locs.precise_location)
    : Abstract_domain.Transfer
      with type state = state
       and type summary = unit
       and type value = offsm_or_top
       and type location = Precise_locs.precise_location
       and type valuation = Valuation.t
  = struct
    type value = offsm_or_top
    type state = Memory.t
    type location = Precise_locs.precise_location
    type summary = unit
    type valuation = Valuation.t

    let update _valuation st = st (* TODO? *)

    let kill loc state =
      snd (Memory.add_binding ~reducing:true ~exact:true
             state loc V_Or_Uninitialized.top)

    let store loc state v =
      let state' =
        match v with
        | Top -> kill loc state
        | O o ->
          if not store_redundant && V_Offsetmap.is_single_interval o then
            kill loc state
          else
            match loc.Locations.size with
            | Int_Base.Top -> assert false
            | Int_Base.Value size ->
              snd (Memory.paste_offsetmap ~reducing:true
                     ~from:o ~dst_loc:loc.Locations.loc ~size ~exact:true state)
      in
      match state' with
      | Memory.Bottom -> `Bottom
      | _ -> `Value state'

    let store_copy state loc v =
      let v = match v with
        | Determinate {v} -> v
        | Exact {v} -> match v with
          | `Value v -> v
          | `Bottom ->
            (* Copy of fully indeterminate bits. We could store an unitialized
               bottom, or something like that. Since this would be redundant
               with the legacy domain, we just drop the value. *)
            Top

      in
      store loc state v

    let assign _kinstr lv _e v _valuation state =
      let loc = Precise_locs.imprecise_location lv.lloc in
      match v with
      | Assign v -> store loc state v
      | Copy (_, vc) -> store_copy state loc vc

    let assume _ _ _ _ state = `Value state

    let summarize kf _stmt ~returned:_ state =
      let fundec = Kernel_function.get_definition kf in
      let written_formals = Value_util.written_formals kf in
      let state = Memory.remove_variables fundec.slocals state in
      let list = Cil_datatype.Varinfo.Set.elements written_formals in
      let state = Memory.remove_variables list state in
      `Value ((), state)

    let resolve_call _stmt _call ~assigned _valuation ~pre:_ ~post =
      let (), post = post in
      match assigned with
      | None -> `Value post
      | Some (lv, v) ->
        let loc = Precise_locs.imprecise_location lv.lloc in
        store_copy post loc v

    let call_action _stmt _call valuation state =
      let state = update valuation state in
      Compute (Continue state, true)

    let default_call _stmt call state =
      let kf = call.kf in
      let returned_value, post_state =
        let top_ret = {
          v = `Value Top; initialized = false; escaping = true;
        } in
        try
          let stmt = Kernel_function.find_return kf in
          match stmt.Cil_types.skind with
          | Cil_types.Return (None, _) -> None, top
          | Cil_types.Return (Some _, _) -> Some top_ret, top
          | _ -> assert false
        with Kernel_function.No_Statement ->
          let name = Kernel_function.get_name kf in
          if  Ast_info.is_frama_c_builtin name then begin
            if Ast_info.is_cea_dump_function name
            then begin
              let l = fst (Cil.CurrentLoc.get ()) in
              Value_parameters.result ~dkey "DUMPING OFFSM STATE \
                                             of file %s line %d@.%a"
                (Filepath.pretty l.Lexing.pos_fname) l.Lexing.pos_lnum
                pretty state;
            end;
            None, state
          end
          else
            let return_type = Kernel_function.get_return_type kf in
            if Cil.isVoidType return_type
            then None, top
            else Some top_ret, top
      in
      let return = { post_state; summary = (); returned_value } in
      `Value [return]

  end

  let compute_using_specification _ _ state =
    let returned_value = None in
    let return = { post_state = state; summary = (); returned_value } in
    `Value [return]

  let extract_expr _oracle _state _exp =
    `Value (Offsm_value.Offsm.top, ()), Alarmset.all

  (* Basic 'find' on a location *)
  let find_loc state loc =
    let size = Int_Base.project loc.Locations.size in
    let _, o = Memory.copy_offsetmap loc.Locations.loc size state in
    match o with
    | `Bottom -> `Bottom
    | `Top -> `Value Offsm_value.Offsm.top
    | `Map o ->
      if Default_offsetmap.is_top o ||
         (not store_redundant && V_Offsetmap.is_single_interval o)
      then
        `Value Offsm_value.Offsm.top
      else
        `Value (O o)

  let cast_lval_if_bitfield typlv o =
    match Eval_typ.bitfield_size typlv with
    | None -> o
    | Some size ->
      let signed = Bit_utils.is_signed_int_enum_pointer typlv in
      let new_size = Integer.of_int (Cil.bitsSizeOf typlv) in
      cast ~old_size:size ~new_size ~signed o

  let extract_lval _oracle state _lv typ locs =
    let o =
      if Cil.typeHasQualifier "volatile" typ ||
         not (Cil.isArithmeticOrPointerType typ)
      then
        `Value (Top, ())
      else
        try
          (* Value on one Locations.loc *)
          let aux loc o =
            let o' = find_loc state loc in
            Bottom.join Offsm_value.Offsm.join o o'
          in
          (* Values on all locations, with the size of the l-value *)
          Precise_locs.fold aux locs `Bottom >>- function
          | Top -> `Value (Top, ())
          | O o ->
            (* Value with the size of the englobing integer type *)
            let o = cast_lval_if_bitfield typ o in
            `Value (O o, ())
        with Int_Base.Error_Top -> `Value (Top, ())
    in
    o, Alarmset.all

  let backward_location _state _lval _typ loc value = `Value (loc, value)

  let reduce_further _state _expr _value = []

  (* Memexec *)
  let filter_by_bases bases state =
    Memory.filter_by_shape (Base.Hptset.shape bases) state
  let reuse ~current_input:state ~previous_output:output =
    let state =
      match output with
      | Memory.Bottom | Memory.Top as state -> state
      | Memory.Map outputs ->
        Memory.fold Memory.add_base outputs state
    in
    state

  (* Initial state *)
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

end
