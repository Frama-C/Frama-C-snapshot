(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2016                                               *)
(*    CEA (Commissariat à l'énergie atomique et aux énergies              *)
(*         alternatives)                                                  *)
(*                                                                        *)
(*  All rights reserved.                                                  *)
(*  Contact CEA LIST for licensing.                                       *)
(*                                                                        *)
(**************************************************************************)

open Cil_types
open Cvalue
open Abstract_interp
open Locations
open Value_util

let register_builtin = Builtins.register_builtin

let dkey = Value_parameters.register_category "imprecision"

exception Found_misaligned_base

let frama_C_is_base_aligned state actuals =
  try begin
    match actuals with
      | [_,x,_; _,y,_] ->
          let i = Cvalue.V.project_ival y in
          begin match i with
            | Ival.Set si ->
              Location_Bytes.fold_i
                (fun b _o () ->
                  Array.iter
                    (fun int ->
                      if not (Base.is_aligned_by b int)
                      then raise Found_misaligned_base)
                    si)
                x
                ();
              { Value_types.c_values =
                  [ Eval_op.wrap_int Cvalue.V.singleton_one, state];
                c_clobbered = Base.SetLattice.bottom;
                c_from = None;
                c_cacheable = Value_types.Cacheable;
              }
            | _ -> raise Found_misaligned_base
          end
      | _ -> raise (Builtins.Invalid_nb_of_args 2)
    end
  with
  | Found_misaligned_base
  | Not_found (* from project_ival *)
  | Location_Bytes.Error_Top (* from fold_i *) ->
      { Value_types.c_values = [Eval_op.wrap_int Cvalue.V.zero_or_one, state];
        c_clobbered = Base.SetLattice.bottom;
        c_from = None;
        c_cacheable = Value_types.Cacheable;
      }

let () = register_builtin "Frama_C_is_base_aligned" frama_C_is_base_aligned


let frama_c_offset state actuals =
  match actuals with
    | [_,x,_] ->
            let value =
              try
                let offsets =
                  Location_Bytes.fold_i
                    (fun _b o a -> Ival.join a o)
                    x
                    Ival.bottom
                in
                Cvalue.V.inject_ival offsets
              with Location_Bytes.Error_Top ->
                Value_parameters.error ~current:true
                  "Builtin Frama_C_offset is applied to a value not \
guaranteed to be an address";
                Cvalue.V.top_int
            in
            { Value_types.c_values = [Eval_op.wrap_size_t value, state];
              c_clobbered = Base.SetLattice.bottom;
              c_from = None;
              c_cacheable = Value_types.Cacheable;
            }
    | _ -> raise (Builtins.Invalid_nb_of_args 1)

let () = register_builtin "Frama_C_offset" frama_c_offset

exception Memcpy_result of (Cvalue.Model.t * Function_Froms.froms * Zone.t)

exception Indeterminate of V_Or_Uninitialized.t 

(*  Called by the [memcpy] builtin. Warns when the offsetmap contains
    an indterminate value, when the imprecision category is enabled *)
let memcpy_check_indeterminate_offsetmap offsm =
  if Value_parameters.is_debug_key_enabled dkey then
    try
      let aux_offset _ (v, _, _) =
        match v with
        | V_Or_Uninitialized.C_init_noesc _ -> ()
        | _ -> raise (Indeterminate v)
      in
      V_Offsetmap.iter aux_offset offsm
    with Indeterminate v ->
      Value_parameters.debug ~current:true ~dkey ~once:true
        "@[In memcpy@ builtin:@ precise@ copy of@ indeterminate@ values %a@]%t"
        V_Or_Uninitialized.pretty v Value_util.pp_callstack

(* Create a dependency [\from arg_n] where n is the nth argument of the
   currently called function. *)
let deps_nth_arg n =
  let open Function_Froms in
  let (kf,_) = List.hd (Value_util.call_stack()) in
  try
    let vi = List.nth (Kernel_function.get_formals kf) n in
    Deps.add_data_dep Deps.bottom (Locations.zone_of_varinfo vi)
  with Failure _ -> Kernel.fatal "%d arguments expected" n

let frama_c_memcpy state actuals =
  let compute (exp_dst,dst,_) (exp_src,src,_) (exp_size,size,_) =
    if Value_parameters.ValShowProgress.get () then
      Value_parameters.feedback ~current:true "Call to builtin memcpy(%a)%t"
        pretty_actuals actuals Value_util.pp_callstack;
    let plevel = Value_parameters.ArrayPrecisionLevel.get() in
    let with_alarms = warn_all_quiet_mode () in
    let size =
      try Cvalue.V.project_ival size
      with Cvalue.V.Not_based_on_null -> Ival.top (* TODO: use size_t *)
    in
    let min,max = Ival.min_and_max size in
    let min = match min with None -> Int.zero | Some m -> Int.max m Int.zero in
    let char_bits = Bit_utils.sizeofchar() in
    let size_min = Int.mul char_bits min in
    let right = loc_bytes_to_loc_bits src in
    let left = loc_bytes_to_loc_bits dst in
    let term_size = Logic_utils.expr_to_term ~cast:true exp_size in
    let array_src = Logic_utils.array_with_range exp_src term_size in
    let array_dst = Logic_utils.array_with_range exp_dst term_size in
    let deps_return = deps_nth_arg 0 in
    let empty_cfrom =
      Function_Froms.({ deps_table = Memory.empty; deps_return })
    in
    let precise_copy state =
      (* First step: copy the bytes we are sure to copy *)
      if Int.gt size_min Int.zero then begin
        Valarms.set_syntactic_context (Valarms.SyMemLogic array_src);
        let loc_left = make_loc left (Int_Base.inject size_min) in
        match Eval_op.copy_offsetmap ~with_alarms right size_min state with
        | `Bottom -> (* Read failed. Source was invalid, but must be read, we
                        stop the analysis *)
          raise (Memcpy_result (Cvalue.Model.bottom,empty_cfrom,Zone.bottom))
        | `Map offsetmap ->
          memcpy_check_indeterminate_offsetmap offsetmap;
          (* Read succeeded. We write the result *)
          Valarms.set_syntactic_context (Valarms.SyMemLogic array_dst);
          let loc_right = make_loc right (Int_Base.inject size_min) in
          let new_state = Eval_op.paste_offsetmap ~with_alarms ~remove_invalid:true
            ~reducing:false ~from:offsetmap ~dst_loc:left ~size:size_min
            ~exact:true state in
          let (deps_table, sure_zone) =
            let zone_left = enumerate_valid_bits ~for_writing:true loc_left in
            let zone_right= enumerate_valid_bits ~for_writing:false loc_right in
            let deps =
              Function_Froms.(Deps.add_data_dep Deps.bottom zone_right)
            in
            (* Note: actually a part may be written for sure (if the
               difference between the offsets in loc_left is smaller
               than size), but keeping it imprecise reflects more the
               imprecision of the value analysis here. *)
            let exact = Location_Bits.cardinal_zero_or_one left in
            let deps_table =
              Function_Froms.Memory.add_binding ~exact
                Function_Froms.Memory.empty zone_left deps in
            let sure_zone = if exact then zone_left else Zone.bottom in
            (deps_table, sure_zone)
          in
          new_state, deps_table, sure_zone
        | `Top -> Warn.warn_top ();
      end
      else (* Nothing certain can be copied *)
        (state, Function_Froms.Memory.empty, Zone.bottom)
    in
    let imprecise_copy new_state precise_deps_table sure_zone =
      (* Second step. Size is imprecise, we will now copy some bits
         that we are not sure to copy *)
      let size_min_ival = Ival.inject_singleton size_min in
      let left = Location_Bits.shift size_min_ival left in
      let right = Location_Bits.shift size_min_ival right in
      (* Size remaining to copy imprecisely *)
      let diff = match max with
        | Some max -> Some (Int.mul char_bits (Int.pred (Int.sub max min)))
        | None -> None
      in
      (* Imprecise locations remaining to be read/written. By using ranges
         modulo char_bits, we read and write byte-by-byte, which can preserve
         some precision in the fallback. If sufficiently few sizes need
         to be copied, we use a more precise method (see do_size below).
         However, in all cases, those locations are used to compute the
         read and written bits. *)
      let range = Ival.inject_top (Some Int.zero) diff Int.zero char_bits in
      let size_char = Int_Base.inject char_bits in      
      let loc_right = make_loc (Location_Bits.shift range right) size_char in
      let loc_left = make_loc (Location_Bits.shift range left) size_char in
      let c_from =
        let open Function_Froms in
        let zone_right = enumerate_valid_bits ~for_writing:false loc_right in
        let zone_left = enumerate_valid_bits ~for_writing:true loc_left in
        let deps = Deps.add_data_dep Deps.bottom zone_right in
        let deps_table =
          Memory.add_binding ~exact:false precise_deps_table zone_left deps
        in
        { deps_table; deps_return }
      in
      try
        (* We try to iter on all the slices inside the value of slice.
           If there are more too many of them, we use a backup solution *)
        ignore (Ival.cardinal_less_than size (plevel / 10));
        let do_size s (left, right, prev_size, state) =
          let s = Int.mul char_bits s in
          let diff = Int.sub s prev_size in
          if Int.equal s size_min then
            (* occurs the very first time. This copy has already been
               performed at the beginning, skip *)
            (left, right, s, state)
          else begin
            (* Copy data between prev_size and s *)
            Valarms.set_syntactic_context (Valarms.SyMemLogic array_src);
            match Eval_op.copy_offsetmap ~with_alarms right diff state with
            | `Bottom ->
              (* This size is completely invalid. The following ones
                 will also be invalid, stop now with current result *)
              raise (Memcpy_result (state,c_from,sure_zone))
            | `Top -> Warn.warn_top ();
            | `Map offsetmap ->
              memcpy_check_indeterminate_offsetmap offsetmap;
              Valarms.set_syntactic_context (Valarms.SyMemLogic array_dst);
              let new_state =
                Eval_op.paste_offsetmap ~with_alarms ~reducing:false
                  ~remove_invalid:true ~from:offsetmap ~dst_loc:left
                  ~size:diff ~exact:false state
              in
              if Db.Value.is_reachable new_state then
                let diffi = Ival.inject_singleton diff in
                let left = Location_Bits.shift diffi left in
                let right = Location_Bits.shift diffi right in
                (left, right, s, new_state)
              else (* As above, invalid size, this time for the destination.
                      We stop there *)
                raise (Memcpy_result (state,c_from,sure_zone))
          end
        in
        let _, _, _, state =
          Ival.fold_int do_size size (left, right, Int.zero, new_state)
        in
        raise (Memcpy_result (state,c_from,sure_zone))
      with
      | Abstract_interp.Not_less_than ->
        Value_parameters.debug ~dkey ~once:true
          ~current:true "In memcpy builtin: too many sizes to enumerate, \
                            possible loss of precision";
        (* Too many slices in the size. We read the entire range
           src+(size_min..size_max-1) in one step, as one byte, and write the
           result as one byte in dst+(size_min..size_max-1) *)
        let alarm, v = (* conflate_bottom=false: we want to copy padding bits *)
          Model.find_unspecified ~conflate_bottom:false state loc_right
        in
        if alarm then begin
          Valarms.set_syntactic_context (Valarms.SyMemLogic array_src);
          Valarms.warn_mem_read with_alarms;
        end;
        begin match v with
        | V_Or_Uninitialized.C_init_noesc _ -> ()
        | _ -> Value_parameters.result ~dkey ~current:true ~once:true
          "@[In memcpy@ builtin:@ imprecise@ copy of@ indeterminate@ values@]%t"
          Value_util.pp_callstack
        end;
        Valarms.set_syntactic_context (Valarms.SyMemLogic array_dst);
        let updated_state = Eval_op.add_binding_unspecified ~with_alarms
          ~remove_invalid:true ~exact:false new_state loc_left v
        in
        (* Beware that all the imprecise sizes may be invalid, in which case
           [add_binding] will return [Bottom]. In this case, return the
           previously computed state *)
        if Model.is_reachable updated_state then
          raise (Memcpy_result (updated_state,c_from,sure_zone))
        else
          raise (Memcpy_result (new_state,c_from,sure_zone))
    in
    try
      if Ival.is_zero size then
        raise (Memcpy_result (state, empty_cfrom, Zone.bottom));
      let (precise_state,precise_deps_table,sure_zone) = precise_copy state in
      if Extlib.may_map ~dft:false (Int.equal min) max then
        (let open Function_Froms in
        let c_from = { deps_table = precise_deps_table; deps_return } in
        raise (Memcpy_result (precise_state, c_from, sure_zone)));
      imprecise_copy precise_state precise_deps_table sure_zone
    with
    | Memcpy_result (new_state,c_from,sure_zone) ->
      if Model.is_reachable new_state then
        (* Copy at least partially succeeded (with perhaps an
           alarm for some of the sizes *)
        { Value_types.c_values = [Eval_op.wrap_ptr dst, new_state];
          c_clobbered = Builtins.clobbered_set_from_ret new_state dst;
          c_from = Some(c_from,  sure_zone);
          c_cacheable = Value_types.Cacheable }
      else
        { Value_types.c_values = [ None, Cvalue.Model.bottom];
          c_clobbered = Base.SetLattice.bottom;
          c_from = Some(c_from,  sure_zone);
          c_cacheable = Value_types.Cacheable }
  in
  match actuals with
  | [dst; src; size] -> compute dst src size
  | _ -> raise (Builtins.Invalid_nb_of_args 3)

let () = register_builtin "Frama_C_memcpy" frama_c_memcpy

(* Reads the [size] bytes between [block] and [block+size-1], and copy
   then [length] times, between [block+size] and [block+size*(n+1)-1]
   This builtin is incorrect if the size is imprecise (relies on the
   declared type of the actual block parameter. This captures the usage that
   is done in generated initialization blocks. *)
(* TODO: add Offsetmap.paste_repeated_slices to Offsetmap, and use it
   there *)
let frama_c_copy_block state actuals =
  if Value_parameters.ValShowProgress.get () then
    Value_parameters.feedback ~current:true "Call to builtin copy_block(%a)%t"
      pretty_actuals actuals Value_util.pp_callstack;
  match actuals with
    | [(exp_block, block, _); (exp_size, size,_); (exp_length, length,_)] ->
        let bt = Cil.typeOf_pointed (Cil.typeOf exp_block) in
        let cell_size =
          try Int.of_int (Cil.bytesSizeOf bt)
          with Cil.SizeOfError _ -> raise Db.Value.Outside_builtin_possibilities
        in
        let with_alarms = warn_all_quiet_mode () in
        let char_bits = Bit_utils.sizeofchar() in
        let size = Cvalue.V.project_ival size in
        let size =
          try
            let min,max = Ival.min_and_max size in
            let min = match min with
              | None -> cell_size
              | Some m -> Int.mul char_bits (Int.max m Int.zero)
            in
            let max = match max with
              | None -> cell_size
              | Some m -> Int.mul char_bits (Int.max m Int.zero)
            in
            (* if we are precise, use the given value. Otherwise, stick with
               the computed one.
            *)
            if Int.equal min max then min else cell_size
          with V.Not_based_on_null -> cell_size
        in
        (* for length, we can have some imprecision. *)
        let length_min, length_max =
          let length = Cvalue.V.project_ival length in
          try
            let min,max = Ival.min_and_max length in
            let min = match min with
              | None -> Int.zero
              | Some m -> Int.max m Int.zero
            in
            let max = match max with
              | None -> Int.div (Bit_utils.max_bit_address ()) size
              | Some m -> Int.max m Int.zero
            in
            min,max
          with V.Not_based_on_null ->
            Int.zero, Int.div (Bit_utils.max_bit_address ()) size
        in
        let term_size = Logic_utils.expr_to_term ~cast:true exp_size in
        let term_length = Logic_utils.expr_to_term ~cast:true exp_length in
        let char_length =
          Logic_const.term ~loc:Cil_datatype.Location.unknown
            (TBinOp(Mult,term_size,term_length)) Linteger
        in
        let array = Logic_utils.array_with_range exp_block char_length in
        Valarms.set_syntactic_context (Valarms.SyMemLogic array);
        let start = loc_bytes_to_loc_bits block in
        (match Eval_op.copy_offsetmap ~with_alarms start size state with
          | `Bottom ->
              (* cannot read the first cell to get init value *)
              { Value_types.c_values = [ None, Cvalue.Model.bottom];
                c_clobbered =  Base.SetLattice.bottom;
                c_from = None;
                c_cacheable = Value_types.Cacheable;
              }
          | `Top -> Warn.warn_top ();
          | `Map offsetmap ->
              let isize = Ival.inject_singleton size in
              (* Write this offsetmap at the other cells of the block. *)
              let rec write_one_cell sure_write bound state cell i =
                if Int.gt bound i then begin
                  let state =
                    Eval_op.paste_offsetmap ~with_alarms ~remove_invalid:true
                      ~reducing:false ~from:offsetmap ~dst_loc:cell ~size
                      ~exact:sure_write state
                  in
                  let cell = Location_Bits.shift isize cell in
                  write_one_cell
                    sure_write bound state cell (Int.add i Int.one)
                end else state
              in
              let cell = Location_Bits.shift isize start in
              let is_sure_write =
                try
                  ignore (Location_Bits.find_lonely_key cell); true
                with Not_found -> false
              in
              let state =
                write_one_cell is_sure_write length_min state cell Int.one
              in
              let state =
                if Int.gt length_max length_min then begin
                  let cell = Location_Bits.shift
                    (* cell is already at index 1. Shift appropriately. *)
                    (Ival.inject_singleton
                       (Int.mul size (Int.sub length_min Int.one)))
                    cell
                  in
                  write_one_cell false length_max state cell length_min
                end else state
              in
              { Value_types.c_values = [None, state];
                c_clobbered = Builtins.clobbered_set_from_ret state block;
                c_from = None;
                c_cacheable = Value_types.Cacheable;
              }
        )
    | _ -> raise (Builtins.Invalid_nb_of_args 3)

let () = register_builtin "Frama_C_copy_block" frama_c_copy_block

(*  Implementation of [memset] that accepts imprecise arguments. Assumes
    the syntactic context is positioned. *)
let frama_c_memset_imprecise state dst v size =
  let size_char = Bit_utils.sizeofchar () in
  let with_alarms = warn_all_quiet_mode () in
  let size_min, size_max_bytes =
    try
      let size = Cvalue.V.project_ival size in
      let min,max = Ival.min_and_max size in
      let min = match min with
        | None -> Int.zero
        | Some m -> Int.mul size_char (Int.max m Int.zero)
      and max = match max with
        | None -> Bit_utils.max_bit_address ()
        | Some m -> m
      in min, max
    with V.Not_based_on_null -> Int.zero, Bit_utils.max_bit_address ()
  in
  let left = loc_bytes_to_loc_bits dst in
  (* Write [v] everywhere that might be written, ie between
     [dst] and [dst+size-1]. *)
  let (new_state,over_zone) =
    if Int.gt size_max_bytes Int.zero then
      let shift =
        Ival.inject_range (Some Int.zero) (Some (Int.pred size_max_bytes))
      in
      let loc = Location_Bytes.shift shift dst in
      let loc = loc_bytes_to_loc_bits loc in
      let loc = make_loc loc (Int_Base.inject size_char) in
      let state = 
        Eval_op.add_binding ~with_alarms
          ~remove_invalid:true ~exact:false state loc v in
      (state,enumerate_valid_bits ~for_writing:true loc)
    else (state,Zone.bottom)
  in
  (* Write "sure" bytes in an exact way: they exist only if there is only
     one base, and within it, size_min+leftmost_loc > rightmost_loc *)
  let (new_state',sure_zone) =
    try
      let base, offset = Location_Bits.find_lonely_key left in
      let minb, maxb = match Ival.min_and_max offset with
        | Some minb, Some maxb -> minb, maxb
        | _ -> raise Not_found
      in
      let sure = Int.sub (Int.add minb size_min) maxb in
      if Int.gt sure Int.zero then
        let left' = Location_Bits.inject base (Ival.inject_singleton maxb) in
        let vuninit = V_Or_Uninitialized.initialized v in
        let from = V_Offsetmap.create ~size:sure vuninit ~size_v:size_char in
        let state = 
          Eval_op.paste_offsetmap ~with_alarms ~remove_invalid:true
            ~reducing:false ~from ~dst_loc:left' ~size:sure ~exact:true new_state in
        let sure_loc = make_loc left' (Int_Base.inject sure) in
        let sure_zone = enumerate_valid_bits ~for_writing:true sure_loc in
        (state,sure_zone)
      else
        (new_state,Zone.bottom)
    with Not_found -> (new_state,Zone.bottom) (* from find_lonely_key + explicit raise *)
  in
  let c_from =
    let open Function_Froms in
    let value_dep = deps_nth_arg 1 in
    let deps_table =
      Memory.add_binding ~exact:false Memory.empty over_zone value_dep
    in
    let deps_table =
      Memory.add_binding ~exact:true deps_table sure_zone value_dep
    in
    let deps_return = deps_nth_arg 0 in
    { deps_table; deps_return }
  in
  { Value_types.c_values = [Eval_op.wrap_ptr dst, new_state'];
    c_clobbered = Base.SetLattice.bottom;
    c_from = Some(c_from,sure_zone);
    c_cacheable = Value_types.Cacheable;
  }
(* let () = register_builtin "Frama_C_memset" frama_c_memset_imprecise *)

(* Type that describes why the 'precise memset' builtin may fail. *)
type imprecise_memset_reason =
| UnsupportedType
| ImpreciseTypeSize
| NoTypeForDest
| NotSingletonLoc
| SizeMismatch
| ImpreciseValue
| ImpreciseSize
| NegativeOrNullSize (* The zero case is licit, but it is simpler to handle
                        through the imprecise builtin. See bts #1799 *)

exception ImpreciseMemset of imprecise_memset_reason

let pretty_imprecise_memset_reason fmt = function
  | UnsupportedType ->
    Format.pp_print_string fmt "destination has an unknown type"
  | ImpreciseTypeSize ->
    Format.pp_print_string fmt "destination has a type with unknown size"
  | NoTypeForDest ->
    Format.pp_print_string fmt "destination has an unknown form"
  | NotSingletonLoc ->
    Format.pp_print_string fmt "destination is not exact"
  | SizeMismatch ->
    Format.pp_print_string fmt "destination type and size differ"
  | ImpreciseValue ->
    Format.pp_print_string fmt "value to write is imprecise"
  | ImpreciseSize ->
    Format.pp_print_string fmt "size is imprecise"
  | NegativeOrNullSize ->
    Format.pp_print_string fmt "size is negative or null"


(*  [memset_typ_offsm typ i] returns an offsetmap of size [sizeof(typ)]
    that maps each byte to the integer [i]. The shape of the type is
    respected: the fields in [typ] are bound to values of the good type,
    not just to 'i%repeated modulo 8'. May raise ImpreciseMemset. *)
let memset_typ_offsm_int full_typ i =
  try
    let size = Int.of_int (Cil.bitsSizeOf full_typ) in
    let vi = V_Or_Uninitialized.initialized (Cvalue.V.inject_int i) in
    let size_char = Bit_utils.sizeofchar () in
    let full_offsm = V_Offsetmap.create ~size vi ~size_v:size_char in
    if Int.is_zero i then
      full_offsm (* Shortcut: no need to follow the type, this offsetmap is
                    optimally precise *)
    else
      let validity = Base.validity_from_size size in
      (* no access error to signal here, given the validity we use. However,
         we want to be notified of float conversion errors. *)
      let alarms_ok = ref true in
      let with_alarms =
        let not_ok () = alarms_ok := false in {
          CilE.others = {CilE.a_ignore with CilE.a_call=not_ok};
          unspecified = {CilE.a_ignore with CilE.a_call=not_ok};
          defined_logic = {CilE.a_ignore with CilE.a_call=not_ok};
          imprecision_tracing = CilE.a_ignore;
        }
      in
      let rec aux styp offset offsm =
        (* Read [full_offsm] between [offset] and [offset+size-1], and return
           the value stored there. *)
        let find size =
          snd (V_Offsetmap.find ~validity
                 ~offsets:(Ival.inject_singleton offset) ~size full_offsm)
        in
        (* Update [full_offsm] between [offset] and [offset+size-1], and store
           exactly [v] there *)
        let update size v =
          let bounds = (offset, Int.(pred (add offset size))) in
          let vinit = V_Or_Uninitialized.initialized v in
          V_Offsetmap.add bounds (vinit, size, Rel.zero) offsm
        in
        match Cil.unrollType styp with
        | TInt _ | TEnum _ | TPtr _ ->
          let size = Eval_typ.sizeof_lval_typ styp (* handles bitfields *) in
          let size = Int_Base.project size in
          let v = V_Or_Uninitialized.get_v (find size) in
          let signed = Bit_utils.is_signed_int_enum_pointer styp in
          let v, _ok = Cvalue.V.cast ~size ~signed v in
          update size v
        | TFloat (fkind, _) ->
          let size = Int.of_int (Cil.bitsSizeOf styp) in
          let v = V_Or_Uninitialized.get_v (find size) in
          (* Use reinterpret_float to get a floating point-value when possible:
             the transfer functions in Ival do not like mismatches
             integer/float. BUT catch errors (is_finite) during the conversion,
             because we prefer having a precise int value instead of a
             bottom/imprecise float .*)
          alarms_ok := true;
          let v' = Eval_op.reinterpret_float ~with_alarms fkind v in
          if !alarms_ok then update size v' else update size v
        | TComp ({ cstruct = true ; cfields = l}, _, _) as tcomp -> (* struct *)
          let aux_field offsm fi =
            let field = Field (fi, NoOffset) in
            let offset_fi = Int.of_int (fst (Cil.bitsOffset tcomp field)) in
            aux fi.ftype (Int.add offset offset_fi) offsm
          in
          List.fold_left aux_field offsm l
        | TComp ({ cstruct = false ; cfields = l}, _, _) -> (* union *)
          (* Use only the first field. This is somewhat arbitrary *)
          aux (List.hd l).ftype offset offsm
        | TArray (typelt, nb, _, _) -> begin
          let nb = Cil.lenOfArray64 nb in (* always succeeds, we computed the
                                             size of the entire type earlier *)
          if Integer.(gt nb zero) then begin
            let sizeelt = Int.of_int (Cil.bitsSizeOf typelt) in
            (* Do the first cell *)
            let offsm' = aux typelt offset offsm in
            if Integer.(gt nb one) then begin
              (* Copy the result *)
              let src = Ival.inject_singleton offset in
              let _alarm_access, copy =
                V_Offsetmap.copy_slice
                  ~validity ~offsets:src ~size:sizeelt offsm'
              in
              (* Paste on all offsets > 1 *)
              let dst =
                let idx =
                  Ival.inject_range (Some Int.one) (Some (Int.pred nb))
                in
                let idx_size = Ival.scale sizeelt idx in
                Ival.add_singleton_int offset idx_size
              in
              match copy with
              | `Bottom -> assert false (* the copy is within bounds *)
              | `Map copy ->
                let _alarm_access, r =
                  V_Offsetmap.paste_slice ~validity
                    ~exact:true ~from:copy ~size:sizeelt ~offsets:dst offsm'
                in
                match r with
                | `Bottom -> assert false (* so is the write *)
                | `Map r -> r
            end
            else offsm' (* size = 1 *)
          end
          else offsm (* size = 0. Do nothing, this is supposed to be invalid
                        anyway *)
        end
        | TVoid _ | TFun _ | TBuiltin_va_list _ ->
          raise (ImpreciseMemset UnsupportedType)
        | TNamed _ -> assert false (* unrolled *)
      in
      aux full_typ Int.zero full_offsm
  with Cil.SizeOfError _ | Int_Base.Error_Top ->
    raise (ImpreciseMemset ImpreciseTypeSize)

(*  Type-aware memset on an entire type. Same as [memset_typ_offsm_int], but
    with a [Cvalue.V] instead of an integer. We accept [-ilevel] different
    possible values in [v] before falling back to the imprecise memset.
    May rais {!ImpreciseMemset}.  *)
let memset_typ_offsm typ v =
  try
    let i = V.project_ival v in
    ignore (Ival.cardinal_less_than i (Ival.get_small_cardinal ()));
    let aux_i i offsm =
      let offsm_i = memset_typ_offsm_int typ i in
      match offsm with
      | None -> Some offsm_i
      | Some o -> Some (Cvalue.V_Offsetmap.join o offsm_i)
    in begin
      match Ival.fold_int aux_i i None with
      | None -> (* v == Ival.bottom *)
        raise (ImpreciseMemset ImpreciseValue)
      | Some o -> o
    end
  with V.Not_based_on_null | Not_less_than ->
    raise (ImpreciseMemset ImpreciseValue)

(*  Precise memset builtin, that requires its arguments to be sufficiently
    precise abstract values. Assumes the syntactic context is positioned. *)
let frama_c_memset_precise state dst v (exp_size, size) =
  try
    let size_char = Bit_utils.sizeofchar () in
    (* We want an exact size, Otherwise, we can use the imprecise memset as a
       fallback *)
    let isize = V.project_ival size in
    let size = Ival.project_int isize in
    let size_bits = Integer.mul size_char size in
    (* Extract the location, check that it is precise. *)
    if not (Location_Bytes.cardinal_zero_or_one dst) then
      raise (ImpreciseMemset NotSingletonLoc);
    if not (Int.gt size Int.zero) then
      raise (ImpreciseMemset NegativeOrNullSize);
    (* Now, try to find a type that matches [size]. *)
    let typ =
      (* If [exp_size] is a sizeof, use this type. *)
      let rec find_sizeof e = match e.enode with
        | SizeOf typ -> Some typ
        | SizeOfE e -> Some (Cil.typeOf e)
        | CastE (_, e) -> find_sizeof e
        | _ -> None
      in
      match find_sizeof exp_size with
      | Some typ -> typ
      | None ->
        (* No such luck. Use the base and the offset of [dst] to resynthesize
           a type *)
        let base_dst, offset_dst = Location_Bytes.find_lonely_binding dst in
        let offset_dst = Ival.project_int offset_dst in
        let offset_dst_bits = Int.mul offset_dst size_char in
        let vi_dst = Base.to_varinfo base_dst in
        let mo = Bit_utils.MatchSize size_bits in
        snd (Bit_utils.(find_offset vi_dst.vtype offset_dst_bits mo))
    in
    let offsm = memset_typ_offsm typ v in
    let dst_loc = Locations.loc_bytes_to_loc_bits dst in
    let (c_from,dst_zone) =
      let input = deps_nth_arg 1 in
      let open Function_Froms in      
      let size_bits = Integer.mul size (Bit_utils.sizeofchar ())in
      let dst_location = Locations.make_loc dst_loc (Int_Base.Value size_bits) in
      let dst_zone = Locations.enumerate_valid_bits
        ~for_writing:true dst_location in
      let deps_table =
        Function_Froms.Memory.add_binding ~exact:true
          Function_Froms.Memory.empty dst_zone input in
      let deps_return = deps_nth_arg 0 in
      let c_from = { deps_table; deps_return  } in
      c_from,dst_zone
    in
    let _ = c_from in
    let with_alarms = warn_all_quiet_mode () in
    let state' =
      Eval_op.paste_offsetmap ~with_alarms ~remove_invalid:true
        ~reducing:false ~from:offsm ~dst_loc ~size:size_bits ~exact:true state
    in
    { Value_types.c_values = [Eval_op.wrap_ptr dst, state'];
      c_clobbered = Base.SetLattice.bottom;
      c_from = Some (c_from,dst_zone);
      c_cacheable = Value_types.Cacheable;
    }
  with
  | Bit_utils.NoMatchingOffset -> raise (ImpreciseMemset SizeMismatch)
  | Base.Not_a_C_variable -> raise (ImpreciseMemset NoTypeForDest)
  | Cil.SizeOfError _ -> raise (ImpreciseMemset ImpreciseTypeSize)
  | Ival.Not_Singleton_Int | V.Not_based_on_null ->
    raise (ImpreciseMemset ImpreciseSize)

(* let () = register_builtin "Frama_C_memset_precise" frama_c_memset_precise *)

let frama_c_memset state actuals =
  if Value_parameters.ValShowProgress.get () then
    Value_parameters.feedback ~current:true "Call to builtin memset(%a)%t"
      pretty_actuals actuals Value_util.pp_callstack;
  match actuals with
    | [(exp_dst, dst, _); (_exp_v, v, _); (exp_size, size, _)] ->
      begin
        (* Position syntactic context *)
        let term_size = Logic_utils.expr_to_term ~cast:true exp_size in
        let array_dst = Logic_utils.array_with_range exp_dst term_size in
        Valarms.set_syntactic_context (Valarms.SyMemLogic array_dst);
        (* Keep only the first byte of the argument *)
        let _, v = Cvalue.V.extract_bits
          ~topify:Origin.K_Misalign_read
          ~start:Int.zero ~stop:(Int.pred (Bit_utils.sizeofchar ()))
          ~size:(Int.of_int (Cil.bitsSizeOfInt IInt))
          v
        in
        try
          frama_c_memset_precise state dst v (exp_size, size)
        with ImpreciseMemset reason ->
          Value_parameters.debug ~dkey ~current:true
            "Call to builtin precise_memset(%a) failed; %a%t"
            pretty_actuals actuals pretty_imprecise_memset_reason reason
            Value_util.pp_callstack;
          frama_c_memset_imprecise state dst v size
      end
    | _ -> raise (Builtins.Invalid_nb_of_args 3)

let () = register_builtin "Frama_C_memset" frama_c_memset

let frama_c_interval_split state actuals =
  try
    begin match actuals with
     | [_,lower,_; _,upper,_] ->
         let upper = Ival.project_int (Cvalue.V.project_ival upper) in
         let lower = Ival.project_int (Cvalue.V.project_ival lower) in
         let i = ref lower in
         let r = ref [] in
         while (Int.le !i upper) do
           r := (Eval_op.wrap_int (Cvalue.V.inject_int !i), state) :: !r;
           i := Int.succ !i;
         done;
         { Value_types.c_values = !r;
           c_clobbered = Base.SetLattice.bottom;
           c_from = None;
           c_cacheable = Value_types.Cacheable;
         }
     | _ -> raise (Builtins.Invalid_nb_of_args 2)
    end
  with
  | Cvalue.V.Not_based_on_null
  | Ival.Not_Singleton_Int ->
      Value_parameters.error
        "Invalid call to Frama_C_interval_split%a" pretty_actuals actuals;
      raise Db.Value.Aborted

let () = register_builtin "Frama_C_interval_split" frama_c_interval_split

let rec topify_pointed_arguments state args =
  match args with
  | [] -> state
  | (exp_arg,value_arg,_) :: tail ->
    let typ = Cil.typeOf_pointed (Cil.typeOf exp_arg) in
    let loc_bits = loc_bytes_to_loc_bits value_arg in
    let loc = make_loc loc_bits (Int_Base.inject (Integer.of_int (Cil.bitsSizeOf typ))) in
    let state =
      Eval_op.add_binding ~with_alarms:(warn_all_quiet_mode ())
        ~exact:false state loc V.top_int
    in
    topify_pointed_arguments state tail

let frama_c_fscanf state actuals =
  match actuals with
  | (_,_file,_) :: (_,_format,_) :: tail ->
    let state = topify_pointed_arguments state tail in
    { Value_types.c_values = [ Eval_op.wrap_int Cvalue.V.top_int, state ];
      c_clobbered = Base.SetLattice.bottom;
      c_from = None;
      c_cacheable = Value_types.Cacheable;
    }
  | _ -> raise (Builtins.Invalid_nb_of_args 2)
  
let () = register_builtin "fscanf" frama_c_fscanf

(* Transforms a garbled mix into Top_int. Let other values unchanged.
   Remark: this currently returns an int. Maybe we need multiple versions? *)
let frama_c_ungarble state actuals =
  begin match actuals with
    | [_,i,_] ->
      let v =
        try
          ignore (V.project_ival i);
          i
        with V.Not_based_on_null ->
          V.inject_ival Ival.top
      in
      { Value_types.c_values = [ Eval_op.wrap_int v, state ];
        c_clobbered = Base.SetLattice.bottom;
        c_from = None;
        c_cacheable = Value_types.Cacheable;
      }
    | _ -> raise (Builtins.Invalid_nb_of_args 1)
  end

let () = register_builtin "Frama_C_ungarble" frama_c_ungarble


(* -------------------------------------------------------------------------- *)
(* --- Variadic calls                                                     --- *)
(* -------------------------------------------------------------------------- *)

(* Tentative code for variadic builtins. Equivalent code is currently in
   eval_slevel, just where a function call is treated. The code below does
   not work: transforming "va_start(foo, bar)" into "__builtin_va_start(foo)"
   cannot work, as "__builtin_va_start" needs to initialize foo... *)

(* Builtin-in that does nothing for simple "va_foo" macros. We just put
   something random into the argument of type va_list *)
let _frama_c_va_nothing builtin state actuals =
  if Value_parameters.ValShowProgress.get () then
    Value_parameters.feedback "Call to builtin %s(%a)%t"
      builtin pretty_actuals actuals Value_util.pp_callstack;
  match actuals with
    | [ {enode = Lval lv}, v, _] ->
        let size = Bit_utils.sizeof (TBuiltin_va_list []) in
        let loc = make_loc (loc_bytes_to_loc_bits v) size in
        Valarms.set_syntactic_context (Valarms.SyMem lv);
        let state = Eval_op.add_binding ~with_alarms:(warn_all_quiet_mode ())
          ~exact:true state loc V.top_int
        in
        { Value_types.c_values = [ None, state ] ;
          c_clobbered = Base.SetLattice.bottom;
          c_from = None;
          c_cacheable = Value_types.Cacheable;
        }

    | _ -> raise (Builtins.Invalid_nb_of_args 1)

(*
let () = register_builtin "__builtin_va_start" (frama_c_va_nothing "va_start")
let () = register_builtin "__builtin_va_end" (frama_c_va_nothing "va_end")
*)

(* Builtin for va_arg. Initializes its last argument, but does not map
   it the a real variadic value yet. (Variadic arguments of the caller are
   not visible at all here) *)
let _frama_c_va_arg state actuals =
  if Value_parameters.ValShowProgress.get () then
    Value_parameters.feedback "Call to builtin va_arg(%a)%t"
      pretty_actuals actuals Value_util.pp_callstack;
  match actuals with
      [_; ({enode = SizeOf _typ}, vsize, _); (edst, vdst, _)] ->
        let size =
          try
            let i = V.project_ival vsize in
            let i = Ival.project_int i in
            let ibytes = Integer.mul i (Bit_utils.sizeofchar ()) in
            Int_Base.inject ibytes
          with V.Not_based_on_null | Ival.Not_Singleton_Int ->
            Int_Base.top
        in
        let loc = make_loc (loc_bytes_to_loc_bits vdst) size in
        (* TODO: missing cast to the proper type. *)
        Valarms.set_syntactic_context (Valarms.SyMem (Mem edst, NoOffset));
        let state = Eval_op.add_binding ~with_alarms:(warn_all_quiet_mode ())
          ~exact:true state loc V.top_int
        in
        { Value_types.c_values = [ None, state ] ;
          c_clobbered = Base.SetLattice.bottom;
          c_from = None;
          c_cacheable = Value_types.Cacheable;
        }

    | _ -> raise (Builtins.Invalid_nb_of_args 2)

(* let () = register_builtin "__builtin_va_arg" frama_c_va_arg *)

(*
Local Variables:
compile-command: "make -C ../../../../.."
End:
*)
