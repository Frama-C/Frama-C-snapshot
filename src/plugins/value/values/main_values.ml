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

let return t = `Value t, Alarmset.none

let cvalue_key = Structure.Key_Value.create_key "cvalue"

module CVal = struct
  include Cvalue.V

  let structure = Structure.Key_Value.Leaf cvalue_key

  let zero = Cvalue.V.singleton_zero
  let float_zeros = Cvalue.V.inject_ival Ival.float_zeros

  let top = Cvalue.V.top
  let top_int = Cvalue.V.top_int
  let inject_int _typ = Cvalue.V.inject_int
  let inject_address vi =
    let base = Base.of_varinfo vi in
    Cvalue.V.inject base Ival.zero

  let equal = Cvalue.V.equal
  let is_included = Cvalue.V.is_included
  let join = Cvalue.V.join
  let narrow a b =
    let n = Cvalue.V.narrow a b in
    if Cvalue.V.is_bottom n
    then `Bottom
    else `Value n

  let constant exp = function
    | CInt64 (i,_k,_s) -> (* Integer constants never overflow, because the
                             front-end chooses a suitable type. *)
      return (Cvalue.V.inject_int i)
    | CChr c           -> return (Cvalue.V.inject_int (Cil.charConstToInt c))
    | CWStr _ | CStr _ ->
      return (Cvalue.V.inject (Base.of_string_exp exp) Ival.zero)
    | CReal (f, fkind, fstring) ->
      Cvalue_forward.eval_float_constant exp f fkind fstring
    | CEnum _ -> assert false

  let forward_unop ~context typ unop value =
    let value, alarms = Cvalue_forward.forward_unop ~context typ unop value in
    (* TODO: `Bottom must be in CValue and Cvalue_forward. *)
    if Cvalue.V.is_bottom value
    then `Bottom, alarms
    else `Value value, alarms

  let forward_binop ~context typ binop v1 v2 =
    let value, alarms =
      match typ with
      | TFloat (fkind, _) ->
        Cvalue_forward.forward_binop_float_alarm
          (Value_util.get_rounding_mode ()) ~context fkind v1 binop v2
      | TInt _ | TPtr _ | _ as typ ->
        Cvalue_forward.forward_binop_int ~context ~typ ~logic:false v1 binop v2
    in
    if Cvalue.V.is_bottom value
    then `Bottom, alarms
    else `Value value, alarms

  let truncate_integer expr range value =
    let v, alarms = Cvalue_forward.truncate_integer expr range value in
    if Cvalue.V.is_bottom v then `Bottom, alarms else `Value v, alarms

  let rewrap_integer = Cvalue_forward.rewrap_integer

  let cast_float expr fkind value =
    let v, alarms = Cvalue_forward.cast_float expr fkind value in
    if Cvalue.V.is_bottom v then `Bottom, alarms else `Value v, alarms

  let do_promotion ~src_typ ~dst_typ exp v =
    let rounding_mode = Value_util.get_rounding_mode () in
    let v, alarms =
      Cvalue_forward.do_promotion ~rounding_mode ~src_typ ~dst_typ exp v
    in
    if Cvalue.V.is_bottom v then `Bottom, alarms else `Value v, alarms


  let backward_binop ~input_type ~resulting_type binop ~left ~right ~result =
    let reduction =
      Cvalue_backward.backward_binop
        ~typ_res:resulting_type ~res_value:result ~typ_e1:input_type left binop right
    in
    match reduction with
    | None -> `Value (None, None)
    | Some (v1, v2) ->
      if Cvalue.V.is_bottom v1 || Cvalue.V.is_bottom v2
      then `Bottom
      else `Value (Some v1, Some v2)

  let backward_unop ~typ_arg op ~arg ~res =
    let reduction = Cvalue_backward.backward_unop ~typ_arg op ~arg ~res in
    match reduction with
    | None -> `Value None
    | Some v as r ->
      if Cvalue.V.is_bottom v
      then `Bottom
      else `Value r

  let backward_cast ~src_typ ~dst_typ ~src_val ~dst_val =
    let reduction =
      Cvalue_backward.backward_cast ~src_typ ~dst_typ ~src_val ~dst_val
    in
    match reduction with
    | None -> `Value None
    | Some v ->
      if Cvalue.V.is_bottom v
      then `Bottom
      else if Cvalue.V.is_included src_val v
      then `Value None
      else `Value (Some v)

  let resolve_functions = Eval_typ.resolve_functions

end

let interval_key = Structure.Key_Value.create_key "interval"

module Interval = struct

  include Datatype.Option (Ival)
  let structure = Structure.Key_Value.Leaf interval_key

  let pretty_typ _ = pretty
  
  let top = None

  let is_included a b = match a, b with
    | _, None        -> true
    | None, _        -> false
    | Some a, Some b -> Ival.is_included a b

  let join a b = match a, b with
    | None, _ | _, None -> None
    | Some a, Some b    -> Some (Ival.join a b)

  let narrow a b = match a, b with
    | None, x | x, None -> `Value x
    | Some a, Some b ->
      let res = Ival.narrow a b in
      if Ival.is_bottom res then `Bottom else `Value (Some res)

  let zero = None
  let float_zeros = None
  let top_int = None
  let inject_int _typ _i = None
  let inject_address _ = None

  let top_eval = `Value top, Alarmset.all
  let constant _ _ = top_eval
  let forward_unop ~context:_ _ _ _ = top_eval
  let forward_binop ~context:_ _ _ _ _ = top_eval
  let do_promotion ~src_typ:_ ~dst_typ:_ _ _ = top_eval

  let resolve_functions ~typ_pointer:_ _ = `Top, true

  (* TODO *)
  let truncate_integer _expr _range value = `Value value, Alarmset.all

  let rewrap_integer range value =
    match value with
    | None -> value
    | Some value ->
      let size = Integer.of_int range.Eval_typ.i_bits in
      let signed = range.Eval_typ.i_signed in
      Some (Ival.cast ~signed ~size ~value)

  let cast_float_aux fkind ival =
    match Value_util.float_kind fkind with
    | Fval.Float32 ->
      let rounding_mode = Value_util.get_rounding_mode () in
      let b, ival = Ival.force_float FFloat ival in
      let b', ival = Ival.cast_float ~rounding_mode ival in
      b || b', ival
    | Fval.Float64 ->
      let b, ival = Ival.force_float FDouble ival in
      let b', ival = Ival.cast_double ival in
      b || b', ival

  let cast_float exp fkind value =
    match value with
    | None -> `Value value, Alarmset.all
    | Some value ->
      let overflow, res = cast_float_aux fkind value in
      let alarms =
        if overflow
        then Alarmset.singleton (Alarms.Is_nan_or_infinite (exp, fkind))
        else Alarmset.none
      in
      `Value (Some res), alarms

  let backward_unop ~typ_arg:_ _unop ~arg:_ ~res:_ = `Value None
  let backward_binop ~input_type:_ ~resulting_type:_ _binop ~left:_ ~right:_ ~result:_ =
    `Value (None, None)
  let backward_cast ~src_typ:_ ~dst_typ:_ ~src_val:_ ~dst_val:_ =
    `Value None
end

(*
Local Variables:
compile-command: "make -C ../../../.."
End:
*)
