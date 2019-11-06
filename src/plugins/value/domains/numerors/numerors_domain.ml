(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2019                                               *)
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
open Cil_types

(* The numerors values, plus some builtin functions. *)
module Numerors_Value = struct
  include Numerors_value

  (* In this domain, we only track floating-point variables. *)
  let track_variable vi = Cil.isFloatingType vi.vtype

  (* No widen in the domain for now *)
  let widen _ _ = top

  let dbetween = function
    | min :: max :: [] -> Numerors_value.dbetween min max
    | _ -> `Value Numerors_value.top

  let rbetween = function
    | min :: max :: [] -> Numerors_value.rbetween min max
    | _ -> `Value Numerors_value.top

  let sqrt = function
    | x :: [] -> Numerors_value.sqrt x
    | _ -> `Value Numerors_value.top

  let log = function
    | x :: [] -> Numerors_value.log x
    | _ -> `Value Numerors_value.top

  let exp = function
    | x :: [] -> Numerors_value.exp x
    | _ -> `Value Numerors_value.top

  let dprint_callstack = ref []
  let dprint = function
    | x :: [] ->
      let call fmt () =
        let abs = Numerors_value.get_max_absolute_error x in
        let rel = Numerors_value.get_max_relative_error x in
        match abs, rel with
        | Some x, Some y ->
          Format.fprintf fmt "@[%a@]@.@[%a@]@."
            Numerors_float.pretty x
            Numerors_float.pretty y
        | _, _ -> ()
        (*
        Format.fprintf fmt "@[%a@]@.@." Numerors_value.pretty x
        *)
      in dprint_callstack := !dprint_callstack @ [call] ;
      `Value Numerors_value.top
    | _ ->  `Value Numerors_value.top

  let builtins =
    [ ("Frama_C_double_interval", dbetween)
    ; ("Frama_C_real_interval_as_double", rbetween)
    ; ("log", log) ; ("exp", exp) ; ("sqrt", sqrt)
    ; ("DPRINT", dprint)
    ]
end

(* The numerors domain: a simple memory over the numerors value. *)
module Domain = struct
  module Name = struct let name = "numerors" end
  include Simple_memory.Make_Domain (Name) (Numerors_Value)

  let post_analysis f =
    match f, Value_parameters.NumerorsLogFile.get () with
    | _, s when s = "" -> ()
    | `Value _, s ->
      let log = open_out s in
      let fmt = Format.formatter_of_out_channel log in
      List.iter (fun f -> f fmt ()) !Numerors_Value.dprint_callstack ;
      close_out log
    | _, _ -> ()
end

(* Reduced product between the cvalue values and the numerors values. *)
let reduce_error cvalue error =
  try
    let ival = Cvalue.V.project_ival cvalue in
    match ival with
    | Ival.Float fval ->
      begin
        match Numerors_value.reduce fval error with
        | `Value error -> cvalue, error
        | `Bottom -> cvalue, error (* TODO: we should be able to reduce to bottom. *)
      end
    | _ -> cvalue, error
  with Cvalue.V.Not_based_on_null -> cvalue, error

(* Reduction of the numerors value resulting from a cast from int to float type,
   using the cvalue component of value abstractions. *)
let reduce_cast (module Abstract: Abstractions.S) =
  let module Val = struct
    include Abstract.Val

    (* Redefines the [forward_cast] function of the value component. *)
    let forward_cast =
      (* If cvalue or numerors do not belong to the abstraction, no reduction:
         the [forward_cast] function is unchanged. *)
      match get Main_values.CVal.key, mem Numerors_value.key with
      | None, _ | _, false -> forward_cast
      | Some get_cvalue, true ->
        (* Otherwise, applies the [forward_cast] function, but updates the
           numerors component of the result. *)
        fun ~src_type ~dst_type value ->
          forward_cast ~src_type ~dst_type value >>-: fun result ->
          match src_type, dst_type with
          | Eval_typ.TSInt _, Eval_typ.TSFloat fkind ->
            begin
              try
                let cvalue = get_cvalue value in
                let ival = Cvalue.V.project_ival cvalue in
                match Ival.min_and_max ival with
                | Some min, Some max ->
                  let min, max = Integer.to_int min, Integer.to_int max in
                  let prec = Numerors_utils.Precisions.of_fkind fkind in
                  let num = Numerors_value.of_ints ~prec min max in
                  set Numerors_value.key num result
                | _, _ -> result
              (* Integer.to_int may fail for too big integers. *)
              with Cvalue.V.Not_based_on_null | Z.Overflow -> result
            end
          | _, _ -> result
  end in
  (module struct
    module Val = Val
    module Loc = Abstract.Loc
    module Dom = Abstract.Dom
  end: Abstractions.S)

(* Register the domain as an Eva abstractions. *)
let () =
  let open Abstractions in
  let domain =
    { name = "numerors";
      priority = 0;
      values = Single (module Numerors_value);
      domain = Domain (module Domain); }
  in
  let reduced_product = Main_values.CVal.key, Numerors_value.key, reduce_error in
  register ~enable:Value_parameters.NumerorsDomain.get domain;
  register_value_reduction reduced_product;
  register_hook reduce_cast;
  Value_parameters.register_numerors ()
