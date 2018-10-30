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

#24 "src/plugins/value/domains/numerors/numerors_domain.ok.ml"

open Eval
open Cil_types

type value = Numerors_value.t
type location = Precise_locs.precise_location
let value_key = Numerors_value.error_key

let ok = true

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

let add_numerors_value (module Value: Abstract_value.Internal) =
  let module External_Value = Structure.Open (Structure.Key_Value) (Value) in
  let module V = struct
    include Value_product.Make (Value) (Numerors_value)

    let forward_cast = match External_Value.get Main_values.cvalue_key with
      | None -> forward_cast
      | Some get_cvalue ->
        fun ~src_type ~dst_type (value, num) ->
          forward_cast ~src_type ~dst_type (value, num) >>-: fun (value', num) ->
          let num = match src_type, dst_type with
            | Eval_typ.TSInt _, Eval_typ.TSFloat fkind ->
              begin
                try
                  let cvalue = get_cvalue value in
                  let ival = Cvalue.V.project_ival cvalue in
                  match Ival.min_and_max ival with
                  | Some min, Some max ->
                    let min, max = Integer.to_int min, Integer.to_int max in
                    let prec = Numerors_utils.Precisions.of_fkind fkind in
                    Numerors_value.of_ints ~prec min max
                  | _, _ -> num
                (* Integer.to_int may fail for too big integers. *)
                with Cvalue.V.Not_based_on_null | Failure _ -> num
              end
            | _, _ -> num
          in
          value', num
  end in
  (module V: Abstract_value.Internal)

let reduce_error (type v) (module V: Abstract_value.External with type t = v) =
  match V.get Numerors_value.error_key, V.get Main_values.cvalue_key with
  | Some get_error, Some get_cvalue ->
    begin
      let set_error = V.set Numerors_value.error_key in
      fun t ->
        let cvalue = get_cvalue t in
        try
          let ival = Cvalue.V.project_ival cvalue in
          match ival with
          | Ival.Float fval ->
            begin
              let error = get_error t in
              let error = Numerors_value.reduce fval error in
              match error with
              | `Value error -> set_error error t
              | `Bottom -> t (* TODO: we should be able to reduce to bottom. *)
            end
          | _ -> t
        with Cvalue.V.Not_based_on_null -> t
    end
  | _, _ -> fun x -> x


module Domain = struct
  module Name = struct let name = "numerors" end
  include Simple_memory.Make_Domain (Name) (Numerors_Value)

  let post_analysis f =
    match f, Value_parameters.NumerorsLogFile.get () with
    | _, s when s = "" -> ()
    | `Value _, s ->
      let log = Pervasives.open_out s in
      let fmt = Format.formatter_of_out_channel log in
      List.iter (fun f -> f fmt ()) !Numerors_Value.dprint_callstack ;
      Pervasives.close_out log
    | _, _ -> ()
end

let numerors_domain () =
  Value_parameters.warning "The numerors domain is experimental.";
  (module Domain: Abstract_domain.Internal with type value = value
                                            and type location = location)
