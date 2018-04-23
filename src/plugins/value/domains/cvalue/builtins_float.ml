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

open Cvalue

let wrap_fk r = function
  | Cil_types.FFloat -> Eval_op.wrap_float r
  | Cil_types.FDouble -> Eval_op.wrap_double r
  | _ -> assert false

let restrict_float expr fk value =
  let open Cvalue_forward in
  match Kernel.SpecialFloat.get () with
  | "none"       -> value, Alarmset.none
  | "nan"        -> restrict_float ~remove_infinite:false expr fk value
  | "non-finite" -> restrict_float ~remove_infinite:true expr fk value
  | _            -> assert false

let arity2 fk caml_fun state actuals =
  match actuals with
  | [_, arg1, _; _, arg2, _] ->
    begin
      let r =
        try
          let i1 = Cvalue.V.project_ival arg1 in
          let f1 = Ival.project_float i1 in
          let i2 = Cvalue.V.project_ival arg2 in
          let f2 = Ival.project_float i2 in
          let f' = Cvalue.V.inject_float (caml_fun (Fval.kind fk) f1 f2) in
          let v, _alarms = restrict_float fk Cil_datatype.Exp.dummy f' in
          (* Alarms should be handled by the preconditions of the builtin *)
          v
        with Cvalue.V.Not_based_on_null ->
          Cvalue.V.topify_arith_origin (V.join arg1 arg2)
      in
      { Value_types.c_values =
          if V.is_bottom r then []
          else [wrap_fk r fk, state ];
        c_clobbered = Base.SetLattice.bottom;
        c_from = None;
        c_cacheable = Value_types.Cacheable; }
    end
  | _ -> raise (Builtins.Invalid_nb_of_args 2)

let register_arity2 c_name fk f =
  let name = "Frama_C_" ^ c_name in
  Builtins.register_builtin name ~replace:c_name (arity2 fk f);
;;

let () =
  let open Fval in
  register_arity2 "atan2" Cil_types.FDouble atan2;
  register_arity2 "atan2f" Cil_types.FFloat atan2;
  register_arity2 "pow" Cil_types.FDouble pow;
  register_arity2 "powf" Cil_types.FFloat pow;
  register_arity2 "fmod" Cil_types.FDouble fmod;
  register_arity2 "fmodf" Cil_types.FFloat fmod;
;;


let arity1 name fk caml_fun state actuals =
  match actuals with
  | [_, arg, _] -> begin
      let r =
        try
          let i = Cvalue.V.project_ival arg in
          let f = Ival.project_float i in
          let f' = Cvalue.V.inject_float (caml_fun (Fval.kind fk) f) in
          let v, _alarms = restrict_float fk Cil_datatype.Exp.dummy f' in
          (* Alarms should be handled by the preconditions of the builtin *)
          v
        with
        | Cvalue.V.Not_based_on_null ->
          if Cvalue.V.is_bottom arg then begin
            V.bottom
          end else begin
            Value_parameters.result ~once:true ~current:true
              "function %s applied to address" name;
            Cvalue.V.topify_arith_origin arg
          end
      in
      { Value_types.c_values =
          if V.is_bottom r then []
          else [wrap_fk r fk, state ];
        c_clobbered = Base.SetLattice.bottom;
        c_from = None;
        c_cacheable = Value_types.Cacheable; }
    end
  | _ -> raise (Builtins.Invalid_nb_of_args 1)

let register_arity1 c_name fk f =
  let name = "Frama_C_" ^ c_name in
  Builtins.register_builtin name ~replace:c_name (arity1 name fk f);
;;

let () =
  let open Fval in
  register_arity1 "cos" Cil_types.FDouble cos;
  register_arity1 "sin" Cil_types.FDouble sin;
  register_arity1 "log" Cil_types.FDouble log;
  register_arity1 "log10" Cil_types.FDouble log10;
  register_arity1 "exp" Cil_types.FDouble exp;
  register_arity1 "sqrt" Cil_types.FDouble sqrt;
  register_arity1 "floor" Cil_types.FDouble floor;
  register_arity1 "ceil" Cil_types.FDouble ceil;
  register_arity1 "trunc" Cil_types.FDouble trunc;
  register_arity1 "round" Cil_types.FDouble fround;

  register_arity1 "cosf" Cil_types.FFloat cos;
  register_arity1 "sinf" Cil_types.FFloat sin;
  register_arity1 "logf" Cil_types.FFloat log;
  register_arity1 "log10f" Cil_types.FFloat log10;
  register_arity1 "expf" Cil_types.FFloat exp;
  register_arity1 "sqrtf" Cil_types.FFloat sqrt;
  register_arity1 "floorf" Cil_types.FFloat floor;
  register_arity1 "ceilf" Cil_types.FFloat ceil;
  register_arity1 "truncf" Cil_types.FFloat trunc;
  register_arity1 "roundf" Cil_types.FFloat fround;
;;
