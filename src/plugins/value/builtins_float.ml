(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2015                                               *)
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

module BAlms = Fval.Builtin_alarms

type fk = Fval.float_kind = Float32  | Float64

let pp_fk fmt = function
  | Float32 -> Format.pp_print_string fmt "float"
  | Float64 -> Format.pp_print_string fmt "double"

let wrap_fk r = function
  | Float32 -> Eval_op.wrap_float r
  | Float64 -> Eval_op.wrap_double r

type args =
  | Arg1 of Fval.t
  | Arg2 of Fval.t * Fval.t

let (not_finite_result_msg : (string -> 'a,_,_,_) format4) =
  "builtin %s: result is always non-finite"

let pp_builtin_alarms fmt alarms =
  let module BA = Fval.Builtin_alarms in
  if not (BA.is_empty alarms) then begin
    let aux_nan alarm l = match alarm with
      | Fval.ANaN s -> s :: l
      | _ -> l
    in
    let nan_reasons = BA.fold aux_nan alarms [] in
    if nan_reasons <> [] then
      Pretty_utils.pp_list ~pre:":@ " ~sep:"@ " ~suf:""
        Format.pp_print_string fmt nan_reasons;
    let pp_alarm fmt = function
      | Fval.APosInf -> Format.pp_print_string fmt "+oo"
      | Fval.ANegInf -> Format.pp_print_string fmt "-oo"
      | Fval.ANaN _ -> Format.pp_print_string fmt "NaN"
      | Fval.AAssume _ -> (* should have been filtered *) assert false
    in
    (* do not print anything for 'AAssume' alarms *)
    let alarms =
      BA.filter (function Fval.AAssume _ -> false | _ -> true) alarms
    in
    Pretty_utils.pp_iter ~pre:",@ computation may result in " ~sep:"/" ~suf:""
      BA.iter pp_alarm fmt alarms;
  end
;;

let warn_alarms name args alarms res =
  let bottom = res = `Bottom in
  if bottom || not (Fval.Builtin_alarms.is_empty alarms) then
    let pp_bot fmt = if bottom then Format.fprintf fmt "completely@ " in
    let pp_arg fmt = match args with
      | Arg1 f ->
        Format.fprintf fmt "out-of-range argument@ (%a)" Fval.pretty f
      | Arg2 (f1, f2) ->
        Format.fprintf fmt "out-of-range arguments@ (%a, %a)"
          Fval.pretty f1 Fval.pretty f2
    in
    Value_parameters.warning ~once:true ~current:true
      "@[builtin %s:@ %t%t@,%a@]" name pp_bot pp_arg pp_builtin_alarms alarms

let lift_bottom f =
  match f with
  | `Bottom -> V.bottom
  | `Value f ->
    assert (Fval.has_finite f); (* TODO: is_finite would be more appropriate *)
    Cvalue.V.inject_ival (Ival.inject_float f)

let arity2 name fk caml_fun state actuals =
  match actuals with
  | [_, arg1, _; _, arg2, _] ->
    begin
      let r =
        try
          let i1 = Cvalue.V.project_ival arg1 in
          let f1 = Ival.project_float i1 in
          let i2 = Cvalue.V.project_ival arg2 in
          let f2 = Ival.project_float i2 in
          let alarms, f_res = caml_fun f1 f2 in
          warn_alarms name (Arg2 (f1, f2)) alarms f_res;
          lift_bottom f_res
        with
        | Ival.Nan_or_infinite (* from project_float *) ->
          Value_parameters.error ~once:true ~current:true
            "@[Invalid@ (integer)@ argument@ for@ builtin %s.@ Probably@ \
             missing@ declaration@ '%a %s(%a, %a);@]'"
            name pp_fk fk name pp_fk fk pp_fk fk;
          Cvalue.V.topify_arith_origin (V.join arg1 arg2)
        | Cvalue.V.Not_based_on_null ->
          Value_parameters.result ~once:true ~current:true "%s"
            ("builtin " ^ name ^ " applied to address");
          Cvalue.V.topify_arith_origin (V.join arg1 arg2)
        | Fval.Non_finite ->
          Value_parameters.warning ~once:true ~current:true
            not_finite_result_msg name;
          V.bottom
      in
      { Value_types.c_values =
          if V.is_bottom r then []
          else [wrap_fk r fk, state ];
        c_clobbered = Base.SetLattice.bottom;
        c_cacheable = Value_types.Cacheable; }
    end
  | _ ->
    Value_parameters.error "%s"
      ("Invalid argument for " ^ name ^ " function");
    raise Db.Value.Aborted

let register_arity2 name fk f =
  let name = "Frama_C_" ^ name in
  Builtins.register_builtin name (arity2 name fk f);
;;

let () =
  let open Fval in
  register_arity2 "atan2" Float64 atan2;
  register_arity2 "pow" Float64 pow;
  register_arity2 "fmod" Float64 fmod;

  register_arity2 "powf" Float32 powf;
;;


let arity1 name fk caml_fun state actuals =
  match actuals with
  | [_, arg, _] -> begin
      let warn () =
        Value_parameters.warning ~once:true ~current:true
          "builtin %s: out-of-range argument %a" name V.pretty arg
      in
      let r =
        try
          let i = Cvalue.V.project_ival arg in
          let f = Ival.project_float i in
          let nearest_even = Fval.Nearest_Even in
          let rounding_mode = Value_util.get_rounding_mode () in
          if rounding_mode <> nearest_even then
            Value_parameters.warning ~once:true
              "option -all-rounding-modes is not supported for builtin %s" name;
          let alarms, f' = caml_fun nearest_even f in
          warn_alarms name (Arg1 f) alarms f';
          lift_bottom f'
        with
        | Ival.Nan_or_infinite (* from project_float *) ->
          Value_parameters.error ~once:true ~current:true
            "@[Invalid@ (integer)@ argument %a@ for@ builtin %s.@ Probably@ \
             missing@ declaration@ '%a %s(%a);@]'"
            V.pretty arg name pp_fk fk name pp_fk fk;
          Cvalue.V.topify_arith_origin arg
        | Cvalue.V.Not_based_on_null ->
          if Cvalue.V.is_bottom arg then begin
            (* Probably does not occur, should be caught earlier by Value *)
            warn ();
            V.bottom
          end else begin
            warn ();
            Value_parameters.result ~once:true ~current:true
              "function %s applied to address" name;
            Cvalue.V.topify_arith_origin arg
          end
        | Fval.Non_finite ->
          Value_parameters.warning ~once:true ~current:true
            not_finite_result_msg name;
          V.bottom
      in
      { Value_types.c_values =
          if V.is_bottom r then []
          else [wrap_fk r fk, state ];
        c_clobbered = Base.SetLattice.bottom;
        c_cacheable = Value_types.Cacheable; }
    end
  | _ ->
    Value_parameters.error "%s"
      ("Invalid argument for " ^ name ^ " function");
    raise Db.Value.Aborted

let register_arity1 name fk f =
  let name = "Frama_C_" ^ name in
  Builtins.register_builtin name (arity1 name fk f);
;;

(* Wrapper for old stype abstract functions, that do not accept a rounding
   mode, and return no alarm *)
let wrap_cos_sin f _rounding_mode v =
  BAlms.empty (* no alarm *), `Value (f v)

(* Wrapper for functions that do not return `Bottom *)
let wrap_not_bottom f rounding_mode v =
  let alarms, r = f rounding_mode v in
  alarms, `Value r

let () =
  let open Fval in
  register_arity1 "cos" Float64 (wrap_cos_sin cos);
  register_arity1 "cos_precise" Float64 (wrap_cos_sin cos_precise);
  register_arity1 "sin" Float64 (wrap_cos_sin sin);
  register_arity1 "sin_precise" Float64 (wrap_cos_sin sin_precise);
  register_arity1 "log" Float64 log;
  register_arity1 "log10" Float64 log10;
  register_arity1 "exp" Float64 (wrap_not_bottom exp);
  register_arity1 "sqrt" Float64 sqrt;
  register_arity1 "floor" Float64 (wrap_not_bottom floor);
  register_arity1 "ceil" Float64 (wrap_not_bottom ceil);
  register_arity1 "trunc" Float64 (wrap_not_bottom trunc);
  register_arity1 "round" Float64 (wrap_not_bottom fround);

  register_arity1 "logf" Float32 logf;
  register_arity1 "log10f" Float32 log10f;
  register_arity1 "expf" Float32 (wrap_not_bottom expf);
  register_arity1 "sqrtf" Float32 sqrtf;
  register_arity1 "floorf" Float32 (wrap_not_bottom floor);
  register_arity1 "ceilf" Float32 (wrap_not_bottom ceil);
  register_arity1 "truncf" Float32 (wrap_not_bottom trunc);
  register_arity1 "roundf" Float32 (wrap_not_bottom fround);
;;
