(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2013                                               *)
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
open Abstract_interp
open Locations
open Cvalue

exception Actual_is_bottom
exception WrongFunctionType (* at a call through a pointer *)

(* We cannot statically check that a call through a function pointer is
   correct wrt the number of arguments and their types (see the examples at
   the end of tests/misc/fun_ptr.i). Thus, we make additional checks  here:
   the arguments size are correct, and the number of arguments is sufficient.*)
let check_arg_size expr formal =
  if Cil.bitsSizeOf (Cil.typeOf expr) <> Cil.bitsSizeOf (formal.vtype)
  then raise WrongFunctionType

let rec fold_left2_best_effort f acc l1 l2 =
  match l1,l2 with
  | _,[] -> acc
  | [],_ -> raise WrongFunctionType (* Too few arguments *)
  | (x1::r1),(x2::r2) -> fold_left2_best_effort f (f acc x1 x2) r1 r2

let actualize_formals ?(check = fun _ _ -> ()) ?(exact = fun _ -> true) kf state actuals =
  let formals = Kernel_function.get_formals kf in
  let treat_one_formal acc (expr, actual_o) formal =
    (check expr formal: unit);
    let loc_without_size =
      Location_Bits.inject (Base.create_varinfo formal) (Ival.zero)
    in
    Cvalue.Model.paste_offsetmap ~with_alarms:CilE.warn_none_mode
      ~from:actual_o
      ~dst_loc:loc_without_size
      ~start:Int.zero
      ~size:(Int_Base.project (Bit_utils.sizeof_vid formal))
      ~exact:(exact formal)
      acc
  in
  fold_left2_best_effort treat_one_formal state actuals formals

let main_initial_state_with_formals kf (state:Cvalue.Model.t) =
  match kf.fundec with
    | Declaration (_, _, None, _) -> state
    | Declaration (_, _, Some l, _)
    | Definition ({ sformals = l }, _) ->
	if l <> [] && Value_parameters.InterpreterMode.get()
	then begin
	    Value_parameters.error "Entry point %a has arguments"
	      Kernel_function.pretty kf;
	    exit 0;
	  end;
        List.fold_right
          Initial_state.initialize_var_using_type
          l
          state

let offsetmap_contains_indeterminate offs =
  V_Offsetmap.fold_on_values
    (fun v _ (allbot, init, noesc) ->
       let allbot = allbot && V.is_bottom (V_Or_Uninitialized.get_v v) in
       let flags = V_Or_Uninitialized.get_flags v in
       let init  = init  && V_Or_Uninitialized.is_initialized flags in
       let noesc = noesc && V_Or_Uninitialized.is_noesc flags in
       (allbot, init, noesc)
    ) offs (true, true, true)


let compute_actual ~with_alarms one_library_fun state e =
  let offsm = match e with
  | { enode = Lval lv } when not (Eval_op.is_bitfield (Cil.typeOfLval lv)) ->
      let loc, _, o = Eval_exprs.offsetmap_of_lv ~with_alarms state lv in
      (match o with
      | Some o ->
          (match Warn.offsetmap_contains_imprecision o with
             | Some v -> Warn.warn_imprecise_lval_read ~with_alarms lv loc v
             | None -> ());
          let allbot, init, noesc = offsetmap_contains_indeterminate o in
          if one_library_fun || allbot then (
            CilE.set_syntactic_context (CilE.SyMem lv);
            if not init then CilE.warn_uninitialized with_alarms;
            if not noesc then CilE.warn_escapingaddr with_alarms;
          );
          if allbot then (
            if with_alarms.CilE.imprecision_tracing.CilE.a_log != None then
              Value_parameters.result ~current:true ~once:true
                "completely invalid@ value in evaluation of@ argument %a"
                Printer.pp_lval lv;
	    raise Actual_is_bottom);
          o
      | None ->
          if with_alarms.CilE.imprecision_tracing.CilE.a_log != None then
            Value_parameters.result ~current:true ~once:true
              "completely invalid@ location in evaluation of@ argument %a"
              Printer.pp_lval lv;
	raise Actual_is_bottom)
  | _ ->
      let interpreted_expr = Eval_exprs.eval_expr ~with_alarms state e in
      if Cvalue.V.is_bottom interpreted_expr
      then begin
        if with_alarms.CilE.imprecision_tracing.CilE.a_log != None then
	  Value_parameters.result ~current:true
	    "all evaluations are invalid@ for function call argument@ @[%a@]"
            Printer.pp_exp e;
	raise Actual_is_bottom
	end;
      let typ = Cil.typeOf e in
      Eval_op.offsetmap_of_v ~typ interpreted_expr
  in
  e, offsm

let () =
  Db.Value.add_formals_to_state :=
    (fun state kf exps ->
       try
         let compute_actual =
           compute_actual ~with_alarms:CilE.warn_none_mode false
         in
         let actuals = List.map (compute_actual state) exps in
         actualize_formals kf state actuals
       with Actual_is_bottom -> Cvalue.Model.bottom)

(*
Local Variables:
compile-command: "make -C ../.."
End:
*)
