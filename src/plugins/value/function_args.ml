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

open Cil_types

exception Actual_is_bottom
exception WrongFunctionType (* at a call through a pointer *)

(* We cannot statically check that a call through a function pointer is
   correct wrt the number of arguments and their types (see the examples at
   the end of tests/misc/fun_ptr.i). Thus, we make additional checks  here:
   the arguments size are correct, and the number of arguments is sufficient.*)
let check_arg_size expr formal =
  try
    if Cil.bitsSizeOf (Cil.typeOf expr) <> Cil.bitsSizeOf (formal.vtype)
    then raise WrongFunctionType
  with Cil.SizeOfError _ -> raise WrongFunctionType

let rec fold_left2_best_effort f acc l1 l2 =
  match l1,l2 with
  | _,[] -> acc
  | [],_ -> raise WrongFunctionType (* Too few arguments *)
  | (x1::r1),(x2::r2) -> fold_left2_best_effort f (f acc x1 x2) r1 r2

let actualize_formals ?(check = fun _ _ -> ()) kf state actuals =
  let formals = Kernel_function.get_formals kf in
  let treat_one_formal acc (expr, actual_o) formal =
    (check expr formal: unit);
    Cvalue.Model.add_base (Base.of_varinfo formal) actual_o acc
  in
  fold_left2_best_effort treat_one_formal state actuals formals

(** For all formals of [kf] whose address is taken, merge their values
    in [prev_state] and [new_state], and update [new_state]. This is
    useful to handle recursive calls. *)
let merge_referenced_formals kf prev_state new_state =
  let formals = Kernel_function.get_formals kf in
  let aux state vi =
    if vi.vaddrof then
      let b = Base.of_varinfo vi in
      let prev_offsm = Cvalue.Model.find_base b prev_state in
      let new_offsm = Cvalue.Model.find_base b new_state in
      match Cvalue.V_Offsetmap.join_top_bottom prev_offsm new_offsm with
      | `Top -> assert false
      | `Bottom -> Cvalue.Model.bottom
      | `Map m -> Cvalue.Model.add_base b m state
    else state
  in
  List.fold_left aux new_state formals

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


let compute_actual ~with_alarms ~warn_indeterminate state e =
  let warn kind =
    if with_alarms.CilE.imprecision_tracing.CilE.a_log then
      Value_parameters.result ~current:true ~once:true
        "completely invalid@ %s in evaluation of@ argument %a"
        kind Printer.pp_exp e;
    raise Actual_is_bottom
  in
  match e with
  | { enode = Lval lv } when not (Eval_typ.is_bitfield (Cil.typeOfLval lv)) ->
    let ploc, state, o =
      try Eval_exprs.offsetmap_of_lv ~with_alarms state lv
      with Int_Base.Error_Top ->
        Value_parameters.abort ~current:true "Function argument %a has \
            unknown size. Aborting" Printer.pp_exp e;
    in begin
      match o with
      | `Map o ->
        let typ_lv = Cil.typeOfLval lv in
        let o, state =
          if warn_indeterminate then
            match Warn.warn_reduce_indeterminate_offsetmap
             ~with_alarms typ_lv o (`PreciseLoc ploc) state
            with
            | `Bottom -> warn "value"
            | `Res r -> r
          else o, state
        in
        begin match Warn.offsetmap_contains_imprecision o with
        | Some v ->
          let loc = Precise_locs.imprecise_location ploc in
          Warn.warn_imprecise_lval_read ~with_alarms lv loc v
        | None -> ()
        end;
        o, state
      | `Bottom -> warn "location"
      | `Top -> Warn.warn_top ()
    end
  | _ ->
    let state, _, interpreted_expr =
      Eval_exprs.eval_expr_with_deps_state ~with_alarms None state e
    in
    if Cvalue.V.is_bottom interpreted_expr then warn "value";
    let typ = Cil.typeOf e in
    Eval_op.offsetmap_of_v ~typ interpreted_expr, state

let () =
  Db.Value.add_formals_to_state :=
    (fun state kf exps ->
       try
         let compute_actual =
           compute_actual ~with_alarms:CilE.warn_none_mode ~warn_indeterminate:false
         in
         let actuals =
           List.map (fun e -> e, fst (compute_actual state e)) exps
         in
         actualize_formals kf state actuals
       with Actual_is_bottom -> Cvalue.Model.bottom)

(*
Local Variables:
compile-command: "make -C ../../.."
End:
*)
