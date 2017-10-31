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

exception Actual_is_bottom
exception WrongFunctionType (* at a call through a pointer *)

let rec fold_left2_best_effort f acc l1 l2 =
  match l1,l2 with
  | _,[] -> acc
  | [],_ -> raise WrongFunctionType (* Too few arguments *)
  | (x1::r1),(x2::r2) -> fold_left2_best_effort f (f acc x1 x2) r1 r2

let actualize_formals kf state actuals =
  let formals = Kernel_function.get_formals kf in
  let treat_one_formal acc actual_o formal =
    Cvalue.Model.add_base (Base.of_varinfo formal) actual_o acc
  in
  fold_left2_best_effort treat_one_formal state actuals formals

let offsetmap_of_lv state lv =
  let open Locations in
  let state, loc_to_read, _typ = !Db.Value.lval_to_precise_loc_state state lv in
  let aux loc offsm_res =
    let size = Int_Base.project loc.size in
    let copy = Cvalue.Model.copy_offsetmap loc.loc size state in
    Bottom.join Cvalue.V_Offsetmap.join copy offsm_res
  in
  Precise_locs.fold aux loc_to_read `Bottom

let compute_actual state e =
  match e with
  | { enode = Lval lv } when not (Eval_typ.is_bitfield (Cil.typeOfLval lv)) ->
    let o =
      try offsetmap_of_lv state lv
      with Abstract_interp.Error_Top ->
        Value_parameters.abort ~current:true "Function argument %a has \
            unknown size. Aborting" Printer.pp_exp e;
    in begin
      match o with
      | `Value o -> o
      | `Bottom -> raise Actual_is_bottom
    end
  | _ ->
    let interpreted_expr = !Db.Value.eval_expr state e in
    if Cvalue.V.is_bottom interpreted_expr then raise Actual_is_bottom;
    let typ = Cil.typeOf e in
    Eval_op.offsetmap_of_v ~typ interpreted_expr

let () =
  Db.Value.add_formals_to_state :=
    (fun state kf exps ->
       try
         let actuals =  List.map (fun e -> compute_actual state e) exps in
         actualize_formals kf state actuals
       with Actual_is_bottom -> Cvalue.Model.bottom)

(*
Local Variables:
compile-command: "make -C ../../../.."
End:
*)
