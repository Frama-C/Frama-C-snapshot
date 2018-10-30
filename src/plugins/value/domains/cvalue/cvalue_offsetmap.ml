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

open Eval

exception Got_imprecise of Cvalue.V.t
let offsetmap_contains_imprecision offs =
  try
    Cvalue.V_Offsetmap.iter_on_values
      (fun v ->
         match Cvalue.V_Or_Uninitialized.get_v v with
         | Locations.Location_Bytes.Map _ -> ()
         | Locations.Location_Bytes.Top _ as v -> raise (Got_imprecise v)
      ) offs;
    None
  with Got_imprecise v -> Some v

let warn_right_imprecision lval loc offsetmap =
  match offsetmap_contains_imprecision offsetmap with
  | Some v -> Warn.warn_right_exp_imprecision lval loc v
  | None -> ()

let warn_if_imprecise lval loc offsm =
  match offsetmap_contains_imprecision offsm with
  | Some v ->
    let loc = Precise_locs.imprecise_location loc in
    Warn.warn_imprecise_lval_read lval loc v
  | None -> ()

let offsetmap_of_lval state lval loc =
  let offsm = Bottom.non_bottom (Eval_op.offsetmap_of_loc loc state) in
  warn_if_imprecise lval loc offsm;
  offsm

let offsetmap_of_v ~typ v =
  let size = Integer.of_int (Cil.bitsSizeOf typ) in
  let v = Cvalue.V.anisotropic_cast ~size v in
  let v = Cvalue.V_Or_Uninitialized.initialized v in
  Cvalue.V_Offsetmap.create ~size v ~size_v:size

let offsetmap_of_assignment state expr = function
  | Copy (lv, _value) -> offsetmap_of_lval state lv.lval lv.lloc
  | Assign value -> offsetmap_of_v ~typ:(Cil.typeOf expr) value
