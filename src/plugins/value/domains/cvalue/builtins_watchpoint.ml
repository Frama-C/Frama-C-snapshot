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

open Abstract_interp
open Cvalue

type watch = Value of V.t | Cardinal of int

let equal_watch w1 w2 =
  match w1, w2 with
    Value v1, Value v2 -> V.equal v1 v2
  | Cardinal c1, Cardinal c2 -> c1 = c2
  | _ -> false

type watchpoint =
  { name_lv : Cil_types.exp;
    loc: Locations.location;
    v: watch;
    mutable remaining_count: Integer.t;
    mutable stmts: Cil_datatype.Stmt.Set.t }

let watch_table : watchpoint list ref = ref []

let new_watchpoint name_lv loc v n =
  { name_lv = name_lv;
    loc = loc;
    v = v;
    remaining_count = n;
    stmts = Cil_datatype.Stmt.Set.empty }

let add_watch make_watch state actuals =
  match actuals with
  | [(dst_e, dst, _); (_, size, _); (_, target_value, _); (_, number, _)] ->
    let size =
      try
        let size = Cvalue.V.project_ival size in
        Int.mul Int.eight (Ival.project_int size)
      with V.Not_based_on_null | Ival.Not_Singleton_Int ->
        raise Db.Value.Outside_builtin_possibilities
    in
    let number =
      try
        let number = Cvalue.V.project_ival number in
        Ival.project_int number
      with V.Not_based_on_null | Ival.Not_Singleton_Int ->
        raise Db.Value.Outside_builtin_possibilities
    in
    let loc_bits = Locations.loc_bytes_to_loc_bits dst in
    let loc = Locations.make_loc loc_bits (Int_Base.inject size) in
    let target_w = make_watch target_value in
    let current = !watch_table in
    if
      List.for_all
        (fun {loc=l; v=w} ->
           not (Locations.loc_equal l loc && equal_watch w target_w))
        current
    then
      watch_table :=
        (new_watchpoint dst_e loc target_w number) :: current;
    { Value_types.c_values = [None, state];
      c_clobbered = Base.SetLattice.bottom;
      c_from = None;
      c_cacheable = Value_types.Cacheable }
  | _ -> raise (Builtins.Invalid_nb_of_args 4)

let make_watch_value target_value = Value target_value

let make_watch_cardinal target_value =
  try
    let target_value = Cvalue.V.project_ival target_value in
    Cardinal (Integer.to_int (Ival.project_int target_value))
  with V.Not_based_on_null | Ival.Not_Singleton_Int
     | Z.Overflow (* from Integer.to_int *) ->
    raise Db.Value.Outside_builtin_possibilities

let () =
  Builtins.register_builtin "Frama_C_watch_value" (add_watch make_watch_value)
let () =
  Builtins.register_builtin
    "Frama_C_watch_cardinal"
    (add_watch make_watch_cardinal)

let watch_hook (stmt, _callstack, states) =
  let treat ({name_lv = name; loc=loc; v=wa; remaining_count=current; stmts=set} as w) =
    List.iter
      (fun state ->
         let vs = Model.find ~conflate_bottom:false state loc in
         let watching =
           match wa with
             Value v ->
             V.intersects vs v
           | Cardinal n ->
             ( try
                 ignore (V.cardinal_less_than vs n) ;
                 false
               with Not_less_than -> true)
         in
         if watching
         then begin
           Value_parameters.feedback ~once:true ~current:true
             "Watchpoint: %a %a%t"
             Printer.pp_exp name
             V.pretty vs
             Value_util.pp_callstack;
           if Integer.is_zero current ||
              (Cil_datatype.Stmt.Set.mem stmt set)
           then ()
           else
             let current = Integer.pred current in
             if Integer.is_zero current then raise Db.Value.Aborted;
             w.remaining_count <- current;
             w.stmts <- Cil_datatype.Stmt.Set.add stmt set;
         end)
      states
  in
  List.iter treat !watch_table

let () = Db.Value.Compute_Statement_Callbacks.extend_once watch_hook

(*
Local Variables:
compile-command: "make -C ../../../../.."
End:
*)
