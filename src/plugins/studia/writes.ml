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

(** Find the statements that writes a given zone. This is a lightweight version
    of module [Scope.Defs]. Instead of using PDGs (that may be very costly to
    compute), we only use Inout. This also means that we can find effects
    *after* the stmt the user has chosen. *)

open Cil_types
open Locations

type effects = {
  direct: bool;
  indirect: bool;
}

(** Does the functions called at [stmt] modify directly or indirectly [zlval] *)
let effects_of_call stmt zlval effects  =
  let aux_kf kf effects =
    let inout = !Db.Operational_inputs.get_internal_precise ~stmt kf in
    let out = inout.Inout_type.over_outputs in
    if Zone.intersects out zlval then
      if !Db.Value.use_spec_instead_of_definition kf then
        { effects with direct = true } (* Mark the effect as direct, there is
                                          no body for this funtion. *)
      else
        { effects with indirect = true } (* Indirect effect *)
    else
      effects
  in
  let kfs = Db.Value.call_to_kernel_function stmt in
  Kernel_function.Hptset.fold aux_kf kfs effects

class find_write zlval = object (self)
  inherit Visitor.frama_c_inplace

  val mutable res = ([] : (stmt * effects) list)

  method! vinst i =
    let stmt = Extlib.the self#current_stmt in
    begin
      let aux_call lvopt _kf _args _loc =
        (* Direct effect through the writing of [lvopt], or indirect inside
           the call. *)
        let z = !Db.Outputs.statement stmt in
        if Zone.intersects z zlval then
          let direct_write = match lvopt with
            | None -> false
            | Some lv ->
              let zlv = !Db.Value.lval_to_zone (Kstmt stmt) lv in
              Zone.intersects zlv zlval
          in
          let effects =
            effects_of_call stmt zlval {direct = direct_write; indirect =false}
          in
          res <- (stmt, effects) :: res
      in
      match i with
      | Set _ | Local_init(_, AssignInit _, _) ->
        (* Effect only throuh the written l-value *)
        let z = !Db.Outputs.statement stmt in
        if Zone.intersects z zlval then begin
          res <- (stmt, {direct = true; indirect = false}) :: res
        end
      | Call (lvopt, f, args, loc) -> aux_call lvopt f args loc
      | Local_init(v, ConsInit(f, args, k), l) ->
        Cil.treat_constructor_as_func aux_call v f args k l
      | _ -> () (* No effect *)
    end;
    Cil.SkipChildren

  method result = res
end

let compute z =
  let vis = new find_write z in
  let aux_kf_fundec kf =
    let all_out = !Db.Operational_inputs.get_internal_precise kf in
    let zout = all_out.Inout_type.over_outputs in
    if Zone.intersects zout z then begin
      let fundec = Kernel_function.get_definition kf in
      ignore
        (Visitor.visitFramacFunction (vis :> Visitor.frama_c_visitor) fundec;)
    end
  in
  let aux_kf kf =
    if Kernel_function.is_definition kf then aux_kf_fundec kf
  in
  Globals.Functions.iter aux_kf;
  vis#result
