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

(** Find the statements that reads a given zone, using Inout. (Thus,
    only operational reads are found.) *)

open Cil_types
open Locations
open Writes

class find_read zlval = object
  inherit Visitor.frama_c_inplace

  val mutable res = ([] : (stmt * effects) list)

  method! vstmt_aux stmt =
    let aux_call lvopt _kf args _loc =
      let z = !Db.Inputs.statement stmt in
      if Zone.intersects z zlval then begin
        (* Computes what is read to evaluate [args] and [lvopt] *)
        let deps =
          List.map (!Db.Inputs.expr stmt) args
        in
        let deps = List.fold_left Zone.join Zone.bottom deps in
        let deps = match lvopt with
          | None -> deps
          | Some lv ->
            let dlv, _ =
              !Db.Value.lval_to_loc_with_deps (Kstmt stmt) ~deps:Zone.bottom lv
            in
            Zone.join dlv deps
        in
        let direct = Zone.intersects deps zlval in
        (* now determine if the functions called at [stmt] read directly or
             indirectly [zlval] *)
        let aux_kf kf effects =
          let inputs = !Db.Inputs.get_internal kf in
          (* TODO: change to this once we can get "full" inputs through Inout.
             Currently, non operational inputs disappear, and this function
             is not suitable.
             let inout = !Db.Operational_inputs.get_internal_precise ~stmt kf in
             let inputs = inout.Inout_type.over_inputs in *)
          if Zone.intersects inputs zlval then
            if !Db.Value.use_spec_instead_of_definition kf then
              (* Direst, as there is no body for this funtion. *)
              { effects with direct = true } 
            else
              { effects with indirect = true } (* Indirect effect *)
          else
            effects (* this function pointer does not read [zlval] *)
        in
        let kfs = Db.Value.call_to_kernel_function stmt in
        let effects =
          Kernel_function.Hptset.fold aux_kf kfs {direct; indirect = false}
        in
        res <- (stmt, effects) :: res
      end
    in
    match stmt.skind with
    | Instr (Call (lvopt, f, args, loc)) ->
      aux_call lvopt f args loc;
      Cil.SkipChildren          
    | Instr (Local_init(v, ConsInit(f, args, k), l)) ->
      Cil.treat_constructor_as_func aux_call v f args k l;
      Cil.SkipChildren
    | Instr _ ->
      let z = !Db.Inputs.statement stmt in
      if Zone.intersects z zlval then begin
        res <- (stmt, {direct = true; indirect = false}) :: res
      end;
      Cil.SkipChildren
    | If (e, _, _, _) | Switch (e, _, _, _) ->
      let z = !Db.Inputs.expr stmt e in
      if Zone.intersects z zlval then begin
        res <- (stmt, {direct = true; indirect = false}) :: res
      end;
      Cil.DoChildren
    | _ -> Cil.DoChildren

  method result = res
end

let compute z =
  let vis = new find_read z in
  let aux_kf_fundec kf =
    let all_in = !Db.Inputs.get_internal kf in
    if Zone.intersects all_in z then begin
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
