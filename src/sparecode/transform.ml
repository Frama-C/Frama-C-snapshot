(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2008                                               *)
(*    CEA (Commissariat à l'Énergie Atomique)                             *)
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
open Cil

let debug1() = Cmdline.Debug.get() >= 1
let debug2() = Cmdline.Debug.get() >= 2


module BoolInfo = struct
  type t_proj = Marks.t_proj
  type t_fct = Marks.t_fct

  let fct_info project kf =
    match Marks.get_marks project kf with
      | None -> []
      | Some fm -> [fm]

  let param_visible fm n =
    let key = PdgIndex.Key.param_key n () in
      Marks.key_visible fm key

  let label_visible fm stmt label =
    let lab_key = PdgIndex.Key.label_key stmt label in
      Marks.key_visible fm lab_key

  let annotation_visible _fm _stmt ~before:_ _annot = 
    (* all the annotation should have been selected by the analysis *)
    true

  let res_call_visible fm call_stmt =
    let key = PdgIndex.Key.call_outret_key call_stmt in
      Marks.key_visible fm key


  let called_info (project, _fm) call_stmt = match call_stmt.skind with
    | Instr (Call (_, _fexp, _, _)) ->
        begin
          let called_functions = Db.Value.call_to_kernel_function call_stmt in
          match called_functions with
            | called_kf :: [] ->
                begin
                let fm =
                    Marks.get_marks project called_kf
                in match fm with
                  | None -> None
                  | Some fm -> Some (called_kf, fm)
                end
            | _ -> None
        end
    | _ -> assert false

  let inst_visible fm stmt =
    match stmt.Cil_types.skind with
        | Cil_types.Block _ ->
            (* block are always visible for syntaxic reasons *)
            true
        | _ ->
            let stmt_key = PdgIndex.Key.stmt_key stmt in
            let visible = Marks.key_visible fm stmt_key in
              if debug2 () then 
                Format.printf "[sparecode] inst_visible : %a -> %s@\n"
                  !Db.Pdg.pretty_key stmt_key 
                  (if visible then "true" else "false");
              visible

  let loc_var_visible fm var =
    let key = PdgIndex.Key.decl_var_key var in
      Marks.key_visible fm key

  let fct_name v _fm = v.Cil_types.vname

  let result_visible kf fm = 
    try inst_visible fm (Kernel_function.find_return kf)
    with Kernel_function.No_Definition -> true
end

module Info = Filter.F (BoolInfo)
