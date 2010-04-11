(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2010                                               *)
(*    CEA   (Commissariat à l'énergie atomique et aux énergies            *)
(*           alternatives)                                                *)
(*    INRIA (Institut National de Recherche en Informatique et en         *)
(*           Automatique)                                                 *)
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
(*  See the GNU Lesser General Public License version v2.1                *)
(*  for more details (enclosed in the file licenses/LGPLv2.1).            *)
(*                                                                        *)
(**************************************************************************)

open Cil_types
open Cil

module BoolInfo = struct
  type t_proj = Marks.t_proj
  type t_fct = Marks.t_fct

  let fct_info project kf =
    match Marks.get_marks project kf with
      | None -> []
      | Some fm -> [fm]

  let body_visible _fm = true

  let param_visible fm n =
    let key = PdgIndex.Key.param_key n () in
      Marks.key_visible fm key

  let label_visible fm stmt label =
    let lab_key = PdgIndex.Key.label_key stmt label in
      Marks.key_visible fm lab_key

  let annotation_visible _fm _stmt ~before:_ _annot = 
    (* all the annotation should have been selected by the analysis *)
    true

  let fun_precond_visible _fm _p = 
    (* TODO : we say that they are removed in order to get correct results,
    * but in fact, we should select them ! *)
    false

  let fun_postcond_visible _fm _p = 
    (* TODO : we say that they are removed in order to get correct results,
    * but in fact, we should select them ! *)
    false

  let fun_variant_visible _fm _p = 
    (* TODO : we say that they are removed in order to get correct results,
    * but in fact, we should select them ! *)
    false

  let fun_assign_visible _fm _p = 
    (* TODO : we say that they are removed in order to get correct results,
    * but in fact, we should select them ! *)
    false

  let res_call_visible fm call_stmt =
    let key = PdgIndex.Key.call_outret_key call_stmt in
      Marks.key_visible fm key


  let called_info (project, _fm) call_stmt = match call_stmt.skind with
    | Instr (Call (_, _fexp, _, _)) ->
        begin
          let called_functions = Db.Value.call_to_kernel_function call_stmt in
	  match Kernel_function.Set.contains_single_elt called_functions with
	    None -> None
	  | Some funct ->
                  begin
                    let fm =
                      Marks.get_marks project funct
                    in match fm with
                      | None -> None
                      | Some fm -> Some (funct, fm)
                  end
        end
    | _ -> Sparecode_params.fatal "this call is not a call"

  let inst_visible fm stmt =
    match stmt.Cil_types.skind with
        | Cil_types.Block _ ->
            (* block are always visible for syntaxic reasons *)
            true
        | _ ->
            let stmt_key = PdgIndex.Key.stmt_key stmt in
            let visible = Marks.key_visible fm stmt_key in
                Sparecode_params.debug ~level:3 
                  "[sparecode] inst_visible : %a -> %s@\n"
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
