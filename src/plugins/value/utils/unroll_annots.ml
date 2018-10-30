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

open Cil_types

exception Parse_error of string option

let parse_error ?msg () = raise (Parse_error msg)

let () = Logic_typing.register_code_annot_next_loop_extension "unroll"
    begin fun ~typing_context ~loc:_ args ->
      match args with
      | [arg] ->
        let open Logic_typing in
        Ext_terms
          [typing_context.type_term typing_context typing_context.pre_state arg]
      | _ -> parse_error ~msg:"must be a single term" ()
    end

let () = Cil_printer.register_behavior_extension "unroll"
    begin fun _pp fmt lp ->
      match lp with
      | Ext_terms [t] -> Printer.pp_term fmt t
      | Ext_id _ | Ext_preds _ | Ext_terms _ -> assert false
    end

let get_unroll_terms stmt =
  Annotations.fold_code_annot
    (fun _emitter annot acc ->
       match annot with
       | {annot_content =
            AExtended (_, true, (_, "unroll", _,Ext_terms [term]))} ->
         term :: acc
       | _ -> acc
    ) stmt []
