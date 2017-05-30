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

class prepare_visitor prj = object (_)
  inherit Visitor.frama_c_copy prj

  method !vstmt_aux _ =
    Cil.DoChildrenPost (fun stmt ->
      match stmt.skind with
      | Switch(_,sw_blk,_,_) ->
        let new_blk = Cil.mkBlock [ stmt ] in
        let new_stmt = Cil.mkStmt (Block new_blk) in
        new_blk.blocals <- sw_blk.blocals;
        sw_blk.blocals <- [];
        new_stmt
      | _ -> stmt)

  initializer
    Project.copy ~selection:(Parameter_state.get_selection ()) prj
end

let prepare () =
  Options.feedback ~level:2 "prepare AST for E-ACSL transformations";
  File.create_project_from_visitor
    "e_acsl_prepare_ast"
    (new prepare_visitor)
