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

let category = File.register_code_transformation_category "variadic"

let () =
  Cmdline.run_after_extended_stage
    begin fun () ->
      State_dependency_graph.add_dependencies
        ~from:Options.Enabled.self
        [ Ast.self ]
    end;
  Cmdline.run_after_configuring_stage
    begin fun () ->
      let translate file =
        if Options.Enabled.get () then
          Translate.translate_variadics file
      in
      File.add_code_transformation_before_cleanup category translate
    end
