(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2011                                               *)
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

let top =
  let dir_bin = Filename.dirname Sys.executable_name in
    Filename.concat dir_bin ".."

let add_top dir =
  let abs_dir = Filename.concat top dir in
    Topdirs.dir_directory abs_dir

let src_dir =
  [ "ai"; "buckx"; "constant_propagation"; "cxx_types";
    "from"; "gui"; "impact"; "inout"; "jessie"; "journal"; "kernel"; "lib";
    "logic"; "memory_state"; "misc"; "modular_dependencies"; "occurrence";
    "pdg"; "pdg_types"; "phantom"; "postdominators"; "project"; "scope";
    "security"; "semantic_callgraph"; "slicing"; "slicing_types"; "sparecode";
    "toplevel"; "users"; "value"; "wp" ]

let () =
  List.iter (fun s -> add_top (Filename.concat "src" s)) src_dir;
  add_top "external"
