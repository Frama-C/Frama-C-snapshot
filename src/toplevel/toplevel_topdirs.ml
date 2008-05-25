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

let top = 
  let dir_bin = Filename.dirname Sys.executable_name in
    Filename.concat dir_bin ".."

let add_top dir =
  let abs_dir = Filename.concat top dir in
    Topdirs.dir_directory abs_dir

let () = add_top "src/kernel"
let () = add_top "src/memory_state"
let () = add_top "src/ai"
let () = add_top "src/logic"
let () = add_top "lib"
let () = add_top "lib/plugins"
let () = add_top "src/toplevel"
let () = add_top "cil/src"

