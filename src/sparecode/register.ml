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

let run ~select_annot ~select_slice_pragma =
  Format.printf "[sparecode] running@\n";

  (*let initial_file = Cil_state.file () in*)
  let kf_entry, _library = Globals.entry_point () in

  let proj = Marks.select_usefull_things ~select_annot ~select_slice_pragma kf_entry in

  let new_proj_name = "unused_removed" in
  let fresh_project = Project.create new_proj_name in
  Transform.Info.build_cil_file fresh_project proj;
  Format.printf "[sparecode] done. Result in new project '%s'.@." new_proj_name;
  fresh_project
    
let () =
  Db.Sparecode.run := run

let () =
  Options.add_plugin
    ~name:"Spare Code (experimental)"
    ~descr:"unused code detection"
    [ "-sparecode-analysis",
      Arg.Unit Cmdline.Sparecode.Analysis.on,
      ": perform a spare code analysis";
      "-sparecode-no-annot",
      Arg.Unit Cmdline.Sparecode.NoAnnot.on,
      ": don't select more things to keep every reachable annotation";
    ]

(*
Local Variables:
compile-command: "make -C ../.."
End:
*)
