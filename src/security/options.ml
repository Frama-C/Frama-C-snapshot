(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2009                                               *)
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

module P = Plugin.Register
  (struct
     let name = "security"
     let shortname = "security"
     let descr = "security analysis (experimental, undocumented)"
   end)
include P

module Analysis =
  False
    (struct
       let option_name = "-security-analysis"
       let descr = "perform security analysis"
     end)

module Lattice =
  String
    (struct
       let option_name = "-security-lattice"
       let default = "weak"
       let arg_name = ""
       let descr = "specify security lattice"
     end)

module PropagateAssertions =
  False
    (struct
       let option_name = "-security-propagate-assertions"
       let descr = "propagate security assertions if possible"
     end)

module Slicing =
  False
    (struct
       let option_name = "-security-slicing"
       let descr = "perfom the security slicing analysis"
     end)

let is_on () = Analysis.get () || Slicing.get ()

module LogicAnnotation =
  EmptyString
    (struct
       let option_name = "-security-annotation"
       let arg_name = ""
       let descr = "recognize security annotations of the specified lattice"
     end)

open Extlib

let get_selection_after_slicing () =
  let add s = Project.Selection.add s Kind.Do_Not_Select_Dependencies in
  (add Analysis.self $ add Lattice.self $ add PropagateAssertions.self
     $ add LogicAnnotation.self)
    (* [JS 2009/04/17] prefix [Debug] by [P] below to prevent wrong
       computation of circular dependencies by ocamldep *)
    (Project.Selection.singleton P.Debug.self Kind.Do_Not_Select_Dependencies)

(*
Local Variables:
compile-command: "LC_ALL=C make -C ../.. -j"
End:
*)
