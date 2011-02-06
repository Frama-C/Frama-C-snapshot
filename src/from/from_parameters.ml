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

include Plugin.Register
  (struct
     let name = "from analysis"
     let shortname = "from"
     let help = "functional dependencies"
   end)

module ForceDeps =
  False
    (struct
       let option_name = "-deps"
       let help = "force dependencies display"
       let kind = `Tuning
     end)

module ForceCallDeps =
  False
    (struct
       let option_name = "-calldeps"
       let help = "force callsite-wise dependencies"
       let kind = `Tuning
     end)

module PathDeps =
False
    (struct
       let option_name = "-experimental-path-deps"
       let help = "experimental"
       let kind = `Tuning
     end)



(*
Local Variables:
compile-command: "LC_ALL=C make -C ../.."
End:
*)
