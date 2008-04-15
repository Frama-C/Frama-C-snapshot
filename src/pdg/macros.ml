(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2008                                               *)
(*    CEA   (Commissariat à l'Énergie Atomique)                           *)
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

let has_debug n = 
  Cmdline.Debug.get () >= n ||
  Cmdline.Pdg.Verbosity.get () > n

let debug n format = Debug.debug_f (has_debug n) format

let bug msg = raise (PdgTypes.Pdg_Internal_Error msg)

let cbug cond msg = if not cond then bug msg

let pretty_node fmt n = PdgTypes.Node.pretty fmt n

let get_pdg_kf pdg = PdgTypes.Pdg.get_kf pdg

let pdg_name pdg =
  Kernel_function.get_name (get_pdg_kf pdg)


(*
Local Variables:
compile-command: "LC_ALL=C make -C ../.."
End:
*)
