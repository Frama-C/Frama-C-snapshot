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

let info() = Cmdline.Debug.get() >= 1 || Cmdline.Pdg.Verbosity.get () >= 1
let debug1() = Cmdline.Debug.get() > 1 || Cmdline.Pdg.Verbosity.get () > 1
let debug2() = Cmdline.Debug.get() > 2 || Cmdline.Pdg.Verbosity.get () > 2

let bug msg = raise (PdgTypes.Pdg_Internal_Error msg)

let cbug cond msg = if not cond then bug msg

let pretty_node fmt n = PdgTypes.Node.pretty fmt n

let pdg_name pdg =
  let var = (PdgTypes.Pdg.get_var_fct pdg) in var.Cil_types.vname

let get_pdg_kf pdg = Globals.Functions.get (PdgTypes.Pdg.get_var_fct pdg)

(*
Local Variables:
compile-command: "LC_ALL=C make -C ../.."
End:
*)
