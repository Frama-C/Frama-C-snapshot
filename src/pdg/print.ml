(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2010                                               *)
(*    CEA   (Commissariat à l'énergie atomique et aux énergies            *)
(*           alternatives)                                                *)
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

open Cil_types

module K = PdgIndex.Key
module S = PdgIndex.Signature

module N = PdgTypes.Node
module G = PdgTypes.G
module Dpd = PdgTypes.Dpd

let pretty_key = K.pretty



    (*
  let build_fct_pdg_dot_file proj kf  =
    let pdg = get_pdg proj kf in
    let dot_filename = Pdg.build_dot_file pdg in
      Format.printf "[pdg] dot file generated in %s@." dot_filename;
      dot_filename

  let show_fct_pdg_dot_file proj kf =
    let filename = build_fct_pdg_dot_file proj kf in
    let cmd = "zgrviewer -Pdot" in
      ignore (Sys.command (cmd^" $PWD/" ^ filename))
*)

(*-----------------------------------------------------------------------*)
