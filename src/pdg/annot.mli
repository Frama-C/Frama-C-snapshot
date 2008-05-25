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

(* $Id: annot.mli,v 1.8 2008/04/01 09:25:21 uid568 Exp $ *)

(** find the nodes needed for the annotation :
* the first list correspond to control dependancies,
* and the second part [(nodes, undef_loc)] correspond to dat dependencies.
* @raise Not_found when the statement is unreachable *)
val find_code_annot_nodes :
  PdgTypes.Pdg.t -> 
  before:bool -> Cil_types.stmt -> Cil_types.code_annotation ->
  PdgTypes.Node.t list * (PdgTypes.Node.t list  * Locations.Zone.t)
(*
Local Variables:
compile-command: "LC_ALL=C make -C ../.. -j"
End:
*)
