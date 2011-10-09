(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2011                                               *)
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

type t_proj

type t_fct

val select_useful_things :
  select_annot:bool -> select_slice_pragma:bool -> Cil_types.kernel_function
  -> t_proj

val get_marks : t_proj -> Cil_types.kernel_function -> t_fct option

val key_visible : t_fct -> PdgIndex.Key.t -> bool

(** Useful mainly if there has been some Pdg.Top *)
val kf_visible : t_proj -> Cil_types.kernel_function -> bool

(*
Local Variables:
compile-command: "make -C ../.."
End:
*)
