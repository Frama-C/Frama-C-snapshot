(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2017                                               *)
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

(* TODO: This .mli exists mainly to avoid problems with 'make -j'. Most
   of those functions are only exported to be registered in Register,
   and this should be done here instead. *)

open SlicingInternals

val create_slice : Kernel_function.t -> fct_slice
val remove_ff : fct_slice -> unit
val remove_uncalled_slices : unit -> unit
val merge_slices : fct_slice -> fct_slice -> bool -> fct_slice
val split_slice : fct_slice -> fct_slice list
val get_slices : Kernel_function.t -> fct_slice list
val get_slice_callers : fct_slice -> fct_slice list
val add_filter : criterion -> unit
val add_fct_src_filter : fct_info -> fct_user_crit -> unit
val add_fct_ff_filter : fct_slice -> fct_user_crit -> unit
val print_proj_worklist : Format.formatter -> unit
val print_project_and_worklist : Format.formatter -> unit
val pretty_slice : Format.formatter -> fct_slice -> unit
val apply_next_action : unit -> unit
val is_request_empty : unit -> bool
val apply_all_actions : unit -> unit
