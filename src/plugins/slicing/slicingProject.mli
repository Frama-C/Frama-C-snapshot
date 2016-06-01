(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2016                                               *)
(*    CEA (Commissariat à l'énergie atomique et aux énergies              *)
(*         alternatives)                                                  *)
(*                                                                        *)
(*  All rights reserved.                                                  *)
(*  Contact CEA LIST for licensing.                                       *)
(*                                                                        *)
(**************************************************************************)

(* TODO: This .mli exists mainly to avoid problems with 'make -j'. Most
   of those functions are only exported to be registered in Register,
   and this should be done here instead. *)

open SlicingInternals

val mk_project : string -> project
val get_name : project -> string
val create_slice : project -> Kernel_function.t -> fct_slice
val remove_ff : project -> fct_slice -> unit
val remove_uncalled_slices : project -> unit
val merge_slices : project -> fct_slice -> fct_slice -> bool -> fct_slice
val split_slice : project -> fct_slice -> fct_slice list
val get_slices : project -> Kernel_function.t -> fct_slice list
val get_slice_callers : fct_slice -> fct_slice list
val add_filter : project -> criterion -> unit
val add_fct_src_filter : project -> fct_info -> fct_user_crit -> unit
val add_fct_ff_filter : project -> fct_slice -> fct_user_crit -> unit
val print_proj_worklist : Format.formatter -> project -> unit
val print_project_and_worklist : Format.formatter -> project -> unit
val pretty_slice : Format.formatter -> fct_slice -> unit
val apply_next_action : project -> unit
val is_request_empty : project -> bool
val apply_all_actions : project -> unit
