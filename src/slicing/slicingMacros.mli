(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2014                                               *)
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

(** This .mli exists mainly to facilitate 'make -j'. A lot of the [get_]
    functions below should be inlined, as there is no good reason to treat
    those types as semi-private *)

open SlicingInternals

val str_level_option : level_option -> string
val get_default_level_option : bool -> level_option
val fi_svar : fct_info -> Cil_types.varinfo
val ff_svar : fct_slice -> Cil_types.varinfo
val get_kf_fi : project -> Kernel_function.t -> fct_info
val fold_fi : ('a -> fct_info -> 'a) -> 'a -> project -> 'a
val get_ff_id : fct_slice -> int
val fi_name : fct_info -> string
val ff_name : fct_slice -> string
val f_name : fct_id -> string
val ff_src_name : fct_slice -> string
val get_fi_kf : fct_info -> Cil_types.kernel_function
val get_ff_kf : fct_slice -> Cil_types.kernel_function
val get_pdg_kf : PdgTypes.Pdg.t -> Kernel_function.t
val get_fi_pdg : fct_info -> Db.Pdg.t
val get_ff_pdg : fct_slice -> Db.Pdg.t
val ff_slicing_level : fct_slice -> level_option
val change_fi_slicing_level : fct_info -> level_option -> unit
val change_slicing_level : project -> Kernel_function.t -> int -> unit
val fi_slices : fct_info -> fct_slice list
val equal_fi : fct_info -> fct_info -> bool
val equal_ff : fct_slice -> fct_slice -> bool
val same_ff_call :
  fct_slice * Cil_types.stmt -> fct_slice * Cil_types.stmt -> bool
val is_call_stmt : Cil_types.stmt -> bool
val get_fi_call : project -> Cil_types.stmt -> fct_info option
val is_src_fun_called : project -> Kernel_function.t -> bool
val is_src_fun_visible : project -> Kernel_function.t -> bool
val fi_has_persistent_selection : fct_info -> bool
val has_persistent_selection : project -> Kernel_function.t -> bool
