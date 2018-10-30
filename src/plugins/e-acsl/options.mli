(**************************************************************************)
(*                                                                        *)
(*  This file is part of the Frama-C's E-ACSL plug-in.                    *)
(*                                                                        *)
(*  Copyright (C) 2012-2018                                               *)
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

include Plugin.S (** implementation of Log.S for E-ACSL *)

module Check: Parameter_sig.Bool
module Run: Parameter_sig.Bool
module Valid: Parameter_sig.Bool
module Prepare: Parameter_sig.Bool
module Gmp_only: Parameter_sig.Bool
module Full_mmodel: Parameter_sig.Bool
module Project_name: Parameter_sig.String
module Builtins: Parameter_sig.String_set
module Temporal_validity: Parameter_sig.Bool
module Validate_format_strings: Parameter_sig.Bool
module Replace_libc_functions: Parameter_sig.Bool

val parameter_states: State.t list

val must_visit: unit -> bool

val dkey_analysis: category
val dkey_dup: category
val dkey_translation: category
val dkey_typing: category

(*
Local Variables:
compile-command: "make"
End:
*)
