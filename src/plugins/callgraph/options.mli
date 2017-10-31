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

include Plugin.S

val name: string
module Filename: Parameter_sig.String
module Roots: Parameter_sig.Kernel_function_set
module Service_roots: Parameter_sig.Kernel_function_set
module Function_pointers: Parameter_sig.Bool
module Uncalled: Parameter_sig.Bool
module Uncalled_leaf: Parameter_sig.Bool
module Services: Parameter_sig.Bool

val dump: (out_channel -> 'a -> unit) -> 'a -> unit
(** dump the given value into [Filename.get ()] by using [output] *)

(*
Local Variables:
compile-command: "make -C ../../.."
End:
*)
