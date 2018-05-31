(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2018                                               *)
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

module type S = sig
  val is_computed: Kernel_function.t -> bool
  val set: Kernel_function.t -> bool -> unit
  val accessor: Db.RteGen.status_accessor
end

module Initialized: S
module Mem_access: S
module Pointer_call: S
module Div_mod: S
module Shift: S
module Signed_overflow: S
module Signed_downcast: S
module Unsigned_overflow: S
module Unsigned_downcast: S
module Float_to_int: S
module Finite_float: S
module Called_precond: S

val emitter: Emitter.t

val all_statuses: Db.RteGen.status_accessor list

(*
  Local Variables:
  compile-command: "make -C ../../.."
  End:
 *)
