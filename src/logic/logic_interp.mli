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

(** Undocumented. 
    Do not use this module if you don't know what you are doing. *)

(* [JS 2011/10/03] To the authors/users of this module: please document it. *)

(* TODO: remove the module Properties from Db and export directly the
   functions from here. *)

open Cil_types

module To_zone : sig
  val  not_yet_implemented : string ref
end

(* [JS 2011/06/09] seem to be unused.
   Be careful: require to call Kernel_function.set_spec if the new funspec is
   put into a kernel function. *)
(** returns a copy of the spec in which all formals in an ensures clause
    are guarded by an \at(x,Old). *)
val formals_in_ensures: kernel_function -> funspec

exception Error of location * string

(*
Local Variables:
compile-command: "make -C ../.."
End:
*)
