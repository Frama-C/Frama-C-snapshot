(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2015                                               *)
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

(** Manages information related to possible exceptions thrown by each
    function in the AST. *)

(** returns the set of exceptions that a given kernel function might throw. *)
val get_kf_exn: Kernel_function.t -> Cil_datatype.Typ.Set.t

(** computes the information if not already done. *)
val compute: unit -> unit

(**/**)
(** internal state of the module. *)
val self_fun: State.t
val self_stmt: State.t
(**/**)

(** transforms functions that may throw into functions returning a union type
    composed of the normal return or one of the exceptions. *)
val remove_exn: Cil_types.file -> unit

(** category of the code transformation above. *)
val transform_category: File.code_transformation_category
