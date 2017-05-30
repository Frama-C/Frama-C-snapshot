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

(** Functions related to the backward propagation of the value of formals
    at the end of a call. When possible, this value is propagated to
    the actual parameter. *)


val written_formals: Cil_types.kernel_function -> Cil_datatype.Varinfo.Set.t
(** [written_formals kf] is an over-approximation of the formals of [kf]
    which may be internally overwritten by [kf] during its call. *)


val safe_argument: Cil_types.exp -> bool
(** [safe_argument e] returns [true] if [e] (which is supposed to be
    an actual parameter) is guaranteed to evaluate in the same way before and
    after the call. *)
