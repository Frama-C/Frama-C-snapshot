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

(** A builtin takes the state and a list of values for the arguments, and
    returns the offsetmap of the return value (None if bottom), and a boolean
    indicating the possibility of alarms.  *)
type str_builtin_sig =
  Cvalue.Model.t -> Cvalue.V.t list -> Cvalue.V_Offsetmap.t option * bool

val frama_c_strlen_wrapper: str_builtin_sig
val frama_c_wcslen_wrapper: str_builtin_sig
val frama_c_strchr_wrapper: str_builtin_sig
val frama_c_wcschr_wrapper: str_builtin_sig
