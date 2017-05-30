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

(** Main numeric values of EVA. *)

(** Abstract values built over Cvalue.V *)
module CVal : Abstract_value.Internal with type t = Cvalue.V.t

(** Key for cvalues. *)
val cvalue_key : CVal.t Abstract_value.key

(** Dummy interval: no forward nor backward propagations.
    [None] is top. *)
module Interval : Abstract_value.Internal with type t = Ival.t option

(** Key for intervals. *)
val interval_key : Interval.t Abstract_value.key

(*
Local Variables:
compile-command: "make -C ../../../.."
End:
*)
