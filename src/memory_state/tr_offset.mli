(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2011                                               *)
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

(** Undocumented. 
    Do not use this module if you don't know what you are doing. *)

(* [JS 2011/10/03] To the authors/users of this module: please document it. *)

type t =
    Set of Ival.O.t
  | Interval of Abstract_interp.Int.t * Abstract_interp.Int.t *
      Abstract_interp.Int.t
  | Imprecise of Abstract_interp.Int.t * Abstract_interp.Int.t
exception Unbounded
val empty : t
val reduce_ival_by_bound :
  Ival.tt -> My_bigint.t -> Base.validity -> bool * (bool * t)
val filter_by_bound_for_reading :
  with_alarms:CilE.warn_mode -> Ival.tt -> My_bigint.t -> Base.validity -> t
val filter_by_bound_for_writing :
  exact:bool ->
  with_alarms:CilE.warn_mode ->
  Ival.tt -> My_bigint.t -> Base.validity -> bool * t

(*
Local Variables:
compile-command: "make -C ../.."
End:
*)
