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

type origin =
  | Misalign_read of Abstract_interp.LocationSetLattice.t
  | Leaf of Abstract_interp.LocationSetLattice.t
  | Merge of Abstract_interp.LocationSetLattice.t
  | Arith of Abstract_interp.LocationSetLattice.t
  | Well
  | Unknown

include Datatype.S with type t = origin

val top: t
val is_top: t -> bool

val bottom: t

val join: t -> t -> t
val meet: t -> t -> t
val narrow: t -> t -> t

val is_included: t -> t -> bool
val is_included_exn: t -> t -> unit

(*
Local Variables:
compile-command: "make -C ../.."
End:
*)
