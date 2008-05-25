(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2008                                               *)
(*    CEA (Commissariat à l'Énergie Atomique)                             *)
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

(* $Id: state_set.mli,v 1.5 2008/04/01 09:25:22 uid568 Exp $ *)

type t
exception Unchanged
val pretty : Format.formatter -> t -> unit
val add : Relations_type.Model.t -> t -> t
val fold : (Relations_type.Model.t -> 'a -> 'a) -> t -> 'a -> 'a
val iter : (Relations_type.Model.t -> unit) -> t -> unit
val merge_into : t -> t -> t
val join : t -> Relations_type.Model.t
val exists : (Relations_type.Model.t -> bool) -> t -> bool
val filter : (Relations_type.Model.t -> bool) -> t -> t
val is_empty : t -> bool
val singleton : Relations_type.Model.t -> t
val cardinal : t -> int
val empty : t
val length : t -> int

(*
Local Variables:
compile-command: "LC_ALL=C make -C ../.. -j"
End:
*)
