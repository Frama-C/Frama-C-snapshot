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

include Datatype.S

(** Key for the first map : from Base.t to Ival.Widen_Hints.t *)
type var_key = Default | All | VarKey of Base.t

(** an [empty] set of hints *)
val empty : t

(** a [default] set of hints *)
val default : t

(** add a set of hints for a [stmt, var], [Default] or [All] (stmts, keys) *)
val add_num_hints:
  Cil_types.stmt option -> var_key -> Ival.Widen_Hints.t -> t -> t

(** add a set of Base for a [stmt] *)
val add_var_hints : Cil_types.stmt -> Base.Set.t -> t -> t

(** widen hints from a [Cil_types.stmt, Base] *)
val hints_from_keys :
  Cil_types.stmt -> t ->
  Base.Set.t * (Base.t -> Locations.Location_Bytes.widen_hint)

(*
Local Variables:
compile-command: "make -C ../.."
End:
*)
