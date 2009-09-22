(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2009                                               *)
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

(* $Id: kernel_type.mli,v 1.8 2009-01-19 16:21:01 uid568 Exp $ *)

(** An extension of {!Type} with Frama-C type.

    @plugin development guide *)

(* ****************************************************************************)
(** {2 Frama-C types} *)
(* ****************************************************************************)

val big_int : Big_int.big_int Type.t
val stmt : Cil_types.stmt Type.t
val kinstr : Cil_types.kinstr Type.t
val lval : Cil_types.lval Type.t
val string_set : Cilutil.StringSet.t Type.t

(* NB: no pretty-printer is defined for these types. *)
val cil_file: Cil_types.file Type.t
val cabs_file: Cabs.file Type.t

val localisation : Db_types.localisation Type.t
val varinfo: Cil_types.varinfo Type.t
val varinfo_dummy: Cil_types.varinfo
val kernel_function: Db_types.kernel_function Type.t

(*
Local Variables:
compile-command: "LC_ALL=C make -C ../.. -j"
End:
*)
