(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2010                                               *)
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

(** An extension of {!Type} with Frama-C type.

    @plugin development guide *)

(* ****************************************************************************)
(** {2 Frama-C types} *)
(* ****************************************************************************)

val big_int : Big_int.big_int Type.t
val stmt : Cil_types.stmt Type.t
val kinstr : Cil_types.kinstr Type.t
val string_set : Cilutil.StringSet.t Type.t
val stmt_set : Cilutil.StmtSet.t Type.t

val localisation : Db_types.localisation Type.t
val kernel_function: Db_types.kernel_function Type.t

(* ****************************************************************************)
(** {3 Types without pretty-printer}

    Values of these types cannot be used as arguments of a journalized
    function, except if you can ensure that they are returned by another
    journalized function. *)
(* ****************************************************************************)

val varinfo: Cil_types.varinfo Type.t
val lval : Cil_types.lval Type.t
val cil_file: Cil_types.file Type.t
val cabs_file: Cabs.file Type.t

(* ****************************************************************************)
(** {3 Dummy values for registering type values}

    The following values may be used for registering new type value.
    They **must not** be used for any other use. *)
(* ****************************************************************************)

val lexing_pos_dummy: Lexing.position
  (** @since Boron-20100401 *)

val varinfo_dummy: Cil_types.varinfo

(*
Local Variables:
compile-command: "make -C ../.."
End:
*)
