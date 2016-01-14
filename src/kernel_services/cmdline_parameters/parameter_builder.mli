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

(** Functors for implementing new command line options. *)

(* ************************************************************************* *)
(** {2 Kernel use only} *)
(* ************************************************************************* *)

module Make
  (P: sig
    val shortname: string
    val parameters: (string, Typed_parameter.t list) Hashtbl.t
    module L: sig 
      val abort: ('a,'b) Log.pretty_aborter
      val warning: 'a Log.pretty_printer
    end
    val messages_group: Cmdline.Group.t
  end):
  Parameter_sig.Builder

(* ************************************************************************* *)
(** {2 Internal use only} *)
(* ************************************************************************* *)

open Cil_types

val find_kf_by_name: (string -> kernel_function) ref
val find_kf_def_by_name: (string -> kernel_function) ref
val kf_category: (unit -> kernel_function Parameter_category.t) ref
val kf_def_category: (unit -> kernel_function Parameter_category.t) ref
val kf_string_category: (unit -> string Parameter_category.t) ref
val fundec_category: (unit -> fundec Parameter_category.t) ref
val force_ast_compute: (unit -> unit) ref

(*
Local Variables:
compile-command: "make -C ../../.."
End:
*)
