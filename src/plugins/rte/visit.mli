(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2019                                               *)
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

(* --- Synchronized with RteGen.mli --- *)

open Cil_types

(** {2 RTE Generator API} *)

(** Annotate kernel-function with respect to options
    and current generator status. *)
val annotate: ?flags:Flags.t -> kernel_function -> unit

(** Returns annotations associated to alarms {i without} registering them. *)
val get_annotations_kf:
  ?flags:Flags.t -> kernel_function -> code_annotation list

(** Returns annotations associated to alarms {i without} registering them. *)
val get_annotations_stmt:
  ?flags:Flags.t -> kernel_function -> stmt -> code_annotation list

(** Returns annotations associated to alarms {i without} registering them. *)
val get_annotations_exp:
  ?flags:Flags.t -> kernel_function -> stmt -> exp -> code_annotation list

(** Returns annotations associated to alarms {i without} registering them. *)
val get_annotations_lval:
  ?flags:Flags.t -> kernel_function -> stmt -> lval -> code_annotation list

(** {2 Low-Level RTE Iterators}

    RTE Iterators allow to traverse a Cil AST fragment (stmt, expr, l-value)
    and reveal its potential Alarms. Each alarm will be presented to a callback
    with type [on_alarm], that you can use in turn to generate an annotation
    or perform any other treatment.

    Flags can be used to select which alarm categories to visit, with
    defaults derived from Kernel and RTE plug-in parameters.
*)

(** Alarm callback.

    The [on_alarm kf stmt ~invalid alarm] callback is invoked on each
    alarm visited by an RTE iterator, provided it fits the selected categories.
    The [kf] and [stmt] designates the statement originating the alarm,
    while [~invalid:true] is set when the alarm trivially evaluates to false.
    In this later case, the corresponding annotation shall be assigned
    the status [False_if_reachable].

*)
type on_alarm = kernel_function -> stmt -> invalid:bool -> Alarms.alarm -> unit

(** Type of low-level iterators visiting an element ['a] of the AST *)
type 'a iterator = ?flags:Flags.t -> on_alarm ->
  Kernel_function.t -> Cil_types.stmt -> 'a -> unit

val iter_lval : lval iterator
val iter_exp : exp iterator
val iter_instr : instr iterator
val iter_stmt : stmt iterator

(** {2 Alarm Helpers} *)

(** Returns a [False_if_reachable] status when invalid. *)
val status : invalid:bool -> Property_status.emitted_status option

(** Registers and returns the annotation associated with the alarm,
    and a boolean flag indicating whether it has been freshly generated
    or not. Simple wrapper over [Alarms.register]. *)
val register :
  Emitter.t ->
  kernel_function -> stmt -> invalid:bool -> Alarms.alarm ->
  code_annotation * bool

(*
Local Variables:
compile-command: "make -C ../../.."
End:
*)
