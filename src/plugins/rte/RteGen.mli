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

(** Consult internal plug-in documentation for more details *)

(** Flags for filtering Alarms *)
module Flags : module type of Flags

(** RTE Generator Status & Emitters *)
module Generator : module type of Generator

(** Visitors to iterate over Alarms and/or generate Code-Annotations *)
module Visit : sig
  open Cil_types

  val annotate: ?flags:Flags.t -> kernel_function -> unit

  val get_annotations_kf:
    ?flags:Flags.t -> kernel_function -> code_annotation list

  val get_annotations_stmt:
    ?flags:Flags.t -> kernel_function -> stmt -> code_annotation list

  val get_annotations_exp:
    ?flags:Flags.t -> kernel_function -> stmt -> exp -> code_annotation list

  val get_annotations_lval:
    ?flags:Flags.t -> kernel_function -> stmt -> lval -> code_annotation list

  type on_alarm = kernel_function -> stmt -> invalid:bool -> Alarms.alarm -> unit
  type 'a iterator = ?flags:Flags.t -> on_alarm ->
    Kernel_function.t -> Cil_types.stmt -> 'a -> unit
  val iter_lval : lval iterator
  val iter_exp : exp iterator
  val iter_instr : instr iterator
  val iter_stmt : stmt iterator
  val register :
    Emitter.t -> kernel_function -> stmt -> invalid:bool -> Alarms.alarm ->
    code_annotation * bool
end
