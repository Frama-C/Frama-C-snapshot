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

type t =
  | Approximation of string
  | Imprecision of string
  | Costly of string
  | Unsoundness of string

type emitter = unit

let emit _emitter msg =
  match msg with
  | Imprecision _ -> () (* Only for debug purposes *)
  | Approximation str
  | Costly str
  | Unsoundness str ->
    Kernel.feedback ~current:true ~once:true "%s" str
;;

let register _name = ()

let emit_approximation emitter = Format.kfprintf (fun _fmt ->
  let str = Format.flush_str_formatter() in
  emit emitter (Approximation str)) Format.str_formatter
;;

let emit_costly emitter = Format.kfprintf (fun _fmt ->
  let str = Format.flush_str_formatter() in
  emit emitter (Costly str)) Format.str_formatter
;;

let emit_imprecision emitter str =
  emit emitter (Imprecision str)
;;


(*
Local Variables:
compile-command: "make -C ../../.."
End:
*)
