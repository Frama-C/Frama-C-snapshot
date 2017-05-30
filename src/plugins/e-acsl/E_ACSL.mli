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

(** E-ACSL. *)

open Cil_types

module Error: sig
  exception Typing_error of string
  exception Not_yet of string
end

module Translate: sig
  exception No_simple_translation of term
  val term_to_exp: typ option -> term -> exp
(** @raise New_typing_error when the given term cannot be typed (something wrong
    happends with this term)
    @raise Not_yet when the given term contains an unsupported construct.
    @raise No_simple_translation when the given term cannot be translated into
    a single expression. *)
end

(** No function is directly exported: they are dynamically registered. *)

(*
Local Variables:
compile-command: "make"
End:
*)
