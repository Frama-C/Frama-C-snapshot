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

(** This file performs various consistency checks over a cil file.
    Code may vary depending on current development of the kernel and/or
    identified bugs. *)

val check_ast: ?is_normalized:bool -> ?ast:Cil_types.file -> string -> unit
(** Visits the given AST (defaults to the AST of the current project)
    to check whether it is consistent. Use a non-default [ast] argument
    at your own risks.

    Note that the check is only partial.
    @since Aluminium-20160501
    @modify Silicon-20161101 adds optional ast argument
*)

module type Extensible_checker =
sig
  class check: ?is_normalized:bool -> string -> Visitor.frama_c_visitor
end

(** Allows to register an extension to current checks. The function
    will be given as input the current state of the checker.

    @since Phosphorus-20170501-beta1
*)
val extend_checker:
  ((module Extensible_checker) -> (module Extensible_checker)) -> unit

(*
Local Variables:
compile-command: "make -C ../../.."
End:
*)
