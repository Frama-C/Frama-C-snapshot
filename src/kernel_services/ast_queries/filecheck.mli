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

(** This file performs various consistency checks over a cil file.
    Code may vary depending on current development of the kernel and/or
    identified bugs. *)

class check: ?is_normalized:bool -> string -> Visitor.frama_c_visitor
  (** visitor that performs various consistency checks over the AST.
      The string argument will be used in the error message in case of
      inconsistency, in order to trace the issue. [is_normalized] defaults to
      [true]. Some checks are deactivated when it is set [false].  *)



(*
Local Variables:
compile-command: "make -C ../.."
End:
*)
