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

(** Datatypes of some useful kernel types.
    @plugin development guide *)

open Db_types

module Rooted_code_annotation:
  Datatype.S with type t = rooted_code_annotation

module Before_after(A: Datatype.S) :
  Datatype.S with type t = A.t before_after

module Rooted_code_annotation_before_after:
  Datatype.S with type t = rooted_code_annotation before_after

module Kernel_function: sig
  include Datatype.S_with_collections with type t = kernel_function
  val id: t -> int
end

module Localisation: Datatype.S with type t = localisation

(*
Local Variables:
compile-command: "make -C ../.."
End:
*)
