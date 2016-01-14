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

module Make
  (C: sig
    val name: string
    val dump: unit -> unit
    val compute: unit -> unit
    type t
    val ty: t Type.t
    val get: unit -> t
  end) =
struct
  let name = "Callgraph." ^ C.name
  let unit_unit = Datatype.func Datatype.unit Datatype.unit
  let dump = Journal.register (name ^ ".dump") unit_unit C.dump
  let compute = Journal.register (name ^ ".compute") unit_unit C.compute
  let get =
    Journal.register (name ^ ".get") (Datatype.func Datatype.unit C.ty) C.get
end

(*
Local Variables:
compile-command: "make -C ../.."
End:
*)
