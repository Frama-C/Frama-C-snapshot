(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2016                                               *)
(*    CEA (Commissariat à l'énergie atomique et aux énergies              *)
(*         alternatives)                                                  *)
(*                                                                        *)
(*  All rights reserved.                                                  *)
(*  Contact CEA LIST for licensing.                                       *)
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
