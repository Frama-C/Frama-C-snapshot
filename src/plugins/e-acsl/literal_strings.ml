(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2018                                               *)
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

open Cil_types

let strings
    : varinfo Datatype.String.Hashtbl.t
    = Datatype.String.Hashtbl.create 16

let reset () = Datatype.String.Hashtbl.clear strings

let is_empty () = Datatype.String.Hashtbl.length strings = 0

let add = Datatype.String.Hashtbl.add strings
let find = Datatype.String.Hashtbl.find strings
let fold f = Datatype.String.Hashtbl.fold_sorted f strings

(*
Local Variables:
compile-command: "make"
End:
*)
