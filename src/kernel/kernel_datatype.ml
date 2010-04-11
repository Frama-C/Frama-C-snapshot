(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2010                                               *)
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

open Db_types

module Rooted_Code_Annotation = struct
  include Project.Datatype.Imperative
    (struct 
       type t = Db_types.rooted_code_annotation
       let name = "rooted_code_annotation"
       let copy _ = assert false
     end)
  let compare x y = match x, y with
    | User a, User b -> Cil_datatype.Code_Annotation.compare a b
    | AI(_, a), AI(_, b) -> Cil_datatype.Code_Annotation.compare a b
    | User _, AI _ -> -1
    | AI _, User _ -> 1
  let () = register_comparable ~compare ()
end

module Before_After(A:Project.Datatype.S) = struct
  include Project.Datatype.Imperative
    (struct
       type t = A.t before_after
       let name = Project.Datatype.extend_name "before_after" A.name
       let copy _ = assert false
     end)
  let compare x y = match x, y with
    | Before a, Before b -> A.compare a b
    | After a, After b -> A.compare a b
    | Before _, After _ -> -1
    | After _, Before _ -> 1
  let () = register_comparable ~compare ()
end

module Rooted_Code_Annotation_Before_After =
  Before_After(Rooted_Code_Annotation)

(*
Local Variables:
compile-command: "make -C ../.."
End:
*)
