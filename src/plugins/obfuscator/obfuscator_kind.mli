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

type k =
  | Behavior
  | Enum
  | Field
  | Formal_var
  | Function
  | Global_var
  | Label
  | Literal_string
  | Local_var
  | Logic_var
  | Predicate
  | Type

include Datatype.S_with_collections with type t = k
val prefix: t -> string

(*
Local Variables:
compile-command: "make -C ../../.."
End:
*)
