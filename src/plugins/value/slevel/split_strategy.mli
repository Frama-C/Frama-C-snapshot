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

type split_strategy =
  | NoSplit
  | SplitAuto
  | SplitEqList of Datatype.Integer.t list
  | FullSplit

include Datatype.S_with_collections with type t = split_strategy

exception ParseFailure of string

val of_string: string -> t
(* @raise ParseFailure *)

val to_string: t -> string
