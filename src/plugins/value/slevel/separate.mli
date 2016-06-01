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

val prologue : unit -> unit
val filter_if :
  Cil_types.stmt ->
  'a Dataflow2.guardaction * 'b Dataflow2.guardaction ->
  'a Dataflow2.guardaction * 'b Dataflow2.guardaction
val epilogue : unit -> unit
