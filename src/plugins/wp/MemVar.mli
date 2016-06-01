(**************************************************************************)
(*                                                                        *)
(*  This file is part of WP plug-in of Frama-C.                           *)
(*                                                                        *)
(*  Copyright (C) 2007-2016                                               *)
(*    CEA (Commissariat a l'energie atomique et aux energies              *)
(*         alternatives)                                                  *)
(*                                                                        *)
(*  All rights reserved.                                                  *)
(*  Contact CEA LIST for licensing.                                       *)
(**************************************************************************)

(* -------------------------------------------------------------------------- *)
(* --- No-Aliasing Memory Model                                           --- *)
(* -------------------------------------------------------------------------- *)

open Cil_types

type param = NotUsed | ByValue | ByRef | InContext | InHeap

module type VarUsage =
sig
  val datatype : string
  val param : varinfo -> param
  val separation : unit -> Separation.clause
end

module Make(V : VarUsage)(M : Memory.Model) : Memory.Model
