(**************************************************************************)
(*                                                                        *)
(*  This file is part of WP plug-in of Frama-C.                           *)
(*                                                                        *)
(*  Copyright (C) 2007-2018                                               *)
(*    CEA (Commissariat a l'energie atomique et aux energies              *)
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

(** Built-in Havoc Tactical (auto-registered) *)

open Tactical
open Strategy

module Havoc :
sig
  val field : selection field
  val tactical : tactical
  val strategy :
    ?priority:float -> havoc:selection -> addr:selection -> strategy
end

module Separated :
sig
  val tactical : tactical
  val strategy : ?priority:float -> selection -> strategy
end

module Validity :
sig
  val tactical : tactical
  val strategy : ?priority:float -> selection -> strategy
end
  
