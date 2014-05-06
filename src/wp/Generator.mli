(**************************************************************************)
(*                                                                        *)
(*  This file is part of WP plug-in of Frama-C.                           *)
(*                                                                        *)
(*  Copyright (C) 2007-2014                                               *)
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

(* -------------------------------------------------------------------------- *)
(* --- WP Computer (main entry points)                                    --- *)
(* -------------------------------------------------------------------------- *)

class type computer =
object
  method lemma : bool
  method add_strategy : WpStrategy.strategy -> unit
  method add_lemma : LogicUsage.logic_lemma -> unit
  method compute : Wpo.t Bag.t
end

type functions =
  | F_All
  | F_List of string list
  | F_Skip of string list

val compute_ip : computer -> Property.t -> Wpo.t Bag.t
val compute_call : computer -> Cil_types.stmt -> Wpo.t Bag.t
val compute_kf : computer -> 
  ?kf:Kernel_function.t ->
  ?bhv:string list ->
  ?prop:string list ->
  unit -> Wpo.t Bag.t
val compute_selection : computer -> 
  ?fct:functions ->
  ?bhv:string list ->
  ?prop:string list ->
  unit -> Wpo.t Bag.t

