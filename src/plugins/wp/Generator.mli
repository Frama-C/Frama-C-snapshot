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
  | F_List of Cil_datatype.Kf.Set.t
  | F_Skip of Cil_datatype.Kf.Set.t

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

