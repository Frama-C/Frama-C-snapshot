(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2011                                               *)
(*    CEA   (Commissariat à l'énergie atomique et aux énergies            *)
(*           alternatives)                                                *)
(*    INRIA (Institut National de Recherche en Informatique et en         *)
(*           Automatique)                                                 *)
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
(*  See the GNU Lesser General Public License version v2.1                *)
(*  for more details (enclosed in the file licenses/LGPLv2.1).            *)
(*                                                                        *)
(**************************************************************************)

exception Cannot_fold

type t_loc = Locations.Zone.t
type t_node = PdgTypes.Node.t
type t = PdgTypes.t_data_state

val make : PdgTypes.LocInfo.t -> Locations.Zone.t -> t
val empty : t

val add_loc_node : t -> exact:bool -> Locations.Zone.t -> t_node -> t
val add_init_state_input : t -> Locations.Zone.t -> t_node -> t


val test_and_merge :
  old:t -> t -> bool * t

(** @raise Cannot_fold if the state is Top *)
val get_loc_nodes :
  t -> Locations.Zone.t -> (t_node * Locations.Zone.t option) list * Locations.Zone.t option

val pretty : Format.formatter -> t -> unit

(* ~~~~~~~~~~~~~~~~~~~ *)

type t_states = t Inthash.t

val store_init_state : t_states -> t -> unit
val store_last_state : t_states -> t -> unit

val get_init_state : t_states -> t
val get_stmt_state : t_states -> Cil_types.stmt -> t
val get_last_state : t_states -> t
