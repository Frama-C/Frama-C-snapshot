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

exception Cannot_fold

open PdgTypes
(** Types data_state and Node.t come froms this module *)

val make : PdgTypes.LocInfo.t -> Locations.Zone.t -> data_state
val empty : data_state
val bottom: data_state

val add_loc_node :
  data_state -> ?initializing:bool -> exact:bool -> Locations.Zone.t -> Node.t -> data_state
val add_init_state_input :
  data_state -> Locations.Zone.t -> Node.t -> data_state


(** Kind of 'join' of the two states
    but test before if the new state is included in ~old.
   @return (true, old U new) if the result is a new state,
           (false, old) if new is included in old. *)
val test_and_merge :
  old:data_state -> data_state -> bool * data_state

(** @raise Cannot_fold if the state is Top *)
val get_loc_nodes :
  data_state -> Locations.Zone.t -> (Node.t * Locations.Zone.t option) list * Locations.Zone.t option

val pretty : Format.formatter -> data_state -> unit

(* ~~~~~~~~~~~~~~~~~~~ *)

type states = data_state Cil_datatype.Stmt.Hashtbl.t

val store_init_state : states -> data_state -> unit
val store_last_state : states -> data_state -> unit

val get_init_state : states -> data_state
val get_stmt_state : states -> Cil_types.stmt -> data_state
val get_last_state : states -> data_state
