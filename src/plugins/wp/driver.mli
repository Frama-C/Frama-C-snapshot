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
(* --- Driver for External Files                                          --- *)
(* -------------------------------------------------------------------------- *)

val load_driver : unit -> LogicBuiltins.driver
(** Memoized loading of drivers according to current
    WP options. Finally sets [LogicBuiltins.driver] and returns it. *)
