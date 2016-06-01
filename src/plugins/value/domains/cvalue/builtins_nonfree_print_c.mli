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

(** Translate a Value state into a bunch of C assertions *)

(** This file is experimental, and partly tuned to Csmith programs. In
    particular, it might not follow your machedp, or fail to translate
    some variables. Use at your own risk *)

val pretty_state_as_c_assert: Cvalue.Model.t Pretty_utils.formatter

val pretty_state_as_c_assignments:  Cvalue.Model.t Pretty_utils.formatter
