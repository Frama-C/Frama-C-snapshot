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

(** Computations of Def points. Some of the functions of this module are
    registered in module {!Db.Scope}. *)

val compute_with_def_type_zone:
  Cil_types.kernel_function -> Cil_types.stmt -> Locations.Zone.t ->
  ((bool * bool) Cil_datatype.Stmt.Map.t * Locations.Zone.t option) option
(** This function is similar to {!Db.Scope.get_defs_with_type}, except
    that it receives a zone as argument, instead of an l-value *)
