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

(* $Id: Inout.mli,v 1.5 2008-04-01 09:25:20 uid568 Exp $ *)

(** Inputs-outputs computations. *)

(** No function is directly exported: they are registered in:
    - {!Db.Inputs} for computations of non functionnal inputs;
    - {!Db.Outputs} for computations of outputs;
    - {!Db.Operational_inputs} for computation of inout context; and
    - {!Db.Derefs}. *)
