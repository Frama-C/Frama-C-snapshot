(**************************************************************************)
(*                                                                        *)
(*  This file is part of WP plug-in of Frama-C.                           *)
(*                                                                        *)
(*  Copyright (C) 2007-2013                                               *)
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
(* --- Mathematics Library for Coq                                        --- *)
(* -------------------------------------------------------------------------- *)

Require Import Qedlib.
Require Import ZArith.
Require Import Reals.

Definition abs_int (x : Z) := if (Zlt_bool x 0)%R then (-x)%Z else x.
Definition abs_real (x : R) := if (Rlt_bool x 0)%Z then (-x)%R else x.
Definition max_int (x : Z) (y:Z) := if (Zlt_bool x y)%Z then y else x.
Definition min_int (x : Z) (y:Z) := if (Zlt_bool x y)%Z then x else y.

