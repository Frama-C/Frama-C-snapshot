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
(* --- C-Float Library for Coq                                            --- *)
(* -------------------------------------------------------------------------- *)

Require Import Reals.

(* C-Float Rounding *)

Inductive rounding_mode  :=
  | Up
  | Down
  | ToZero
  | NearestTiesToAway
  | NearestTiesToEven.
  
Parameter round_float: rounding_mode -> R -> R.
Parameter round_double: rounding_mode -> R -> R.

(* C-Float Conversions *)

Definition to_float32 := round_float NearestTiesToEven.
Definition to_float64 := round_double NearestTiesToEven.
Definition is_float32 x := (to_float32 x = x).
Definition is_float64 x := (to_float64 x = x).
Parameter is_finite32 : R -> Prop.
Parameter is_finite64 : R -> Prop.

Hypothesis to_float_is_finite_32 : forall x, is_finite32(to_float32 x).
Hypothesis to_float_is_finite_64 : forall x, is_finite64(to_float64 x).

(* C-Float Conversions are projections *)

Hypothesis proj_float32 : forall x, to_float32(to_float32 x) = to_float32 x.
Hypothesis proj_float64 : forall x, to_float64(to_float64 x) = to_float64 x.

(* C-Float Arithmetics *)

Definition add_float32 x y := to_float32 (x+y)%R.
Definition add_float64 x y := to_float64 (x+y)%R.

Definition sub_float32 x y := to_float32 (x-y)%R.
Definition sub_float64 x y := to_float64 (x-y)%R.

Definition mul_float32 x y := to_float32 (x*y)%R.
Definition mul_float64 x y := to_float64 (x*y)%R.

Definition div_float32 x y := to_float32 (x/y)%R.
Definition div_float64 x y := to_float64 (x/y)%R.

(* Real Arithmetics *)

Definition ropp x := (-x)%R.
Definition radd x y := (x+y)%R.
Definition rsub x y := (x-y)%R.
Definition rmul x y := (x*y)%R.
Definition rdiv x y := (x/y)%R.


