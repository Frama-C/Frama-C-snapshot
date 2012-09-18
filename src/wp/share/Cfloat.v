(**************************************************************************)
(*                                                                        *)
(*  This file is part of WP plug-in of Frama-C.                           *)
(*                                                                        *)
(*  Copyright (C) 2007-2012                                               *)
(*    CEA (Commissariat a l'énergie atomique et aux énergies              *)
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

(* C-Float Conversions *)

Parameter to_float16  : R -> R.
Parameter to_float32  : R -> R.
Parameter to_float64  : R -> R.
Parameter to_float96  : R -> R.
Parameter to_float128 : R -> R.

(* C-Float Conversions are projections *)

Hypothesis proj_float16 : forall x, to_float16(to_float16 x)=to_float16 x.
Hypothesis proj_float32 : forall x, to_float32(to_float32 x)=to_float32 x.
Hypothesis proj_float64 : forall x, to_float64(to_float64 x)=to_float64 x.
Hypothesis proj_float96 : forall x, to_float96(to_float96 x)=to_float96 x.
Hypothesis proj_float128 : forall x, to_float128(to_float128 x)=to_float128 x.

Inductive float_format  :=
  | Single
  | Double
  | Quad.

Inductive rounding_mode  :=
  | Up
  | Down
  | ToZero
  | NearestAway
  | NearestEven.

Inductive sign  :=
  | Positive
  | Negative.
  
Parameter round_double: rounding_mode -> R -> R.
Parameter round_float: rounding_mode -> R -> R.
