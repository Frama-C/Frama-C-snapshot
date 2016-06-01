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

(* This file is generated by Why3's Coq-realize driver *)
(* Beware! Only edit allowed sections below    *)
Require Import BuiltIn.
Require Reals.R_sqrt.
Require BuiltIn.
Require int.Int.
Require int.Abs.
Require real.Real.
Require real.RealInfix.
Require real.Square.

Require Import RIneq.

(* Why3 goal *)
Lemma abs_def : forall (x:Z), ((0%Z <= x)%Z ->
  ((ZArith.BinInt.Z.abs x) = x)) /\ ((~ (0%Z <= x)%Z) ->
  ((ZArith.BinInt.Z.abs x) = (-x)%Z)).
Proof.
exact int.Abs.abs_def.
Qed.

(* Why3 goal *)
Lemma sqrt_lin1 : forall (x:R), (1%R < x)%R -> ((Reals.R_sqrt.sqrt x) < x)%R.
Proof.
intros x h1.
refine (Reals.R_sqrt.sqrt_less _ _ h1).
apply (Rle_trans 0 1 x Rle_0_1)%R.
exact (Rlt_le _ _ h1).
Qed.

(* Why3 goal *)
Lemma sqrt_lin0 : forall (x:R), ((0%R < x)%R /\ (x < 1%R)%R) ->
  (x < (Reals.R_sqrt.sqrt x))%R.
Proof.
intros x (h1,h2).
exact (Reals.R_sqrt.sqrt_more x h1 h2).
Qed.

(* Why3 goal *)
Lemma sqrt_0 : ((Reals.R_sqrt.sqrt 0%R) = 0%R).
exact Reals.R_sqrt.sqrt_0.
Qed.

(* Why3 goal *)
Lemma sqrt_1 : ((Reals.R_sqrt.sqrt 1%R) = 1%R).
exact Reals.R_sqrt.sqrt_1.
Qed.

