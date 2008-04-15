(*
 * The Why certification tool
 * Copyright (C) 2002 Jean-Christophe FILLIATRE
 * 
 * This software is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public
 * License version 2, as published by the Free Software Foundation.
 * 
 * This software is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
 * 
 * See the GNU General Public License version 2 for more details
 * (enclosed in the file GPL).
 *)

(* $Id: WhyPrelude.v,v 1.4 2006/11/02 09:18:20 hubert Exp $ *)

Require Export WhyCoqCompat.

Require Export WhyTuples.
Require Export WhyInt.
Require Export WhyBool.
Require Export WhyArrays.
Require Export WhyPermut.
Require Export WhySorted.
Require Export WhyTactics.
Require Export WhyExn.
Require Export WhyLemmas.

Definition ref_set 
  (T:Set) (v:T) (x0:T) : sig_2 _ unit (fun x1 _ => x1=v)
  := exist_2 (fun x1 _ => x1=v) v tt (refl_equal v).
Implicit Arguments ref_set [T].

Definition div_int_ :
  forall (x: Z), forall (y: Z), forall (_: y <> 0%Z),
  (sig_1 Z (fun (result: Z)  => (result = ((Zdiv x y))))).
intros; exists (x/y)%Z; auto.
Defined.

Open Scope Z_scope.

Definition array_get :
  forall (A: Set), forall (i: Z), forall (t: (array A)), forall (_: 0 <=
  i /\ i < (array_length t)),
  (sig_1 A (fun (result: A)  => (result = (access t i)))).
intros; exists (access t i); auto.
Defined.
Implicit Arguments array_get [A].

Definition array_set :
  forall (A: Set), forall (i: Z), forall (v: A), forall (t: (array A)),
  forall (_: 0 <= i /\ i < (array_length t)),
  (sig_2 (array A) unit
   (fun (t0: (array A)) (result: unit)  => (t0 = (update t i v)))).
intros; exists (update t i v) tt; auto.
Defined.
Implicit Arguments array_set [A].

(*Why logic*) Definition lt_int_bool : Z -> Z -> bool.
Admitted.

(*Why logic*) Definition le_int_bool : Z -> Z -> bool.
Admitted.

(*Why logic*) Definition gt_int_bool : Z -> Z -> bool.
Admitted.

(*Why logic*) Definition ge_int_bool : Z -> Z -> bool.
Admitted.

(*Why logic*) Definition eq_int_bool : Z -> Z -> bool.
Admitted.

(*Why logic*) Definition neq_int_bool : Z -> Z -> bool.
Admitted.

(*Why axiom*) Lemma lt_int_bool_axiom :
  (forall (x:Z), (forall (y:Z), ((lt_int_bool x y) = true <-> x < y))).
Admitted.

(*Why axiom*) Lemma le_int_bool_axiom :
  (forall (x:Z), (forall (y:Z), ((le_int_bool x y) = true <-> x <= y))).
Admitted.

(*Why axiom*) Lemma gt_int_bool_axiom :
  (forall (x:Z), (forall (y:Z), ((gt_int_bool x y) = true <-> x > y))).
Admitted.

(*Why axiom*) Lemma ge_int_bool_axiom :
  (forall (x:Z), (forall (y:Z), ((ge_int_bool x y) = true <-> x >= y))).
Admitted.

(*Why axiom*) Lemma eq_int_bool_axiom :
  (forall (x:Z), (forall (y:Z), ((eq_int_bool x y) = true <-> x = y))).
Admitted.

(*Why axiom*) Lemma neq_int_bool_axiom :
  (forall (x:Z), (forall (y:Z), ((neq_int_bool x y) = true <-> x <> y))).
Admitted.

