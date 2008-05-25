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

(* $Id: WhyPermut.v,v 1.15 2006/11/02 09:18:20 hubert Exp $ *)

Require Import WhyArrays.
Require Import Omega.

Set Implicit Arguments.
Unset Strict Implicit.

(****************************************************************************)
(*                   Exchange of two elements in an array                   *)
(*                        Definition and properties                         *)
(****************************************************************************)

(* Definition *)

Inductive exchange (A:Set) (t t':array A) (i j:Z) : Prop :=
    exchange_c :
      array_length t = array_length t' ->
      (0 <= i < array_length t)%Z ->
      (0 <= j < array_length t)%Z ->
      access t i = access t' j ->
      access t j = access t' i ->
      (forall k:Z,
         (0 <= k < array_length t)%Z ->
         k <> i -> k <> j -> access t k = access t' k) ->
      exchange t t' i j.
    
(* Properties about exchanges *)

Lemma exchange_1 :
 forall (A:Set) (t:array A) (i j:Z),
   (0 <= i < array_length t)%Z ->
   (0 <= j < array_length t)%Z ->
   access (update (update t i (access t j)) j (access t i)) i =
   access t j.
Proof.
intros A t i j H_i H_j.
AccessStore j i H; WhyArrays; auto with datatypes.
Qed.

Hint Resolve exchange_1 : v62 datatypes.


Lemma exchange_proof :
 forall (A:Set) (t:array A) (i j:Z),
   (0 <= i < array_length t)%Z ->
   (0 <= j < array_length t)%Z ->
   exchange (update (update t i (access t j)) j (access t i)) t i j.
Proof.
intros A t i j H_i H_j.
apply exchange_c; WhyArrays; auto with datatypes.
intros k H_k not_k_i not_k_j.
cut (j <> k); auto with datatypes.
 intro not_j_k.
AccessOther; auto with datatypes.
Qed.

Hint Resolve exchange_proof : v62 datatypes.


Lemma exchange_sym :
 forall (A:Set) (t t':array A) (i j:Z),
   exchange t t' i j -> exchange t' t i j.
Proof.
intros A t t' i j H1.
elim H1; intro eq; rewrite eq; clear H1; intros.
constructor 1; auto with datatypes.
intros.
 rewrite (H3 k); auto with datatypes.
Qed.

Hint Resolve exchange_sym : v62 datatypes.


Lemma exchange_id :
 forall (A:Set) (t t':array A) (i j:Z),
   exchange t t' i j ->
   i = j ->
   forall k:Z, (0 <= k < array_length t)%Z -> access t k = access t' k.
Proof.
intros A t t' i j Hex Heq k Hk.
elim Hex.
 clear Hex.
 intros.
rewrite Heq in H2.
 rewrite Heq in H3.
case (Z_eq_dec k j).
   intro Heq'.
 rewrite Heq'.
 assumption.
  intro Hnoteq.
 apply (H4 k); auto with datatypes.
 rewrite Heq.
 assumption.
Qed.

Hint Resolve exchange_id : v62 datatypes.


Lemma exchange_length :
 forall (A:Set) (t t':array A) (i j:Z),
   exchange t t' i j -> array_length t = array_length t'.
Proof.
intros A t t' i j.
simple induction 1; auto.
Qed.

Hint Resolve exchange_length : v62 datatypes.

(****************************************************************************)
(*                    Permutations of elements in arrays                    *)
(*                        Definition and properties                         *)
(****************************************************************************)

(* We define  as the smallest equivalence relation which contains
 * transpositions i.e. exchange of two elements.
 *)

Inductive permut (A:Set) : array A -> array A -> Prop :=
  | exchange_is_permut :
      forall (t t':array A) (i j:Z), exchange t t' i j -> permut t t'
  | permut_refl : forall t:array A, permut t t
  | permut_sym : forall t t':array A, permut t t' -> permut t' t
  | permut_trans :
      forall t t' t'':array A,
        permut t t' -> permut t' t'' -> permut t t''.

Hint Resolve exchange_is_permut permut_refl permut_sym permut_trans :
 v62 datatypes.

Lemma permut_length :
 forall t t':array Z, permut t t' -> array_length t = array_length t'.
Proof.
intros t t'; simple induction 1; auto; intros.
elim H0; auto.
omega.
Qed.

Hint Resolve permut_length : v62 datatypes.

(* We also define the permutation on a segment of an array, ,
 * the other parts of the array being unchanged
 *
 * One again we define it as the smallest equivalence relation containing
 * transpositions on the given segment.
 *)

Inductive sub_permut (A:Set) (g d:Z) : array A -> array A -> Prop :=
  | exchange_is_sub_permut :
      forall (t t':array A) (i j:Z),
        (g <= i <= d)%Z ->
        (g <= j <= d)%Z -> exchange t t' i j -> sub_permut g d t t'
  | sub_permut_refl : forall t:array A, sub_permut g d t t
  | sub_permut_sym :
      forall t t':array A, sub_permut g d t t' -> sub_permut g d t' t
  | sub_permut_trans :
      forall t t' t'':array A,
        sub_permut g d t t' ->
        sub_permut g d t' t'' -> sub_permut g d t t''.

Hint Resolve exchange_is_sub_permut sub_permut_refl sub_permut_sym
 sub_permut_trans : v62 datatypes.

Lemma sub_permut_length :
 forall (A:Set) (t t':array A) (g d:Z),
   sub_permut g d t t' -> array_length t = array_length t'.
Proof.
intros t t' g d; simple induction 1; auto; intros.
elim H2; auto.
omega.
Qed.

Hint Resolve sub_permut_length : v62 datatypes.

Lemma sub_permut_function :
 forall (A:Set) (t t':array A) (g d:Z),
   sub_permut g d t t' ->
   (0 <= g)%Z ->
   (d < array_length t)%Z ->
   forall i:Z,
     (g <= i <= d)%Z ->
     ( exists j : Z, (g <= j <= d)%Z /\ access t i = access t' j) /\
     ( exists j : Z, (g <= j <= d)%Z /\ access t' i = access t j).
Proof.
intros A t t' g d.
simple induction 1; intros.
(* 1. exchange *)
elim H2; intros.
elim (Z_eq_dec i0 i); intros.
(* i0 = i *)
split; [ exists j | exists j ].
split; [ assumption | rewrite a; assumption ].
split; [ assumption | rewrite a; symmetry; assumption ].
(* i0 <> i *)
elim (Z_eq_dec i0 j); intros.
(* i0 = j *)
split; [ exists i | exists i ].
split; [ assumption | rewrite a; assumption ].
split; [ assumption | rewrite a; symmetry; assumption ].
(* i0 <> j *)
split; [ exists i0 | exists i0 ].
split; [ assumption | apply H11; omega ].
split; [ assumption | symmetry; apply H11; omega ].

(* 2. refl *)
split; [ exists i | exists i ].
split; [ assumption | trivial ].
split; [ assumption | trivial ].

(* 3. sym *)
rewrite <- (sub_permut_length H0) in H3.
elim (H1 H2 H3 i); auto.

(* 4. trans *)
split.

elim (H1 H4 H5 i).
 intros.
elim H7; intros.
elim H9; intros.
rewrite (sub_permut_length H0) in H5.
elim (H3 H4 H5 x).
 intros.
elim H12; intros.
elim H14; intros.
exists x0.
 split; [ assumption | idtac ].
transitivity (access t'0 x); auto.
auto.
auto.

rewrite (sub_permut_length H0) in H5.
elim (H3 H4 H5 i).
 intros.
elim H8; intros.
elim H9; intros.
rewrite <- (sub_permut_length H0) in H5.
elim (H1 H4 H5 x).
 intros.
elim H13; intros.
elim H14; intros.
exists x0.
 split; [ assumption | idtac ].
transitivity (access t'0 x); auto.
auto.
auto.
Qed.

(* To express that some parts of arrays are equal we introduce the
 * property  which says that a segment is the same on two
 * arrays.
 *)

Definition array_id (A:Set) (t t':array A) (g d:Z) :=
  forall i:Z, (g <= i <= d)%Z -> access t i = access t' i.

(* array_id is an equivalence relation *)

Lemma array_id_refl :
 forall (A:Set) (t:array A) (g d:Z), array_id t t g d.
Proof.
unfold array_id.
auto with datatypes.
Qed.

Hint Resolve array_id_refl : v62 datatypes.

Lemma array_id_sym :
 forall (A:Set) (t t':array A) (g d:Z),
   array_id t t' g d -> array_id t' t g d.
Proof.
unfold array_id.
 intros.
symmetry; auto with datatypes.
Qed.

Hint Resolve array_id_sym : v62 datatypes.

Lemma array_id_trans :
 forall (A:Set) (t t' t'':array A) (g d:Z),
   array_id t t' g d -> array_id t' t'' g d -> array_id t t'' g d.
Proof.
unfold array_id.
 intros.
apply trans_eq with (y := access t' i); auto with datatypes.
Qed.

Hint Resolve array_id_trans : v62 datatypes.

(* Outside the segment [g,d] the elements are equal *)

Lemma sub_permut_id :
 forall (A:Set) (t t':array A) (g d:Z),
   sub_permut g d t t' ->
   array_id t t' 0 (g - 1) /\
   array_id t t' (d + 1) (array_length t - 1).
Proof.
intros A t t' g d.
simple induction 1; intros.
elim H2; intros.
unfold array_id; split; intros.
apply H8; omega.
apply H8; omega.
auto with datatypes.
rewrite <- (sub_permut_length H0).
decompose [and] H1; auto with datatypes.
intuition.
apply array_id_trans with t'0; auto with datatypes.
apply array_id_trans with t'0; auto with datatypes.
rewrite (sub_permut_length H0); auto.
Qed.

Hint Resolve sub_permut_id .

Lemma sub_permut_eq :
 forall (A:Set) (t t':array A) (g d:Z),
   sub_permut g d t t' ->
   forall i:Z,
     (0 <= i < g)%Z \/ (d < i < array_length t)%Z ->
     access t i = access t' i.
Proof.
intros A t t' g d Htt' i Hi.
elim (sub_permut_id Htt').
 unfold array_id.
 intros.
elim Hi; [ intro; apply H; omega | intro; apply H0; omega ].
Qed.

(* sub_permut is a particular case of permutation *)

Lemma sub_permut_is_permut :
 forall (A:Set) (t t':array A) (g d:Z),
   sub_permut g d t t' -> permut t t'.
Proof.
intros A t t' g d.
simple induction 1; intros; eauto with datatypes.
Qed.

Hint Resolve sub_permut_is_permut .

(* If we have a sub-permutation on an empty segment, then we have a 
 * sub-permutation on any segment.
 *)

Lemma sub_permut_void :
 forall (A:Set) (t t':array A) (g g' d d':Z),
   (d < g)%Z -> sub_permut g d t t' -> sub_permut g' d' t t'.
Proof.
intros A t t' g g' d d' Hdg.
simple induction 1; intros.
absurd (g <= d)%Z; omega.
auto with datatypes.
auto with datatypes.
eauto with datatypes.
Qed.

(* A sub-permutation on a segment may be extended to any segment that
 * contains the first one.
 *)

Lemma sub_permut_extension :
 forall (A:Set) (t t':array A) (g g' d d':Z),
   (g' <= g)%Z ->
   (d <= d')%Z -> sub_permut g d t t' -> sub_permut g' d' t t'.
Proof.
intros A t t' g g' d d' Hgg' Hdd'.
simple induction 1; intros.
apply exchange_is_sub_permut with (i := i) (j := j);
 [ omega | omega | assumption ].
auto with datatypes.
auto with datatypes.
eauto with datatypes.
Qed.
