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

(*  Library about sorted (sub-)arrays / Nicolas Magaud, July 1998 *)

(* $Id: WhySorted.v,v 1.12 2006/12/08 11:53:54 filliatr Exp $ *)

Require Export WhyArrays.
Require Import WhyPermut.

Require Import ZArithRing.
Require Import Omega.

Set Implicit Arguments.
Unset Strict Implicit.

(* Definition *)

Definition sorted_array (A:array Z) (deb fin:Z) :=
  (deb <= fin)%Z ->
  forall x:Z,
    (x >= deb)%Z -> (x < fin)%Z -> (access A x <= access A (x + 1))%Z.

(* Elements of a sorted sub-array are in increasing order *)

(* one element and the next one *)

Lemma sorted_elements_1 :
 forall (A:array Z) (n m:Z),
   sorted_array A n m ->
   forall k:Z,
     (k >= n)%Z ->
     forall i:Z,
       (0 <= i)%Z ->
       (k + i <= m)%Z -> (access A k <= access A (k + i))%Z.
Proof.
intros A n m H_sorted k H_k i H_i.
pattern i.
 apply natlike_ind.
intro.
replace (k + 0)%Z with k; omega.
 (*** Ring `k+0` => BUG ***)

intros.
apply Zle_trans with (m := access A (k + x)).
apply H0; omega.

unfold Zsucc.
replace (k + (x + 1))%Z with (k + x + 1)%Z.
unfold sorted_array in H_sorted.
apply H_sorted; omega.

omega.

assumption.
Qed.

(* one element and any of the following *)

Lemma sorted_elements :
 forall (A:array Z) (n m k l:Z),
   sorted_array A n m ->
   (k >= n)%Z ->
   (l < array_length A)%Z ->
   (k <= l)%Z -> (l <= m)%Z -> (access A k <= access A l)%Z.
Proof.
intros.
replace l with (k + (l - k))%Z.
apply sorted_elements_1 with (n := n) (m := m);
 [ assumption | omega | omega | omega ].
omega.
Qed.

Hint Resolve sorted_elements : datatypes v62.

(* A sub-array of a sorted array is sorted *)

Lemma sub_sorted_array :
 forall (A:array Z) (deb fin i j:Z),
   sorted_array A deb fin ->
   (i >= deb)%Z -> (j <= fin)%Z -> (i <= j)%Z -> sorted_array A i j.
Proof.
unfold sorted_array.
intros.
apply H; omega.
Qed.

Hint Resolve sub_sorted_array : datatypes v62.

(* Extension on the left of the property of being sorted *)

Lemma left_extension :
 forall (A:array Z) (i j:Z),
   (i > 0)%Z ->
   (j < array_length A)%Z ->
   sorted_array A i j ->
   (access A (i - 1) <= access A i)%Z -> sorted_array A (i - 1) j.
Proof.
intros; unfold sorted_array; intros.
elim (Z_ge_lt_dec x i).
   (* (`x >= i`) + (`x < i`) *)
intro Hcut.
apply H1; omega.

intro Hcut.
replace x with (i - 1)%Z.
replace (i - 1 + 1)%Z with i; [ assumption | omega ].

omega.
Qed.

(* Extension on the right *)

Lemma right_extension :
 forall (A:array Z) (i j:Z),
   (i >= 0)%Z ->
   (j < array_length A - 1)%Z ->
   sorted_array A i j ->
   (access A j <= access A (j + 1))%Z -> sorted_array A i (j + 1).
Proof.
intros; unfold sorted_array; intros.
elim (Z_lt_ge_dec x j).
intro Hcut.
 apply H1; omega.

intro HCut.
replace x with j; [ assumption | omega ].
Qed.

(* Substitution of the leftmost value by a smaller value *) 

Lemma left_substitution :
 forall (A:array Z) (i j v:Z),
   (i >= 0)%Z ->
   (j < array_length A)%Z ->
   sorted_array A i j ->
   (v <= access A i)%Z -> sorted_array (update A i v) i j.
Proof.
intros A i j v H_i H_j H_sorted H_v.
unfold sorted_array; intros.

cut (x = i \/ (x > i)%Z).
intro Hcut; elim Hcut; clear Hcut; intro.
rewrite H2.
AccessSame; try omega.
 AccessOther; try omega.
apply Zle_trans with (m := access A i);
 [ assumption | apply H_sorted; omega ].
do 2 (AccessOther; try omega).
apply H_sorted; omega.
omega.
Qed.

(* Substitution of the rightmost value by a larger value *)

Lemma right_substitution :
 forall (A:array Z) (i j v:Z),
   (i >= 0)%Z ->
   (j < array_length A)%Z ->
   sorted_array A i j ->
   (access A j <= v)%Z -> sorted_array (update A j v) i j.
Proof.
intros A i j v H_i H_j H_sorted H_v.
unfold sorted_array; intros.

cut (x = (j - 1)%Z \/ (x < j - 1)%Z).
intro Hcut; elim Hcut; clear Hcut; intro.
rewrite H2.
replace (j-1+1)%Z with j.
2: omega.
AccessOther; try omega.
 apply Zle_trans with (m := access A j).
 apply sorted_elements with (n := i) (m := j); try omega; assumption.
assumption.
do 2 (AccessOther; try omega).
apply H_sorted; omega.

omega.
Qed.

(* Affectation outside of the sorted region *)

Lemma no_effect :
 forall (A:array Z) (i j k v:Z),
   (i >= 0)%Z ->
   (j < array_length A)%Z ->
   sorted_array A i j ->
   (0 <= k < i)%Z \/ (j < k < array_length A)%Z ->
   sorted_array (update A k v) i j.
Proof.
intros.
unfold sorted_array; intros.
 do 2 (AccessOther; try omega).
apply H1; assumption.
Qed.

Lemma sorted_array_id :
 forall (t1 t2:array Z) (g d:Z),
   sorted_array t1 g d -> array_id t1 t2 g d -> sorted_array t2 g d.
Proof.
intros t1 t2 g d Hsorted Hid.
unfold array_id in Hid.
unfold sorted_array in Hsorted.
 unfold sorted_array.
intros Hgd x H1x H2x.
rewrite <- (Hid x); [ idtac | omega ].
rewrite <- (Hid (x + 1)%Z); [ idtac | omega ].
apply Hsorted; assumption.
Qed.
