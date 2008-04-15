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

(* $Id: WhyPermut.v,v 1.2 2006/11/02 09:18:21 hubert Exp $ *)

Require WhyArrays.
Require Omega.

Set Implicit Arguments.

(****************************************************************************)
(*                   Exchange of two elements in an array                   *)
(*                        Definition and properties                         *)
(****************************************************************************)

(* Definition *)

Inductive exchange [A:Set; t,t':(array A); i,j:Z] : Prop :=
  exchange_c :
    (array_length t) = (array_length t') ->
    `0 <= i < (array_length t)` -> `0 <= j < (array_length t)` ->
    (#t[i] = #t'[j]) ->
    (#t[j] = #t'[i]) ->
    ((k:Z)`0<=k<(array_length t)` -> `k<>i` -> `k<>j` -> #t[k] = #t'[k]) ->
    (exchange t t' i j).
    
(* Properties about exchanges *)

Lemma exchange_1 : (A:Set)(t:(array A))
  (i,j:Z) `0<=i<(array_length t)` -> `0<=j<(array_length t)` ->
  (access (store (store t i #t[j]) j #t[i]) i) = #t[j].
Proof.
Intros A t i j H_i H_j.
AccessStore j i H; WhyArrays; Auto with datatypes.
Save.

Hints Resolve exchange_1 : v62 datatypes.


Lemma exchange_proof :
  (A:Set)(t:(array A))
  (i,j:Z) `0<=i<(array_length t)` -> `0<=j<(array_length t)` ->
  (exchange (store (store t i (access t j)) j (access t i)) t i j).
Proof.
Intros A t i j H_i H_j.
Apply exchange_c; WhyArrays; Auto with datatypes.
Intros k H_k not_k_i not_k_j.
Cut ~j=k; Auto with datatypes. Intro not_j_k.
AccessOther; Auto with datatypes.
Save.

Hints Resolve exchange_proof : v62 datatypes.


Lemma exchange_sym :
  (A:Set)(t,t':(array A))(i,j:Z)
  (exchange t t' i j) -> (exchange t' t i j).
Proof.
Intros A t t' i j H1.
Elim H1; Intro eq; Rewrite eq; Clear H1; Intros.
Constructor 1; Auto with datatypes.
Intros. Rewrite (H3 k); Auto with datatypes.
Save.

Hints Resolve exchange_sym : v62 datatypes.


Lemma exchange_id :
  (A:Set)(t,t':(array A))(i,j:Z)
  (exchange t t' i j) -> 
  i=j ->
  (k:Z) `0 <= k < (array_length t)` -> (access t k)=(access t' k).
Proof.
Intros A t t' i j Hex Heq k Hk.
Elim Hex. Clear Hex. Intros.
Rewrite Heq in H2. Rewrite Heq in H3.
Case (Z_eq_dec k j). 
  Intro Heq'. Rewrite Heq'. Assumption.
  Intro Hnoteq. Apply (H4 k); Auto with datatypes. Rewrite Heq. Assumption.
Save.

Hints Resolve exchange_id : v62 datatypes.


Lemma exchange_length :
  (A:Set)(t,t':(array A))(i,j:Z)
  (exchange t t' i j) -> 
  (array_length t) = (array_length t').
Proof.
Intros A t t' i j. Induction 1; Auto.
Save.

Hints Resolve exchange_length : v62 datatypes.

(****************************************************************************)
(*                    Permutations of elements in arrays                    *)
(*                        Definition and properties                         *)
(****************************************************************************)

(* We define "permut" as the smallest equivalence relation which contains
 * transpositions i.e. exchange of two elements.
 *)

Inductive permut [A:Set] : (array A)->(array A)->Prop :=
    exchange_is_permut : 
      (t,t':(array A))(i,j:Z)(exchange t t' i j) -> (permut t t')
  | permut_refl : 
      (t:(array A))(permut t t)
  | permut_sym : 
      (t,t':(array A))(permut t t') -> (permut t' t)
  | permut_trans : 
      (t,t',t'':(array A))
      (permut t t') -> (permut t' t'') -> (permut t t'').

Hints Resolve exchange_is_permut permut_refl permut_sym permut_trans : v62 datatypes.

Lemma permut_length :
  (t,t':(array Z))
  (permut t t') ->
  (array_length t) = (array_length t').
Proof.
Intros t t'; Induction 1; Auto; Intros.
Elim H0; Auto.
Omega.
Save.

Hints Resolve permut_length : v62 datatypes.

(* We also define the permutation on a segment of an array, "sub_permut",
 * the other parts of the array being unchanged
 *
 * One again we define it as the smallest equivalence relation containing
 * transpositions on the given segment.
 *)

Inductive sub_permut [A:Set; g,d:Z] : (array A)->(array A)->Prop :=
    exchange_is_sub_permut : 
      (t,t':(array A))(i,j:Z)`g <= i <= d` -> `g <= j <= d`
      -> (exchange t t' i j) -> (sub_permut g d t t')
  | sub_permut_refl : 
      (t:(array A))(sub_permut g d t t)
  | sub_permut_sym : 
      (t,t':(array A))(sub_permut g d t t') -> (sub_permut g d t' t)
  | sub_permut_trans : 
      (t,t',t'':(array A))
      (sub_permut g d t t') -> (sub_permut g d t' t'') 
      -> (sub_permut g d t t'').

Hints Resolve exchange_is_sub_permut sub_permut_refl sub_permut_sym sub_permut_trans
  : v62 datatypes.

Lemma sub_permut_length :
  (A:Set)(t,t':(array A))(g,d:Z)
  (sub_permut g d t t') ->
  (array_length t) = (array_length t').
Proof.
Intros t t' g d; Induction 1; Auto; Intros.
Elim H2; Auto.
Omega.
Save.

Hints Resolve sub_permut_length : v62 datatypes.

Lemma sub_permut_function :
  (A:Set)(t,t':(array A))(g,d:Z)
  (sub_permut g d t t') ->
  `0 <= g` -> `d < (array_length t)` -> 
  (i:Z) `g <= i <= d`
    -> (EX j:Z | `g <= j <= d` /\ #t[i]=#t'[j])
    /\ (EX j:Z | `g <= j <= d` /\ #t'[i]=#t[j]).
Proof.
Intros A t t' g d.
Induction 1; Intros.
(* 1. exchange *)
Elim H2; Intros.
Elim (Z_eq_dec i0 i); Intros.
(* i0 = i *)
Split ; [ Exists j | Exists j ].
Split; [ Assumption | Rewrite a; Assumption ].
Split; [ Assumption | Rewrite a; Symmetry; Assumption ].
(* i0 <> i *)
Elim (Z_eq_dec i0 j); Intros.
(* i0 = j *)
Split ; [ Exists i | Exists i ].
Split; [ Assumption | Rewrite a; Assumption ].
Split; [ Assumption | Rewrite a; Symmetry; Assumption ].
(* i0 <> j *)
Split ; [ Exists i0 | Exists i0 ].
Split; [ Assumption | Apply H11; Omega ].
Split; [ Assumption | Symmetry; Apply H11; Omega ].

(* 2. refl *)
Split ; [ Exists i | Exists i].
Split; [ Assumption | Trivial ].
Split; [ Assumption | Trivial ].

(* 3. sym *)
Rewrite <- (sub_permut_length H0) in H3.
Elim (H1 H2 H3 i); Auto.

(* 4. trans *)
Split.

Elim (H1 H4 H5 i). Intros.
Elim H7; Intros.
Elim H9; Intros.
Rewrite (sub_permut_length H0) in H5.
Elim (H3 H4 H5 x). Intros.
Elim H12; Intros.
Elim H14; Intros.
Exists x0. Split ; [ Assumption | Idtac ].
Transitivity (access t'0 x); Auto.
Auto.
Auto.

Rewrite (sub_permut_length H0) in H5.
Elim (H3 H4 H5 i). Intros.
Elim H8; Intros.
Elim H9; Intros.
Rewrite <- (sub_permut_length H0) in H5.
Elim (H1 H4 H5 x). Intros.
Elim H13; Intros.
Elim H14; Intros.
Exists x0. Split ; [ Assumption | Idtac ].
Transitivity (access t'0 x); Auto.
Auto.
Auto.
Save.

(* To express that some parts of arrays are equal we introduce the
 * property "array_id" which says that a segment is the same on two
 * arrays.
 *)

Definition array_id := [A:Set][t,t':(array A)][g,d:Z]
  (i:Z) `g <= i <= d` -> #t[i] = #t'[i].

(* array_id is an equivalence relation *)

Lemma array_id_refl : 
  (A:Set)(t:(array A))(g,d:Z)
  (array_id t t g d).
Proof.
Unfold array_id.
Auto with datatypes.
Save.

Hints Resolve array_id_refl : v62 datatypes.

Lemma array_id_sym :
  (A:Set)(t,t':(array A))(g,d:Z)
  (array_id t t' g d)
  -> (array_id t' t g d).
Proof.
Unfold array_id. Intros.
Symmetry; Auto with datatypes.
Save.

Hints Resolve  array_id_sym : v62 datatypes.

Lemma array_id_trans :
  (A:Set)(t,t',t'':(array A))(g,d:Z)
  (array_id t t' g d)
  -> (array_id t' t'' g d)
    -> (array_id t t'' g d).
Proof.
Unfold array_id. Intros.
Apply trans_eq with y:=#t'[i]; Auto with datatypes.
Save.

Hints Resolve array_id_trans: v62 datatypes.

(* Outside the segment [g,d] the elements are equal *)

Lemma sub_permut_id :
  (A:Set)(t,t':(array A))(g,d:Z)
  (sub_permut g d t t') ->
  (array_id t t' `0` `g-1`) /\ 
  (array_id t t' `d+1` `(array_length t)-1`).
Proof.
Intros A t t' g d. Induction 1; Intros.
Elim H2; Intros.
Unfold array_id; Split; Intros.
Apply H8; Omega.
Apply H8; Omega.
Auto with datatypes.
Rewrite <- (sub_permut_length H0).
Decompose [and] H1; Auto with datatypes.
Intuition.
Apply array_id_trans with t'0; Auto with datatypes.
Apply array_id_trans with t'0; Auto with datatypes.
Rewrite (sub_permut_length H0); Auto.
Save.

Hints Resolve sub_permut_id.

Lemma sub_permut_eq :
  (A:Set)(t,t':(array A))(g,d:Z)
  (sub_permut g d t t') ->
  (i:Z) (`0<=i<g` \/ `d<i<(array_length t)`) -> #t[i]=#t'[i].
Proof.
Intros A t t' g d Htt' i Hi.
Elim (sub_permut_id Htt'). Unfold array_id. 
Intros.
Elim Hi; [ Intro; Apply H; Omega | Intro; Apply H0; Omega ].
Save.

(* sub_permut is a particular case of permutation *)

Lemma sub_permut_is_permut :
  (A:Set)(t,t':(array A))(g,d:Z)
  (sub_permut g d t t') ->
  (permut t t').
Proof.
Intros A t t' g d. Induction 1; Intros; EAuto with datatypes.
Save.

Hints Resolve sub_permut_is_permut.

(* If we have a sub-permutation on an empty segment, then we have a 
 * sub-permutation on any segment.
 *)

Lemma sub_permut_void :
  (A:Set)(t,t':(array A))
  (g,g',d,d':Z) `d < g`
   -> (sub_permut g d t t') -> (sub_permut g' d' t t').
Proof.
Intros A t t' g g' d d' Hdg.
(Induction 1; Intros).
(Absurd `g <= d`; Omega).
Auto with datatypes.
Auto with datatypes.
EAuto with datatypes.
Save.

(* A sub-permutation on a segment may be extended to any segment that
 * contains the first one.
 *)

Lemma sub_permut_extension :
  (A:Set)(t,t':(array A))
  (g,g',d,d':Z) `g' <= g` -> `d <= d'`
   -> (sub_permut g d t t') -> (sub_permut g' d' t t').
Proof.
Intros A t t' g g' d d' Hgg' Hdd'.
(Induction 1; Intros).
Apply exchange_is_sub_permut with i:=i j:=j; [ Omega | Omega | Assumption ].
Auto with datatypes.
Auto with datatypes.
EAuto with datatypes.
Save.
