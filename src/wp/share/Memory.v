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
(* --- Formalization of Pointers                                          --- *)
(* -------------------------------------------------------------------------- *)

Require Import ZArith.
Require Import Qedlib.

(** ** Addresses *)

Record addr : Set := { base : Z ; offset : Z }.

Lemma addr_eq : forall (p q : addr), 
  base p = base q -> offset p = offset q -> p=q.
Proof. 
  intros p q.
  induction p. induction q. simpl.
  repeat ( intro H ; rewrite H ; clear H ).
  reflexivity.
Qed.

Lemma addr_eq_inv : forall a i b j,
  {| base := a ; offset := i |} = {| base := b ; offset := j |} ->
  ( a = b /\ i = j ).
Proof.
  intros. split.
  assert ( E : base {| base := a; offset := i |} = base {| base := b; offset := j |} ).
    by (rewrite H).
  by (simpl in E).
  assert ( E : offset {| base := a; offset := i |} = offset {| base := b; offset := j |} ).
    by (rewrite H).
  by (simpl in E).
Qed.

Lemma addr_neq_inv : forall p q,
  ( base p <> base q \/ offset p <> offset q ) -> p <> q.
Proof.
  intros. elim H ; clear H ; intros NEQ EQ ; by (rewrite EQ in NEQ).
Qed.

Lemma addr_neq_base : forall p q,
  base p <> base q -> p <> q.
Proof.
  intros p q NEQ EQ. by (rewrite EQ in NEQ).
Qed.

Lemma addr_neq_offset : forall p q,
  offset p <> offset q -> p <> q.
Proof.
  intros p q NEQ EQ. by (rewrite EQ in NEQ).
Qed.

(** ** Addresses Comparison *)

Definition addr_le (p q : addr) := (base p = base q) /\ (offset p <= offset q)%Z.
Definition addr_lt (p q : addr) := (base p = base q) /\ (offset p < offset q)%Z.
Definition addr_eq_bool (p q : addr) := andb (Zeq_bool (base p) (base q)) (Zeq_bool (offset p) (offset q)).
Definition addr_le_bool (p q : addr) := andb (Zeq_bool (base p) (base q)) (Zle_bool (offset p) (offset q)).
Definition addr_lt_bool (p q : addr) := andb (Zeq_bool (base p) (base q)) (Zlt_bool (offset p) (offset q)).

Lemma addr_le_boolean : boolean addr_le_bool addr_le.
Proof.
  unfold boolean. intros x y.
  unfold addr_le_bool,addr_le.
  by ( case_eq (base x) (base y) ; case_leq (offset x) (offset y) ).
Qed.

Lemma addr_lt_boolean : boolean addr_lt_bool addr_lt.
Proof.
  unfold boolean. intros x y.
  unfold addr_lt_bool,addr_lt.
  by ( case_eq (base x) (base y) ; case_lt (offset x) (offset y) ).
Qed.

(** ** Pointer Arithmetic *)

Definition null := {| base := 0 ; offset := 0 |}.
Definition global b := {| base := b ; offset := 0 |}.
Definition shift p k := {| base := base p ; offset := offset p + k |}.

Lemma shift_shift : forall p k1 k2, shift (shift p k1) k2 = shift p (k1+k2).
Proof. 
  intros. 
  repeat (unfold shift ; simpl). 
  apply addr_eq ; simpl ; auto with zarith.
Qed.

Lemma shift_zero : forall p, shift p 0 = p.
Proof.
  intros. unfold shift ; apply addr_eq ; simpl ; auto with zarith.
Qed.

Definition in_range q p a := exists i, 0 <= i < a /\ q = shift p i.

Definition included p a q b := 
  (a > 0)%Z ->
    ( (b >= 0)%Z /\
      base p = base q /\
      (offset q <= offset p)%Z /\ 
      (offset p + a <= offset q + b )%Z ).

Lemma included_correct : forall p a q b,
  included p a q b <-> (forall r, in_range r p a -> in_range r q b).
Proof.
  (* ==> *)
  intros. split.
  intros INC r [i [ri rsi]].
  unfold included in INC.
  exists (offset p + i - offset q).
  unfold shift. rewrite rsi. unfold shift ; simpl. 
  split ; try apply addr_eq ; simpl ; auto with zarith.
  (* <== *)
  intro Range.
  unfold included.
  intros apos.
  generalize (Range p). intro RP.
  pose (r := shift p (a-1)).
  generalize (Range r). intro RR.
  assert (PA: in_range p p a). 
    exists 0 ; split ; auto with zarith. 
      unfold shift. apply addr_eq ; simpl ; auto with zarith.
  assert (PB: in_range p q b). by (apply RP).
  assert (RA: in_range r p a).
    unfold r. exists (a-1) ; split ; auto with zarith.
  assert (RB: in_range r q b). by (apply RR).
  destruct RA as [i [Ir Iaddr]].
  destruct RB as [j [Jr Jaddr]].
  destruct PB as [k [Kr Kaddr]].
  rewrite Iaddr in Jaddr.
  assert (BASE: base p = base q). by (rewrite Kaddr).
  assert (OFF1: offset p = offset q + k). by (rewrite Kaddr).
  assert (OFF2: offset (shift p i) = offset (shift q j)). by (rewrite Jaddr).
  assert (OFF3: offset r = offset (shift p i)). by (rewrite Iaddr).
  unfold r in OFF3. simpl in OFF3.
  assert (OFF4: i = a-1) by omega.
  simpl in OFF2.
  intuition auto with zarith.
Qed.

Definition separated p a q b :=
  (a <= 0)%Z \/ (b <= 0)%Z \/ 
  ( base p <> base q ) \/
  ( offset q + b <= offset p )%Z \/
  ( offset p + a <= offset q )%Z.

Lemma separated_neq : forall p a q b p' q',
  separated p a q b ->
  included p' 1 p a ->
  included q' 1 q b ->
  p' <> q'.
Proof.
  intros p a q b p' q' SEP InP InQ EQ.
  unfold separated in SEP.
  unfold included in InP,InQ.
  case_lt 0%Z a.
  case_lt 0%Z b.
  intros BPOS APOS.
  generalize InP ; clear InP.
  intro H ; elim H ; clear H ; auto with zarith.
  intro H. clear H.
  intro H ; elim H ; clear H.
  intro BaseP.
  intro H ; elim H ; clear H.
  intros InP1 InP2.
  generalize InQ ; clear InQ.
  intro H ; elim H ; clear H ; auto with zarith.
  intro H. clear H.
  intro H ; elim H ; clear H.
  intro BaseQ.
  intro H ; elim H ; clear H.
  intros InQ1 InQ2.
  generalize SEP ; clear SEP.
  intro H ; elim H ; clear H ; auto with zarith.
  intro H ; elim H ; clear H ; auto with zarith.
  intro H ; elim H ; clear H ; auto with zarith.
  rewrite <- EQ in BaseQ.
  rewrite BaseP in BaseQ.
  contradiction.
  rewrite <- EQ in InQ1,InQ2.
  omega.
Qed.

Lemma separated_not_included : forall p a q b,
  (a > 0)%Z -> (b > 0)%Z -> 
    separated p a q b -> ~(included p a q b).
Proof.
  unfold separated. unfold included. unfold not.
  intuition.
Qed.

Lemma included_not_separated : forall p a q b,
  (a > 0)%Z -> (b > 0)%Z -> 
    included p a q b -> ~(separated p a q b).
Proof.
  unfold separated. unfold included. unfold not.
  intuition.
Qed.

Lemma included_trans : forall p a q b r c,
  included p a q b -> included q b r c -> included p a r c.
Proof.
  intros p a q b r c.
  unfold included. intuition. 
Qed.

Lemma included_refl : forall p a,
  included p a p a.
Proof.
  intros p a. unfold included. intuition.
Qed.

Lemma separated_trans : forall p a q b r c,
  included p a q b -> separated q b r c -> separated p a r c.
Proof.
  intros p a q b r c.
  case_leq a 0%Z. unfold separated. intuition.
  intro Apos.
  unfold included. unfold separated. intuition.
Qed.

Lemma separated_sym : forall p a q b,
  separated p a q b -> separated q b p a.
Proof.
  intros p a q b.
  unfold separated. intuition.
Qed.

Ltac pointer_arith :=
  repeat (unfold separated,included,shift ; simpl) ;
  repeat (intros ; split) ;
  forward.

(** ** Validity *)

Definition malloc := farray Z Z.

Definition valid_rd ( m : malloc ) p n :=
  (n > 0)%Z -> ( 0 <= offset p /\ offset p + n <= m (base p))%Z.

Definition valid_rw ( m : malloc ) p n :=
  (n > 0)%Z -> ( 0 < base p /\ 0 <= offset p /\ offset p + n <= m (base p))%Z.

Lemma valid_rw_rd : forall m p n, valid_rw m p n -> valid_rd m p n.
Proof.
  intros m p n.
  unfold valid_rw. unfold valid_rd.
  intuition (auto with zarith).
Qed.

Lemma valid_string : forall (m : malloc) p,
  ( base p < 0 )%Z ->
  ( 0 <= offset p < m (base p) )%Z ->
  ( valid_rd m p 1 /\ ~(valid_rw m p 1) ).
Proof.
  intros m p.
  unfold valid_rd. unfold valid_rw.
  intuition (auto with zarith).
Qed.

(** ** Memories *)

Definition mem (A : Set) := farray addr A.

Definition eqmem{A} (m1 m2 : mem A) p n :=
  forall q, included q 1 p n -> m1 q = m2 q.

Definition havoc{A} (m1 m2 : mem A) p n :=
  forall q, separated q 1 p n -> m1 q = m2 q.

Lemma eqmem_sym : forall A (m1 m2 : mem A) p n,
  eqmem m1 m2 p n -> eqmem m2 m1 p n.
Proof. intros A m1 m2 p a. unfold eqmem. 
  intro H. intros. cut (m1 q = m2 q). intro E ; rewrite E ; reflexivity.
  apply H ; auto.
Qed.

Lemma havoc_sym : forall A (m1 m2 : mem A) p n,
  havoc m1 m2 p n -> havoc m2 m1 p n.
Proof. intros A m1 m2 p n. unfold havoc. 
  intro H. intros. cut (m1 q = m2 q). intro E ; rewrite E ; reflexivity.
  apply H ; auto.
Qed.

Lemma update_separated : forall A (m : mem A) p x q,
  separated q 1 p 1 -> m.[ p <- x ] q = m q.
Proof.
  intros A m p x q SEP.
  apply access_update_neq.
  generalize SEP. unfold separated.
  intuition.
  rewrite H in H0. auto.
  rewrite H in H1. auto with zarith.
  rewrite H in H1. auto with zarith.
Qed.

Lemma eqmem_shift : forall A (m1 m2 : mem A) p n k,
  eqmem m1 m2 p n -> (0 <= k < n)%Z -> m1 (shift p k) = m2 (shift p k).
Proof.
  intros.
  apply H. unfold included. intuition ;
  unfold shift ; simpl ; auto with zarith.
Qed.

Lemma havoc_left : forall A (m1 m2 : mem A) p n k,
  havoc m1 m2 p n -> (k < 0)%Z -> m1 (shift p k) = m2 (shift p k).
Proof.
  intros.
  apply H. unfold separated. intuition pointer_arith.
Qed.

Lemma havoc_right : forall A (m1 m2 : mem A) p n k,
  havoc m1 m2 p n -> (k >= n)%Z -> m1 (shift p k) = m2 (shift p k).
Proof.
  intros.
  apply H. unfold separated. intuition pointer_arith.
Qed.

Lemma eqmem_included : forall A (m1 m2 : mem A) p q a b,
  included p a q b -> eqmem m1 m2 q b -> eqmem m1 m2 p a.
Proof.
  intros A m1 m2 p q a b INC EQ.
  unfold eqmem. intros r Range.
  apply EQ.
  apply included_trans with (q:=p) (b:=a) ; auto.
Qed.

Lemma havoc_separated : forall A (m1 m2 : mem A) p q a b,
  separated p a q b -> havoc m1 m2 q b -> eqmem m1 m2 p a.
Proof.
  intros A m1 m2 p q a b SEP HAVOC.
  unfold eqmem. intros r Range.
  apply HAVOC.
  apply separated_trans with (q:=p) (b:=a) ; auto.
Qed.

(** ** Regions *)

Parameter region : Z -> Z.
Parameter linked : malloc -> Prop.
Parameter sconst : mem Z -> Prop.
Definition framed (m : mem addr) := forall p, region (base (m p)) <= 0%Z.

Lemma separated_region : forall p a q b, 
  region (base p) <> region (base q) -> separated p a q b.
Proof.
  intros p a q b RDIFF.
  unfold separated.
  right. right. left.
  intuition.
  apply RDIFF. rewrite H. auto.
Qed.

(** ** Cast *)

Parameter cast : addr -> Z.
Hypothesis cast_injective : forall p q, cast p = cast q -> p = q.

(** ** Physical Addresses *)

Parameter hardware : Z -> Z.

