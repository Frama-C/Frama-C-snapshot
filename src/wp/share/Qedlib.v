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

Require Import Bool.
Require Import ZArith.
Require Import Reals.

Set Implicit Arguments.

(** ** Tactical *)

Ltac forward := 
  repeat (first [ split | intros ]) ; 
  try discriminate ; try contradiction ; try tauto ; try constructor ; auto with zarith.

Ltac finish := forward ; fail.

Tactic Notation "by" tactic(A) := A ; finish.

(** ** Conditional Property *)

Definition itep (A B C : Prop) := (A -> B) /\ (~A -> C).

Lemma ite_then : forall A B C : Prop, itep A B C -> A -> B.
Proof. by (unfold itep). Qed.

Lemma ite_else : forall A B C : Prop, itep A B C -> ~A -> C.
Proof. by (unfold itep). Qed.

Lemma ite_both : forall A B C : Prop, itep A B C -> (B \/ C).
Proof. by (unfold itep). Qed.

(** ** Booleans *)

Inductive reflect (P:Prop) : bool -> Prop :=
  | R_true  : P -> reflect P true
  | R_false : ~P -> reflect P false.

Definition boolean {A : Set} 
  (f : A -> A -> bool) 
  (p : A -> A -> Prop) : Prop := forall x y, reflect (p x y) (f x y).
  
(*
  forall x y, (f x y = true <-> p x y) /\
              (f x y = false <-> ~(p x y)).
*)

Ltac case_leq x y := 
  generalize (Zle_cases x y) ; induction (Zle_bool x y) ; try omega.

Ltac case_lt x y :=
  generalize (Zlt_cases x y) ; induction (Zlt_bool x y) ; try omega.

Ltac case_eq x y :=
  generalize (Zeq_bool_if x y) ; induction (Zeq_bool x y) ; try omega.

Lemma Zneq_cases : forall x y, if Zneq_bool x y then x <> y else x = y.
Proof.
  intros x y.
  generalize (Zeq_bool_if x y).
  unfold Zeq_bool.
  unfold Zneq_bool.
  induction (x ?= y)%Z ; auto.
Qed.

Ltac case_neq x y :=
  generalize (Zneq_cases x y) ; induction (Zneq_bool x y) ; try omega.

Theorem Zeq_boolean : boolean Zeq_bool (fun x y => (x=y)%Z).
Proof.
  unfold boolean. intros x y. by (case_eq x y).
Qed.

Theorem Zneq_boolean : boolean Zneq_bool (fun x y => (x <> y)%Z).
Proof.
  unfold boolean. intros x y. by (case_neq x y).
Qed.

Theorem Zlt_boolean : boolean Zlt_bool Zlt.
Proof.
  unfold boolean. intros x y. by (case_lt x y).
Qed.

Theorem Zle_boolean : boolean Zle_bool Zle.
Proof.
  unfold boolean. intros x y. by (case_leq x y).
Qed.

Parameter Req_bool : R -> R -> bool.
Parameter Rlt_bool : R -> R -> bool.
Parameter Rle_bool : R -> R -> bool.
Parameter Rneq_bool : R -> R -> bool.
Hypothesis Rlt_boolean : boolean Rlt_bool Rlt.
Hypothesis Rle_boolean : boolean Rle_bool Rle.
Hypothesis Req_boolean : boolean Req_bool (fun x y => (x=y)%Z).
Hypothesis Rneq_boolean : boolean Rneq_bool (fun x y => (x<>y)%Z).

Parameter Aeq_bool : forall A, A -> A -> bool.
Hypothesis Aeq_boolean : forall A : Set, boolean (@Aeq_bool A) (fun x y => x=y).
Definition Aneq_bool {A : Set} (x y : A) := negb (Aeq_bool x y).
Hypothesis Aneq_boolean : forall A : Set, boolean (@Aneq_bool A) (fun x y => x<>y).

(** ** Arrays *)

Definition equality (A : Set) := forall (x y : A), {x=y}+{x<>y}.
Record farray (A B : Set) := { array_eq : equality A ; access :> A -> B }.
Definition array (A : Set) := farray Z A.
Definition array_def {A : Set} (f : Z -> A) := {| array_eq := Z_eq_dec ; access := f |}.
Hypothesis extensionality: forall (A B : Set) (f g : A -> B),
  (forall x, f x = g x) -> f = g.

Lemma farray_eq : forall A B (m1 m2 : farray A B),
   array_eq m1 = array_eq m2 ->
   (forall k, m1 k = m2 k) -> m1 = m2.
Proof.
  intros A B m1 m2.
  induction m1. induction m2. simpl.
  (intro H ; rewrite H ; clear H).
  intro K. 
  assert (H : access0 = access1). by (apply extensionality).
  by (rewrite H).
Qed.
  
Definition update {A B : Set} 
  (m : farray A B) (k : A) (v : B) : (farray A B) :=
  Build_farray (array_eq m) (fun i => if array_eq m i k then v else m i).

Notation " a .[ k <- v ] " := (update a k v) (at level 70).

Lemma access_update : 
  forall (A B : Set) (m : farray A B) k v,
  m.[k <- v] k = v.
Proof. intros. simpl. elim (array_eq _ k k) ; intuition. Qed.

Lemma access_update_neq :
  forall (A B : Set) (m : farray A B) i j v,
  i <> j -> m.[ i <- v ] j = m j.
Proof. 
  intros.
  simpl.
  by (induction (array_eq m j i) ; [absurd (i=j)|]).
Qed.

(** ** Division on Z *)

Definition Cdiv (n d : Z) : Z :=
  match n , d with
    | 0%Z , _ | _ , 0%Z => 0%Z
    | Zpos _ , Zpos _ => ( n / d )%Z
    | Zneg _ , Zneg _ => ( (-n) / (-d) )%Z
    | Zneg _ , Zpos _ => (- ( (-n) / d ))%Z
    | Zpos _ , Zneg _ => (- ( n / (-d) ))%Z
  end.

Definition Cmod (n d : Z) : Z := let q := Cdiv n d in (n - q * n)%Z.

Theorem Cdiv_enclidian : 
  forall (n d : Z), let q := Cdiv n d in let r := Cmod n d in
  (n * q + r = n)%Z.
Proof. intros. unfold r. unfold Cmod. fold q. ring. Qed.

Lemma Zdiv_less :
  forall (n d : Z), (n > 0)%Z -> (d > 0)%Z -> (d * (n/d) <= n)%Z.
Proof.
  intros n d Npos Dpos.
  generalize (Zmod_eq n d).
  pose (x := (n/d)%Z).
  fold x. intro H. generalize (H Dpos). clear H.
  pose (r := (n mod d)%Z).
  fold r. intro H.
  generalize (Z_mod_lt n d).
  intro R. generalize (R Dpos). clear R. fold r.
  assert (D : (d*x = n - r)%Z). rewrite H. ring.
  rewrite D. intro R. omega.
Qed.


