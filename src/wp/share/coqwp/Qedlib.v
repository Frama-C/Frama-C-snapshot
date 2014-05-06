(**************************************************************************)
(*                                                                        *)
(*  This file is part of WP plug-in of Frama-C.                           *)
(*                                                                        *)
(*  Copyright (C) 2007-2014                                               *)
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

Require Import Bool.
Require Import ZArith.
Require Import Reals.

Require BuiltIn.
Require map.Map.

Open Scope Z_scope.

Set Implicit Arguments.

(** ** Tactical *)

Ltac forward := 
  repeat (first [ split | intros ]) ; 
  try discriminate ; 
  try contradiction ; 
  try tauto ; 
  try constructor ; 
  try (apply False_ind ; omega ; fail) ;
  try (apply False_ind ; auto with zarith ; fail) ;
  auto with zarith.

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

Remark contrap: forall P Q : Prop,
  (P -> Q) -> ~Q -> ~P.
Proof. intuition. Qed.

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
  induction (x ?= y) ; auto.
Qed.

Ltac case_neq x y :=
  generalize (Zneq_cases x y) ; induction (Zneq_bool x y) ; try omega.

Inductive Zcases (x y : Z) :=
  | Case_lt : (x < y) -> Zcases x y
  | Case_eq : (x = y) -> Zcases x y
  | Case_gt : (x > y) -> Zcases x y.

Program Definition Zcompare x y : Zcases x y.
Proof.
  intros.
  case_leq x y.
  case_lt x y. intros H _. exact (Case_lt H).
  intros H1 H2. assert (H : x=y) by omega. exact (Case_eq H).
  intro H. exact (Case_gt H).
Qed.

Theorem Zeq_boolean : boolean Zeq_bool (fun x y => (x=y)).
Proof.
  unfold boolean. intros x y. by (case_eq x y).
Qed.


Theorem Zneq_boolean : boolean Zneq_bool (fun x y => (x <> y)).
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
Hypothesis Req_boolean : boolean Req_bool (fun x y => (x=y)).
Hypothesis Rneq_boolean : boolean Rneq_bool (fun x y => (x<>y)).

Parameter Aeq_bool : forall A : Set, A -> A -> bool.
Hypothesis Aeq_boolean : forall A : Set, boolean (@Aeq_bool A) (fun x y => x=y).
Definition Aneq_bool {A : Set} (x y : A) := negb (Aeq_bool x y).
Hypothesis Aneq_boolean : forall A : Set, boolean (@Aneq_bool A) (fun x y => x<>y).

(** ** Integer Induction (after a given rank) *)

Theorem Z_induction(m : Z)(P : Z -> Prop) : 
  (forall n, n <= m -> P n ) ->
  (forall n, n >= m -> P n -> P (n+1)) ->
  (forall n, P n).
Proof.
  intros.
  induction (Z_le_dec n m) ; auto with zarith.
  apply Z.le_ind with (n := m) ; auto with zarith.
  unfold Morphisms.Proper.
  unfold Morphisms.respectful.
  intros. rewrite H1. intuition.
  intros.
  apply H0 ; auto with zarith.
Qed.

(** ** Real Constants *)

(** signed power *)

Definition real_base e a n :=
  match n with
  | 0 => 1%R
  | Zpos n => (a * pow e (Pos.to_nat n))%R
  | Zneg n => (a / pow e (Pos.to_nat n))%R
  end.

(** an integer multiplied by a (signed) power of 10. *)
Definition real_dec := real_base 10%R.

(** an integer multiplied by a (signed) power of 2. *)
Definition real_hex := real_base 2%R.

(** ** Arrays *)

Record farray (A B : Type) := { whytype1 : BuiltIn.WhyType A ;
                               whytype2 : BuiltIn.WhyType B ;
                               access :> @Map.map A whytype1 B whytype2 }.
Definition array (A : Type) := farray Z A.
Hypothesis extensionality: forall (A B : Type) (f g : A -> B),
  (forall x, f x = g x) -> f = g.

Definition select {A B : Type}
  (m : farray A B) (k : A) : B := Map.get m k.

Lemma farray_eq : forall A B (m1 m2 : farray A B),
   whytype1 m1 = whytype1 m2 -> whytype2 m1 = whytype2 m2 ->
   (forall k, select m1 k = select m2 k) -> m1 = m2.
Proof.
  intros A B m1 m2.
  destruct m1. destruct m2. simpl.
  intros H1 H2; rewrite H1; rewrite H2 ; clear H1 H2.
  destruct access0. destruct access1. compute.
  intro K.
  rewrite (extensionality b b0 K).
  reflexivity.
Qed.

Definition update {A B : Type}
  (m : farray A B) (k : A) (v : B) : (farray A B) :=
  {| whytype1 := whytype1 m; whytype2 := whytype2 m; access := Map.set m k v|}.

Notation " a .[ k ] " := (select a k) (at level 60).
Notation " a .[ k <- v ] " := (update a k v) (at level 60).

Lemma access_update :
  forall (A B : Type) (m : farray A B) k v,
  m.[k <- v].[k] = v.
Proof.
  intros.
  apply Map.Select_eq.
  reflexivity.
Qed.

Lemma access_update_neq :
  forall (A B : Type) (m : farray A B) i j v,
  i <> j -> m.[ i <- v ].[j] = m.[j].
Proof.
  intros.
  apply Map.Select_neq.
  assumption.
Qed.

(** ** Division on Z *)

Definition Cdiv (n d : Z) : Z :=
  match n , d with
    | 0 , _ | _ , 0 => 0
    | Zpos a , Zpos b 
    | Zneg a , Zneg b => (Zpos a/Zpos b)
    | Zpos a , Zneg b
    | Zneg a , Zpos b => (-(Zpos a/Zpos b))
  end.

Definition Cmod (n d : Z) : Z :=
  match n , d with
    | 0 , _ | _ , 0 => 0
    | Zpos a , Zpos b
    | Zpos a , Zneg b => ( (Zpos a) mod (Zpos b) )
    | Zneg a , Zpos b
    | Zneg a , Zneg b => (-( (Zpos a) mod (Zpos b) )) 
  end.

Lemma Cdiv_cases : forall n d,
  ((n >= 0) -> (d > 0) -> Cdiv n d = n/d) /\
  ((n <= 0) -> (d > 0) -> Cdiv n d = -((-n)/d)) /\
  ((n >= 0) -> (d < 0) -> Cdiv n d = -(n/(-d))) /\
  ((n <= 0) -> (d < 0) -> Cdiv n d = (-n)/(-d)).
Proof.
  intros.
  destruct n as [|a|a] ; 
  destruct d as [|b|b] ;
  intuition ;
  by auto with zarith.
Qed.

Lemma Cmod_cases : forall n d,
  ((n >= 0) -> (d > 0) -> Cmod n d = n mod d) /\
  ((n <= 0) -> (d > 0) -> Cmod n d = -((-n) mod d)) /\
  ((n >= 0) -> (d < 0) -> Cmod n d = (n mod (-d))) /\
  ((n <= 0) -> (d < 0) -> Cmod n d = -((-n) mod (-d))).
Proof.
  intros.
  destruct n as [|a|a] ; 
  destruct d as [|b|b] ;
  intuition ;
  by auto with zarith.
Qed.

Theorem Cdiv_enclidian : 
  forall (n d : Z), 
  d <> 0 ->
  let q := Cdiv n d in let r := Cmod n d in
  (q * d + r = n).
Proof.
  intros n d NEQ q r.
  assert (OPP: forall p, (- (Zneg p) = Zpos p)) by auto with zarith.
  assert (NEG: forall p, (Zneg p = - (Zpos p))) by auto with zarith.
  destruct n as [|a|a] ; 
  destruct d as [|b|b] ; auto with zarith ;
  unfold Cdiv in q ; unfold Cmod in r ; 
  unfold q ; unfold r ; 
  repeat rewrite OPP ; repeat rewrite NEG ; 
  rewrite (Zmod_eq_full (Zpos a) (Zpos b)) ; try discriminate ;
  try ring.
Qed.

Lemma Cmod_less : forall n d,
  ((n >= 0) -> (d > 0) ->  0 <= Cmod n d <  d) /\
  ((n <= 0) -> (d > 0) -> -d <  Cmod n d <= 0) /\
  ((n >= 0) -> (d < 0) ->  0 <= Cmod n d < -d) /\
  ((n <= 0) -> (d < 0) ->  d <  Cmod n d <= 0).
Proof.
  intros.
  destruct n as [|a|a] ; 
  destruct d as [|b|b] ;
  intuition ; simpl ; forward ;
  generalize (Z_mod_lt (Zpos a) (Zpos b) (Zgt_pos_0 b)) ;
  repeat (replace (Zneg b) with (- Zpos b) by auto with zarith) ;
  intuition (auto with zarith).
Qed.

Lemma Zdiv_less :
  forall (n d : Z), (n > 0) -> (d > 0) -> (d * (n/d) <= n).
Proof.
  intros n d Npos Dpos.
  generalize (Zmod_eq n d).
  pose (x := (n/d)).
  fold x. intro H. generalize (H Dpos). clear H.
  pose (r := (n mod d)).
  fold r. intro H.
  generalize (Z_mod_lt n d).
  intro R. generalize (R Dpos). clear R. fold r.
  replace (d*x) with (x*d) by ring.
  omega.
Qed.


