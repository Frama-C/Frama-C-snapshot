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

(* $Id: WhyNTMonad.v,v 1.1 2008/04/15 07:44:30 regisgia Exp $ *)

(* The type of monad of non terminating computation. *)
Definition nt_m (A: Type) := nat -> (option A).

(* [nt_unit v] embeds a value into a computation. *)
Definition nt_unit : forall A, A -> nt_m A := fun A v => (fun n => Some v).
Implicit Arguments nt_unit [A].

(* [nt_bind x f] compose two computations. *)
Definition nt_bind : forall A B, nt_m A -> (A -> nt_m B) -> nt_m B :=
  fun A B m f =>
     (fun n => match m n with None => None | Some x => f x n end).

(* If the composition of two computations returns a value, then each
   computation has returned a value. *)
Lemma nt_bind_inversion : forall A B n x y z, (nt_bind A B x y) n = Some z -> 
  exists z', x n = Some z' /\ (y z') n = Some z.
Proof.
  intros A B n x y z H.
  unfold nt_bind in H.
  case_eq (x n); 
  [ intros a |  ]; intros Ha; rewrite Ha in H; [ exists a | ]; 
  (auto || congruence).
Qed.

Implicit Arguments nt_bind [A B].

(* The 'do notation' helps writing sequential programs. *)
Notation "'do' X <- A ; B" := (nt_bind A (fun X => B))
 (at level 200, X ident, A at level 100, B at level 200).

(* If you provide a natural number that is big enough, you can run the 
   computation. *)
Definition run A (n: nat) (m: nt_m A)  := m n.

(* An equivalence between computations. *)
Definition equiv_run A (m m': nt_m A) := forall n, run A n m = run A n m'.
Hint Unfold equiv_run.
Implicit Arguments equiv_run [A].

(* Monadic laws. *)

(* Unit is a left unit for bind. *)
Lemma unit_l: forall A B (x : A) (m : A -> nt_m B), 
  equiv_run (nt_bind (nt_unit x) m) (m x).
Proof. auto. Qed.

(* Unit is a right unit for bind. *)
Lemma unit_r: forall A (x: nt_m A), equiv_run (nt_bind x (nt_unit (A:=A))) x.
Proof. 
  intros A x n. 
  unfold nt_bind. unfold run.  
  case_eq (x n); auto.
Qed.

(* Bind is associative. *)
Lemma assoc: forall A B C (x: nt_m A) (m1 : A -> nt_m B) (m2: B -> nt_m C), 
equiv_run (nt_bind (nt_bind x m1) m2) (nt_bind x (fun y => nt_bind (m1 y) m2)).
Proof.
  intros A B C x m1 m2 n; unfold nt_bind; unfold run;
  case_eq (x n); [ intro a; case_eq (m1 a n) |]; auto.
Qed.


(* Recursion operator, iterates as long as a natural number is positive. *)
Definition nt_fix : forall A B, 
  ((A -> nt_m B) -> (A -> nt_m B)) -> A -> nt_m B :=
   fix nt_fix A B F x (n : nat) { struct n } : option B :=
      match n with
      | O => None (A := B)
      | S n => F (fun y _ => nt_fix A B F y n) x n
      end.
Implicit Arguments nt_fix [A B].

(* If a recursion has lead to a value, the natural number was positive. *)
Lemma nt_fix_progress : forall A B F x n y,
  (nt_fix (A:=A) (B:=B) F x n = Some y) -> exists k, n = S k.
Proof.
  destruct n as [ | k ]; simpl; [ congruence | exists k; auto ].
Qed.

(* If a recursion has lead to a value, an unfold of the functional has
   been done. *)
Lemma nt_fix_inversion : forall A B F x n y,
  nt_fix (A:=A) (B:=B) F x n = Some y ->
  exists k, 
    (n = S k /\ 
    nt_fix (A:=A) (B:=B) F x n = F (fun y _ => nt_fix (A:=A) (B:=B) F y k) x k).
Proof.
  intros A B F x n y H.
  destruct (nt_fix_progress _ _ _ _ _ _ H) as [ k Hk ]; 
  exists k; intuition; subst; auto.
Qed.

(* Proving an invariant by induction. *)
Lemma nt_fix_induction : forall A B F x n y (I: A -> B -> Prop),
  nt_fix (A:=A) (B:=B) F x n = Some y ->
  (forall n x' y', 
    F (fun y _ => nt_fix (A:=A) (B:=B) F y n) x' n = Some y' -> I x' y') ->
  I x y.
Proof.
  intros A B F x n y I H InvH;
  induction n;
  destruct (nt_fix_inversion _ _ _ _ _ _ H) as [ i Hi ]; destruct Hi;
  [ congruence
  | simpl in H; generalize ((InvH n x y) H); trivial ].
Qed.

(* Lifting of arrows. *)  
Definition nt_lift : forall A B, (A -> B) -> (nt_m A -> nt_m B) := 
  fun A B f => fun x => nt_bind x (fun k => nt_unit (f k)).
Implicit Arguments nt_lift [A B].
