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

(* This file is a contribution by Christine Paulin *)

(* $Id: WhyExn.v,v 1.8 2006/11/02 09:18:20 hubert Exp $ *)

Set Implicit Arguments.
Unset Strict Implicit.

Section Generic.

Variables U T : Set.

(* Exception monad *)
Inductive EM : Set :=
  | Val : T -> EM
  | Exn : U -> EM.

Variable Q : U -> Prop.
Variable P : T -> Prop.

Definition qcomb (v:EM) : Prop := match v with Val v => P v | Exn u => Q u end.
(* if v then P else Q *)

Inductive QEM (A:Set) : Set :=
  | Qval : A -> QEM A
  | Qexn : forall u:U, Q u -> QEM A.

Definition decomp : forall v:EM, qcomb v -> QEM ({t : T | P t}).
intros [t| u]; simpl; intro p.
apply Qval; exists t; trivial.
apply Qexn with u; trivial.
Defined.

Definition uncurry :
  forall (A:Set) (P:A -> Prop) (C:Set),
    (forall x:A, P x -> C) -> {x : A | P x} -> C.
simple destruct 2; intros.
apply H with x; trivial.
Defined.

Definition QEM_mon : forall A B:Set, (A -> B) -> QEM A -> QEM B.
simple destruct 2; intros.
apply Qval; auto.
apply Qexn with u; trivial.
Defined.

End Generic.

Definition decomp1 (T:Set) (P:T -> Prop) (E1:Set) (Q1:E1 -> Prop)
  (v:EM E1 T) (pv:qcomb Q1 P v) : QEM Q1 ({t : T | P t}) :=
  decomp (Q:=_) (P:=_) (v:=_) pv.

Definition decomp2 (T:Set) (P:T -> Prop) (E1:Set) (Q1:E1 -> Prop)
  (E2:Set) (Q2:E2 -> Prop) (v:EM E1 (EM E2 T))
  (pv:qcomb Q1 (qcomb Q2 P) v) : QEM Q1 (QEM Q2 ({t : T | P t})) :=
  QEM_mon (Q:=Q1)
    (uncurry (P:=_) (decomp1 (T:=T) (P:=P) (E1:=E2) (Q1:=Q2)))
    (decomp (Q:=_) (P:=_) (v:=_) pv).

Definition decomp3 (T:Set) (P:T -> Prop) (E1:Set) (Q1:E1 -> Prop)
  (E2:Set) (Q2:E2 -> Prop) (E3:Set) (Q3:E3 -> Prop)
  (v:EM E1 (EM E2 (EM E3 T))) (pv:qcomb Q1 (qcomb Q2 (qcomb Q3 P)) v) :
  QEM Q1 (QEM Q2 (QEM Q3 ({t : T | P t}))) :=
  QEM_mon (Q:=Q1)
    (uncurry (P:=_)
       (decomp2 (T:=T) (P:=P) (E1:=E2) (Q1:=Q2) (E2:=E3) (Q2:=Q3)))
    (decomp (Q:=_) (P:=_) (v:=_) pv).

Definition decomp4 (T:Set) (P:T -> Prop) (E1:Set) (Q1:E1 -> Prop)
  (E2:Set) (Q2:E2 -> Prop) (E3:Set) (Q3:E3 -> Prop) (E4:Set)
  (Q4:E4 -> Prop) (v:EM E1 (EM E2 (EM E3 (EM E4 T))))
  (pv:qcomb Q1 (qcomb Q2 (qcomb Q3 (qcomb Q4 P))) v) :
  QEM Q1 (QEM Q2 (QEM Q3 (QEM Q4 ({t : T | P t})))) :=
  QEM_mon (Q:=Q1)
    (uncurry (P:=_)
       (decomp3 (T:=T) (P:=P) (E1:=E2) (Q1:=Q2) (E2:=E3) (Q2:=Q3)
          (E3:=E4) (Q3:=Q4))) (decomp (Q:=_) (P:=_) (v:=_) pv).

