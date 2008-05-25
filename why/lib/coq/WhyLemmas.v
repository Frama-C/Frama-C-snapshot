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

(* $Id: WhyLemmas.v,v 1.21 2006/11/02 09:18:20 hubert Exp $ *)

(* lemmas used to build automatic proofs *)

(* lemmas used to build automatic proofs *)

Set Implicit Arguments.
Unset Strict Implicit.

Lemma loop_variant_1 :
 forall (A:Set) (v phi0:A) (I:Prop) (P:A -> Prop),
   v = phi0 -> I /\ P phi0 -> P v.
Proof.
intros.
 rewrite H.
 tauto.
Qed.

Lemma why_rewrite_var :
 forall (A:Set) (x t:A), x = t -> forall P:A -> Prop, P x -> P t.
Proof.
intros; case H; trivial.
Qed.
Implicit Arguments why_rewrite_var [A x t].

Lemma why_rewrite_var_left :
 forall (A:Set) (x t:A), x = t -> forall P:A -> Prop, P t -> P x.
Proof.
intros; rewrite H; trivial.
Qed.
Implicit Arguments why_rewrite_var_left [A x t].

Lemma why_boolean_case :
 forall (A B C D:Prop) (b:bool),
   (if b then A else B) -> (A -> C) -> (B -> D) -> if b then C else D.
Proof.
simple destruct b; intuition.
Qed.

Lemma why_boolean_wp :
 forall (A B:Prop) (q:bool -> Prop),
   (A -> q true) ->
   (B -> q false) -> forall b:bool, (if b then A else B) -> q b.
Proof.
simple destruct b; assumption.
Qed.
Implicit Arguments why_boolean_wp [A B].

Lemma why_boolean_if_1 :
 forall (q1t q1f q3t q3f:Prop) (q2:bool -> Prop),
   q1t ->
   forall (b:bool) (q2b:q2 b),
     if b
     then q1t /\ q2 true \/ q1f /\ q3t
     else q1t /\ q2 false \/ q1f /\ q3f.
Proof.
simple destruct b; auto.
Qed.

Lemma why_boolean_forall :
 forall q:bool -> Prop, q true /\ q false -> forall b:bool, q b.
Proof.
simple induction b; intuition.
Qed.
Implicit Arguments why_boolean_forall [].

Lemma why_boolean_discriminate : false = true -> forall p:Prop, p.
Proof.
intros; discriminate.
Qed.

Lemma why_boolean_destruct_1 :
  forall (b:bool) (p:Prop), ((true=b -> p) /\ (false=b -> p)) -> p.
Proof.
  destruct b; intuition.
Qed.

Lemma why_boolean_destruct_2 :
  forall (b:bool) (p:Prop), ((false=b -> p) /\ (true=b -> p)) -> p.
Proof.
  destruct b; intuition.
Qed.

Require Import WhyInt.
Require Import WhyTuples.

Parameter why_any_int : forall x_:unit, sig_1 Z (fun result:Z => True).
Parameter
  why_any_unit : forall x_:unit, sig_1 unit (fun result:unit => True).

Lemma why_asym_conj : forall (a b : Prop), a -> (a -> b) -> a /\ b.
Proof.
  tauto.
Qed.

Lemma why_split_iff : forall (a b : Prop), (a -> b) -> (b -> a) -> (a <-> b).
Proof.
  tauto.
Qed.
