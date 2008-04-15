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

(* $Id: WhyLemmas.v,v 1.2 2006/11/02 09:18:21 hubert Exp $ *)

(* lemmas used to build automatic proofs *)

(* lemmas used to build automatic proofs *)

Implicit Arguments On.

Lemma loop_variant_1 : 
  (A:Set)(v,phi0:A)(I:Prop)(P:A->Prop)
  v=phi0 -> (I /\ (P phi0)) -> (P v).
Proof.
Intros. Rewrite H. Tauto.
Save.

Lemma why_rewrite_var :
  (A:Set)(x,t:A)x=t->(P:A->Prop)(P x)->(P t).
Proof.
Intros; Case H; Trivial.
Save.
Implicits why_rewrite_var [1 2 3].

Lemma why_boolean_case :
  (A,B,C,D:Prop)
  (b:bool)(if b then A else B)->(A->C)->(B->D)->(if b then C else D).
Proof.
Destruct b; Intuition.
Save.

Lemma why_boolean_wp :
  (A,B:Prop)(q:bool->Prop)
  (A -> (q true)) -> (B -> (q false)) -> 
  (b:bool)(if b then A else B) -> (q b).
Proof.
Destruct b; Assumption.
Save.
Implicits why_boolean_wp [1 2].

Lemma why_boolean_if_1 :
  (q1t,q1f,q3t,q3f:Prop)(q2:bool->Prop)
  q1t->(b:bool)(q2b:(q2 b))
  (if b then (q1t /\ (q2 true))  \/ (q1f /\ q3t)
        else (q1t /\ (q2 false)) \/ (q1f /\ q3f)).
Proof.
Destruct b; Auto.
Save.

Lemma why_boolean_forall :
  (q:bool -> Prop)(q true) /\ (q false) -> (b:bool)(q b).
Proof.
Induction b; Intuition.
Save.
Implicits why_boolean_forall [].

Lemma why_boolean_discriminate : false=true->(p:Prop)p.
Proof.
Intros; Discriminate.
Save.

Require WhyInt.
Require WhyTuples.

Parameter why_any_int : (_: unit)(sig_1 Z [result: Z](True)).
Parameter why_any_unit : (_: unit)(sig_1 unit [result: unit](True)).
