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

(* $Id: WhyBool.v,v 1.13 2008/03/17 09:30:05 filliatr Exp $ *)

Require Import ZArith.
Require Import Sumbool.
Require Export Bool_nat.
Require Export Zwf.

Definition annot_bool :
  forall b:bool, {b' : bool | if b' then b = true else b = false}.
intro b.
exists b.
 case b; trivial.
Qed.

Definition if_then_else (A:Set) (a:bool) (b c:A) := if a then b else c.
Implicit Arguments if_then_else.

(* Logical connectives *)

Definition spec_and (A B C D:Prop) (b:bool) :=
  if b then A /\ C else B \/ D.

Definition prog_bool_and :
  forall Q1 Q2:bool -> Prop,
    sig Q1 ->
    sig Q2 ->
    {b : bool | if b then Q1 true /\ Q2 true else Q1 false \/ Q2 false}.
intros Q1 Q2 H1 H2.
elim H1.
 intro b1.
 elim H2.
 intro b2.
 case b1; case b2; intros.
exists true; auto.
exists false; auto.
 exists false; auto.
 exists false; auto.
Qed.

Definition spec_or (A B C D:Prop) (b:bool) :=
  if b then A \/ C else B /\ D.

Definition prog_bool_or :
  forall Q1 Q2:bool -> Prop,
    sig Q1 ->
    sig Q2 ->
    {b : bool | if b then Q1 true \/ Q2 true else Q1 false /\ Q2 false}.
intros Q1 Q2 H1 H2.
elim H1.
 intro b1.
 elim H2.
 intro b2.
 case b1; case b2; intros.
exists true; auto.
 exists true; auto.
 exists true; auto.
exists false; auto.
Qed.

Definition spec_not (A B:Prop) (b:bool) := if b then B else A.

Definition prog_bool_not :
  forall Q:bool -> Prop,
    sig Q -> {b : bool | if b then Q false else Q true}.
intros Q H.
elim H.
 intro b.
case b; intro.
exists false; auto.
 exists true; auto.
Qed.

(** Conversely, we build a [sumbool] out of a boolean, 
    for the need of validations *)

Definition btest (q:bool -> Prop) (b:bool) (p:q b) :
  {q true} + {q false} :=
  match b return q b -> {q true} + {q false} with
  | true => fun p => @left _ (q false) p
  | false => fun p => @right _ (q false) p
  end p.


(** Equality over booleans *)

Definition B_eq_dec : forall x y:bool, {x = y} + {x <> y}.
 decide equality.
 Qed.

Definition B_eq_bool (x y:bool) := bool_of_sumbool (B_eq_dec x y).

Definition B_noteq_bool (x y:bool) :=
  bool_of_sumbool (sumbool_not _ _ (B_eq_dec x y)).

(** Equality over type unit *)

Definition U_eq_dec : forall x y:unit, {x = y} + {x <> y}.
 decide equality.
 Qed.

Definition U_eq_bool (x y:unit) := bool_of_sumbool (U_eq_dec x y).

Definition U_noteq_bool (x y:unit) :=
  bool_of_sumbool (sumbool_not _ _ (U_eq_dec x y)).

