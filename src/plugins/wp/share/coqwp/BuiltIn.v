(**************************************************************************)
(*                                                                        *)
(*  The Why3 Verification Platform   /   The Why3 Development Team        *)
(*  Copyright 2010-2019   --   Inria - CNRS - Paris-Sud University        *)
(*                                                                        *)
(*  This software is distributed under the terms of the GNU Lesser        *)
(*  General Public License version 2.1, with the special exception        *)
(*  on linking described in file LICENSE.                                 *)
(*                                                                        *)
(**************************************************************************)

Require Export ZArith.
Require Export Rbase.

Require Import ClassicalEpsilon.

Class WhyType T := {
  why_inhabitant : T ;
  why_decidable_eq : forall x y : T, { x = y } + { x <> y }
}.

Notation int := Z.
Notation IZR := IZR (only parsing).

Global Instance int_WhyType : WhyType int.
Proof.
split.
exact Z0.
exact Z.eq_dec.
Qed.

Notation real := R.

Global Instance real_WhyType : WhyType real.
Proof.
split.
exact R0.
intros x y.
destruct (total_order_T x y) as [[H|H]|H] ;
  try (left ; exact H) ; right.
now apply Rlt_not_eq.
now apply Rgt_not_eq.
Qed.

Global Instance tuple_WhyType : forall T {T' : WhyType T} U {U' : WhyType U}, WhyType (T * U).
Proof.
intros T WT U WU.
split.
split ; apply why_inhabitant.
intros (x1,x2) (y1,y2).
destruct (why_decidable_eq x1 y1) as [H1|H1].
destruct (why_decidable_eq x2 y2) as [H2|H2].
left.
now apply f_equal2.
right.
now injection.
right.
now injection.
Qed.

Global Instance unit_WhyType : WhyType unit.
Proof.
split.
exact tt.
intros [] [].
now left.
Qed.

Global Instance bool_WhyType : WhyType bool.
Proof.
split.
exact false.
exact Bool.bool_dec.
Qed.

Global Instance func_WhyType : forall (a:Type) {a_WT:WhyType a} (b:Type) {b_WT:WhyType b}, WhyType (a -> b).
Proof.
intros.
repeat split.
exact (fun _ => why_inhabitant).
intros x y.
apply excluded_middle_informative.
Qed.
