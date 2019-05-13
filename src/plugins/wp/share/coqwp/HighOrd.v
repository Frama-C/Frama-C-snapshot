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

Require Import BuiltIn.

Definition func : forall (a:Type) (b:Type), Type.
intros a b.
exact (a -> b).
Defined.

Definition infix_at: forall {a:Type} {a_WT:WhyType a}
  {b:Type} {b_WT:WhyType b}, (a -> b) -> a -> b.
intros a aWT b bWT f x.
exact (f x).
Defined.

Definition pred (a: Type) := func a bool.
