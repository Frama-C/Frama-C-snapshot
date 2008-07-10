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

(* $Id: WhyArrays.v,v 1.15 2008/04/15 08:41:44 filliatr Exp $ *)

(**************************************)
(* Functional arrays, for use in Why. *)
(**************************************)

(* This is an axiomatization of arrays.
 *
 * The type (array N T) is the type of arrays ranging from 0 to N-1 
 * which elements are of type T.
 *
 * Arrays are created with new, accessed with access and modified with update. 
 *
 * Operations of accessing and storing are not guarded, but axioms are.
 * So these arrays can be viewed as arrays where accessing and storing
 * out of the bounds has no effect.
 *)


Require Export WhyInt.

Set Implicit Arguments.
Unset Strict Implicit.


(* The type of arrays *)

Parameter raw_array : Set -> Set.

Definition array (T:Set) := prod Z (raw_array T).


(* Array length *)

Definition array_length (T:Set) (t:array T) : Z := let (n, _) := t in n.


(* Functions to create, access and modify arrays *)

Parameter raw_new : forall T:Set, T -> raw_array T.

Definition new (T:Set) (n:Z) (a:T) : array T := (n, raw_new a).

Parameter raw_access : forall T:Set, raw_array T -> Z -> T.

Definition access (T:Set) (t:array T) (i:Z) : T :=
  let (_, r) := t in raw_access r i.

Parameter
  raw_update : forall T:Set, raw_array T -> Z -> T -> raw_array T.

Definition update (T:Set) (t:array T) (i:Z) (v:T) : array T :=
  (array_length t, let (_, r) := t in raw_update r i v).


(* Update does not change length *)

Lemma array_length_update :
 forall (T:Set) (t:array T) (i:Z) (v:T),
   array_length (update t i v) = array_length t.
Proof.
trivial.
Qed.


(* Axioms *)

Axiom
  new_def :
    forall (T:Set) (n:Z) (v0:T) (i:Z),
      (0 <= i < n)%Z -> access (new n v0) i = v0.

Axiom
  update_def_1 :
    forall (T:Set) (t:array T) (v:T) (i:Z),
      (0 <= i < array_length t)%Z -> access (update t i v) i = v.

Axiom
  update_def_2 :
    forall (T:Set) (t:array T) (v:T) (i j:Z),
      (0 <= i < array_length t)%Z ->
      (0 <= j < array_length t)%Z ->
      i <> j -> access (update t i v) j = access t j.

Hint Resolve new_def update_def_1 update_def_2 : datatypes v62.


(* A tactic to simplify access in arrays *)

Ltac WhyArrays :=
  repeat rewrite update_def_1; repeat rewrite array_length_update.

Ltac AccessStore i j H :=
  elim (Z_eq_dec i j);
   [ intro H; rewrite H; rewrite update_def_1; WhyArrays
   | intro H; rewrite update_def_2; [ idtac | idtac | idtac | exact H ] ].

Ltac AccessSame := rewrite update_def_1; WhyArrays; try omega.

Ltac AccessOther := rewrite update_def_2; WhyArrays; try omega.

Ltac ArraySubst t := subst t; simpl; WhyArrays; try omega.

(* Syntax and pretty-print for arrays *)

(* <Warning> : Grammar is replaced by Notation *)

(***
Syntax constr level 0 :
  array_access
    [ << (access ($VAR $t) $c) >> ] -> [  $t  $c:L  ].
***)
