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

(* $Id: WhyArrays.v,v 1.3 2006/11/02 09:18:20 hubert Exp $ *)

(**************************************)
(* Functional arrays, for use in Why. *)
(**************************************)

(* This is an axiomatization of arrays.
 *
 * The type (array N T) is the type of arrays ranging from 0 to N-1 
 * which elements are of type T.
 *
 * Arrays are created with new, accessed with access and modified with store. 
 *
 * Operations of accessing and storing are not guarded, but axioms are.
 * So these arrays can be viewed as arrays where accessing and storing
 * out of the bounds has no effect.
 *)


Require Export WhyInt.

Implicit Arguments On.


(* The type of arrays *)

Parameter raw_array : Set -> Set.

Definition array [T:Set] := (prod Z (raw_array T)).


(* Array length *)

Definition array_length : (T:Set)(array T) -> Z :=
  [T:Set; t:(array T)]let (n, _) = t in n.


(* Functions to create, access and modify arrays *)

Parameter raw_new : (T:Set) T -> (raw_array T).

Definition new : (T:Set) Z -> T -> (array T) := 
  [T:Set; n:Z; a:T](n, (raw_new a)).

Parameter raw_access : (T:Set) (raw_array T) -> Z -> T.

Definition access : (T:Set) (array T) -> Z -> T := 
  [T:Set; t:(array T); i:Z]let (_, r) = t in (raw_access r i).

Parameter raw_store : (T:Set) (raw_array T) -> Z -> T -> (raw_array T).

Definition store : (T:Set) (array T) -> Z -> T -> (array T) :=
  [T:Set; t:(array T); i:Z; v:T]
  ((array_length t), let (_, r) = t in (raw_store r i v)).


(* Update does not change length *)

Lemma array_length_store : 
  (T:Set)(t:(array T))(i:Z)(v:T)
  (array_length (store t i v)) = (array_length t).
Proof.
Trivial.
Save.


(* Axioms *)

Axiom new_def : (T:Set)(n:Z)(v0:T)
                (i:Z) `0 <= i < n` -> (access (new n v0) i) = v0.

Axiom store_def_1 : (T:Set)(t:(array T))(v:T)
                    (i:Z) `0 <= i < (array_length t)` ->
                    (access (store t i v) i) = v.

Axiom store_def_2 : (T:Set)(t:(array T))(v:T)
                    (i:Z)(j:Z) 
		    `0 <= i < (array_length t)` -> 
		    `0 <= j < (array_length t)` ->
		    `i <> j` ->
                    (access (store t i v) j) = (access t j).

Hints Resolve new_def store_def_1 store_def_2 : datatypes v62.


(* A tactic to simplify access in arrays *)

Tactic Definition WhyArrays :=
  Repeat Rewrite store_def_1;
  Repeat Rewrite array_length_store.

Tactic Definition AccessStore i j H :=
  Elim (Z_eq_dec i j); [ 
    Intro H; Rewrite H; Rewrite store_def_1; WhyArrays
  | Intro H; Rewrite store_def_2; 
             [ Idtac | Idtac | Idtac | Exact H ] ].

Tactic Definition AccessSame :=
  Rewrite store_def_1; WhyArrays; Try Omega.

Tactic Definition AccessOther :=
  Rewrite store_def_2; WhyArrays; Try Omega.

Tactic Definition ArraySubst t :=
  Subst t; Simpl; WhyArrays; Try Omega.

(* Syntax and pretty-print for arrays *)

Grammar constr constr0 :=
  array_access
    [ "#" ident($t) "[" constr($c) "]" ] -> [ (access $t $c) ].

(***
Syntax constr level 0 :
  array_access
    [ << (access ($VAR $t) $c) >> ] -> [ "#" $t "[" $c:L "]" ].
***)
