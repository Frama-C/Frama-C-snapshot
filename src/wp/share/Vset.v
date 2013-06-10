(**************************************************************************)
(*                                                                        *)
(*  This file is part of WP plug-in of Frama-C.                           *)
(*                                                                        *)
(*  Copyright (C) 2007-2013                                               *)
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

Require Import ZArith.

Set Implicit Arguments.

(* -------------------------------------------------------------------------- *)
(* --- Set Library for Coq                                                --- *)
(* -------------------------------------------------------------------------- *)

Parameter set : Set -> Set.
Parameter member : forall A : Set, A -> (set A) -> Prop.
Hypothesis eqset : forall A : Set, forall (a b : set A), 
  a = b <-> (forall x, member x a <-> member x b).

Parameter descr : forall A : Set, (A -> Prop) -> (set A).
Hypothesis description : forall (A:Set) (f : A -> Prop) x, member x (descr f) <-> f x.

Definition union {A:Set} (a b : set A) := descr (fun x => member x a \/ member x b).
Definition inter {A:Set} (a b : set A) := descr (fun x => member x a /\ member x b).
Definition empty {A:Set} : set A := descr (fun x => False).
Definition disjoint {A:Set} (a b : set A) :=
  forall (x y : A), member x a -> member x b -> False.

Definition singleton {A : Set} (e : A) : set A := descr (fun x => x=e).

Lemma union_empty : forall A, forall a : set A, union a empty = a.
Proof.
  intros.
  apply eqset. intro x. unfold union.
  rewrite description. unfold empty. rewrite description. tauto.
Qed.

Lemma inter_empty : forall A, forall a : set A, inter a empty = empty.
Proof.
  intros.
  apply eqset. intro x. unfold inter.
  rewrite description. unfold empty. rewrite description. tauto.
Qed.

Definition range a b := descr (fun x => a <= x <= b)%Z.
Definition range_sup a := descr (fun x => a <= x)%Z.
Definition range_inf b := descr (fun x => x <= b)%Z.
Definition range_all := descr (fun (_:Z) => True).

