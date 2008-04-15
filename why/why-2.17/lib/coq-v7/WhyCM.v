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

(* $Id: WhyCM.v,v 1.2 2006/11/02 09:18:21 hubert Exp $ *)

Require Export WhyArrays.

(* A low level C model *)

Implicit Arguments On.

(* Addresses *)

Parameter adr : Set.

Axiom eq_adr_dec : (a1,a2:adr) { a1=a2 } + { ~a1=a2 }.

Parameter dummy_adr : adr.

(* Pointers *)

Inductive pointer : Set :=
  | null : pointer
  | Ref : adr -> Z -> pointer.

Lemma eq_null_dec : (p:pointer) { p=null } + { ~p=null }.
Proof.
Destruct p; Intuition.
Right; Intro H; Discriminate H.
Save.

(* Polymorphic Stores *)

Module Type AnySet.
  Parameter T : Set.
End AnySet.

Module Store [X : AnySet].

Definition cstore : Set := adr -> (array X.T).

Definition get [s:cstore; p:pointer] : X.T :=
  Cases p of
  | null => (access (s dummy_adr) `0`)
  | (Ref a ofs) => (access (s a) ofs) end.

Definition cstore_update [s:cstore; a:adr; v:(array X.T)] : cstore :=
  [a':adr]Cases (eq_adr_dec a' a) of 
          | (left _) => v
          | (right _) => (s a') end.

Definition set [s:cstore; p:pointer; v:X.T] : cstore :=
  Cases p of 
  | null => s
  | (Ref a ofs) => (cstore_update s a (store (s a) ofs v)) end.

(* Pointer validity *)

Definition is_valid [s:cstore; p:pointer] : Prop :=
  Cases p of 
  | null => False
  | (Ref a ofs) => `0 <= ofs < (array_length (s a))` end.

(* access/update lemmas *)

Lemma cstore_update_same : 
  (s:cstore)(a:adr)(v:(array X.T))
  ((cstore_update s a v) a) = v.
Proof.
Intros. Unfold cstore_update; Case (eq_adr_dec a a); Intuition.
Save.

Lemma get_set_same : 
  (s:cstore)(p:pointer)(v:X.T)
  (is_valid s p) -> (get (set s p v) p) = v.
Proof.
Destruct p; Simpl; Intuition.
Unfold cstore_update; Case (eq_adr_dec a a); Intuition.
Save.

Lemma get_set_eq : 
  (s:cstore)(p1,p2:pointer)(v:X.T)
  (is_valid s p1) -> p1=p2 -> (get (set s p1 v) p2) = v.
Proof.
Destruct p1; Destruct p2; Simpl; Intuition.
Discriminate H0.
Injection H0; Intros.
Unfold cstore_update; Subst a z; Case (eq_adr_dec a0 a0); Intuition.
Save.

Lemma get_set_other : 
  (s:cstore)(p1,p2:pointer)(v:X.T)
  (is_valid s p1) -> (is_valid s p2) -> ~p1=p2 -> 
  (get (set s p1 v) p2) = (get s p2).
Proof.
Destruct p1; Destruct p2; Simpl; Intuition.
Unfold cstore_update; Case (eq_adr_dec a0 a); Intuition.
Assert z=z0 \/ ~z=z0. Omega. Intuition.
Subst a0 z; Intuition.
Rewrite store_def_2; Subst a0; Intuition.
Save.

(* lemmas about [is_valid] *)

Lemma is_valid_set : 
  (s:cstore)(p1,p2:pointer)(v:X.T)
  (is_valid s p1) -> (is_valid (set s p2 v) p1).
Proof.
Unfold is_valid; Destruct p1; Intuition.
Unfold set; Case p2; Intuition.
Unfold cstore_update; Case (eq_adr_dec a a0); Intuition.
Subst a; WhyArrays; Trivial.
Save.

Hints Resolve is_valid_set.

End Store.

(* Instanciations on integers and pointers *)

Module Int : AnySet with Definition T := Z.
  Definition T := Z.
End Int.
Module IntStore := (Store Int).

Definition int_store := IntStore.cstore.
Definition get_int := IntStore.get.
Definition set_int := IntStore.set.
Definition is_valid_int := IntStore.is_valid.

Module Pointer : AnySet with Definition T := pointer.
  Definition T := pointer.
End Pointer.
Module PointerStore := (Store Pointer).

Definition pointer_store := PointerStore.cstore.
Definition pget := PointerStore.get.
Definition pset := PointerStore.set.
Definition is_valid_pointer := PointerStore.is_valid.
Hints Unfold pget pset is_valid_pointer.

(* The set of allocated addresses *)

Definition allocated := adr -> bool.
