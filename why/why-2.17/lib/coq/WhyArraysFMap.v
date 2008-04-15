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

(* $Id: WhyArraysFMap.v,v 1.2 2008/04/15 15:02:37 regisgia Exp $ *)

(**************************************)
(* Functional arrays, for use in Why. *)
(**************************************)

(* 
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

Require Import Coq.FSets.FMapPositive.
Import FMapPositive.PositiveMap.
Require Import Coq.FSets.FMapFacts. 
Module F := Coq.FSets.FMapFacts.Facts (FMapPositive.PositiveMap).
Import F.  

(* The type of arrays 

   Arrays are implemented using functional maps of the standard library. *)

Record raw_array (T:Set) : Type := mk_raw_array { 
  default: T;
  carrier: FMapPositive.PositiveMap.t T
}.

Definition array (T:Set) := prod Z (raw_array T).

(* Array length *)

Definition array_length (T:Set) (t:array T) : Z := let (n, _) := t in n.
Definition array_length_ := array_length.


(* Functions to create, access and modify arrays *)

Definition raw_new (T:Set) (x:T) : raw_array T := mk_raw_array x (empty T).

Definition new (T:Set) (n:Z) (a:T) : array T := (n, raw_new a).

Definition set_carrier T x c := mk_raw_array (T:=T) (default x) c.
Hint Unfold set_carrier.

Definition raw_access (T:Set) (x:raw_array T) (n:Z) : T := 
  match n with
    | Z0 => 
      match PositiveMap.find xH (carrier x) with
        | Some v => v
        | None => default x
      end
    | Zpos n => 
      match PositiveMap.find (Psucc n) (carrier x) with
        | Some v => v
        | None => default x
      end
    | _ => default x
  end.

Definition access (T:Set) (t:array T) (i:Z) : T :=
  let (_, r) := t in raw_access r i.
Hint Unfold access.

Definition raw_update : forall T:Set, raw_array T -> Z -> T -> raw_array T :=
 fun T x n v => 
   match n with
     | Z0 => set_carrier (T:=T) x (add xH v (carrier x))
     | Zpos k => set_carrier (T:=T) x (add (Psucc k) v (carrier x))
     | _ => x
   end.

Definition update (T:Set) (t:array T) (i:Z) (v:T) : array T :=
  (array_length t, let (_, r) := t in raw_update r i v).
Hint Unfold update.

Definition elements (T:Set) (x:array T) := elements (carrier (snd x)).

Require Import List.

Definition from_list (T:Set) (default: T) (l: list T) := 
  let n := List.length l in
  let a := new (Z_of_nat n) default in
    snd (List.fold_left (fun (accu : (Z * array T)) => fun x => 
      let (i, a) := accu in 
        ((i + 1)%Z, update a i x)) l (0%Z, a)).

(* Update does not change length *)

Lemma array_length_update :
 forall (T:Set) (t:array T) (i:Z) (v:T),
   array_length (update t i v) = array_length t.
Proof. trivial. Qed.

(* Properties *)

Lemma new_def : forall (T:Set) (n:Z) (v0:T) (i:Z), 
(0 <= i < n)%Z ->
access (new n v0) i = v0.
Proof.
  intros T n v0 i H;
  destruct i; [|simpl; rewrite (gempty T)|]; auto.
Qed.

Lemma update_def_0 : 
  forall (T:Set) (t:array T) (v:T) (i:Z), 
  default (snd (update t i v)) = default (snd t).
Proof.
  intros T u v i. case_eq u; destruct i; auto.
Qed.

Opaque find add.

Lemma update_def_1 :
  forall (T:Set) (t:array T) (v:T) (i:Z),
    (0 <= i < array_length t)%Z -> access (update t i v) i = v.
Proof.
  intros T a v i H.
  destruct i; simpl; case_eq a; intros z r Ha;
  simpl;
  [ rewrite (find_1 (A:=T) (x:=xH) (e:=v) (m := (add xH v (carrier r))))
  | rewrite 
    (find_1 (A:=T) (x:=Psucc p) (e := v) (m := (add (Psucc p) v (carrier r))))
  | assert False; intuition
  ]; auto; red; apply add_1; unfold E.eq; auto.
Qed.

Lemma aux_find: forall T (t:t T) (i j: positive) (v: T), 
  i <> j -> PositiveMap.find i t = PositiveMap.find i (add j v t).
Proof.
  intros T a i j v Hdiff.
  generalize (find_mapsto_iff a i).
  generalize (find_mapsto_iff (add j v a) i).
  intros H1 H2.
  destruct (PositiveMap.find i a) as [ u | ]; [
    generalize (add_neq_mapsto_iff a (x:=j) (y:=i) v u)
  | destruct (PositiveMap.find i (add j v a)) as [ u | ]; 
    [ generalize (add_neq_mapsto_iff a (x:=j) (y:=i) v u) | ]
  ]; intuition; firstorder.
Qed.

Lemma update_def_2 :
  forall (T:Set) (t:array T) (v:T) (i j:Z),
    (0 <= i < array_length t)%Z ->
    (0 <= j < array_length t)%Z ->
    i <> j -> access (update t i v) j = access t j.
Proof.
  intros T a v i j Hi Hj diff; 
  generalize Psucc_not_one; intros;
  destruct i; destruct j; destruct Hi; destruct Hj; 
  case_eq a; intros z r eq_a; simpl; (congruence; auto with zarith) || idtac;
  [ rewrite <- (aux_find (carrier r) v (i := Psucc p) (j := xH))
  | rewrite -> (aux_find (carrier r) v (i := xH) (j := Psucc p))
  | rewrite -> (aux_find (carrier r) v (i := Psucc p0) (j := Psucc p)) ];
  trivial; (congruence || idtac); assert (p <> p0); [ congruence |
  intro He; generalize (Psucc_inj _ _ He); congruence
  ].
Qed.
  
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
