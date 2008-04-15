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

(* Why compatibility file with Coq 7.3 (released version) *)

(*** Zwf *******************************************************************)

Require ZArith.
Require Export Wf_nat.
Require Omega.

(** Well-founded relations on Z. *)

(** We define the following family of relations on [Z x Z]: 

    [x (Zwf c) y]   iff   [x < y & c <= y]
 *)

Definition Zwf := [c:Z][x,y:Z] `c <= y` /\ `x < y`.

(** and we prove that [(Zwf c)] is well founded *)

Section wf_proof.

Variable c : Z.

(** The proof of well-foundness is classic: we do the proof by induction
    on a measure in nat, which is here [|x-c|] *)

Local f := [z:Z](absolu (Zminus z c)).

Lemma Zwf_well_founded : (well_founded Z (Zwf c)).
Red; Intros.
Assert (n:nat)(a:Z)(lt (f a) n)\/(`a<c`) -> (Acc Z (Zwf c) a).
Clear a; Induction n; Intros.
(** n= 0 *)
Case H; Intros.
Case (lt_n_O (f a)); Auto.
Apply Acc_intro; Unfold Zwf; Intros.
Assert False;Omega Orelse Contradiction.
(** inductive case *)
Case H0; Clear H0; Intro; Auto.
Apply Acc_intro; Intros.
Apply H.
Unfold Zwf in H1.
Case (Zle_or_lt c y); Intro; Auto with zarith.
Left.
Red in H0.
Apply lt_le_trans with (f a); Auto with arith.
Unfold f.
Apply absolu_lt; Omega.
Apply (H (S (f a))); Auto.
Save.

End wf_proof.

Hints Resolve Zwf_well_founded : datatypes v62.


(** We also define the other family of relations:

    [x (Zwf_up c) y]   iff   [y < x <= c]
 *)

Definition Zwf_up := [c:Z][x,y:Z] `y < x <= c`.

(** and we prove that [(Zwf_up c)] is well founded *)

Section wf_proof_up.

Variable c : Z.

(** The proof of well-foundness is classic: we do the proof by induction
    on a measure in nat, which is here [|c-x|] *)

Local f := [z:Z](absolu (Zminus c z)).

Lemma Zwf_up_well_founded : (well_founded Z (Zwf_up c)).
Proof.
Apply well_founded_lt_compat with f:=f.
Unfold Zwf_up f.
Intros.
Apply absolu_lt.
Unfold Zminus. Split.
Apply Zle_left; Intuition.
Apply Zlt_reg_l; Unfold Zlt; Rewrite <- Zcompare_Zopp; Intuition.
Save.

End wf_proof_up.

Hints Resolve Zwf_up_well_founded : datatypes v62.


(*** Bool_nat ***************************************************************)

Require Export Compare_dec.
Require Export Peano_dec.
Require Sumbool.

(** Any decidability function in type [sumbool] can be turned into a function
    returning a boolean with the corresponding specification: *)

Definition bool_of_sumbool : 
  (A,B:Prop) {A}+{B} -> { b:bool | if b then A else B }.
Proof.
Intros A B H.
Elim H; [ Intro; Exists true; Assumption
        | Intro; Exists false; Assumption ].
Save.
Implicits bool_of_sumbool.

(** The decidability of equality and order relations over
    type [nat] give some boolean functions with the adequate specification. *)

Definition notzerop := [n:nat] (sumbool_not ? ? (zerop n)).
Definition lt_ge_dec : (x,y:nat){(lt x y)}+{(ge x y)} := 
  [n,m:nat] (sumbool_not ? ? (le_lt_dec m n)).

Definition nat_lt_ge_bool := 
  [x,y:nat](bool_of_sumbool (lt_ge_dec x y)).
Definition nat_ge_lt_bool := 
  [x,y:nat](bool_of_sumbool (sumbool_not ? ? (lt_ge_dec x y))).

Definition nat_le_gt_bool := 
  [x,y:nat](bool_of_sumbool (le_gt_dec x y)).
Definition nat_gt_le_bool := 
  [x,y:nat](bool_of_sumbool (sumbool_not ? ? (le_gt_dec x y))).

Definition nat_eq_bool :=
  [x,y:nat](bool_of_sumbool (eq_nat_dec x y)).
Definition nat_noteq_bool := 
  [x,y:nat](bool_of_sumbool (sumbool_not ? ? (eq_nat_dec x y))).

Definition zerop_bool := [x:nat](bool_of_sumbool (zerop x)).
Definition notzerop_bool := [x:nat](bool_of_sumbool (notzerop x)).
