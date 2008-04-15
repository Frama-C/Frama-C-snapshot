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

(*  Library about sorted (sub-)arrays / Nicolas Magaud, July 1998 *)

(* $Id: WhySorted.v,v 1.2 2006/11/02 09:18:21 hubert Exp $ *)

Require Export WhyArrays.
Require WhyPermut.

Require ZArithRing.
Require Omega.

Set Implicit Arguments.

(* Definition *)

Definition sorted_array :=
  [A:(array Z)][deb:Z][fin:Z]
     `deb<=fin` -> (x:Z) `x>=deb` -> `x<fin` -> (Zle #A[x] #A[`x+1`]).

(* Elements of a sorted sub-array are in increasing order *)

(* one element and the next one *)

Lemma sorted_elements_1 :
  (A:(array Z))(n:Z)(m:Z)
  (sorted_array A n m)
  -> (k:Z)`k>=n`
  -> (i:Z) `0<=i` -> `k+i<=m`
  -> (Zle (access A k) (access A `k+i`)).
Proof.
Intros A n m H_sorted k H_k i H_i.
Pattern i. Apply natlike_ind.
Intro.
Replace `k+0` with k; Omega. (*** Ring `k+0` => BUG ***)

Intros.
Apply Zle_trans with m:=(access A `k+x`).
Apply H0 ; Omega.

Unfold Zs.
Replace `k+(x+1)` with `(k+x)+1`.
Unfold sorted_array in H_sorted.
Apply H_sorted ; Omega.

Omega.

Assumption.
Save.

(* one element and any of the following *)

Lemma sorted_elements :
  (A:(array Z))(n:Z)(m:Z)(k:Z)(l:Z)
  (sorted_array A n m)
  -> `k>=n` -> `l<(array_length A)` -> `k<=l` -> `l<=m`
  -> (Zle (access A k) (access A l)).
Proof.
Intros.
Replace l with `k+(l-k)`.
Apply sorted_elements_1 with n:=n m:=m; [Assumption | Omega | Omega | Omega].
Omega.
Save.

Hints Resolve sorted_elements : datatypes v62.

(* A sub-array of a sorted array is sorted *)

Lemma sub_sorted_array : (A:(array Z))(deb:Z)(fin:Z)(i:Z)(j:Z)
      (sorted_array A deb fin) -> 
        (`i>=deb` -> `j<=fin` -> `i<=j` -> (sorted_array A i j)).
Proof.
Unfold sorted_array.
Intros.
Apply H ; Omega.
Save.

Hints Resolve sub_sorted_array : datatypes v62.

(* Extension on the left of the property of being sorted *)

Lemma left_extension : (A:(array Z))(i:Z)(j:Z)
   `i>0` -> `j<(array_length A)` -> (sorted_array A i j) 
   -> (Zle #A[`i-1`]  #A[i]) -> (sorted_array A `i-1` j).
Proof.
(Intros; Unfold sorted_array ; Intros).
Elim (Z_ge_lt_dec x i).   (* (`x >= i`) + (`x < i`) *)
Intro Hcut.
Apply H1 ; Omega.

Intro Hcut.
Replace x with `i-1`.
Replace `i-1+1` with i ; [Assumption | Omega].

Omega.
Save.

(* Extension on the right *)

Lemma right_extension : (A:(array Z))(i:Z)(j:Z)
   `i>=0` -> `j<(array_length A)-1` -> (sorted_array A i j) 
   -> (Zle #A[j]  #A[`j+1`]) -> (sorted_array A i `j+1`).
Proof.
(Intros; Unfold sorted_array ; Intros).
Elim (Z_lt_ge_dec x j).
Intro Hcut. 
Apply H1 ; Omega.

Intro HCut.
Replace x with j ; [Assumption | Omega].
Save.

(* Substitution of the leftmost value by a smaller value *) 

Lemma left_substitution : 
   (A:(array Z))(i:Z)(j:Z)(v:Z)
   `i>=0`  -> `j<(array_length A)`  -> (sorted_array A i j)
   -> (Zle v #A[i])
   -> (sorted_array (store A i v) i j).
Proof.
Intros A i j v H_i H_j H_sorted H_v.
Unfold sorted_array ; Intros.

Cut `x = i`\/`x > i`.
(Intro Hcut ; Elim Hcut ; Clear Hcut ; Intro).
Rewrite H2.
AccessSame; Try Omega. AccessOther; Try Omega.
Apply Zle_trans with m:=(access A i) ; [Assumption | Apply H_sorted ; Omega].
Do 2 (AccessOther; Try Omega).
Apply H_sorted ; Omega.
Omega.
Save.

(* Substitution of the rightmost value by a larger value *)

Lemma right_substitution : 
   (A:(array Z))(i:Z)(j:Z)(v:Z)
   `i>=0`  -> `j<(array_length A)`  -> (sorted_array A i j)
   -> (Zle #A[j] v)
   -> (sorted_array (store A j v) i j).
Proof.
Intros A i j v H_i H_j H_sorted H_v.
Unfold sorted_array ; Intros.

Cut `x = j-1`\/`x < j-1`.
(Intro Hcut ; Elim Hcut ; Clear Hcut ; Intro).
Rewrite H2.
Ring `j-1+1`.
AccessOther; Try Omega. 
Apply Zle_trans with m:=(access A j). 
Apply sorted_elements with n:=i m:=j ; Try Omega ; Assumption.
Assumption.
Do 2 (AccessOther; Try Omega).
Apply H_sorted ; Omega.

Omega.
Save.

(* Affectation outside of the sorted region *)

Lemma no_effect : 
   (A:(array Z))(i:Z)(j:Z)(k:Z)(v:Z)
   `i>=0`  -> `j<(array_length A)`  -> (sorted_array A i j)
   -> `0<=k<i`\/`j<k<(array_length A)`
   -> (sorted_array (store A k v) i j).
Proof.
Intros.
Unfold sorted_array ; Intros. 
Do 2 (AccessOther; Try Omega).
Apply H1 ; Assumption.
Save.

Lemma sorted_array_id : (t1,t2:(array Z))(g,d:Z)
  (sorted_array t1 g d) -> (array_id t1 t2 g d) -> (sorted_array t2 g d).
Proof.
Intros t1 t2 g d Hsorted Hid.
Unfold array_id in Hid.
Unfold sorted_array in Hsorted. Unfold sorted_array.
Intros Hgd x H1x H2x.
Rewrite <- (Hid x); [ Idtac | Omega ].
Rewrite <- (Hid `x+1`); [ Idtac | Omega ].
Apply Hsorted; Assumption.
Save.
