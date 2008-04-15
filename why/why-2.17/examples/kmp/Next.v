(* Load Programs. *)(**************************************************************************)
(*                                                                        *)
(* Proof of the Knuth-Morris-Pratt Algorithm.                             *)
(*                                                                        *)
(* Jean-Christophe Filliâtre (LRI, Université Paris Sud)                  *)
(* November 1998                                                          *)
(*                                                                        *)
(**************************************************************************)

Require Import Match.
Require Import ZArithRing.
Require Import Omega.

Set Implicit Arguments.
Unset Strict Implicit.

(* next[j] is the maximal n < j such that the n first elements of p
 * match the n last elemnts of p[0..j-1]
 *
 *          _______ ____ j____
 *     p = |_______|abcd|_____|
 *                  ____ n___________
 *                 |abcd|____________|
 *
 * This property is expressed by the predicate (Next p j n).
 *)

Section next_prop.

Variable A : Set.
Variable p : array A.

(* Definition *)
 
(* n is maximal *)
Inductive Next (j n:Z) : Prop :=
    Next_cons :
      (0 <= n < j)%Z ->
      match_ p (j - n) p 0 n ->
      (forall z:Z, (n < z < j)%Z -> ~ match_ p (j - z) p 0 z) ->
      Next j n.


(* Some properties of the predicate Next *)

Variable a : array A.

Lemma next_iteration :
 forall i j n:Z,
   (0 < j < array_length p)%Z ->
   (j <= i <= array_length a)%Z ->
   match_ a (i - j) p 0 j -> Next j n -> match_ a (i - n) p 0 n.
Proof.
intros i j n Hj Hi Hmatch Hnext.
elim Hnext; intros.
apply match_cons.
 omega.
 omega.
intros i0 Hi0.
apply trans_equal with (y := access p (j - n + i0)).
elim Hmatch; intros.
replace (i - n + i0)%Z with (i - j + (j - n + i0))%Z.
replace (j - n + i0)%Z with (0 + (j - n + i0))%Z.
apply H4.
omega.
 omega.
 omega.
elim H0; intros.
apply H4.
omega.
Qed.


Lemma next_is_maximal :
 forall i j n k:Z,
   (0 < j < array_length p)%Z ->
   (j <= i <= array_length a)%Z ->
   (i - j < k < i - n)%Z ->
   match_ a (i - j) p 0 j ->
   Next j n -> ~ match_ a k p 0 (array_length p).
Proof.
intros i j n k Hj Hi Hk Hmatch Hnext.
red.
 intro Hmax.
elim Hnext; intros.
absurd (match_ p (j - (i - k)) p 0 (i - k)).
apply H1; omega.
apply match_trans with (t2 := a) (i2 := k).
apply match_sym.
apply match_left_weakening with (n := j).
ring (k - (j - (i - k)))%Z.
 ring (j - (i - k) - (j - (i - k)))%Z.
 assumption.
omega.
apply match_right_weakening with (n := array_length p).
assumption.
omega.
Qed.

Lemma next_1_0 : (1 <= array_length p)%Z -> Next 1 0.
Proof.
intro HM.
apply Next_cons.
 omega.
apply match_empty; omega.
intros z Hz.
 absurd (0 < z)%Z; omega.
Qed.

End next_prop.
