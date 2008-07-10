(* Load Programs. *)(**************************************************************************)
(*                                                                        *)
(* Proof of the Heapsort Algorithm.                                       *)
(*                                                                        *)
(* Jean-Christophe Filliâtre (LRI, Université Paris Sud)                  *)
(* March 1999                                                             *)
(*                                                                        *)
(**************************************************************************)

Require Import ZArith.
Require Import Why.
Require Import Omega.

Ltac Omega' := abstract omega.

Set Implicit Arguments.
Unset Strict Implicit.

(* First we define the heap structure.
 * 
 * The heap is represented in an array[0..N-1] with the two sons
 * of the ``root'' i at indexes 2i+1 and 2i+2.
 * 
 * A sub_array t[k..n] is a heap if the following conditions are satisfied:
 *  - t[k] <= t[2k+1]             (if 2k+1 <= n)
 *  - t[2k+1..n] is itself a heap (     ''     )
 *  - t[k] <= t[2k+2]             (if 2k+2 <= n)
 *  - t[2k+2..n] is itself a heap (     ''     )
 *
 * It is expressed by the predicate (heap t n k), defined as follows:
 *)

Inductive heap (t:array Z) (n:Z) : Z -> Prop :=
    heap_cons :
      forall k:Z,
        (0 <= k <= n)%Z ->
        ((2 * k + 1 <= n)%Z -> (access t k >= access t (2 * k + 1))%Z) ->
        ((2 * k + 1 <= n)%Z -> heap t n (2 * k + 1)%Z) ->
        ((2 * k + 2 <= n)%Z -> (access t k >= access t (2 * k + 2))%Z) ->
        ((2 * k + 2 <= n)%Z -> heap t n (2 * k + 2)%Z) -> heap t n k.

(* Some lemmas about heaps *)

(* A tree reduce to one element is a heap *)

Lemma heap_leaf :
 forall (t:array Z) (n k:Z),
   (0 <= k <= n)%Z -> (2 * k >= n)%Z -> heap t n k.
Proof.
intros t n k H1k H2k.
apply heap_cons;
 [ Omega'
 | intro; absurd (2 * k >= n)%Z; [ Omega' | assumption ]
 | intro; absurd (2 * k >= n)%Z; [ Omega' | assumption ]
 | intro; absurd (2 * k >= n)%Z; [ Omega' | assumption ]
 | intro; absurd (2 * k >= n)%Z; [ Omega' | assumption ] ].
Qed.

(* The tree with only two elements (it is the case when 2k+1 is equal
 * to n, the greatest element) is a heap as soon as t[k] <= t[2k+1] *)

Lemma heap_son :
 forall (t:array Z) (n:Z),
   (n >= 0)%Z ->
   forall k:Z,
     (2 * k + 1)%Z = n ->
     (access t k >= access t (2 * k + 1))%Z -> heap t n k.
Proof.
intros t n Hn k Hk Ht.
apply heap_cons;
 [ Omega'
 | intro; assumption
 | intro; apply heap_leaf; Omega'
 | intro; absurd ((2 * k + 1)%Z = n); Omega'
 | intro; absurd ((2 * k + 1)%Z = n); Omega' ].
Qed.

(* If we have a heap t[k..n] and t[k..n]=t'[k..n] then we have a heap
 * in t'[k..n] *)

Lemma heap_id :
 forall (t t':array Z) (n k:Z),
   heap t n k -> array_id t t' k n -> heap t' n k.
Proof.
intros t t' n k H_heap.
unfold array_id.
elim H_heap; intros; clear H_heap.
apply heap_cons.
 assumption.
intro; rewrite <- (H6 k0);
 [ rewrite <- (H6 (2 * k0 + 1)%Z); [ auto | idtac ] | idtac ];
 clear H0 H1 H2 H3 H4 H5 H6; Omega'.
intro; apply H2;
 [ assumption
 | intros i Hi; apply (H6 i); clear H0 H1 H2 H3 H4 H5 H6; Omega' ].
intro; rewrite <- (H6 k0);
 [ rewrite <- (H6 (2 * k0 + 2)%Z); [ auto | idtac ] | idtac ];
 clear H0 H1 H2 H3 H4 H5 H6; Omega'.
intro; apply H5;
 [ assumption
 | intros i Hi; apply (H6 i); clear H0 H1 H2 H3 H4 H5 H6; Omega' ].
Qed.

(* If t[k..n] is a heap then t[k..n-1] is a heap *)

Lemma heap_weakening :
 forall (t:array Z) (n k:Z),
   (1 <= n)%Z -> heap t n k -> (k <= n - 1)%Z -> heap t (n - 1) k.
Proof.
intros t n k Hn H.
 elim H; intros.
apply heap_cons.
clear H1 H2 H3 H4 H5 H6; omega.
intro; apply H1; clear H2 H3 H4 H5 H6; omega.
intro; apply H3; clear H1 H2 H4 H5 H6; omega.
intro; apply H4; clear H1 H2 H3 H5 H6; omega.
intro; apply H6; clear H1 H2 H3 H4 H5; omega.
Qed.

(* To prove the lemma heap_all (see further), we need an induction principle
 * following the structure of heaps *)

Lemma heap_induction :
 forall P:Z -> Prop,
   P 0%Z ->
   (forall k:Z, (0 <= k)%Z -> P k -> P (2 * k + 1)%Z /\ P (2 * k + 2)%Z) ->
   forall k:Z, (0 <= k)%Z -> P k.
Proof.
intros P H H0 k Hk; generalize Hk; pattern k; apply Z_lt_induction.
intros.
elim (Z_modulo_2 x); intro.
(* x = 2y+2 *)
elim (Z_le_lt_eq_dec 0 x).
 (* 0 < x *)
intro.
elim a; intros.
replace x with (2 * (x0 - 1) + 2)%Z.
elim (H0 (x0 - 1)%Z).
auto.
 omega.
apply H1; omega.
 omega.
(* 0 = x *)
intro H3.
 rewrite <- H3.
 assumption.
 assumption.
(* x = 2y+1 *)
elim b; intros.
rewrite p.
elim (H0 x0).
auto.
 omega.
apply H1; omega.
assumption.
Qed.

(* If t[0..n] is a heap, then every sub-array t[k..n] is also a heap *)

Lemma heap_all :
 forall (t:array Z) (n:Z),
   heap t n 0 -> forall i:Z, (0 <= i <= n)%Z -> heap t n i.
Proof.
intros t n H0 i H.
 generalize H.
pattern i.
apply heap_induction.
 auto.

intros.
split.
intro.
 generalize H3.
elim H2.
 intros.
apply H6; intuition.

omega.

intro.
 generalize H3.
 elim H2.
 intros.
apply H9; intuition.

omega.
intuition.
Qed.

