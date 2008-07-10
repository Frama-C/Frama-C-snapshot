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

Require Import heap.

Set Implicit Arguments.
Unset Strict Implicit.

(* We will also need another property about a heap, which is to have
 * all his elements smaller or equal than a given value v.
 *
 * It is expressed by the predicate (inftree t n v k), inductively
 * defined as follows:
 *)

Inductive inftree (t:array Z) (n v:Z) : Z -> Prop :=
    inftree_cons :
      forall k:Z,
        (0 <= k <= n)%Z ->
        (access t k <= v)%Z ->
        ((2 * k + 1 <= n)%Z -> inftree t n v (2 * k + 1)%Z) ->
        ((2 * k + 2 <= n)%Z -> inftree t n v (2 * k + 2)%Z) ->
        inftree t n v k.

(* Some lemmas about inftree *)

Lemma inftree_1 :
 forall (t:array Z) (n v k:Z), inftree t n v k -> (access t k <= v)%Z.
Proof.
intros t n v k H.
 elim H; auto.
Qed.

Lemma inftree_id :
 forall (t1 t2:array Z) (n v k:Z),
   inftree t1 n v k ->
   (forall i:Z, (k <= i <= n)%Z -> access t1 i = access t2 i) ->
   inftree t2 n v k.
Proof.
intros t1 t2 n v k H.
 elim H; intros.
apply inftree_cons.
assumption.

rewrite <- (H6 k0).
 assumption.
 Omega'.

intro.
 apply H3.
 assumption.
intros i Hi.
 apply H6; Omega'.

intro.
 apply H5.
 assumption.
intros i Hi.
 apply H6; Omega'.
Qed.

Lemma inftree_2 :
 forall (t1 t2:array Z) (n v k j:Z),
   (n < array_length t1)%Z ->
   inftree t1 n v j ->
   exchange t2 t1 k j ->
   (k < j)%Z -> (access t1 k <= access t1 j)%Z -> inftree t2 n v j.
Proof.
intros t1 t2 n v k j Hn H.
 case H.
intros.
apply inftree_cons.
assumption.
decompose [exchange] H4.
 Omega'.

intro.
 apply inftree_id with (t1 := t1).
 auto.
decompose [exchange] H4.
intros i Hi.
 symmetry.
 apply H13.
Omega'.
 Omega'.
 Omega'.

intro.
 apply inftree_id with (t1 := t1).
 auto.
decompose [exchange] H4.
intros i Hi.
 symmetry.
 apply H13.
Omega'.
 Omega'.
 Omega'.
Qed.

Lemma inftree_trans :
 forall (t:array Z) (n k v v':Z),
   (v <= v')%Z -> inftree t n v k -> inftree t n v' k.
Proof.
intros t n k v v' Hvv' H.
elim H; intros.
apply inftree_cons.
assumption.
 Omega'.
 auto.
 auto.
Qed.

Lemma inftree_3 :
 forall (t:array Z) (n k:Z), heap t n k -> inftree t n (access t k) k.
Proof.
intros t n k H.
 elim H; intros.
apply inftree_cons.
assumption.
auto with zarith.
intro.
 apply inftree_trans with
  (v := access t (2 * k0 + 1)) (v' := access t k0).
  Omega'.
 auto.
intro.
 apply inftree_trans with
  (v := access t (2 * k0 + 2)) (v' := access t k0).
  Omega'.
 auto.
Qed.

Lemma inftree_all :
 forall (t:array Z) (n v:Z),
   inftree t n v 0 -> forall i:Z, (0 <= i <= n)%Z -> inftree t n v i.
Proof.
intros t n v H0 i H.
generalize H.
pattern i.
apply heap_induction.
auto.

intros.
elim (Z_le_gt_dec k n).
intro.
generalize H1 a.
case H2; intros.
intuition.

split.
intro; apply H5; omega.
intro; apply H6; omega.

intro.
 split; intro; absurd (k > n)%Z; omega.
intuition.
Qed.

Lemma inftree_0_right :
 forall (t:array Z) (n v:Z),
   inftree t n v 0 ->
   forall i:Z, (0 <= i <= n)%Z -> (access t i <= v)%Z.
Proof.
intros t n v H.
generalize (inftree_all H).
intros.
apply inftree_1 with (n := n).
exact (H0 i H1).
Qed.

Lemma inftree_0_left :
 forall (t:array Z) (n v:Z),
   (0 <= n)%Z ->
   (forall i:Z, (0 <= i <= n)%Z -> (access t i <= v)%Z) ->
   inftree t n v 0.
Proof.
intros.
cut (forall i:Z, (0 <= i <= n)%Z -> inftree t n v i).
intro.
apply H1; omega.

intros i Hi.
 generalize Hi.
replace i with (n - (n - i))%Z.
pattern (n - i)%Z.
apply Z_lt_induction.
intros.
apply inftree_cons.
assumption.

apply H0; omega.

intro.
replace (2 * (n - x) + 1)%Z with (n - (n - (2 * (n - x) + 1)))%Z.
apply H1; omega.
omega.

intro.
replace (2 * (n - x) + 2)%Z with (n - (n - (2 * (n - x) + 2)))%Z.
apply H1; omega.
omega.

omega.
 omega.
Qed.

Lemma inftree_exchange :
 forall (t1 t2:array Z) (n v:Z),
   (n < array_length t1)%Z ->
   inftree t1 n v 0 -> exchange t2 t1 0 n -> inftree t2 n v 0.
Proof.
intros.
apply inftree_0_left.
decompose [exchange] H1.
omega.

generalize (inftree_0_right H0).
intros.
decompose [exchange] H1.
elim (Z_lt_ge_dec 0 i); intro.
elim (Z_lt_ge_dec i n); intro.
rewrite (H9 i).
apply H2; omega.

omega.
 omega.
 omega.

replace i with n.
rewrite H8.
apply H2; omega.

omega.

replace i with 0%Z.
rewrite H7.
apply H2; omega.

omega.
Qed.

Lemma inftree_weakening :
 forall (t:array Z) (n v k:Z),
   (1 <= n < array_length t)%Z ->
   inftree t n v k -> (k <= n - 1)%Z -> inftree t (n - 1) v k.
Proof.
intros t n v k Hn Htree.
elim Htree; intros.
apply inftree_cons.
omega.
assumption.
intro; apply H2; omega.
intro; apply H4; omega.
Qed.
