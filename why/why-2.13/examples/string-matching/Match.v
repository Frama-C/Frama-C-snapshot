(* Load Programs. *)(**************************************************************************)
(*                                                                        *)
(* Proof of the Knuth-Morris-Pratt Algorithm.                             *)
(*                                                                        *)
(* Jean-Christophe Filliâtre (LRI, Université Paris Sud)                  *)
(* November 1998                                                          *)
(*                                                                        *)
(**************************************************************************)

Require Export WhyArrays.

Set Implicit Arguments.
Unset Strict Implicit.


(* Here we define the property (match t1 i1 t2 i2 n) which expresses
 * that the two segments [i1...i1+n-1] of t1 and [i2...i2+n-1] of t2
 * are equal.
 *)

Inductive match_ (A:Set) (t1:array A) (i1:Z) (t2:array A) (i2 n:Z) :
Prop :=
    match_cons :
      (0 <= i1 <= array_length t1 - n)%Z ->
      (0 <= i2 <= array_length t2 - n)%Z ->
      (forall i:Z,
         (0 <= i < n)%Z -> access t1 (i1 + i) = access t2 (i2 + i)) ->
      match_ t1 i1 t2 i2 n.


(* Lemmas about match *)

Require Import Omega.

Section match_lemmas.

Variable A : Set.
Variable t1 : array A.
Variable t2 : array A.

Lemma match_empty :
 forall i1 i2:Z,
   (0 <= i1 <= array_length t1)%Z ->
   (0 <= i2 <= array_length t2)%Z -> match_ t1 i1 t2 i2 0.
Proof.
intros i1 i2 Hi1 Hi2.
apply match_cons.
 omega.
 omega.
intros.
 absurd (i < 0)%Z; omega.
Qed.

Lemma match_right_extension :
 forall i1 i2 n:Z,
   match_ t1 i1 t2 i2 n ->
   (i1 <= array_length t1 - n - 1)%Z ->
   (i2 <= array_length t2 - n - 1)%Z ->
   access t1 (i1 + n) = access t2 (i2 + n) ->
   match_ t1 i1 t2 i2 (n + 1).
Proof.
intros i1 i2 n Hmatch Hi1 Hi2 Hn.
elim Hmatch; intros Hi1' Hi2' Heq.
apply match_cons.
omega.
omega.
intros i Hi.
elim (Z_le_lt_eq_dec i n).
intro.
 apply Heq.
 omega.
intro H.
 rewrite H.
 auto.
omega.
Qed.

Lemma match_contradiction_at_first :
 forall i1 i2 n:Z,
   (0 < n)%Z -> access t1 i1 <> access t2 i2 -> ~ match_ t1 i1 t2 i2 n.
Proof.
intros i1 i2 n Hn Heq.
red.
 intro Hmatch.
 elim Hmatch; intros.
absurd (access t1 i1 = access t2 i2); [ assumption | idtac ].
replace i1 with (i1 + 0)%Z.
 replace i2 with (i2 + 0)%Z.
apply (H1 0%Z).
omega.
omega.
omega.
Qed.

Lemma match_contradiction_at_i :
 forall i1 i2 i n:Z,
   (0 < n)%Z ->
   (0 <= i < n)%Z ->
   access t1 (i1 + i) <> access t2 (i2 + i) -> ~ match_ t1 i1 t2 i2 n.
Proof.
intros i1 i2 i n Hn Hi Heq.
red.
 intro Hmatch.
 elim Hmatch; intros.
absurd (access t1 (i1 + i) = access t2 (i2 + i));
 [ assumption | idtac ].
apply (H1 i); omega.
Qed.
  
Lemma match_right_weakening :
 forall i1 i2 n n':Z,
   match_ t1 i1 t2 i2 n -> (n' < n)%Z -> match_ t1 i1 t2 i2 n'.
Proof.
intros i1 i2 n n' Hmatch Hn.
elim Hmatch; intros.
apply match_cons.
 omega.
 omega.
intros i Hi.
 apply H1; omega.
Qed.

Lemma match_left_weakening :
 forall i1 i2 n n':Z,
   match_ t1 (i1 - (n - n')) t2 (i2 - (n - n')) n ->
   (n' < n)%Z -> match_ t1 i1 t2 i2 n'.
Proof.
intros i1 i2 n n' Hmatch Hn.
decompose [match_] Hmatch.
apply match_cons.
 omega.
 omega.
intros i Hi.
replace (i1 + i)%Z with (i1 - (n - n') + (i + (n - n')))%Z.
replace (i2 + i)%Z with (i2 - (n - n') + (i + (n - n')))%Z.
apply H1.
omega.
 omega.
 omega.
Qed.

Lemma match_sym :
 forall i1 i2 n:Z, match_ t1 i1 t2 i2 n -> match_ t2 i2 t1 i1 n.
Proof.
intros i1 i2 n Hmatch.
decompose [match_] Hmatch.
apply match_cons.
 omega.
 omega.
intros i Hi.
 symmetry.
apply H1; omega.
Qed.

Variable t3 : array A.

Lemma match_trans :
 forall i1 i2 i3 n:Z,
   match_ t1 i1 t2 i2 n -> match_ t2 i2 t3 i3 n -> match_ t1 i1 t3 i3 n.
Proof.
intros i1 i2 i3 n H12 H23.
decompose [match_] H12.
 decompose [match_] H23.
apply match_cons.
 omega.
 omega.
intros i Hi.
 apply trans_equal with (y := access t2 (i2 + i)).
apply H1; omega.
apply H4; omega.
Qed.

Lemma match_left_extension :
 forall i j n:Z,
   (0 <= i)%Z ->
   (0 <= j)%Z ->
   (0 < n)%Z ->
   access t1 i = access t2 j ->
   match_ t1 (i + 1) t2 (j + 1) (n - 1) -> match_ t1 i t2 j n.
Proof.
intros i j n H1 H2 H3 H4 Hmatch.
decompose [match_] Hmatch.
apply match_cons.
 omega.
 omega.
intuition.
assert (i0 = 0%Z \/ (0 < i0)%Z).
 omega.
 intuition.
subst; ring (i + 0)%Z; ring (j + 0)%Z; assumption.
replace (i + i0)%Z with (i + 1 + (i0 - 1))%Z; try omega.
replace (j + i0)%Z with (j + 1 + (i0 - 1))%Z; try omega.
apply H5; omega.
Qed.

End match_lemmas.


  
