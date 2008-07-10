(**************************************************************************)
(*                                                                        *)
(* Proof of the Quicksort Algorithm.                                      *)
(*                                                                        *)
(* Jean-Christophe Filliâtre (LRI, Université Paris Sud)                  *)
(* August 1998                                                            *)
(*                                                                        *)
(**************************************************************************)

Require Import Why.

Require Import Omega.

Set Implicit Arguments.
Unset Strict Implicit.


(* Here we define the property  which expresses that,
 * on the segment [g,d] of the array t, all elements on the left of p
 * are lesser or equal than t[p] and all elements on the right of p
 * are greater or equal than t[p].
 *
 * So we introduce the properties array_le and array_ge which express
 * that all elements of a segment [g,d] are <= (resp. >=) than a given value
 *)

Inductive array_le (t:array Z) (g d v:Z) : Prop :=
    array_le_cons :
      (forall i:Z, (g <= i <= d)%Z -> (access t i <= v)%Z) ->
      array_le t g d v.

Inductive array_ge (t:array Z) (g d v:Z) : Prop :=
    array_ge_cons :
      (forall i:Z, (g <= i <= d)%Z -> (v <= access t i)%Z) ->
      array_ge t g d v.

Inductive partition_p (t:array Z) (g d p:Z) : Prop :=
    piv :
      (g <= p)%Z ->
      (p <= d)%Z ->
      array_le t g (p - 1) (access t p) ->
      array_ge t (p + 1) d (access t p) -> partition_p t g d p.


(* Lemmas about array_le *)

Lemma array_le_empty :
 forall (t:array Z) (g d v:Z), (d < g)%Z -> array_le t g d v.
Proof.
intros t g d v H.
constructor.
 intros.
absurd (g <= d)%Z; omega.
Qed.

Lemma array_le_right_extension :
 forall (t:array Z) (g d v:Z),
   array_le t g d v ->
   (access t (d + 1) <= v)%Z -> array_le t g (d + 1) v.
Proof.
intros t g d v H_le Hv.
elim H_le; intros.
constructor.
 intros.
elim (Z_eq_dec i (d + 1)); intro.
rewrite a; assumption.
apply H; omega.
Qed.

Lemma array_le_exchange :
 forall (t t':array Z) (g d v x y:Z),
   (0 <= g)%Z ->
   (d < array_length t)%Z ->
   array_le t g d v ->
   (d < x <= y)%Z -> exchange t t' x y -> array_le t' g d v.
Proof.
intros t t' g d v x y Hg Hd Hle Hxy Hex.
elim Hle; intros.
 elim Hex; intros.
constructor.
 intros.
rewrite <- H5; try omega.
apply H; assumption.
Qed.

(* Lemmas about array_ge *)

Lemma array_ge_empty :
 forall (t:array Z) (g d v:Z), (d < g)%Z -> array_ge t g d v.
Proof.
intros t g d v H.
constructor.
 intros.
absurd (g <= d)%Z; omega.
Qed.

Lemma array_ge_left_extension :
 forall (t:array Z) (g d v:Z),
   array_ge t g d v ->
   (v <= access t (g - 1))%Z -> array_ge t (g - 1) d v.
Proof.
intros t g d v H_ge Hv.
elim H_ge; intros.
constructor.
 intros.
elim (Z_eq_dec i (g - 1)); intro.
rewrite a; assumption.
apply H; omega.
Qed.

Lemma array_ge_exchange :
 forall (t t':array Z) (g d v x y:Z),
   (0 <= g)%Z ->
   (d < array_length t)%Z ->
   array_ge t g d v ->
   (x <= y < g)%Z -> exchange t t' x y -> array_ge t' g d v.
Proof.
intros t t' g d v x y Hg Hd Hge Hxy Hex.
elim Hge; intros.
 elim Hex; intros.
constructor.
 intros.
rewrite <- H5; try omega.
apply H; assumption.
Qed.


(* Lemmas about partition_p and sub_permut *)

Lemma partition_p_permut_left :
 forall (t t':array Z) (g d p:Z),
   (0 <= g)%Z ->
   (g < d)%Z ->
   (d < array_length t)%Z ->
   (g <= p <= d)%Z ->
   partition_p t g d p ->
   sub_permut g (Zpred p) t t' -> partition_p t' g d p.
Proof.
intros t t' g d p hyp1 hyp2 hyp3 hyp4 piv_t perm.
elim piv_t; intros.
cut (Zpred p < array_length t)%Z; [ intro | unfold Zpred; omega ].
generalize (sub_permut_function perm hyp1 H3).
 intro.
constructor; try assumption.
(* array_le *)
constructor.
 intros.
rewrite <- (sub_permut_eq perm (i:=p)).
elim H1; intros.
elim (H4 i); try (unfold Zpred; omega).
 intros.
elim H8; intros.
elim H9; intros.
 rewrite H11.
apply H6; unfold Zpred in H10; omega.
unfold Zpred; omega.
(* array_ge *)
constructor.
 intros.
rewrite <- (sub_permut_eq perm (i:=p)).
rewrite <- (sub_permut_eq perm (i:=i)).
elim H2; intros.
apply H6; omega.
unfold Zpred; omega.
unfold Zpred; omega.
Qed.


Lemma partition_p_permut_right :
 forall (t t':array Z) (g d p:Z),
   (0 <= g)%Z ->
   (g < d)%Z ->
   (d < array_length t)%Z ->
   (g <= p <= d)%Z ->
   partition_p t g d p ->
   sub_permut (Zsucc p) d t t' -> partition_p t' g d p.
Proof.
intros t t' g d p hyp1 hyp2 hyp3 hyp4 piv_t perm.
elim piv_t; intros.
cut (0 <= Zsucc p)%Z; [ intro | unfold Zpred; omega ].
generalize (sub_permut_function perm H3 hyp3).
 intro.
constructor; try assumption.
(* array_le *)
constructor.
 intros.
rewrite <- (sub_permut_eq perm (i:=p)).
rewrite <- (sub_permut_eq perm (i:=i)).
elim H1; intros.
apply H6; omega.
unfold Zsucc; omega.
unfold Zsucc; omega.
(* array_ge *)
constructor.
 intros.
rewrite <- (sub_permut_eq perm (i:=p)).
elim H2; intros.
elim (H4 i); try (unfold Zsucc; omega).
 intros.
elim H8; intros.
elim H9; intros.
 rewrite H11.
apply H6; unfold Zsucc in H10; omega.
unfold Zsucc; omega.
Qed.
