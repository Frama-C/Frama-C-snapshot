(* Load Programs. *)(**************************************************************************)
(*                                                                        *)
(* Proof of the Quicksort Algorithm.                                      *)
(*                                                                        *)
(* Jean-Christophe Filliâtre (LRI, Université Paris Sud)                  *)
(* August 1998                                                            *)
(*                                                                        *)
(**************************************************************************)

Require Import Why.
Require Import Partition.

Require Import Omega.

(* Here we prove the main lemma of quicksort:
 *
 * We have four arrays t0, t1, t2 and t3, and some indexes g,d,p
 * with the following properties
 *
 *                  g           p             d
 *   t0 :  |        |                         |        |
 *   t1 :  |  = t0  |  <= v    |v|    >=v     |  = t0  |
 *   t2 :  |  = t1  |  sorted  |v|    = t1    |  = t1  |
 *   t3 :  |  = t2  |  = t2    |v|   sorted   |  = t2  |
 *
 * and we have to prove that t3 is sorted and is a permutation of t0 
 *)

Lemma quicksort_lemma :
 forall (t0 t1 t2 t3:array Z) (g d p:Z),
   (0 <= g)%Z ->
   (g < d)%Z ->
   (d < array_length t0)%Z ->
   (g <= p <= d)%Z ->
   partition_p t1 g d p ->
   sub_permut g d t1 t0 ->
   sorted_array t2 g (Zpred p) ->
   sub_permut g (Zpred p) t2 t1 ->
   sorted_array t3 (Zsucc p) d ->
   sub_permut (Zsucc p) d t3 t2 ->
   sorted_array t3 g d /\ sub_permut g d t3 t0.
Proof.
intros t0 t1 t2 t3 g d p H1 H2 H3 H4 piv_t1 perm_t1 sort_t2 perm_t2
 sort_t3 perm_t3.
generalize (sub_permut_length perm_t1); intro HL1.
 generalize (sub_permut_length perm_t2); intro HL2.
 generalize (sub_permut_length perm_t3); intro HL3.
 rewrite <- HL1 in H3.
generalize
 (partition_p_permut_left H1 H2 H3 H4 piv_t1 (sub_permut_sym perm_t2)).
  intro piv_t2.
rewrite <- HL2 in H3.
generalize
 (partition_p_permut_right H1 H2 H3 H4 piv_t2 (sub_permut_sym perm_t3)).
  intro piv_t3.
generalize (sub_permut_is_permut perm_t1); intro.
  elim (sub_permut_id perm_t1); intros.
generalize (sub_permut_is_permut perm_t2); intro.
  elim (sub_permut_id perm_t2); intros.
generalize (sub_permut_is_permut perm_t3); intro.
  elim (sub_permut_id perm_t3); intros.
split.
(* sorted_array *)
unfold sorted_array.
 intros.
elim (Z_lt_ge_dec x (p - 1)); intros.
(* x < p-1 *)
unfold array_id in H10.
rewrite (H10 x); try omega.
 rewrite (H10 (x + 1)%Z); try omega.
unfold sorted_array in sort_t2.
apply sort_t2; unfold Zpred; omega.
elim (Z_lt_ge_dec x p); intros.
(* x = p-1 *)
elim piv_t3; intros.
elim H17; intros.
cut ((x + 1)%Z = p).
 intro Heq.
 rewrite Heq.
apply H19; omega.
omega.
elim (Z_lt_ge_dec x (p + 1)); intros.
(* x = p *)
elim piv_t3; intros.
elim H18; intros.
cut (x = p).
 intro Heq.
 rewrite Heq.
apply H19; omega.
omega.
(* x > p *)
unfold sorted_array in sort_t3.
apply sort_t3; unfold Zsucc; omega.

(* sub_permut *)
apply sub_permut_trans with (t' := t2).
elim (Z_le_gt_dec (Zsucc p) d); intro.
apply sub_permut_extension with (g := Zsucc p) (d := d); try omega.
assumption.
apply sub_permut_void with (g := Zsucc p) (d := d); try omega.
assumption.
apply sub_permut_trans with (t' := t1).
elim (Z_le_gt_dec g (Zpred p)); intro.
apply sub_permut_extension with (g := g) (d := Zpred p);
 try (unfold Zpred; omega).
assumption.
apply sub_permut_void with (g := g) (d := Zpred p);
 try (unfold Zpred; omega).
omega.
assumption.
assumption.
Qed.


(* The trivial case, when the segment of the array contains at most
 * one element.
 *)

Lemma quicksort_trivial :
 forall (t:array Z) (g d:Z),
   (0 <= g)%Z ->
   (g >= d)%Z ->
   (d < array_length t)%Z -> sorted_array t g d /\ sub_permut g d t t.
Proof.
intros t g d H1 H2 H3.
split.
unfold sorted_array.
intros.
absurd (x < d)%Z; omega.
auto with datatypes.
Qed.

    
