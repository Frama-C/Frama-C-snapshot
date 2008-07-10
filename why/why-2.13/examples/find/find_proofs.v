(* Load Programs. *)(**********************************************************************)
(*                                                                    *)
(* FIND, an historical example.                                       *)
(*                                                                    *)
(* The proof of this program was originally done by C. A. R. Hoare    *)
(* and fully detailed in the following paper:                         *)
(*                                                                    *)
(* C. A. R. Hoare, , Communications of the  *)
(* ACM, 14(1), 39--45, January 1971.                                  *)
(*                                                                    *)
(**********************************************************************)
(* Jean-Christophe FILLIATRE, February 98                             *)
(**********************************************************************)
(* find_proofs.v                                                      *)
(**********************************************************************)

Require Import find_spec.
Require Import find_lemmas.
Require Import Why.
Require Import Omega.

Lemma zero_f_SN : (0 <= f < Zsucc N)%Z.
Proof.
generalize le_1_f; generalize le_f_N; intros; omega.
Qed.

(* Lemmas to prove arrays bounds obligations. *)

Lemma bound_3 :
 forall (m n i r:Z) (A:array Z),
   i_invariant m n i r A ->
   (i <= n)%Z -> (access A i < r)%Z -> (Zsucc i <= n)%Z.
Proof.
unfold i_invariant.
intros m n i r A Hi le_i_n lt_Ai_r.
decompose [and] Hi.
case (Z_le_lt_eq_dec i n le_i_n).
intro.
 abstract omega.
intro eq_i_n.
 elim (H2 le_i_n).
 intros.
absurd (access A i < r)%Z.
cut (x = i).
 intro eq_x_i.
 rewrite eq_x_i in H0.
 abstract omega.
abstract omega.
assumption.
Qed.


Lemma bound_4 :
 forall (m n j r:Z) (A:array Z),
   j_invariant m n j r A ->
   (m <= j)%Z -> (r < access A j)%Z -> (m <= Zpred j)%Z.
Proof.
unfold j_invariant.
intros m n j r A Hj le_m_j lt_r_Aj.
decompose [and] Hj.
case (Z_le_lt_eq_dec m j le_m_j).
intro.
 unfold Zpred.
 abstract omega.
intro eq_m_j.
 elim (H2 le_m_j).
 intros.
absurd (r < access A j)%Z.
cut (x = j).
 intro eq_x_j.
 rewrite eq_x_j in H0.
 abstract omega.
abstract omega.
assumption.
Qed.


(* Some subgoals of the main proof *)

Lemma subgoal_1 :
 forall (m1 n1 i2 j2 i3:Z) (A A0 A1:array Z),
   m_invariant m1 A0 /\
   n_invariant n1 A0 /\ permut A0 A /\ (1 <= m1)%Z /\ (n1 <= N)%Z ->
   (m1 < n1)%Z ->
   (0 <= f < Zsucc N)%Z ->
   i_invariant m1 n1 i2 (access A0 f) A1 /\
   j_invariant m1 n1 j2 (access A0 f) A1 /\
   m_invariant m1 A1 /\
   n_invariant n1 A1 /\
   (0 <= j2)%Z /\
   (i2 <= N + 1)%Z /\
   termination i2 j2 m1 n1 (access A0 f) A1 /\ permut A1 A ->
   (i2 <= j2)%Z ->
   i_invariant m1 n1 i3 (access A0 f) A1 /\
   (i2 <= i3)%Z /\
   (i3 <= n1)%Z /\ termination i3 j2 m1 n1 (access A0 f) A1 ->
   (access A1 i3 < access A0 f)%Z ->
   i_invariant m1 n1 (Zsucc i3) (access A0 f) A1 /\
   (i2 <= Zsucc i3)%Z /\
   (Zsucc i3 <= n1)%Z /\ termination (Zsucc i3) j2 m1 n1 (access A0 f) A1.

Proof.
intros m1 n1 i2 j2 i3 A A0 A1 HH_43 HH_42 HH_40 HH_28 HH_27 HH_4 HH_2.
split.
apply Lemma_8_15.
 elim HH_4; auto.
decompose [and] HH_4.
 elim H.
 intros.
 abstract omega.
split; [ abstract omega | split ].
cut (Zsucc i3 <= n1)%Z.
 abstract omega.
apply bound_3 with (m := m1) (r := access A0 f) (A := A1); auto.
elim HH_4; auto.
 abstract omega.
(* term. *)
unfold termination.
 decompose [and] HH_4.
 elim H3.
intro.
 left.
 abstract omega.
intro.
 right.
 decompose [and] H2.
case (Z_le_lt_eq_dec i3 f H6).
  intro.
 abstract omega.
  intro eq_i3_f.
 absurd (access A1 i3 < access A0 f)%Z; auto.
  rewrite eq_i3_f.
 abstract omega.
Qed.


Lemma subgoal_2 :
 forall (m1 n1 i2 j2 i3 j3:Z) (A A0 A1:array Z),
   m_invariant m1 A0 /\
   n_invariant n1 A0 /\ permut A0 A /\ (1 <= m1)%Z /\ (n1 <= N)%Z ->
   (m1 < n1)%Z ->
   (0 <= f < Zsucc N)%Z ->
   i_invariant m1 n1 i2 (access A0 f) A1 /\
   j_invariant m1 n1 j2 (access A0 f) A1 /\
   m_invariant m1 A1 /\
   n_invariant n1 A1 /\
   (0 <= j2)%Z /\
   (i2 <= N + 1)%Z /\
   termination i2 j2 m1 n1 (access A0 f) A1 /\ permut A1 A ->
   (i2 <= j2)%Z ->
   (i_invariant m1 n1 i3 (access A0 f) A1 /\
    (i2 <= i3)%Z /\
    (i3 <= n1)%Z /\ termination i3 j2 m1 n1 (access A0 f) A1) /\
   (access A1 i3 >= access A0 f)%Z ->
   j_invariant m1 n1 j3 (access A0 f) A1 /\
   (j3 <= j2)%Z /\
   (m1 <= j3)%Z /\ termination i3 j3 m1 n1 (access A0 f) A1 ->
   (access A0 f < access A1 j3)%Z ->
   j_invariant m1 n1 (Zpred j3) (access A0 f) A1 /\
   (Zpred j3 <= j2)%Z /\
   (m1 <= Zpred j3)%Z /\
   termination i3 (Zpred j3) m1 n1 (access A0 f) A1.

Proof.
intros m1 n1 i2 j2 i3 j3 A A0 A1 HH_43 HH_42 HH_40 HH_28 HH_27 HH_25
 HH_9 HH_7.
split.
apply Lemma_9_17.
 elim HH_9; auto.
unfold j_invariant in HH_9.
 decompose [and] HH_9.
 elim H3.
 intros.
 abstract omega.
assumption.
cut (m1 <= Zpred j3)%Z.
 unfold Zpred.
  split; [ abstract omega | split; [ abstract omega | idtac ] ].
(* term. *)
unfold termination.
decompose [and] HH_9.
 elim H4.
intro.
 left.
 abstract omega.
intro.
 right.
 decompose [and] H3.
case (Z_le_lt_eq_dec f j3 H8).
  intro.
 abstract omega.
  intro eq_f_j3.
 absurd (access A0 f < access A1 j3)%Z; auto.
  rewrite <- eq_f_j3.
 abstract omega.
(* *)
apply bound_4 with (n := n1) (r := access A0 f) (A := A1); auto.
elim HH_9; auto.
 abstract omega.
Qed.


Lemma subgoal_3 :
 forall (m0 n0 i0 j0 i1 j1:Z) (A A0 A1 A2:array Z),
   array_length A = (N + 1)%Z ->
   m_invariant m0 A0 /\
   n_invariant n0 A0 /\ permut A0 A /\ (1 <= m0)%Z /\ (n0 <= N)%Z ->
   (m0 < n0)%Z ->
   (0 <= f < Zsucc N)%Z ->
   i_invariant m0 n0 i0 (access A0 f) A1 /\
   j_invariant m0 n0 j0 (access A0 f) A1 /\
   m_invariant m0 A1 /\
   n_invariant n0 A1 /\
   (0 <= j0)%Z /\
   (i0 <= N + 1)%Z /\
   termination i0 j0 m0 n0 (access A0 f) A1 /\ permut A1 A ->
   (i0 <= j0)%Z ->
   (i_invariant m0 n0 i1 (access A0 f) A1 /\
    (i0 <= i1)%Z /\
    (i1 <= n0)%Z /\ termination i1 j0 m0 n0 (access A0 f) A1) /\
   (access A1 i1 >= access A0 f)%Z ->
   (j_invariant m0 n0 j1 (access A0 f) A1 /\
    (j1 <= j0)%Z /\
    (m0 <= j1)%Z /\ termination i1 j1 m0 n0 (access A0 f) A1) /\
   (access A0 f >= access A1 j1)%Z ->
   (access A1 j1 <= access A0 f <= access A1 i1)%Z ->
   (i1 <= j1)%Z ->
   exchange A2 A1 i1 j1 ->
   (access A2 i1 <= access A0 f)%Z ->
   (access A0 f <= access A2 j1)%Z ->
   Zwf 0 (N + 2 + Zpred j1 - Zsucc i1) (N + 2 + j0 - i0) /\
   i_invariant m0 n0 (Zsucc i1) (access A0 f) A2 /\
   j_invariant m0 n0 (Zpred j1) (access A0 f) A2 /\
   m_invariant m0 A2 /\
   n_invariant n0 A2 /\
   (0 <= Zpred j1)%Z /\
   (Zsucc i1 <= N + 1)%Z /\
   termination (Zsucc i1) (Zpred j1) m0 n0 (access A0 f) A2 /\ permut A2 A.
Proof.
intros m0 n0 i0 j0 i1 j1 A A0 A1 A2 HN Inv_mn Test7 Pre12 Inv_ij Test4
 Inv_i Inv_j Pre10 Test3 Post7 Pre8 Pre7.
split.
(* [Zwf] *)
  unfold Zwf.
  decompose [and] Inv_i.
 decompose [and] Inv_j.
 unfold Zpred.
   abstract omega.
split.
(* [i_invariant] *)
  apply Lemma_8_10 with (j := j1) (A' := A1); try assumption.
  SameLength A2 A1.
 intuition; SameLength A1 A.
 omega.
  decompose [and] Inv_i.
 elim H1.
 abstract omega.
  abstract omega.
 abstract omega.
    decompose [and] Inv_i.
 assumption.
  decompose [and] Inv_j.
 assumption.
split.
(* [j_invariant] *)
  apply Lemma_9_11 with (i := i1) (A' := A1); try assumption.
  SameLength A2 A1.
 intuition; SameLength A1 A.
 omega.
  decompose [and] Inv_j.
 elim H1.
 abstract omega.
   decompose [and] Inv_i.
 elim H1.
 abstract omega.
   decompose [and] Inv_mn.
 assumption.
  decompose [and] Inv_j.
 assumption.
  decompose [and] Inv_i.
 assumption.
split.
(* [m_invariant] *)
  apply Lemma_12' with (i := i1) (j := j1) (A := A1).
  intuition; SameLength A1 A; omega.
  decompose [and] Inv_i.
 elim H1.
 abstract omega.
  auto with datatypes.
  decompose [and] Inv_ij.
 assumption.
split.
(* [n_invariant] *)
  apply Lemma_13' with (i := i1) (j := j1) (A := A1).
  intuition; SameLength A1 A.
 omega.
  decompose [and] Inv_i.
 elim H1.
 abstract omega.
  decompose [and] Inv_j.
 elim H1.
 abstract omega.
  auto with datatypes.
  decompose [and] Inv_ij.
 assumption.
split.
(* [0<=j-1] *)
  unfold Zpred.
 abstract omega.
split.
(* [i+1<=N+1] *)
  abstract omega.
split.
(* [termination] *)
  unfold termination.
   left.
 unfold Zpred.
   decompose [and] Inv_i.
 elim H1.
 intros.
   decompose [and] Inv_j.
 elim H8.
 intros.
   abstract omega.
(* [permut] *)
  decompose [and] Inv_ij.
 eauto with datatypes.
Qed.


Lemma subgoal_5 :
 forall (m1 n1 i2 j2:Z) (A A0 A1:array Z),
   m_invariant m1 A0 /\
   n_invariant n1 A0 /\ permut A0 A /\ (1 <= m1)%Z /\ (n1 <= N)%Z ->
   (m1 < n1)%Z ->
   (0 <= f < Zsucc N)%Z ->
   (i_invariant m1 n1 i2 (access A0 f) A1 /\
    j_invariant m1 n1 j2 (access A0 f) A1 /\
    m_invariant m1 A1 /\
    n_invariant n1 A1 /\
    (0 <= j2)%Z /\
    (i2 <= N + 1)%Z /\
    termination i2 j2 m1 n1 (access A0 f) A1 /\ permut A1 A) /\
   (i2 > j2)%Z ->
   (m1 < i2)%Z /\ (j2 < n1)%Z ->
   (f <= j2)%Z ->
   Zwf 0 (j2 - m1) (n1 - m1) /\
   m_invariant m1 A1 /\
   n_invariant j2 A1 /\ permut A1 A /\ (1 <= m1)%Z /\ (j2 <= N)%Z.

Proof.
intros m1 n1 i2 j2 A A0 A1 HH_44 HH_43 HH_41 HH_40 HH_39 HH_37.
decompose [and] HH_40.
split;
 [ idtac
 | split;
    [ assumption | split; [ idtac | split; [ assumption | idtac ] ] ] ].
abstract (unfold Zwf; elim H; elim H0; elim H1; elim H2; intros; omega).
apply Lemma_6_a with (m := m1) (n := n1) (i := i2) (r := access A0 f);
 auto.
abstract omega.
Qed.


Lemma subgoal_6 :
 forall (m1 n1 i2 j2:Z) (A A0 A1:array Z),
   m_invariant m1 A0 /\
   n_invariant n1 A0 /\ permut A0 A /\ (1 <= m1)%Z /\ (n1 <= N)%Z ->
   (m1 < n1)%Z ->
   (0 <= f < Zsucc N)%Z ->
   (i_invariant m1 n1 i2 (access A0 f) A1 /\
    j_invariant m1 n1 j2 (access A0 f) A1 /\
    m_invariant m1 A1 /\
    n_invariant n1 A1 /\
    (0 <= j2)%Z /\
    (i2 <= N + 1)%Z /\
    termination i2 j2 m1 n1 (access A0 f) A1 /\ permut A1 A) /\
   (i2 > j2)%Z ->
   (m1 < i2)%Z /\ (j2 < n1)%Z ->
   (f > j2)%Z ->
   (i2 <= f)%Z ->
   Zwf 0 (n1 - i2) (n1 - m1) /\
   m_invariant i2 A1 /\
   n_invariant n1 A1 /\ permut A1 A /\ (1 <= i2)%Z /\ (n1 <= N)%Z.
Proof.
intros m1 n1 i2 j2 A A0 A1 HH_44 HH_43 HH_41 HH_40 HH_39 HH_36 HH_33.
decompose [and] HH_40.
split;
 [ idtac
 | split;
    [ idtac | split; [ assumption | split; [ assumption | idtac ] ] ] ].
abstract (unfold Zwf; elim H; elim H1; elim H2; elim H3; intros; omega).
apply Lemma_6_b with (m := m1) (n := n1) (j := j2) (r := access A0 f);
 auto.
abstract omega.
Qed.


Lemma subgoal_7 :
 forall (m1 n1 i2 j2:Z) (A A0 A1:array Z),
   m_invariant m1 A0 /\
   n_invariant n1 A0 /\ permut A0 A /\ (1 <= m1)%Z /\ (n1 <= N)%Z ->
   (m1 < n1)%Z ->
   (0 <= f < Zsucc N)%Z ->
   (i_invariant m1 n1 i2 (access A0 f) A1 /\
    j_invariant m1 n1 j2 (access A0 f) A1 /\
    m_invariant m1 A1 /\
    n_invariant n1 A1 /\
    (0 <= j2)%Z /\
    (i2 <= N + 1)%Z /\
    termination i2 j2 m1 n1 (access A0 f) A1 /\ permut A1 A) /\
   (i2 > j2)%Z ->
   (m1 < i2)%Z /\ (j2 < n1)%Z ->
   (f > j2)%Z ->
   (i2 > f)%Z ->
   Zwf 0 (f - f) (n1 - m1) /\
   m_invariant f A1 /\
   n_invariant f A1 /\ permut A1 A /\ (1 <= f <= N)%Z.

Proof.
intros m1 n1 i2 j2 A A0 A1 HH_44 HH_43 HH_41 HH_40 HH_39 HH_36 HH_32.
decompose [and] HH_40.
split;
 [ idtac
 | split; [ idtac | split; [ idtac | split; [ assumption | idtac ] ] ] ].
abstract (unfold Zwf; elim H; elim H0; elim H1; elim H2; intros; omega).
apply Lemma_6_c1 with
 (m := m1) (n := n1) (i := i2) (j := j2) (r := access A0 f); auto.
abstract omega.
apply Lemma_6_c2 with
 (m := m1) (n := n1) (i := i2) (j := j2) (r := access A0 f); auto.
abstract omega.
abstract omega.
Qed.
