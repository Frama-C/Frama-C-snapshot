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
(* find_lemmas.v                                                      *)
(**********************************************************************)

Require Import find_spec.
Require Import Why.
Require Import Omega.

(* Lemmas *)

Lemma Lemma_1 : forall A:array Z, m_invariant 1 A.
Proof.
intro A.
 unfold m_invariant.
split; [ exact le_1_f | intros ].
absurd (1 <= p)%Z; abstract omega.
Qed.

Lemma Lemma_2 : forall A:array Z, n_invariant N A.
Proof.
intro A.
 unfold n_invariant.
split; [ exact le_f_N | intros ].
absurd (q <= N)%Z; abstract omega.
Qed.

Lemma Lemma_3 :
 forall (m n:Z) (A:array Z),
   m_invariant m A -> n_invariant n A -> (m >= n)%Z -> found A.
Proof.
unfold m_invariant, n_invariant, found.
intros m n A H_m H_n le_n_m p q H1 H2 H3 H4.
cut (m = f).
 cut (n = f).
 intros eq_m_f eq_n_f.
decompose [and] H_m.
 decompose [and] H_n.
split.
case (Z_le_lt_eq_dec p f H2).
  intro lt_p_f.
 generalize (H0 p f).
 intros.
 abstract omega.
  intro eq_p_f.
 rewrite eq_p_f.
 abstract omega.
case (Z_le_lt_eq_dec f q H3).
  intro lt_f_q.
 generalize (H6 f q).
 intros.
 abstract omega.
  intro eq_f_q.
 rewrite eq_f_q.
 abstract omega.
abstract omega.
abstract omega.
Qed.

Lemma Lemma_4 :
 forall (m:Z) (A:array Z),
   m_invariant m A ->
   forall p:Z, (1 <= p < m)%Z -> (access A p <= access A f)%Z.
Proof.
unfold m_invariant.
 intros m A H_m p H_p.
decompose [and] H_m.
generalize (H0 p f).
intros.
 generalize le_f_N.
 abstract omega.
Qed.

Lemma Lemma_5 :
 forall (n:Z) (A:array Z),
   n_invariant n A ->
   forall q:Z, (n < q <= N)%Z -> (access A f <= access A q)%Z.
Proof.
unfold n_invariant.
 intros n A H_n q H_q.
decompose [and] H_n.
generalize (H0 f q).
intros.
 generalize le_1_f.
 abstract omega.
Qed.

Lemma Lemma_6_a :
 forall (m n i j r:Z) (A:array Z),
   (i > j)%Z ->
   m_invariant m A ->
   n_invariant n A ->
   i_invariant m n i r A ->
   j_invariant m n j r A -> (f <= j)%Z -> n_invariant j A.
Proof.
unfold m_invariant, n_invariant, i_invariant, j_invariant.
intros m n i j r A lt_i_j H_m H_n H_i H_j le_f_j.
decompose [and] H_m.
 decompose [and] H_n.
 decompose [and] H_i.
 decompose [and] H_j.
 clear H_m H_n H_i H_j.
split.
assumption.
intros.
 generalize (H5 p).
 generalize (H8 q).
abstract omega.
Qed.

Lemma Lemma_6_b :
 forall (m n i j r:Z) (A:array Z),
   (i > j)%Z ->
   m_invariant m A ->
   n_invariant n A ->
   i_invariant m n i r A ->
   j_invariant m n j r A -> (i <= f)%Z -> m_invariant i A.
Proof.
unfold m_invariant, n_invariant, i_invariant, j_invariant.
intros m n i j r A lt_j_i H_m H_n H_i H_j le_i_f.
decompose [and] H_m.
 decompose [and] H_n.
 decompose [and] H_i.
 decompose [and] H_j.
 clear H_m H_n H_i H_j.
split.
assumption.
intros.
 generalize (H5 p).
 generalize (H8 q).
abstract omega.
Qed.

Lemma Lemma_6_c1 :
 forall (m n i j r:Z) (A:array Z),
   (i > j)%Z ->
   m_invariant m A ->
   n_invariant n A ->
   i_invariant m n i r A ->
   j_invariant m n j r A -> (j < f < i)%Z -> m_invariant f A.
Proof.
unfold m_invariant, n_invariant, i_invariant, j_invariant.
intros m n i j r A lt_j_i H_m H_n H_i H_j lt_j_f_i.
decompose [and] H_m.
 decompose [and] H_n.
 decompose [and] H_i.
 decompose [and] H_j.
 clear H_m H_n H_i H_j.
split.
abstract omega.
intros.
generalize (H5 p).
 generalize (H8 q).
abstract omega.
Qed.

Lemma Lemma_6_c2 :
 forall (m n i j r:Z) (A:array Z),
   (i > j)%Z ->
   m_invariant m A ->
   n_invariant n A ->
   i_invariant m n i r A ->
   j_invariant m n j r A -> (j < f < i)%Z -> n_invariant f A.
Proof.
unfold m_invariant, n_invariant, i_invariant, j_invariant.
intros m n i j r A lt_j_i H_m H_n H_i H_j lt_j_f_i.
decompose [and] H_m.
 decompose [and] H_n.
 decompose [and] H_i.
 decompose [and] H_j.
 clear H_m H_n H_i H_j.
split.
abstract omega.
intros.
generalize (H5 p).
 generalize (H8 q).
abstract omega.
Qed.

(* Lemma 7 is not useful because we don't do a \verb statement
   to exit the loop. Actually, we do \verb and [Lemma_6_c1]
   and [Lemma_6_c2] are what we need *)

Lemma Lemma_8 :
 forall (i m r:Z) (A:array Z),
   (access A i <= r)%Z ->
   (m <= i)%Z /\
   (forall p:Z, (1 <= p)%Z -> (p < i)%Z -> (access A p <= r)%Z) ->
   (m <= Zsucc i)%Z /\
   (forall p:Z, (1 <= p)%Z -> (p < Zsucc i)%Z -> (access A p <= r)%Z).
Proof.
intros i m r A H H0.
decompose [and] H0.
split.
abstract omega.
intros.
elim (Z_le_lt_eq_dec p i); [ auto | idtac | abstract omega ].
intro Heq.
 rewrite Heq.
 auto.
Qed.

Lemma Lemma_9 :
 forall (j n r:Z) (A:array Z),
   (r <= access A j)%Z ->
   (j <= n)%Z /\
   (forall q:Z, (j < q)%Z -> (q <= N)%Z -> (r <= access A q)%Z) ->
   (Zpred j <= n)%Z /\
   (forall q:Z, (Zpred j < q)%Z -> (q <= N)%Z -> (r <= access A q)%Z).
Proof.
intros j n r A H H0.
decompose [and] H0.
split.
unfold Zpred.
 abstract omega.
intros.
elim (Z_le_lt_eq_dec j q).
auto.
intro Heq.
 rewrite <- Heq.
 auto.
unfold Zpred in H3.
 abstract omega.
Qed.

Lemma Lemma_10 :
 forall (m i j r:Z) (A A':array Z),
   (m <= i <= j)%Z ->
   exchange A A' i j ->
   (forall p:Z, (1 <= p)%Z -> (p < i)%Z -> (access A p <= r)%Z) ->
   (m <= i)%Z /\
   (forall p:Z, (1 <= p)%Z -> (p < i)%Z -> (access A' p <= r)%Z).
Proof.
intros m i j r A A' H_i_j H_ex H_A.
elim H_ex.
 intros H_l H_i H_j H_Ai H_Aj H_Ak.
split; [ abstract omega | intros p H1 H2 ].
generalize (H_Ak p).
 intros.
generalize (H_A p).
 intros.
 abstract omega.
Qed.

Lemma Lemma_8_10_1 :
 forall (m n i j r:Z) (A A':array Z),
   (m <= i <= j)%Z ->
   (n <= N)%Z ->
   (1 <= m)%Z ->
   exchange A A' i j ->
   (access A i <= r)%Z ->
   (r <= access A j)%Z ->
   i_invariant m n i r A' ->
   j_invariant m n j r A' -> i_invariant m n i r A.
Proof.
intros m n i j r A A' H_i_j le_n_N le_1_m H_ex H_i_r H_r_j H_i H_j.
unfold i_invariant.
 unfold i_invariant in H_i.
 decompose [and] H_i.
split; [ abstract omega | split ].
intros p H1' H2'.
 cut (exchange A' A i j).
 intro H_ex'.
elim (Lemma_10 m i j r A' A H_i_j H_ex' H1).
auto.
 auto with datatypes.
intro.
 exists j.
   unfold j_invariant in H_j.
 decompose [and] H_j.
  abstract omega.
Qed.

Lemma Lemma_8_10 :
 forall (m n i j r:Z) (A A':array Z),
   array_length A = (N + 1)%Z ->
   (m <= i <= j)%Z ->
   (n <= N)%Z ->
   (1 <= m)%Z ->
   exchange A A' i j ->
   (access A i <= r)%Z ->
   (r <= access A j)%Z ->
   i_invariant m n i r A' ->
   j_invariant m n j r A' -> i_invariant m n (Zsucc i) r A.
Proof.
intros m n i j r A A' Hl H_i_j le_n_N le_1_m H_ex H_i_r H_r_j H_i H_j.
cut (i_invariant m n i r A).
 intro H_int.
 unfold i_invariant.
 unfold i_invariant in H_int.
 decompose [and] H_int.
split; [ abstract omega | split ].
intros p H1' H2'.
 elim (Lemma_8 i m r A H_i_r (conj H H1)); auto.
unfold j_invariant in H_j.
 decompose [and] H_j.
cut (i <= j)%Z.
 intro le_i_j.
 case (Z_le_lt_eq_dec i j le_i_j).
   intros.
 exists j.
 abstract omega.
intros eq_i_j lt_i_n.
  exists (Zsucc i).
  split; [ abstract omega | split; [ assumption | idtac ] ].
  generalize (exchange_id H_ex eq_i_j).
 intro Hk.
  cut (access A (Zsucc i) = access A' (Zsucc i)).
 intro Hr.
 rewrite Hr.
  apply (H4 (Zsucc i)); abstract omega.
  apply (Hk (Zsucc i)); try omega.
omega.
apply Lemma_8_10_1 with (j := j) (A' := A'); assumption.
Qed.

Lemma Lemma_11 :
 forall (n i j r:Z) (A A':array Z),
   array_length A = (N + 1)%Z ->
   (i <= j <= n)%Z ->
   exchange A A' i j ->
   (forall q:Z, (j < q)%Z -> (q <= N)%Z -> (r <= access A q)%Z) ->
   (j <= n)%Z /\
   (forall q:Z, (j < q)%Z -> (q <= N)%Z -> (r <= access A' q)%Z).
Proof.
intros n i j r A A' HN H_i_j H_ex H_A.
elim H_ex.
 intros Hl H_i H_j H_Ai H_Aj H_Ak.
split; [ abstract omega | intros q H1 H2 ].
generalize (H_Ak q).
 intros.
generalize (H_A q).
 intros.
 abstract omega.
Qed.

Lemma Lemma_9_11_1 :
 forall (m n i j r:Z) (A A':array Z),
   array_length A = (N + 1)%Z ->
   (i <= j <= n)%Z ->
   (n <= N)%Z ->
   (1 <= m)%Z ->
   exchange A A' i j ->
   (access A i <= r)%Z ->
   (r <= access A j)%Z ->
   j_invariant m n j r A' ->
   i_invariant m n i r A' -> j_invariant m n j r A.
Proof.
intros m n i j r A A' HN H_i_j le_n_N le_1_m H_ex H_i_r H_r_j H_j H_i.
unfold j_invariant.
 unfold j_invariant in H_j.
 decompose [and] H_j.
split; [ abstract omega | split ].
intros q H1' H2'.
 cut (exchange A' A i j).
 intro H_ex'.
SameLength A A'.
 rewrite H0 in HN.
elim (Lemma_11 n i j r A' A HN H_i_j H_ex' H1).
auto.
 auto with datatypes.
intro.
 exists i.
   unfold i_invariant in H_i.
 decompose [and] H_i.
  abstract omega.
Qed.

Lemma Lemma_9_11 :
 forall (m n i j r:Z) (A A':array Z),
   array_length A = (N + 1)%Z ->
   (i <= j <= n)%Z ->
   (n <= N)%Z ->
   (1 <= m)%Z ->
   exchange A A' i j ->
   (access A i <= r)%Z ->
   (r <= access A j)%Z ->
   j_invariant m n j r A' ->
   i_invariant m n i r A' -> j_invariant m n (Zpred j) r A.
Proof.
intros m n i j r A A' HN H_i_j le_n_N le_1_m H_ex H_i_r H_r_j H_j H_i.
cut (j_invariant m n j r A).
 intro H_int.
 unfold j_invariant.
 unfold j_invariant in H_int.
 decompose [and] H_int.
split; [ unfold Zpred; abstract omega | split ].
intros q H1' H2'.
 elim (Lemma_9 j n r A H_r_j (conj H H1)); auto.
unfold i_invariant in H_i.
 decompose [and] H_i.
cut (i <= j)%Z.
 intro le_i_j.
 case (Z_le_lt_eq_dec i j le_i_j).
   intros.
 exists i.
 unfold Zpred.
 abstract omega.
intros eq_i_j lt_m_j.
  exists (Zpred j).
   split;
    [ unfold Zpred; unfold Zpred in lt_m_j; abstract omega
    | split;
       [ unfold Zpred; unfold Zpred in lt_m_j; abstract omega | idtac ] ].
  generalize (exchange_id H_ex eq_i_j).
 intro Hk.
  cut (access A (Zpred j) = access A' (Zpred j)).
 intro Hr.
 rewrite Hr.
  apply (H4 (Zpred j)); unfold Zpred; unfold Zpred in lt_m_j;
   abstract omega.
  apply (Hk (Zpred j)); unfold Zpred; abstract omega.
abstract omega.
apply Lemma_9_11_1 with (i := i) (A' := A'); assumption.
Qed.

Lemma Lemma_12 :
 forall (m i j:Z) (A A':array Z),
   array_length A = (N + 1)%Z ->
   (m <= i <= j)%Z ->
   exchange A A' i j ->
   (forall p q:Z,
      (1 <= p)%Z ->
      (p < m)%Z ->
      (m <= q)%Z -> (q <= N)%Z -> (access A p <= access A q)%Z) ->
   forall p q:Z,
     (1 <= p)%Z ->
     (p < m)%Z ->
     (m <= q)%Z -> (q <= N)%Z -> (access A' p <= access A' q)%Z.
Proof.
intros m i j A A' HN H_m_i_j H_ex H_A p q H1 H2 H3 H4.
elim H_ex.
 intros Hl H_i H_j H_ij H_ji H_k.
cut (access A' p = access A p).
 intro H_A'p_Ap.
case (Z_eq_dec q i).
  intro eq_q_i.
  generalize (H_A p j H1 H2).
 intro H6.
  rewrite eq_q_i.
 abstract omega.
intro not_eq_q_i.
 case (Z_eq_dec q j).
  intro eq_q_j.
  generalize (H_A p i H1 H2).
 intro H6.
  rewrite eq_q_j.
 abstract omega.
intro not_eq_q_j.
  generalize (H_k q).
 intros.
  generalize (H_A p q).
 intros.
  abstract omega.
generalize (H_k p).
 intros.
 abstract omega.
Qed.

Lemma Lemma_12' :
 forall (m i j:Z) (A A':array Z),
   array_length A = (N + 1)%Z ->
   (m <= i <= j)%Z ->
   exchange A A' i j -> m_invariant m A -> m_invariant m A'.
Proof.
unfold m_invariant.
intros m i j A A' HN H_m_i_j H_ex H_m.
decompose [and] H_m.
split; [ assumption | idtac ].
exact (Lemma_12 m i j A A' HN H_m_i_j H_ex H0).
Qed.

Lemma Lemma_13 :
 forall (n i j:Z) (A A':array Z),
   array_length A = (N + 1)%Z ->
   (1 <= i)%Z ->
   (i <= j <= n)%Z ->
   exchange A A' i j ->
   (forall p q:Z,
      (1 <= p)%Z ->
      (p <= n)%Z ->
      (n < q)%Z -> (q <= N)%Z -> (access A p <= access A q)%Z) ->
   forall p q:Z,
     (1 <= p)%Z ->
     (p <= n)%Z ->
     (n < q)%Z -> (q <= N)%Z -> (access A' p <= access A' q)%Z.
Proof.
intros n i j A A' HN le_1_i H_n_i_j H_ex H_A p q H1 H2 H3 H4.
elim H_ex.
 intros Hl H_i H_j H_ij H_ji H_k.
cut (access A' q = access A q).
 intro H_A'q_Aq.
case (Z_eq_dec p i).
  intro eq_p_i.
  generalize (H_A j q).
 intro H6.
  rewrite eq_p_i.
 abstract omega.
intro not_eq_p_i.
 case (Z_eq_dec p j).
  intro eq_p_j.
  generalize (H_A i q).
 intro H6.
  rewrite eq_p_j.
 abstract omega.
intro not_eq_p_j.
  generalize (H_k p).
 intros.
  generalize (H_A p q).
 intros.
  abstract omega.
generalize (H_k q).
 intros.
 abstract omega.
Qed.

Lemma Lemma_13' :
 forall (n i j:Z) (A A':array Z),
   array_length A = (N + 1)%Z ->
   (1 <= i)%Z ->
   (i <= j <= n)%Z ->
   exchange A A' i j -> n_invariant n A -> n_invariant n A'.
Proof.
unfold n_invariant.
intros n i j A A' HN le_1_i H_i_j_n H_ex H_n.
decompose [and] H_n.
split; [ assumption | idtac ].
exact (Lemma_13 n i j A A' HN le_1_i H_i_j_n H_ex H0).
Qed.

Lemma Lemma_14 :
 forall (m n:Z) (A:array Z),
   (m <= f <= n)%Z ->
    exists p : Z, (m <= p)%Z /\ (p <= n)%Z /\ (access A f <= access A p)%Z.
Proof.
intros m n A H_m_f_n.
exists f.
 abstract omega.
Qed.

Lemma Lemma_4_14 :
 forall (m n:Z) (A:array Z),
   (m <= f <= n)%Z ->
   m_invariant m A -> i_invariant m n m (access A f) A.
Proof.
intros m n A H_f H_m.
unfold i_invariant.
split; [ abstract omega | split ].
intros.
 eapply Lemma_4; eauto.
intro.
 apply Lemma_14; auto.
Qed.

Lemma Lemma_14' :
 forall (m n:Z) (A:array Z),
   (m <= f <= n)%Z ->
    exists q : Z, (m <= q)%Z /\ (q <= n)%Z /\ (access A q <= access A f)%Z.
Proof.
intros m n A H_m_f_n.
exists f.
 abstract omega.
Qed.

Lemma Lemma_5_14' :
 forall (m n:Z) (A:array Z),
   (m <= f <= n)%Z ->
   n_invariant n A -> j_invariant m n n (access A f) A.
Proof.
intros m n A H_f H_n.
unfold j_invariant.
split; [ abstract omega | split ].
intros.
 eapply Lemma_5; eauto.
intro.
 apply Lemma_14'; auto.
Qed.

Lemma Lemma_15 :
 forall (n i r:Z) (A:array Z),
   (access A i < r)%Z ->
   ( exists p : Z, (i <= p)%Z /\ (p <= n)%Z /\ (r <= access A p)%Z) ->
    exists p : Z, (Zsucc i <= p)%Z /\ (p <= n)%Z /\ (r <= access A p)%Z.
Proof.
intros n i r A H H0.
elim H0.
 intros p Hp.
 decompose [and] Hp.
case (Z_le_lt_eq_dec i p H1).
intro.
 exists p.
 abstract omega.
intro eq_i_p.
 absurd (r <= access A p)%Z.
 rewrite <- eq_i_p.
 abstract omega.
 assumption.
Qed.

Lemma Lemma_8_15 :
 forall (i m n r:Z) (A:array Z),
   i_invariant m n i r A ->
   (access A i < r)%Z -> i_invariant m n (Zsucc i) r A.
Proof.
unfold i_invariant.
intros i m n r A H H0.
decompose [and] H.
split.
abstract omega.
split.
elim (Lemma_8 i m r A); [ auto | abstract omega | auto ].
intro.
 apply Lemma_15; auto.
 apply H4; abstract omega.
Qed.


Lemma Lemma_17' :
 forall (j m n r:Z) (A:array Z),
   j_invariant m n j r A ->
   (1 <= m)%Z -> (m <= j)%Z -> (r < access A j)%Z -> (0 < j)%Z.
Proof.
unfold j_invariant.
intros j m n r A H Hm le_m_j H0.
decompose [and] H.
elim (H4 le_m_j).
 intros q Hq.
 decompose [and] Hq.
  case (Z_le_lt_eq_dec q j H6).
    intro.
 abstract omega.
    intro eq_q_j.
 absurd (access A q <= r)%Z.
 rewrite eq_q_j.
     abstract omega.
 assumption.
Qed.

Lemma Lemma_17 :
 forall (m j r:Z) (A:array Z),
   (r < access A j)%Z ->
   ( exists q : Z, (m <= q)%Z /\ (q <= j)%Z /\ (access A q <= r)%Z) ->
    exists q : Z, (m <= q)%Z /\ (q <= Zpred j)%Z /\ (access A q <= r)%Z.
Proof.
intros m j r A H H0.
elim H0.
 intros q Hq.
 decompose [and] Hq.
case (Z_le_lt_eq_dec q j H3).
intro.
 exists q.
 unfold Zpred.
 abstract omega.
intro eq_q_j.
 absurd (access A q <= r)%Z.
 rewrite eq_q_j.
 abstract omega.
 assumption.
Qed.

Lemma Lemma_9_17 :
 forall (j m n r:Z) (A:array Z),
   j_invariant m n j r A ->
   (r < access A j)%Z -> j_invariant m n (Zpred j) r A.
Proof.
unfold j_invariant.
intros j m n r A H H0.
decompose [and] H.
split.
unfold Zpred.
 abstract omega.
split.
elim (Lemma_9 j n r A); [ auto | abstract omega | auto ].
intro.
 apply Lemma_17; auto.
 apply H4.
unfold Zpred in H2; abstract omega.
Qed.
