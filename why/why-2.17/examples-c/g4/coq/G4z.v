Require Export nat_of_Z.
Require Export G4.

Definition reachable_ base c2 c1 c0 :=
  reachable (quad (n base) (n c2) (n c1) (n c0)).

Lemma reachable_3222 : reachable_ 3 2 2 2.
Proof.
  compute.
  apply rt_refl; auto.
Qed.

Lemma reachable_0 : forall b c2 c1 c0,
  0 <= b -> 0 <= c2 -> 0 <= c1 -> 0 < c0 -> 
  reachable_ b c2 c1 c0 -> reachable_ (b+1) c2 c1 (c0-1).
Proof.
  unfold reachable_.
  intros. rewrite (n_pred c0) in H3; auto.
  rewrite (n_succ b); auto.
  red. red. 
  apply rt_trans with (quad (n b) (n c2) (n c1) (S (n (c0 - 1)))); auto.
  apply rt_step. apply exp0.
Qed.

Lemma reachable_1 : forall b c2 c1,
  0 <= b -> 0 <= c2 -> 0 < c1 -> 
  reachable_ b c2 c1 0 -> reachable_ (b+1) c2 (c1-1) b.
Proof.
  unfold reachable_.
  intros. rewrite (n_pred c1) in H2; auto.
  rewrite (n_succ b); auto.
  red. red. 
  apply rt_trans with (quad (n b) (n c2) (S (n (c1 - 1))) 0); auto.
  apply rt_step. apply exp1.
Qed.

Lemma reachable_2 : forall b c2,
  0 <= b -> 0 < c2 -> 
  reachable_ b c2 0 0 -> reachable_ (b+1) (c2-1) b b.
Proof.
  unfold reachable_.
  intros. rewrite (n_pred c2) in H1; auto.
  rewrite (n_succ b); auto.
  red. red. 
  apply rt_trans with (quad (n b) (S (n (c2 - 1))) 0 0); auto.
  apply rt_step. apply exp2.
Qed.

Inductive Rn : item -> nat -> item -> Prop :=
  | R0 : forall i, Rn i 0 i
  | RS : forall i1 i2 i3 n, Rn i1 n i2 -> R i2 i3 -> Rn i1 (S n) i3.
Hint Constructors Rn.

Lemma Rstar_Rn : 
  forall i1 i2, Rstar i1 i2 -> exists n, Rn i1 n i2.
Proof.
  induction 1.
  exists (S O); subst.
  apply RS with x; auto.
  exists O; auto.
  elim IHclos_refl_trans1; intros n1 H1.
  elim IHclos_refl_trans2; intros n2 H2.
  exists (plus n1 n2).
  generalize z H2; clear H2; induction n2.
  intros; rewrite <- plus_n_O.
  inversion H2; subst; auto.
  intros; rewrite <- plus_n_Sm.
  inversion H2; subst; auto.
  apply RS with i2; auto.
Qed.

Lemma Rn_unicity : forall i0 n i1 i2, Rn i0 n i1 -> Rn i0 n i2 -> i1=i2.
Proof.
  induction n; intros.
  inversion H; inversion H0; subst; auto.
  inversion H; inversion H0; subst.
  assert (i4=i7).
  apply IHn; auto.
  subst.
  apply next_unicity with i7; auto.
Qed.

Lemma Rn_split :
  forall i0 n2 i1 n1, lt n1 n2 -> Rn i0 n2 i1 ->
    exists i, Rn i0 n1 i /\ ~(final i).
Proof.
  induction n2; intros.
  absurd (n1<0)%nat; omega.
  inversion H0; subst.
  assert (n1=n2 \/ (n1<n2)%nat). omega. 
  decompose [or] H1; clear H1.
  subst. exists i3; intuition.
  generalize H4.
  apply (final_no_future i3); auto.
  elim (IHn2 i3 n1 H3 H2); intros.
  exists x; intuition.
Qed.

Lemma final_length :
  forall i0 i1 i2 n1 n2, Rn i0 n1 i1 -> Rn i0 n2 i2 -> final i1 -> le n2 n1.
Proof.
  intros.
  assert (le n2 n1 \/ lt n1 n2). omega. intuition.
  elim (Rn_split i0 n2 i2 n1); auto; intuition.
  assert (x=i1). apply Rn_unicity with i0 n1; auto.
  subst; intuition.
Qed.

Lemma final_unicity :
  forall b1 b2, reachable (quad b1 0 0 0) -> reachable (quad b2 0 0 0) ->
  b1 = b2.
Proof.
  unfold reachable; intros.
  elim (Rstar_Rn _ _ H); intros; clear H.
  elim (Rstar_Rn _ _ H0); intros; clear H0.
  assert (le x x0). 
  apply (final_length (quad 3 2 2 2) (quad b2 0 0 0) (quad b1 0 0 0)); auto.
  econstructor; auto.
  assert (le x0 x). 
  apply (final_length (quad 3 2 2 2) (quad b1 0 0 0) (quad b2 0 0 0)); auto.
  econstructor; auto.
  assert (x=x0). omega.
  assert (quad b1 0 0 0 = quad b2 0 0 0).
  subst. apply Rn_unicity with (quad 3 2 2 2) x0; auto.
  injection H4; intuition.
Qed.

Lemma reachable_end : forall b,
  reachable (quad b 0 0 0) -> b = (3 * 2 ^ (3 * 2 ^ (3 ^3) + 3 ^3) - 1)%nat.
Proof.
  intros; apply final_unicity.
  assumption.
  exact G4_length.
Qed.

Lemma Zpower_pos : forall x y, x>0 -> y>=0 -> Zpower x y >= 1.
Admitted.

Lemma pos_a : 0 <= pow 3 3.
Proof.
  assert (pow 3 3 >= 1). 
  apply Zpower_pos; omega.
  omega.
Save.

Lemma pos_b : 0 <= pow 2 (pow 3 3).
Proof.
  assert (pow 2 (pow 3 3) >= 1).
  apply Zpower_pos. omega.
  generalize pos_a; omega.
  omega.
Save.
  
Lemma pos_1 : pow 2 (3 * pow 2 (pow 3 3) + pow 3 3) >= 1.
Proof.
  apply Zpower_pos. omega.
  assert (pow 2 (pow 3 3) >= 1).
  apply Zpower_pos. omega.
  generalize pos_a; omega.
  generalize pos_a; omega.
Save.

Lemma n1_is_1 : n 1 = 1%nat.
Proof. auto. Save.

Lemma n2_is_2 : n 2 = 2%nat.
Proof. auto. Save.

Lemma n3_is_3 : n 3 = 3%nat.
Proof. auto. Save.

Lemma reachable_end_ : forall b, 0<=b ->
  reachable_ b 0 0 0 -> b = 3 * (pow 2 (3 * (pow 2 (pow 3 3)) + (pow 3 3))) - 1.
Proof.
  unfold reachable_.
  replace (n 0) with O.
  intros. generalize (reachable_end (n b) H0). clear H0.
  intro; apply n_inj.
  assumption.
  generalize pos_1. omega.
  rewrite n_minus_morphism.
  rewrite n1_is_1.
  assert (forall x y, y=x -> n b=(x-1)%nat -> n b=(y-1)%nat).
  clear H0; intros; subst; auto.
  apply (H1 (3 * 2 ^ (3 * 2 ^ 3 ^ 3 + 3 ^ 3))%nat).
  clear b H H0 H1.
  rewrite n_mult_morphism.
  rewrite n3_is_3.
  rewrite n_pow_morphism.
  rewrite n2_is_2.
  rewrite n_plus_morphism.
  rewrite n_mult_morphism.
  rewrite n_pow_morphism.
  rewrite n3_is_3.
  rewrite n2_is_2.
  rewrite n_pow_morphism.
  rewrite n3_is_3.
  reflexivity.
  omega.
  omega.
  omega.
  exact pos_a.
  exact pos_b.
  omega. 
  exact pos_a.
  generalize pos_b; omega.
  omega.
  generalize pos_a pos_b; omega.
  generalize pos_1; omega.
  omega.
  exact H0.
  generalize pos_1; omega.
  auto.
Qed.
