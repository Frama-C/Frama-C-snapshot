Require Export ZArith.
Require Export g4_spec_why.
Require Export G4.

Definition n (z:Z) : nat := Zabs_nat z.
Open Scope Z_scope.

Lemma n_succ : forall x, 0 <= x -> n (x+1) = S (n x).
Proof.
  destruct x; intuition.
  simpl.
  replace (p+1)%positive with (Psucc p).
  apply nat_of_P_succ_morphism.
  destruct p; auto.
  generalize (Zlt_neg_0 p); intuition.
Save.

Lemma n_pred : forall x, 0<x -> n x = S (n (x-1)).
Admitted.

Lemma n_minus_morphism : forall x y, 0<=y<=x ->
  n (x - y) = (n x - n y)%nat.
Admitted.

Lemma n_plus_morphism : forall x y, 0<=y -> 0<=x ->
  n (x + y) = (n x + n y)%nat.
Admitted.

Lemma n_mult_morphism : forall x y, 0<=y -> 0<=x ->
  n (x * y) = (n x * n y)%nat.
Admitted.

Lemma n_pow_morphism : forall x y, 0<=x -> 0<=y ->
  n (pow x y) = (n x ^ n y)%nat.
Admitted.

Lemma n_inj : forall x y, 0<=x -> 0<=y -> n x = n y -> x=y.
Admitted.
