
Require Import Sumbool.
Require Import Why.
Require Import Omega.
Require Import ZArithRing.

Require Import Zcomplements.
Require Import Zpower.

Definition square x := (x * x)%Z.
Definition double x := (2 * x)%Z.

Definition div2 := Zdiv2.

Definition is_odd x :=
  bool_of_sumbool (sumbool_not _ _ (Zeven_odd_dec x)).

(* Some auxiliary lemmas about Zdiv2 are necessary *)

Lemma Zdiv2_ge_0 : forall x:Z, (x >= 0)%Z -> (Zdiv2 x >= 0)%Z.
Proof.
simple destruct x; auto with zarith.
simple destruct p; auto with zarith.
simpl.
 omega.
intros.
 absurd (Zneg p >= 0)%Z; red; auto with zarith.
Qed.

Lemma Zdiv2_lt : forall x:Z, (x > 0)%Z -> (Zdiv2 x < x)%Z.
Proof.
simple destruct x.
intro.
 absurd (0 > 0)%Z; [ omega | assumption ].
simple destruct p; auto with zarith.

simpl.
intro p0.
replace (Zpos (xI p0)) with (2 * Zpos p0 + 1)%Z.
omega.
simpl.
 auto with zarith.

intro p0.
simpl.
replace (Zpos (xO p0)) with (2 * Zpos p0)%Z.
omega.
simpl.
 auto with zarith.

simpl.
 omega.

intros.
 absurd (Zneg p > 0)%Z; red; auto with zarith.
elim p; auto with zarith.
Qed.

(* A property of Zpower:  x^(2*n) = (x^2)^n *)

Lemma Zpower_2n :
 forall x n:Z, (n >= 0)%Z -> Zpower x (double n) = Zpower (square x) n.
Proof.
unfold double.
intros x0 n Hn.
replace (2 * n)%Z with (n + n)%Z.
rewrite Zpower_exp.
pattern n.
apply natlike_ind.

simpl.
 auto with zarith.

intros.
unfold Zsucc.
rewrite Zpower_exp.
rewrite Zpower_exp.
replace (Zpower x0 1) with x0.
replace (Zpower (square x0) 1) with (square x0).
rewrite <- H0.
unfold square.
ring.

unfold Zpower; unfold Zpower_pos; simpl.
 omega.

unfold Zpower; unfold Zpower_pos; simpl.
 omega.

omega.
omega.
omega.
omega.
omega.
assumption.
assumption.
omega.
Qed.

(* Obligations *)


(*Why logic*) Definition x : Z.
Admitted.

Proof.
intuition; subst.
ring; auto.
Qed.

Proof.
simpl; intros.
repeat split; try omega.
intuition.
assert (h: x ^ n = y0 * m0 ^ (2 * Zdiv2 n0 + 1)).
assert (hn0 : n0 >= 0). assumption.
rewrite <- (Zodd_div2 n0 hn0); auto.
rewrite h.
rewrite Zpower_exp.
replace (Zpower m0 1) with m0.
rewrite Zpower_2n.
unfold square.
subst m1 n1 y1; unfold div2.
ring.
generalize (Zdiv2_ge_0 n0); omega.
unfold Zpower; unfold Zpower_pos; simpl; ring.
generalize (Zdiv2_ge_0 n0); omega.
omega.
subst; apply Zdiv2_ge_0; omega.
subst; apply Zdiv2_lt; omega.
Qed.

Proof.
simpl; intuition.
assert (h: x ^ n = y0 * m0 ^ (2 * Zdiv2 n0)).
rewrite <- (Zeven_div2 n0); auto.
rewrite h.
rewrite Zpower_2n.
unfold square.
subst; unfold div2.
ring.
generalize (Zdiv2_ge_0 n0); omega.
subst; apply Zdiv2_ge_0; omega.
subst; unfold Zwf; intuition; apply Zdiv2_lt; omega.
Qed.

Proof.
intros.
intuition.
transitivity (y0 * m0 ^ n0); auto.
replace n0 with 0%Z.
simpl; ring.
omega.
Qed.



(*Why logic*) Definition div2 : Z -> Z.
Admitted.

(*Why logic*) Definition Zeven : Z -> Prop.
Admitted.

(*Why logic*) Definition Zodd : Z -> Prop.
Admitted.

(*Why logic*) Definition Zpower : Z -> Z -> Z.
Admitted.

