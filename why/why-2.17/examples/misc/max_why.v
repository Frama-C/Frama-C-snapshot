
Require Import Why.

Parameter l : Z.
Axiom l_pos : (0 < l)%Z.



Proof.
intuition; subst.
generalize l_pos; auto with *.
generalize l_pos; auto with *.
assert (k=0). omega. subst; auto with *.
Save.

Proof.
intuition.
Save.

Proof.
intuition.
Save.

Proof.
intuition.
assert ((k < y0)%Z \/ k = y0).
 omega.
 intuition; subst.
assert (access a k <= access a x0); auto with *.
intuition.
Save.

Proof.
intuition.
assert ((k < y0)%Z \/ k = y0).
 omega.
 intuition.
subst; intuition.
Save.

Proof.
intuition.
assert (y0 = l). omega. subst.
replace (access a0 (l-1)) with (access a x0); auto with *.
assert (k = x0 \/ k <> x0).
 omega.
 intuition.
subst; intuition.
replace (access a0 x0) with (access a (l-1)); auto with *.
replace (access a0 k) with (access a k); auto with *.
symmetry; auto with *.
Qed.


(*Why logic*) Definition l : Z.
Admitted.

(*Why axiom*) Lemma l_pos : 0 < l.
Admitted.

