
Require Import Why.
Require Import Omega.
Require Import ZArithRing.

 Proof.
 unfold Zwf; intros; omega.
Qed.

Proof.
unfold Zwf; intros; omega.
Qed.

Proof.
intuition.
Qed.



 Proof.
 intros; omega.
 Qed.

 Proof.
 intros; omega.
 Qed.



Proof.
intros; unfold Zwf; omega.
Qed.

Proof.
intros; omega.
Qed.

Proof.
intros; omega.
Qed.

Proof.
intros; omega.
Qed.



Proof.
intros; omega.
Qed.

Proof.
intros; omega.
Qed.



Proof.
ergo.
Qed.

Proof.
simpl; intros.
repeat split; unfold Zwf; try omega.
subst x2 z0.
intuition.
subst.
ring.
Qed.

Proof.
simpl; intros.
assert (z = 0%Z). omega.
intuition; subst.
ring.
Qed.


Proof.
 intros; omega.
Qed.

Proof.
 intros; omega.
Qed.



Proof.
intros; subst; intuition.
Save.

Proof.
intuition; subst; ring.
Save.

Proof.
unfold Zwf; intuition.
Save.

Proof.
intuition.
subst; ring.
Save.

Proof.
intuition.
Save.

Proof.
intuition.
Save.

Proof.
intuition.
Save.

Proof.
intuition.
Save.


