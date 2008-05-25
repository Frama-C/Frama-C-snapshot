
Require Import Why.


(* Why obligation from file , characters 41-84 *)
Lemma f_po_1 : forall (x x0:Z) (Post1:x0 = (1 - x)%Z), x0 = (1 - x)%Z.
Proof.
intuition.
Qed.


(* Why obligation from file , characters 103-149 *)
Lemma g_po_1 :
 forall (x x0:Z) (Post1:x0 = (1 - x)%Z) (x1:Z) (Post3:x1 = (1 - x0)%Z),
   x1 = x.
Proof.
intuition.
Qed.


