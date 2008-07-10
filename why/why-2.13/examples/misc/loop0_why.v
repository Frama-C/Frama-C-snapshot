
Require Import Why.
Require Import Omega.

(* Why obligation from file "", line 0, characters 0-0: *)
(*Why goal*) Lemma p_po_1 : 
  forall (x: Z),
  forall (HW_1: x >= 0),
  0 <= x /\ x <= x.
 Proof.
 intuition.
 Qed.

(* Why obligation from file "", line 0, characters 0-0: *)
(*Why goal*) Lemma p_po_2 : 
  forall (x: Z),
  forall (HW_1: x >= 0),
  forall (x0: Z),
  forall (HW_2: 0 <= x0 /\ x0 <= x),
  forall (HW_3: x0 > 0),
  forall (x1: Z),
  forall (HW_4: x1 = (x0 - 1)),
  (0 <= x1 /\ x1 <= x) /\ (Zwf 0 x1 x0).
Proof.
intuition.
Qed.

(* Why obligation from file "", line 0, characters 0-0: *)
(*Why goal*) Lemma p_po_3 : 
  forall (x: Z),
  forall (HW_1: x >= 0),
  forall (x0: Z),
  forall (HW_2: 0 <= x0 /\ x0 <= x),
  forall (HW_5: x0 <= 0),
  x0 = 0.
Proof.
intuition.
Qed.
(*Why*) Parameter p_valid :
  forall (_: unit), forall (x: Z), forall (_: x >= 0),
  (sig_2 Z unit (fun (x0: Z) (result: unit)  => (x0 = 0))).

