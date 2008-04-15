
Require Import Why.
Require Import Omega.

(* Why obligation from file "good/loops.mlw", line 9, characters 16-23: *)
(*Why goal*) Lemma loop1_po_1 : 
  forall (i: Z),
  forall (HW_1: i <= 10),
  forall (i0: Z),
  forall (HW_2: i0 <= 10),
  forall (HW_3: i0 < 10),
  forall (i1: Z),
  forall (HW_4: i1 = (i0 + 1)),
  i1 <= 10.
Proof.
intuition.
Save.

(* Why obligation from file "good/loops.mlw", line 9, characters 32-36: *)
(*Why goal*) Lemma loop1_po_2 : 
  forall (i: Z),
  forall (HW_1: i <= 10),
  forall (i0: Z),
  forall (HW_2: i0 <= 10),
  forall (HW_3: i0 < 10),
  forall (i1: Z),
  forall (HW_4: i1 = (i0 + 1)),
  (Zwf 0 (10 - i1) (10 - i0)).
Proof.
intuition.
Save.



(* Why obligation from file "good/loops.mlw", line 12, characters 4-10: *)
(*Why goal*) Lemma loop1_po_3 : 
  forall (i: Z),
  forall (HW_1: i <= 10),
  forall (i0: Z),
  forall (HW_2: i0 <= 10),
  forall (HW_5: i0 >= 10),
  i0 = 10.
Proof.
intuition.
Save.

(* Why obligation from file "good/loops.mlw", line 19, characters 34-41: *)
(*Why goal*) Lemma oppose_po_1 : 
  forall (x0: Z),
  forall (x: Z),
  forall (HW_1: x = (Zopp x0)),
  x = (Zopp x0).
Proof.
intuition.
Save.



(* Why obligation from file "good/loops.mlw", line 24, characters 33-40: *)
(*Why goal*) Lemma loop2_po_1 : 
  forall (x: Z),
  forall (HW_1: x <= 10),
  forall (x0: Z),
  forall (HW_2: x0 <= 10),
  forall (HW_3: x0 < 10),
  forall (x1: Z),
  forall (HW_4: x1 = (x0 + 1)),
  x1 <= 10.
Proof.
intuition.
Save.

(* Why obligation from file "good/loops.mlw", line 24, characters 49-53: *)
(*Why goal*) Lemma loop2_po_2 : 
  forall (x: Z),
  forall (HW_1: x <= 10),
  forall (x0: Z),
  forall (HW_2: x0 <= 10),
  forall (HW_3: x0 < 10),
  forall (x1: Z),
  forall (HW_4: x1 = (x0 + 1)),
  (Zwf 0 (10 - x1) (10 - x0)).
Proof.
intuition.
Save.

(* Why obligation from file "good/loops.mlw", line 25, characters 6-14: *)
(*Why goal*) Lemma loop2_po_3 : 
  forall (x: Z),
  forall (HW_1: x <= 10),
  forall (x0: Z),
  forall (HW_2: x0 <= 10),
  forall (HW_5: x0 >= 10),
  x0 = 10.
Proof.
intuition.
Save.

(* Why obligation from file "good/loops.mlw", line 27, characters 6-13: *)
(*Why goal*) Lemma loop2_po_4 : 
  forall (x: Z),
  forall (HW_1: x <= 10),
  forall (x0: Z),
  forall (HW_2: x0 <= 10),
  forall (HW_5: x0 >= 10),
  forall (HW_6: x0 > 0),
  forall (x1: Z),
  forall (HW_7: x1 = (Zopp x0)),
  x1 = (Zopp 10).
Proof.
intuition.
Save.



(* Why obligation from file "good/loops.mlw", line 27, characters 6-13: *)
(*Why goal*) Lemma loop2_po_5 : 
  forall (x: Z),
  forall (HW_1: x <= 10),
  forall (x0: Z),
  forall (HW_2: x0 <= 10),
  forall (HW_5: x0 >= 10),
  forall (HW_8: x0 <= 0),
  x0 = (Zopp 10).
Proof.
intuition.
Save.

