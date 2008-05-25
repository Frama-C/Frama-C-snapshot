
Require Import Why.

Parameter l : Z.
Axiom l_pos : (0 < l)%Z.


(*Why*) Parameter swap :
  forall (i: Z), forall (j: Z), forall (a: (array Z)),
  forall (_: (array_length a) = l),
  (sig_2 (array Z) unit
   (fun (a0: (array Z)) (result: unit)  => ((array_length a0) = l /\
    (access a0 i) = (access a j) /\ (access a0 j) = (access a i) /\
    (forall (k:Z),
     (0 <= k /\ k < l -> (k <> i -> (k <> j -> (access a0 k) = (access a k)))))))).

(* Why obligation from file "", line 0, characters 0-0: *)
(*Why goal*) Lemma pgm_max_end_po_1 : 
  forall (a: (array Z)),
  forall (HW_1: (array_length a) = l),
  forall (x: Z),
  forall (HW_2: x = 0),
  forall (y: Z),
  forall (HW_3: y = 1),
  (0 <= y /\ y <= l) /\ (0 <= x /\ x < l) /\
  (forall (k:Z), (0 <= k /\ k < y -> (access a k) <= (access a x))).
Proof.
intuition; subst.
generalize l_pos; auto with *.
generalize l_pos; auto with *.
assert (k=0). omega. subst; auto with *.
Save.

(* Why obligation from file "", line 0, characters 0-0: *)
(*Why goal*) Lemma pgm_max_end_po_2 : 
  forall (a: (array Z)),
  forall (HW_1: (array_length a) = l),
  forall (x: Z),
  forall (HW_2: x = 0),
  forall (y: Z),
  forall (HW_3: y = 1),
  forall (HW_4: (0 <= y /\ y <= l) /\ (0 <= x /\ x < l) /\
                (forall (k:Z),
                 (0 <= k /\ k < y -> (access a k) <= (access a x)))),
  forall (x0: Z),
  forall (y0: Z),
  forall (HW_5: (0 <= y0 /\ y0 <= l) /\ (0 <= x0 /\ x0 < l) /\
                (forall (k:Z),
                 (0 <= k /\ k < y0 -> (access a k) <= (access a x0)))),
  forall (HW_6: y0 < l),
  0 <= y0 /\ y0 < (array_length a).
Proof.
intuition.
Save.

(* Why obligation from file "", line 0, characters 0-0: *)
(*Why goal*) Lemma pgm_max_end_po_3 : 
  forall (a: (array Z)),
  forall (HW_1: (array_length a) = l),
  forall (x: Z),
  forall (HW_2: x = 0),
  forall (y: Z),
  forall (HW_3: y = 1),
  forall (HW_4: (0 <= y /\ y <= l) /\ (0 <= x /\ x < l) /\
                (forall (k:Z),
                 (0 <= k /\ k < y -> (access a k) <= (access a x)))),
  forall (x0: Z),
  forall (y0: Z),
  forall (HW_5: (0 <= y0 /\ y0 <= l) /\ (0 <= x0 /\ x0 < l) /\
                (forall (k:Z),
                 (0 <= k /\ k < y0 -> (access a k) <= (access a x0)))),
  forall (HW_6: y0 < l),
  forall (HW_7: 0 <= y0 /\ y0 < (array_length a)),
  forall (result: Z),
  forall (HW_8: result = (access a y0)),
  0 <= x0 /\ x0 < (array_length a).
Proof.
intuition.
Save.

(* Why obligation from file "", line 0, characters 0-0: *)
(*Why goal*) Lemma pgm_max_end_po_4 : 
  forall (a: (array Z)),
  forall (HW_1: (array_length a) = l),
  forall (x: Z),
  forall (HW_2: x = 0),
  forall (y: Z),
  forall (HW_3: y = 1),
  forall (HW_4: (0 <= y /\ y <= l) /\ (0 <= x /\ x < l) /\
                (forall (k:Z),
                 (0 <= k /\ k < y -> (access a k) <= (access a x)))),
  forall (x0: Z),
  forall (y0: Z),
  forall (HW_5: (0 <= y0 /\ y0 <= l) /\ (0 <= x0 /\ x0 < l) /\
                (forall (k:Z),
                 (0 <= k /\ k < y0 -> (access a k) <= (access a x0)))),
  forall (HW_6: y0 < l),
  forall (HW_7: 0 <= y0 /\ y0 < (array_length a)),
  forall (result: Z),
  forall (HW_8: result = (access a y0)),
  forall (HW_9: 0 <= x0 /\ x0 < (array_length a)),
  forall (result0: Z),
  forall (HW_10: result0 = (access a x0)),
  forall (HW_11: result > result0),
  forall (x1: Z),
  forall (HW_12: x1 = y0),
  forall (y1: Z),
  forall (HW_13: y1 = (y0 + 1)),
  ((0 <= y1 /\ y1 <= l) /\ (0 <= x1 /\ x1 < l) /\
  (forall (k:Z), (0 <= k /\ k < y1 -> (access a k) <= (access a x1)))) /\
  (Zwf 0 (l - y1) (l - y0)).
Proof.
intuition.
assert ((k < y0)%Z \/ k = y0).
 omega.
 intuition; subst.
assert (access a k <= access a x0); auto with *.
intuition.
Save.

(* Why obligation from file "", line 0, characters 0-0: *)
(*Why goal*) Lemma pgm_max_end_po_5 : 
  forall (a: (array Z)),
  forall (HW_1: (array_length a) = l),
  forall (x: Z),
  forall (HW_2: x = 0),
  forall (y: Z),
  forall (HW_3: y = 1),
  forall (HW_4: (0 <= y /\ y <= l) /\ (0 <= x /\ x < l) /\
                (forall (k:Z),
                 (0 <= k /\ k < y -> (access a k) <= (access a x)))),
  forall (x0: Z),
  forall (y0: Z),
  forall (HW_5: (0 <= y0 /\ y0 <= l) /\ (0 <= x0 /\ x0 < l) /\
                (forall (k:Z),
                 (0 <= k /\ k < y0 -> (access a k) <= (access a x0)))),
  forall (HW_6: y0 < l),
  forall (HW_7: 0 <= y0 /\ y0 < (array_length a)),
  forall (result: Z),
  forall (HW_8: result = (access a y0)),
  forall (HW_9: 0 <= x0 /\ x0 < (array_length a)),
  forall (result0: Z),
  forall (HW_10: result0 = (access a x0)),
  forall (HW_14: result <= result0),
  forall (y1: Z),
  forall (HW_15: y1 = (y0 + 1)),
  ((0 <= y1 /\ y1 <= l) /\ (0 <= x0 /\ x0 < l) /\
  (forall (k:Z), (0 <= k /\ k < y1 -> (access a k) <= (access a x0)))) /\
  (Zwf 0 (l - y1) (l - y0)).
Proof.
intuition.
assert ((k < y0)%Z \/ k = y0).
 omega.
 intuition.
subst; intuition.
Save.

(* Why obligation from file "", line 0, characters 0-0: *)
(*Why goal*) Lemma pgm_max_end_po_6 : 
  forall (a: (array Z)),
  forall (HW_1: (array_length a) = l),
  forall (x: Z),
  forall (HW_2: x = 0),
  forall (y: Z),
  forall (HW_3: y = 1),
  forall (HW_4: (0 <= y /\ y <= l) /\ (0 <= x /\ x < l) /\
                (forall (k:Z),
                 (0 <= k /\ k < y -> (access a k) <= (access a x)))),
  forall (x0: Z),
  forall (y0: Z),
  forall (HW_5: (0 <= y0 /\ y0 <= l) /\ (0 <= x0 /\ x0 < l) /\
                (forall (k:Z),
                 (0 <= k /\ k < y0 -> (access a k) <= (access a x0)))),
  forall (HW_16: y0 >= l),
  forall (HW_17: (array_length a) = l),
  forall (a0: (array Z)),
  forall (HW_18: (array_length a0) = l /\ (access a0 x0) =
                 (access a (l - 1)) /\ (access a0 (l - 1)) = (access a x0) /\
                 (forall (k:Z),
                  (0 <= k /\ k < l ->
                   (k <> x0 -> (k <> (l - 1) -> (access a0 k) = (access a k)))))),
  (forall (k:Z),
   (0 <= k /\ k < (l - 1) -> (k <> x0 -> (access a0 k) = (access a k)))) /\
  (access a0 x0) = (access a (l - 1)) /\ (access a0 (l - 1)) =
  (access a x0) /\
  (forall (k:Z),
   (0 <= k /\ k < (l - 1) -> (access a0 k) <= (access a0 (l - 1)))).
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

(*Why*) Parameter pgm_max_end_valid :
  forall (_: unit), forall (a: (array Z)), forall (x: Z), forall (y: Z),
  forall (_: (array_length a) = l),
  (sig_4 (array Z) Z Z unit
   (fun (a0: (array Z)) (x0: Z) (y0: Z) (result: unit)  =>
    ((forall (k:Z),
      (0 <= k /\ k < (l - 1) -> (k <> x0 -> (access a0 k) = (access a k)))) /\
    (access a0 x0) = (access a (l - 1)) /\ (access a0 (l - 1)) =
    (access a x0) /\
    (forall (k:Z),
     (0 <= k /\ k < (l - 1) -> (access a0 k) <= (access a0 (l - 1))))))).

