
Require Why.

(*Why*) Parameter N : Z.

(* Why obligation from file "good/return.mlw", characters 285-290 *)
Lemma p_po_1 : 
  (t: (array Z))
  (Pre9: `(array_length t) = N`)
  (i0: Z)
  (Post1: i0 = `0`)
  (Variant1: Z)
  (i1: Z)
  (Pre8: Variant1 = `N - i1`)
  (Pre7: `0 <= i1`)
  (Test4: `i1 < N`)
  `0 <= i1` /\ `i1 < (array_length t)`.
Proof.
Intuition.
Save.

(* Why obligation from file "good/return.mlw", characters 314-316 *)
Lemma p_po_2 : 
  (t: (array Z))
  (Pre9: `(array_length t) = N`)
  (i0: Z)
  (Post1: i0 = `0`)
  (Variant1: Z)
  (i1: Z)
  (Pre8: Variant1 = `N - i1`)
  (Pre7: `0 <= i1`)
  (Test4: `i1 < N`)
  (Pre6: `0 <= i1` /\ `i1 < (array_length t)`)
  (Test3: `(access t i1) = 0`)
  (`0 <= i1` /\ `i1 < N` -> `(access t i1) = 0`).
Proof.
Intuition.
Save.

(* Why obligation from file "good/return.mlw", characters 317-317 *)
Lemma p_po_3 : 
  (t: (array Z))
  (Pre9: `(array_length t) = N`)
  (i0: Z)
  (Post1: i0 = `0`)
  (Variant1: Z)
  (i1: Z)
  (Pre8: Variant1 = `N - i1`)
  (Pre7: `0 <= i1`)
  (Test4: `i1 < N`)
  (Pre6: `0 <= i1` /\ `i1 < (array_length t)`)
  (Test2: `(access t i1) <> 0`)
  ((i:Z) (i = `i1 + 1` -> `0 <= i` /\ (Zwf `0` `N - i` `N - i1`))).
Proof.
Intuition.
Save.

(* Why obligation from file "good/return.mlw", characters 189-195 *)
Lemma p_po_4 : 
  (t: (array Z))
  (Pre9: `(array_length t) = N`)
  (i0: Z)
  (Post1: i0 = `0`)
  `0 <= i0`.
Proof.
Intuition.
Save.

(* Why obligation from file "good/return.mlw", characters 351-352 *)
Lemma p_po_5 : 
  (t: (array Z))
  (Pre9: `(array_length t) = N`)
  (i0: Z)
  (Post1: i0 = `0`)
  (i1: Z)
  (Post3: `0 <= i1` /\ `i1 >= N`)
  (`0 <= N` /\ `N < N` -> `(access t N) = 0`).
Proof.
Auto with *.
Save.

