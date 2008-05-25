
Require Why.


(* Why obligation from file "init.mlw", characters 41-84 *)
Lemma f_po_1 : 
  (x: Z)
  (x0: Z)
  (Post1: x0 = `1 - x`)
  `x0 = 1 - x`.
Proof.
Intuition.
Save.


(* Why obligation from file "init.mlw", characters 103-149 *)
Lemma g_po_1 : 
  (x: Z)
  (x0: Z)
  (Post1: `x0 = 1 - x`)
  (x1: Z)
  (Post3: `x1 = 1 - x0`)
  `x1 = x`.
Proof.
Intuition.
Save.


