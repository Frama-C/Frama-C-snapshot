
Require Import Why.
Require Omega.

Parameter foo : Set.

(*Why*) Parameter v2 : Z.

(*Why*) Parameter v3 : Z.

(*Why*) Parameter v5 : foo.

(*Why*) Parameter f1 : (_: Z)(_: bool)Z.

(*Why*) Parameter f2 : (_: Z)bool.

(*Why*) Parameter f3 :
  (x: Z)(y: Z)(H: `x >= 0`)
  (sig_2 Z Z [y0: Z][result: Z](`y0 = y + x + result`)).

(*Why*) Parameter f4 : (_: unit)unit.

(*Why*) Parameter f5 : (_: foo)foo.

(*Why*) Parameter f6 : (x: foo)foo.

(*Why*) Parameter f7 : (x: foo)foo.

(*Why*) Parameter f8 :
  (t: (array Z))(sig_1 unit [result: unit](`(access t 1) = 2`)).





(* Why obligation from file "all.mlw", characters 675-693 *)
Lemma p2_po_1 : 
  ~False.
Proof.
Tauto.
Save.





(* Why obligation from file "all.mlw", characters 703-725 *)
Lemma p3_po_1 : 
  True /\ True.
Proof.
Tauto.
Save.





(* Why obligation from file "all.mlw", characters 735-757 *)
Lemma p4_po_1 : 
  True \/ False.
Proof.
Tauto.
Save.





(* Why obligation from file "all.mlw", characters 767-794 *)
Lemma p5_po_1 : 
  False \/ ~False.
Proof. 
Auto.
Save.





(* Why obligation from file "all.mlw", characters 804-830 *)
Lemma p6_po_1 : 
  (True -> ~False).
Proof.
Auto.
Save.





(* Why obligation from file "all.mlw", characters 840-866 *)
Lemma p7_po_1 : 
  ((x:Z) `x = x`).
Proof.
Auto.
Save.





(* Why obligation from file "all.mlw", characters 876-911 *)
Lemma p8_po_1 : 
  True /\ ((x:Z) `x = x`).
Proof.
Auto.
Save.


(* Why obligation from file "all.mlw", characters 921-968 *)
Lemma p9_po_1 : 
  ((x:Z) ((y:Z) (`x = y` -> `x = y`))).
Proof.
Trivial.
Save.

(* Why obligation from file "all.mlw", characters 1164-1167 *)
Lemma ar6_po_1 : 
  ~(`1` = `0`).
Proof.
Omega.
Save.





(* Why obligation from file "all.mlw", characters 1178-1181 *)
Lemma ar7_po_1 : 
  ~(`1` = `0`).
Proof.
Omega.
Save.

















































































































(* Why obligation from file "all.mlw", characters 2122-2156 *)
Lemma arr1_po_1 : 
  (v6: (array Z))
  (Pre2: `(array_length v6) >= 1`)
  `0 <= 0` /\ `0 < (array_length v6)`.
Proof. (* arr1_po_1 *)
Intros; Omega.
Save.





(* Why obligation from file "all.mlw", characters 2169-2205 *)
Lemma arr2_po_1 : 
  (v6: (array Z))
  (Pre2: `(array_length v6) >= 4`)
  `0 <= 1 + 2` /\ `1 + 2 < (array_length v6)`.
Proof. (* arr2_po_1 *)
Intros; Omega.
Save.





(* Why obligation from file "all.mlw", characters 2217-2264 *)
Lemma arr3_po_1 : 
  (v4: Z)
  (v6: (array Z))
  (Pre2: `(array_length v6) >= 1` /\ `v4 = 0`)
  `0 <= v4` /\ `v4 < (array_length v6)`.
Proof. (* arr3_po_1 *)
Intros; Omega.
Save.





(* Why obligation from file "all.mlw", characters 2320-2325 *)
Lemma arr4_po_1 : 
  (v6: (array Z))
  (Pre3: `(array_length v6) >= 10` /\ `(access v6 0) = 9`)
  `0 <= 0` /\ `0 < (array_length v6)`.
Proof. (* arr4_po_1 *)
Intros; Omega.
Save.

(* Why obligation from file "all.mlw", characters 2276-2329 *)
Lemma arr4_po_2 : 
  (v6: (array Z))
  (Pre3: `(array_length v6) >= 10` /\ `(access v6 0) = 9`)
  (Pre2: `0 <= 0` /\ `0 < (array_length v6)`)
  `0 <= (access v6 0)` /\ `(access v6 0) < (array_length v6)`.
Proof. (* arr4_po_2 *)
Intros; Omega.
Save.





(* Why obligation from file "all.mlw", characters 2342-2381 *)
Lemma arr5_po_1 : 
  (v6: (array Z))
  (Pre2: `(array_length v6) >= 1`)
  `0 <= 0` /\ `0 < (array_length v6)`.
Proof. (* arr5_po_1 *)
Intros; Simpl; Omega.
Save.


(* Why obligation from file "all.mlw", characters 2393-2436 *)
Lemma arr6_po_1 : 
  (v6: (array Z))
  (Pre2: `(array_length v6) >= 4`)
  `0 <= 1 + 2` /\ `1 + 2 < (array_length v6)`.
Proof. (* arr6_po_1 *)
Intros; Simpl; Omega.
Save.



(* Why obligation from file "all.mlw", characters 2448-2506 *)
Lemma arr7_po_1 : 
  (v6: (array Z))
  (Pre3: `(array_length v6) >= 10` /\ `(access v6 0) = 9`)
  `0 <= (access v6 0)` /\ `(access v6 0) < (array_length v6)`.
Proof. (* arr7_po_1 *)
Intros; Omega.
Save.

(* Why obligation from file "all.mlw", characters 2492-2497 *)
Lemma arr7_po_2 : 
  (v6: (array Z))
  (Pre3: `(array_length v6) >= 10` /\ `(access v6 0) = 9`)
  (Pre2: `0 <= (access v6 0)` /\ `(access v6 0) < (array_length v6)`)
  `0 <= 0` /\ `0 < (array_length v6)`.
Proof. (* arr7_po_2 *)
Intros; Simpl; Omega.
Save.





(* Why obligation from file "all.mlw", characters 2611-2619 *)
Lemma fc3_po_1 : 
  (a: Z)
  (Post2: a = `0`)
  (b: Z)
  (Post1: b = `0`)
  `a >= 0`.
Proof. Intros; Omega. Save.









(* Why obligation from file "all.mlw", characters 2762-2810 *)
Lemma an2_po_1 : 
  (v4: Z)
  (Pre1: `v4 >= 0`)
  (v4_0: Z)
  (Post1: v4_0 = `v4 + 1`)
  `v4_0 > v4`.
Proof.
Intros; Omega.
Save.


(*Why*) Inductive ET_E1 [T:Set] : Set :=
  | Val_E1 : T -> (ET_E1 T)
  | Exn_E1 : (ET_E1 T).

(*Why*) Definition post_E1 :=
  [T:Set][P:Prop][Q:T->Prop][x:(ET_E1 T)]
  Cases x of 
  | (Val_E1 v) => (Q v)
  | Exn_E1 => P
  end.

(*Why*) Implicits post_E1.

(*Why*) Inductive ET_E2 [T:Set] : Set :=
  | Val_E2 : T -> (ET_E2 T)
  | Exn_E2 : Z -> (ET_E2 T).

(*Why*) Definition post_E2 :=
  [T:Set][P:Z -> Prop][Q:T->Prop][x:(ET_E2 T)]
  Cases x of 
  | (Val_E2 v) => (Q v)
  | (Exn_E2 v) => (P v)
  end.

(*Why*) Implicits post_E2.

(*Why*) Inductive ET_E3 [T:Set] : Set :=
  | Val_E3 : T -> (ET_E3 T)
  | Exn_E3 : foo -> (ET_E3 T).

(*Why*) Definition post_E3 :=
  [T:Set][P:foo -> Prop][Q:T->Prop][x:(ET_E3 T)]
  Cases x of 
  | (Val_E3 v) => (Q v)
  | (Exn_E3 v) => (P v)
  end.

(*Why*) Implicits post_E3.



(*Why*) Parameter f1_ex : (n: Z)(EM unit unit).

(*Why*) Parameter f2_ex : (x: Z)(tuple_2 Z (EM unit (EM Z bool))).

