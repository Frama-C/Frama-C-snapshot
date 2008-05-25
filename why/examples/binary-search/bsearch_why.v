
Require Import Why.
Require Import ZArith.
Require Import Zcomplements.
Require Import Omega.
 
Ltac Omega' := abstract omega.

(* Parameters and axioms *)

Parameter v : Z.

(* Specification *)

Inductive In (t:array Z) (l u:Z) : Prop :=
    In_cons : forall i:Z, (l <= i <= u)%Z -> access t i = v -> In t l u.

Definition mean (x y:Z) := Zdiv2 (x + y).

(* Lemmas *)

Lemma le_mean : forall x y:Z, (0 <= x <= y)%Z -> (x <= mean x y)%Z.
Proof.
intros.
apply Zmult_le_reg_r with (p := 2%Z).
omega.
rewrite Zmult_comm.
rewrite (Zmult_comm (mean x y) 2).
unfold mean.
elim (Zeven_odd_dec (x + y)); intro.
rewrite <- Zeven_div2 with (x + y)%Z.
omega.
assumption.
elim (Z_eq_dec x y); intro.
absurd (x = y); try assumption.
rewrite a in b.
absurd (Zodd (y + y)); try assumption.
absurd ((y + y)%Z = (2 * Zdiv2 (y + y) + 1)%Z).
omega.
apply Zodd_div2.
omega.
assumption.
cut ((x + y)%Z = (2 * Zdiv2 (x + y) + 1)%Z).
omega.
apply Zodd_div2.
omega.
assumption.
Qed.

Lemma ge_mean : forall x y:Z, (0 <= x <= y)%Z -> (mean x y <= y)%Z.
Proof.
intros.
apply Zmult_le_reg_r with (p := 2%Z).
omega.
rewrite Zmult_comm.
rewrite (Zmult_comm y 2).
unfold mean.
elim (Zeven_odd_dec (x + y)); intro.
rewrite <- Zeven_div2 with (x + y)%Z.
omega.
assumption.
cut ((x + y)%Z = (2 * Zdiv2 (x + y) + 1)%Z).
omega.
apply Zodd_div2.
omega.
assumption.
Qed.

Lemma In_right_side :
 forall t:array Z,
   sorted_array t 1 (array_length t - 1) ->
   forall l u:Z,
     (1 <= l)%Z ->
     (u <= array_length t - 1)%Z ->
     (l <= u)%Z ->
     (l <= mean l u <= u)%Z ->
     (In t 1 (array_length t - 1) -> In t l u) ->
     (access t (mean l u) < v)%Z ->
     In t 1 (array_length t - 1) -> In t (mean l u + 1) u.
     Proof.
     intros t Hsorted l u Hl Hu Hlu Hm Inv Hv H.
generalize (Inv H).
 intro.
decompose [In] H0.
apply In_cons with i.
elim (Z_gt_le_dec (mean l u + 1) i); intro.
absurd (access t i = v).
apply Zlt_not_eq.
apply Zle_lt_trans with (access t (mean l u)).
apply sorted_elements with (n := 1%Z) (m := (array_length t - 1)%Z).
assumption.
Omega'.
Omega'.
Omega'.
Omega'.
assumption.
assumption.
Omega'.
assumption.
Qed.

Lemma In_left_side :
 forall t:array Z,
   sorted_array t 1 (array_length t - 1) ->
   forall l u:Z,
     (1 <= l)%Z ->
     (u <= array_length t - 1)%Z ->
     (l <= u)%Z ->
     (l <= mean l u <= u)%Z ->
     (In t 1 (array_length t - 1) -> In t l u) ->
     (access t (mean l u) > v)%Z ->
     In t 1 (array_length t - 1) -> In t l (mean l u - 1).
     Proof.
     intros t Hsorted l u Hl Hu Hlu Hm Inv Hv H.
generalize (Inv H).
 intro.
decompose [In] H0.
apply In_cons with i.
elim (Z_gt_le_dec i (mean l u - 1)); intro.
absurd (access t i = v).
apply sym_not_eq.
apply Zlt_not_eq.
apply Zlt_le_trans with (access t (mean l u)).
Omega'.
apply sorted_elements with (n := 1%Z) (m := (array_length t - 1)%Z).
assumption.
Omega'.
Omega'.
Omega'.
Omega'.
assumption.
Omega'.
assumption.
Qed.

(* Obligations *)

(* Why obligation from file "", line 0, characters 0-0: *)
(*Why goal*) Lemma binary_search_po_1 : 
  forall (t: (array Z)),
  forall (HW_1: (array_length t) >= 1 /\
                (sorted_array t 1 ((array_length t) - 1))),
  forall (l: Z),
  forall (HW_2: l = 1),
  forall (result: Z),
  forall (HW_3: result = (array_length t)),
  forall (u: Z),
  forall (HW_4: u = (result - 1)),
  forall (p: Z),
  forall (HW_5: p = 0),
  1 <= l /\ u <= ((array_length t) - 1) /\ (0 <= p /\ p <=
  ((array_length t) - 1)) /\
  ((p = 0 -> ((In t 1 ((array_length t) - 1)) -> (In t l u)))) /\
  ((p > 0 -> (access t p) = v)).
Proof.
intuition.
subst;auto.
Save.

(* Why obligation from file "", line 0, characters 0-0: *)
(*Why goal*) Lemma binary_search_po_2 : 
  forall (t: (array Z)),
  forall (HW_1: (array_length t) >= 1 /\
                (sorted_array t 1 ((array_length t) - 1))),
  forall (l: Z),
  forall (HW_2: l = 1),
  forall (result: Z),
  forall (HW_3: result = (array_length t)),
  forall (u: Z),
  forall (HW_4: u = (result - 1)),
  forall (p: Z),
  forall (HW_5: p = 0),
  forall (l0: Z),
  forall (p0: Z),
  forall (u0: Z),
  forall (HW_6: 1 <= l0 /\ u0 <= ((array_length t) - 1) /\ (0 <= p0 /\ p0 <=
                ((array_length t) - 1)) /\
                ((p0 = 0 -> ((In t 1 ((array_length t) - 1)) -> (In t l0 u0)))) /\
                ((p0 > 0 -> (access t p0) = v))),
  forall (HW_7: l0 <= u0),
  forall (m: Z),
  forall (HW_8: m = (mean l0 u0)),
  l0 <= m /\ m <= u0.
Proof.
intuition; subst.
apply le_mean; intuition.
apply ge_mean; intuition.
Save.

(* Why obligation from file "", line 0, characters 0-0: *)
(*Why goal*) Lemma binary_search_po_3 : 
  forall (t: (array Z)),
  forall (HW_1: (array_length t) >= 1 /\
                (sorted_array t 1 ((array_length t) - 1))),
  forall (l: Z),
  forall (HW_2: l = 1),
  forall (result: Z),
  forall (HW_3: result = (array_length t)),
  forall (u: Z),
  forall (HW_4: u = (result - 1)),
  forall (p: Z),
  forall (HW_5: p = 0),
  forall (l0: Z),
  forall (p0: Z),
  forall (u0: Z),
  forall (HW_6: 1 <= l0 /\ u0 <= ((array_length t) - 1) /\ (0 <= p0 /\ p0 <=
                ((array_length t) - 1)) /\
                ((p0 = 0 -> ((In t 1 ((array_length t) - 1)) -> (In t l0 u0)))) /\
                ((p0 > 0 -> (access t p0) = v))),
  forall (HW_7: l0 <= u0),
  forall (m: Z),
  forall (HW_8: m = (mean l0 u0)),
  forall (HW_9: l0 <= m /\ m <= u0),
  0 <= m /\ m < (array_length t).
Proof.
intuition.
Save.

(* Why obligation from file "", line 0, characters 0-0: *)
(*Why goal*) Lemma binary_search_po_4 : 
  forall (t: (array Z)),
  forall (HW_1: (array_length t) >= 1 /\
                (sorted_array t 1 ((array_length t) - 1))),
  forall (l: Z),
  forall (HW_2: l = 1),
  forall (result: Z),
  forall (HW_3: result = (array_length t)),
  forall (u: Z),
  forall (HW_4: u = (result - 1)),
  forall (p: Z),
  forall (HW_5: p = 0),
  forall (l0: Z),
  forall (p0: Z),
  forall (u0: Z),
  forall (HW_6: 1 <= l0 /\ u0 <= ((array_length t) - 1) /\ (0 <= p0 /\ p0 <=
                ((array_length t) - 1)) /\
                ((p0 = 0 -> ((In t 1 ((array_length t) - 1)) -> (In t l0 u0)))) /\
                ((p0 > 0 -> (access t p0) = v))),
  forall (HW_7: l0 <= u0),
  forall (m: Z),
  forall (HW_8: m = (mean l0 u0)),
  forall (HW_9: l0 <= m /\ m <= u0),
  forall (HW_10: 0 <= m /\ m < (array_length t)),
  forall (result0: Z),
  forall (HW_11: result0 = (access t m)),
  forall (HW_12: result0 < v),
  forall (l1: Z),
  forall (HW_13: l1 = (m + 1)),
  (1 <= l1 /\ u0 <= ((array_length t) - 1) /\ (0 <= p0 /\ p0 <=
  ((array_length t) - 1)) /\
  ((p0 = 0 -> ((In t 1 ((array_length t) - 1)) -> (In t l1 u0)))) /\
  ((p0 > 0 -> (access t p0) = v))) /\ (Zwf 0 (2 + u0 - l1) (2 + u0 - l0)).
Proof.
intuition.
subst; apply In_right_side; intuition.
Save.

(* Why obligation from file "", line 0, characters 0-0: *)
(*Why goal*) Lemma binary_search_po_5 : 
  forall (t: (array Z)),
  forall (HW_1: (array_length t) >= 1 /\
                (sorted_array t 1 ((array_length t) - 1))),
  forall (l: Z),
  forall (HW_2: l = 1),
  forall (result: Z),
  forall (HW_3: result = (array_length t)),
  forall (u: Z),
  forall (HW_4: u = (result - 1)),
  forall (p: Z),
  forall (HW_5: p = 0),
  forall (l0: Z),
  forall (p0: Z),
  forall (u0: Z),
  forall (HW_6: 1 <= l0 /\ u0 <= ((array_length t) - 1) /\ (0 <= p0 /\ p0 <=
                ((array_length t) - 1)) /\
                ((p0 = 0 -> ((In t 1 ((array_length t) - 1)) -> (In t l0 u0)))) /\
                ((p0 > 0 -> (access t p0) = v))),
  forall (HW_7: l0 <= u0),
  forall (m: Z),
  forall (HW_8: m = (mean l0 u0)),
  forall (HW_9: l0 <= m /\ m <= u0),
  forall (HW_10: 0 <= m /\ m < (array_length t)),
  forall (result0: Z),
  forall (HW_11: result0 = (access t m)),
  forall (HW_14: result0 >= v),
  forall (HW_15: 0 <= m /\ m < (array_length t)),
  forall (result1: Z),
  forall (HW_16: result1 = (access t m)),
  forall (HW_17: result1 > v),
  forall (u1: Z),
  forall (HW_18: u1 = (m - 1)),
  (1 <= l0 /\ u1 <= ((array_length t) - 1) /\ (0 <= p0 /\ p0 <=
  ((array_length t) - 1)) /\
  ((p0 = 0 -> ((In t 1 ((array_length t) - 1)) -> (In t l0 u1)))) /\
  ((p0 > 0 -> (access t p0) = v))) /\ (Zwf 0 (2 + u1 - l0) (2 + u0 - l0)).
Proof.
intuition.
subst; apply In_left_side; intuition.
Save.

(* Why obligation from file "", line 0, characters 0-0: *)
(*Why goal*) Lemma binary_search_po_6 : 
  forall (t: (array Z)),
  forall (HW_1: (array_length t) >= 1 /\
                (sorted_array t 1 ((array_length t) - 1))),
  forall (l: Z),
  forall (HW_2: l = 1),
  forall (result: Z),
  forall (HW_3: result = (array_length t)),
  forall (u: Z),
  forall (HW_4: u = (result - 1)),
  forall (p: Z),
  forall (HW_5: p = 0),
  forall (l0: Z),
  forall (p0: Z),
  forall (u0: Z),
  forall (HW_6: 1 <= l0 /\ u0 <= ((array_length t) - 1) /\ (0 <= p0 /\ p0 <=
                ((array_length t) - 1)) /\
                ((p0 = 0 -> ((In t 1 ((array_length t) - 1)) -> (In t l0 u0)))) /\
                ((p0 > 0 -> (access t p0) = v))),
  forall (HW_7: l0 <= u0),
  forall (m: Z),
  forall (HW_8: m = (mean l0 u0)),
  forall (HW_9: l0 <= m /\ m <= u0),
  forall (HW_10: 0 <= m /\ m < (array_length t)),
  forall (result0: Z),
  forall (HW_11: result0 = (access t m)),
  forall (HW_14: result0 >= v),
  forall (HW_15: 0 <= m /\ m < (array_length t)),
  forall (result1: Z),
  forall (HW_16: result1 = (access t m)),
  forall (HW_19: result1 <= v),
  forall (p1: Z),
  forall (HW_20: p1 = m),
  forall (l1: Z),
  forall (HW_21: l1 = (u0 + 1)),
  (1 <= l1 /\ u0 <= ((array_length t) - 1) /\ (0 <= p1 /\ p1 <=
  ((array_length t) - 1)) /\
  ((p1 = 0 -> ((In t 1 ((array_length t) - 1)) -> (In t l1 u0)))) /\
  ((p1 > 0 -> (access t p1) = v))) /\ (Zwf 0 (2 + u0 - l1) (2 + u0 - l0)).
Proof.
intuition; subst.
absurd (mean l0 u0 = 0); omega.
omega.
Save.

(* Why obligation from file "", line 0, characters 0-0: *)
(*Why goal*) Lemma binary_search_po_7 : 
  forall (t: (array Z)),
  forall (HW_1: (array_length t) >= 1 /\
                (sorted_array t 1 ((array_length t) - 1))),
  forall (l: Z),
  forall (HW_2: l = 1),
  forall (result: Z),
  forall (HW_3: result = (array_length t)),
  forall (u: Z),
  forall (HW_4: u = (result - 1)),
  forall (p: Z),
  forall (HW_5: p = 0),
  forall (l0: Z),
  forall (p0: Z),
  forall (u0: Z),
  forall (HW_6: 1 <= l0 /\ u0 <= ((array_length t) - 1) /\ (0 <= p0 /\ p0 <=
                ((array_length t) - 1)) /\
                ((p0 = 0 -> ((In t 1 ((array_length t) - 1)) -> (In t l0 u0)))) /\
                ((p0 > 0 -> (access t p0) = v))),
  forall (HW_22: l0 > u0),
  (1 <= p0 /\ p0 <= ((array_length t) - 1)) /\ (access t p0) = v \/ p0 = 0 /\
  ~(In t 1 ((array_length t) - 1)).
Proof.
intuition; subst.
assert (1 <= p0 \/ p0=0). omega. intuition.
subst; right; intuition.
assert (h: (In t l0 u0)). assumption.
inversion h; omega.
Save.

(*Why*) Parameter binary_search_valid :
  forall (_: unit), forall (l: Z), forall (m: Z), forall (p: Z),
  forall (t: (array Z)), forall (u: Z), forall (_: (array_length t) >= 1 /\
  (sorted_array t 1 ((array_length t) - 1))),
  (sig_5 Z Z Z Z unit
   (fun (l0: Z) (m0: Z) (p0: Z) (u0: Z) (result: unit)  => ((1 <= p0 /\ p0 <=
    ((array_length t) - 1)) /\ (access t p0) = v \/ p0 = 0 /\
    ~(In t 1 ((array_length t) - 1))))).

