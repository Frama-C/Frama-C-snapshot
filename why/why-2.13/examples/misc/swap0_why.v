Require Import Why.
Require Import Omega.

(* Why obligation from file "", line 0, characters 0-0: *)
(*Why goal*) Lemma swap1_po_1 : 
  forall (x: Z),
  forall (y: Z),
  forall (x0: Z),
  forall (HW_1: x0 = y),
  forall (y0: Z),
  forall (HW_2: y0 = x),
  x0 = y /\ y0 = x.
Proof.
intuition.
Qed.


(*Why*) Parameter swap1_valid :
  forall (_: unit), forall (x: Z), forall (y: Z),
  (sig_3 Z Z unit (fun (x0: Z) (y0: Z) (result: unit)  => (x0 = y /\ y0 = x))).

(* Why obligation from file "", line 0, characters 0-0: *)
(*Why goal*) Lemma swap2_po_1 : 
  forall (x: Z),
  forall (y: Z),
  forall (x0: Z),
  forall (HW_1: x0 = y),
  forall (y0: Z),
  forall (HW_2: y0 = x),
  x0 = y /\ y0 = x.
Proof.
intuition.
Qed.


(*Why*) Parameter swap2_valid :
  forall (_: unit), forall (x: Z), forall (y: Z),
  (sig_3 Z Z unit (fun (x0: Z) (y0: Z) (result: unit)  => (x0 = y /\ y0 = x))).

(* Why obligation from file "", line 0, characters 0-0: *)
(*Why goal*) Lemma swap3_po_1 : 
  forall (a: Z),
  forall (b: Z),
  forall (a0: Z),
  forall (HW_1: a0 = b),
  forall (b0: Z),
  forall (HW_2: b0 = a),
  a0 = b /\ b0 = a.
Proof.
intuition.
Qed.

(*Why*) Parameter swap3_valid :
  forall (b: Z), forall (a: Z),
  (sig_3 Z Z unit (fun (b0: Z) (a0: Z) (result: unit)  => (a0 = b /\ b0 = a))).

(*Why*) Parameter test_swap3_valid :
  forall (_: unit), (sig_1 unit (fun (result: unit)  => (True))).

(*Why*) Parameter call_swap3_x_y_valid :
  forall (_: unit), forall (x: Z), forall (y: Z),
  (sig_3 Z Z unit (fun (x0: Z) (y0: Z) (result: unit)  => (x0 = y /\ y0 = x))).

(* Why obligation from file "", line 0, characters 0-0: *)
(*Why goal*) Lemma call_swap3_y_x_po_1 : 
  forall (x: Z),
  forall (y: Z),
  forall (x0: Z),
  forall (y0: Z),
  forall (HW_1: y0 = x /\ x0 = y),
  x0 = y /\ y0 = x.
 (* call_swap3_y_x_po_1 *)
Proof.
intuition.
Qed.


(*Why*) Parameter call_swap3_y_x_valid :
  forall (_: unit), forall (x: Z), forall (y: Z),
  (sig_3 Z Z unit (fun (x0: Z) (y0: Z) (result: unit)  => (x0 = y /\ y0 = x))).

(* Why obligation from file "", line 0, characters 0-0: *)
(*Why goal*) Lemma swap4_po_1 : 
  forall (a: Z),
  forall (b: Z),
  forall (tmp: Z),
  forall (HW_1: tmp = a),
  forall (a0: Z),
  forall (HW_2: a0 = b),
  forall (b0: Z),
  forall (HW_3: b0 = tmp),
  a0 = b /\ b0 = a.
Proof.
intuition.
Qed.


(*Why*) Parameter swap4_valid :
  forall (tmp: Z), forall (b: Z), forall (a: Z),
  (sig_4 Z Z Z unit
   (fun (tmp0: Z) (b0: Z) (a0: Z) (result: unit)  => (a0 = b /\ b0 = a))).

(*Why*) Parameter test_swap4_valid :
  forall (_: unit), forall (tmp: Z),
  (sig_2 Z unit (fun (tmp0: Z) (result: unit)  => (True))).

(* Why obligation from file "", line 0, characters 0-0: *)
(*Why goal*) Lemma call_swap4_x_y_po_1 : 
  forall (x: Z),
  forall (y: Z),
  forall (HW_1: x = 3),
  forall (x0: Z),
  forall (y0: Z),
  forall (HW_2: x0 = y /\ y0 = x),
  y0 = 3.
Proof.
intuition.
Qed.


(*Why*) Parameter call_swap4_x_y_valid :
  forall (_: unit), forall (tmp: Z), forall (x: Z), forall (y: Z),
  forall (_: x = 3),
  (sig_4 Z Z Z unit
   (fun (tmp0: Z) (x0: Z) (y0: Z) (result: unit)  => (y0 = 3))).

(* Why obligation from file "", line 0, characters 0-0: *)
(*Why goal*) Lemma call_swap4_y_x_po_1 : 
  forall (x: Z),
  forall (y: Z),
  forall (HW_1: x = 3),
  forall (x0: Z),
  forall (y0: Z),
  forall (HW_2: y0 = x /\ x0 = y),
  y0 = 3.
Proof.
intuition.
Qed.


(*Why*) Parameter call_swap4_y_x_valid :
  forall (_: unit), forall (tmp: Z), forall (x: Z), forall (y: Z),
  forall (_: x = 3),
  (sig_4 Z Z Z unit
   (fun (tmp0: Z) (x0: Z) (y0: Z) (result: unit)  => (y0 = 3))).

