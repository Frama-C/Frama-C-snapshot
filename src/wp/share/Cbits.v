(**************************************************************************)
(*                                                                        *)
(*  This file is part of WP plug-in of Frama-C.                           *)
(*                                                                        *)
(*  Copyright (C) 2007-2013                                               *)
(*    CEA (Commissariat a l'energie atomique et aux energies              *)
(*         alternatives)                                                  *)
(*                                                                        *)
(*  you can redistribute it and/or modify it under the terms of the GNU   *)
(*  Lesser General Public License as published by the Free Software       *)
(*  Foundation, version 2.1.                                              *)
(*                                                                        *)
(*  It is distributed in the hope that it will be useful,                 *)
(*  but WITHOUT ANY WARRANTY; without even the implied warranty of        *)
(*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the         *)
(*  GNU Lesser General Public License for more details.                   *)
(*                                                                        *)
(*  See the GNU Lesser General Public License version 2.1                 *)
(*  for more details (enclosed in the file licenses/LGPLv2.1).            *)
(*                                                                        *)
(**************************************************************************)

(* -------------------------------------------------------------------------- *)
(** * ACSL Logical and Bitwise Operators *)
(* -------------------------------------------------------------------------- *)

(** 
    The characteristic function of integers have {!arith:arithmetic} properties
    and allows to define logical operators over [Z]. Logical [land],
    [lor], [lxor] and [lnot] are defined as the lifting over bits of
    the associated boolean operators. As a corollary, the consistency
    of bitwise [lnot] definition and two's complements [zlnot] is
    assessed.

    These definitions are provided in two stages:
      - {!bitwise:bitwise} definitions of logical operators
      - {!ACSL:ACSL} operators definitions

*)

(** ** Tacticals *)

Require Import ZArith.
Require Import FunctionalExtensionality.
Require Import Qedlib.
Require Import Cint.
Require Import Bits.

Open Local Scope Z_scope.

Local Ltac omegaContradiction := cut False; [contradiction|omega].
			   
Local Ltac caseEq name :=
  generalize (refl_equal name); pattern name at -1 in |- *; case name.
  
Local Ltac unfold_hyp h :=
  match goal with 
    | h:(?X1) |- _ => unfold X1 in h
    | h:(?X1 _ ) |- _ => unfold X1 in h
    | h:(?X1 _ _) |- _ => unfold X1 in h
    | h:(?X1 _ _ _) |- _ => unfold X1 in h
    | h:(?X1 _ _ _ _) |- _ => unfold X1 in h
    | _ => idtac
  end.

Lemma split_range: forall a x b: Z, 
  a <= x -> x < b -> a <= x < b.
Proof.
  intros. omega.
Qed.

(** Some remarks about absolute value *)
  
Remark zabs_gt: forall n m: Z, 
  Zabs m < Zabs n -> (Zabs_nat m < Zabs_nat n)%nat.
Proof. 
  intros. apply (inj_lt_rev (Zabs_nat m) (Zabs_nat n)).
  rewrite (inj_Zabs_nat n). rewrite (inj_Zabs_nat m). omega.
Qed.
					 
Remark zabs_le: forall n m: Z,
  Zabs n <= Zabs m -> (Zabs_nat n <= Zabs_nat m)%nat.
Proof.
  intros. apply (inj_le_rev (Zabs_nat n) (Zabs_nat m)).
  rewrite (inj_Zabs_nat n). rewrite (inj_Zabs_nat m). omega.
Qed.
					 
Remark zabs_le_plus: forall (n m:Z) (k: nat),
  Zabs n <= Zabs m -> (Zabs_nat n <= k + Zabs_nat m)%nat.
Proof.
  intros. 
  apply (inj_le_rev (Zabs_nat n) (k + Zabs_nat m)%nat).
  rewrite (inj_Zabs_nat n). rewrite inj_plus. rewrite (inj_Zabs_nat m).
  omega.
Qed.
					 
Remark zabs_nat_zabs: forall n: Z,
  Zabs_nat (Zabs n) = Zabs_nat n.
Proof.
  intro.
  rewrite <- (inj_Zabs_nat n). rewrite Zabs_nat_Z_of_nat. auto.
Qed.

Remark zabs_minus: forall n m: Z,
  Zabs n <= Zabs m -> (Zabs_nat m - Zabs_nat n)%nat = Zabs_nat (Zabs m - Zabs n).
Proof.
  intros. 
  rewrite Zabs_nat_Zminus by (generalize (Zabs_pos n); omega).
  repeat rewrite zabs_nat_zabs. 
  auto.
Qed.
					 
Remark zabs_plus: forall n m: Z,
  (Zabs_nat m + Zabs_nat n)%nat = Zabs_nat (Zabs m + Zabs n).
Proof.
  intros. 
  rewrite Zabs_nat_Zplus.
  (** cont. *) repeat rewrite zabs_nat_zabs. auto.
  (** hyp 1 *) generalize (Zabs_pos m). omega.
  (** hyp 2 *) generalize (Zabs_pos n). omega.
Qed.

(** Some usefull properties *)
  
Remark upper_positive_mult_positive: forall p x: Z,
  0 <= x -> 0 < p -> x <= x * p. 
Proof.
  intros.
  rewrite <- Zmult_1_r at 1.
  apply Zmult_le_compat_l; omega.
Qed.
						       
Remark lower_negative_mult_positive: forall p x: Z,
  x <= 0 -> 0 < p -> x * p <= x. 
Proof.
  intros.
  cut (-x <= -(x * p)).
    omega.
  rewrite Zopp_mult_distr_l.
  apply upper_positive_mult_positive; omega.
Qed.
 
(* -------------------------------------------------------------------------- *)
(** {@arith:} *)
(** * Arithmetic Properties of the Characteristic Function of integers *)
(* -------------------------------------------------------------------------- *)

Remark Zbit_2x_0: forall x: Z,
  Zbit (2 * x) O = false.
Proof.
  unfold Zbit. unfold bits_of_Z. intro.
  case_leq 0 (2*x); intro; unfold btest. 
  (** case 0 <= 2*x *)
    unfold Nabs. unfold N_decomp. unfold P_decomp. 
    destruct x; auto.
  (** case 0 > 2*x *) 
    unfold zlnot. 
    destruct x; auto.
    destruct p; simpl; auto.
Qed.					      

Remark Zle_2x: forall x:Z, 
  Zle_bool 0 (2*x) = Zle_bool 0 x.
Proof.
  induction x; auto.
Qed.

Remark Zbit_2x_p: forall (n:nat) (x:Z),
  Zbit (2*x) (S n) = Zbit x n.
Proof. 
  intros.
  unfold Zbit; unfold bits_of_Z; rewrite Zle_2x.
  case_leq 0 x; intro; unfold btest. 
  (** case 0<=x *)
     unfold Nabs; unfold N_decomp; unfold P_decomp; destruct x; auto.
  (** case 0>x *)
     unfold zlnot; destruct x; 
       [ compute in H; discriminate H
       | discriminate H
       | destruct p; simpl; auto].
Qed.

Remark Zle_div2: forall x:Z,
  Zle_bool 0 (x/2) = Zle_bool 0 x.
Proof.
  intro x.
  case_leq 0 (x/2); case_leq 0 x; try auto; intros; apply False_ind.
  (** 0>x *)
    assert (x/2 < 0); [ apply Zdiv_lt_upper_bound | ]; omega.
  (** 0<=x *)
    assert (0 <= (x/2)); [ apply Z_div_pos | ]; omega.
Qed.

Remark Zbit_div2: forall (n:nat) (x:Z),
  Zbit (x/2) n = Zbit x (S n).
Proof.
  intros.
  unfold Zbit; unfold bits_of_Z; rewrite Zle_div2.
  case_leq 0 x; intro; unfold btest; 
     unfold Nabs; unfold N_decomp; unfold P_decomp. 
  (** case 0<=x *)
     destruct x; [by compute | | (apply False_ind; compute in H; auto) ].
     destruct p.
     (** 2p+1 *)
       rewrite <- (Zdiv_unique (Zpos (xI (p)) ) 2 (Zpos p) 1);
         by compute.
     (** 2p *)
       rewrite <- (Zdiv_unique (Zpos (xO (p)) ) 2 (Zpos p) 0);
         by compute.
     (** one *)
       by compute.
  (** case 0>x *)
     unfold zlnot.
     destruct x; [ by compute | (compute in H; discriminate H) | ]. 
     destruct p.
     (** -(2p+1) *)
       rewrite <- (Zdiv_unique (Zneg (xI (p)) ) 2 (Zneg p - 1) 1);
         [ (replace (Zneg p - 1 + 1) -> (Zneg p) by omega);
           (replace (-Zneg p) -> (Zpos p) by compute);     
            replace (-(Zneg (xI (p)) + 1)) -> (Zpos (xO(p))) by compute
         | 
         | replace (2*(Zneg p - 1) + 1) -> (2*Zneg p - 1) by omega
         ]; by compute.
     (** -2p *)
       rewrite <- (Zdiv_unique (Zneg (xO (p)) ) 2 (Zneg p) 0); 
         [ | by compute | by compute].
       replace (-(Zneg (xO (p)) + 1)) -> (Zpos (xO(p)) - 1) by compute.
       replace (-(Zneg p + 1)) -> ((-Zneg p) - 1) by omega.
       replace (- Zneg p) -> (Zpos p) by compute.  
       destruct p;
         [ (** -2(2p+1) *)
           (replace (Zpos (xI(p)) -1) -> (Zpos (xO(p))) by compute);
            replace (Zpos (xO(xI(p))) -1) -> (Zpos (xI(xO(p)))) by compute
         | (** -2(2p) *)
         | (** -2 *)
         ]; by compute.      
      (** minus one *)
       by compute.
Qed.
		       
Lemma Zbit_shift_l: forall (n m:nat) (x:Z), 
  Zbit (x * (two_power_nat n)) m = if leb n m then Zbit x (m - n)%nat else false.
Proof.
  induction n; intros.
  (** base *)
    rewrite (leb_correct O m) by omega.			
    unfold two_power_nat. unfold shift_nat. rewrite <- (minus_n_O m).
    f_equal. simpl. omega.
  (** ind. *) 
    rewrite two_power_nat_S.
    replace (x * (2 * two_power_nat n)) -> ((2 * x) * two_power_nat n) by ring.
    rewrite (IHn m (2*x)).
    nat_compare Inf EQ Sup n m.
    (** n<m *) 
      rewrite (leb_correct n m) by omega.
      rewrite (leb_correct (S n) m) by omega.
      rewrite <-(Zbit_2x_p (m - S n) x).
      f_equal. 
      rewrite (minus_Sn_m) by omega. 
      by simpl.
   (** n=m *) 
      rewrite (leb_correct n n) by omega.
      rewrite (leb_correct_conv n (S n)) by omega.
      rewrite <- minus_n_n.
      apply Zbit_2x_0.  
   (** n>m *) 
      rewrite (leb_correct_conv m n) by omega.
      rewrite (leb_correct_conv m (S n)) by omega.
      auto. 
Qed.

Lemma Zbit_shift_r: forall (n m:nat) (x:Z),
  Zbit (x / (two_power_nat n)) m = Zbit x (n + m)%nat.
Proof.
  induction n; intros.
  (** base *)
    unfold two_power_nat. unfold shift_nat. 
    f_equal. simpl. apply Zdiv_1_r.
  (** ind. *) 
    rewrite two_power_nat_S.
    replace (2 * two_power_nat n) -> ((two_power_nat n)*2) by ring.
    rewrite <- Zdiv_Zdiv;
      [ | generalize (two_power_nat_is_positive n); omega | omega].
    rewrite (plus_Snm_nSm n m).  
    rewrite <- (IHn (S m) x). 
    apply Zbit_div2.
Qed.

(** {@bitwise:} *)
(** * Bitwise Shifting Operators *)

Program Definition bitwise_lsl (x: bits) (n:nat): bits :=
  let sign := (bsign x) in
  let btest := (fun i: nat => if leb n i %nat then btest x (i - n)%nat
                              else false) in
  let last := last btest ((bsize x) + n) sign in
  mkbits last sign btest _ .
Next Obligation.
  apply trailing_last.
  generalize (btrail x).
  unfold trailing.
  intro Tx. intro k.
  nat_compare Inf EQ Sup n k.
  (** n < k *) 
    rewrite (leb_correct n k) by omega.
    intros. rewrite (Tx (k - n)%nat) by omega.  
    auto.
  (** n = k *)
    rewrite (leb_correct n n) by omega.
    intros. rewrite (Tx (n - n)%nat) by omega.
    auto.
  (** n > k *) 
    intro.
    omegaContradiction.
Qed.

Program Definition bitwise_lsr (x: bits) (n:nat): bits :=
  let sign := (bsign x) in
  let btest := (fun i: nat => btest x (i + n)%nat) in
  let last := last btest (bsize x) sign in
  mkbits last sign btest _ .
Next Obligation.
  apply trailing_last.
  generalize (btrail x).
  unfold trailing.
  intro Tx. intros.
  rewrite (Tx (k + n)%nat); auto with arith.
Qed.

Definition lsl_shift_def (x:Z) (n:nat): Z :=
  Z_of_bits (bitwise_lsl (bits_of_Z x) n).

Definition lsr_shift_def (x:Z) (n:nat): Z :=
  Z_of_bits (bitwise_lsr (bits_of_Z x) n).

(** ** Link between bitwise shifting operators and arithmetics *)

Definition lsl_arithmetic_def (x:Z) (n:nat): Z := 
  x * (two_power_nat n).
 
Lemma lsl_arithmetic_shift:
  lsl_shift_def = lsl_arithmetic_def.
Proof.
  extensionality x; extensionality n; Zbit_ext k.
  (** right term *)
  unfold lsl_arithmetic_def; rewrite (Zbit_shift_l n k x).
  (** left term *) 
  unfold lsl_shift_def; unfold Zbit; rewrite Z_decomp_recomp;
    unfold bitwise_lsl; unfold btest.
  auto.
Qed.
						    
Definition lsr_arithmetic_def (x:Z) (n:nat): Z := 
  x / (two_power_nat n).

(** Note: [lsr_arithmetic_def x n] is different than [lsr_arithmetic_def x (two_power_nat n)] for negative [x]. *)
Remark lsr_differs_to_Cdiv: lsr_arithmetic_def (-1) 1%nat <> Cdiv (-1) (two_power_nat 1).
Proof.
  by compute.
Qed.
							 
Lemma lsr_arithmetic_shift:
  lsr_shift_def = lsr_arithmetic_def.
Proof.
  extensionality x; extensionality n; Zbit_ext k.
  (** right term *)
  unfold lsr_arithmetic_def; rewrite (Zbit_shift_r n k x);
    replace (n+k)%nat -> (k+n)%nat by omega.
  (** left term *) 
  unfold lsr_shift_def; unfold Zbit; rewrite Z_decomp_recomp;
    unfold bitwise_lsr; unfold btest.
  auto.
Qed.
					     
(** * Bitwise Logical Operators *)
  
Program Definition bitwise (f: bool -> bool -> bool) (x y: bits): bits :=
  let sign := f (bsign x) (bsign y) in
  let btest := (fun i: nat => f (btest x i) (btest y i)) in
  let last := last btest (max (bsize x) (bsize y)) sign in
  mkbits last sign btest _ .
Next Obligation.
  apply trailing_last.
  generalize (btrail x).
  generalize (btrail y).
  unfold trailing.
  intros Ty Tx k Max.
  rewrite Tx. rewrite Ty. trivial.
  generalize (Max.max_lub_r (bsize x) (bsize y) k). omega.
  generalize (Max.max_lub_l (bsize x) (bsize y) k). omega.
Qed.

Definition Z_bitwise (f: bool -> bool -> bool) (x y: Z): Z :=
  Z_of_bits (bitwise f (bits_of_Z x) (bits_of_Z y)).
	
(** ** Properties of Bitwise Logical Operators *)

Lemma Zbit_bitwise: forall (f: bool -> bool -> bool) (x y: Z) (k: nat),
  Zbit (Z_bitwise f x y) k = f (Zbit x k) (Zbit y k).
Proof.
  intros. unfold Zbit. unfold Z_bitwise.
  rewrite Z_decomp_recomp. auto.
Qed.

(** Tactical. *)
Ltac Zbit_bitwise k := Zbit_ext k; repeat rewrite Zbit_bitwise.
  
(** Range of bitwise operators *)
Lemma Z_bitwise_ZxHpos: forall (f: bool -> bool -> bool) (x y: Z),
  (ZxHpos (Z_bitwise f x y) <= max (ZxHpos x) (ZxHpos y))%nat.
Proof.
  intros f x y.
  unfold Z_bitwise. rewrite (bsize_over_approx). unfold bitwise.
  unfold btest at 1; unfold bsize at 1; unfold bsign at 3;
    apply Max.max_case_strong;
    rewrite <- (bsize_exact x); rewrite <- (bsize_exact y); intro CASE.
  (** (ZxHpos y <= ZxHpos x) *)
    rewrite Max.max_l by auto.
    generalize (last_leq (fun i: nat => f (btest (bits_of_Z x) i) (btest (bits_of_Z y) i))
      (ZxHpos x) (f (bsign (bits_of_Z x)) (bsign (bits_of_Z y)))); intro.
    generalize (last_leq (fun i : nat => f (btest (bits_of_Z x) i) (btest (bits_of_Z y) i))
      (last (fun i : nat => f (btest (bits_of_Z x) i) (btest (bits_of_Z y) i))
      (ZxHpos x) (f (bsign (bits_of_Z x)) (bsign (bits_of_Z y))))
      (f (bsign (bits_of_Z x)) (bsign (bits_of_Z y)))); intro.
    omega.
  (** cont. (ZxHpos x <= ZxHpos y) *)
    rewrite Max.max_r by auto.
    generalize (last_leq (fun i: nat => f (btest (bits_of_Z x) i) (btest (bits_of_Z y) i))
      (ZxHpos y) (f (bsign (bits_of_Z x)) (bsign (bits_of_Z y)))); intro.
    generalize (last_leq (fun i: nat => f (btest (bits_of_Z x) i) (btest (bits_of_Z y) i))
      (last (fun i: nat => f (btest (bits_of_Z x) i) (btest (bits_of_Z y) i))
      (ZxHpos y) (f (bsign (bits_of_Z x)) (bsign (bits_of_Z y))))
      (f (bsign (bits_of_Z x)) (bsign (bits_of_Z y)))); intro.
    omega.
Qed.
  
Lemma Z_bitwise_ZxHbound: forall (f: bool -> bool -> bool) (x y: Z),
   ZxHbound (Z_bitwise f x y) <= Zmax (ZxHbound x) (ZxHbound y).
Proof.
  intros f x y.
  generalize (Z_bitwise_ZxHpos f x y).
  apply Z.max_case_strong; intro.
  (** ZxHbound y <= ZxHbound x *)
    assert (ZxHpos y <= ZxHpos x)%nat by by (apply ZxHpos_le).
    rewrite max_l; by try (intro; apply ZxHbound_le).
 (** ZxHbound x <= ZxHbound y *)
    assert (ZxHpos x <= ZxHpos y)%nat by by (apply ZxHpos_le).
    rewrite max_r; by try (intro; apply ZxHbound_le).
Qed.
  
Lemma Z_bitwise_in_sint_range: forall (f: bool -> bool -> bool) (n: nat) (x y: Z), 
  let b := two_power_nat n
  in -b <= x < b -> -b <= y < b -> -b <= (Z_bitwise f x y) < b.
Proof.
  intros f n x y b Rx Ry.
  assert (ZxHbound x <= b) as Bx.
    unfold b. unfold b in Rx.
    apply (ZxHpower n x).
    omega.
  assert (ZxHbound y <= b) as By.
    unfold b. unfold b in Ry.
    apply (ZxHpower n y).
    omega.
  generalize (Z_bitwise_ZxHbound f x y).
  pose (zxy := Z_bitwise f x y); fold zxy.
  generalize (ZxHrange zxy).
  apply Zmax_case_strong.
  (** ZxHbound y <= ZxHbound x *)
    intros Ryx Rzxy.
    destruct Rzxy.
    omega.
  (** ZxHbound x <= ZxHbound y *)
    intros Ryx Rzxy.
    destruct Rzxy.
    omega.
Qed.
  
Lemma Z_bitwise_in_uint_range: forall (f: bool -> bool -> bool) (n: nat) (x y: Z),  
  let b := two_power_nat n
  in 0 <= x < b -> 0 <= y < b -> f false false = false -> 0 <= (Z_bitwise f x y) < b.
Proof.
  intros f n x y b Rx Ry.
  assert (ZxHbound x <= b) as Bx.
    unfold b. unfold b in Rx.
    apply (ZxHpower n x).
    omega.
  assert (ZxHbound y <= b) as By.
    unfold b. unfold b in Ry.
    apply (ZxHpower n y).
    omega.
   intro Fsign.
   assert (0 <= (Z_bitwise f x y)) as Bz.
     unfold Z_bitwise.
     pose (bz := (bitwise f (bits_of_Z x) (bits_of_Z y))). fold bz.
     unfold Z_of_bits.
     destruct (bsign bz) eqn:BSIGN.
     (** negative sign *)
       assert (bsign bz = false) as OPP.
         unfold bz. unfold bitwise. unfold bsign.
         unfold bits_of_Z. unfold bsign.
         case_leq 0 x; intro; try omegaContradiction.
         case_leq 0 y; intros; try omegaContradiction.
         auto.
       rewrite BSIGN in OPP. 
       discriminate.
     (** positive sign *)
       apply (N_recomp_pos).
  generalize (Z_bitwise_ZxHbound f x y).
  pose (zxy := Z_bitwise f x y); fold zxy; fold zxy in Bz.
  generalize (ZxHrange zxy).
  apply Zmax_case_strong.
  (** ZxHbound y <= ZxHbound x *)
    intros Ryx Rzxy.
    destruct Rzxy.
    auto with zarith.
  (** ZxHbound x <= ZxHbound y *)
    intros Ryx Rzxy.
    destruct Rzxy.
    (* auto with zarith. *)
    omega.
Qed.
  
(** Commutative bitwise operators *)

Definition commutative {A B: Type} (f: A -> A -> B) :=
  forall x y: A, f x y = f y x.

Lemma Z_bitwise_commut: forall (f: bool -> bool -> bool), 
  commutative f -> commutative (Z_bitwise f).
Proof.
  unfold commutative. intros. apply btest_ext. simpl.
  extensionality k.
  apply H.
Qed.

(** Associative bitwise operators *)

Definition associative {A: Type} (f: A -> A -> A) :=
  forall x y z: A, f (f x y) z = f x (f y z).

Lemma Z_bitwise_assoc: forall (f: bool -> bool -> bool), 
  associative f -> associative (Z_bitwise f).
Proof.
  unfold associative. intros. apply btest_ext. simpl.
  extensionality k. unfold Z_bitwise.
  repeat rewrite Z_decomp_recomp. simpl. apply H.
Qed.									  
									  
(** Idempotent bitwise operators *)

Definition idempotent {A: Type} (f: A -> A -> A) :=
  forall x: A, f x x = x.

Lemma Z_bitwise_idempotent: forall (f: bool -> bool -> bool), 
  idempotent f -> idempotent (Z_bitwise f).
Proof.
  unfold idempotent. intros. Zbit_bitwise k. auto.
Qed.									  
									  
(** Distributive bitwise operators *)

Definition distributive_l {A: Type} (f : A -> A -> A) (g : A -> A -> A) :=
  forall x y z: A, f x (g y z) = g (f x y) (f x z).

Definition distributive_r {A: Type} (f : A -> A -> A) (g : A -> A -> A) :=
  forall x y z: A, f (g x y) z = g (f x z) (f y z).

Lemma Z_bitwise_distrib_l: forall (f g: bool -> bool -> bool),
  distributive_l f g -> distributive_l (Z_bitwise f) (Z_bitwise g) .
Proof.
  unfold distributive_l. intros. Zbit_bitwise k. auto.
Qed.									  
									  
Lemma Z_bitwise_distrib_r: forall (f g: bool -> bool -> bool),
  distributive_r f g -> distributive_r (Z_bitwise f) (Z_bitwise g) .
Proof.
  unfold distributive_r. intros. Zbit_bitwise k. auto.
Qed.									  
									  
(** Neutral elements of bitwise operators *)

Definition neutral {A: Type} (e: A) (f: A -> A -> A) :=
  forall x: A, f e x  = x.

Lemma Z_bitwise_neutral (e:bool): forall (f: bool -> bool -> bool),
  neutral e f -> neutral (if e then (-1) else 0) (Z_bitwise f).
Proof.
  unfold neutral. intros. Zbit_bitwise k.
  destruct e; simpl.
  (** TRUE *)  
    rewrite Zbit_of_mone. rewrite H. auto.
  (** FALSE *)  
    rewrite Zbit_of_zero. rewrite H. auto.
Qed.									  
									  
(** Absorbant element of bitwise operators *)

Definition absorbant {A: Type} (a: A) (f: A -> A -> A) :=
  forall x: A, f a x  = a.

Lemma Z_bitwise_absorbant (a:bool) :
  forall f, absorbant a f -> absorbant (if a then (-1) else 0) (Z_bitwise f).
Proof.
  unfold absorbant. intros. Zbit_bitwise k.
  destruct a; simpl.
  (** TRUE *)  
    rewrite Zbit_of_mone. rewrite H. auto.
  (** FALSE *)  
    rewrite Zbit_of_zero. rewrite H. auto.
Qed.									  

(** {@ACSL:} *)	
								  
(** * ACSL shifting operators *)

Parameter lsl_undef: Z -> Z -> Z.

Definition lsl_def (x:Z) (n:Z): Z :=
  lsl_shift_def x (Zabs_nat n).

Definition lsl (x : Z) (y : Z) : Z :=
  if Zle_bool 0 y then lsl_def x y
  else lsl_undef x y.

(* Lemma test_compute: lsl 2 1 = 4. *)
(* Proof. *)
(* compute; reflexivity. *)
(* Qed. *)

Parameter lsr_undef: Z -> Z -> Z.

Definition lsr_def (x:Z) (n:Z): Z :=
  lsr_shift_def x (Zabs_nat n).

Definition lsr (x : Z) (y : Z) : Z :=
  if Zle_bool 0 y then lsr_def x y
  else lsr_undef x y.

(** ** Properties of shifting operators *)

Theorem Zbit_lsl: forall (x n: Z) (k: nat), 
  Zbit (lsl_def x n) k = if (Zle_bool (Zabs n) (Z_of_nat k)) then Zbit x (Zabs_nat ((Z_of_nat k) - (Zabs n))) else false.
Proof.
  intros. unfold lsl_def. 
  rewrite lsl_arithmetic_shift. unfold lsl_arithmetic_def.
  rewrite Zbit_shift_l.
  case_leq (Zabs n) (Z_of_nat k).
  (** case |n| <= k *) intro LEQ.
    cut (leb (Zabs_nat n) k= true).
      intro LEB. rewrite LEB. f_equal.
      rewrite Zabs_nat_Zminus; try split; try apply Zabs_pos; auto.
      rewrite Zabs_nat_Z_of_nat.
      rewrite zabs_nat_zabs; auto.
      auto.
    apply leb_correct. rewrite <- Zabs_nat_Z_of_nat.
    apply zabs_le. 
    rewrite <- (inj_Zabs_nat (Z_of_nat k)). rewrite Zabs_nat_Z_of_nat.
    auto.
  (** case |n| > k *) intro GT.
    cut (leb (Zabs_nat n) k = false).
    intro GTB. rewrite GTB. auto. 
    apply leb_correct_conv.
    rewrite <- (Zabs_nat_Z_of_nat k).
    apply zabs_gt. 
    rewrite <- (inj_Zabs_nat (Z_of_nat k)). rewrite Zabs_nat_Z_of_nat.
    omega.
Qed.
					 
Theorem Zbit_lsr: forall (x n: Z) (k: nat),
  Zbit (lsr_def x n) k = Zbit x (k + (Zabs_nat n))%nat.
Proof.
  intros. 
  (** left  term *) 
  unfold lsr_def. unfold lsr_shift_def.
  unfold Zbit. rewrite Z_decomp_recomp.
  unfold bitwise_lsr. unfold btest at 1.
  auto.
Qed.

Lemma lsl_of_lsl: forall (n m: Z) (x:Z),
  lsl_def (lsl_def x n) m = lsl_def x (Zabs n + Zabs m).
Proof.
  intros. unfold lsl_def. 
  rewrite <- zabs_plus.
  rewrite lsl_arithmetic_shift.
  unfold lsl_arithmetic_def. 
  replace (x * two_power_nat (Zabs_nat n) * two_power_nat (Zabs_nat m))
       -> (x *(two_power_nat (Zabs_nat n) * two_power_nat (Zabs_nat m))) by ring.
  f_equal.
  repeat rewrite two_power_nat_correct.
  rewrite Zpower_nat_is_exp.
  auto.
Qed.

Lemma lsr_of_lsr: forall (n m: Z) (x:Z),
  lsr_def (lsr_def x n) m = lsr_def x (Zabs n + Zabs m).
Proof.
  intros. unfold lsr_def. 
  rewrite <- zabs_plus.
  unfold lsr_shift_def at 3. unfold lsr_shift_def at 1. 
  unfold bitwise_lsr.
  apply btest_ext.
  unfold btest at 1. unfold btest at 2.
  extensionality k.
  unfold lsr_shift_def. 
  rewrite Z_decomp_recomp.
  unfold bitwise_lsr. unfold btest at 1.
  f_equal.
  omega.
Qed.


Lemma lsr_of_lsl: forall (n m: Z) (x:Z),
  Zabs n <= Zabs m -> lsr_def (lsl_def x n) m = lsr_def x (Zabs m - Zabs n).
Proof.
  intros. unfold lsr_def. 
  rewrite <- zabs_minus by auto.
  unfold lsr_shift_def. unfold bitwise_lsr.
  apply btest_ext.
  unfold btest at 1. unfold btest at 2.
  extensionality k.
  unfold lsl_def. unfold lsl_shift_def.
  rewrite Z_decomp_recomp.
  unfold bitwise_lsl. unfold btest at 1.
  rewrite (leb_correct (Zabs_nat n) (k + Zabs_nat m)).
  f_equal.
  (** arg 1 *) 
     rewrite (inj_eq_rev (k + Zabs_nat m - Zabs_nat n) (k + (Zabs_nat m - Zabs_nat n))). 
     auto.
     rewrite inj_minus1 by (apply zabs_le_plus; omega). 
     repeat rewrite inj_plus. 
     rewrite inj_minus1 at 1 by (apply zabs_le; auto). 
     omega.
  (** arg 2 *) 
     apply zabs_le_plus.
     omega.
Qed.

(** * ACSL bitwise operators *)

Definition limpl (x y: Z): Z :=
  Z_bitwise implb x y.
Definition land (x y: Z): Z :=
  Z_bitwise andb x y.
Definition lor  (x y: Z): Z :=
  Z_bitwise orb x y.
Definition lxor (x y: Z): Z :=
  Z_bitwise xorb x y.
Definition lnot (x: Z): Z :=
  lxor (-1) x.

(** ** Properties of lnot operator *)

(** lnot x equals -(x+1) *)
Theorem lnot_zlnot_equiv: forall x: Z,
  lnot x = zlnot x.
Proof.
  intro x. unfold lnot. unfold lxor. Zbit_bitwise k.
  rewrite Zbit_of_mone. rewrite Bool.xorb_true_l.
  (** Now to prove that zlnot negates bits *)
  unfold Zbit. unfold bits_of_Z.
  pose (y := zlnot x). fold y.
  case_leq 0 x; case_leq 0 y; intros Y X; 
    try ( unfold y in Y; unfold zlnot in Y; apply False_ind; omega); 
    simpl.
  (** Negative *)
    unfold y. rewrite zlnot_inv. unfold fnot. trivial.
  (** Positive *)
    unfold fnot. rewrite Bool.negb_involutive. trivial.
Qed.
						   
(** Tactical *)
Local Ltac lnot_with_omega :=
  repeat rewrite lnot_zlnot_equiv; unfold zlnot; omega.
    
Theorem lnot_0: lnot 0 = -1.
Proof.
  auto with arith.
Qed.
						       
Theorem lnot_1: lnot (-1) = 0.
Proof.
  auto with arith.
Qed.
						       
(** Involution of the double negation *)					    
Theorem lnot_inv: forall x: Z,
  lnot (lnot x) = x.
Proof.
  intros x. lnot_with_omega.
Qed.
					    
Theorem lnot_sym: forall x y: Z,
  lnot x = y -> lnot y = x.
Proof.
  intros x y. lnot_with_omega.
Qed.

Theorem lnot_inj: forall x y: Z,
  lnot x = lnot y -> y = x.
Proof.
  intros x y. lnot_with_omega.
Qed.

(** ** Associative and commutative bitwise operators *)
  
(** land is AC *)
Theorem land_assoc: associative land.
Proof.
  apply (Z_bitwise_assoc andb).	
  unfold associative. intros. symmetry. apply Bool.andb_assoc.		       
Qed.
Theorem land_commut: commutative land.
Proof.
  apply (Z_bitwise_commut andb Bool.andb_comm).
Qed.
  
(** lor is AC *)
Theorem lor_assoc: associative lor.
Proof.
  apply (Z_bitwise_assoc orb). 
  unfold associative. intros. symmetry. apply Bool.orb_assoc.				       
Qed.
Theorem lor_commut: commutative lor.
Proof.
  apply (Z_bitwise_commut orb Bool.orb_comm).
Qed.

(** lxor is AC *)
Theorem lxor_assoc: associative lxor.
Proof.
  apply (Z_bitwise_assoc xorb Bool.xorb_assoc).			       
Qed.
Theorem lxor_commut: commutative lxor.
Proof.
  apply (Z_bitwise_commut xorb Bool.xorb_comm).
Qed.
						       
(** ** Idempotent bitwise operators *)
  
(** land is idempotent *)
Theorem land_idemp: idempotent land.
Proof.
  apply (Z_bitwise_idempotent andb).
  unfold idempotent. intro. destruct x; auto.
Qed.
  
(** lor is idempotent *)
Theorem lor_idemp: idempotent lor.
Proof.
  apply (Z_bitwise_idempotent orb).
  unfold idempotent. intro. destruct x; auto.
Qed.
  
(** ** Neutral elements of bitwise operators *)
  
(** Zero is the neutral element of lor *)
Theorem lor_0: neutral 0 lor.
Proof.
  apply (Z_bitwise_neutral false orb).
  unfold neutral. auto.
Qed.
						       
(** Zero is the neutral element of lxor *)
Theorem lxor_0: neutral 0 lxor.
Proof.
  apply (Z_bitwise_neutral false xorb).
  unfold neutral. apply Bool.orb_false_r.
Qed.
						       
(** Minus one is the neutral element of land *)
Theorem land_1: neutral (-1) land.
Proof.
  apply (Z_bitwise_neutral true andb).
  unfold neutral. auto.
Qed.
  
(** ** Absorbant elements of bitwise operators *)
  
(** Zero is the absorbant element of land *)
Theorem land_0: absorbant 0 land.
Proof.
  apply (Z_bitwise_absorbant false andb).
  unfold absorbant. auto.
Qed.						       

(** Minus one is the absorbant element of lor *)
Theorem lor_1: absorbant (-1) lor.
Proof.
  apply (Z_bitwise_absorbant true orb).
  unfold absorbant. auto.
Qed.						       

(** ** De Morgan laws of bitwise operators *)
  
Theorem lnot_land_de_morgan: forall x y: Z,
  lnot (land x y) = lor (lnot x) (lnot y).
Proof.
  intros. unfold lnot. unfold lxor. 
  Zbit_bitwise k. rewrite Zbit_of_mone. rewrite Bool.xorb_true_l.
  unfold land. rewrite Zbit_bitwise. 
  unfold lor. rewrite Zbit_bitwise. unfold Zbit. unfold Z_bitwise. 
  rewrite Z_decomp_recomp. rewrite Z_decomp_recomp. unfold bitwise. simpl. 
  pose (xb:= btest (bits_of_Z x) k). fold xb. 
  pose (yb:= btest (bits_of_Z y) k). fold yb.
  destruct xb; destruct yb; simpl; auto.
Qed.
						       
Theorem lnot_lor_de_morgan: forall x y: Z, 
  lnot (lor x y) = land (lnot x) (lnot y).
Proof.
  intros. unfold lnot. unfold lxor. Zbit_bitwise k.
  rewrite Zbit_of_mone. rewrite Bool.xorb_true_l.
  unfold land. rewrite Zbit_bitwise. 
  unfold lor. rewrite Zbit_bitwise. unfold Zbit. unfold Z_bitwise. 
  rewrite Z_decomp_recomp. rewrite Z_decomp_recomp. unfold bitwise. simpl. 
  pose (xb:= btest (bits_of_Z x) k). fold xb. 
  pose (yb:= btest (bits_of_Z y) k). fold yb.
  destruct xb; destruct yb; simpl; auto.
Qed.

(** ** Distributivity of bitwise operators *)

(** Distributive lor land *)						       
Theorem lor_land_distrib_l: distributive_l lor land.
Proof.
  apply (Z_bitwise_distrib_l orb andb).
  unfold distributive_l.
  destruct x; destruct y; destruct z; auto.
Qed.
Theorem lor_land_distrib_r: distributive_r lor land.
Proof.
  apply (Z_bitwise_distrib_r orb andb).
  unfold distributive_r.
  destruct x; destruct y; destruct z; auto.
Qed.
						       
(** Distributive land lor *)						       
Theorem land_lor_distrib_l: distributive_l land lor.
Proof.
  apply (Z_bitwise_distrib_l andb orb).
  unfold distributive_l.
  destruct x; destruct y; destruct z; auto.
Qed.						       
Theorem land_lor_distrib_r: distributive_r land lor.
Proof.
  apply (Z_bitwise_distrib_r andb orb).
  unfold distributive_r.
  destruct x; destruct y; destruct z; auto.
Qed.						       

(** Distributive land lxor *)						       
Theorem land_lxor_distrib_l: distributive_l land lxor.
Proof.
  apply (Z_bitwise_distrib_l andb xorb).
  unfold distributive_l.
  destruct x; destruct y; destruct z; auto.
Qed.						       
Theorem land_lxor_distrib_r: distributive_r land lxor.
Proof.
  apply (Z_bitwise_distrib_r andb xorb).
  unfold distributive_r.
  destruct x; destruct y; destruct z; auto.
Qed.						       

(** ** Properties of lxor operator *)

Theorem lxor_nilpotent: forall x: Z,
  lxor x x = 0.
Proof.
  intro. unfold lxor. Zbit_bitwise k. 
  rewrite Bool.xorb_nilpotent. rewrite Zbit_of_zero. auto.
Qed.
					    
Theorem lxor_1: forall x: Z,
  lxor (-1) x = lnot x.
Proof.
  trivial.
Qed.
						       
Theorem lxor_lnot: forall x y: Z,
  lxor (lnot x) y = lnot (lxor x y).
Proof.
  intros. unfold lnot. apply (lxor_assoc (-1) x y).
Qed.						       

(** ** Link between shifting and bitwise operators *)
Local Ltac lsl_distrib_r lop z :=
  unfold distributive_r;
  let k := fresh in
  intros; unfold lop; Zbit_bitwise k;
  repeat rewrite Zbit_lsl; rewrite Zbit_bitwise;
  case_leq (Zabs z) (Z_of_nat k);
    [ (intro; trivial) | trivial ].

(** Distributive lsl lor *)						       
Lemma lsl_lor_distrib_r: distributive_r lsl_def lor.
Proof.
  lsl_distrib_r lor z.
Qed.
						  
(** Distributive lsl land *)						       
Lemma lsl_land_distrib_r: distributive_r lsl_def land.
Proof.
  lsl_distrib_r land z.
Qed.
						  
(** Distributive lsl lxor *)						       
Lemma lsl_lxor_distrib_r: distributive_r lsl_def lxor.
Proof.
  lsl_distrib_r lxor z.
Qed.
						  
Local Ltac lsr_distrib_r lop :=
  unfold distributive_r;
  intros; Zbit_ext fresh; 
  unfold lop; rewrite Zbit_bitwise;
  repeat rewrite Zbit_lsr; rewrite Zbit_bitwise;
  trivial.

(** Distributive lsr lor *)						       
Lemma lsr_lor_distrib_r: distributive_r lsr_def lor.
Proof.
  lsr_distrib_r lor.
Qed.
						  
(** Distributive lsr land *)						       
Lemma lsr_land_distrib_r: distributive_r lsr_def land.
Proof.
  lsr_distrib_r land.
Qed.
						  
(** Distributive lsr lxor *)						       
Lemma lsr_lxor_distrib_r: distributive_r lsr_def lxor.
Proof.
  lsr_distrib_r lxor.
Qed.
						  
(** lsr lnot *)						       
Lemma lsr_lnot: forall x y: Z,
  lnot (lsr_def x y) = lsr_def (lnot x) y .
Proof.
  unfold lnot.
  lsr_distrib_r lxor.
Qed.
						  
(** ** Some properties of equations of bitwise operators *)

Local Ltac f_equal_hyp h f k :=
  match goal with 
    | [ h:(?X1 = ?X2) |- _ ] =>
        let H := fresh in assert (H : f X1 k = f X2 k) by (f_equal; auto); clear h;
        assert (h: f X1 k = f X2 k) by auto; clear H
  end.

Local Ltac linear2 :=
  intros x y; (try split); intro H; (try split);
  let k := fresh "k" in
  Zbit_ext k; 
  try (destruct H as [H H0] ; f_equal_hyp H0 Zbit k; generalize H0; clear H0);
  f_equal_hyp H Zbit k; generalize H; clear H;
  (try unfold limpl); (try unfold lnot);
  (try unfold land); (try unfold lor); (try unfold lxor); 
  repeat (replace (Zbit (-1) k) -> true by simpl); 
  repeat (replace (Zbit 0 k) -> false by simpl); 
  repeat rewrite Zbit_bitwise;
  destruct (Zbit x k); destruct (Zbit y k); simpl; auto.

Lemma linear_land: forall x y: Z,
  limpl x y = -1 <-> land x y = x.
Proof.
  linear2.
Qed.

Lemma linear_lor: forall x y: Z,
  lor x y = x <-> limpl y x = -1.
Proof.
  linear2.
Qed.

Lemma linear_lxor: forall x y: Z,
  lxor x y = x <-> y=0.
Proof.
  linear2.
Qed.

Lemma linear_limpl_r: forall x y: Z,
  limpl x y = y <-> lor x y = -1.
Proof.
  linear2.
Qed.

Local Ltac F_equal_hyp h f k :=
  match goal with 
    | [ h:(?X1 = ?X2) |- _ ] => idtac h;
        let H := fresh in assert (H : f X1 k = f X2 k) by (f_equal; auto); clear h;
        assert (h: f X1 k = f X2 k) by (apply H); clear H
  end.

Lemma linear_limpl_l: forall x y: Z,
  limpl x y = x <-> x=-1 /\ y=-1.
Proof.
  linear2.
Qed.

Lemma linear_land_lnot: forall x y: Z,
  land x y = lnot x <-> x=-1 /\ y=0.
Proof.
  linear2.
Qed.

Lemma linear_lor_lnot: forall x y: Z,
  lor x y = lnot x <-> x=0 /\ y=-1.
Proof.
  linear2.
Qed.

Lemma linear_lxor_lnot : forall x y: Z,
  lxor x y = lnot x <-> y=-1.
Proof.
  linear2.
Qed.

Lemma linear_limpl_r_lnot: forall x y: Z,
  limpl x y = lnot y <-> x=0 /\ y=0.
Proof.
  linear2.
Qed.

Lemma linear_limpl_l_lnot: forall x y: Z,
  limpl x y = lnot x <-> land x y = 0.
Proof.
  linear2.
Qed.

Local Ltac linear3 :=
  intros x y z; (try split); intro H; (try split);
  let k := fresh "k" in
  Zbit_ext k; 
  try (destruct H as [H H0] ; f_equal_hyp H0 Zbit k; generalize H0; clear H0);
  f_equal_hyp H Zbit k; generalize H; clear H;
  (try unfold limpl); (try unfold lnot);
  (try unfold land); (try unfold lor); (try unfold lxor); 
  repeat (replace (Zbit (-1) k) -> true by simpl); 
  repeat (replace (Zbit 0 k) -> false by simpl); 
  repeat rewrite Zbit_bitwise;
  destruct (Zbit x k); destruct (Zbit y k); destruct (Zbit z k); simpl; auto.

Lemma linear_lxor_land: forall x y z: Z,
  lxor x y = land x z <-> lnot y = limpl x z.
Proof.
  linear3.
Qed.

Lemma linear_lxor_lor: forall x y z: Z,
  lxor x y = lor x z <-> lnot y = limpl z x.
Proof.
  linear3.
Qed.

Lemma linear_lxor_limpl_l: forall x y z: Z,
  lxor x y = limpl x z <-> lnot y = land x z.
Proof.
  linear3.
Qed.

Lemma linear_lxor_limpl_r: forall x y z: Z,
  lxor x y = limpl z x <-> lnot y = lor z x.
Proof.
  linear3.
Qed.

Lemma linear_land_land: forall x y z: Z,
  land x y = land z x <-> land x (lxor y z) = 0.
Proof.
  linear3.
Qed.

Lemma linear_lnot_land_land: forall x y z: Z,
  lnot (land x y) = land z x <-> x=-1 /\ y = lnot z.
Proof.
  linear3.
Qed.

Lemma linear_lor_lor: forall x y z: Z,
  lor x y = lor z x <-> land (lnot x) (lxor y z) = 0.
Proof.
  linear3.
Qed.

Lemma linear_lnot_lor_lor: forall x y z: Z,
  lnot (lor x y) = lor z x <-> x=0 /\ y = lnot z.
Proof.
  linear3.
Qed.

Lemma linear_lor_land: forall x y z: Z,
  lor x y = land x z <-> y = land x (lnot (lxor y z)).
Proof.
  linear3.
Qed.

Lemma land_discrimination_inv: forall x y z:Z,
  x = land y z -> land x (lnot y) = 0.
Proof.
  linear3.
Qed.
  
Lemma land_discrimination: forall x y z:Z,
  land x (lnot y) <> 0 -> x <> land y z.
Proof.
  intros x y z.
  generalize (land_discrimination_inv x y z).
  intuition.
Qed.

Lemma land_system: forall x1 x2 y1 y2 z:Z,
  (x1 = land z y1 /\ x2 = land z y2) <-> lor x1 x2 = land z (lor (land (lnot x1) (land (lnot x2) (lor y1 y2))) 
                                                       (lor (land x1 (land y1 (lnot (lxor x2 y2))))
                                                           ((land x2 (land y2 (lnot (lxor x1 y1))))))).
Proof.
 intros x1 x2 y1 y2 z. split; 
   intro H ; try split;
   Zbit_ext k; 
   try (destruct H as [H H0]; f_equal_hyp H0 Zbit k; generalize H0; clear H0);
   f_equal_hyp H Zbit k; generalize H; clear H;
   (try unfold limpl); (try unfold lnot);
   (try unfold land); (try unfold lor); (try unfold lxor); 
   repeat (replace (Zbit (-1) k) -> true by simpl); 
   repeat (replace (Zbit 0 k) -> false by simpl); 
   repeat rewrite Zbit_bitwise;
   destruct (Zbit x1 k); destruct (Zbit x2 k); 
   destruct (Zbit y1 k); destruct (Zbit y2 k); destruct (Zbit z k); simpl; auto.
Qed.

(** * Bit extraction *)

Parameter bit_test: Z -> Z -> bool.

(* Extended version for negative value. *)
Definition zbit_test_def (x:Z) (n:Z): bool :=
  Zbit x (Zabs_nat n).	
					
Axiom bit_test_partial_def: forall x n: Z, 
  n >=0 -> bit_test x n = zbit_test_def x n.
						       
(** Tactical *)
Local Ltac bit_extraction bin_op :=
  intros; unfold zbit_test_def; unfold bin_op; rewrite Zbit_bitwise; auto. 
    
(** ** Link between Bit extraction and bitwise shifting operators *)

Theorem lsl_extraction: forall x n m: Z, 
  zbit_test_def (lsl_def x n) m =
    if Zle_bool (Zabs n) (Zabs m) 
    then zbit_test_def x ((Zabs m) - (Zabs n)) 
    else false.
Proof.
  intros. unfold zbit_test_def.	
  rewrite Zbit_lsl. repeat rewrite inj_Zabs_nat. 
  auto.
Qed.
					 
Theorem lsr_extraction: forall x n m: Z, 
  zbit_test_def (lsr_def x n) m = zbit_test_def x ((Zabs m) + (Zabs n)).
Proof.
  intros. unfold zbit_test_def. 
  (** right term *) 
  rewrite <- zabs_plus.
  (** left  term *) 
  rewrite Zbit_lsr. 
  auto.
Qed.
					 
(** ** Link between Bit extraction and bitwise operators *)

Theorem land_extraction: forall x y i: Z, 
  zbit_test_def (land x y) i = andb (zbit_test_def x i) (zbit_test_def y i).
Proof.
  bit_extraction land.
Qed.

Theorem lor_extraction: forall x y i: Z, 
  zbit_test_def (lor x y) i = orb (zbit_test_def x i) (zbit_test_def y i).
Proof.
  bit_extraction lor.
Qed.

Theorem lxor_extraction: forall x y i: Z, 
  zbit_test_def (lxor x y) i = xorb (zbit_test_def x i) (zbit_test_def y i).
Proof.
  bit_extraction lxor.
Qed.

Theorem lnot_extraction: forall x i: Z, 
  zbit_test_def (lnot x) i = negb (zbit_test_def x i).
Proof.
  unfold lnot.
  bit_extraction lxor.
Qed.

(** * Some C-Integer Bits Conversions are identity *)

(* Tactical *)
Lemma lnot_in_range: forall a b z: Z,
  a <= z < b -> -b <= lnot z < -a.
Proof.
  intros.
  rewrite lnot_zlnot_equiv. unfold zlnot.
  omega.
Qed.						       							

Lemma lsr_upper_bound: forall b x y: Z,
  0 <= y -> x < b -> 0 <= b -> lsr x y < b.
Proof.
  intros b x y Ry Rx Rb.
  apply Zle_is_le_bool in Ry; unfold lsr; rewrite Ry.
  unfold lsr_def. rewrite lsr_arithmetic_shift. unfold lsr_arithmetic_def.
  pose (d := two_power_nat (Zabs_nat y)); fold d.
  assert (PWR2: 0 < d) by apply two_power_nat_is_positive.
  apply Zdiv_lt_upper_bound; auto.
  assert (b <= b * d) by apply (upper_positive_mult_positive d b Rb PWR2).
  omega.
Qed.
						       
Lemma lsr_lower_bound: forall b x y: Z,
  0 <= y -> b <= x -> b <= 0 -> b <= lsr x y.
Proof.
  intros b x y Ry Rx Rb.
  apply Zle_is_le_bool in Ry; unfold lsr; rewrite Ry.
  unfold lsr_def. rewrite lsr_arithmetic_shift. unfold lsr_arithmetic_def.
  pose (d := two_power_nat (Zabs_nat y)); fold d.
  assert (PWR2: 0 < d) by apply two_power_nat_is_positive.
  apply Zdiv_le_lower_bound; auto.
  assert (b * d <= b) by apply (lower_negative_mult_positive d b Rb PWR2).
  omega.
Qed.						       							

Local Ltac is_sint_lnot b :=
  intros x Rx; apply id_to_range; apply (lnot_in_range (-b) b x Rx).

Local Ltac is_sint_bitwise f n :=
  intros x y Rx Ry; apply id_to_range; apply (Z_bitwise_in_sint_range f n x y Rx Ry); by compute.  

Local Ltac is_uint_bitwise f n :=
  intros x y Rx Ry; apply id_to_range; apply (Z_bitwise_in_uint_range f n x y Rx Ry); by compute.  

(** ** Signed conversions *)

(* sint8 *)
Lemma is_sint8_lnot: forall x: Z,
  is_sint8 x -> to_sint8 (lnot x) = lnot x. 
Proof. 
  is_sint_lnot 128. 
Qed.
						       
Lemma is_sint8_lxor: forall x y: Z,
  is_sint8 x -> is_sint8 y -> to_sint8 (lxor x y) = lxor x y.
Proof.
  is_sint_bitwise xorb 7%nat.  
Qed.						       

Lemma is_sint8_lor: forall x y: Z,
  is_sint8 x -> is_sint8 y -> to_sint8 (lor x y) = lor x y.
Proof.
  is_sint_bitwise orb 7%nat.  
Qed.						       

Lemma is_sint8_land: forall x y: Z,
  is_sint8 x -> is_sint8 y -> to_sint8 (land x y) = land x y.
Proof.
  is_sint_bitwise andb 7%nat.  
Qed.						       

Local Ltac lsr_in_sint_range n :=
  intros x y Ry Rx;
  unfold_hyp Rx;
  apply id_to_range;
  split;
  [ (apply (lsr_lower_bound (-n) _ _ Ry); omega)
  | (apply (lsr_upper_bound n _ _ Ry); omega)].

Local Ltac lsr_in_uint_range n :=
  intros x y Ry Rx;
  unfold_hyp Rx;
  apply id_to_range;
  split;
  [ (apply (lsr_lower_bound 0 _ _ Ry); omega)
  | (apply (lsr_upper_bound n _ _ Ry); omega)].


Lemma is_sint8_lsr: forall x y: Z,
  0 <= y -> is_sint8 x -> to_sint8 (lsr x y) = lsr x y.
Proof.	
  lsr_in_sint_range 128.
Qed.					       

(* sint16 *)
Lemma is_sint16_lnot: forall x: Z,
  is_sint16 x -> to_sint16 (lnot x) = lnot x. 
Proof.
  is_sint_lnot 32768. 
Qed.

Lemma is_sint16_lxor: forall x y: Z,
  is_sint16 x -> is_sint16 y -> to_sint16 (lxor x y) = lxor x y.
Proof.
  is_sint_bitwise xorb 15%nat.  
Qed.						       

Lemma is_sint16_lor: forall x y: Z,
  is_sint16 x -> is_sint16 y -> to_sint16 (lor x y) = lor x y.
Proof.
  is_sint_bitwise orb 15%nat.  
Qed.						       

Lemma is_sint16_land: forall x y: Z,
  is_sint16 x -> is_sint16 y -> to_sint16 (land x y) = land x y.						       
Proof.
  is_sint_bitwise andb 15%nat.  
Qed.						       

Lemma is_sint16_lsr: forall x y: Z,
  0 <= y -> is_sint16 x -> to_sint16 (lsr x y) = lsr x y.
Proof.	
  lsr_in_sint_range 32768.
Qed.						       

(* sint32 *)
Lemma is_sint32_lnot: forall x: Z,
  is_sint32 x -> to_sint32 (lnot x) = lnot x. 
Proof.
  is_sint_lnot 2147483648. 
Qed.

Lemma is_sint32_lxor: forall x y: Z,
  is_sint32 x -> is_sint32 y -> to_sint32 (lxor x y) = lxor x y.
Proof.
  is_sint_bitwise xorb 31%nat.  
Qed.						       

Lemma is_sint32_lor: forall x y: Z,
  is_sint32 x -> is_sint32 y -> to_sint32 (lor x y) = lor x y.
Proof.
  is_sint_bitwise orb 31%nat.  
Qed.						       

Lemma is_sint32_land: forall x y: Z,
  is_sint32 x -> is_sint32 y -> to_sint32 (land x y) = land x y.
Proof.
  is_sint_bitwise andb 31%nat.  
Qed.						       

Lemma is_sint32_lsr: forall x y: Z,
  0 <= y -> is_sint32 x -> to_sint32 (lsr x y) = lsr x y.
Proof.	
  lsr_in_sint_range 2147483648.
Qed.						       

(* sint64 *)
Lemma is_sint64_lnot: forall x: Z,
 is_sint64 x -> to_sint64 (lnot x) = lnot x. 
Proof.
  is_sint_lnot 9223372036854775808. 
Qed.

Lemma is_sint64_lxor: forall x y: Z,
  is_sint64 x -> is_sint64 y -> to_sint64 (lxor x y) = lxor x y.
Proof.
  is_sint_bitwise xorb 63%nat.  
Qed.						       

Lemma is_sint64_lor: forall x y: Z,
  is_sint64 x -> is_sint64 y -> to_sint64 (lor x y) = lor x y.
Proof.
  is_sint_bitwise orb 63%nat.  
Qed.						       

Lemma is_sint64_land: forall x y: Z,
  is_sint64 x -> is_sint64 y -> to_sint64 (land x y) = land x y.
Proof.
  is_sint_bitwise andb 63%nat.  
Qed.						       

Lemma is_sint64_lsr: forall x y: Z,
  0 <= y -> is_sint64 x -> to_sint64 (lsr x y) = lsr x y.
Proof.	
  lsr_in_sint_range 9223372036854775808.
Qed.						       


(** ** Unsigned conversions *)

(* uint8 *)
Lemma is_uint8_lor: forall x y: Z,
  is_uint8 x -> is_uint8 y -> to_uint8 (lor x y) = lor x y.
Proof.
  is_uint_bitwise orb 8%nat. 
Qed.						       

Lemma is_uint8_land: forall x y: Z,
  is_uint8 x -> is_uint8 y -> to_uint8 (land x y) = land x y.
Proof.
  is_uint_bitwise andb 8%nat.  
Qed.						       

Lemma is_uint8_lsr: forall x y: Z,
  0 <= y -> is_uint8 x -> to_uint8 (lsr x y) = lsr x y.
Proof.	
  lsr_in_uint_range 256.
Qed.						       

(* uint16 *)
Lemma is_uint16_lor: forall x y: Z,
  is_uint16 x -> is_uint16 y -> to_uint16 (lor x y) = lor x y.
Proof.
  is_uint_bitwise orb 16%nat.  
Qed.						       

Lemma is_uint16_land: forall x y: Z,
  is_uint16 x -> is_uint16 y -> to_uint16 (land x y) = land x y.
Proof.
  is_uint_bitwise andb 16%nat.  
Qed.						       

Lemma is_uint16_lsr: forall x y: Z,
  0 <= y -> is_uint16 x -> to_uint16 (lsr x y) = lsr x y.
Proof.	
  lsr_in_uint_range 65536.
Qed.						       

(* uint32 *)
Lemma is_uint32_lor: forall x y: Z,
  is_uint32 x -> is_uint32 y -> to_uint32 (lor x y) = lor x y.
Proof.
  is_uint_bitwise orb 32%nat.  
Qed.						       

Lemma is_uint32_land: forall x y: Z,
  is_uint32 x -> is_uint32 y -> to_uint32 (land x y) = land x y.
Proof.
  is_uint_bitwise andb 32%nat.  
Qed.						       

Lemma is_uint32_lsr: forall x y: Z,
  0 <= y -> is_uint32 x -> to_uint32 (lsr x y) = lsr x y.
Proof.	
  lsr_in_uint_range 4294967296.  
Qed.						       

(* uint64 *)
Lemma is_uint64_lor: forall x y: Z,
  is_uint64 x -> is_uint64 y -> to_uint64 (lor x y) = lor x y.
Proof.
  is_uint_bitwise orb 64%nat.  
Qed.						       

Lemma is_uint64_land: forall x y: Z,
  is_uint64 x -> is_uint64 y -> to_uint64 (land x y) = land x y.
Proof.
  is_uint_bitwise andb 64%nat.  
Qed.						       

Lemma is_uint64_lsr: forall x y: Z,
  0 <= y -> is_uint64 x -> to_uint64 (lsr x y) = lsr x y.
Proof.	
  lsr_in_uint_range 18446744073709551616.  
Qed.		

(** * Tacticals. *)

(** ** Main tactics.*)
Ltac rewrite_cst :=
  first [ Bits.rewrite_cst
        | COMPUTE1 bitwise_lsl Cst_Z Cst_nat	
        | COMPUTE1 bitwise_lsr Cst_Z Cst_nat
        | COMPUTE1 lsl_shift_def Cst_Z Cst_nat
        | COMPUTE1 lsr_shift_def Cst_Z Cst_nat
        | COMPUTE1 lsl_arithmetic_def Cst_Z Cst_nat
        | COMPUTE1 lsr_arithmetic_def Cst_Z Cst_nat
        | COMPUTE1 lsl_def Cst_Z Cst_Z
        | COMPUTE1 lsr_def Cst_Z Cst_Z
        | COMPUTE1 land Cst_Z Cst_Z
        | COMPUTE1 lor Cst_Z Cst_Z
        | COMPUTE1 lxor Cst_Z Cst_Z
        | COMPUTE1 lnot Cst_Z
        | COMPUTE1 zbit_test_def Cst_Z Cst_Z
        ].
	
(** Example of use. *)
Remark rewrite_cst_example: forall x, x + (land 0 (zlnot (land 0 5))) = x + Z_of_nat (ZxHpos 0).
Proof.
  repeat rewrite_cst.
  intro. auto.
Qed.
