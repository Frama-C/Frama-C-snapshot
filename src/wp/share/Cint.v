(**************************************************************************)
(*                                                                        *)
(*  This file is part of WP plug-in of Frama-C.                           *)
(*                                                                        *)
(*  Copyright (C) 2007-2012                                               *)
(*    CEA (Commissariat a l'énergie atomique et aux énergies              *)
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
(* --- C-Integer Library for Coq                                          --- *)
(* -------------------------------------------------------------------------- *)

Require Import ZArith.
Require Import MinMax.	       

Open Local Scope Z_scope.

(* C-Integer Bits Operations *)

Fixpoint Pdigits (p:positive) : nat :=
  match p with 						   
  | xH => S O
  | xI p => S (Pdigits p)
  | xO p => S (Pdigits p)
  end.

Definition Zdigits (x :Z) : nat :=
  match x with 						   
  | Z0     => Pdigits xH
  | Zpos p => Pdigits p
  | Zneg xH => Pdigits xH
  | Zneg (xI p) => Pdigits p
  | Zneg (xO p) => S (Pdigits p)
  end.
			 
Definition Zmax_digits (x y:Z) : nat :=
  max (Zdigits x) (Zdigits y). 
			 
Lemma Zmax_digits_commut:
  forall a b: Z, Zmax_digits a b = Zmax_digits b a.
Proof.
  intros. unfold Zmax_digits. apply max_comm.
Qed.
				    
Definition Z_bin_decomp (x: Z) : bool * Z :=
  match x with
  | Z0 => (false, 0)
  | Zpos p =>
      match p with
      | xI q => (true, Zpos q)
      | xO q => (false, Zpos q)
      | xH => (true, 0)
      end
  | Zneg p =>
      match p with
      | xI q => (true, (Zneg q - 1))
      | xO q => (false, Zneg q)
      | xH => (true, (-1))
      end
end.

Definition Z_shift_add (b: bool) (x: Z) :=
   if b then 2 * x + 1 else 2 * x.

Lemma Z_shift_add_bin_decomp:
  forall x,
  Z_shift_add (fst (Z_bin_decomp x)) (snd (Z_bin_decomp x)) = x.
Proof.
  destruct x; simpl.
  auto.
  destruct p; reflexivity.
  destruct p; try reflexivity. simpl.
  assert (forall z, 2 * (z + 1) - 1 = 2 * z + 1). intro; omega.
  generalize (H (Zpos p)); simpl. congruence.
Qed.

Lemma Z_bin_decomp_shift_add:
  forall b x, Z_bin_decomp (Z_shift_add b x) = (b, x).
Proof.
  intros.
  intros. unfold Z_shift_add. destruct b; destruct x; simpl; auto.
  destruct p; simpl; auto. f_equal. f_equal.
  rewrite <- Pplus_one_succ_r. apply Psucc_o_double_minus_one_eq_xO.
Qed.

Lemma Z_shift_add_inj:
  forall b1 x1 b2 x2,
  Z_shift_add b1 x1 = Z_shift_add b2 x2 -> b1 = b2 /\ x1 = x2.
Proof.
  intros.
  assert ((b1, x1) = (b2, x2)).
    repeat rewrite <- Z_bin_decomp_shift_add. rewrite H; auto.
  split; congruence.
Qed.
  
Fixpoint bits_of_Z (n: nat) (x: Z) {struct n}: Z -> bool :=
  match n with
  | O =>
      (fun i: Z => false)
  | S m =>
      let (b, y) := Z_bin_decomp x in
      let f := bits_of_Z m y in
      (fun i: Z => match i with
                   | Z0 => b
                   | Zpos _ => f (i-1)
                   | Zneg _ => false
                    end)
  end.

Remark bits_of_Z_zero:
  forall n x, bits_of_Z n 0 x = false.
Proof.
  induction n; simpl; intros. auto. destruct x; auto. 
Qed.

Fixpoint Zpos_of_bits (n: nat) (f: Z -> bool) (i: Z) {struct n}: Z :=
  match n with
  | O => Z0
  | S m => Z_shift_add (f i) (Zpos_of_bits m f (Zsucc i))
  end.

Definition lnot x:Z := -(x + 1).

Definition Z_of_bits (n: nat) (f: Z -> bool): Z :=
  match f (Z_of_nat (S n)) with
  | false => Zpos_of_bits n f Z0
  | true => lnot (Zpos_of_bits n (fun i: Z => negb (f i)) Z0)
  end.
		       
Definition Zbitwise_binop (f: bool -> bool -> bool) (x y: Z) : Z :=
  let rxy := Zmax_digits x y in
  let fx := bits_of_Z rxy x in
  let fy := bits_of_Z rxy y in	
    Z_of_bits rxy (fun i: Z => f (fx i) (fy i)).
						   
Lemma Zbitwise_binop_commut:
  forall f,
  (forall a b, f a b = f b a) ->
  forall x y,
  Zbitwise_binop f x y = Zbitwise_binop f y x.		    
Proof.
  unfold Zbitwise_binop. 
  intros.  
  f_equal.  
  apply Zmax_digits_commut.
  rewrite Zmax_digits_commut.
  Require Import FunctionalExtensionality.  
  extensionality ext. apply H.  
Qed.
  
Lemma Zbitwise_binop_assoc:
  forall f,
  (forall a b c, f a (f b c) = f (f a b) c) ->
  forall x y z,
  Zbitwise_binop f (Zbitwise_binop f x y) z =
  Zbitwise_binop f x (Zbitwise_binop f y z).				    
Proof.
  admit.			        
Qed.

(* C-Integer Ranges *)

Definition is_uint8(x:Z) := 0 <= x < 256.
Definition is_sint8(x:Z) := -128 <= x < 128.
Definition is_uint16(x:Z) := 0 <= x < 65536.
Definition is_sint16(x:Z) := -32768 <= x < 32768.
Definition is_uint32(x:Z) := 0 <= x < 4294967296.
Definition is_sint32(x:Z) := -2147483648 <= x < 2147483648.
Definition is_uint64(x:Z) := 0 <= x < 18446744073709551616.
Definition is_sint64(x:Z) := -9223372036854775808 <= x < 9223372036854775808.

Definition to_range a b z := a + (z-a) mod (b-a).

Lemma is_to_range: forall a b z, a<b -> a <= to_range a b z < b.
Proof.
  intros.
  unfold to_range.
  assert (Q : b-a > 0) ; auto with zarith.
  generalize (Z_mod_lt (z-a) (b-a) Q).
  intro R.
  auto with zarith.
Qed.

Definition to_uint8 := to_range 0 256.
Definition to_sint8 := to_range (-128) 128.
Definition to_uint16 := to_range 0 65536.
Definition to_sint16 := to_range (-32768) 32768.
Definition to_uint32 := to_range 0 4294967296.
Definition to_sint32 := to_range (-2147483648) 2147483648.
Definition to_uint64 := to_range 0 18446744073709551616.
Definition to_sint64 := to_range (-9223372036854775808) 9223372036854775808.

(* C-Integer Conversions are in-range *)

Local Ltac to_range := intro x ; apply is_to_range ; omega.

Lemma is_to_uint8 : forall x, is_uint8(to_uint8 x). to_range. Qed.
Lemma is_to_sint8 : forall x, is_sint8(to_sint8 x). to_range. Qed.
Lemma is_to_uint16 : forall x, is_uint16(to_uint16 x). to_range. Qed.
Lemma is_to_sint16 : forall x, is_sint16(to_sint16 x). to_range. Qed.
Lemma is_to_uint32 : forall x, is_uint32(to_uint32 x). to_range. Qed.
Lemma is_to_sint32 : forall x, is_sint32(to_sint32 x). to_range. Qed.
Lemma is_to_uint64 : forall x, is_uint64(to_uint64 x). to_range. Qed.
Lemma is_to_sint64 : forall x, is_sint64(to_sint64 x). to_range. Qed.

(* C-Integer Conversions are identity when in-range *)

Lemma id_to_range : forall a b x, a <= x < b -> to_range a b x = x.
Proof.
  intros a b x Range. unfold to_range.
  assert (Q : b-a > 0) ; auto with zarith.
  cut ((x-a) mod (b-a) = (x-a)). omega.
  apply Zmod_small. omega.
Qed.
  
Local Ltac id_range := intro x ; apply id_to_range ; omega.

Lemma id_uint8 : forall x, is_uint8 x -> (to_uint8 x) = x. id_range. Qed. 
Lemma id_sint8 : forall x, is_sint8 x -> (to_sint8 x) = x. id_range. Qed.
Lemma id_uint16 : forall x, is_uint16 x -> (to_uint16 x) = x. id_range. Qed.
Lemma id_sint16 : forall x, is_sint16 x -> (to_sint16 x) = x. id_range. Qed.
Lemma id_uint32 : forall x, is_uint32 x -> (to_uint32 x) = x. id_range. Qed.
Lemma id_sint32 : forall x, is_sint32 x -> (to_sint32 x) = x. id_range. Qed.
Lemma id_uint64 : forall x, is_uint64 x -> (to_uint64 x) = x. id_range. Qed.
Lemma id_sint64 : forall x, is_sint64 x -> (to_sint64 x) = x. id_range. Qed.

(* C-Integer Conversions are projections *)
    
Local Ltac proj := intro x ; apply id_to_range ; apply is_to_range ; omega.

Lemma proj_uint8 : forall x, to_uint8(to_uint8 x)=to_uint8 x. 
Proof. proj. Qed.
Lemma proj_sint8 : forall x, to_sint8(to_sint8 x)=to_sint8 x. 
Proof. proj. Qed.
Lemma proj_uint16 : forall x, to_uint16(to_uint16 x)=to_uint16 x. 
Proof. proj. Qed.
Lemma proj_sint16 : forall x, to_sint16(to_sint16 x)=to_sint16 x. 
Proof. proj. Qed.
Lemma proj_uint32 : forall x, to_uint32(to_uint32 x)=to_uint32 x. 
Proof. proj. Qed.
Lemma proj_sint32 : forall x, to_sint32(to_sint32 x)=to_sint32 x. 
Proof. proj. Qed.
Lemma proj_uint64 : forall x, to_uint64(to_uint64 x)=to_uint64 x. 
Proof. proj. Qed.
Lemma proj_sint64 : forall x, to_sint64(to_sint64 x)=to_sint64 x. 
Proof. proj. Qed.

Definition land (x y: Z): Z := Zbitwise_binop andb x y.
Definition lor (x y: Z): Z := Zbitwise_binop orb x y.
Definition lxor (x y: Z): Z := Zbitwise_binop xorb x y.

Parameter lsr  : Z -> Z -> Z.
Definition lsr_def (x:Z) (n:nat) := x / (two_power_nat n).
Axiom lsr_partial_def : forall x y : Z, 
  y >=0 -> lsr x y = lsr_def x (Zabs_nat y).
  
Parameter lsl  : Z -> Z -> Z.
Definition lsl_def (x:Z) (n:nat) := x * (two_power_nat n).
Axiom lsl_partial_def : forall x y : Z, 
  y >=0 -> lsl x y = lsl_def x (Zabs_nat y).
						       
(* land is AC *)
Theorem land_assoc: forall x y z, land (land x y) z = land x (land y z).
Proof.
  Require Import Bool.  
  apply (Zbitwise_binop_assoc andb andb_assoc).			       
Qed.

Theorem land_commut: forall x y, land x y = land y x.
Proof.
 Require Import Bool.  
 apply (Zbitwise_binop_commut andb andb_comm).
Qed.
  
(* lxor is AC *)
Theorem lxor_assoc: forall x y z, lxor (lxor x y) z = lxor x (lxor y z).
Proof.
  Require Import Bool.  
  apply (Zbitwise_binop_assoc xorb).
  intros. symmetry. apply xorb_assoc.			       
Qed.

Theorem lxor_commut: forall x y, lxor x y = lxor y x.
Proof.
 Require Import Bool.  
 apply (Zbitwise_binop_commut xorb xorb_comm).
Qed.
						       
(* lor is AC *)
Theorem lor_assoc: forall x y z, lor (lor x y) z = lor x (lor y z).
Proof.
  Require Import Bool.  
  apply (Zbitwise_binop_assoc orb orb_assoc).			       
Qed.

Theorem lor_commut: forall x y, lor x y = lor y x.
Proof.
 Require Import Bool.  
 apply (Zbitwise_binop_commut orb orb_comm).
Qed.

(* others *)
Lemma bnot_inject : forall x y : Z, (lnot x) = (lnot y) -> x = y.
Proof.
  unfold lnot. intros. omega.
Qed.
						       
Lemma bnot_invol2 : forall x : Z, lnot (lnot x) = x.
Proof.
  unfold lnot. intros. omega.
Qed.						       
Lemma band_idem : forall x : Z, land x x = x.
Admitted.
Lemma bor_idem : forall x : Z, lor x x = x.
Admitted.						       
Lemma bxor_idem : forall x : Z, lxor x x = -1.
Admitted.						       
Lemma bnot_band_de_morgan : forall x y : Z, lnot (land x y) = lor (lnot x) (lnot y).
Admitted.						       
Lemma bnot_bor_de_morgan : forall x y : Z, lnot (lor x y) = land (lnot x) (lnot y).
Admitted.						       
Lemma bnot_bxor : forall x y : Z, lnot (lxor x y) = lxor (lnot x) y.
Admitted.						       
Lemma bor_band_distrib : forall x y z : Z, lor (land x y) z = land (lor x z) (lor y z).
Admitted.						       
Lemma band_bor_distrib : forall x y z : Z, land (lor x y) z = lor (land x z) (land y z).
Admitted.						       
Lemma band_bxor_distrib : forall x y z : Z, land (lxor x y) z = lxor (land x z) (land y z).
Admitted.						       
Lemma bnot_0 : (lnot 0) = -1.
Proof.
  unfold lnot. intros. omega.
Qed.						       
Lemma bor_0 : forall x : Z, (lor 0 x) = x.
Admitted.						       
Lemma band_0 : forall x : Z, (land 0 x) = 0.
Admitted.						       
Lemma bxor_0 : forall x : Z, (lxor 0 x) = x.
Admitted.						       
  
(* Some C-Integer Bits Conversions are identity *)

(* Signed conversions *)

Lemma bnot_in_range : forall a z: Z,
 -a <= z < a -> -a <= (lnot z) < a.
Proof.
  intros.
  unfold lnot.
  omega.
Qed.						       							
Local Ltac is_sint_bnot n := intro x; intro Q; apply id_to_range; apply (bnot_in_range n x Q). 

(* sint8 *)
Lemma is_sint8_bnot : forall x: Z,
 is_sint8 x -> to_sint8 (lnot x) = lnot x. is_sint_bnot 128. Qed.
						       
Lemma is_sint8_bxor : forall x y: Z,
 is_sint8 x -> is_sint8 y -> to_sint8 (lxor x y) = lxor x y.
Admitted.						       

Lemma is_sint8_bor : forall x y: Z,
 is_sint8 x -> is_sint8 y -> to_sint8 (lor x y) = lor x y.
Admitted.						       

Lemma is_sint8_band : forall x y: Z,
 is_sint8 x -> is_sint8 y -> to_sint8 (land x y) = land x y.
Admitted.						       

Lemma is_sint8_blsr : forall x y: Z,
 is_sint8 x -> is_sint8 y -> to_sint8 (lsr x y) = lsr x y.
Admitted.						       

(* sint16 *)
Lemma is_sint16_bnot : forall x: Z,
 is_sint16 x -> to_sint16 (lnot x) = lnot x. is_sint_bnot 32768. Qed.

Lemma is_sint16_bxor : forall x y: Z,
 is_sint16 x ->  is_sint16 y ->  to_sint16 (lxor x y) = lxor x y.
Admitted.						       

Lemma is_sint16_bor : forall x y: Z,
 is_sint16 x -> is_sint16 y -> to_sint16 (lor x y) = lor x y.
Admitted.						       

Lemma is_sint16_band : forall x y: Z,
 is_sint16 x -> is_sint16 y -> to_sint16 (land x y) = land x y.
Admitted.						       

Lemma is_sint16_blsr : forall x y: Z,
 is_sint16 x -> is_sint16 y -> to_sint16 (lsr x y) = lsr x y.
Admitted.						       

(* sint32 *)
Lemma is_sint32_bnot : forall x: Z,
 is_sint32 x -> to_sint32 (lnot x) = lnot x. is_sint_bnot 2147483648. Qed.

Lemma is_sint32_bxor : forall x y: Z,
 is_sint32 x ->  is_sint32 y -> to_sint32 (lxor x y) = lxor x y.
Admitted.						       

Lemma is_sint32_bor : forall x y: Z,
 is_sint32 x -> is_sint32 y -> to_sint32 (lor x y) = lor x y.
Admitted.						       

Lemma is_sint32_band : forall x y: Z,
 is_sint32 x -> is_sint32 y -> to_sint32 (land x y) = land x y.
Admitted.						       

Lemma is_sint32_blsr : forall x y: Z,
 is_sint32 x -> is_sint32 y -> to_sint32 (lsr x y) = lsr x y.
Admitted.						       

(* sint64 *)
Lemma is_sint64_bnot : forall x: Z,
 is_sint64 x -> to_sint64 (lnot x) = lnot x. is_sint_bnot 9223372036854775808. Qed.

Lemma is_sint64_bxor : forall x y: Z,
 is_sint64 x ->  is_sint64 y -> to_sint64 (lxor x y) = lxor x y.
Admitted.						       

Lemma is_sint64_bor : forall x y: Z,
 is_sint64 x -> is_sint64 y -> to_sint64 (lor x y) = lor x y.
Admitted.						       

Lemma is_sint64_band : forall x y: Z,
 is_sint64 x -> is_sint64 y -> land x y = to_sint64 (land x y).
Admitted.						       

Lemma is_sint64_blsr : forall x y: Z,
 is_sint64 x -> is_sint64 y -> to_sint64 (lsr x y) = lsr x y.
Admitted.						       


(* Unsigned conversions *)

(* uint8 *)
Lemma is_uint8_bor : forall x y: Z,
 is_uint8 x ->  is_uint8 y -> to_uint8 (lor x y) = lor x y.
Admitted.						       

Lemma is_uint8_band : forall x y: Z,
 is_uint8 x -> is_uint8 y -> to_uint8 (land x y) = land x y.
Admitted.						       

Lemma is_uint8_blsr : forall x y: Z,
 is_uint8 x -> is_uint8 y -> to_uint8 (lsr x y) = lsr x y.
Admitted.						       

(* uint16 *)
Lemma is_uint16_bor : forall x y: Z,
 is_uint16 x ->  is_uint16 y -> to_uint16 (lor x y) = lor x y.
Admitted.						       

Lemma is_uint16_band : forall x y: Z,
 is_uint16 x -> is_uint16 y -> to_uint16 (land x y) = land x y.
Admitted.						       

Lemma is_uint16_blsr : forall x y: Z,
 is_uint16 x -> is_uint16 y -> to_uint16 (lsr x y) = lsr x y.
Admitted.						       

(* uint32 *)
Lemma is_uint32_bor : forall x y: Z,
 is_uint32 x ->  is_uint32 y -> to_uint32 (lor x y) = lor x y.
Admitted.						       

Lemma is_uint32_band : forall x y: Z,
 is_uint32 x -> is_uint32 y -> to_uint32 (land x y) = land x y.
Admitted.						       

Lemma is_uint32_blsr : forall x y: Z,
 is_uint32 x -> is_uint32 y -> to_uint32 (lsr x y) = lsr x y.
Admitted.						       

(* uint64 *)
Lemma is_uint64_bor : forall x y: Z,
 is_uint64 x ->  is_uint64 y -> to_uint64 (lor x y) = lor x y.
Admitted.						       

Lemma is_uint64_band : forall x y: Z,
 is_uint64 x -> is_uint64 y -> to_uint64 (land x y) = land x y.
Admitted.						       

Lemma is_uint64_blsr : forall x y: Z,
 is_uint64 x -> is_uint64 y -> to_uint64 (lsr x y) = lsr x y.
Admitted.						       
