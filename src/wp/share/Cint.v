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
(* --- C-Integer Library for Coq                                          --- *)
(* -------------------------------------------------------------------------- *)

Require Import ZArith.
Require Import Qedlib.

Open Local Scope Z_scope.
  
(** * C-Integer Ranges *)

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

(** * C-Integer Conversions are in-range *)

Local Ltac to_range := intro x ; apply is_to_range ; omega.

Lemma is_to_uint8 : forall x, is_uint8(to_uint8 x). 
Proof. to_range. Qed.
Lemma is_to_sint8 : forall x, is_sint8(to_sint8 x). 
Proof. to_range. Qed.
Lemma is_to_uint16 : forall x, is_uint16(to_uint16 x). 
Proof. to_range. Qed.
Lemma is_to_sint16 : forall x, is_sint16(to_sint16 x). 
Proof. to_range. Qed.
Lemma is_to_uint32 : forall x, is_uint32(to_uint32 x). 
Proof. to_range. Qed.
Lemma is_to_sint32 : forall x, is_sint32(to_sint32 x). 
Proof. to_range. Qed.
Lemma is_to_uint64 : forall x, is_uint64(to_uint64 x). 
Proof. to_range. Qed.
Lemma is_to_sint64 : forall x, is_sint64(to_sint64 x). 
Proof. to_range. Qed.

(** * C-Integer Conversions are identity when in-range *)

Lemma id_to_range : forall a b x, a <= x < b -> to_range a b x = x.
Proof.
  intros a b x Range. unfold to_range.
  assert (Q : b-a > 0) ; auto with zarith.
  cut ((x-a) mod (b-a) = (x-a)). omega.
  apply Zmod_small. omega.
Qed.
  
Local Ltac id_range := intro x ; apply id_to_range ; omega.

Lemma id_uint8 : forall x, is_uint8 x -> (to_uint8 x) = x. 
Proof.  id_range. Qed. 
Lemma id_sint8 : forall x, is_sint8 x -> (to_sint8 x) = x. 
Proof.  id_range. Qed.
Lemma id_uint16 : forall x, is_uint16 x -> (to_uint16 x) = x. 
Proof.  id_range. Qed.
Lemma id_sint16 : forall x, is_sint16 x -> (to_sint16 x) = x. 
Proof.  id_range. Qed.
Lemma id_uint32 : forall x, is_uint32 x -> (to_uint32 x) = x. 
Proof.  id_range. Qed.
Lemma id_sint32 : forall x, is_sint32 x -> (to_sint32 x) = x. 
Proof.  id_range. Qed.
Lemma id_uint64 : forall x, is_uint64 x -> (to_uint64 x) = x. 
Proof.  id_range. Qed.
Lemma id_sint64 : forall x, is_sint64 x -> (to_sint64 x) = x. 
Proof.  id_range. Qed.

(** * C-Integer Conversions are projections *)
    
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

(** * Tacticals. *)
    
Fixpoint Cst_nat n := 
  match n with O => true | S c => Cst_nat c 
  end.
Fixpoint Cst_pos p := 
  match p with xH => true | xI c | xO c => Cst_pos c 
  end.
Fixpoint Cst_N n := 
  match n with N0 => true | Npos c => Cst_pos c 
  end.
Definition Cst_Z x := 
  match x with Z0 => true | Zpos c | Zneg c => Cst_pos c 
  end.
Ltac COMPUTE e :=
  let R := fresh in pose (R := e); fold R; compute in R; unfold R; clear R.
Ltac COMPUTE_HYP h e :=
  let R := fresh in pose (R := e); fold R in h; compute in R; unfold R in h; clear R.
Ltac GUARD cst e := 
  let E := fresh in pose (E := cst e); compute in E; 
  match goal with
    | [ E:=true |- _] => clear E
  end.
Ltac COMPUTE1 f cst := 
  match goal with 
   | [ |- context[f ?e] ]      => GUARD cst e; COMPUTE (f e)
   | [ H:=context[f ?e] |- _ ] => GUARD cst e; COMPUTE_HYP H (f e)
   | [ H: context[f ?e] |- _ ] => GUARD cst e; COMPUTE_HYP H (f e)
  end.
Ltac COMPUTE2 f cst1 cst2 := 
  match goal with  
   | [ |- context[f ?e1 ?e2] ]     => GUARD cst1 e1; GUARD cst2 e2; COMPUTE (f e1 e2)
   | [ H:=context[f ?e1 ?e2] |- _] => GUARD cst1 e1; GUARD cst2 e2; COMPUTE_HYP H (f e1 e2)
   | [ H: context[f ?e1 ?e2] |- _] => GUARD cst1 e1; GUARD cst2 e2; COMPUTE_HYP H (f e1 e2)
  end.
Ltac COMPUTE2AC f cst tac := 
  match goal with  
   | [ |- context[f ?e1 (f ?e2 ?e3) ]] => GUARD cst e1; 
                                          first [ (GUARD cst e2; (replace (f e1 (f e2 e3)) -> (f e3 (f e1 e2)) by tac); COMPUTE (f e1 e2))
						| (GUARD cst e3; (replace (f e1 (f e2 e3)) -> (f e2 (f e1 e3)) by tac); COMPUTE (f e1 e3))]
   | [ |- context[f (f ?e3 ?e2) ?e1 ]] => GUARD cst e1; 
                                          first [ (GUARD cst e2; (replace (f (f e3 e2) e1) -> (f e3 (f e2 e1)) by tac); COMPUTE (f e2 e1))
						| (GUARD cst e3; (replace (f (f e3 e2) e1) -> (f e2 (f e3 e1)) by tac); COMPUTE (f e3 e1))]
   | [ H:=context[f ?e1 (f ?e2 ?e3) ] |- _] => GUARD cst e1; 
                                          first [ (GUARD cst e2; (replace (f e1 (f e2 e3)) -> (f e3 (f e1 e2)) by tac in H); COMPUTE_HYP H (f e1 e2))
						| (GUARD cst e3; (replace (f e1 (f e2 e3)) -> (f e2 (f e1 e3)) by tac in H); COMPUTE_HYP H (f e1 e3))]
   | [ H:=context[f (f ?e3 ?e2) ?e1 ] |- _] => GUARD cst e1; 
                                          first [ (GUARD cst e2; (replace (f (f e3 e2) e1) -> (f e3 (f e2 e1)) by tac in H); COMPUTE_HYP H (f e2 e1))
						| (GUARD cst e3; (replace (f (f e3 e2) e1) -> (f e2 (f e3 e1)) by tac in H); COMPUTE_HYP H (f e3 e1))]
   | [ H: context[f ?e1 (f ?e2 ?e3) ] |- _] => GUARD cst e1; 
                                          first [ (GUARD cst e2; (replace (f e1 (f e2 e3)) -> (f e3 (f e1 e2)) by tac in H); COMPUTE (f e1 e2))
						| (GUARD cst e3; (replace (f e1 (f e2 e3)) -> (f e2 (f e1 e3)) by tac in H); COMPUTE_HYP H (f e1 e3))]
   | [ H: context[f (f ?e3 ?e2) ?e1 ] |- _] => GUARD cst e1; 
                                          first [ (GUARD cst e2; (replace (f (f e3 e2) e1) -> (f e3 (f e2 e1)) by tac in H); COMPUTE_HYP H (f e2 e1))
						| (GUARD cst e3; (replace (f (f e3 e2) e1) -> (f e2 (f e3 e1)) by tac in H); COMPUTE_HYP H (f e3 e1))]
  end.
Ltac COMPUTE3 f cst1 cst2 cst3 := 
  match goal with  
   | [ |- context[f ?e1 ?e2 ?e3] ]      => GUARD cst1 e1; GUARD cst2 e2; GUARD cst3 e3; COMPUTE (f e1 e2 e3)
   | [ H:=context[f ?e1 ?e2 ?e3] |- _ ] => GUARD cst1 e1; GUARD cst2 e2; GUARD cst3 e3; COMPUTE_HYP H (f e1 e2 e3)
   | [ H: context[f ?e1 ?e2 ?e3] |- _ ] => GUARD cst1 e1; GUARD cst2 e2; GUARD cst3 e3; COMPUTE_HYP H (f e1 e2 e3)
  end.

(** ** Main tactics.*)
Ltac ring_tactic := ring.
  
Ltac rewrite_cst :=
  first [ COMPUTE Zopp Cst_Z 
        | COMPUTE Zsucc Cst_Z
        | COMPUTE Zpred Cst_Z
        | COMPUTE Zdouble_plus_one Cst_Z
        | COMPUTE Zdouble_minus_one Cst_Z
        | COMPUTE Zdouble Cst_Z
        | COMPUTE Zabs Cst_Z
	  
        | COMPUTE Zabs_N Cst_Z
        | COMPUTE Zabs_nat Cst_Z

        | COMPUTE Z_of_N Cst_N 
        | COMPUTE Z_of_nat Cst_nat
        | COMPUTE two_power_nat Cst_nat

        | COMPUTE2 Zminus Cst_Z Cst_Z
        | COMPUTE2 Zplus Cst_Z Cst_Z
        | COMPUTE2 Zmult Cst_Z Cst_Z

        | COMPUTE2AC Zplus Cst_Z ring_tactic
        | COMPUTE2AC Zmult Cst_Z ring_tactic
	  
        | COMPUTE to_uint8 Cst_Z
        | COMPUTE to_sint8 Cst_Z
        | COMPUTE to_uint16 Cst_Z
        | COMPUTE to_sint16 Cst_Z
        | COMPUTE to_uint32 Cst_Z
        | COMPUTE to_sint32 Cst_Z
        | COMPUTE to_uint64 Cst_Z
        | COMPUTE to_sint64 Cst_Z
        | COMPUTE3 to_range Cst_Z Cst_Z Cst_Z
        ].

(** Example of use. *)
Remark rewrite_cst_example: forall x y, 1 + ((2 * x) * 3 + 2) = (3 * (2 * y)+ 2) + 1 -> 1 + (2 + (x * 2) * 3 ) = (2 + 3 * (y* 2)) + 1.
Proof.
  intros. repeat rewrite_cst. auto.
Qed.
