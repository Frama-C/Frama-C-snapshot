(**************************************************************************)
(*                                                                        *)
(*  This file is part of WP plug-in of Frama-C.                           *)
(*                                                                        *)
(*  Copyright (C) 2007-2019                                               *)
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
(** * C-Integer Library for Coq *)
(* -------------------------------------------------------------------------- *)

(** This module provides a theory of bits over [Z] natural integers.
     - for natural [n], the [k]-th bit of [2^n] if [(k=n)] ;
     - for positive integer [x>=0], it is the union of the bits of its binary 
       decomposition (hence, natural powers of two) ;
     - finally, the bits of a negative integer [x<0] are the reverted ones 
       of its two's complement [-(x+1)].

    The realization of the theory proceeds into several stages, 
    following the Coq definition of type [Z]. We take advantage of the 
    bitwize representation of positive integers provided by the [positive] 
    type in Coq. The successive stages are:
      - properties of {!trailing:bit-functions} (finally ending by 1-sequence or 0-sequence);
      - bits of {!positive:positive} integers [p>0];
      - bits of {!natural:natural} integers [n>=0];
      - bits of {!integer:integers} [n:Z].

    The {!Zbit:characteristic} function of integers, denoted [Zbit], 
    have the expected logical properties:
      - [(Zbit 0 k)] is [false];
      - [(Zbit (-1) k)] is [true];
      - [(Zbit (2^n) k)] is [(k=n)];
      - [Zbit] is injective, ie: the bit representation of each integer is unique.

*)

(** ** Type of characteristic functions of integers *)
Definition Zfc := nat -> bool.
	   
(** ** Tacticals *)

Require Import ZArith.
Require Import FunctionalExtensionality.
Require Import Qedlib.

Close Scope Z_scope.

(** Induction after a given rank. *)
Remark upper_nat_ind: forall P (n:nat),
  P n -> (forall k, (n < k) -> P k) -> (forall k, (n <= k) -> P k).
Proof.
  intros.
  case (le_lt_eq_dec n k); intuition (subst; auto with arith).
Qed. 

(** Induction over bool with equality. *)
Ltac case_eqb H e :=
  pattern e; apply Sumbool.bool_eq_ind; intro H.

(** Find arithmetic contradiction. *)
Ltac arithContradiction :=
  cut False; [contradiction; try omega|];auto with arith.

(** Cases [Inf:i<j], [EQ:i=j] and [Sup:i>j]. *)
Ltac nat_compare Inf EQ Sup i j :=
  destruct (lt_eq_lt_dec i j) as [ TMP | Sup ];
  [ destruct TMP as [ Inf | EQ ]; [ | try rewrite <- EQ ] | ];
  auto with arith.

(** Cases [Inf:i<j], [EQ:i=j] and [Sup:i>j]. *)
Ltac Z_compare Inf EQ Sup i j :=
  destruct (Z_dec i j) as [ TMP| EQ ];
  [ destruct TMP as [ Inf | Sup ] | try rewrite <- EQ ];
  auto with zarith.

(** For proving a symmetrical relation [P], 
    it is sufficient to prove [P i j] for [i<j] and [P i i]. *)
Lemma symmetrical_ind: forall (P : nat -> nat -> Prop),
   (forall i j, P i j -> P j i) ->
   (forall i, P i i) ->
   (forall i j, i < j -> P i j) ->
   (forall i j, P i j).
Proof.
  intros P Sym Diag Triangle i j.
  nat_compare Inf EQ Sup i j.
Qed.

(** * remarks about two_power_nat *)
Remark two_power_nat_is_positive: forall n,
  (0 < two_power_nat n)%Z.
Proof.
  induction n. 
  (** base *) 
  + by compute.
  (** ind. *) 
  + rewrite two_power_nat_S.
    apply Zmult_lt_0_compat.
    by compute.
    auto.
Qed.

Remark two_power_nat_plus: forall n m,
  (two_power_nat (n+m) = (two_power_nat n)*(two_power_nat m))%Z.
Proof.
  induction m.
  (replace (two_power_nat 0) with 1%Z by (compute;forward)).
  (replace (n + 0) with n by (auto with zarith)).
  ring.
  rewrite two_power_nat_S.
  replace (n + S m) with (S(n+m)) by (auto with zarith).
  rewrite two_power_nat_S.
  rewrite IHm.
  ring.
Qed.
						      
Remark two_power_nat_increase: forall n m,
  n <= m -> (two_power_nat n <= two_power_nat m)%Z.
Proof.
  intros.
  rewrite ((le_plus_minus n m) H).
  rewrite two_power_nat_plus.
  generalize (two_power_nat_is_positive (m - n)).
  pose (K:=(two_power_nat (m - n))); fold K; intro.  
  generalize (two_power_nat_is_positive n); intro.
  rewrite <- (Z.mul_1_r (two_power_nat n)) at 1.
  apply Zmult_le_compat_l; omega.
Qed.
						    
Remark two_power_nat_increase_strict: forall n m,
  n < m -> (two_power_nat n < two_power_nat m)%Z.
Proof.
  intros.
  rewrite (le_plus_minus (n+1) m) by omega.
  rewrite two_power_nat_plus.
  generalize (two_power_nat_is_positive (m - (n+1))).
  pose (K:=(two_power_nat (m - (n+1)))); fold K; intro.  
  rewrite two_power_nat_plus.
  replace (two_power_nat 1) with 2%Z by (compute; trivial).
  generalize (two_power_nat_is_positive n); intro.
  rewrite <- (Z.mul_1_r (two_power_nat n)) at 1.
  replace (two_power_nat n * 2 * K)%Z with (two_power_nat n * (2 * K))%Z
    by ring.
  apply Zmult_gt_0_lt_compat_l; omega.
Qed.
						    
(** {@trailing:} *)
(** * Eventually constant functions *)
(** The bits representation of [Z] integers are eventually constant
    [nat -> bool] functions. Positive integers finally ends with an infinite 
    sequence of 0-bits, while negative inetegers ends with 1-bits. 

    Hence, it is always possible to defined the highest significant sign-bit
    of a bit function. This section formalize these properties: predicate [trailing]
    defines an eventually constant bit function, and function [last] returns its
    highest significant bit.
*)

(** Function [f] has constant value [b] from rank [k]. *)
Definition trailing f (n:nat) (b:bool) := forall k, n <= k -> f k = b.

(** Returns the lowest index such than [f n=b], and [n] otherwise. *)
Fixpoint last f n b {struct n} :=
  match n with
  | O => O
  | S m => if Bool.eqb (f m) b then last f m b else n
  end.

(** Functions last decreases. *)
Remark last_leq : forall f n b, last f n b <= n.
Proof.
  intros f n b.
  induction n; auto.
  simpl.
  destruct (Bool.eqb (f n) b); auto.
Qed.

(** Trailing of previous position. *)
Remark trailing_step : forall f n b, 
  f n = b -> trailing f (S n) b -> trailing f n b.
Proof.
  intros f n b fn tl.
  unfold trailing.
  apply upper_nat_ind; auto with arith.
Qed.

(** Last preserves trailing. *)
Remark trailing_last : forall f n b, trailing f n b -> trailing f (last f n b) b.
Proof.
  intros f n b.
  induction n; simpl; auto.
  intro IHS.
  case_eqb H (Bool.eqb (f n) b); auto.
  apply IHn.
  apply trailing_step; [ apply Bool.eqb_prop | ]; auto.
Qed.

(** The [last] is null or points to a flip. *)
Remark last_null_or_flip: forall (f: Zfc) (n: nat) (b: bool), 
   last f n b = O \/ exists k, last f n b = S k /\ f k <> b.
Proof.
  intros f n b. induction n; simpl; auto.
  case_eqb BIT (Bool.eqb (f n) b). auto.
  right. exists n. split; [ auto | apply Bool.eqb_false_iff; auto ].
Qed.

(** The [last] of trailing is unique. *)
Lemma last_trail_ext: forall (f: Zfc) (b: bool) (n m: nat), 
  trailing f n b -> trailing f m b -> last f n b = last f m b.
Proof.
  intros f b.
  cut (forall n m,  
    trailing f n b -> trailing f m b -> last f n b < last f m b -> False).
  { intros ABSURD n m.
    intros Hn Hm.
    nat_compare INF EQ SUP (last f n b) (last f m b); auto.
    (** INF *) * apply False_ind; apply (ABSURD n m); auto.
    (** SUP *) * apply False_ind; apply (ABSURD m n); auto.
  }
  intros n m Hn Hm.
  pose ( i := last f n b ). fold i.
  pose ( j := last f m b ). fold j.
  intro Leq.
  assert (Hi : trailing f i b) by (unfold i; apply trailing_last; auto).
  assert (Hj : trailing f j b) by (unfold j; apply trailing_last; auto).
  assert (Range : forall k, i <= k <= j -> f k = b) 
      by (intros k [lo up]; auto with arith).
      
  generalize (last_null_or_flip f m b).
  intros [ Last_null | Last_flip ].
  (** Last is Null *)
  + fold j in Last_null. rewrite Last_null in Leq. omega.
  (** Last if a flip *)
  + destruct Last_flip as [ k [ kj flip ] ].
    fold j in kj. 
    absurd (f k = b); auto.
    apply Range; omega.
Qed.

(** {@positive:} *)
(** * Bits of positive integers *)

(** Strictly positive integers are represented in Coq by theirs bits,
    with lowest bits as head constructors, and highest bit at tail.

    Conversely, given a finite range of bits ended by a 1-bit, the
    reconstruction of a [positive] integer is defined.  *)

(** Position of the highest significant bit of a positive. *)
Fixpoint xHpos (p:positive): nat :=
  match p with 						   
  | xH => O
  | xI p => S (xHpos p)
  | xO p => S (xHpos p)
  end.

(** [xHpos] increases. *)
Remark xHpos_incr : 
  forall p a: positive, xHpos p <= xHpos (p + a).
Proof.
 induction p; intros; simpl; case a; intros; simpl; try omega;
 apply le_n_S;
 try rewrite Pplus_one_succ_r;
 try (rewrite Pplus_carry_spec; rewrite Pplus_one_succ_r;rewrite<- Pplus_assoc);
 try solve [apply (IHp p0) | apply (IHp 1%positive)|apply (IHp (p0+1%positive)%positive)].
Qed.

(** Return the value of the [i]-th bit of a positive. *)
Fixpoint P_decomp (x: positive) (i: nat) { struct x } : bool :=
  match i, x with
  | O, xH => true
  | O, xI _ => true
  | O, xO _ => false
  | S m, xH => false
  | S m, xI p => P_decomp p m
  | S m, xO p => P_decomp p m									   
  end.

(** Returns the positive of bits [[f i,...,f (i+n-1),1]].
    Remark the [n]-th bit is always 1 ([xH]). *)
Fixpoint P_recomp (n: nat) (f : Zfc) (i: nat) {struct n } :=
  match n with
  | O => xH
  | S m => if (f i) then xI (P_recomp m f (S i)) else xO (P_recomp m f (S i))
  end.

(** ** Properties of decomposition *)

(** After the highest bits, all bits are false. *)
Remark P_decomp_limit: forall x k, 
  k > xHpos x -> P_decomp x k = false.
Proof.
 induction x; simpl; intros; destruct k.  
 inversion H. apply IHx. auto with arith. 
 inversion H. apply IHx. auto with arith. 
 inversion H. auto with arith.
Qed.

(** The highest bit is true. *)
Remark P_decomp_xHpos: forall x,
  P_decomp x (xHpos x) = true.
Proof.
  induction x; simpl; intros; auto.
Qed.

(** The [P_shift] of [nat -> A] functions. *)
Definition P_shift {A:Type} f i k : A := f (i + k).

(** bits of a positive with one more 1-bit. *)
Remark P_decomp_shift1: forall p: positive,
  P_shift (P_decomp p~1) 1 = P_decomp p.
Proof.
  intro p. extensionality k. unfold P_shift. auto.
Qed.

(** bits of a positive with one more 0-bit. *)
Remark P_decomp_shift0: forall p: positive,
  P_shift (P_decomp p~0) 1 = P_decomp p.
Proof.
  intro p. extensionality k. unfold P_shift. auto.
Qed.

(** ** Properties of recomposition *)

(** Recomposition of shifted bits. *)
Remark P_recomp_shift: forall (f: Zfc) (n i j: nat), 
  P_recomp n f (i+j) = P_recomp n (P_shift f i) j.
Proof.
  intros f n.
  induction n; intros i j; simpl; auto.
  unfold P_shift at 1.
  case_eqb BIT (f (i+j)); f_equal; 
    (replace (S(i+j)) with (i + S j) by omega);
    apply IHn.
Qed.

(** Highest bits of recomposition. *)
Remark xHpos_P_recomp: forall (n: nat) (f: Zfc) (i: nat),
   xHpos (P_recomp n f i) = n.
Proof.
  intros n f.
  induction n. simpl. auto.
  intros. simpl. destruct (f i); simpl; f_equal; apply IHn.
Qed.

(** ** Involution of decomposition and recomposition *)

(** Invariance by 1-bit shift. *)
Remark NEXT_I: forall (n: nat) (p: positive),
  P_recomp n (P_decomp p~1) 1 = P_recomp n (P_decomp p) 0.
Proof.
  intros.
  replace 1 with (1+0) by omega.
  rewrite P_recomp_shift.
  rewrite P_decomp_shift1.
  auto.
Qed.

(** Invariance by 0-bit shift. *)
Remark NEXT_O: forall (n: nat) (p: positive),
  P_recomp n (P_decomp p~0) 1 = P_recomp n (P_decomp p) 0.
Proof.
  intros.
  replace 1 with (1+0) by omega.
  rewrite P_recomp_shift.
  rewrite P_decomp_shift0.
  auto.
Qed.

(** Recomposition of Decomposition. *)
Lemma P_recomp_decomp: forall (n: nat) (p: positive), 
  n = xHpos p -> P_recomp n (P_decomp p) O = p.
Proof. 
  induction n;intros;simpl.
  destruct p; inversion H; auto.
  destruct p; unfold P_decomp at 1; f_equal.
  rewrite NEXT_I. apply IHn. inversion H; auto with arith.
  rewrite NEXT_O. apply IHn. inversion H; auto with arith.
  inversion H.
Qed.

(** Decomposition of Recomposition. 
    The induction scheeme of the proof requires to recompose
    an arbitrary shifted function. *)
Lemma P_decomp_recomp: forall (f: Zfc) (n i k: nat), 
  k < n -> P_decomp (P_recomp n f i) k = f (i+k).
Proof.
  intros f n.
  induction n. intros. apply False_ind. omega.
  intros i k Limit.
  simpl. destruct k.
  case_eqb Fi (f i); simpl; rewrite <- Fi; f_equal; omega.
  destruct (f i); simpl.
  rewrite IHn. f_equal. omega. omega.
  rewrite IHn. f_equal. omega. omega.
Qed.

(** Last bits of positive. *)
Remark last_P_decomp: forall (p: positive) (m: nat), 
  m = xHpos p -> last (P_decomp p) (S m) false = (S m).
Proof.
  intros p m Hm.
  unfold last; rewrite Hm; rewrite P_decomp_xHpos; simpl; auto.
Qed.

(** {@natural:} *)
(** * Bits of natural integers *)
(** The section naturally extends bits of [positive] to [N].
    Zero is represented by the infinite sequence of 0-bits. *)

(** Conversion from [Z] to [N]. *)
Definition Nabs (x:Z): N :=
  match x with
  | Z0 => N0						
  | Zpos p => Npos p
  | Zneg p => Npos p						
  end.			
		
(** Number of significative bits (last 1-bit) of a natural. *)
Definition NxHpos (n:N): nat := 
  match n with
  | N0 => O
  | Npos p => S (xHpos p)						
  end.

(** NxHpos increases. *)
Remark NxHpos_incr: forall x a: N, NxHpos x <= NxHpos (x + a).
Proof.
  destruct x; destruct a; simpl; try (by compute).
  cut (xHpos p <= xHpos (p + p0)). omega.
  apply xHpos_incr.
Qed.

(** Arithmetic properties of [NxHpos] *)
  
Remark NxHpos_2x_p0: forall n:N,
  (0 < n)%N -> NxHpos (2 * n) = S (NxHpos n).
Proof.
  destruct n; by simpl.  
Qed.					      
					      
Remark NxHpos_2x_p1: forall n:N,
  NxHpos (2 * n + 1) = S (NxHpos n).
Proof.
  destruct n ; by simpl.  
Qed.					      
					      
Remark NxHpos_div2_p: forall n:N,
  (0 < n)%N -> NxHpos (Ndiv2 n) = pred (NxHpos n).
Proof.
  destruct n.
  (** zero *)
  + by compute.
  (** positive *)
  + by destruct p.
Qed.					      
		
(** Bits of a natural integer *)
Definition N_decomp (x: N): Zfc :=
  match x with 
  | N0 => (fun _ => false)
  | Npos p => P_decomp p						
  end.							

(** Recomposition of an integer from a range of [n]-bits *)
Definition N_recomp (n: nat) (f: Zfc): Z :=
  match last f n false with
  | O   => Z0
  | S m => Zpos (P_recomp m f 0) 						
  end.							

(** Recomposition result is a positive integer. *)
Remark N_recomp_pos: forall (n: nat) (f: Zfc),
  (0 <= N_recomp n f)%Z.
Proof. 
  intros. unfold N_recomp. 
  destruct (last f n false); auto with zarith.
Qed.

(** Zero has a unique representation *)
Remark N_recomp_zero: forall (n: nat) (f: Zfc), 
  trailing f n false -> (N_recomp n f = 0)%Z -> forall k, f k = false.
Proof.
  intros n f Trail.
  unfold N_recomp. 
  destruct (last_null_or_flip f n false) as [ZERO | FLIP].
  rewrite ZERO. intros. 
  generalize (trailing_last f n false).
  intro TLAST. rewrite ZERO in TLAST.
  apply TLAST; auto with arith.
  destruct FLIP as [k [L F]].
  rewrite L. discriminate.
Qed.

(** One has a unique representation *)
Remark N_recomp_one: forall (n: nat) (f: Zfc),
  trailing f n false -> (N_recomp n f = 1)%Z ->
    f O = true /\ forall k, f (S k) = false.
Proof.
  intros n f Trail.
  unfold N_recomp.
  destruct (last_null_or_flip f n false) as [ZERO | FLIP].
  rewrite ZERO. intros. apply False_ind. omega.
  destruct (last f n false) eqn:LAST. 
  intros. apply False_ind. omega.
  intro ONE.
  assert (XH: P_recomp n0 f 0 = xH). inversion ONE; trivial.
  destruct FLIP as [ K1 [ SKN B1not ] ].
  assert (NK : n0 = K1) by ( auto with arith ). rewrite NK in *.
  assert (B1 : f K1 = true) by ( destruct (f K1); auto ).
  assert (T1 : trailing f (S K1) false).
  { rewrite <- LAST. apply trailing_last; auto. }
  destruct K1. 
  + rewrite B1 in *.
    split; auto. intro k; destruct k; apply T1; auto with arith.
  + simpl in XH.
    destruct (f 0).
    * apply False_ind. discriminate.
    * apply False_ind. discriminate.
Qed.

(** Involution of Decomposition and Recomposition *)
Lemma N_decomp_recomp: forall (n: nat) (f: Zfc),
  trailing f n false -> N_decomp (Nabs (N_recomp n f)) = f.
Proof.
  intros n f Trail.
  unfold N_recomp.
  generalize (last_null_or_flip f n false).
  intros [ ZERO | FLIP ].
  (** ZERO *)
  + rewrite ZERO. simpl. extensionality k. symmetry.
    cut (trailing f 0 false). intro H. apply H. omega.
    rewrite <- ZERO.
    apply trailing_last. auto.
  (** FLIP *)
  + destruct FLIP as [k [Last Flip]].
    rewrite Last. simpl.
    extensionality i.
    nat_compare Inf EQ Sup i k.
    (** Inf *) 
    * apply P_decomp_recomp. auto.
    (** Eq *) 
    * generalize (xHpos_P_recomp i f 0).
      pose (x := P_recomp i f 0).
      fold x.
      intro xHi.
      rewrite <- xHi.
      rewrite P_decomp_xHpos.
      rewrite xHi. rewrite EQ.
      case_eqb FK (f k); auto; contradiction.
    (** Sup *)
    * generalize (xHpos_P_recomp k f 0).
      pose (x := P_recomp k f 0).
      fold x.
      intro xHk.
      rewrite (P_decomp_limit x i); [|rewrite xHk;auto].
      cut (trailing f (S k) false). intro H. symmetry. apply H. omega.
      rewrite <- Last. apply trailing_last. auto.
Qed.

(** [NxHpos] of a recomposition *)
Lemma NxHpos_N_recomp_pos: forall (n: nat) (f: Zfc), 
  NxHpos (Nabs (N_recomp n f)) = last f n false.
Proof.
  intros.
  unfold N_recomp.
  elim (last_null_or_flip f n false).
  intro ZERO. rewrite ZERO. auto.
  intros [ k [ LAST FLIP ] ].
  rewrite LAST. simpl. rewrite xHpos_P_recomp. trivial.
Qed.

(** {@integer:} *)
(** * Bits of Integers *)

Local Open Scope Z_scope.

(** The bits representation of an integer consists of a bit function,
    packed with its trailing property. 
    
    This representation is _not_ unique. However, 
    the unicity of last significant bits implies an extensionality 
    equality: if two [bits] records have the same bit function, they represent
    the same integer, see [Lemma btest_ext].
*)

Record bits: Type := mkbits 
  { bsize:nat; 
    bsign: bool; 
    btest: Zfc; 
    btrail : trailing btest bsize bsign }.

(** ** Two's complement and bits inversion *)
(** As specified in the introduction, the extension positive integers [N] to [Z] 
    is realized by two's complement and bit inversion. *)

(** Two's complement and related properties. *)

Definition zlnot x:Z := -(x + 1).

Remark zlnot_inv: forall x,
  zlnot (zlnot x) = x.
Proof.
  intros. unfold zlnot. auto with zarith. 
Qed.
Remark zlnot_inj: forall x y : Z,
  (zlnot x) = (zlnot y) -> x = y.
Proof.
  unfold zlnot. intros. omega. 
Qed.
Remark zlnot_sym: forall x y : Z,
  (zlnot x) = y -> x = (zlnot y).
Proof.
  unfold zlnot. intros. omega. 
Qed.

Lemma P_zlnot_sym: forall P (b: Z),
  0 <= b -> ((forall z: Z, -b <= z -> P z) -> (forall z:Z, z < b -> P (zlnot z))).
Proof.
  intros P b Bge0 Hyp z H. assert (-b <= zlnot z). unfold zlnot. omega.
  apply Hyp. auto.
Qed.

Lemma P_zlnot_sym_rev: forall P (b: Z),
  0 <= b -> ((forall z:Z, z < b -> P z) -> (forall z:Z, -b <= z -> P (zlnot z))).
Proof.
  intros P b Bge0 Hyp z H. assert (zlnot z < b). unfold zlnot. omega.
  apply Hyp. auto.
Qed.

(** Bit inversion and related properties. *)

Definition fnot (f: Zfc): Zfc := (fun k => negb (f k)).

Remark fnot_inv: forall f: Zfc,
  fnot (fnot f) = f.
Proof. intros. extensionality k. unfold fnot. destruct (f k); auto.
Qed.
Remark fnot_inj: forall f g, fnot f = fnot g -> f = g.
Proof. intros.
  generalize (fnot_inv f); intro E; rewrite <- E; clear E.
  generalize (fnot_inv g); intro E; rewrite <- E; clear E.
  rewrite H. auto.
Qed.
Remark fnot_sym: forall f g: Zfc,
  (fnot f) = g -> f = (fnot g).
Proof. intros.
  apply (fnot_inj f). rewrite (fnot_inv). auto.
Qed.

(** Lifting of [fnot] to [trailing] *)
Remark trailing_fnot: forall (f: Zfc) (n: nat) (b: bool), 
  trailing (fnot f) n (negb b) -> trailing f n b.
Proof.
  intros. unfold trailing. intros k Hk.
  generalize (H k Hk).
  intro E.
  rewrite <- (fnot_inv f).
  unfold fnot. unfold fnot in E. rewrite E.
  rewrite Bool.negb_involutive. trivial.
Qed.

(** Lifting of [fnot] to [last] *)
Remark last_fnot: forall (f: Zfc) (n: nat) (b: bool), 
  last (fnot f) n (negb b) = last f n b.
Proof.
  intros. induction n.
  simpl. trivial.
  simpl. case_eqb H (Bool.eqb (f n) b). 
  (** TRUE *)
  + unfold fnot.
    destruct (f n); destruct b; simpl in *; (discriminate || apply IHn).
  (** FALSE *)
  + unfold fnot.
    destruct (f n); destruct b; simpl in *; ( discriminate || auto).
Qed.

(** ** Decomposition and Recomposition of integers *)

(** Trailing bits of positive integers *)
Remark Zpos_decomp_trail: forall n: N,
  trailing (N_decomp n) (NxHpos n) false.
Proof.
  intro n.
  induction n.
  unfold trailing. auto.
  unfold trailing.
  simpl.
  intro k. apply P_decomp_limit.
Qed.

(** Trailing bits of positive integers *)
Remark Zneg_decomp_trail: forall n: N,
  trailing (fnot (N_decomp n)) (NxHpos n) true.
Proof.
  intro n.
  unfold trailing.
  intros. unfold fnot.
  by (rewrite Zpos_decomp_trail).
Qed.  

(** Bits decomposition of [Z] integers *)
Program Definition bits_of_Z (x:Z): bits :=		       
  if (Zle_bool 0 x) 
  then let n := Nabs x in
       mkbits (NxHpos n) false (N_decomp n) (Zpos_decomp_trail n)
  else let n := Nabs (zlnot x) in
       mkbits (NxHpos n) true (fnot (N_decomp n)) (Zneg_decomp_trail n).

(** Recomposition of an integers from its bits *)
Definition Z_of_bits (b: bits): Z :=
  if bsign b 
  then zlnot (N_recomp (bsize b) (fnot (btest b)))
  else N_recomp (bsize b) (btest b).

(** ** Extensional unicity of bits representation *)

(** Same [Zfc] implies equality of signs *)
Remark btest_sign: forall x y: bits,
  btest x = btest y -> bsign x = bsign y.
Proof.
  destruct x. destruct y. 
  simpl in * .  
  pose (k := max bsize0 bsize1).
  generalize (btrail0 k). intro H0. 
  generalize (btrail1 k). intro H1.
  intro BEQ.
  rewrite <- H0; unfold k; auto with arith.
  rewrite <- H1; unfold k; auto with arith.
  rewrite BEQ.
  auto.
Qed.

(** Opposite [Zfc] implies opposite signs *)
Remark btest_sign_sym: forall x y: bits,
  btest x = fnot (btest y) -> bsign x = negb (bsign y).
Proof.
  destruct x. destruct y. 
  simpl in * .  
  pose (k := max bsize0 bsize1).
  generalize (btrail0 k). intro H0. 
  generalize (btrail1 k). intro H1.
  intro BEQ.
  rewrite <- H0; unfold k; auto with arith.
  rewrite <- H1; unfold k; auto with arith.
  rewrite BEQ.
  auto.
Qed.

(** Same [Zfc] leads to equal represented integers *)
Lemma btest_ext: forall x y: bits, 
  btest x = btest y -> Z_of_bits x = Z_of_bits y.
Proof.
  intros x y BEQ. 
  assert (bsign x = bsign y) as SEQ. apply btest_sign. auto.
  unfold Z_of_bits. rewrite <- BEQ. rewrite <- SEQ. 
  case_eqb SIGNX (bsign x); [ f_equal | ]; 
    unfold N_recomp; 
    rewrite <- (last_trail_ext _ _ (bsize x) (bsize y)); auto.
  (** x<0 , trailing ~x |x| false *)
  + generalize (btrail x). rewrite SIGNX.
    unfold trailing. intros T k R. unfold fnot. rewrite T; auto with arith.
  (** x<0 , trailing ~x |y| false *)
  + rewrite BEQ.
    generalize (btrail y). rewrite SIGNX in SEQ. rewrite <- SEQ.
    unfold trailing. intros T k R. unfold fnot. rewrite T; auto with arith.
  (** x>0 , trailing x |x| false *)
  + generalize (btrail x). rewrite SIGNX. auto.
  (** x>0 , trailing x |y| false *)
  + generalize (btrail y). rewrite SIGNX in SEQ. rewrite <- SEQ. rewrite <- BEQ. auto.
Qed.

(** Opposite [Zfc] leads to two's complement represented integers *)
Lemma btest_ext_sym: forall x y: bits,
  btest x = fnot (btest y) -> Z_of_bits x = zlnot (Z_of_bits y).
Proof.
  intros x y BEQ1. 
  assert (btest y = fnot (btest x)) as BEQ2 by (apply fnot_sym; symmetry; auto).
  assert (bsign x = negb (bsign y)) as SEQ1 by (by apply btest_sign_sym).
  assert (bsign y = negb (bsign x)) as SEQ2 by (by apply btest_sign_sym).
  unfold Z_of_bits. 
  rewrite <- BEQ1. rewrite SEQ2. rewrite <- BEQ2. 
  case_eqb SIGNX (bsign x); 
    (try replace (negb true) with false by (compute ; forward)); 
    (try replace (negb false) with true by (compute ; forward)); 
    (try rewrite zlnot_inv); 
    [ f_equal | ]; 
    unfold N_recomp; 
    rewrite <- (last_trail_ext _ _ (bsize x) (bsize y)); auto.
  (** x<0 , trailing ~x |x| false *)
  + rewrite BEQ2.
    generalize (btrail x). rewrite SIGNX.
    unfold trailing. intros T k R. unfold fnot. rewrite T; auto with arith.
  (** x<0 , trailing ~x |y| false *)
  + rewrite BEQ2.
    generalize (btrail y). rewrite <- BEQ2. rewrite SIGNX in SEQ2. rewrite SEQ2.
    replace (negb true) with false by auto.
    auto.
  (** x>0 , trailing x |x| false *)
  + generalize (btrail x). rewrite SIGNX. auto.
  (** x>0 , trailing x |y| false *)
  + generalize (btrail y). rewrite SIGNX in SEQ2. rewrite SEQ2. 
    replace (negb false) with true by auto. rewrite BEQ1.
    unfold trailing. intros T k R. unfold fnot. rewrite T; auto with arith.
Qed.

(** ** Involution of Decomposition and Recomposition *)
(** These two fundamental lemmas allow reasoning conversely with bits or integers. *)

(** [Z_of_bits] is the inverse of [bits_of_Z] *)
Lemma Z_recomp_decomp: forall x: Z,
  Z_of_bits (bits_of_Z x) = x.
Proof.
  intro x.
  unfold bits_of_Z.
  induction x; simpl.
  (** x = 0 *)
  + unfold Z_of_bits. simpl. unfold N_recomp. simpl. trivial.
  (** x = Zpos p *)
  + unfold Z_of_bits. simpl. unfold N_recomp.
    rewrite last_P_decomp; auto.
    rewrite P_recomp_decomp; auto.
  (** x = Zneg p *)
  + unfold Z_of_bits. simpl.
    rewrite fnot_inv. 
    pose ( z := zlnot (Zneg p) ).
    fold z.
    generalize (zlnot_inv (Zneg p)). intro H. rewrite <- H.
    f_equal. fold z.
    assert (ZDEF: z = Zpos p - 1).
    (** ZDEF *)
    * unfold z. unfold zlnot. 
      pose (u := Zneg p). fold u.
      pose (v := Zpos p). fold v.
      replace u with (-v) by (unfold u; unfold v; simpl; trivial).
      omega.
    (** cont. *)
    * assert  (Q : z = 0 \/ exists q, z = Zpos q).
      { destruct p. 
        - simpl in ZDEF. right. exists (p~1%positive - 1)%positive. trivial.
        - simpl in ZDEF. right. exists (p~0%positive - 1)%positive. trivial.
        - simpl in ZDEF. left. trivial. }
      elim Q.
      - intro Z; rewrite Z; simpl.
        unfold N_recomp. simpl. trivial.
      - intros [q Z]. rewrite Z; simpl.
      unfold N_recomp.
      rewrite last_P_decomp; auto.
      rewrite P_recomp_decomp; auto.
Qed.
	
(** [bits_of_Z] is the inverse of [Z_of_bits] modulo [btest] *)
Lemma Z_decomp_recomp: forall b: bits,
  btest (bits_of_Z (Z_of_bits b)) = btest b.
Proof.
  intros.
  unfold Z_of_bits.
  destruct (bsign b) eqn:BSIGN ; unfold bits_of_Z.
  (** NEGATIVE SIGN *)
  + pose ( f := fnot (btest b) ). fold f.
    assert ( Fnot : btest b = fnot f). unfold f. rewrite fnot_inv. auto.
    pose ( x := N_recomp (bsize b) f ). fold x.
    assert ( Xpos : 0 <= x ) by ( apply N_recomp_pos; auto with zarith ).
    repeat rewrite zlnot_inv.
    case_leq 0 (zlnot x); intro SIGN; simpl.
    (** 0 <= zlnot x -> contradiction *) 
    * unfold zlnot in SIGN.
      apply False_ind. omega.
    (** 0 > zlnot x *)
    * apply fnot_inj. rewrite fnot_inv. fold f. unfold x.
      apply N_decomp_recomp. 
      apply trailing_fnot. 
      simpl. rewrite <- BSIGN. rewrite <- Fnot.
      apply (btrail b).
  (** POSITIVE SIGN *)
  + pose ( f := btest b ). fold f.
    pose ( x := N_recomp (bsize b) f ). fold x.
    assert ( Xpos : 0 <= x ) by ( apply N_recomp_pos; auto with zarith ).
    case_leq 0 x; intro H; try (apply False_ind; omega; fail).
    simpl. unfold f. unfold x. 
    apply N_decomp_recomp. 
    rewrite <- BSIGN. apply (btrail b).
Qed.

(** Two's complement symmetry. *)
Lemma Z_decomp_recomp_sym: forall b: bits,
  btest (bits_of_Z (zlnot (Z_of_bits b))) = fnot (btest b).
Proof.
  intros. unfold Z_of_bits.
  destruct (bsign b) eqn:BSIGN; unfold bits_of_Z;   
    (try rewrite zlnot_inv). 
  (** POSITIVE SIGN *)
  + pose ( f := fnot (btest b)). fold f.
    pose ( x := N_recomp (bsize b) f ). fold x.
    assert ( Xpos : 0 <= x ) by ( apply N_recomp_pos; auto with zarith ).
    case_leq 0 x; intro H; try (apply False_ind; omega; fail).
    simpl. unfold f. unfold x. 
    apply N_decomp_recomp. 
    apply trailing_fnot. 
    replace (negb false) with true by auto.
    rewrite fnot_inv.
    rewrite <- BSIGN.
    apply (btrail b).
  (** NEGATIVE SIGN *)
  + pose ( f := fnot (btest b) ). fold f.
    assert ( Fnot : btest b = fnot f). unfold f. rewrite fnot_inv. auto.
    pose ( x := N_recomp (bsize b) (btest b) ). fold x.
    assert ( Xpos : 0 <= x) by ( apply N_recomp_pos; auto with zarith ).
    case_leq 0 (zlnot x); intro SIGN; simpl.
    (** 0 <= zlnot x -> contradiction *) 
    * unfold zlnot in SIGN.
      apply False_ind. omega.
    (** 0 > zlnot x *)
    * unfold f. f_equal.
      apply N_decomp_recomp. 
      rewrite <- BSIGN.
      apply (btrail b).
Qed.

(** [Zfc] can be used to discriminate. *)
Lemma btest_discrimination: forall x y: bits,
  btest x <> btest y -> Z_of_bits x <> Z_of_bits y.
Proof.
  intros x y.
  intro BNEQ; apply contrap with (Q := btest x = btest y); auto; clear BNEQ.
  intro.
  rewrite <- (Z_decomp_recomp x); rewrite <- (Z_decomp_recomp y). 
  f_equal; f_equal; auto.
Qed.

(** Sign can be used to discriminate. *)
Lemma sign_discrimination: forall x y: bits,
  bsign x <> bsign y -> Z_of_bits x <> Z_of_bits y.
Proof.
  intros x y SNEQ. apply btest_discrimination.
  apply contrap with (Q := bsign x = bsign y); auto.
  apply btest_sign.
Qed.

(** Sign encoding *)
Lemma Zsign_encoding: forall z:Z,
  bsign (bits_of_Z z) = negb (Zle_bool 0 z).
Proof.
  intro z. unfold bits_of_Z. unfold bsign.
  case_leq 0 z; auto.
Qed.
 
Lemma bsign_encoding: forall b:bits,
  bsign b = negb (Zle_bool 0 (Z_of_bits b)).
Proof.
  intro b.
  rewrite <- Zsign_encoding.
  unfold Z_of_bits.
  destruct (bsign b) eqn:BSIGN ; unfold bits_of_Z.
  (** NEGATIVE SIGN *)
  + pose ( f := fnot (btest b) ). fold f.
    assert ( Fnot : btest b = fnot f). unfold f. rewrite fnot_inv. auto.
    pose ( x := N_recomp (bsize b) f ). fold x.
    assert ( Xpos : 0 <= x ) by ( apply N_recomp_pos; auto with zarith ).
    repeat rewrite zlnot_inv.
    case_leq 0 (zlnot x); intro SIGN; simpl.
    (** 0 <= zlnot x -> contradiction *) 
    * unfold zlnot in SIGN.
      apply False_ind. omega.
    (** 0 > zlnot x *)
    * auto.
  (** POSITIVE SIGN *)
  + pose ( f := btest b ). fold f.
    pose ( x := N_recomp (bsize b) f ). fold x.
    assert ( Xpos : 0 <= x ) by ( apply N_recomp_pos; auto with zarith ).
    case_leq 0 x; intro H; try (apply False_ind; omega; fail).
    simpl. auto.
Qed.
 
(** {@Zbit:} *)	  
(** * Characteristic Function of integers *)

(** Extracts the [k]-th bit of [x]. *)
Definition Zbit (x : Z): Zfc := btest (bits_of_Z x).  

Definition TRUE  : Zfc := fun _ => true.
Definition FALSE : Zfc := fun _ => false.

Lemma Zbit_of_zero: Zbit 0 = FALSE.
Proof.
  unfold Zbit. unfold bits_of_Z. simpl. auto.
Qed.
Lemma Zbit_of_mone: Zbit (-1) = TRUE. 
Proof.
  unfold Zbit. unfold bits_of_Z. simpl. auto.
Qed.

(** The expected characteristic of binary decomposition of an integer *)
Theorem Zbit_power: forall n k:nat,
  Zbit (two_power_nat n) k = beq_nat n k.
Proof.
  unfold two_power_nat. unfold Zbit. unfold bits_of_Z.
  simpl.
  induction n; intro k.
  (** base *)
  + simpl; auto.
  (** ind. *)
  + unfold shift_nat. destruct k; simpl; auto.
Qed.

(** The extensional unicity of [Zbit] for each integer *)
Theorem Zbit_ext : 
  forall x y: Z, Zbit x = Zbit y -> x = y.
Proof.
  unfold Zbit.
  intros. 
  rewrite <- (Z_recomp_decomp x). 
  rewrite <- (Z_recomp_decomp y). 
  apply btest_ext. auto.
Qed.

(** Two's complement symmetry *)
Theorem Zbit_ext_sym : 
  forall x y: Z, Zbit x = fnot (Zbit y) -> x = zlnot y.
Proof.
  unfold Zbit.
  intros. 
  rewrite <- (Z_recomp_decomp x). 
  rewrite <- (Z_recomp_decomp y). 
  apply btest_ext_sym. auto.
Qed.
			  
(** * Position of the Highest Significant Bit in two's complement representation *)
Definition ZxHpos (z:Z): nat := 
  if (Zle_bool 0 z) then NxHpos (Nabs z) else NxHpos (Nabs (zlnot z)).

(** Zero has no significant bit, as minus one *)
Remark ZxHpos_is_zero: ZxHpos 0 = O /\ ZxHpos (-1) = O.
Proof.
  split; by compute. 
Qed.

(** [bsize] of a [bits_of_Z] gives the exact position. 
    The use of the [last] function gives the exact position. *)
Lemma bsize_exact: forall z:Z,
  ZxHpos (z) = bsize (bits_of_Z z).
Proof.
  intro. unfold bits_of_Z. unfold ZxHpos.
  case_leq 0 z; unfold bsize; auto.
Qed.
 
(** [bsize] over approximates the exact position. 
    The use of the [last] function gives the exact position. *)
Lemma bsize_over_approx: forall b:bits,
  ZxHpos (Z_of_bits b) = last (btest b) (bsize b) (bsign b).
Proof.
  intros. unfold ZxHpos.
  unfold Z_of_bits.
  destruct (bsign b) eqn:BSIGN.
  (** Negative *)
  + pose ( f := fnot (btest b)). fold f.
    pose ( x := N_recomp (bsize b) f ). fold x.
    assert ( Xpos : 0 <= x ) by (apply N_recomp_pos; auto with zarith ).
    case_leq 0 (zlnot x); 
      intro H; 
      try (unfold zlnot in H; apply False_ind; omega; fail).
    rewrite zlnot_inv.
    unfold x. unfold f. 
    rewrite NxHpos_N_recomp_pos. 
    rewrite <- last_fnot.
    rewrite fnot_inv.
    by simpl.
  (** Positive *)
  + case_leq 0 (N_recomp (bsize b) (btest b)); intro N_recomp.
    rewrite NxHpos_N_recomp_pos. trivial.
    generalize  (N_recomp_pos(bsize b) (btest b)).
    intros.  apply False_ind. omega.
Qed.

(** Two's complement symmetry *)
Remark ZxHpos_sym: forall z: Z,
  ZxHpos (zlnot z) = ZxHpos z.
Proof.
  intro.
  unfold ZxHpos; try rewrite zlnot_inv; unfold zlnot. 
  case_leq 0 z; case_leq 0 (-(z+1)). 
Qed.

(** Position of the highest significant bit of [two_power_nat]. *)
Remark ZxHpos_of_two_power_nat: forall n: nat,
  (ZxHpos (two_power_nat n) = S n)%nat.
Proof.
  intro. unfold ZxHpos.
  case_leq 0 (two_power_nat n); intro.
  (** 0 <=two_power_nat n *)
  + induction n.
    (** Base *)
    * by simpl.
    (** cont. *)
    * rewrite two_power_nat_S.
      rewrite two_power_nat_S in H.
      replace (Nabs (2 * two_power_nat n)) with (2 * Nabs (two_power_nat n))%N
        by by compute.
      rewrite NxHpos_2x_p0.
        by rewrite IHn.
      by compute.
  (** 0 > two_power_nat n *)
  + generalize (two_power_nat_is_positive n). omega.
Qed.
 
(** Position of the highest significant bit of the predecessor of [two_power_nat]. *)
Remark ZxHpos_of_two_power_nat_minus_one: forall n: nat,
  (ZxHpos ((two_power_nat n) - 1) = n)%nat.
Proof.
  intro. unfold ZxHpos.
  case_leq 0 ((two_power_nat n) -1); intro.
  (** 0 <=(two_power_nat n) - 1 *)
  + induction n.
    (** Base *)
    * by simpl.
    (** cont. *)
    * rewrite two_power_nat_S.
      rewrite two_power_nat_S in H.
      assert ((Nabs (2 * two_power_nat n - 1) = 2 * Nabs (two_power_nat n - 1) +1)%N) as EQ.
      { generalize (two_power_nat_is_positive n); intro.
        assert (0 <= (two_power_nat n - 1)) as A0 by omega.
        assert (0 < 2 * (two_power_nat n - 1) + 1) as A1 by omega.
        replace (2 * two_power_nat n - 1) with (2 * (two_power_nat n - 1) + 1) by omega.
        destruct (two_power_nat n - 1); by auto. }
      rewrite EQ.
      rewrite NxHpos_2x_p1.
      by rewrite IHn.
  (** 0 > two_power_nat n *)
  + generalize (two_power_nat_is_positive n). omega.
Qed.
 
(** [ZxHpos] increases for positive input values *)  
Remark ZxHpos_incr_for_positive: forall x a: Z,
  0 <= x -> 0 <= a -> (ZxHpos x <= ZxHpos (x + a))%nat.
Proof.
  intros.
  unfold ZxHpos.
  case_leq 0 x. case_leq 0 (x + a). intros.
  destruct x; destruct a; try (by compute).
  replace (Nabs (Zpos p + Zpos p0)) with (Npos p + Npos p0)%N by forward.
  apply NxHpos_incr.
Qed.

(** [ZxHpos] decreases for negative input values *)  
Remark ZxHpos_decr_for_negative: forall x a: Z,
  x <= 0 -> a <= 0 -> (ZxHpos x <= ZxHpos (x + a))%nat.
Proof.
  intros.
  unfold ZxHpos. unfold zlnot.
  case_leq 0 x; case_leq 0 (x + a); 
    intros; try (replace x with 0 by omega); try (by compute).
  case_leq (-1) x; 
    intros; try (replace x with (-1) by omega); try (by compute).
  assert (- (x + 1) >= 0) as X by omega.
  assert (- a >= 0) as A by omega.
  clear H; clear H0; clear H1; clear H2; clear H3. 
  replace (-(x+a+1)) with (-(x+1) + -a) by omega.
  pose (b := -a); fold b; fold b in A.
  pose (y := -(x+1)); fold y; fold y in X.
  destruct y; destruct b; try (by compute).
  replace (Nabs (Zpos p + Zpos p0)) with (Npos p + Npos p0)%N by forward.
  apply NxHpos_incr.
Qed.

(** [two_power_nat_of (ZxHpos z)] gives an upper bound for [z] *)
Remark two_power_nat_of_ZxHpos: forall z: Z,
  z < two_power_nat (ZxHpos z).
Proof.
  destruct z.
  (** zero *) 
  + by compute.
  (** positive *) 
  + unfold ZxHpos. 
    replace (Nabs (Zpos p)) with (Npos p) by forward.
    replace (Zpos p) with (Z_of_N (Npos p)) by forward.
    induction p.
    (** 2p+1 *)
    * simpl. simpl in IHp.
      replace (Zpos p~1) with (2*(Zpos p) + 1)%Z by (auto with zarith).
      rewrite two_power_nat_S.
      omega.
    (** 2p *)
    * simpl. simpl in IHp.
      replace (Zpos p~0) with (2*Zpos p)%Z by (auto with zarith).
      rewrite two_power_nat_S.
      omega.
    (** one *)
    * by compute.
  (** negative *)
  + assert (Zneg p  < 0)%Z by (by simpl).
    generalize (two_power_nat_is_positive (ZxHpos (Zneg p))).
    omega.
Qed.

(** Lower upper [two_power_nat] bound of an integer. *)
Definition ZxHbound (z: Z): Z := two_power_nat (ZxHpos z).

Remark ZxHbound_of_two_power_nat_minus_one: forall n: nat,
  ZxHbound ((two_power_nat n) - 1) = two_power_nat n.
Proof.
  intro. unfold ZxHbound. 
  rewrite ZxHpos_of_two_power_nat_minus_one. auto.
Qed.

(** [ZxHbound] gives an upper and lower bound. *)
Lemma ZxHrange: forall z: Z, 
  let bound := ZxHbound z in -bound <= z < bound.
Proof.
  intro. unfold ZxHbound.
  case_leq 0 z; intro.
  (** 0 <= z *)
  + generalize (two_power_nat_of_ZxHpos z). split; omega.
  (** 0 > z *)
  + generalize (two_power_nat_of_ZxHpos (-(z+1))).
    rewrite <- (ZxHpos_sym z).
    unfold zlnot.
    split; omega.
Qed.

Remark ZxHpos_le: forall x y: Z,
  ZxHbound x <= ZxHbound y -> (ZxHpos x <= ZxHpos y)%nat.
Proof.
  unfold ZxHbound. 
  intros x y.
  pose (X := ZxHpos x). fold X.
  pose (Y := ZxHpos y). fold Y.
  generalize X Y.
  induction X0; intro.
  (** base *)
  + generalize (two_power_nat_is_positive Y0).
    replace (two_power_nat 0) with 1 by (compute ; forward).
    omega.
  (** cont. *)
  + rewrite two_power_nat_S. 
    induction Y0.
    (** base *)
    * generalize (two_power_nat_is_positive X0).
      replace (two_power_nat 0) with 1 by (compute ; forward).
      omega.
    (** cont. *)
    * rewrite two_power_nat_S. 
      cut ((2 * two_power_nat X0) <= (2 * two_power_nat Y0) -> (S X0 <= S Y0)%nat). omega.
      generalize (IHX0 Y0).
      omega.
Qed.

Remark ZxHbound_le: forall x y: Z,
  (ZxHpos x <= ZxHpos y)%nat -> ZxHbound x <= ZxHbound y.
Proof.
  unfold ZxHbound. 
  intros x y.
  pose (X := ZxHpos x). fold X.
  pose (Y := ZxHpos y). fold Y.
  repeat rewrite two_power_nat_S.
  generalize X Y.
  induction X0; intro.
  (** base *)
  + generalize (two_power_nat_is_positive Y0).
    replace (two_power_nat 0) with 1 by (compute ; forward).
    omega.
  (** cont. *)
  + rewrite two_power_nat_S. 
    induction Y0.
    (** base *)
    * generalize (two_power_nat_is_positive X0).
      replace (two_power_nat 0) with 1 by (compute ; forward).
      omega.
    (** cont. *)
    * intro.
      rewrite two_power_nat_S. 
      cut ((2 * two_power_nat X0) <= (2 * two_power_nat Y0)). omega.
      apply (IHX0 Y0).
      omega.
Qed.

Remark ZxHbound_lt: forall x y: Z,
  (ZxHpos x < ZxHpos y)%nat -> ZxHbound x < ZxHbound y.
Proof.
  unfold ZxHbound. 
  intros x y.
  pose (X := ZxHpos x). fold X.
  pose (Y := ZxHpos y). fold Y.
  repeat rewrite two_power_nat_S.
  generalize X Y.
  induction X0; intro.
  (** base *)
  + generalize (two_power_nat_is_positive Y0).
    replace (two_power_nat 0) with 1 by (compute ; forward).
    induction Y0; repeat rewrite two_power_nat_S; omega.
  (** cont. *)
  + rewrite two_power_nat_S. 
    induction Y0.
    (** base *)
    * generalize (two_power_nat_is_positive X0).
      replace (two_power_nat 0) with 1 by (compute;forward).
      omega.
    (** cont. *)
    * intro.
      rewrite two_power_nat_S. 
      apply (IHX0 Y0).
      omega.
Qed.

Lemma ZxHpower: forall (n: nat) (z: Z),
   -(two_power_nat n) <= z < two_power_nat n -> ZxHbound z <= two_power_nat n.
Proof.
  intros.
  rewrite <- ZxHbound_of_two_power_nat_minus_one.
  apply ZxHbound_le.
  destruct H.
  case_leq 0 z; intro.
  (** 0 <= z *)
  + clear H.
    replace (two_power_nat n - 1) with (z + ((two_power_nat n - 1) - z)) by auto with zarith.
    pose (d := ((two_power_nat n - 1) - z)); fold d.
    assert (0 <= d) as D by (unfold d; omega).
    by (apply ZxHpos_incr_for_positive).
  (** 0 > z *)
  + rewrite <- (ZxHpos_sym z).
    unfold zlnot.
    replace (two_power_nat n - 1) with (-(z+1) + (z+two_power_nat n)) by auto with zarith.
    pose (x := -(z+1)); fold x.
    pose (d := (z + two_power_nat n)); fold d.
    assert (0 <= d) as D by (unfold d; omega).
    apply ZxHpos_incr_for_positive.
    unfold x. omega. unfold d. omega.
Qed.

(** ** Main tactics.*)
Ltac Zbit_ext k := apply Zbit_ext; extensionality k.

Ltac auto_bits := autorewrite with bits ; auto with zarith.
Hint Rewrite Zbit_of_zero Zbit_of_mone : bits.
