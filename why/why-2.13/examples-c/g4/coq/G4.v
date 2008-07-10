(**********************************   G4.v  **********************************)
(*       Pierre Castéran                                                     *)


Require Import Arith.
Require Import Omega.
Require Import Relations.



Record item :Set := quad {
                    b: nat ; 
                    c2 : nat ; (* coefficient of b² *)
                    c1 :nat ; (* coefficient of b *)
                    c0 :nat    (* coefficient of b° *) }.


(* relation between two consecutive items of the sequence *)

Inductive R: item -> item ->Prop :=
 exp0 : forall n i j k, R (quad n i j (S k)) (quad  (S n) i j k)
|exp1 : forall n i j , R (quad n i (S j) 0) (quad (S n) i j n)
|exp2 : forall n i  , R (quad n (S i) 0 0)  (quad (S n) i n n).

Definition Rplus := clos_trans _ R.
Definition Rstar := clos_refl_trans _ R.

Definition reachable (q:item) := Rstar (quad 3 2 2 2) q.

(* We want to prove the following result :
   

Theorem G4_length : reachable
                    (quad
                      (3 * 2 ^ (3 * 2 ^ (3 ^3) + 3 ^3) - 1)
                      0
                      0
                      0).

*)


(* Let us introduce some concepts *)

Inductive final : item -> Prop :=
 final_intro :forall n , final (quad n 0 0 0).


Lemma final_no_future : forall q, final q -> forall q', ~ R q q'.
Proof.
 red;intros q Hq ; inversion_clear Hq. 
 inversion 1.
Qed.


Definition next (q:item) : ~(final q) ->
                           {i:item | R q i}.
 intro q ;case q.
 intros n i j k.
 case k.
 case j.
 case i.
 intro; elimtype False.  
 case H.
 constructor.
 intros i0 _ ;exists (quad (S n) i0 n n). 
 constructor.
 intros j0 _.
 exists (quad (S n) i j0 n);constructor.
 intros k0 _.
 exists (quad (S n) i j k0);constructor.
Defined.

Lemma next_unicity : forall q q' q'',
      R q q' -> R q q'' -> q' = q''.
Proof.
 destruct 1; inversion_clear 1;auto.
Qed.

Lemma reachable_Rstar : forall q q', reachable q -> Rstar q q' -> 
                           reachable q'.
Proof.
 unfold reachable.
 induction 1.
 constructor 3 with y.
 constructor 1;auto.
 auto.
 auto.
 constructor 3 with z.
 constructor 3 with y;auto.
 auto.
Qed.

Lemma Rplus_Rstar : forall q q', Rplus q q' -> Rstar q q'.
Proof.
 induction 1.
 constructor 1;auto.
 econstructor 3;eauto.
Qed.

(* First, we want to explore interactively the reachable items *)


Definition final_dec : forall i, {final i}+{~final i}.
 intro i; case i.
 intros b0 i1;case i1.
 intro j1;case j1.
 intro k1;case k1.
 left;constructor.
 right;red;inversion 1.
 right;red;inversion 1.
 right;red;inversion 1.
Defined.


Fixpoint nexts  (n:nat)(q:item){struct n} : option item :=
  match n with 0 => Some q
             | S p => (match (final_dec q) 
                       with left _ => None
                           | right H => (match (next q H) with
                                            exist q'' _ => nexts p q''
                                         end)
                       end)
  end.


Lemma nexts_ok : forall n q q', nexts n q  = Some q' ->
                                Rstar q q'.
Proof.
 induction n.
 simpl.
 injection 1.
 destruct 1;constructor 2.
 simpl.
 intros q q'; case (final_dec q).
 discriminate 2.
 intros H0.
 case (next q H0).
 intros; econstructor 3.
 econstructor 1.
 eexact r.
 apply IHn.  
 auto.
Qed.


Definition nth_R n := nexts n (quad 3 2 2 2).

(* let us look for regularities *)

Eval compute in nth_R 0.
(*  = Some (quad 3 2 2 2)
     : option item
*)

Eval compute in nth_R 2.

(*
    = Some (quad 5 2 2 0)
     : option item
*)

Eval compute in nth_R 3.

(*
    = Some (quad 6 2 1 5)
     : option item
*)

Eval compute in nth_R 8.
(*     = Some (quad 11 2 1 0)
     : option item
*)

Eval compute in nth_R 20.
(*
     = Some (quad 23 2 0 0)
     : option item
*)

Eval compute in nth_R 21.

(*
 = Some (quad 24 1 23 23)
     : option item
*)
Eval compute in nth_R 44.
(*
  = Some (quad 47 1 23 0)
     : option item
*)
Eval compute in nth_R 92.

(*
= Some (quad 95 1 22 0)
     : option item
*)

Eval compute in nth_R 188.

(*
    = Some (quad 191 1 21 0)
     : option item 
*)


(* Eureka : 11 = 3 * 2 ^ 2 - 1
            23 = 3 * 2 ^ 3 - 1
            47 = 3 * 2 ^ 4 - 1 
            95 = 3 * 2 ^ 5 - 1
           191 = 3 * 2 ^ 6 - 1
*)


(* let us define the function F n = 3 * 2 ^ n - 1 *)

Fixpoint power (b:nat)(n:nat){struct n}:nat :=
 match n with 0 => 1
            | S p => b * power b p
 end.

Notation "n ^ p" := (power n p):nat_scope.


Definition F n := pred (3*(2 ^n)).

Lemma F_Sn : forall n,  F (S n) = S (2*(F n)).
 induction n;simpl.
 compute.
 auto.
 unfold F;simpl.
 unfold F at 1 in IHn.
 simpl in IHn.
 omega.
Qed.

(* we start a sequence of reachability lemmas, until we reach
  a final item of the sequence *)

  
Lemma acc4 : reachable (quad 4 2 2 1).
 unfold reachable, Rplus.
 constructor 1.
 constructor.
Qed. 

Lemma acc5 : reachable (quad 5 2 2 0).
 eapply reachable_Rstar.
 eexact acc4.
 constructor 1;constructor.
Qed.




(* A first generalization of the previous examples *)

Lemma L1 :  forall k i j n, 
      Rplus (quad n i j (S k)) (quad (S k + n) i j 0).
Proof.
 induction k.
 simpl.
 left.
 constructor.
 simpl.
 eright.
 eleft;econstructor.
 simpl in IHk.
 replace (S (S (k+n))) with (S (k + (S n))).
 auto.
 apply IHk.
 auto with arith.
Qed.


Lemma L1' :  forall k i j n, 
      Rstar (quad n i j k) (quad (k+n) i j 0).
 intro k; case k;intros.
 simpl;constructor 2.
 apply Rplus_Rstar.  
 apply L1.
Qed.


(* hmmm, let us look at items of the form (base,i,j,0)  *)

Lemma L2 : forall n i j, 
    Rplus (quad n i (S j) 0) (quad (S (2 * n)) i j 0).
Proof.
 intros.
 case n.
 simpl.
 econstructor 1.
 constructor.
 econstructor 2. 
 econstructor 1; econstructor.
 replace (S (2* S n0)) with ( S n0  + S (S n0)).
 apply L1.
 omega.
Qed.

Lemma L3 :  forall n i j,
                   reachable (quad (F n) i (S j) 0) ->
                   reachable (quad (F (S n)) i j 0).
Proof.
 intros.
 econstructor 3. 
 unfold reachable in H. 
 eexact H.
 rewrite F_Sn.
 apply Rplus_Rstar; apply L2.
Qed.

Lemma L4 :  forall k n i j, 
          reachable (quad (F n) i (k+j) 0) ->
          reachable (quad (F (k+n)) i j 0).
Proof.
 induction k.
 simpl.
 auto.
 intros; econstructor 3. 
 unfold reachable; unfold reachable in IHk; eapply IHk.
 replace (S k + j) with (k + (S j)) in H.
 unfold reachable in H;eauto.
 omega.
 simpl;rewrite F_Sn.  
 apply Rplus_Rstar; apply L2.
Qed.


Lemma example2 : reachable (quad (F 1) 2 2 0).
Proof.
 compute.
 apply Rplus_Rstar. 
 replace 5 with (2+3).
 apply L1.
 simpl;trivial.
Qed.

Lemma example3 : reachable (quad (F 2) 2 1 0).
Proof.
 apply L3.
 apply example2.
Qed.

Lemma example4 : reachable (quad (F 3) 2 0 0).
Proof.
 replace 3 with (2+1).
 apply L4.
 simpl (2+0);apply example2.
 simpl;auto.
Qed.

(* Ah, ah : the second component of the current tuple becomes 1 *)

Lemma example5 : reachable (quad (S (F 3)) 1 (F 3) (F 3)). 
Proof.
 eapply reachable_Rstar. 
 eexact example4.
 constructor 1.
 constructor.
Qed.



Lemma example6 : reachable (quad (F 4) 1 (F 3) 0).
Proof.
 rewrite (F_Sn 3).
 eapply reachable_Rstar.
 eexact example5.
 replace (S (2 * (F 3))) with (F 3 + (S ( F 3))).
 apply L1'.
 compute.
 trivial.
Qed.


Lemma example7 :  reachable (quad (F 27) 1 0 0).
Proof.
 replace 27 with (F 3 + 4).
 apply L4. 
 rewrite <- plus_n_O.
 apply example6.
 compute;trivial.
Qed.

(* Ok, the second component becomes 0 *)

Lemma example8 : reachable (quad (S (F 27)) 0 (F 27) (F 27)).
Proof.
 eapply reachable_Rstar.
 eexact example7.
 constructor 1.
 constructor.
Qed.

Lemma example9 : reachable (quad (F 28) 0 (F 27) 0).
Proof.
 rewrite F_Sn.
 replace (S (2 * F 27)) with (F 27 + (S (F 27))).
 2:omega.
 eapply reachable_Rstar. 
 eexact example8. 
 apply L1'.
Qed.

(* yep! we are finished ! *)

Lemma example10 : reachable (quad (F (F 27 + 28)) 0 0 0).
Proof.
 apply L4.
 rewrite <- plus_n_O.
 apply example9.
Qed.


(* F (F 27 + 28) = F (402653211) =  3 * 2 ^ 402653211 - 1  =

               3
             (3  )       3
       (3 x 2        + 3   )
  3 x 2                       -1
*)


Lemma big_number_eq : F (F 27 + 28) = 
          3 * 2 ^ (3 * 2 ^ (3 ^3) + 3 ^3) - 1.
Proof.
 unfold F.
 rewrite <- pred_of_minus.
 replace 27 with (3^3).
 generalize (3^3).
 intro n; assert (0 < 2 ^ n). 
 induction n.
 simpl;auto with arith.
 simpl.
 auto with arith. 
 generalize H ; generalize (2 ^n).
 intros.
 replace (pred (3*n0) + S n) with (3*n0 + n).
 auto.
 omega.
 simpl;trivial.
Qed.



Theorem G4_length : reachable
                    (quad
                      (3 * 2 ^ (3 * 2 ^ (3 ^3) + 3 ^3) - 1)
                      0
                      0
                      0).
Proof.
 generalize big_number_eq.
 generalize (3 * 2 ^ (3 * 2 ^ 3 ^ 3 + 3 ^ 3) - 1).
 generalize example10.
 generalize (F (F 27 + 28)).
 induction 2.
 auto.
Qed.




