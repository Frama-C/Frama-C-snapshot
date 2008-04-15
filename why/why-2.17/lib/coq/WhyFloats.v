
(* Why model for floating-point numbers
   Implements the file lib/why/floats.why *)

Require Export Reals.
Require Export AllFloat.
Require Export RND.

Let radix := 2%Z.

Section Utiles.

Lemma radixGreaterOne: (1 < radix)%Z.
auto with zarith.
Qed.


Definition nat_to_N (n:nat) := match n with 
   |  0    => N0
   | (S m) => (Npos (P_of_succ_nat m))
   end.

Lemma nat_to_N_correct: forall n:nat, Z_of_N (nat_to_N n)=n.
intros.
intros; induction n; simpl; auto.
Qed.


Definition make_bound (p E:nat) := Bound 
      (P_of_succ_nat (pred (Zabs_nat (Zpower_nat radix p))))
      (nat_to_N E).

Lemma make_EGivesEmin: forall p E:nat, 
        (Z_of_N (dExp (make_bound p E)))=E.
intros; simpl; apply nat_to_N_correct.
Qed.

Lemma make_pGivesBound: forall p E:nat, 
        Zpos (vNum (make_bound p E))=(Zpower_nat radix p).
intros.
unfold make_bound, vNum.
apply
 trans_eq
  with
    (Z_of_nat
       (nat_of_P
          (P_of_succ_nat
             (pred (Zabs_nat (Zpower_nat radix p)))))).
unfold Z_of_nat in |- *; rewrite nat_of_P_o_P_of_succ_nat_eq_succ;
 auto with zarith.
rewrite nat_of_P_o_P_of_succ_nat_eq_succ; auto with arith zarith.
cut (Zabs (Zpower_nat radix p) = Zpower_nat radix p).
intros H; pattern (Zpower_nat radix p) at 2 in |- *; rewrite <- H.
rewrite Zabs_absolu.
rewrite <- (S_pred (Zabs_nat (Zpower_nat radix p)) 0);
 auto with arith zarith.
apply lt_Zlt_inv; simpl in |- *; auto with zarith arith.
rewrite <- Zabs_absolu; rewrite H; auto with arith zarith.
apply Zabs_eq; auto with arith zarith.
Qed.


End Utiles.



Inductive mode : Set := nearest_even | to_zero | up | down | nearest_away.

Coercion Local FtoRradix := FtoR radix.

(** Single precision *)

Let bsingle := make_bound 24 149.

Lemma psGreaterThanOne: 1 < 24.
auto with arith.
Qed.

Lemma psGivesBound: Zpos (vNum bsingle) = Zpower_nat radix 24.
unfold bsingle; apply make_pGivesBound.
Qed.

Record single : Set := mk_single {
   sf         : float;
   Hcansf     : Fcanonic radix bsingle sf;
   s_to_exact : R;
   s_to_model : R
 }.


Definition s_to_r (f:single) := FtoRradix (sf f).

Definition single_round_error (f:single):= 
    (Rabs ((s_to_exact f) - (s_to_r f))).

Definition single_total_error (f:single):= 
    (Rabs ((s_to_model f) - (s_to_r f))).

Definition single_set_model (f:single) (r:R) :=
      mk_single (sf f) (Hcansf f) (s_to_exact f) r.



Definition r_to_s_aux (m:mode) (r r1 r2:R) := match m with
  |  nearest_even => mk_single (RND_EvenClosest bsingle radix 24 r) 
                     (RND_EvenClosest_canonic bsingle radix 24 
                         radixGreaterOne psGreaterThanOne psGivesBound r)
                     r1 r2
  |  to_zero      => mk_single (RND_Zero bsingle radix 24 r) 
                     (RND_Zero_canonic bsingle radix 24 
                         radixGreaterOne psGreaterThanOne psGivesBound r)
                     r1 r2
  |  up           => mk_single (RND_Max bsingle radix 24 r) 
                     (RND_Max_canonic bsingle radix 24 
                         radixGreaterOne psGreaterThanOne psGivesBound r)
                     r1 r2
  |  down         => mk_single (RND_Min bsingle radix 24 r) 
                     (RND_Min_canonic bsingle radix 24 
                         radixGreaterOne psGreaterThanOne psGivesBound r)
                     r1 r2
  |  nearest_away => mk_single (RND_ClosestUp bsingle radix 24 r) 
                     (RND_ClosestUp_canonic bsingle radix 24 
                         radixGreaterOne psGreaterThanOne psGivesBound r)
                     r1 r2
  end.

   
Definition r_to_s (m:mode) (r:R) := r_to_s_aux m r r r.
   

Definition add_single (m:mode) (f1 f2:single) :=
     r_to_s_aux m (sf f1+sf f2) 
                  (s_to_exact f1+s_to_exact f2) (s_to_model f1+s_to_model f2).

Definition sub_single (m:mode) (f1 f2:single) :=
     r_to_s_aux m (sf f1-sf f2) 
                  (s_to_exact f1-s_to_exact f2) (s_to_model f1-s_to_model f2).

Definition mul_single (m:mode) (f1 f2:single) :=
     r_to_s_aux m (sf f1*sf f2) 
                  (s_to_exact f1*s_to_exact f2) (s_to_model f1*s_to_model f2).

Definition div_single (m:mode) (f1 f2:single) :=
     r_to_s_aux m (sf f1/sf f2) 
                  (s_to_exact f1/s_to_exact f2) (s_to_model f1/s_to_model f2).

Definition sqrt_single (m:mode) (f:single) :=
     r_to_s_aux m (sqrt (sf f))
                  (sqrt(s_to_exact f)) (sqrt(s_to_model f)).

Definition neg_single (m:mode) (f:single) :=
   mk_single (Fopp (sf f)) 
         (FcanonicFopp radix bsingle (sf f) (Hcansf f))
         (-(s_to_exact f)) (-(s_to_model f)).

Definition abs_single (m:mode) (f:single) :=
   mk_single (Fabs (sf f)) 
         (FcanonicFabs radix radixGreaterOne bsingle (sf f) (Hcansf f))
         (Rabs (s_to_exact f)) (Rabs (s_to_model f)).

Definition max_single := ((2-powerRZ radix (-23))*powerRZ radix 127)%R.


(** Double precision *)

Let bdouble := make_bound 53 1074.

Lemma pdGreaterThanOne: 1 < 53.
auto with arith.
Qed.

Lemma pdGivesBound: Zpos (vNum bdouble) = Zpower_nat radix 53.
unfold bdouble; apply make_pGivesBound.
Qed.

Record double : Set := mk_double {
   df         : float;
   Hcandf     : Fcanonic radix bdouble df;
   d_to_exact : R;
   d_to_model : R
 }.


Definition d_to_r (f:double) := FtoRradix (df f).

Definition double_round_error (f:double):= 
    (Rabs ((d_to_exact f) - (d_to_r f))).

Definition double_total_error (f:double):= 
    (Rabs ((d_to_model f) - (d_to_r f))).

Definition double_set_model (f:double) (r:R) :=
      mk_double (df f) (Hcandf f) (d_to_exact f) r.



Definition r_to_d_aux (m:mode) (r r1 r2:R) := match m with
  |  nearest_even => mk_double (RND_EvenClosest bdouble radix 53 r) 
                     (RND_EvenClosest_canonic bdouble radix 53 
                         radixGreaterOne pdGreaterThanOne pdGivesBound r)
                     r1 r2
  |  to_zero      => mk_double (RND_Zero bdouble radix 53 r) 
                     (RND_Zero_canonic bdouble radix 53 
                         radixGreaterOne pdGreaterThanOne pdGivesBound r)
                     r1 r2
  |  up           => mk_double (RND_Min bdouble radix 53 r) 
                     (RND_Min_canonic bdouble radix 53 
                         radixGreaterOne pdGreaterThanOne pdGivesBound r)
                     r1 r2
  |  down         => mk_double (RND_Max bdouble radix 53 r) 
                     (RND_Max_canonic bdouble radix 53 
                         radixGreaterOne pdGreaterThanOne pdGivesBound r)
                     r1 r2
  |  nearest_away => mk_double (RND_ClosestUp bdouble radix 53 r) 
                     (RND_ClosestUp_canonic bdouble radix 53 
                         radixGreaterOne pdGreaterThanOne pdGivesBound r)
                     r1 r2
  end.

   
Definition r_to_d (m:mode) (r:R) := r_to_d_aux m r r r.
   

Definition add_double (m:mode) (f1 f2:double) :=
     r_to_d_aux m (df f1+df f2) 
                  (d_to_exact f1+d_to_exact f2) (d_to_model f1+d_to_model f2).

Definition sub_double (m:mode) (f1 f2:double) :=
     r_to_d_aux m (df f1-df f2) 
                  (d_to_exact f1-d_to_exact f2) (d_to_model f1-d_to_model f2).

Definition mul_double (m:mode) (f1 f2:double) :=
     r_to_d_aux m (df f1*df f2) 
                  (d_to_exact f1*d_to_exact f2) (d_to_model f1*d_to_model f2).

Definition div_double (m:mode) (f1 f2:double) :=
     r_to_d_aux m (df f1/df f2) 
                  (d_to_exact f1/d_to_exact f2) (d_to_model f1/d_to_model f2).

Definition sqrt_double (m:mode) (f:double) :=
     r_to_d_aux m (sqrt (df f))
                  (sqrt(d_to_exact f)) (sqrt(d_to_model f)).

Definition neg_double (m:mode) (f:double) :=
   mk_double (Fopp (df f)) 
         (FcanonicFopp radix bdouble (df f) (Hcandf f))
         (-(d_to_exact f)) (-(d_to_model f)).

Definition abs_double (m:mode) (f:double) :=
   mk_double (Fabs (df f)) 
         (FcanonicFabs radix radixGreaterOne bdouble (df f) (Hcandf f))
         (Rabs (d_to_exact f)) (Rabs (d_to_model f)).

Definition max_double := ((2-powerRZ radix (-52))*powerRZ radix 1023)%R.


(** Quad precision *)

Definition quad: Set.
Admitted.

Definition add_quad : mode -> quad -> quad -> quad.
Admitted.

Definition sub_quad : mode -> quad -> quad -> quad.
Admitted.

Definition mul_quad : mode -> quad -> quad -> quad.
Admitted.

Definition div_quad : mode -> quad -> quad -> quad.
Admitted.

Definition neg_quad : mode -> quad -> quad.
Admitted.

Definition abs_quad : mode -> quad -> quad.
Admitted.

Definition sqrt_quad : mode -> quad -> quad.
Admitted.

Definition q_to_r : quad -> R.
Admitted.

Definition q_to_exact : quad -> R.
Admitted.

Definition q_to_model : quad -> R.
Admitted.

Definition r_to_q : mode -> R -> quad.
Admitted.

Definition quad_round_error : quad -> R.
Admitted.

Definition quad_total_error : quad -> R.
Admitted.

Definition quad_set_model : quad -> R -> quad.
Admitted.

Definition max_quad : R.
Admitted.


(** Jumping from one format to another *)

Lemma double_of_single_aux: forall f:single, 
   Fcanonic radix bdouble (Fnormalize radix bdouble 53 (sf f)).
intros; apply FnormalizeCanonic;[auto with zarith|auto with zarith|
       apply pdGivesBound|idtac].
destruct f; simpl.
assert (Fbounded bsingle sf0);[apply FcanonicBound with radix; auto|idtac].
elim H; intros; split.
rewrite psGivesBound in H0; rewrite pdGivesBound; auto with zarith.
apply Zlt_le_trans with (1:=H0); auto with zarith.
unfold bsingle in H1; unfold bdouble.
rewrite make_EGivesEmin in H1; rewrite make_EGivesEmin; auto with zarith.
apply Zle_trans with (2:=H1); auto with zarith.
Qed.

Definition double_of_single (f:single) :=
   mk_double (Fnormalize radix bdouble 53 (sf f)) 
         (double_of_single_aux f)
         (s_to_exact f) (s_to_model f).

Definition single_of_double (m:mode) (d:double) := r_to_s m (df d).

Definition quad_of_single : single -> quad.
Admitted.

Definition single_of_quad : mode -> quad -> single.
Admitted.

Definition quad_of_double : double -> quad.
Admitted.

Definition double_of_quad : mode -> quad -> double.
Admitted.


(** Small integers, like 1 or 2, do not suffer from rounding *)

Theorem small_int_no_round: forall (m:mode), forall (z:Z), 
 (Zabs z <= Zpower_nat radix 53)%Z -> (d_to_r (r_to_d m (IZR z))= IZR z)%R.
intros.
assert (exists f:float, (FtoRradix f=IZR z) /\ Fbounded bdouble f).
case (Zle_lt_or_eq (Zabs z)  (Zpower_nat radix 53)); auto;intros H1.
exists (Float z 0); split;[unfold FtoRradix, FtoR; simpl; ring|idtac].
split;[rewrite pdGivesBound|simpl]; auto with zarith.
case (Zle_or_lt 0 z); intros H2;
  [idtac|rewrite <- Zabs_Zopp in H1]; rewrite Zabs_eq in H1; auto with zarith.
exists (Float  (Zpower_nat radix 52) 1); split.
rewrite H1; unfold FtoRradix, FtoR.
apply trans_eq with ((Zpower_nat radix 52)*(powerRZ radix 1))%R; auto.
repeat rewrite Zpower_nat_Z_powerRZ; auto.
rewrite <- powerRZ_add; auto with real zarith.
split;[rewrite pdGivesBound|simpl]; auto with zarith.
exists (Float  (-(Zpower_nat radix 52))%Z 1); split.
apply trans_eq with (-(-z)%Z)%R; [idtac|rewrite Ropp_Ropp_IZR;ring].
rewrite H1; unfold FtoRradix, FtoR.
apply trans_eq with ((-(Zpower_nat radix 52))%Z*(powerRZ radix 1))%R; auto.
rewrite Ropp_Ropp_IZR; repeat rewrite Zpower_nat_Z_powerRZ; auto.
apply trans_eq with (-(powerRZ radix (S 51) * powerRZ radix 1))%R;[ring|idtac].
rewrite <- powerRZ_add; auto with real zarith.
split;[rewrite pdGivesBound|simpl]; auto with zarith.
elim H0; intros f T; elim T; intros H1 H2; clear T H0.
rewrite <- H1.
unfold r_to_d, r_to_d_aux, d_to_r; simpl.
unfold FtoRradix; apply sym_eq.
case m; simpl.
apply RoundedModeProjectorIdemEq with bdouble 53%nat 
  (EvenClosest bdouble radix 53); fold FtoRradix;
  [auto with zarith|auto with zarith| apply pdGivesBound|
    idtac|auto|idtac].
apply EvenClosestRoundedModeP;[auto with zarith| auto with zarith| 
  apply pdGivesBound].
apply RND_EvenClosest_correct; [auto with zarith| auto with zarith| 
  apply pdGivesBound].
apply RoundedModeProjectorIdemEq with bdouble 53%nat 
  (ToZeroP bdouble radix); fold FtoRradix;
  [auto with zarith|auto with zarith| apply pdGivesBound|
    idtac|auto|idtac].
apply ToZeroRoundedModeP with 53%nat;[auto with zarith| auto with zarith| 
  apply pdGivesBound].
apply RND_Zero_correct; [auto with zarith| auto with zarith| 
  apply pdGivesBound].
apply RoundedModeProjectorIdemEq with bdouble 53%nat 
  (isMin bdouble radix); fold FtoRradix;
  [auto with zarith|auto with zarith| apply pdGivesBound|
    idtac|auto|idtac].
apply MinRoundedModeP with 53%nat;[auto with zarith| auto with zarith| 
  apply pdGivesBound].
apply RND_Min_correct; [auto with zarith| auto with zarith| 
  apply pdGivesBound].
apply RoundedModeProjectorIdemEq with bdouble 53%nat 
  (isMax bdouble radix); fold FtoRradix;
  [auto with zarith|auto with zarith| apply pdGivesBound|
    idtac|auto|idtac].
apply MaxRoundedModeP with 53%nat;[auto with zarith| auto with zarith| 
  apply pdGivesBound].
apply RND_Max_correct; [auto with zarith| auto with zarith| 
  apply pdGivesBound].
apply RoundedModeProjectorIdemEq with bdouble 53%nat 
  (Closest bdouble radix); fold FtoRradix;
  [auto with zarith|auto with zarith| apply pdGivesBound|
    idtac|auto|idtac].
apply ClosestRoundedModeP with 53%nat;[auto with zarith| auto with zarith| 
  apply pdGivesBound].
apply RND_ClosestUp_correct; [auto with zarith| auto with zarith| 
  apply pdGivesBound].
Qed.

Theorem zero_no_round: forall (m:mode), ((d_to_r (r_to_d m (IZR 0))=0))%R.
intros.
rewrite small_int_no_round; auto with real zarith.
Qed.

Theorem one_no_round: forall (m:mode), ((d_to_r (r_to_d m (IZR 1))=1))%R.
intros.
rewrite small_int_no_round; auto with real zarith.
Qed.

Theorem two_no_round: forall (m:mode),  ((d_to_r (r_to_d m (IZR 2))=2))%R.
intros.
rewrite small_int_no_round; auto with real zarith.
Qed.

Theorem zero_rounded_in_zero: forall (r:R), (r=0)%R ->
 (FtoRradix (RND_EvenClosest bdouble radix 53 r)=0)%R.
intros; rewrite <- zero_no_round with nearest_even.
rewrite H; unfold d_to_r, r_to_d; simpl; auto.
Qed.

Theorem one_rounded_in_one: forall (r:R), (r=1)%R ->
 (FtoRradix (RND_EvenClosest bdouble radix 53 r)=1)%R.
intros; rewrite <- one_no_round with nearest_even.
rewrite H; unfold d_to_r, r_to_d; simpl; auto.
Qed.

Theorem max_single_pos: (0 < max_single)%R.
unfold max_single.
apply Rle_lt_trans with (0*powerRZ radix 127)%R;[right; ring|apply Rmult_lt_compat_r; auto with real zarith].
apply Rplus_lt_reg_r with (powerRZ radix (-23)); ring_simplify.
apply Rlt_le_trans with (powerRZ radix 1); auto with real zarith.
apply Rlt_powerRZ; unfold radix; simpl; auto with real zarith.
Qed.


Theorem max_double_pos: (0 < max_double)%R.
unfold max_double.
apply Rle_lt_trans with (0*powerRZ radix 1023)%R;[right; ring|apply Rmult_lt_compat_r; auto with real zarith].
apply Rplus_lt_reg_r with (powerRZ radix (-52)); ring_simplify.
apply Rlt_le_trans with (powerRZ radix 1); auto with real zarith.
apply Rlt_powerRZ; unfold radix; simpl; auto with real zarith.
Qed.


Hint Resolve psGreaterThanOne psGivesBound pdGreaterThanOne pdGivesBound  EvenClosestRoundedModeP RND_EvenClosest_correct RND_EvenClosest_canonic  (FcanonicBound radix) (zero_no_round nearest_even) (one_no_round nearest_even) (two_no_round nearest_even) zero_rounded_in_zero one_rounded_in_one max_single_pos max_double_pos.