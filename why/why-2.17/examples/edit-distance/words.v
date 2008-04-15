(* Load Programs. *)
(*s Axiomatization of words over a alphabet [A]. *)

Require Import List.
Require Import ZArith.
Require Import ZArithRing.
Require Import WhyArrays.
Require Import Zwf.
Require Import Omega.

(*s Alphabet. This is an abstract type [A] where equality is decidable.*)

Parameter A : Set.
 
Axiom A_eq_dec : forall a b:A, {a = b} + {a <> b}.

(*s Words are list of characters. *)

Definition word := list A.
Definition eps := nil (A:=A).

Definition concat := app.
Implicit Arguments concat [A].


(*s Distance. It is axiomatized as a predicate [dist w1 w2 n] expressing that
    [w1] and [w2] are at distance at most [n] (i.e. there exists a path of 
    length [n] from [w1] to [w2]). *)

Inductive dist : word -> word -> Z -> Prop :=
  | dist_eps : dist eps eps 0%Z
  | dist_add_left :
      forall (w1 w2:word) (n:Z),
        dist w1 w2 n -> forall a:A, dist (cons a w1) w2 (n + 1)%Z
  | dist_add_right :
      forall (w1 w2:word) (n:Z),
        dist w1 w2 n -> forall a:A, dist w1 (cons a w2) (n + 1)%Z
  | dist_context :
      forall (w1 w2:word) (n:Z),
        dist w1 w2 n -> forall a:A, dist (cons a w1) (cons a w2) n.

(*s Then we introducve the minimal distance between two words. *)

Definition min_dist (w1 w2:word) (n:Z) :=
  dist w1 w2 n /\ (forall m:Z, dist w1 w2 m -> (n <= m)%Z).


(*s Length of a word (in type [Z]). *)

Fixpoint Zlength (w:word) : Z :=
  match w with
  | nil => 0%Z
  | cons _ w' => (Zlength w' + 1)%Z
  end.


(*s Auxiliary result on words:
    Any word [au] starting with a character can be written as a
    word [vb] ending with a character. *)

Fixpoint last_char (a:A) (u:word) {struct u} : A :=
  match u with
  | nil => a
  | cons c u' => last_char c u'
  end.

Fixpoint but_last (a:A) (u:word) {struct u} : word :=
  match u with
  | nil => eps
  | cons c u' => cons a (but_last c u')
  end.

Lemma first_last_explicit :
 forall (u:word) (a:A),
   concat (but_last a u) (cons (last_char a u) eps) = cons a u.
Proof.
simple induction u; simpl.
reflexivity.
intros.
rewrite (H a).
reflexivity.
Qed.

Lemma first_last :
 forall (a:A) (u:word),
    exists v, 
      exists b, 
        concat v (cons b eps) = cons a u /\
          Zlength v = Zlength u.
Proof.
intros a u.
exists (but_last a u).
exists (last_char a u).
split.
 exact (first_last_explicit u a).
generalize a; induction u; simpl; intros.
omega.
generalize (IHu a0); intros; omega.
Qed.


(*s Key lemma: if [dist w1 aw2 n] then there exists [u1], [v1],
    [k] such that [dist v1 w2 k], [w1=u1v1] and [k+(length u1)-1<=m]. *)

Lemma key_lemma_right :
 forall (w1 w'2:word) (m:Z) (a:A),
   dist w1 w'2 m ->
   forall w2:word,
     w'2 = cons a w2 ->
      exists u1,
       exists v1,
        exists k,
           w1 = concat u1 v1 /\
             dist v1 w2 k /\ (k + Zlength u1 <= m + 1)%Z.
Proof.
intros w1 w'2 m a H; elim H.
(* 1. [dist_eps]: absurd *)
intros; discriminate H0.
(* 2. [dist_add_left]: we use induction hypothesis. *)
intros w'1 w3 n Hdist Hrec b w2 Heq.
  elim (Hrec w2 Heq); intros u'1 Hex.
elim Hex; clear Hex; intros v'1 Hex.
elim Hex; clear Hex; intros k Hex.
decompose [and] Hex; clear Hex.
elim (first_last b u'1); intros u1 Hex.
elim Hex; intros c [Hc Hlength].
exists u1; exists (cons c v'1); exists (k + 1)%Z.
repeat split.
rewrite H0.
rewrite app_comm_cons.
rewrite <- Hc.
rewrite app_ass; reflexivity.
apply dist_add_left; assumption.
omega.
(* 3. [dist_add_right]: direct *)
intros.
exists eps; exists w0; exists n.
repeat split.
inversion H2.
rewrite <- H5; assumption.
simpl; omega.
(* 4. [dist_context]: direct *)
intros.
inversion H2.
exists (cons a eps); exists w0; exists n.
repeat split.
rewrite <- H5; assumption.
simpl; omega.
Qed.


(*s To get the symmetric key lemma, we use the symmetry of [dist]. *)

Lemma dist_symetry :
 forall (w1 w2:word) (n:Z), dist w1 w2 n -> dist w2 w1 n.
Proof.
simple induction 1; intros.
exact dist_eps.
apply dist_add_right; assumption.
apply dist_add_left; assumption.
apply dist_context; assumption.
Qed.

Lemma key_lemma_left :
 forall (w1 w2:word) (m:Z) (a:A),
   dist (cons a w1) w2 m ->
    exists u2,
      exists v2,
        exists k,
          w2 = concat u2 v2 /\
           dist w1 v2 k /\ (k + Zlength u2 <= m + 1)%Z.
Proof.
intros w1 w2 m a Hdist.
generalize (dist_symetry (cons a w1) w2 m Hdist); intro Hrev.

elim (key_lemma_right w2 (cons a w1) m a Hrev w1).
intros u2 Hex; elim Hex; clear Hex.
intros v2 Hex; elim Hex; clear Hex.
intros k Hex.
decompose [and] Hex; clear Hex.
exists u2.
exists v2.
exists k.
repeat split; try assumption.
apply dist_symetry; assumption.
reflexivity.
Qed.


(*s Concatenation to the left: if [dist w1 w2 n] then
    [dist uw1 w2 (|u|+n)] and [dist w1 uw2 (|u|+n)]. *)

Lemma dist_concat_left :
 forall (u v w:word) (n:Z),
   dist v w n -> dist (concat u v) w (Zlength u + n).
Proof.
simple induction u; simpl; intros.
auto.
replace (Zlength l + 1 + n)%Z with (Zlength l + n + 1)%Z;
 [ idtac | omega ].
apply dist_add_left; auto.
Qed.

Lemma dist_concat_right :
 forall (u v w:word) (n:Z),
   dist v w n -> dist v (concat u w) (Zlength u + n).
Proof.
simple induction u; simpl; intros.
auto.
replace (Zlength l + 1 + n)%Z with (Zlength l + n + 1)%Z;
 [ idtac | omega ].
apply dist_add_right; auto.
Qed.


(*s First main lemma for correctness: [d(aw1,aw2)=d(w1,w2)]. *)

Lemma min_dist_equal :
 forall (w1 w2:word) (a:A) (n:Z),
   min_dist w1 w2 n -> min_dist (cons a w1) (cons a w2) n.
Proof.
intros w1 w2 a n.
unfold min_dist.
generalize dist_context; intuition.

inversion H0.

elim (key_lemma_right w1 (cons a w2) n0 a H7 w2);
 [ idtac | reflexivity ].
intros u1 Hex; elim Hex; clear Hex.
intros v1 Hex; elim Hex; clear Hex.
intros k Hex.
 decompose [and] Hex; clear Hex.
generalize (dist_concat_left u1 v1 w2 k H10); intro.
apply Zle_trans with (Zlength u1 + k)%Z.
apply H2.
rewrite H8; assumption.
omega.
elim (key_lemma_left w1 w2 n0 a H7).
intros u2 Hex; elim Hex; clear Hex.
intros v2 Hex; elim Hex; clear Hex.
intros k Hex.
 decompose [and] Hex; clear Hex.
generalize (dist_concat_right u2 w1 v2 k H10); intro.
apply Zle_trans with (Zlength u2 + k)%Z.
apply H2.
rewrite H8; assumption.
omega.
auto.
Qed.


(*s Second main lemma for correctness: \par\noindent
    if [~a=b], then [d(aw1,bw2)=1+min(d(aw1,w2),d(w1,bw2))]. *)

Lemma min_dist_diff :
 forall (w1 w2:word) (a b:A) (m p:Z),
   a <> b ->
   min_dist (cons a w1) w2 p ->
   min_dist w1 (cons b w2) m ->
   min_dist (cons a w1) (cons b w2) (Zmin m p + 1).
 Proof.
intros w1 w2 a b m p.
 unfold min_dist; intuition.
unfold Zmin.
case (m ?= p)%Z; generalize dist_add_left dist_add_right; intuition.

generalize (Zle_min_l m p) (Zle_min_r m p) Zplus_le_compat_r Zle_trans.
inversion H1; intuition eauto.
Qed.

(*s Two trivial lemmas needed for correctness. *)

Lemma min_dist_eps :
 forall (w:word) (a:A) (n:Z),
   min_dist w eps n -> min_dist (cons a w) eps (n + 1).
 Proof.
unfold min_dist.
intros w a n [H1 H2].
 split.
apply dist_add_left.
assumption.
intros m Hm; inversion Hm.
generalize (H2 n0 H5).
intros; omega.
Qed.

Lemma min_dist_eps_length : forall w:word, min_dist eps w (Zlength w).
Proof.
intros w; unfold min_dist; intuition.
induction w; simpl; intros.
exact dist_eps.
apply dist_add_right; assumption.
generalize m H.
 clear m H.
induction w; intros m H; inversion H; simpl.
omega.
generalize (IHw n H4); intro; omega.
Qed.


(*s Suffixes of an array of characters. 
    Given an array [t] of length [n] of characters, we define
    [suffix t i] as the word [t(i)t(i+1)...t(n-1)],
    by well-founded recursion over [n-i]. *)

Section suffixes.

Variable n : Z.
Variable t : array A.

Definition F : forall i:Z, (forall j:Z, Zwf_up n j i -> word) -> word.
intros i f.
elim (Z_lt_ge_dec i 0); intro Hi.
exact eps.
elim (Z_lt_ge_dec i n); intro H1.
refine (cons (access t i) (f (i + 1)%Z _)).
unfold Zwf_up; omega.
exact eps.
Defined.

Definition suffix := Fix (Zwf_up_well_founded n) (fun i:Z => word) F.

(*s To use [Fix_eq], we need to establish extensionality of [F]. *)

Lemma extensionality :
 forall (x:Z) (f g:forall y:Z, Zwf_up n y x -> word),
   (forall (y:Z) (p:Zwf_up n y x), f y p = g y p) -> F x f = F x g.
Proof.
intros.
unfold F.
case (Z_lt_ge_dec x 0); simpl.
auto.
intro; case (Z_lt_ge_dec x n); simpl.
intro.
 apply (f_equal (cons (access t x))).
apply H.
auto.
Qed.

(*s Induction case for [suffix]. *)

Lemma suffix_is_cons :
 forall i:Z,
   (0 <= i < n)%Z -> suffix i = cons (access t i) (suffix (i + 1)).
Proof.
intros i Hi.
rewrite
 (Fix_eq (Zwf_up_well_founded n) (fun i:Z => word) F extensionality)
 .
unfold F.
case (Z_lt_ge_dec i 0).
intros; absurd (i < 0)%Z; omega.
case (Z_lt_ge_dec i n).
intros; simpl.
reflexivity.
intros; absurd (i >= n)%Z; omega.
Qed.

(*s Base case: the empty suffix. *)

Lemma suffix_n_is_eps : suffix n = eps.
 Proof.
rewrite
 (Fix_eq (Zwf_up_well_founded n) (fun i:Z => word) F extensionality)
 .
unfold F.
case (Z_lt_ge_dec n 0).
reflexivity.
case (Z_lt_ge_dec n n).
intros; absurd (n < n)%Z; omega.
reflexivity.
Qed.

(*s The whole array as a word. *)

Definition word_of_array := suffix 0.

(*s Length of a suffix. *)

Lemma suffix_length :
 forall i:Z, (0 <= i <= n)%Z -> Zlength (suffix i) = (n - i)%Z.
(* proof is by induction over [n-i] *)
Proof.
intros i Hi.
 generalize Hi.
 replace i with (n - (n - i))%Z.
replace (n - (n - (n - i)))%Z with (n - i)%Z.
pattern (n - i)%Z; apply natlike_ind.
(* base case *)
intros; replace (n - 0)%Z with n.
rewrite suffix_n_is_eps; reflexivity.
omega.
(* induction case *)
intros.
rewrite suffix_is_cons; [ idtac | omega ].
simpl.
unfold Zsucc; ring.
replace (n - (1 + x) + 1)%Z with (n - x)%Z; [ idtac | ring ].
apply H0; omega.
omega.
omega.
omega.
Qed.

End suffixes.


(*s Bonus: we show the equivalence with another definition of the
    distance. *)

Inductive dist' : word -> word -> Z -> Prop :=
  | dist'_eps : dist' eps eps 0%Z
  | dist'_add_left :
      forall (w1 w2:word) (n:Z),
        dist' w1 w2 n -> forall a:A, dist' (cons a w1) w2 (n + 1)%Z
  | dist'_add_right :
      forall (w1 w2:word) (n:Z),
        dist' w1 w2 n -> forall a:A, dist' w1 (cons a w2) (n + 1)%Z
  | dist'_context :
      forall (w1 w2 u v:word) (n:Z),
        dist' w1 w2 n ->
        dist' (concat u (concat w1 v)) (concat u (concat w2 v)) n.

Lemma cons_is_concat :
 forall (w:word) (a:A), cons a w = concat (cons a eps) (concat w eps).
Proof.
intros w a; simpl.
unfold concat; unfold eps; rewrite <- app_nil_end.
reflexivity.
Qed.

Lemma dist_is_dist' :
 forall (w1 w2:word) (n:Z), dist w1 w2 n -> dist' w1 w2 n.
Proof.
simple induction 1.
exact dist'_eps.
intros; apply dist'_add_left; assumption.
intros; apply dist'_add_right; assumption.
intros.
rewrite (cons_is_concat w0 a).
rewrite (cons_is_concat w3 a).
apply dist'_context; assumption.
Qed.

Lemma dist_concat_both_left :
 forall (n:Z) (u w1 w2:word),
   dist w1 w2 n -> dist (concat u w1) (concat u w2) n.
Proof.
simple induction u; simpl; intros.
auto.
apply dist_context; auto.
Qed.

Lemma dist_concat_both_right :
 forall (n:Z) (w1 w2:word),
   dist w1 w2 n -> forall u:word, dist (concat w1 u) (concat w2 u) n.
Proof.
simple induction 1; simpl; intros.
induction u; simpl.
exact dist_eps.
apply dist_context; assumption.
apply dist_add_left.
 exact (H1 u).
apply dist_add_right.
 exact (H1 u).
apply dist_context.
 exact (H1 u).
Qed.

Lemma dist'_is_dist :
 forall (w1 w2:word) (n:Z), dist' w1 w2 n -> dist w1 w2 n.
Proof.
simple induction 1.
exact dist_eps.
intros; apply dist_add_left; assumption.
intros; apply dist_add_right; assumption.
intros.
apply dist_concat_both_left.
apply dist_concat_both_right.
assumption.
Qed.
