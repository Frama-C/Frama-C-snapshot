Require Import Why.
Require Export caduceus_why.
Require Export List.
Require Export GenericLists.

(** * Paths *)

(** [(path t p1 l p2)] :
    there is a path from pointer [p1] to pointer [p2] using links in store [t],
    and the list of pointers along this path is [l]. *)
Inductive path (a: alloc_table) (l: memory pointer)(r: memory pointer) : 
pointer ->  pointer -> list pointer -> Prop :=
  | Path_null : forall p:pointer, path a l r p  p nil
  | Path_left :
      forall p1 p2:pointer,
      forall lp : list pointer,
        valid a p1 ->
          path a l r (acc l p1) p2 lp-> path a l r p1  p2 (p1::lp)
  | Path_right :
      	forall p1 p2:pointer,
      forall lp : list pointer ,
        	valid a p1 ->
        	  path a l r (acc r p1)  p2 lp -> path a l r p1 p2 (p1::lp). 


Lemma split_reachacle_1 : forall (a : alloc_table)
( l r : memory pointer) (path1: list pointer) (p1 p2 p : pointer) , 
path a l r p1 p2 (path1++ p :: nil) -> 
path a l r p1 p path1 /\ path a l r p p2 (p :: nil).
induction path1.
simpl; intuition.
inversion H; subst; constructor.
inversion H; subst; auto.
simpl; intros.
inversion H; subst.
generalize (IHpath1 (acc l a0) p2 p H5); intuition.
constructor 2; auto.
generalize (IHpath1 (acc r a0) p2 p H5); intuition.
constructor 3; auto.
Qed.

Lemma split_path : forall (a : alloc_table) (p1 p2 : pointer) 
( l r : memory pointer) (path2 path1: list pointer), 
path a l r p1 p2 (path1++path2) -> exists p3 : pointer , 
path a l r p1 p3 path1 /\ path a l r p3 p2 path2.
induction path2.
exists p2.
intuition.
rewrite <- app_nil_end in H.
auto.
constructor.
intros.
exists a0.
replace (path1++a0 :: path2) with ((path1++a0::nil)++path2) in H.
generalize (IHpath2 (path1++a0::nil) H).
intros (p3, (hp3a,hp3b)).
generalize (split_reachacle_1 a l r path1 p1 p3 a0 hp3a).
intuition.
inversion H2; subst.
constructor 2; auto.
inversion H6; subst; auto.
constructor 3; auto.
inversion H6; subst; auto.
rewrite app_ass; auto.
Qed.


Lemma path_no_cycle : forall (a : alloc_table) 
  (p1 p2 : pointer) (l r : memory pointer) 
  (pa : list pointer), path a l r p1 p2 pa -> 
  exists pa' :list pointer, 
  incl pa' pa /\ no_rep pa' /\ path a l r p1 p2 pa'. 
intros.
induction H.
exists (@nil pointer).
intuition.
simpl;auto.
constructor.
inversion IHpath.
case (In_dec eq_pointer_dec p1 x).
intro.
generalize (split_list p1 x i);intro.
inversion_clear H2;inversion_clear H1;inversion_clear H3.
exists (p1::x1).
intuition.
subst.
apply incl_cons.
left;auto.
red.
red in H2.
intros.
right.
apply H2.
apply in_or_app.
right;right;auto.
apply no_rep_sublist with x x0 ;subst;auto.
generalize (Path_left a l r p1 p2  x H H5).
subst x.
intro.
generalize (split_path a p1 p2 l r (p1::x1) (p1::x0) H1).
intros (p3, (Hp3a,Hp3b)).
inversion Hp3b;subst;auto.
intro.
inversion_clear H1.
inversion_clear H3.
exists (p1::x).
split.
apply incl_cons.
left;auto.
red.
red in H2.
intros.
right.
apply H2;auto.
split.
constructor;auto.
case (In_dec eq_pointer_dec p1 x);intuition.
constructor 2;auto.
inversion IHpath.
case (In_dec eq_pointer_dec p1 x).
intro.
generalize (split_list p1 x i);intro.
inversion_clear H2;inversion_clear H3.
exists (p1::x1).
inversion_clear H1.
split.
apply incl_cons.
left;auto.
red.
red in H3.
intros.
right.
apply H3.
subst.
apply in_or_app.
right;right;auto.
inversion_clear H4.
split.
apply no_rep_sublist with x x0 ;subst;auto.
generalize (Path_right a l r p1 p2  x H H5).
subst x.
intro.
generalize (split_path a p1 p2 l r (p1::x1) (p1::x0) H2).
intros (p3, (Hp3a,Hp3b)).
inversion Hp3b;subst;auto.
intro.
exists (p1::x).
inversion_clear H1;inversion_clear H3.
split.
apply incl_cons.
left;auto.
red.
red in H2.
intros.
right.
apply H2;auto.
split.
simpl.
case (In_dec eq_pointer_dec p1 x);intuition.
constructor 3;auto.
Qed.

Lemma path_in_list : forall (a:alloc_table)(l r: memory pointer)
(p1 p2 : pointer)(lp: list pointer), path a l r p1 p2 lp -> 
(exists lp' : list pointer, lp = p1::lp')  \/ lp = nil.
intros.
inversion H;subst.
right.
auto.
left.
exists lp0;auto.
left.
exists lp0;auto.
Qed.


Definition unmarked_reachable (a: alloc_table) 
  (m:memory Z) (l r: memory pointer) (p1 p2:pointer) 
  : Prop :=
  exists lp : list pointer, 
    (forall x : pointer, In x lp -> (acc m x) = 0 ) 
    /\ path a l r p1 p2 lp.

Definition clr_list (a: alloc_table)  (c:memory Z) (l: memory pointer)
(r: memory pointer) : pointer ->list pointer-> Prop :=
let next t := if Z_eq_dec (acc c t) 0 then (acc l t) else (acc r t) in
llist a next .

Inductive all_in_list (m:memory Z) : list pointer -> Prop :=
| all_in_list_nil : all_in_list m nil 
| all_in_list_cons : forall l : list pointer, forall t : pointer,
all_in_list m l -> (acc m t) <> 0 -> all_in_list m (cons t l).

Definition reachable (a: alloc_table) 
  (l: memory pointer)(r: memory pointer) 
  (p1 :pointer) (p2:pointer) : Prop :=
  exists lp : list pointer, path a l r p1 p2 lp. 

(* WF relation over graph *)

Record weight_type : Set := weight
  { wa : alloc_table;
    wm : memory Z;
    wc : memory Z;
    wl : memory pointer;
    wr : memory pointer;
    wp : pointer;
    wt : pointer
  }.

Definition reachable_elements (a:alloc_table) 
  (l r:memory pointer) (p t: pointer)
  (lp : list pointer) : Prop :=
  no_rep lp /\
  forall p2 : pointer, p2 <> null -> 
    (reachable a l r p p2 \/ reachable a l r t p2) 
    <-> In p2 lp.

Fixpoint mesure_mark (m : memory Z) (l : list pointer) {struct l} : nat :=
match l with
| nil => 0%nat
| p::l => plus ( if Z_eq_dec (acc m p) 0 then 1%nat else 0%nat) (mesure_mark m l)
end.

Lemma mesure_mark_app : 
  forall m : memory Z , 
  forall l1 l2 : list pointer,
  plus (mesure_mark m l1) (mesure_mark m l2) = 
  mesure_mark m (l1++l2).
Proof.
induction l1; simpl;auto.
case (Z_eq_dec (acc m a) 0); simpl; intuition.
Qed.

Lemma mesure_mark_eq : 
  forall m : memory Z,
  forall lp1 lp2 : list pointer, 
  (forall p : pointer, In p lp1 -> In p lp2) -> 
  (forall p : pointer, In p lp2 -> In p lp1) ->
  no_rep lp1 -> no_rep lp2 ->
  mesure_mark m lp1 = mesure_mark m lp2.
intros m lp1.
induction lp1.
destruct lp2.
auto.
assert (In p (p::lp2)).
  left;auto.
intros.
elim (H1 p H).
assert ( In a (a::lp1)).
  left;auto.
intros.
generalize (H0 a H).
intro.
generalize (split_list a lp2 H4). 
intros (lp2a,(lp2b,sub)).
subst.
generalize (mesure_mark_app m lp2a (a::lp2b)).
intro.
rewrite <- mesure_mark_app.
assert (mesure_mark m lp1 = mesure_mark m (lp2a++lp2b)).
apply IHlp1.
apply no_rep_in_app with a; auto .
apply no_rep_in_app2 with a; auto .
apply no_rep_sublist1 with a; auto.
apply (no_rep_sublist2 (lp2a ++ a :: lp2b) lp2a lp2b a H3).
auto.
simpl.
destruct (Z_eq_dec (acc m a)).
simpl.
rewrite <- plus_n_Sm.
rewrite H6.
apply (f_equal S).
rewrite mesure_mark_app; auto.
simpl.
rewrite H6.
rewrite mesure_mark_app; auto.
Qed.


Require Export Lexicographic_Product.
Require Import Relation_Operators.


Section lexico.

Variables A B : Set.

Variable Ra : A -> A -> Prop.
Variable Rb : B -> B -> Prop.

Definition lex := lexprod A (fun _:A => B) Ra (fun _:A => Rb).

Lemma lex_well_founded :
 well_founded Ra -> well_founded Rb -> well_founded lex.
Proof.
intros wfa wfb.
exact
 (wf_lexprod A (fun _:A => B) Ra (fun _:A => Rb) wfa (fun _ => wfb)).
Qed.

End lexico.
(*
Section LT_WF_REL.
Variable A : Set.

Variable X : Set.
Variable lt : X -> X -> Prop.
Variable F : A -> X -> Prop.
Definition inv_lt_rel x y :=
   exists2 n : _, F x n & (forall m, F y m -> lt n m).

Hypothesis lt_wf : well_founded lt.

Remark acc_lt_rel : forall x:A, (exists n : _, F x n) -> Acc inv_lt_rel x.
intros x [n fxn]; generalize x fxn; clear x fxn.
pattern n; apply (well_founded_induction_type lt_wf); intros.
constructor; intros.
inversion H0.
apply (H x1); auto.
Qed.

Theorem well_founded_inv_rel_compat : well_founded inv_lt_rel.
constructor; intros.
inversion H.
apply acc_lt_rel; trivial.
exists x; trivial.
Qed.

End LT_WF_REL.
*)
Definition lex_nat := lex _ _ lt lt.

Lemma lex_nat_well_founded : well_founded lex_nat.
exact (lex_well_founded _ _ lt lt lt_wf lt_wf).
Qed.

Definition interp_mark_m (e : weight_type) (n:nat): Prop :=
 exists lp : list pointer,
  reachable_elements e.(wa) e.(wl) e.(wr) e.(wp) 
    e.(wt) lp /\ (mesure_mark e.(wm) lp)=n.

Definition interp_mark_c  (e: weight_type) (n:nat): Prop :=
 exists lp : list pointer,
  reachable_elements e.(wa) e.(wl) e.(wr) e.(wp) e.(wt) lp /\ 
   (mesure_mark e.(wc) lp)=n.

Definition interp_stack (e : weight_type) (n:nat) : Prop :=
 exists stack : list pointer,
  clr_list e.(wa) e.(wc) e.(wl) e.(wr) e.(wp) stack /\ 
  (List.length stack)=n.

Definition natnat := {x : nat & nat}.

Definition interp_mark_c_and_stack (e : weight_type) 
  (p: natnat) : Prop :=
  let (n,m) := p in
    interp_mark_c e n /\ interp_stack e m.

Require Import Inverse_Image.

Definition inv_lt_rel (A:Set) B (lt : B -> B -> Prop) F (x:A) (y:A) :=
   exists2 n : B, F x n & (forall m, F y m -> lt n m).

Definition order_mark_c_and_stack : weight_type -> 
  weight_type -> Prop := 
  inv_lt_rel _ natnat lex_nat interp_mark_c_and_stack.

Lemma order_mark_c_and_stack_wf : 
  well_founded order_mark_c_and_stack.
Proof.
unfold order_mark_c_and_stack.
unfold inv_lt_rel.
apply wf_inverse_rel.
apply lex_nat_well_founded.
Qed.

Definition natnatnat := {x : nat & natnat}.

Definition interp_mark_m_and_c_and_stack (e : weight_type) (t: natnatnat) : Prop :=
  let (n,p) := t in
    interp_mark_m e n /\ interp_mark_c_and_stack e p.

Definition lex_natnatnat := lex _ _ lt lex_nat.

Lemma lex_natnatnat_well_founded : well_founded lex_natnatnat.
exact (lex_well_founded _ _ lt lex_nat lt_wf lex_nat_well_founded).
Qed.

Definition order_mark_m_and_c_and_stack : weight_type -> weight_type -> Prop := 
  inv_lt_rel _ natnatnat lex_natnatnat interp_mark_m_and_c_and_stack.


Lemma order_mark_m_and_c_and_stack_wf : 
  well_founded order_mark_m_and_c_and_stack.
Proof.
unfold order_mark_m_and_c_and_stack.
unfold inv_lt_rel.
apply wf_inverse_rel.
apply lex_natnatnat_well_founded.
Qed.

(* invariance of reachability by memory updates *)

Require Export caduceus_tactics.

Lemma path_upd_right : 
  forall (alloc : alloc_table) (l r : memory pointer)
    (p:pointer) (lp : list pointer) (p1 p0 p2:pointer), 
    ~ In p lp -> path alloc l r p1 p0 lp -> 
    path alloc l (upd r p p2) p1 p0 lp.
Proof.
induction lp.
intros p1 p0 p2 h1 h2; inversion_clear h2; constructor.
intros p1 p0 p2 h1 h2; inversion_clear h2.
constructor 2;auto.
apply IHlp; intuition.
constructor 3;auto.
assert (p <> a).
  apply not_in_cons_neq with lp; auto.
caduceus.
Qed.

Lemma path_upd_left : forall (alloc : alloc_table) 
  (l r : memory pointer) (p:pointer)
  (lp : list pointer) (p1 p0 p2 : pointer), 
  ~ In p lp -> path alloc l r p1 p0 lp -> 
  path alloc (upd l p p2) r p1 p0 lp.
Proof.
induction lp.
intros.
inversion_clear H0; constructor.
intros.
inversion_clear H0.
constructor 2;auto.
assert (p <> a).
  apply not_in_cons_neq with lp; auto.
caduceus.
constructor 3;auto.
apply IHlp; intuition.
Qed.

Lemma path_inv_upd_right:
  forall (alloc : alloc_table) 
  (l r : memory pointer) (p:pointer)
  (lp : list pointer) (p1 p0 p2 : pointer),  
  ~ In p lp -> path alloc l (upd r p p2) p1 p0 lp 
  -> path alloc l r p1 p0 lp.
Proof.
induction lp.
intros.
inversion_clear H0; constructor.
intros.
inversion_clear H0.
constructor 2;auto.
apply IHlp with p2; intuition.
constructor 3;auto.
rewrite acc_upd_neq in H2; auto.
apply IHlp with p2; intuition.
apply not_in_cons_neq with lp; auto.
Qed. 

Lemma path_inv_upd_left : 
  forall (alloc : alloc_table) (l r : memory pointer)
  (p:pointer)(lp : list pointer)(p1 p0 p2:pointer),  
  ~ In p lp ->path alloc (upd l p p2) r p1 p0 lp 
  -> path alloc l r p1 p0 lp.
Proof.
induction lp.
intros.
inversion_clear H0; constructor.
intros.
inversion_clear H0.
constructor 2;auto.
rewrite acc_upd_neq in H2; auto.
apply IHlp with p2; intuition.
apply not_in_cons_neq with lp; auto.
constructor 3;auto.
apply IHlp with p2; intuition.
Qed.

Lemma clr_list_upd_right :
  forall (a : alloc_table) 
  (c : memory Z) (l r : memory pointer)
  (stack : list pointer) (p t: pointer),
  ~In p stack -> 
  lpath a (fun u : pointer => 
      if Z_eq_dec (u # c) 0 then u # l else u # r)
    (p # r) stack null ->
  lpath a (fun u : pointer =>
      if Z_eq_dec (u # c) 0 then u # l 
      else u # (upd r p t)) 
    (p # r) stack null.
Proof.
intros alloc c l r stack p t h1 h2.
induction h2.
constructor.
assert (p<>p1).
  apply not_in_cons_neq with l0; auto.
constructor 2; auto.
caduceus.
Qed.


Lemma mesure_mark_upd : 
  forall (c:memory Z) (p:pointer) (lp:list pointer),
  ~In p lp -> 
    mesure_mark (upd c p 1) lp = mesure_mark c lp.
Proof.
induction lp.
auto.
intro.
assert (p<>a).
  apply not_in_cons_neq with lp; auto.
simpl.
rewrite acc_upd_neq;auto.
rewrite IHlp; auto.
intro;apply H;right;auto.
Qed.



