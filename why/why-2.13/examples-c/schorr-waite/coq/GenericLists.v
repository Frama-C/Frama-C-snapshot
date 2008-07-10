
Require Import Why.
Require Export caduceus_why.
Require Export List.

Section MoreList.
(** this should go in List stdlib*)

Lemma not_in_cons_neq :
  forall A:Set, forall x:A, forall y l,
  ~ In x (y :: l) -> x <> y.
Proof.
unfold not.
intros; apply H; left; auto.
Qed.

Implicit Arguments not_in_cons_neq.

Lemma In_app_cons :
 forall (A:Set) (l:list A) (x:A),
   In x l ->
     exists l1 : list A, exists l2 : list A, l = app l1 (cons x l2).
Proof.
simple induction l; simpl; intuition.
exists (nil (A:=A)); exists l0; simpl; subst; auto.
elim (H x H1); clear H; intros.
elim H; clear H; intuition.
exists (cons a x0); exists x1; simpl; subst; auto.
Qed.

Implicit Arguments In_app_cons.

End MoreList.


(** the Coq pointer list associated to a (finite) linked list *)

Parameter global:Set.

Definition pointer := (pointer global).

Definition plist := list pointer.

Definition in_list := (@In pointer).

(** * Paths *)

(** [(lpath t p1 l p2)] :
    there is a path from pointer [p1] to pointer [p2] using links in store [t],
    and the list of pointers along this path is [l]. *)

Inductive lpath (a: alloc_table) (next: pointer -> pointer) : pointer -> plist -> pointer -> Prop :=
  | Path_null : forall p:pointer, lpath a next p nil p
  | Path_cons :
      forall p1 p2:pointer,
        valid a p1 ->
        forall l:list pointer,
          lpath a next (next p1) l p2 -> lpath a next  p1 (cons p1 l) p2.

Hint Constructors lpath.

Lemma lpath_eq_fun : forall (a: alloc_table) (f g :pointer -> pointer) (p q: pointer) 
(l : list pointer),
 (forall p, In p l -> f p = g p) ->
 lpath a f p l q  -> lpath a g p l q
.
intros.
induction H0.
constructor.
constructor.
auto.
rewrite <-  H.
apply IHlpath.
intros.
apply H.
right;auto.
left;auto.
Qed.

(** * Lists *)

(** [(llist t p l)]: there is a (finite) linked list starting from pointer [p]
    using links in store [t], and this list of pointers is [l] *)

Definition llist (a: alloc_table) (next : pointer -> pointer) (p:pointer) (l:plist) :=
  lpath a next p l null.

Hint Unfold llist .

(** inductive characterization of [llist] (which could have been an
    inductive definition of [llist])  *)

Lemma llist_null :
 forall (a: alloc_table) (next : pointer -> pointer) (p:pointer), llist a next p nil -> p = null.
Proof.
unfold llist; inversion 1; trivial.
Qed.

Lemma llist_cons :
 forall (a: alloc_table) (next : pointer-> pointer) (p1 p2:pointer) (l:plist),
   llist a next p1 (cons p2 l) -> p1 = p2 /\ llist a next (next p2) l.
Proof.
unfold llist; inversion 1; intuition.
Qed.

(** invariance of a list when updating a cell outside of this list *)
(*
Lemma llist_pset_same :
 forall (a: alloc_table) (next : pointer -> pointer) (p:pointer) (l:plist),
   llist a next p l ->
   forall p1 p2:pointer,
     ~ In p1 l -> llist a (upd t p1 p2) p l.
Proof.
unfold llist; simple induction 1; intuition.
apply Path_cons; auto.
 rewrite acc_upd_neq; auto.
auto with *.
red; intro; apply H3; subst p0; auto with *.
Qed.
Hint Resolve llist_pset_same .
*)
(** [llist] is a function *)

Lemma llist_function :
 forall (a a': alloc_table) (next : pointer-> pointer) (l1 l2:plist) (p:pointer),
   llist a next p l1 -> llist a' next p l2 -> l1 = l2.
Proof.
Admitted.
(*
simple induction l1; intuition.
inversion H; subst.
inversion H0; intuition.
inversion H1; elim H6; auto.
inversion H0; subst.
inversion H1; subst.
inversion H0.
inversion H3; elim H9; auto.
apply (f_equal (cons a0)).
apply H with (next a0); auto.
Qed.
*)
Implicit Arguments llist_function.

Lemma llist_append :
 forall (a: alloc_table) (next : pointer -> pointer) (l1 l2:plist) (p:pointer),
   llist a next p (app l1 l2) ->
    exists p' : pointer, lpath a next p l1 p' /\ llist a next p' l2.
Proof.
simple induction l1; simpl; intuition.
exists p; auto.
inversion H0; subst.
elim (H l2 (next a0) H6); intuition.
exists x; auto.
Qed.

Implicit Arguments llist_append.

Lemma list_length_absurd :
 forall (A:Set) (l1 l2:list A), length l1 <> length l2 -> l1 <> l2.
Proof.
simple induction l1; simple induction l2; simpl; intuition.
discriminate H1.
discriminate H1.
injection H2; intros.
apply (H l0); intuition.
Qed.

Lemma length_app :
 forall (A:Set) (l1 l2:list A),
   length (app l1 l2) = (length l1 + length l2)%nat.
Proof.
simple induction l1; simpl; intuition.
Qed.

(** a list starting with [p] does not contain [p] in its remaining elements *)

Lemma llist_not_starting :
 forall (a: alloc_table) (next: pointer -> pointer) (p:pointer) (l:plist),
   llist a next (next p) l -> ~ In p l.
Proof.
red; intros.
elim (In_app_cons l p H0); intros.
elim H1; clear H1; intros.
subst l.
elim (llist_append x (cons p x0) H); intuition.
inversion H3; subst.
generalize (llist_function H8 H).
intro; apply (list_length_absurd _ x0 (app x (cons p x0))); auto.
rewrite length_app; simpl; omega.
Qed.

Lemma llist_no_rep: forall (a : alloc_table)(next : pointer->pointer)(p q : pointer) 
(l : list pointer) , llist a next p (q :: l) -> p=q /\ ~ In p l.
unfold llist.
intros.
inversion H.
subst.
split;auto.
apply llist_not_starting with a next.
auto.
Qed.

(** * Finite lists characterization *)

Inductive is_list (a: alloc_table) (next: pointer-> pointer) : pointer -> Prop :=
  | List_null : is_list a next null
  | List_cons :
      forall p:pointer,
        valid a p -> is_list a next (next p) -> is_list a next p.

Hint Constructors is_list.


Lemma is_list_llist :
 forall (a: alloc_table) (next: pointer-> pointer) (p:pointer),
   is_list a next p -> exists l : plist, llist a next p l.
Proof.
intros; elim H.
exists (nil (A:=pointer)); intuition.
intros; elim H2; intros.
exists (cons p0 x); intuition.
Qed.

Lemma llist_is_list :
 forall (a: alloc_table) (next: pointer-> pointer) (l:plist) (p:pointer),
   llist a next p l -> is_list a next p.
Proof.
simple induction l; intuition.
inversion H; auto.
inversion H0; intuition.
Qed.


(** * WF relation over linked lists *)

(*
Definition Length := ((memory pointer) * pointer)%type.
Definition length (a: alloc_table) 
   (t: memory pointer) (p:pointer) := (t, p).

Definition length_order (c1 c2: memory pointer * pointer) : Prop :=
  let (t1, p1) := c1 in
  let (t2, p2) := c2 in
    exists a : alloc_table,
    exists l1 : plist,
    exists l2 : plist,
    llist a (acc t1) p1 l1 /\ llist a (acc t2) p2 l2 /\ (List.length l1 < List.length l2)%nat.

Lemma length_order_wf : well_founded length_order.
Proof.
apply well_founded_inv_lt_rel_compat with
 (F := fun (x:Length) n =>
         let (t, p) := x in 
         exists a : alloc_table,
         exists l : plist, llist a (acc t) p l /\ List.length l = n).
unfold length_order, inv_lt_rel.
simple destruct x; simple destruct y; intuition.
elim H; clear H; intros a H; elim H; clear H; 
intros l1 H; elim H; clear H; intros l2 H.
intuition.
exists (List.length l1).
exists a; exists l1; intuition.
intuition.
elim H1; clear H1; intros a0 H1; elim H1; intros l2'; intuition.
generalize (llist_function H H4); intro; subst l2.
omega.
Qed.

Hint Resolve length_order_wf .

Lemma length_acc :
  forall a tl p,
  is_list a (acc tl) p -> p <> null ->
  length_order (length a tl (acc tl p)) (length a tl p).
Proof.
unfold length,length_order; intros.
exists a.
inversion H; intuition.
absurd (p = null); auto.
subst p0.
generalize (is_list_llist H2).
intros [l1 Hl1]; exists l1.
generalize (is_list_llist H).
intros [l2 Hl2]; exists l2.
intuition.
inversion Hl2.
absurd (p = null); auto.
assert (l1 = l).
apply llist_function with a a (acc tl) (acc tl p); auto.
subst; auto.
Qed.

Hint Resolve length_acc.
*)

(** * Disjointness *)

(** [disjoint l1 l2]: no common element between lists [l1] and [l2] *)

Definition disjoint (A:Set) (l1 l2:list A) : Prop :=
  (forall x:A, In x l1 -> ~ In x l2) /\
  (forall x:A, In x l2 -> ~ In x l1).
Implicit Arguments disjoint.

Section Disjointness.

Variable A : Set.
Variables l1 l2 : list A.
Variable x : A.

Lemma disjoint_cons :
 disjoint l1 (cons x l2) -> ~ In x l2 -> disjoint (cons x l1) l2.
Proof.
unfold disjoint; intuition.
elim (in_inv H); intuition.
subst x; intuition.
apply H1 with x0; intuition.
elim (in_inv H3); intuition.
subst x; intuition.
apply H1 with x0; intuition.
Qed.

Lemma disjoint_nil_l : disjoint nil l2.
Proof.
unfold disjoint; intuition.
Qed.

Lemma disjoint_l_nil : disjoint l1 nil.
Proof.
unfold disjoint; intuition.
Qed.

End Disjointness.
Hint Resolve disjoint_cons disjoint_nil_l disjoint_l_nil .

(** * Cyclicity *)

(*
Section Cyclicity.

Variable a : alloc_table.
Variable tl : pointer -> pointer.

Definition finite (p : pointer) :=
  exists r : plist, 
  forall p', 
  (exists lpp', lpath a tl p lpp' p') -> p'<>null -> valid a p' /\ In p' r.

Lemma finite_is_valid : forall p, p<>null -> finite p -> valid a p.
Proof.
unfold finite; intuition.
elim H0; clear H0; intuition.
assert (h : exists lpp' : plist, lpath a tl p lpp' p).
exists (@nil pointer); auto.
generalize (H0 p h); clear H0; intuition.
Save.

Definition cyclic (p : pointer) :=
  exists l1:plist, exists l2:plist, exists p',
  p'<>null /\ lpath a tl p l1 p' /\ lpath a tl p' l2 p'.

(** [cyclic] and [is_list] are exclusive *)
Lemma cyclic_is_list_exclusive : forall p,
  cyclic p -> is_list a tl p -> False.
Proof.
intros.
elim H; clear H; intros l1 H; elim H; clear H; intros l2 H;
elim H; clear H; intros p'; intuition.
elim (is_list_llist H0); clear H0; intros l0 Hl0.
unfold llist in Hl0.
Admitted.

(** when a list is finite, it is either [cyclic] or [is_list] *)
Lemma finite_cyclic_or_is_list : forall p, 
  finite p -> cyclic p \/ is_list a tl p.
Proof.
Admitted.

End Cyclicity.
*)
(*
Axiom eq_pointer_dec : forall p1 p2 : pointer, {p1 = p2} + {p1 <> p2}.
*)
Section NoRepetition.

Fixpoint no_rep (l:list pointer) {struct l}: Prop :=
match l with
| nil => True
| (a::l) => (*if In_dec eq_pointer_dec a l then False else True*)  ~ In a l /\ no_rep l
end.

Lemma split_list : 
  forall (A:Set)(x:A) l, In x l -> 
  exists l1, exists l2, l = l1 ++ x :: l2.
induction l; simpl; intuition.
exists (nil :list  A); exists l; subst; auto.
elim H; clear H; intros l1 H; elim H; clear H; intros l2 H.
exists (a :: l1); exists l2; subst; auto.
Qed.

Implicit Arguments split_list.

Lemma no_rep_sublist1 : 
 forall (l : list pointer)(p:pointer),
  no_rep (p::l) -> no_rep l. 
intros.
destruct H.
apply H0.
Qed.

Lemma no_rep_sublist : 
  forall (l l1 l2 : list pointer), 
  no_rep l -> l = l1 ++ l2 -> no_rep l2. 
induction l.
intros.
assert (l1++l2=nil);auto.
generalize (app_eq_nil l1 l2 H1).
intuition;subst;auto.
intros.
destruct l1.
assert (a::l = l2).
auto.
subst;auto.
apply IHl with l1.
apply no_rep_sublist1 with a;auto.
rewrite <-app_comm_cons in H0.
inversion H0.
auto.
Qed.

Ltac caseq t := generalize (refl_equal t);pattern t at -1;case t.

Lemma no_rep_p : forall (p1:pointer)(lp:list pointer), no_rep(p1::lp)->
~In p1 lp.
intros.
simpl in H.
intro.
inversion_clear H.
auto.
(*caseq (In_dec eq_pointer_dec p1 lp).
intros i e.
rewrite e in H1.
elim H1.
intros i.
elim (i H0).
*)
Qed.

Lemma no_rep_sublist2 : forall (l l1 l2 : list pointer) ( a : pointer), no_rep l -> 
l = l1 ++a::l2 -> no_rep (l1++l2). 
induction l.
intros.
destruct l1.
simpl in H0.
inversion H0.
inversion H0.
intros.
induction l1.
simpl in *|-*.
apply no_rep_sublist1 with a.
inversion H0;subst.
auto.
inversion H0.
subst.
unfold no_rep.
simpl.
elim H.
intros.
intuition.

(*
destruct (In_dec eq_pointer_dec a1 (l1 ++ a0::l2) ) .*)
inversion H1.
destruct (In_dec eq_pointer_dec a1 (l1 ++ l2)).
elim n.
elim (in_app_or _ _ _ i).
intro; apply in_or_app; auto.
intro; apply in_or_app; intuition.
intuition.
apply IHl with a0;auto.
Qed.

Lemma no_rep_sublist_bis : forall (l2 l l1  : list pointer), no_rep l -> 
l = l1 ++ l2 -> no_rep l1. 
induction l2.
intros.
subst.
rewrite <- app_nil_end in H.
auto.
intros.
subst.
assert ((l1 ++ a :: l2) = (l1 ++ a :: l2)).
auto.
generalize (no_rep_sublist2 (l1 ++ a :: l2) l1 l2 a H H0).
intro.
apply IHl2 with (l1++l2);auto.
Qed.

Lemma no_rep_false : forall (p1 : pointer)(lp : list pointer),
no_rep (p1 :: p1 :: lp) -> False.
intros.
simpl in H.
inversion_clear H.
casetype False.
destruct (In_dec eq_pointer_dec p1 (p1 :: lp));auto.
apply n.
left;auto.
Qed.

Lemma no_rep_false2 : forall (p1 : pointer)(lp1 lp2 : list pointer),
no_rep (p1 ::lp1 ++ p1 :: lp2) -> False.
intros.
simpl in H.
inversion_clear H.
destruct (In_dec eq_pointer_dec p1 (lp1 ++ p1 :: lp2)).
inversion H0.
apply n;clear n.
clear H1.
induction lp1.
simpl;left;auto.
right;auto.
Qed.

Lemma no_rep_p_bis : forall (p1:pointer)(lp1 lp2:list pointer), no_rep(lp1++p1::lp2)->
~In p1 lp1.
intros.
intro.
induction lp1.
inversion H0.
generalize (eq_pointer_dec a p1).
intros [p|p].
subst.
apply no_rep_false2 with p1 lp1 lp2;auto.
apply IHlp1.
apply no_rep_sublist1 with a;auto.
inversion H0.
elim (p H1).
auto.
Qed.


Lemma no_rep_in_app : 
  forall p : pointer, 
  forall lp1 lp2a lp2b : list pointer,
  no_rep (p::lp1) -> no_rep (lp2a++p::lp2b) ->
  (forall q : pointer, 
     In q (p::lp1) -> In q (lp2a++p::lp2b)) ->
  forall q : pointer, In q lp1 -> In q (lp2a ++ lp2b).
intros.
case (eq_pointer_dec q p).
intro.
subst.
generalize  (no_rep_p _ _ H).
intuition.
intro.
assert (In q (lp2a ++ p :: lp2b) -> In q (lp2a++lp2b)).
intros.
apply in_or_app.
generalize (in_app_or lp2a (p::lp2b) q H3) .
intros [H4|H4].
left;auto.
destruct H4.
elim n;auto.
right;auto.
apply H3.
apply H1.
right;auto.
Qed.

Lemma no_rep_sublist3 : forall (l l1 l2 : list pointer) ( a : pointer), no_rep l -> 
l = l1 ++a::l2 -> no_rep (a::l1++l2). 
induction l.
intros.
destruct l1.
simpl in H0.
inversion H0.
inversion H0.
intros.
destruct l1.
simpl in *|-*.
inversion H.
inversion H0;subst;auto.
inversion H0;subst.
rewrite <- app_comm_cons.
inversion H.
case (In_dec eq_pointer_dec p (a0 :: l1 ++ l2));intros.
destruct (In_dec eq_pointer_dec p (l1 ++ a0 :: l2));intros.
inversion H1.
elim n.
apply in_or_app.
rewrite app_comm_cons in i.
generalize (in_app_or (a0:: l1) l2 p i).
intuition.
generalize (in_inv H4).
intuition.
right;left;auto.
rewrite <- app_comm_cons in H0.
assert (l1 ++ a0 :: l2 = l1 ++ a0 :: l2).
auto.
generalize (IHl l1 l2 a0 H2 H3).
intro.
simpl.
case (In_dec eq_pointer_dec a0 (p :: l1 ++ l2));intro.
inversion i.
subst.
elim n.
left;auto.
inversion H4.
destruct (In_dec eq_pointer_dec a0 (l1 ++ l2)).
inversion H6.
elim (n0 H5).
split;auto.
split.
destruct (In_dec eq_pointer_dec p (l1 ++ l2)).
elim n.
right;auto.
auto.
apply no_rep_sublist1 with a0;auto.
Qed.

Lemma no_rep_in_app2 : 
  forall p : pointer, 
  forall lp1 lp2a lp2b : list pointer,
  no_rep (p::lp1) -> no_rep (lp2a++p::lp2b) ->
  (forall q : pointer, 
    In q (lp2a++p::lp2b) -> In q (p::lp1)) ->
  forall q : pointer, In q (lp2a ++ lp2b) -> In q lp1.
intros.
case (eq_pointer_dec q p).
intro.
subst.
assert (lp2a ++ p :: lp2b = lp2a ++ p :: lp2b).
auto.
generalize (no_rep_sublist2 
  (lp2a ++ p :: lp2b) lp2a lp2b p H0 H3);intro.
generalize (no_rep_sublist 
  (lp2a ++ p :: lp2b) lp2a (p::lp2b)  H0).
intros.
generalize (no_rep_sublist3 
  (lp2a ++ p :: lp2b) lp2a lp2b p H0 H3).
intro.
inversion H6.
destruct (In_dec eq_pointer_dec p (lp2a ++ lp2b)).
inversion H7.
elim ( n H2).
intro.
assert (In q (lp2a ++ p :: lp2b)).
generalize ( in_app_or lp2a lp2b q H2).
intuition.
generalize (H1 q H3);intro.
inversion H4.
elim (n ).
auto.
auto.
Qed.

End NoRepetition.



(* consecutives pairs in a list *)

Fixpoint pair_in_list (p1:pointer) (p2:pointer)(l:list pointer) {struct l} : Prop :=
  match l with
  | nil => False
  | b :: m => (b = p1 /\ match  m with
                                      | nil => False
                                      | c::n => c = p2
                                      end) 
                                      \/ pair_in_list p1 p2 m
  end.

Lemma pair_in_list_in :
  forall (l : plist) (p1 p2 p3: pointer),
    pair_in_list p1 p2 (p3::l) -> In p2 l. 
induction l.
simpl; tauto.
simpl; destruct l.
intros; subst; intuition.
intros; intuition.
subst; intuition.
right.
apply IHl with p1 null.
simpl; right; auto.
Qed.
