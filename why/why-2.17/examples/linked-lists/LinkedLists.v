(* Load Programs. *)
Require Import Why.
Require Export PolyList.

(** the Coq pointer list associated to a (finite) linked list *)

Definition plist := list pointer.


(** * Paths *)

(** [(lpath t p1 l p2)] :
    there is a path from pointer [p1] to pointer [p2] using links in store [t],
    and the list of pointers along this path is [l]. *)

Inductive lpath (t:pointer_store) :
pointer -> plist -> pointer -> Prop :=
  | Path_null : forall p:pointer, lpath t p nil p
  | Path_cons :
      forall p1 p2:pointer,
        is_valid_pointer t p1 ->
        forall l:list pointer,
          lpath t (pget t p1) l p2 -> lpath t p1 (cons p1 l) p2.

Hint lpath : core := Constructors lpath.


(** * Lists *)

(** [(llist t p l)]: there is a (finite) linked list starting from pointer [p]
    using links in store [t], and this list of pointers is [l] *)

Definition llist (t:pointer_store) (p:pointer) (l:plist) :=
  lpath t p l null.

Hints Unfold llist .

(** inductive characterization of [llist] (which could have been an
    inductive definition of [llist])  *)

Lemma llist_null :
 forall (t:pointer_store) (p:pointer), llist t p nil -> p = null.
Proof.
unfold llist; inversion 1; trivial.
Qed.

Lemma llist_cons :
 forall (t:pointer_store) (p1 p2:pointer) (l:plist),
   llist t p1 (cons p2 l) -> p1 = p2 /\ llist t (pget t p2) l.
Proof.
unfold llist; inversion 1; intuition.
Qed.

(** invariance of a list when updating a cell outside of this list *)

Lemma llist_pset_same :
 forall (t:pointer_store) (p:pointer) (l:plist),
   llist t p l ->
   forall p1 p2:pointer,
     is_valid_pointer t p1 -> ~ In p1 l -> llist (pset t p1 p2) p l.
Proof.
unfold llist; simple_induction 1; intuition.
apply Path_cons; auto.
 rewrite PointerStore.get_set_other; auto.
auto with *.
red; intro; apply H4; subst p0; auto with *.
Qed.
Hints Resolve llist_pset_same .

(** [llist] is a function *)

Lemma llist_function :
 forall (t:pointer_store) (l1 l2:plist) (p:pointer),
   llist t p l1 -> llist t p l2 -> l1 = l2.
Proof.
simple_induction l1; intuition.
inversion H; subst.
inversion H0; intuition.
inversion H1.
inversion H0; subst.
inversion H1; subst.
inversion H5.
apply (f_equal (cons a)).
apply H with (pget t a); auto.
Qed.

Lemma llist_append :
 forall (t:pointer_store) (l1 l2:plist) (p:pointer),
   llist t p (app l1 l2) ->
    EX p' : pointer | lpath t p l1 p' /\ llist t p' l2.
Proof.
simple_induction l1; simpl; intuition.
exists p; auto.
inversion H0; subst.
elim (H l2 (pget t a) H6); intuition.
exists x; auto.
Qed.

(** this should go in PolyList *)
Lemma In_app_cons :
 forall (A:Set) (l:list A) (x:A),
   In x l ->
    EX l1 : list A | ( EX l2 : list A | l = app l1 (cons x l2)).
Proof.
simple_induction l; simpl; intuition.
exists (nil (A:=A)); exists l0; simpl; subst; auto.
elim (H x H1); clear H; intros.
elim H; clear H; intuition.
exists (cons a x0); exists x1; simpl; subst; auto.
Qed.

Lemma list_length_absurd :
 forall (A:Set) (l1 l2:list A), length l1 <> length l2 -> l1 <> l2.
Proof.
simple_induction l1; simple_induction l2; simpl; intuition.
discriminate H1.
discriminate H1.
injection H2; intros.
apply (H l0); intuition.
Qed.

Lemma length_app :
 forall (A:Set) (l1 l2:list A),
   length (app l1 l2) = (length l1 + length l2)%N.
Proof.
simple_induction l1; simpl; intuition.
Qed.

(** a list starting with [p] does not contain [p] in its remaining elements *)

Lemma llist_not_starting :
 forall (t:pointer_store) (p:pointer) (l:plist),
   llist t (pget t p) l -> ~ In p l.
Proof.
red; intros.
elim (In_app_cons _ l p H0); intros.
elim H1; clear H1; intros.
subst l.
elim (llist_append t x (cons p x0) (pget t p) H); intuition.
inversion H3; subst.
generalize (llist_function t x0 (app x (cons p x0)) (pget t p) H8 H).
intro; apply (list_length_absurd _ x0 (app x (cons p x0))); auto.
rewrite length_app; simpl; omega.
Qed.


(** * Finite lists characterization *)

Inductive is_list (t:pointer_store) : pointer -> Prop :=
  | List_null : is_list t null
  | List_cons :
      forall p:pointer,
        is_valid_pointer t p -> is_list t (pget t p) -> is_list t p.

Hint is_list : core := Constructors is_list.

Lemma is_list_llist :
 forall (t:pointer_store) (p:pointer),
   is_list t p ->  EX l : plist | llist t p l.
Proof.
intros; simple_induction H.
exists (nil (A:=pointer)); intuition.
elim HrecH; intros.
exists (cons p x); intuition.
Qed.

Lemma llist_is_list :
 forall (t:pointer_store) (l:plist) (p:pointer),
   llist t p l -> is_list t p.
Proof.
simple_induction l; intuition.
inversion H; auto.
inversion H0; intuition.
Qed.


(** * WF relation over linked lists *)

Definition StorePointerPair := pointer_store * pointer.
Definition store_pointer_pair (t:pointer_store) (p:pointer) := (t, p).

Definition ll_order (c1 c2:pointer_store * pointer) : Prop :=
  let (t1, p1) := c1 in
  let (t2, p2) := c2 in
   EX l1 : plist
  | ( EX l2 : plist
     | llist t1 p1 l1 /\ llist t2 p2 l2 /\ (length l1 < length l2)%N).

Lemma ll_order_wf : well_founded ll_order.
Proof.
apply well_founded_inv_lt_rel_compat with
 (F := fun (x:StorePointerPair) n =>
         let (t, p) := x in  EX l : plist | llist t p l /\ length l = n).
unfold ll_order, inv_lt_rel.
simple_destruct x; simple_destruct y; intuition.
elim H; clear H; intros l1 H; elim H; clear H; intros l2 H.
intuition.
exists (length l1).
exists l1; intuition.
intuition.
elim H1; intros l2'; intuition.
generalize (llist_function _ _ _ _ H H4); intro; subst l2.
omega.
Qed.
Hints Resolve ll_order_wf .


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
Hints Resolve disjoint_cons disjoint_nil_l disjoint_l_nil .

