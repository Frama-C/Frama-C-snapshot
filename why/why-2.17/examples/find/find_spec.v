(* Load Programs. *)(**********************************************************************)
(*                                                                    *)
(* FIND, an historical example.                                       *)
(*                                                                    *)
(* The proof of this program was originally done by C. A. R. Hoare    *)
(* and fully detailed in the following paper:                         *)
(*                                                                    *)
(* C. A. R. Hoare, , Communications of the  *)
(* ACM, 14(1), 39--45, January 1971.                                  *)
(*                                                                    *)
(**********************************************************************)
(* Jean-Christophe FILLIATRE, February 98                             *)
(**********************************************************************)
(* find_spec.v                                                        *)
(**********************************************************************)

Require Export WhyArrays.

(* parameters = globals of the program *)

Parameter N : Z.
   (* size of the array *)
Parameter f : Z.
   (* the `pivot' *)

Axiom le_1_f : (1 <= f)%Z.
Axiom le_f_N : (f <= N)%Z.

(* Specification part *)

Definition n_invariant (n:Z) (A:array Z) :=
  (f <= n)%Z /\
  (forall p q:Z,
     (1 <= p)%Z ->
     (p <= n)%Z ->
     (n < q)%Z -> (q <= N)%Z -> (access A p <= access A q)%Z).

Definition m_invariant (m:Z) (A:array Z) :=
  (m <= f)%Z /\
  (forall p q:Z,
     (1 <= p)%Z ->
     (p < m)%Z ->
     (m <= q)%Z -> (q <= N)%Z -> (access A p <= access A q)%Z).

Definition i_invariant (m n i r:Z) (A:array Z) :=
  (m <= i)%Z /\
  (forall p:Z, (1 <= p)%Z -> (p < i)%Z -> (access A p <= r)%Z) /\
  ((i <= n)%Z ->
    exists p : Z, (i <= p)%Z /\ (p <= n)%Z /\ (r <= access A p)%Z).
  
Definition j_invariant (m n j r:Z) (A:array Z) :=
  (j <= n)%Z /\
  (forall q:Z, (j < q)%Z -> (q <= N)%Z -> (r <= access A q)%Z) /\
  ((m <= j)%Z ->
    exists q : Z, (m <= q)%Z /\ (q <= j)%Z /\ (access A q <= r)%Z).

Definition termination (i j i0 j0 r:Z) (A:array Z) :=
  (i > i0)%Z /\ (j < j0)%Z \/ (i <= f <= j)%Z /\ access A f = r.

Definition found (A:array Z) :=
  forall p q:Z,
    (1 <= p)%Z ->
    (p <= f)%Z ->
    (f <= q)%Z ->
    (q <= N)%Z ->
    (access A p <= access A f)%Z /\ (access A f <= access A q)%Z.

