
Require Import WhyArrays.
Require Import WhyPermut.

Ltac CallSubst x := subst x.

Ltac ArrayLength :=
  match goal with
  | h:(exchange _ _ _ _) |- _ =>
      (rewrite (exchange_length h); try omega) ||
        (rewrite <- (exchange_length h); try omega)
  | h:(sub_permut _ _ _ _) |- _ =>
      (rewrite (sub_permut_length h); try omega) ||
        (rewrite <- (sub_permut_length h); try omega)
  | h:(permut _ _ _ _) |- _ =>
      (rewrite (permut_length h); try omega) ||
        (rewrite <- (permut_length h); try omega)
  | _ => idtac
  end.

(* This tactic tries to prove (array_length t1)=(array_length t2)
   and introduces it in the context *)

Ltac ProveSameLength t1 t2 :=
  match goal with
  | |- (?X1 = ?X1) => reflexivity
  | h:(exchange t1 t2 _ _) |- _ =>
      exact (exchange_length h)
  | h:(sub_permut _ _ t1 t2) |- _ =>
      exact (sub_permut_length h)
  | h:(permut t1 t2) |- _ => exact (permut_length h)
  end.

Ltac ProveSameLengthSym t1 t2 :=
  ProveSameLength t1 t2 || (symmetry; ProveSameLength t2 t1).

Ltac SameLength t1 t2 :=
  assert (array_length t1 = array_length t2);
   [ ProveSameLengthSym t1 t2 | idtac ].

Ltac elimh p :=  match goal with
  | h:p |- _ => elim h
end.
