
Require WhyArrays.
Require WhyPermut.

Tactic Definition CallSubst x := Subst x.

Tactic Definition ArrayLength :=
  Match Context With
  | [ h: (exchange ? ? ? ?) |- ? ] ->
    (Rewrite (exchange_length h); Try Omega) Orelse 
    (Rewrite <- (exchange_length h); Try Omega)
  | [ h: (sub_permut ? ? ? ?) |- ? ] ->
    (Rewrite (sub_permut_length h); Try Omega) Orelse 
    (Rewrite <- (sub_permut_length h); Try Omega)
  | [ h: (permut ? ? ? ?) |- ? ] ->
    (Rewrite (permut_length h); Try Omega) Orelse 
    (Rewrite <- (permut_length h); Try Omega)
  | _ -> 
    Idtac.

(* This tactic tries to prove (array_length t1)=(array_length t2)
   and introduces it in the context *)

Tactic Definition ProveSameLength t1 t2 :=
  Match Context With
  | [ |- (eq ? ?1 ?1) ] ->
      Reflexivity
  | [ h: (exchange t1 t2 ? ?) |- ? ] ->
      Exact (exchange_length h)
  | [ h: (sub_permut ? ? t1 t2) |- ? ] ->
      Exact (sub_permut_length h)
  | [ h: (permut t1 t2) |- ? ] ->
      Exact (permut_length h)
.

Tactic Definition ProveSameLengthSym t1 t2 :=
  ProveSameLength t1 t2 Orelse (Symmetry; ProveSameLength t2 t1).

Tactic Definition SameLength t1 t2 :=
  Assert (array_length t1)=(array_length t2);
  [ ProveSameLengthSym t1 t2 | Idtac ].

