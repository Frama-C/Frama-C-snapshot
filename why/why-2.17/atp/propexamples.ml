(**************************************************************************)
(*                                                                        *)
(*  The Why platform for program certification                            *)
(*  Copyright (C) 2002-2008                                               *)
(*    Romain BARDOU                                                       *)
(*    Jean-François COUCHOT                                               *)
(*    Mehdi DOGGUY                                                        *)
(*    Jean-Christophe FILLIÂTRE                                           *)
(*    Thierry HUBERT                                                      *)
(*    Claude MARCHÉ                                                       *)
(*    Yannick MOY                                                         *)
(*    Christine PAULIN                                                    *)
(*    Yann RÉGIS-GIANAS                                                   *)
(*    Nicolas ROUSSET                                                     *)
(*    Xavier URBAIN                                                       *)
(*                                                                        *)
(*  This software is free software; you can redistribute it and/or        *)
(*  modify it under the terms of the GNU Library General Public           *)
(*  License version 2, with the special exception on linking              *)
(*  described in file LICENSE.                                            *)
(*                                                                        *)
(*  This software is distributed in the hope that it will be useful,      *)
(*  but WITHOUT ANY WARRANTY; without even the implied warranty of        *)
(*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                  *)
(*                                                                        *)
(**************************************************************************)

(* ========================================================================= *)
(* Some propositional formulas to test, and functions to generate classes.   *)
(*                                                                           *)
(* Copyright (c) 2003, John Harrison. (See "LICENSE.txt" for details.)       *)
(* ========================================================================= *)

START_INTERACTIVE;;

forall tautology
 [<<p ==> q <=> ~q ==> ~p>>;
  <<~ ~p <=> p>>;
  <<~(p ==> q) ==> q ==> p>>;
  <<~p ==> q <=> ~q ==> p>>;
  <<(p \/ q ==> p \/ r) ==> p \/ (q ==> r)>>;
  <<p \/ ~p>>;
  <<p \/ ~ ~ ~p>>;
  <<((p ==> q) ==> p) ==> p>>;
  <<(p \/ q) /\ (~p \/ q) /\ (p \/ ~q) ==> ~(~q \/ ~q)>>;
  <<(q ==> r) /\ (r ==> p /\ q) /\ (p ==> q /\ r) ==> (p <=> q)>>;
  <<p <=> p>>;
  <<((p <=> q) <=> r) <=> (p <=> (q <=> r))>>;
  <<p \/ q /\ r <=> (p \/ q) /\ (p \/ r)>>;
  <<(p <=> q) <=> (q \/ ~p) /\ (~q \/ p)>>;
  <<p ==> q <=> ~p \/ q>>;
  <<(p ==> q) \/ (q ==> p)>>;
  <<p /\ (q ==> r) ==> s <=> (~p \/ q \/ s) /\ (~p \/ ~r \/ s)>>];;

(* ------------------------------------------------------------------------- *)
(* Some graph-colouring examples.                                            *)
(* ------------------------------------------------------------------------- *)

let fm =
 <<(a1 \/ a2 \/ a3) /\
   (b1 \/ b2 \/ b3) /\
   (c1 \/ c2 \/ c3) /\
   (d1 \/ d2 \/ d3) /\
   ~(a1 /\ a2) /\ ~(a1 /\ a3) /\ ~(a2 /\ a3) /\
   ~(b1 /\ b2) /\ ~(b1 /\ b3) /\ ~(b2 /\ b3) /\
   ~(c1 /\ c2) /\ ~(c1 /\ c3) /\ ~(c2 /\ c3) /\
   ~(d1 /\ d2) /\ ~(d1 /\ d3) /\ ~(d2 /\ d3) /\
   ~(a1 /\ d1) /\ ~(a2 /\ d2) /\ ~(a3 /\ d3) /\
   ~(b1 /\ d1) /\ ~(b2 /\ d2) /\ ~(b3 /\ d3) /\
   ~(c1 /\ d1) /\ ~(c2 /\ d2) /\ ~(c3 /\ d3) /\
   ~(a1 /\ b1) /\ ~(a2 /\ b2) /\ ~(a3 /\ b3) /\
   ~(b1 /\ c1) /\ ~(b2 /\ c2) /\ ~(b3 /\ c3) /\
   ~(c1 /\ a1) /\ ~(c2 /\ a2) /\ ~(c3 /\ a3)>>;;

satisfiable fm;;

END_INTERACTIVE;;

(* ------------------------------------------------------------------------- *)
(* Generate assertion equivalent to R(s,t) <= n for the Ramsey number R(s,t) *)
(* ------------------------------------------------------------------------- *)

let var l =
  match l with
    [m;n] -> Atom(P("p_"^(string_of_int m)^"_"^(string_of_int n)))
  | _ -> failwith "var: expected 2-element list";;

let ramsey s t n =
  let vertices = 1 -- n in
  let yesgrps = map (allsets 2) (allsets s vertices)
  and nogrps = map (allsets 2) (allsets t vertices) in
  Or(list_disj (map (list_conj ** map var) yesgrps),
     list_disj (map (list_conj ** map (fun p -> Not(var p))) nogrps));;

(* ------------------------------------------------------------------------- *)
(* Some currently tractable examples.                                        *)
(* ------------------------------------------------------------------------- *)

START_INTERACTIVE;;

ramsey 3 3 4;;

tautology(ramsey 3 3 5);;

tautology(ramsey 3 3 6);;

END_INTERACTIVE;;

(* ------------------------------------------------------------------------- *)
(* Half adder.                                                               *)
(* ------------------------------------------------------------------------- *)

let halfsum x y = Iff(x,Not y);;

let halfcarry x y = And(x,y);;

let ha x y s c = And(Iff(s,halfsum x y),Iff(c,halfcarry x y));;

(* ------------------------------------------------------------------------- *)
(* Full adder.                                                               *)
(* ------------------------------------------------------------------------- *)

let carry x y z = Or(And(x,y),And(Or(x,y),z));;

let sum x y z = halfsum (halfsum x y) z;;

let fa x y z s c = And(Iff(s,sum x y z),Iff(c,carry x y z));;

(* ------------------------------------------------------------------------- *)
(* Useful idiom.                                                             *)
(* ------------------------------------------------------------------------- *)

let conjoin f l = list_conj (map f l);;

(* ------------------------------------------------------------------------- *)
(* n-bit ripple carry adder with carry c(0) propagated in and c(n) out.      *)
(* ------------------------------------------------------------------------- *)

let ripplecarry x y c out n =
  conjoin (fun i -> fa (x i) (y i) (c i) (out i) (c(i + 1)))
          (0 -- (n - 1));;

(* ------------------------------------------------------------------------- *)
(* Example.                                                                  *)
(* ------------------------------------------------------------------------- *)

let mk_index s i = Atom(P(s^"_"^(string_of_int i)));;

START_INTERACTIVE;;

let [x; y; out; c] = map mk_index ["X"; "Y"; "OUT"; "C"];;

ripplecarry x y c out 2;;

END_INTERACTIVE;;

(* ------------------------------------------------------------------------- *)
(* Special case with 0 instead of c(0).                                      *)
(* ------------------------------------------------------------------------- *)

let ripplecarry0 x y c out n =
  psimplify
   (ripplecarry x y (fun i -> if i = 0 then False else c i) out n);;

(* ------------------------------------------------------------------------- *)
(* Example.                                                                  *)
(* ------------------------------------------------------------------------- *)

START_INTERACTIVE;;

ripplecarry0 x y c out 2;;

END_INTERACTIVE;;

(* ------------------------------------------------------------------------- *)
(* Carry-select adder                                                        *)
(* ------------------------------------------------------------------------- *)

let ripplecarry1 x y c out n =
  psimplify
   (ripplecarry x y (fun i -> if i = 0 then True else c i) out n);;

let mux sel in0 in1 = Or(And(Not sel,in0),And(sel,in1));;

let offset n x i = x(n + i);;

let rec carryselect x y c0 c1 s0 s1 c s n k =
  let k' = min n k in
  let fm =
    And(And(ripplecarry0 x y c0 s0 k',ripplecarry1 x y c1 s1 k'),
        And(Iff(c k',mux (c 0) (c0 k') (c1 k')),
            conjoin (fun i -> Iff(s i,mux (c 0) (s0 i) (s1 i)))
                    (0 -- (k' - 1)))) in
  if k' < k then fm else
  And(fm,carryselect
            (offset k x) (offset k y) (offset k c0) (offset k c1)
            (offset k s0) (offset k s1) (offset k c) (offset k s)
            (n - k) k);;

(* ------------------------------------------------------------------------- *)
(* Equivalence problems for carry-select vs ripple carry adders.             *)
(* ------------------------------------------------------------------------- *)

let mk_adder_test n k =
  let [x; y; c; s; c0; s0; c1; s1; c2; s2] = map mk_index
      ["x"; "y"; "c"; "s"; "c0"; "s0"; "c1"; "s1"; "c2"; "s2"] in
  Imp(And(And(carryselect x y c0 c1 s0 s1 c s n k,Not(c 0)),
          ripplecarry0 x y c2 s2 n),
      And(Iff(c n,c2 n),
          conjoin (fun i -> Iff(s i,s2 i)) (0 -- (n - 1))));;

(* ------------------------------------------------------------------------- *)
(* Ripple carry stage that separates off the final result.                   *)
(*                                                                           *)
(*       UUUUUUUUUUUUUUUUUUUU  (u)                                           *)
(*    +  VVVVVVVVVVVVVVVVVVVV  (v)                                           *)
(*                                                                           *)
(*    = WWWWWWWWWWWWWWWWWWWW   (w)                                           *)
(*    +                     Z  (z)                                           *)
(* ------------------------------------------------------------------------- *)

let rippleshift u v c z w n =
  ripplecarry0 u v (fun i -> if i = n then w(n - 1) else c(i + 1))
                   (fun i -> if i = 0 then z else w(i - 1)) n;;

(* ------------------------------------------------------------------------- *)
(* Naive multiplier based on repeated ripple carry.                          *)
(* ------------------------------------------------------------------------- *)

let mult_rip x u v out n =
  if n = 1 then And(Iff(out 0,x 0 0),Not(out 1)) else
  psimplify
   (And(Iff(out 0,x 0 0),
        And(rippleshift
               (fun i -> if i = n - 1 then False else x 0 (i + 1))
               (x 1) (v 2) (out 1) (u 2) n,
            if n = 2 then And(Iff(out 2,u 2 0),Iff(out 3,u 2 1)) else
            conjoin (fun k -> rippleshift (u k) (x k) (v(k + 1)) (out k)
                                (if k = n - 1 then fun i -> out(n + i)
                                 else u(k + 1)) n) (2 -- (n - 1)))));;

(* ------------------------------------------------------------------------- *)
(* One stage in a sequential multiplier based on CSAs.                       *)
(*                                                                           *)
(*        UUUUUUUUUUUUUUU         (u)                                        *)
(*        VVVVVVVVVVVVVVV         (v)                                        *)
(*       XXXXXXXXXXXXXXX          (x)                                        *)
(*     ------------------------------------------------                      *)
(*       WWWWWWWWWWWWWWW          (w)                                        *)
(*       YYYYYYYYYYYYYYY          (y)                                        *)
(*                      Z         (z)                                        *)
(* ------------------------------------------------------------------------- *)

let csastage u v x z w y n =
  And(ha (u 0) (v 0) z (w 0),
      And(conjoin (fun i -> fa (u(i + 1)) (v(i + 1)) (x i)
                               (y i) (w(i + 1))) (0 -- (n - 2)),
          Iff(y(n - 1),x(n - 1))));;

(* ------------------------------------------------------------------------- *)
(* CSA-based multiplier only using ripple carry for last iteration.          *)
(* ------------------------------------------------------------------------- *)

let csa_init x u v out n =
  And(Iff(out 0,x 0 0),
      And(ha (x 0 1) (x 1 0) (out 1) (u 3 0),
          And(conjoin (fun i -> fa (x 0 (i + 2)) (x 1 (i + 1)) (x 2 i)
                                   (v 3 i) (u 3 (i + 1)))
                      (0 -- (n - 3)),
              And(ha (x 1 (n - 1)) (x 2 (n - 2))
                     (v 3 (n - 2)) (u 3 (n - 1)),
                  Iff(v 3 (n - 1),x 2 (n - 1))))));;

let mult_csa x u v out n =
  And(csa_init x u v out n,
      And(conjoin (fun i -> csastage (u i) (v i) (x i)
                                 (out(i - 1)) (u(i + 1)) (v(i + 1)) n)
                  (3 -- (n - 1)),
          rippleshift (u n) (v n) (v(n + 1))
                      (out(n - 1)) (fun i -> out(n + i)) n));;

(* ------------------------------------------------------------------------- *)
(* Equivalence for ripple vs CSA sequential multipler.                       *)
(* ------------------------------------------------------------------------- *)

let mk_index2 x i j =
  Atom(P(x^"_"^(string_of_int i)^"_"^(string_of_int j)));;

let mk_mult_test n =
  let [x; y; out; out'] = map mk_index ["x"; "y"; "**"; "p"] in
  let m i j = And(x i,y j)
  and [u; v; u'; v'] = map mk_index2 ["u"; "v"; "w"; "z"] in
  Imp(And(mult_rip m u v out n,
          mult_csa m u' v' out' n),
      conjoin (fun i -> Iff(out i,out' i)) (0 -- (n - 1)));;

(* ------------------------------------------------------------------------- *)
(* Primality examples (using CSAs since they are slightly shorter).          *)
(* For large examples, should use "num" instead of "int" in these functions. *)
(* ------------------------------------------------------------------------- *)

let rec bitlength x = if x = 0 then 0 else 1 + bitlength (x / 2);;

let rec bit n x = if n = 0 then x mod 2 = 1 else bit (n - 1) (x / 2);;

let congruent_to x m n =
  conjoin (fun i -> if bit i m then x i else Not(x i))
          (0 -- (n - 1));;

let prime p =
  let [x; y; out] = map mk_index ["x"; "y"; "out"] in
  let m i j = And(x i,y j)
  and [u; v] = map mk_index2 ["u"; "v"] in
  let n = bitlength p in
  Not(And(mult_rip m u v out (n - 1),
      congruent_to out p (max n (2 * n - 2))));;
