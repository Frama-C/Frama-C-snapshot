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
(* Relation between FOL and propositonal logic; Herbrand theorem.            *)
(*                                                                           *)
(* Copyright (c) 2003, John Harrison. (See "LICENSE.txt" for details.)       *)
(* ========================================================================= *)

(* ------------------------------------------------------------------------- *)
(* We want to generalize a formula before negating and refuting.             *)
(* ------------------------------------------------------------------------- *)

let generalize fm = itlist (fun x p -> Forall(x,p)) (fv fm) fm;;

(* ------------------------------------------------------------------------- *)
(* Propositional valuation.                                                  *)
(* ------------------------------------------------------------------------- *)

let pholds d fm = eval fm (fun p -> d(Atom p));;

(* ------------------------------------------------------------------------- *)
(* Characteristic function of Herbrand interpretation for p.                 *)
(* ------------------------------------------------------------------------- *)

let rec herbdom funcs tm =
  match tm with
    Var _ -> false
  | Fn(f,a) -> mem (f,length a) funcs & forall (herbdom funcs) a;;

(* ------------------------------------------------------------------------- *)
(* Get the constants for Herbrand base, adding nullary one if necessary.     *)
(* ------------------------------------------------------------------------- *)

let herbfuns fm =
  let cns,fns = partition (fun (_,ar) -> ar = 0) (functions fm) in
  if cns = [] then ["c",0],fns else cns,fns;;

(* ------------------------------------------------------------------------- *)
(* Enumeration of ground terms and m-tuples, ordered by total fns.           *)
(* ------------------------------------------------------------------------- *)

let rec groundterms cntms funcs n =
  if n = 0 then cntms else
  itlist (fun (f,m) -> (@)
                (map (fun args -> Fn(f,args))
                     (groundtuples cntms funcs (n - 1) m)))
         funcs []

and groundtuples cntms funcs n m =
  if m = 0 then if n = 0 then [[]] else [] else
  itlist (fun k -> (@)
                (allpairs (fun h t -> h::t)
                          (groundterms cntms funcs k)
                          (groundtuples cntms funcs (n - k) (m - 1))))
         (0 -- n) [];;

(* ------------------------------------------------------------------------- *)
(* Iterate modifier "mfn" over ground terms till "tfn" fails.                *)
(* ------------------------------------------------------------------------- *)

let rec herbloop mfn tfn fl0 cntms funcs fvs n fl tried tuples =
  print_string(string_of_int(length tried)^" ground instances tried; "^
               string_of_int(length fl)^" items in list");
  print_newline();
  match tuples with
    [] -> let newtups = groundtuples cntms funcs n (length fvs) in
          herbloop mfn tfn fl0 cntms funcs fvs (n + 1) fl tried newtups
  | tup::tups ->
          let fl' = mfn fl0 (formsubst(instantiate fvs tup)) fl in
          if not(tfn fl') then tup::tried else
          herbloop mfn tfn fl0 cntms funcs fvs n fl' (tup::tried) tups;;

(* ------------------------------------------------------------------------- *)
(* Hence a simple Gilmore-type procedure.                                    *)
(* ------------------------------------------------------------------------- *)

let gilmore_loop =
  let mfn djs0 ifn djs =
    filter (non contradictory) (distrib (smap (smap ifn) djs0) djs) in
  herbloop mfn (fun djs -> djs <> []);;

let gilmore fm =
  let sfm = skolemize(Not(generalize fm)) in
  let fvs = fv sfm and consts,funcs = herbfuns sfm in
  let cntms = smap (fun (c,_) -> Fn(c,[])) consts in
  length(gilmore_loop (simpdnf sfm) cntms funcs fvs 0 [[]] [] []);;

(* ------------------------------------------------------------------------- *)
(* First example and a little tracing.                                       *)
(* ------------------------------------------------------------------------- *)

START_INTERACTIVE;;
gilmore <<exists x. forall y. P(x) ==> P(y)>>;;

let sfm = skolemize(Not <<exists x. forall y. P(x) ==> P(y)>>);;

(* ------------------------------------------------------------------------- *)
(* Quick examples.                                                           *)
(* ------------------------------------------------------------------------- *)

let p19 = gilmore
 <<exists x. forall y z. (P(y) ==> Q(z)) ==> P(x) ==> Q(x)>>;;

let p24 = gilmore
 <<~(exists x. U(x) /\ Q(x)) /\
   (forall x. P(x) ==> Q(x) \/ R(x)) /\
   ~(exists x. P(x) ==> (exists x. Q(x))) /\
   (forall x. Q(x) /\ R(x) ==> U(x))
   ==> (exists x. P(x) /\ R(x))>>;;

let p39 = gilmore
 <<~(exists x. forall y. P(y,x) <=> ~P(y,y))>>;;

let p42 = gilmore
 <<~(exists y. forall x. P(x,y) <=> ~(exists z. P(x,z) /\ P(z,x)))>>;;

let p44 = gilmore
 <<(forall x. P(x) ==> (exists y. G(y) /\ H(x,y)) /\
   (exists y. G(y) /\ ~H(x,y))) /\
   (exists x. J(x) /\ (forall y. G(y) ==> H(x,y)))
   ==> (exists x. J(x) /\ ~P(x))>>;;

let p59 = gilmore
 <<(forall x. P(x) <=> ~P(f(x))) ==> (exists x. P(x) /\ ~P(f(x)))>>;;

(* ------------------------------------------------------------------------- *)
(* Slightly less easy examples.                                              *)
(* ------------------------------------------------------------------------- *)

let p45 = gilmore
 <<(forall x.
     P(x) /\ (forall y. G(y) /\ H(x,y) ==> J(x,y))
     ==> (forall y. G(y) /\ H(x,y) ==> R(y))) /\
   ~(exists y. L(y) /\ R(y)) /\
   (exists x. P(x) /\ (forall y. H(x,y) ==> L(y)) /\
                      (forall y. G(y) /\ H(x,y) ==> J(x,y)))
   ==> (exists x. P(x) /\ ~(exists y. G(y) /\ H(x,y)))>>;;

let p60 = gilmore
 <<forall x. P(x,f(x)) <=>
             exists y. (forall z. P(z,y) ==> P(z,f(x))) /\ P(x,y)>>;;

let p43 = gilmore
 <<(forall x y. Q(x,y) <=> forall z. P(z,x) <=> P(z,y))
   ==> forall x y. Q(x,y) <=> Q(y,x)>>;;

END_INTERACTIVE;;

(* ------------------------------------------------------------------------- *)
(* The Davis-Putnam procedure for first order logic.                         *)
(* ------------------------------------------------------------------------- *)

let clausal fm = smap (smap negate) (simpdnf(nnf(Not fm)));;

let dp_mfn cjs0 ifn cjs = union (smap (smap ifn) cjs0) cjs;;

let dp_loop = herbloop dp_mfn dpll;;

let davisputnam fm =
  let sfm = skolemize(Not(generalize fm)) in
  if sfm = False then 0
  else if sfm = True then failwith "davisputnam" else
  let fvs = fv sfm and consts,funcs = herbfuns sfm in
  let cntms = smap (fun (c,_) -> Fn(c,[])) consts in
  length(dp_loop (clausal sfm) cntms funcs fvs 0 [] [] []);;

(* ------------------------------------------------------------------------- *)
(* Show how much better than the Gilmore procedure this can be.              *)
(* ------------------------------------------------------------------------- *)

START_INTERACTIVE;;
let p20 = davisputnam
 <<(forall x y. exists z. forall w. P(x) /\ Q(y) ==> R(z) /\ U(w))
   ==> (exists x y. P(x) /\ Q(y)) ==> (exists z. R(z))>>;;
END_INTERACTIVE;;

(* ------------------------------------------------------------------------- *)
(* Show the sensitivity to order: try also with variant suggested.           *)
(* ------------------------------------------------------------------------- *)

let rec herbloop' mfn tfn fl0 cntms funcs fvs n fl tried tuples =
  print_string(string_of_int(length tried)^" ground instances tried; "^
               string_of_int(length fl)^" items in list");
  print_newline();
  match tuples with
    [] -> let newtups = rev(groundtuples cntms funcs n (length fvs)) in
          herbloop' mfn tfn fl0 cntms funcs fvs (n + 1) fl tried newtups
  | tup::tups ->
          let fl' = mfn fl0 (formsubst(instantiate fvs tup)) fl in
          if not(tfn fl') then tup::tried else
          herbloop' mfn tfn fl0 cntms funcs fvs n fl' (tup::tried) tups;;

let dp_loop' =
  herbloop' (fun cjs0 ifn cjs -> union (smap (smap ifn) cjs0) cjs) dpll;;

let davisputnam' fm =
  let sfm = skolemize(Not(generalize fm)) in
  if sfm = False then 0
  else if sfm = True then failwith "davisputnam" else
  let fvs = fv sfm and consts,funcs = herbfuns sfm in
  let cntms = smap (fun (c,_) -> Fn(c,[])) consts in
  length(dp_loop' (clausal sfm) cntms funcs fvs 0 [] [] []);;

START_INTERACTIVE;;
let p36 = davisputnam
 <<(forall x. exists y. P(x,y)) /\
   (forall x. exists y. G(x,y)) /\
   (forall x y. P(x,y) \/ G(x,y)
                ==> (forall z. P(y,z) \/ G(y,z) ==> H(x,z)))
   ==> (forall x. exists y. H(x,y))>>;;

let p36 = davisputnam'
 <<(forall x. exists y. P(x,y)) /\
   (forall x. exists y. G(x,y)) /\
   (forall x y. P(x,y) \/ G(x,y)
                ==> (forall z. P(y,z) \/ G(y,z) ==> H(x,z)))
   ==> (forall x. exists y. H(x,y))>>;;

let p29 = davisputnam
 <<(exists x. P(x)) /\ (exists x. G(x)) ==>
   ((forall x. P(x) ==> H(x)) /\ (forall x. G(x) ==> J(x)) <=>
    (forall x y. P(x) /\ G(y) ==> H(x) /\ J(y)))>>;;

let p29 = davisputnam'
 <<(exists x. P(x)) /\ (exists x. G(x)) ==>
   ((forall x. P(x) ==> H(x)) /\ (forall x. G(x) ==> J(x)) <=>
    (forall x y. P(x) /\ G(y) ==> H(x) /\ J(y)))>>;;
END_INTERACTIVE;;

(* ------------------------------------------------------------------------- *)
(* Try to cut out useless instantiations in final result.                    *)
(* ------------------------------------------------------------------------- *)

let rec dp_refine cjs0 fvs dunno need =
  match dunno with
    [] -> need
  | cl::dknow ->
      let mfn = dp_mfn cjs0 ** formsubst ** instantiate fvs in
      let need' =
       if dpll(itlist mfn (need @ dknow) []) then cl::need else need in
      dp_refine cjs0 fvs dknow need';;

let dp_refine_loop cjs0 cntms funcs fvs n cjs tried tuples =
  let tups = dp_loop cjs0 cntms funcs fvs n cjs tried tuples in
  dp_refine cjs0 fvs tups [];;

(* ------------------------------------------------------------------------- *)
(* Show how few of the instances we really need. Hence unification!          *)
(* ------------------------------------------------------------------------- *)

let davisputnam'' fm =
  let sfm = skolemize(Not(generalize fm)) in
  if sfm = False then 0
  else if sfm = True then failwith "davisputnam" else
  let fvs = fv sfm and consts,funcs = herbfuns sfm in
  let cntms = smap (fun (c,_) -> Fn(c,[])) consts in
  length(dp_refine_loop (clausal sfm) cntms funcs fvs 0 [] [] []);;

START_INTERACTIVE;;
let p36 = davisputnam''
 <<(forall x. exists y. P(x,y)) /\
   (forall x. exists y. G(x,y)) /\
   (forall x y. P(x,y) \/ G(x,y)
                ==> (forall z. P(y,z) \/ G(y,z) ==> H(x,z)))
   ==> (forall x. exists y. H(x,y))>>;;

let p29 = davisputnam''
 <<(exists x. P(x)) /\ (exists x. G(x)) ==>
   ((forall x. P(x) ==> H(x)) /\ (forall x. G(x) ==> J(x)) <=>
    (forall x y. P(x) /\ G(y) ==> H(x) /\ J(y)))>>;;
END_INTERACTIVE;;
