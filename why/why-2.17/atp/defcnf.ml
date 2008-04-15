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
(* Definitional CNF.                                                         *)
(*                                                                           *)
(* Copyright (c) 2003, John Harrison. (See "LICENSE.txt" for details.)       *)
(* ========================================================================= *)

(* ------------------------------------------------------------------------- *)
(* Variant of NNF without splitting equivalences.                            *)
(* ------------------------------------------------------------------------- *)

let rec nenf fm =
  match fm with
    Not(Not p) -> nenf p
  | Not(And(p,q)) -> Or(nenf(Not p),nenf(Not q))
  | Not(Or(p,q)) -> And(nenf(Not p),nenf(Not q))
  | Not(Imp(p,q)) -> And(nenf p,nenf(Not q))
  | Not(Iff(p,q)) -> Iff(nenf p,nenf(Not q))
  | And(p,q) -> And(nenf p,nenf q)
  | Or(p,q) -> Or(nenf p,nenf q)
  | Imp(p,q) -> Or(nenf(Not p),nenf q)
  | Iff(p,q) -> Iff(nenf p,nenf q)
  | _ -> fm;;

(* ------------------------------------------------------------------------- *)
(* Make a stylized variable and update the index.                            *)
(* ------------------------------------------------------------------------- *)

let mkprop n = Atom(P("p_"^(string_of_num n))),n +/ Int 1;;

(* ------------------------------------------------------------------------- *)
(* Make n large enough that "v_m" won't clash with s for any m >= n          *)
(* ------------------------------------------------------------------------- *)

let max_varindex pfx =
  let m = String.length pfx in
  fun s n ->
    let l = String.length s in
    if l <= m or String.sub s 0 m <> pfx then n else
    let s' = String.sub s m (l - m) in
    if forall numeric (explode s') then max_num n (num_of_string s')
    else n;;

(* ------------------------------------------------------------------------- *)
(* Basic definitional CNF procedure.                                         *)
(* ------------------------------------------------------------------------- *)

let rec maincnf (fm,defs,n as trip) =
  match fm with
    And(p,q) -> defstep (fun (p,q) -> And(p,q)) (p,q) trip
  | Or(p,q) -> defstep (fun (p,q) -> Or(p,q)) (p,q) trip
  | Iff(p,q) -> defstep (fun (p,q) -> Iff(p,q)) (p,q) trip
  | _ -> trip

and defstep op (p,q) (fm,defs,n) =
  let fm1,defs1,n1 = maincnf (p,defs,n) in
  let fm2,defs2,n2 = maincnf (q,defs1,n1) in
  let fm' = op(fm1,fm2) in
  try (fst(apply defs2 fm'),defs2,n2) with Failure _ ->
  let v,n3 = mkprop n2 in (v,(fm'|->(v,Iff(v,fm'))) defs2,n3);;

let defcnf fm =
  let fm' = nenf(psimplify fm) in
  let n = Int 1 +/ overatoms (max_varindex "p_" ** pname) fm' (Int 0) in
  let (fm'',defs,_) = maincnf (fm',undefined,n) in
  let deflist = map (snd ** snd) (funset defs) in
  let subcnfs = itlist ((@) ** simpcnf) deflist (simpcnf fm'') in
  list_conj (map list_disj (setify subcnfs));;

(* ------------------------------------------------------------------------- *)
(* Example.                                                                  *)
(* ------------------------------------------------------------------------- *)

START_INTERACTIVE;;
let fm = <<(p \/ (q /\ ~r)) /\ s>>;;

defcnf fm;;

cnf fm;;

cnf <<p <=> (q <=> r)>>;;
END_INTERACTIVE;;

(* ------------------------------------------------------------------------- *)
(* Version tweaked to exploit initial structure.                             *)
(* ------------------------------------------------------------------------- *)

let subcnf sfn op (p,q) (fm,defs,n) =
  let fm1,defs1,n1 = sfn(p,defs,n) in
  let fm2,defs2,n2 = sfn(q,defs1,n1) in (op(fm1,fm2),defs2,n2);;

let rec orcnf (fm,defs,n as trip) =
  match fm with
    Or(p,q) -> subcnf orcnf (fun (p,q) -> Or(p,q)) (p,q) trip
  | _ -> maincnf trip;;

let rec andcnf (fm,defs,n as trip) =
  match fm with
    And(p,q) -> subcnf andcnf (fun (p,q) -> And(p,q)) (p,q) trip
  | _ -> orcnf trip;;

let defcnfs fm =
  let fm' = nenf(psimplify fm) in
  let n = Int 1 +/ overatoms (max_varindex "p_" ** pname) fm' (Int 0) in
  let (fm'',defs,_) = andcnf (fm',undefined,n) in
  let deflist = map (snd ** snd) (funset defs) in
  setify(itlist ((@) ** simpcnf) deflist (simpcnf fm''));;

let defcnf fm = list_conj (map list_disj (defcnfs fm));;

(* ------------------------------------------------------------------------- *)
(* Examples.                                                                 *)
(* ------------------------------------------------------------------------- *)

START_INTERACTIVE;;
defcnf fm;;
END_INTERACTIVE;;

(* ------------------------------------------------------------------------- *)
(* Version using only implication where possible.                            *)
(* ------------------------------------------------------------------------- *)

let defstep pos sfn op (p,q) (fm,defs,n) =
  let fm1,defs1,n1 = sfn (p,defs,n) in
  let fm2,defs2,n2 = sfn (q,defs1,n1) in
  let (fl,fm' as ffm') = (pos,op(fm1,fm2)) in
  try (fst(apply defs2 ffm'),defs2,n2) with Failure _ ->
  let (v,n3) = mkprop n2 in
  let cons = if pos then fun (p,q) -> Imp(p,q)
             else fun (p,q) -> Iff(p,q) in
  (v,(ffm' |-> (v,cons(v,fm'))) defs2,n3);;

let rec maincnf pos (fm,defs,n as trip) =
  match fm with
    And(p,q) ->
        defstep pos (maincnf pos) (fun (p,q) -> And(p,q)) (p,q) trip
  | Or(p,q) ->
        defstep pos (maincnf pos) (fun (p,q) -> Or(p,q)) (p,q) trip
  | Iff(p,q) ->
        defstep pos (maincnf false) (fun (p,q) -> Iff(p,q)) (p,q) trip
  | _ -> trip;;

let rec orcnf pos (fm,defs,n as trip) =
  match fm with
    Or(p,q) -> subcnf (orcnf pos) (fun (p,q) -> Or(p,q)) (p,q) trip
  | _ -> maincnf pos trip;;

let rec andcnf pos (fm,defs,n as trip) =
  match fm with
    And(p,q) -> subcnf (andcnf pos) (fun (p,q) -> And(p,q)) (p,q) trip
  | _ -> orcnf pos trip;;

let defcnfs imps fm =
  let fm' = nenf(psimplify fm) in
  let n = Int 1 +/ overatoms (max_varindex "p_" ** pname) fm' (Int 0) in
  let (fm'',defs,_) = andcnf imps (fm',undefined,n) in
  let deflist = map (snd ** snd) (funset defs) in
  setify(itlist ((@) ** simpcnf) deflist (simpcnf fm''));;

let defcnf imps fm = list_conj (map list_disj (defcnfs imps fm));;

(* ------------------------------------------------------------------------- *)
(* Example.                                                                  *)
(* ------------------------------------------------------------------------- *)

START_INTERACTIVE;;
defcnf false fm;;

defcnf true fm;;
END_INTERACTIVE;;

(* ------------------------------------------------------------------------- *)
(* Version that guarantees 3-CNF.                                            *)
(* ------------------------------------------------------------------------- *)

let rec andcnf3 pos (fm,defs,n as trip) =
  match fm with
    And(p,q) -> subcnf (andcnf3 pos) (fun (p,q) -> And(p,q)) (p,q) trip
  | _ -> maincnf pos trip;;

let defcnf3s imps fm =
  let fm' = nenf(psimplify fm) in
  let n = Int 1 +/ overatoms (max_varindex "p_" ** pname) fm' (Int 0) in
  let (fm'',defs,_) = andcnf3 imps (fm',undefined,n) in
  let deflist = map (snd ** snd) (funset defs) in
  setify(itlist ((@) ** simpcnf) deflist (simpcnf fm''));;

let defcnf3 imps fm = list_conj (map list_disj (defcnf3s imps fm));;

(* ------------------------------------------------------------------------- *)
(* Example.                                                                  *)
(* ------------------------------------------------------------------------- *)

START_INTERACTIVE;;
let fm = <<(p \/ q \/ r \/ s) /\ ~p /\ (~p \/ q)>>;;

defcnf true fm;;

defcnf3 true fm;;
END_INTERACTIVE;;
