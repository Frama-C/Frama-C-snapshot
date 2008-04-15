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
(* The Davis-Putnam and Davis-Putnam-Loveland-Logemann procedures.           *)
(*                                                                           *)
(* Copyright (c) 2003, John Harrison. (See "LICENSE.txt" for details.)       *)
(* ========================================================================= *)

let clausal fm = defcnfs false fm;;

(* ------------------------------------------------------------------------- *)
(* Examples of clausal form.                                                 *)
(* ------------------------------------------------------------------------- *)

START_INTERACTIVE;;
let fm = <<p /\ (q <=> (~p <=> r))>>;;

clausal fm;;
END_INTERACTIVE;;

(* ------------------------------------------------------------------------- *)
(* The DP procedure.                                                         *)
(* ------------------------------------------------------------------------- *)

let one_literal_rule clauses =
  let ucl = hd (find (fun cl -> length cl = 1) clauses) in
  let ucl' = negate ucl in
  let clauses1 = filter (fun cl -> not (mem ucl cl)) clauses in
  smap (fun cl -> subtract cl [ucl']) clauses1;;

let affirmative_negative_rule clauses =
  let literals = itlist union clauses [] in
  let neglits,poslits = partition negative literals in
  let neglits' = smap negate neglits in
  let common = intersect poslits neglits' in
  let pos_only_lits = subtract poslits common
  and neg_only_lits = subtract neglits' common in
  let elim = union pos_only_lits (smap negate neg_only_lits) in
  if elim = [] then failwith "affirmative_negative_rule" else
  filter (fun cl -> intersect cl elim = []) clauses;;

let find_blowup cls l =
  let m = length(filter (mem l) cls)
  and n = length(filter (mem (negate l)) cls) in
  m * n - m - n,l;;

let resolution_rule clauses =
  let pvs = filter positive (itlist union clauses []) in
  let lblows = map (find_blowup clauses) pvs in
  let p = assoc (end_itlist min (map fst lblows)) lblows in
  let p' = negate p in
  let pos,notpos = partition (mem p) clauses in
  let neg,none = partition (mem p') notpos in
  let pos' = smap (filter (fun l -> l <> p)) pos
  and neg' = smap (filter (fun l -> l <> p')) neg in
  let res0 = allpairs union pos' neg' in
  union none (filter (non contradictory) res0);;

(* ------------------------------------------------------------------------- *)
(* Overall procedure.                                                        *)
(* ------------------------------------------------------------------------- *)

let rec dp clauses =
  if clauses = [] then true
  else if mem [] clauses then false else
  try dp(one_literal_rule clauses)
  with Failure _ -> try
      dp(affirmative_negative_rule clauses)
  with Failure _ ->
      dp(resolution_rule clauses);;

(* ------------------------------------------------------------------------- *)
(* Davis-Putnam satisfiability tester and tautology checker.                 *)
(* ------------------------------------------------------------------------- *)

let dpsat fm = dp(clausal fm);;

let dptaut fm = not(dpsat(Not fm));;

(* ------------------------------------------------------------------------- *)
(* Examples.                                                                 *)
(* ------------------------------------------------------------------------- *)

START_INTERACTIVE;;
tautology(prime 11);;

dptaut(prime 11);;
END_INTERACTIVE;;

(* ------------------------------------------------------------------------- *)
(* The same thing but with the DPLL procedure.                               *)
(* ------------------------------------------------------------------------- *)

let find_count cls l =
  let m = length(filter (mem l) cls)
  and n = length(filter (mem (negate l)) cls) in
  m + n,l;;

let rec dpll clauses =
  if clauses = [] then true
  else if mem [] clauses then false else
  try dpll(one_literal_rule clauses)
  with Failure _ -> try
      dpll(affirmative_negative_rule clauses)
  with Failure _ ->
    let pvs = filter positive (itlist union clauses []) in
    let lcounts = map (find_count clauses) pvs in
    let p = assoc (end_itlist max (map fst lcounts)) lcounts in
    dpll (insert [p] clauses) or
    dpll (insert [negate p] clauses);;

let dpllsat fm = dpll(clausal fm);;

let dplltaut fm = not(dpllsat(Not fm));;

(* ------------------------------------------------------------------------- *)
(* The same example.                                                         *)
(* ------------------------------------------------------------------------- *)

dplltaut(prime 11);;
