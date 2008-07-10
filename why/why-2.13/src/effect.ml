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
(*  modify it under the terms of the GNU General Public                   *)
(*  License version 2, as published by the Free Software Foundation.      *)
(*                                                                        *)
(*  This software is distributed in the hope that it will be useful,      *)
(*  but WITHOUT ANY WARRANTY; without even the implied warranty of        *)
(*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                  *)
(*                                                                        *)
(*  See the GNU General Public License version 2 for more details         *)
(*  (enclosed in the file GPL).                                           *)
(*                                                                        *)
(**************************************************************************)

(*i $Id: effect.ml,v 1.25 2008/04/15 08:12:50 regisgia Exp $ i*)

(*s Effects. *)

open Ident

(*s An effect is composed of three sets of variables.

    The first one is the set of all variables (the input),
    the second one is the set of possibly modified variables (the output),
    and the third one the set of possibly raised exceptions.
 
    INVARIANTS: 
    1. there are no duplicate elements in each list 
    2. output is contained in input

    REMARK: for most operations, sets will be more relevant than lists,
    but order must not change when a substitution is applied and thus
    lists are preferred *)

type t = { 
  input : Ident.set;
  output : Ident.set;
  exns : Ident.set;
  nt : bool
}

(*s the empty effect *)

let bottom = { input = Idset.empty; output = Idset.empty; exns = Idset.empty; nt = false }

(*s basic operations *)

let list_add x l = if List.mem x l then l else x :: l

let list_remove x l = 
  let rec rem_rec = function
    | [] -> []
    | y :: l -> if x = y then l else y :: rem_rec l
  in
  if List.mem x l then rem_rec l else l

let mem x (r,w) = (List.mem x r) || (List.mem x w)

(* [list_union] is a merge sort *)
let list_union l1 l2 = 
  let rec basic_union = function
    | [], s2 -> s2
    | s1, [] -> s1
    | ((v1 :: l1) as s1), ((v2 :: l2) as s2) ->
	if v1 < v2 then
	  v1 :: basic_union (l1,s2)
	else if v1 > v2 then
	  v2 :: basic_union (s1,l2)
	else
	  v1 :: basic_union (l1,l2)
  in
  basic_union (List.sort compare l1, List.sort compare l2)

(*s adds reads and writes variables *)

let add_read x e = { e with input = Idset.add x e.input }

let add_reads = Idset.fold add_read

let add_write x e = 
  { e with input = Idset.add x e.input; output = Idset.add x e.output }

let add_writes = Idset.fold add_write

let add_exn x e = { e with exns = Idset.add x e.exns }

let add_exns = Idset.fold add_exn

let add_nontermination e = { e with nt = true }

(*s access *)

let get_reads e = Idset.elements e.input
let get_writes e = Idset.elements e.output
let get_exns e = Idset.elements e.exns
let get_repr e = 
  Idset.elements e.input, Idset.elements e.output, Idset.elements e.exns, e.nt

(*s tests *)

let is_read  e id = Idset.mem id e.input
let is_write e id = Idset.mem id e.output
let is_exn e id = Idset.mem id e.exns
let is_nonterminating e = e.nt


let keep_writes e = { e with input = e.output }

(*s union and disjunction *)

let union e1 e2 =
  { input = Idset.union e1.input e2.input;
    output = Idset.union e1.output e2.output;
    exns = Idset.union e1.exns e2.exns;
    nt = e1.nt || e2.nt
  }

(*s comparison relation *)

let le e1 e2 = failwith "effects: le: not yet implemented"

let inf e1 e2 = failwith "effects: inf: not yet implemented"

(*s remove *)

let remove x e = 
  { e with 
      input = Idset.remove x e.input;
      output = Idset.remove x e.output }

let remove_exn x e = { e with exns = Idset.remove x e.exns }

let remove_nontermination e = { e with nt = false }

let erase_exns e = { e with exns = Idset.empty }

(*s occurrence *)

let occur x e = Idset.mem x e.input || Idset.mem x e.output

(*s substitution *)

(***
let list_subst x x' l =
  let rec subst = function
    | [] -> []
    | y :: r -> if y = x then x' :: r else y :: subst r
  in
  if List.mem x l then subst l else l

let subst_one x x' e = 
  { e with input = list_subst x x' e.input; output = list_subst x x' e.output }

let subst = Idmap.fold subst_one
***)

let subst subst e =
  let subst_set s =
    Idset.fold (fun x acc -> 
		  let x' = try Idmap.find x subst with Not_found -> x in
		  Idset.add x' acc)
      s Idset.empty
  in
  { e with input = subst_set e.input; output = subst_set e.output }

(*s pretty-print *)

open Format

(* copied to avoid circularity Effect <-> Misc *)
let rec print_list sep print fmt = function
  | [] -> ()
  | [x] -> print fmt x
  | x :: r -> print fmt x; sep fmt (); print_list sep print fmt r

let print fmt { input = r; output = w; exns = e; nt = nt } =
  let r = Idset.elements r in
  let w = Idset.elements w in
  fprintf fmt "@[";
  if r <> [] then begin
    fprintf fmt "reads ";
    print_list (fun fmt () -> fprintf fmt ",@ ") Ident.print fmt r;
  end;
  if r <> [] && w <> [] then fprintf fmt "@ ";
  if w <> [] then begin
    fprintf fmt "writes ";
    print_list (fun fmt () -> fprintf fmt ",@ ") Ident.print fmt w;
  end;
  let e = Idset.elements e in
  if (r <> [] || w <> []) && e <> [] then fprintf fmt "@ ";
  if e <> [] then begin
    fprintf fmt "raises ";
    print_list (fun fmt () -> fprintf fmt ",@ ") Ident.print fmt e;
  end;
  if r <> [] || w <> [] || e <> [] then fprintf fmt "@ ";
  if nt then fprintf fmt ",@ nt";
  fprintf fmt "@]"

