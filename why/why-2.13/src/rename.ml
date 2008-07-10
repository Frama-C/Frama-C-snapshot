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

(*i $Id: rename.ml,v 1.14 2008/02/05 12:10:50 marche Exp $ i*)

open Ident
open Misc
open Report
open Error

(* Variables names management *)

type date = string

(* The following data structure keeps the successive names of the variables
 * as we traverse the program. A each step a ``date'' and a
 * collection of new names is (possibly) given, and updates the
 * previous renaming.
 * 
 * Then, we can ask for the name of a variable, at current date or
 * at a given date.
 *
 * It is easily represented by a list of date x assoc list, most recent coming 
 * first i.e. as follows:
 *
 *   [ date (= current), [ (x,xi); ... ];
 *     date            , [ (z,zk); ... ];
 *     ...
 *     date (= initial), [ (x,xj); (y,yi); ... ]
 *
 * We also keep a list of all names already introduced, in order to
 * quickly get fresh names.
 *)

type t =
  { levels : (date * (Ident.t * Ident.t) list) list;
    avoid  : Ident.set;
    cpt    : int }
    

let empty_ren = { levels = []; avoid = Idset.empty; cpt = 0 }

let update r d ids =
  let al,av = renaming_of_ids r.avoid ids in
  { levels = (d,al) :: r.levels; avoid = av; cpt = r.cpt }

let push_date r d = update r d []

let next r ids =
  let al,av = renaming_of_ids r.avoid ids in
  let n = succ r.cpt in
  let d = string_of_int n in
  { levels = (d,al) :: r.levels; avoid = av; cpt = n }


let find r x =
  let rec find_in_one = function
    | []           -> raise Not_found
    | (y,v) :: rem -> if y = x then v else find_in_one rem
  in
  let rec find_in_all = function
    | []           -> raise Not_found
    | (_,l) :: rem -> try find_in_one l with Not_found -> find_in_all rem
  in
  find_in_all r.levels


let current_var = find

let current_vars r ids = List.map (fun id -> id,current_var r id) ids


let avoid r ids = 
  { levels = r.levels; avoid = Idset.union r.avoid ids; cpt = r.cpt }

let fresh r id =
  match renaming_of_ids r.avoid [id] with
    | [_,id'], av -> id', { r with avoid = av }
    | _ -> assert false

let fresh_many r ids = 
  let ids',av = renaming_of_ids r.avoid ids in
  ids', { r with avoid = av }


let current_date r =
  match r.levels with
    | [] -> invalid_arg "Renamings.current_date"
    | (d,_) :: _ -> d

let all_dates r = List.map fst r.levels

let rec valid_date da r = 
  let rec valid = function
    | [] -> false
    | (d,_) :: rem -> (d = da) || (valid rem)
  in
  valid r.levels

(* [until d r] selects the part of the renaming [r] starting from date [d] *)
let rec until da r =
  let rec cut = function
    | [] -> failwith ("unknown label " ^ da)
    | (d,_) :: rem as r -> if d = da then r else cut rem
  in
  { avoid = r.avoid; levels = cut r.levels; cpt = r.cpt }

let var_at_date r d id =
  try
    find (until d r) id
  with Not_found ->
    raise_unlocated (NoVariableAtDate (id, d))

let vars_at_date r d ids =
  let r' = until d r in List.map (fun id -> id,find r' id) ids


(* pretty-printers *)

open Format
open Pp

let print fmt r = 
  fprintf fmt "@[<hov 2>";
  print_list pp_print_newline
    (fun fmt (d,l) -> 
       fprintf fmt "%s: " d;
       print_list pp_print_space 
	 (fun fmt (id,id') -> 
	    fprintf fmt "(%s,%s)" (Ident.string id) (Ident.string id'))
	 fmt l)
    fmt r.levels


 
