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

open Logic
open Cc
open Ident
open Format
open Misc
open Pp
open Tags

let rec print_pure_type fmt = function
  | PTint -> fprintf fmt "int"
  | PTbool -> fprintf fmt "bool"
  | PTunit -> fprintf fmt "unit"
  | PTreal -> fprintf fmt "real"
  | PTexternal ([v], id) when id == farray -> 
      fprintf fmt "@[%a array@]" print_pure_type v
  | PTexternal([],id) -> Ident.print fmt id
  | PTexternal([l],id) -> 
      fprintf fmt "@[%a %a@]"
	print_pure_type l
	Ident.print id
  | PTexternal(l,id) -> 
      fprintf fmt "@[(%a) %a@]"
	(print_list comma print_pure_type) l
	Ident.print id
  | PTvar { type_val = Some t} -> 
      fprintf fmt "%a" print_pure_type t      
  | PTvar v ->
      fprintf fmt "A%d" v.tag

let print_binder = Coq.print_binder_v8
let print_binder_type = Coq.print_binder_type_v8

let rec print_cc_type fmt = function
  | TTpure pt -> 
      print_pure_type fmt pt
  | t -> Coq.print_cc_type_v8 fmt t
