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

open Format
open Pp
open Misc
open Logic
open Cc

let oblig = Queue.create ()


let split_one (loc,n,o) =
  (* normal oblig *)
  let cn = ref 0 in
  let l = ref [] in 
  let push_normal o = 
    incr cn; l := (loc,n ^ "_" ^ string_of_int !cn,o) :: !l 
  in
  (* fpi oblig *)
  let cf = ref 0 in
  let push_fpi o = incr cf; Queue.add (loc, n ^ "_" ^ string_of_int !cf, o) in
  let rec split_rec = function
    | _ -> assert false
  in
  split_rec o;
  List.rev !l

let split ol = List.flatten (List.map split_one ol)
  
let print_real fmt = function
  | (i,f,"") -> fprintf fmt "%s.%s" i f
  | (i,f,e) -> fprintf fmt "%s.%se%s" i f e

let rec print_term fmt = function
  | Tconst (ConstFloat f) -> print_real fmt f
  | Tconst _ -> assert false
  | Tvar id -> Ident.print fmt id
  | Tapp (id, tl, _) -> 
      fprintf fmt "(%s %a)" (Ident.string id) (print_list space print_term) tl
  | Tderef _ -> assert false

let rec print_pred fmt = function
  | Pfpi (t,f1,f2) -> 
      fprintf fmt "(fpi %a %a %a)" print_term t print_real f1 print_real f2
  | Papp (id, tl, _) -> 
      fprintf fmt "(%s %a)" (Ident.string id) (print_list space print_term) tl
  | _ -> assert false

let print_hyp fmt = function
  | Svar _ -> assert false
  | Spred (_, p) -> print_pred fmt p

let print_hyps = print_list space print_hyp

let print_obligation fmt (loc,s,o) =
  fprintf fmt "%% %s from %a@\n" s Loc.report_position loc;
  begin match o with
    | [], p -> 
	fprintf fmt "(@[%a@])" print_pred p
    | [h], p -> 
	fprintf fmt "(@[IMPLIES %a@ %a@])" print_hyp h print_pred p
    | hl, p -> 
	fprintf fmt "(@[IMPLIES (AND %a)@ %a@])" print_hyps hl print_pred p
  end;
  fprintf fmt "@.@."

let print_obligations fmt =
  Queue.iter (print_obligation fmt) oblig;
  fprintf fmt "@."

let output f =
  print_in_file ~margin:78 print_obligations (f ^ ".fpi")

let reset () = Queue.clear oblig
