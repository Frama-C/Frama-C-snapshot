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

(** Ouput the normalized AST. Useful for debugging. Not parsable. *)

open Jc_pervasives
open Jc_output_misc
open Jc_ast
open Format
open Pp

let rec expr fmt e =
  let out x = fprintf fmt x in
  match e#node with
    | JCNEconst c ->
        const fmt c
    | JCNElabel(id, e1) ->
        out "(@[<hv 2>%s:@ %a@])" id expr e1
    | JCNEvar id ->
        out "%s" id
    | JCNEderef(e1, id) ->
        out "%a.%s" expr e1 id
    | JCNEbinary(e1, op, e2) ->
        out "(@[<hv 2>%a %s@ %a@])" expr e1 (string_of_op op) expr e2
    | JCNEunary(op, e1) ->
        out "(%s %a)" (string_of_op op) expr e1
    | JCNEapp(id, labs, el) ->
        out "@[<hv 2>%s@,{%a}@,(%a)@]" id (print_list comma label)
          labs (print_list comma expr) el
    | JCNEassign(e1, e2) ->
        out "(@[<hv 2>%a =@ %a@])" expr e1 expr e2
    | JCNEinstanceof(e1, id) ->
        out "(TODO instanceof)"
    | JCNEcast(e1, id) ->
        out "(TODO cast)"
    | JCNEif(e1, e2, e3) ->
        out "(TODO if)"
    | JCNEoffset(k, e1) ->
        out "(\\offset_m%a(%a))" offset_kind k expr e1
    | JCNEalloc(e1, id) ->
        out "(TODO alloc)"
    | JCNEfree e1 ->
        out "(TODO free)"
    | JCNElet(Some pty, id, Some e1, e2) ->
        out "(@[<hv 2>let %a %s = %a in@ %a@])" ptype pty id expr e1 expr e2
    | JCNElet(Some pty, id, None, e1) ->
        out "(@[<hv 2>let %a %s in@ %a@])" ptype pty id expr e1
    | JCNElet(None, id, Some e1, e2) ->
        out "(@[<hv 2>let %s = %a in@ %a@])" id expr e1 expr e2
    | JCNElet(None, id, None, e1) ->
        out "(@[<hv 2>let %s in@ %a@])" id expr e1
    | JCNEassert (behav,e1) ->
        out "(assert %a%a)" 
	  (print_list_delim 
	     (constant_string "for ") (constant_string ": ") 
	     comma string)
	  behav
	  expr e1
    | JCNEblock el ->
        out "{@ @[<hv 2>%a@]@ }" (print_list semi expr) el
    | JCNEloop(inv, Some e2, e3) ->
        out "@[<hv 2>%a@ variant %a;@ %a done@]" 
	  (print_list nothing 
	     (fun fmt (behav,inv) -> out "@\ninvariant %a%a;"
		(print_list_delim 
		   (constant_string "for ") (constant_string ": ") 
		   comma string)
		behav
		expr inv))
	  inv
	  expr e2
          expr e3
    | JCNEloop(inv, None, e2) ->
        out "@[<hv 2>%a@ %a done@]" 
	  (print_list nothing 
	     (fun fmt (behav,inv) -> out "@\ninvariant %a%a;"
		(print_list_delim 
		   (constant_string "for ") (constant_string ": ") 
		   comma string)
		behav
		expr inv))
	  inv
	  expr e2
    | JCNEreturn(Some e1) ->
        out "(return %a)" expr e1
    | JCNEreturn None ->
        out "(return)"
    | JCNEtry(e1, l, e2) ->
        out "(@[<hv 2>try %a with@ %a@ | default -> %a@])" expr e1
          (print_list space
             (fun fmt (id, s, e3) ->
                fprintf fmt "| %s %s -> %a" id#name s expr e3))
          l expr e2
    | JCNEthrow(id, Some e1) ->
        out "(throw %s %a)" id#name expr e1
    | JCNEthrow(id, None) ->
        out "(throw %s)" id#name
    | JCNEpack(e1, ido) ->
        out "(TODO pack)"
    | JCNEunpack(e1, ido) ->
        out "(TODO unpack)"
    | JCNEmatch(e1, pel) ->
        out "(TODO match)"
    | JCNEquantifier(Forall, pty, idl, e1) ->
        out "(@[<hv 2>\\forall %a %a,@ %a@])" ptype pty
          (print_list space string) idl expr e1
    | JCNEquantifier(Exists, pty, idl, e1) ->
        out "(@[<hv 2>\\exists %a %a,@ %a@])" ptype pty
          (print_list space string) idl expr e1
    | JCNEold e1 ->
        out "(TODO old)"
    | JCNEat(e1, lab) ->
        out "\\at(%a,@ %a)" expr e1 label lab
    | JCNEmutable(e1, tag) ->
        out "(TODO mutable)"
    | JCNEtagequality(tag1, tag2) ->
        out "(TODO tagequality)"
    | JCNErange(Some e1, Some e2) ->
        out "(%a .. %a)" expr e1 expr e2
    | JCNErange(Some e1, None) ->
        out "(%a ..)" expr e1
    | JCNErange(None, Some e1) ->
        out "(.. %a)" expr e1
    | JCNErange(None, None) ->
        out "(..)"

(*
Local Variables: 
compile-command: "LC_ALL=C make -j -C .. bin/jessie.byte"
End: 
*)
