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

open Java_ast
open Format
open Lexing

let parse_annot loc s f =
  let lb = Lexing.from_string s in
(*
  eprintf "lb.pos_fname = %s@." lb.lex_curr_p.pos_fname;
  eprintf "lb.pos_lnum = %d@." lb.lex_curr_p.pos_lnum;
  eprintf "lb.pos_bol = %d@." lb.lex_curr_p.pos_bol;
  eprintf "lb.pos_cnum = %d@." lb.lex_curr_p.pos_cnum;
*)
  lb.lex_curr_p <- {loc with pos_bol = loc.pos_bol - loc.pos_cnum - 3};
(*
  eprintf "lb.pos_fname = %s@." lb.lex_curr_p.pos_fname;
  eprintf "lb.pos_lnum = %d@." lb.lex_curr_p.pos_lnum;
  eprintf "lb.pos_bol = %d@." lb.lex_curr_p.pos_bol;
  eprintf "lb.pos_cnum = %d@." lb.lex_curr_p.pos_cnum;
*)
  try
    f Java_lexer.next_token lb
  with 
    | Parsing.Parse_error ->
	Java_options.parsing_error (Java_lexer.loc lb) ""
    | Java_options.Java_error (_,msg) ->
	Java_options.parsing_error (Java_lexer.loc lb) "%s" msg

let rec statement s =
  { s with java_pstatement_node = match s.java_pstatement_node with
      | JPSannot(loc,s) -> parse_annot loc s Java_parser.kml_statement_annot
      | JPSghost_local_decls _ | JPSghost_statement _ | JPSloop_annot _ 
      | JPSassert _ -> assert false
      | JPSsynchronized (e, s') -> JPSsynchronized(e, statements s')	
      | JPSblock b -> JPSblock(statements b)
      | JPSswitch(e, l) -> 
	  JPSswitch(e, List.map (fun (labs,b) -> (labs,statements b)) l)
      | JPStry (s, l, f) -> 
	  let l = List.map (fun (p,s) -> (p,statements s)) l in
	    JPStry(statements s,l,Option_misc.map (statements) f)
      | JPSfor_decl (d, e, sl, s) -> JPSfor_decl(d, e, sl, statement s)
      | JPSfor (el1, e, el2, s) -> JPSfor (el1, e, el2, statement s)
      | JPSdo (s', e) -> JPSdo (statement s',e)
      | JPSwhile (e, s') -> JPSwhile(e, statement s')
      | JPSif (e, s1, s2) -> JPSif(e, statement s1, statement s2)
      | JPSlabel (l, s') -> JPSlabel(l,statement s')
      | JPScontinue _
      | JPSbreak _
      | JPSreturn _
      | JPSthrow _
      | JPSvar_decl _
      | JPSexpr _
      | JPSskip -> s.java_pstatement_node }
    
and statements b = List.map statement b

let modifier m =
  match m with
    | Annot_modifier (loc, s) -> parse_annot loc s Java_parser.kml_modifier
    | _ -> m

let variable_declaration vd =
  { vd with variable_modifiers = List.map modifier vd.variable_modifiers }

let rec parameter p =
  match p with
    | Simple_parameter (mo, ty, id) -> 
	Simple_parameter (Option_misc.map modifier mo, ty, id)
    | Array_parameter p ->
	Array_parameter (parameter p)

let rec method_declarator md =
  match md with
    | Simple_method_declarator (id, pl) ->
	Simple_method_declarator (id, List.map parameter pl)
    | Array_method_declarator md -> 
	Array_method_declarator (method_declarator md)

let method_declaration md =
  { md with
      method_modifiers = List.map modifier md.method_modifiers;
      method_declarator = method_declarator md.method_declarator; } 
    
let rec field_decl f = 
  match f with
    | JPFmethod (md, None) -> JPFmethod (method_declaration md, None)
    | JPFmethod (md, Some b) -> JPFmethod (method_declaration md, Some (statements b))
    | JPFconstructor(c,eci,b) -> JPFconstructor(c,eci,statements b)
    | JPFvariable vd -> JPFvariable (variable_declaration vd) 
    | JPFstatic_initializer b -> JPFstatic_initializer (statements b)
    | JPFannot (loc,s) -> parse_annot loc s Java_parser.kml_field_decl
    | JPFinvariant _ | JPFstatic_invariant _ 
    | JPFmethod_spec _ -> assert false
    | JPFclass c -> JPFclass (class_decl c)
    | JPFinterface i -> JPFinterface (interface_decl i)
  
and class_decl c = 
  { c with class_fields = List.map field_decl c.class_fields }

and interface_decl i = 
  { i with interface_members = List.map field_decl i.interface_members }

let type_decl d =
  match d with
    | JPTclass c -> JPTclass (class_decl c)
    | JPTinterface i -> JPTinterface (interface_decl i)
    | JPTannot(loc,s) -> parse_annot loc s Java_parser.kml_type_decl
    | JPTlemma _ 
    | JPTlogic_type_decl _
    | JPTlogic_reads _ 
    | JPTlogic_def _ -> assert false


let compilation_unit cu =
  { cu with cu_type_decls = List.map type_decl cu.cu_type_decls }

let file f = 
  try
    let c = open_in f in
    let d = Java_lexer.parse f c in
      close_in c; 
      compilation_unit d
  with
    | Java_lexer.Lexical_error(l,s) ->
	eprintf "%a: lexical error: %s@." Loc.gen_report_position l s;
	exit 1

(*
Local Variables: 
compile-command: "make -C .. bin/krakatoa.byte"
End: 
*)
