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

open Ml_misc
open Jc_env
open Jc_ast
open Ml_ocaml.Typedtree
open Ml_ocaml.Ident
open Ml_type
open Ml_env
open Ml_constant

let rec pattern env pat =
  let newenv, node = match pat.pat_desc with
    | Tpat_any ->
	env, JCPany
    | Tpat_var id ->
	let env, vi = add_var (name id) (make pat.pat_type) env in
	env, JCPvar vi
    | Tpat_alias(p, id) ->
	let env, p = pattern env p in
	let env, vi = add_var (name id) (make pat.pat_type) env in
	env, JCPas(p, vi)
    | Tpat_constant c ->
	env, JCPconst(constant c)
    | Tpat_tuple pl ->
	let si = structure pat.pat_type in
	let env, fpl = list_fold_mapi
	  (fun env i p ->
	     let env, p = pattern env p in
	     env, (proj pat.pat_type i, p))
	  env pl
	in
	env, JCPstruct(si, fpl)
    | Tpat_construct(cd, pl) ->
	let ci = constructor pat.pat_type cd in
	let env, fpl = list_fold_map2
	  (fun env fi p ->
	     let env, p = pattern env p in
	     env, (fi, p))
	  env ci.ml_ci_arguments pl
	in
	env, JCPstruct(ci.ml_ci_structure, fpl)
    | Tpat_variant _ ->
	assert false (* TODO *)
    | Tpat_record lpl ->
	let si = structure pat.pat_type in
	let env, fpl = list_fold_map
	  (fun env (l, p) ->
	     let li = label pat.pat_type l in
	     let env, p = pattern env p in
	     env, (li.ml_li_field, p))
	  env lpl
	in
	env, JCPstruct(si, fpl)
    | Tpat_array _ ->
	assert false (* TODO *)
    | Tpat_or(p1, p2, _) ->
	let _, p1 = pattern env p1 in
	let env, p2 = pattern env p2 in
	env, JCPor(p1, p2)
  in newenv, {
    jc_pattern_node = node;
    jc_pattern_loc = Loc.dummy_position;
    jc_pattern_type = JCTnull;
  }

let pattern_list make env body pats =
  let benv, pats = list_fold_map pattern env pats in
  let args = List.map
    (fun pat -> match pat.jc_pattern_node with
       | JCPvar vi | JCPas(_, vi) -> vi
       | JCPor _ | JCPconst _ | JCPany | JCPstruct _ ->
	   new_var ~add:"arg" pat.jc_pattern_type)
    pats
  in
  let result = List.fold_right2
    (fun pat vi body ->
       match pat.jc_pattern_node with
	 | JCPvar _ -> body
	 | _ -> make pat vi body)
    pats args (body benv)
  in
  args, result

let pattern_list_term =
  pattern_list
    (fun pat vi body -> make_term
       (JCTmatch(
	  make_var_term vi,
	  [ pat, body ]
	))
       body.jc_term_type)

let pattern_list_assertion =
  pattern_list
    (fun pat vi body -> make_assertion
       (JCAmatch(
	  make_var_term vi,
	  [ pat, body ]
	)))

let pattern_ make env body pat =
  let args, result = make env body [pat] in
  let arg = match args with
    | [vi] -> vi
    | _ -> assert false (* impossible *)
  in
  arg, result

let pattern_term = pattern_ pattern_list_term

let pattern_assertion = pattern_ pattern_list_assertion

(*
Local Variables: 
compile-command: "unset LANG; make -j -C .. bin/jessica.opt"
End: 
*)
