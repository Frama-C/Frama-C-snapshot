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

open Jc_pervasives
open Jc_name
open Jc_ast
open Jc_env
open Jc_interp_misc
open Output

module type TAssertionMaker = sig
  type t
  val make_subtag: term -> struct_info -> t
  val make_or: t -> t -> t
  val make_and: t -> t -> t
  val make_not: t -> t
  val make_false: t
  val make_true: t
  val make_eq: jc_type -> term -> term -> t
end

module Pattern(AM: TAssertionMaker) = struct
  (** [pattern arg ty pat] translates the pattern [pat] applied to the term
    [arg] which have a [jc_type] of [ty].
    Returns [notcond, cond, vars] where:
    [notcond] is an assertion equivalent to "the pattern cannot be applied";
    [cond] is an assertion equivalent to "the pattern can be applied" and giving
      the values of each binded variable;
    [vars] is a list of [name, user_name, ty] where [user_name] is a variable
      binded by the pattern, [name] its unique name used in [cond], and
      [ty] its [jc_type]. *)
  let rec pattern arg ty pat = match pat#node with
    | JCPstruct(st, fpl) ->
	let subtag = AM.make_subtag arg st in
	List.fold_left
	  (fun (accnotcond, acccond, accvars) (fi, pat) ->
	     let arg = make_select_fi fi arg in
	     let ty = fi.jc_field_info_type in
	     let notcond, cond, vars = pattern arg ty pat in
	     AM.make_or accnotcond notcond,
	     AM.make_and acccond cond,
	     accvars @ vars)
	  (AM.make_not subtag, subtag, [])
	  fpl
    | JCPvar vi ->
	AM.make_false,
	AM.make_eq ty (LVar vi.jc_var_info_final_name) arg,
	[vi.jc_var_info_final_name, ty]
    | JCPor(p1, p2) ->
      (* typing ensures that both patterns bind the same variables *)
	let notcond1, cond1, vars = pattern arg ty p1 in
	let notcond2, cond2, _ = pattern arg ty p2 in
	AM.make_and notcond1 notcond2,
	AM.make_or cond1 cond2,
	vars
    | JCPas(p, vi) ->
	let notcond, cond, vars = pattern arg ty p in
	notcond,
	AM.make_and
	  cond
	  (AM.make_eq ty (LVar vi.jc_var_info_final_name) arg),
	(vi.jc_var_info_final_name, ty)::vars
    | JCPany | JCPconst JCCvoid ->
	AM.make_false,
	AM.make_true,
	[]
    | JCPconst c ->
	let eq = AM.make_eq ty arg (LConst (const c)) in
	AM.make_not eq,
	eq,
	[]
end

module MakeAssertion = struct
  type t = Output.assertion

  let make_subtag x st =
    let r = Jc_region.dummy_region in
    make_subtag (make_typeof st r x) (LVar (tag_name st))
  let make_or a b = LOr(a, b)
  let make_and a b = LAnd(a, b)
  let make_not a = LNot a
  let make_false = LFalse
  let make_true = LTrue
  let make_eq _ = make_eq
end
module PatternAssertion = Pattern(MakeAssertion)

module MakeTerm = struct
  type t = Output.term

  let make_subtag x st =
    let r = Jc_region.dummy_region in
    make_subtag_bool (make_typeof st r x) (LVar (tag_name st))
  let make_or = make_or_term
  let make_and = make_and_term
  let make_not = make_not_term
  let make_false = LConst(Prim_bool false)
  let make_true = LConst(Prim_bool false)
  let make_eq = make_eq_term
end
module PatternTerm = Pattern(MakeTerm)

let make_blackbox_annot pre ty reads writes post exn_posts =
  BlackBox(Annot_type(pre, ty, reads, writes, post, exn_posts))

let pattern_list_expr translate_body arg r ty pbl =
  List.fold_left
    (fun accbody (pat, body) ->
       let notcond, cond, vars = PatternAssertion.pattern arg ty pat in
       let body = translate_body body in
       let reads =
	 let ef = Jc_effect.pattern empty_effects (*LabelHere region*) pat in
	 all_effects ef
       in
       let writes = List.map fst vars in
       let post = LIf(LVar "result", cond, notcond) in
       let bbox = make_blackbox_annot LTrue bool_type reads writes post [] in
       let branch = List.fold_left
	 (fun acc (name, ty) -> Let_ref(name, any_value ty, acc))
	 (If(bbox, body, accbody))
	 vars
       in
       branch)
    Absurd
    (List.rev pbl)

let pattern_list_term translate_body arg ty pbl default =
  (* Right now, all binders are put at the predicate level, so there might
     be problems (though variables are renamed so it should be ok).
     Will be better when (if) Why has Let binders at the term level. *)
  List.fold_left
    (fun (accbody, acclets) (pat, body) ->
       let _, cond, vars = PatternTerm.pattern arg ty pat in
       let body = translate_body body in
       let body = make_if_term cond body accbody in
       let lets = List.map
	 (fun (n, ty) -> JCforall(n, tr_base_type ty))
	 vars
       in
       body, lets)
    (default, [])
    (List.rev pbl)

let pattern_list_assertion translate_body arg ty pbl default =
  List.fold_left
    (fun (accbody, accnotcond) (pat, body) ->
       (* not previous case => (forall vars, arg matches pattern => body) *)
       let notcond, cond, vars = PatternAssertion.pattern arg ty pat in
       let body = translate_body body in
       let case =
	 List.fold_left
	   (fun acc (n, ty) -> LForall(n, tr_base_type ty, acc))
	   (LImpl(cond, body))
	   vars
       in
       let full_case = LImpl(accnotcond, case) in
       LAnd(accbody, full_case), LAnd(accnotcond, notcond))
    (default, LTrue)
    pbl

(*
  Local Variables: 
  compile-command: "unset LANG; make -j -C .. bin/jessie.byte"
  End:
*)
