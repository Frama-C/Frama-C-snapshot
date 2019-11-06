(**************************************************************************)
(*                                                                        *)
(*  This file is part of the Frama-C's E-ACSL plug-in.                    *)
(*                                                                        *)
(*  Copyright (C) 2012-2019                                               *)
(*    CEA (Commissariat à l'énergie atomique et aux énergies              *)
(*         alternatives)                                                  *)
(*                                                                        *)
(*  you can redistribute it and/or modify it under the terms of the GNU   *)
(*  Lesser General Public License as published by the Free Software       *)
(*  Foundation, version 2.1.                                              *)
(*                                                                        *)
(*  It is distributed in the hope that it will be useful,                 *)
(*  but WITHOUT ANY WARRANTY; without even the implied warranty of        *)
(*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the         *)
(*  GNU Lesser General Public License for more details.                   *)
(*                                                                        *)
(*  See the GNU Lesser General Public License version 2.1                 *)
(*  for more details (enclosed in the file licenses/LGPLv2.1).            *)
(*                                                                        *)
(**************************************************************************)

open Cil
open Cil_types

(**************************************************************************)
(********************** Forward references ********************************)
(**************************************************************************)

let translate_named_predicate_ref
  : (kernel_function -> Env.t -> predicate -> Env.t) ref
  = Extlib.mk_fun "translate_named_predicate_ref"

let named_predicate_ref
  : (kernel_function -> Env.t -> predicate -> exp * Env.t) ref
  = Extlib.mk_fun "named_predicate_ref"

let term_to_exp_ref
  : (kernel_function -> Env.t -> term -> exp * Env.t) ref
  = Extlib.mk_fun "term_to_exp_ref"

(**************************************************************************)
(************************* Loop invariants ********************************)
(**************************************************************************)

module Loop_invariants_actions = Hook.Make(struct end)

let apply_after_transformation prj =
  Project.on prj Loop_invariants_actions.apply ()

let mv_invariants env ~old stmt =
  Options.feedback ~current:true ~level:3
    "keep loop invariants attached to its loop";
  match Env.current_kf env with
  | None -> assert false
  | Some kf ->
    let filter _ ca = match ca.annot_content with
      | AInvariant(_, b, _) -> b
      | _ -> false
    in
    let l = Annotations.code_annot_emitter ~filter stmt in
    if l != [] then
      Loop_invariants_actions.extend
	(fun () ->
	  List.iter
	    (fun (ca, e) ->
	      Annotations.remove_code_annot e ~kf old ca;
	      Annotations.add_code_annot e ~kf stmt ca)
	    l)

let preserve_invariant prj env kf stmt = match stmt.skind with
  | Loop(_, ({ bstmts = stmts } as blk), loc, cont, break) ->
    let rec handle_invariants (stmts, env, _ as acc) = function
      | [] ->
	(* empty loop body: no need to verify the invariant twice *)
	acc
      | [ last ] ->
	let invariants, env = Env.pop_loop env in
	let env = Env.push env in
	let env =
    let translate_named_predicate = !translate_named_predicate_ref in
	  Project.on
	    prj
	    (List.fold_left (translate_named_predicate kf) env)
	    invariants
	in
	let blk, env =
	  Env.pop_and_get env last ~global_clear:false Env.Before
	in
	Misc.mk_block prj last blk :: stmts, env, invariants != []
      | s :: tl -> handle_invariants (s :: stmts, env, false) tl
    in
    let env = Env.set_annotation_kind env Misc.Invariant in
    let stmts, env, has_loop = handle_invariants ([], env, false) stmts in
    let new_blk = { blk with bstmts = List.rev stmts } in
    { stmt with skind = Loop([], new_blk, loc, cont, break) },
    env,
    has_loop
  | _ -> stmt, env, false

(**************************************************************************)
(**************************** Nested loops ********************************)
(**************************************************************************)

(* It could happen that the bounds provided for a quantifier [lv] are bigger
  than its type. [bounds_for_small_type] handles such cases
  and provides smaller bounds whenever possible.
  Let B be the inferred interval and R the range of [lv.typ]
  - Case 1: B \subseteq R
    Example: [\forall unsigned char c; 4 <= c <= 100 ==> 0 <= c <= 255]
    Return: B
  - Case 2: B \not\subseteq R and the bounds of B are inferred exactly
    Example: [\forall unsigned char c; 4 <= c <= 300 ==> 0 <= c <= 255]
    Return: B \intersect R
  - Case 3: B \not\subseteq R and the bounds of B are NOT inferred exactly
    Example: [\let m = n > 0 ? 4 : 341; \forall char u; 1 < u < m ==> u > 0]
    Return: R with a guard guaranteeing that [lv] does not overflow *)
let bounds_for_small_type ~loc (t1, lv, t2) =
  match lv.lv_type with
  | Ltype _ | Lvar _ | Lreal | Larrow _ ->
    Options.abort "quantification over non-integer type is not part of E-ACSL"

  | Linteger ->
    t1, t2, None

  | Ctype ty ->
    let iv1 = Interval.(extract_ival (infer t1)) in
    let iv2 = Interval.(extract_ival (infer t2)) in
    (* Ival.join is NOT correct here:
       Eg: (Ival.join [-3..-3] [300..300]) gives {-3, 300}
       but NOT [-3..300] *)
    let iv = Ival.inject_range (Ival.min_int iv1) (Ival.max_int iv2) in
    let ity = Interval.extract_ival (Interval.interv_of_typ ty) in
    if Ival.is_included iv ity then
      (* case 1 *)
      t1, t2, None
    else if Ival.is_singleton_int iv1 && Ival.is_singleton_int iv2 then begin
      (* case 2 *)
      let i = Ival.meet iv ity in
      (* now we potentially have a better interval for [lv]
         ==> update the binding *)
      Interval.Env.replace lv (Interval.Ival i);
      (* the smaller bounds *)
      let min, max = Misc.finite_min_and_max i in
      let t1 = Logic_const.tint ~loc min in
      let t2 = Logic_const.tint ~loc max in
      let ctx = Typing.number_ty_of_typ ty in
      (* we are assured that we will not have a GMP,
         once again because we intersected with [ity] *)
      Typing.type_term ~use_gmp_opt:false ~ctx t1;
      Typing.type_term ~use_gmp_opt:false ~ctx t2;
      t1, t2, None
    end else
      (* case 3 *)
      let min, max = Misc.finite_min_and_max ity in
      let guard_lower = Logic_const.tint ~loc min in
      let guard_upper = Logic_const.tint ~loc max in
      let lv_term = Logic_const.tvar ~loc lv in
      let guard_lower = Logic_const.prel ~loc (Rle, guard_lower, lv_term) in
      let guard_upper = Logic_const.prel ~loc (Rle, lv_term, guard_upper) in
      let guard = Logic_const.pand ~loc (guard_lower, guard_upper) in
      t1, t2, Some guard

let rec mk_nested_loops ~loc mk_innermost_block kf env lscope_vars =
  let term_to_exp = !term_to_exp_ref in
  match lscope_vars with
  | [] ->
    mk_innermost_block env
  | Lscope.Lvs_quantif(t1, rel1, logic_x, rel2, t2) :: lscope_vars' ->
    let t1, t2, guard_for_small_type_opt =
      bounds_for_small_type ~loc (t1, logic_x, t2)
    in
    let ctx =
      let ty1 = Typing.get_number_ty t1 in
      let ty2 = Typing.get_number_ty t2 in
      Typing.join ty1 ty2
    in
    let t_plus_one ?ty t =
      (* whenever provided, [ty] is known to be the type of the result *)
      let tone = Cil.lone ~loc () in
      let res = Logic_const.term ~loc (TBinOp(PlusA, t, tone)) Linteger in
      Extlib.may
        (fun ty ->
          Typing.unsafe_set tone ~ctx:ty ctx;
          Typing.unsafe_set t ~ctx:ty ctx;
          Typing.unsafe_set res ty)
        ty;
      res
    in
    let t1 = match rel1 with
      | Rlt ->
        let t = t_plus_one t1 in
        Typing.type_term ~use_gmp_opt:false ~ctx t;
        t
      | Rle ->
        t1
      | Rgt | Rge | Req | Rneq ->
        assert false
      in
    let t2_one, bop2 = match rel2 with
      | Rlt ->
        t2, Lt
      | Rle ->
        (* we increment the loop counter one more time (at the end of the
          loop). Thus to prevent overflow, check the type of [t2+1]
          instead of [t2]. *)
        t_plus_one t2, Le
      | Rgt | Rge | Req | Rneq ->
        assert false
    in
    Typing.type_term ~use_gmp_opt:false ~ctx t2_one;
    let ctx_one =
      let ty1 = Typing.get_number_ty t1 in
      let ty2 = Typing.get_number_ty t2_one in
      Typing.join ty1 ty2
    in
    let ty =
      try Typing.typ_of_number_ty ctx_one
      with Typing.Not_a_number -> assert false
    in
    (* loop counter corresponding to the quantified variable *)
    let var_x, x, env = Env.Logic_binding.add ~ty env logic_x in
    let lv_x = var var_x in
    let env = match ctx_one with
      | Typing.C_integer _ -> env
      | Typing.Gmpz -> Env.add_stmt env (Gmp.init ~loc x)
      | Typing.(C_float _ | Rational | Real | Nan) -> assert false
    in
    (* build the inner loops and loop body *)
    let body, env =
      mk_nested_loops ~loc mk_innermost_block kf env lscope_vars'
    in
    (* initialize the loop counter to [t1] *)
    let e1, env = term_to_exp kf (Env.push env) t1 in
    let init_blk, env = Env.pop_and_get
      env
      (Gmp.affect ~loc:e1.eloc lv_x x e1)
      ~global_clear:false
      Env.Middle
    in
    (* generate the guard [x bop t2] *)
    let block_to_stmt b = mkStmt ~valid_sid:true (Block b) in
    let tlv = Logic_const.tvar ~loc logic_x in
    let guard =
      (* must copy [t2] to force being typed again *)
      Logic_const.term ~loc
        (TBinOp(bop2, tlv, { t2 with term_node = t2.term_node } )) Linteger
    in
    Typing.type_term ~use_gmp_opt:false ~ctx:Typing.c_int guard;
    let guard_exp, env = term_to_exp kf (Env.push env) guard in
    let break_stmt = mkStmt ~valid_sid:true (Break guard_exp.eloc) in
    let guard_blk, env = Env.pop_and_get
      env
      (mkStmt
        ~valid_sid:true
        (If(
          guard_exp,
          mkBlock [ mkEmptyStmt ~loc () ],
          mkBlock [ break_stmt ],
          guard_exp.eloc)))
      ~global_clear:false
      Env.Middle
    in
    let guard = block_to_stmt guard_blk in
    (* increment the loop counter [x++];
       previous typing ensures that [x++] fits type [ty] *)
    let tlv_one = t_plus_one ~ty:ctx_one tlv in
    let incr, env = term_to_exp kf (Env.push env) tlv_one in
    let next_blk, env = Env.pop_and_get
      env
      (Gmp.affect ~loc:incr.eloc lv_x x incr)
      ~global_clear:false
      Env.Middle
    in
    (* generate the whole loop *)
    let next = block_to_stmt next_blk in
    let stmts, env = match guard_for_small_type_opt with
      | None ->
        guard :: body @ [ next ], env
      | Some p ->
        let e, env = !named_predicate_ref kf (Env.push env) p in
        let stmt, env =
          Misc.mk_e_acsl_guard ~reverse:true Misc.RTE kf e p, env
        in
        let b, env = Env.pop_and_get env stmt ~global_clear:false Env.After in
        let guard_for_small_type = Cil.mkStmt ~valid_sid:true (Block b) in
        guard_for_small_type :: guard :: body @ [ next ], env
    in
    let start = block_to_stmt init_blk in
    let stmt = mkStmt
      ~valid_sid:true
      (Loop(
        [],
        mkBlock stmts,
        loc,
        None,
        Some break_stmt))
    in
    (* remove logic binding before returning *)
    Env.Logic_binding.remove env logic_x;
    [ start ;  stmt ], env
  | Lscope.Lvs_let(lv, t) :: lscope_vars' ->
    let ty = Typing.get_typ t in
    let vi_of_lv, exp_of_lv, env = Env.Logic_binding.add ~ty env lv in
    let e, env = term_to_exp kf env t in
    let ty = Cil.typeOf e in
    let init_set =
      if Gmp_types.Q.is_t ty then Rational.init_set else Gmp.init_set
    in
    let let_stmt = init_set ~loc (Cil.var vi_of_lv) exp_of_lv  e in
    let stmts, env =
      mk_nested_loops ~loc mk_innermost_block kf env lscope_vars'
    in
    (* remove the logic binding now that the block is constructed *)
    Env.Logic_binding.remove env lv;
    (* return *)
    let_stmt :: stmts, env
  | Lscope.Lvs_formal _ :: _ ->
    Error.not_yet
      "creating nested loops from formal variable of a logic function"
  | Lscope.Lvs_global _ :: _ ->
    Error.not_yet
      "creating nested loops from global logic variable"

(*
Local Variables:
compile-command: "make -C ../.."
End:
*)
