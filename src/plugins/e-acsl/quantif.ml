(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2017                                               *)
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

open Cil_types
open Cil
open Cil_datatype

let predicate_to_exp_ref
    : (kernel_function -> Env.t -> predicate -> exp * Env.t) ref
    = Extlib.mk_fun "named_predicate_to_exp_ref"

let term_to_exp_ref
    : (kernel_function -> Env.t -> term -> exp * Env.t) ref
    = Extlib.mk_fun "term_to_exp_ref"

let compute_quantif_guards quantif bounded_vars hyps =
  let error msg pp x =
    let msg1 = Format.asprintf msg pp x in
    let msg2 =
      Format.asprintf "@[ in quantification@ %a@]"
        Printer.pp_predicate quantif
    in
    Error.untypable (msg1 ^ msg2)
  in
  let rec left_term acc vars left_bound t = match t.term_node with
    | TLogic_coerce(_, t) -> left_term acc vars left_bound t
    | TLval(TVar x, TNoOffset) ->
      (* check if [x] is the correct variable *)
      let v, vars = match vars with
        | [] -> error "@[too much constraint(s)%a@]" (fun _ () -> ()) ()
        | v :: tl -> match v.lv_type with
          | Ctype ty when isIntegralType ty -> v, tl
          | Linteger -> v, tl
          | Ltype _ as ty when Logic_const.is_boolean_type ty -> v, tl
          | Ctype _ | Ltype _ | Lvar _ | Lreal | Larrow _ ->
            error "@[non integer variable %a@]" Printer.pp_logic_var v
      in
      if Logic_var.equal x v then acc, Some (left_bound, x), vars
      else error "@[invalid binder %a@]" Printer.pp_term t
    | _ -> error "@[invalid binder %a@]" Printer.pp_term t
  in
  let rec parse acc vars p = match p.pred_content with
    | Pand(p, { pred_content = Prel((Rlt | Rle) as r, t1, t2) }) ->
      (* && is left-associative in the AST *)
      let acc, partial, vars = parse acc vars p in
      (match partial with
      | None ->
        (* left part of a new constraint: the searched variable is [t2] *)
        left_term acc vars (t1, r) t2
      | Some ((t_left, r_left), v)  ->
        (* right part of an existing constraint: the variable is [t1] *)
        let rec right_term t = match t.term_node with
          | TLogic_coerce(_, t) -> right_term t
          | TLval(TVar x, TNoOffset) ->
            if Logic_var.equal x v then
              (* new full constraint found *)
              (t_left, r_left, x, r, t2) :: acc, None, vars
            else
              error "@[invalid binder %a@]" Printer.pp_term t
          | _ -> error "@[invalid binder %a@]" Printer.pp_term t
        in
        right_term t1)
    | Prel((Rlt | Rle) as r, t1, t2) ->
      (* left-most predicate: the searched variable is [t2] *)
      left_term acc vars (t1, r) t2
    | _ -> error "@[invalid guard %a@]" Printer.pp_predicate p
  in
  let acc, partial, vars = parse [] bounded_vars hyps in
  (match partial with
  | None -> ()
  | Some(_, x) ->
    error "@[missing upper-bound for variable %a@]" Printer.pp_logic_var x);
  (match vars with
  | [] -> ()
  | _ :: _ ->
    let msg = 
      Format.asprintf
	"@[unguarded variable%s %tin quantification@ %a@]" 
	(if List.length vars = 1 then "" else "s") 
	(fun fmt -> 
	  List.iter
	    (fun v -> Format.fprintf fmt "@[%a @]" Printer.pp_logic_var v) 
	    vars)
	Printer.pp_predicate quantif
    in
    Error.untypable msg);
  List.rev acc

let () = Typing.compute_quantif_guards_ref := compute_quantif_guards

module Label_ids = 
  State_builder.Counter(struct let name = "E_ACSL.Label_ids" end)

let convert kf env loc is_forall p bounded_vars hyps goal =
  (* part depending on the kind of quantifications 
     (either universal or existential) *)
  let init_val, found_val, mk_guard = 
    let z = zero ~loc in
    let o = one ~loc in
    if is_forall then o, z, (fun x -> x) 
    else z, o, (fun e -> new_exp ~loc:e.eloc (UnOp(LNot, e, intType)))
  in
  let named_predicate_to_exp = !predicate_to_exp_ref in
  let term_to_exp = !term_to_exp_ref in
  (* universal quantification over integers (or a subtype of integer) *)
  let guards = compute_quantif_guards p bounded_vars hyps in
  let var_res, res, env =
    (* variable storing the result of the quantifier *)
    let name = if is_forall then "forall" else "exists" in
    Env.new_var
      ~loc
      ~name
      env
      None
      intType
      (fun v _ ->
	let lv = var v in
	[ mkStmtOneInstr ~valid_sid:true (Set(lv, init_val, loc)) ])
  in
  let end_loop_ref = ref dummyStmt in
  let rec mk_for_loop env = function
    | [] -> 
      (* innermost loop body: store the result in [res] and go out according
	 to evaluation of the goal *)
      let test, env = named_predicate_to_exp kf (Env.push env) goal in
      let then_block = mkBlock [ mkEmptyStmt ~loc () ] in
      let else_block = 
	(* use a 'goto', not a simple 'break' in order to handle 'forall' with
	   multiple binders (leading to imbricated loops) *)
	mkBlock
	  [ mkStmtOneInstr
	       ~valid_sid:true (Set(var var_res, found_val, loc));
	    mkStmt ~valid_sid:true (Goto(end_loop_ref, loc)) ]
      in
      let blk, env = 
	Env.pop_and_get
	  env
	  (mkStmt ~valid_sid:true
	     (If(mk_guard test, then_block, else_block, loc)))
	  ~global_clear:false
	  Env.After
      in
      let blk = Cil.flatten_transient_sub_blocks blk in
      [ mkStmt ~valid_sid:true (Block blk) ], env
    | (t1, rel1, logic_x, rel2, t2) :: tl ->
      let ctx =
        let ty1 = Typing.get_integer_ty t1 in
        let ty2 = Typing.get_integer_ty t2 in
        Typing.join ty1 ty2
      in
      let t_plus_one ?ty t =
        (* whenever provided, [ty] is known to be the type of the result *)
        let tone = Logic_const.tinteger ~loc 1 in
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
        | Rle -> t1
        | Rgt | Rge | Req | Rneq -> assert false
      in
      let t2_one, bop2 = match rel2 with
        | Rlt -> t2, Lt
        | Rle ->
          (* we increment the loop counter one more time (at the end of the
             loop). Thus to prevent overflow, check the type of [t2+1]
             instead of [t2]. *)
          t_plus_one t2, Le
        | Rgt | Rge | Req | Rneq -> assert false
      in
      Typing.type_term ~use_gmp_opt:false ~ctx t2_one;
      let ctx_one =
        let ty1 = Typing.get_integer_ty t1 in
        let ty2 = Typing.get_integer_ty t2_one in
        Typing.join ty1 ty2
      in
      let ty =
        try Typing.typ_of_integer_ty ctx_one
        with Typing.Not_an_integer -> assert false
      in
      (* loop counter corresponding to the quantified variable *)
      let var_x, x, env = Env.Logic_binding.add ~ty env logic_x in
      let lv_x = var var_x in
      let env = match ctx_one with
        | Typing.C_type _ -> env
        | Typing.Gmp -> Env.add_stmt env (Gmpz.init ~loc x)
        | Typing.Other -> assert false
      in
      (* build the inner loops and loop body *)
      let body, env = mk_for_loop env tl in
      (* initialize the loop counter to [t1] *)
      let e1, env = term_to_exp kf (Env.push env) t1 in
      let init_blk, env =
        Env.pop_and_get
          env
          (Gmpz.affect ~loc:e1.eloc lv_x x e1)
          ~global_clear:false
          Env.Middle
      in
      (* generate the guard [x bop t2] *)
      let stmts_block b = [ mkStmt ~valid_sid:true (Block b) ] in
      let tlv = Logic_const.tvar ~loc logic_x in
      let guard =
        (* must copy [t2] to force being typed again *)
        Logic_const.term ~loc
          (TBinOp(bop2, tlv, { t2 with term_node = t2.term_node } )) Linteger
      in
      Typing.type_term ~use_gmp_opt:false ~ctx:Typing.c_int guard;
      let guard_exp, env = term_to_exp kf (Env.push env) guard in
      let break_stmt = mkStmt ~valid_sid:true (Break guard_exp.eloc) in
      let guard_blk, env =
	Env.pop_and_get
	  env
	  (mkStmt ~valid_sid:true
	     (If(guard_exp,
		 mkBlock [ mkEmptyStmt ~loc () ],
		 mkBlock [ break_stmt ], 
		 guard_exp.eloc)))
	  ~global_clear:false
	  Env.Middle
      in
      let guard = stmts_block guard_blk in
      (* increment the loop counter [x++];
         previous typing ensures that [x++] fits type [ty] *)
      (* TODO: should check that it does not overflow in the case of the type
         of the loop variable is __declared__ too small. *)
      let tlv_one = t_plus_one ~ty:ctx_one tlv in
      let incr, env = term_to_exp kf (Env.push env) tlv_one in
      let next_blk, env = 
	Env.pop_and_get
	  env
	  (Gmpz.affect ~loc:incr.eloc lv_x x incr)
	  ~global_clear:false
	  Env.Middle
      in
      (* generate the whole loop *)
      let start = stmts_block init_blk in
      let next = stmts_block next_blk in
      start @
	[ mkStmt ~valid_sid:true
	    (Loop ([],
		   mkBlock (guard @ body @ next),
		   loc, 
		   None, 
		   Some break_stmt)) ], 
      env
  in
  let stmts, env = mk_for_loop env guards in
  let env = 
    Env.add_stmt env (mkStmt ~valid_sid:true (Block (mkBlock stmts))) 
  in
  (* where to jump to go out of the loop *)
  let end_loop = mkEmptyStmt ~loc () in
  let label_name = "e_acsl_end_loop" ^ string_of_int (Label_ids.next ()) in
  let label = Label(label_name, loc, false) in
  end_loop.labels <- label :: end_loop.labels;
  end_loop_ref := end_loop;
  let env = Env.add_stmt env end_loop in
  let env = List.fold_left Env.Logic_binding.remove env bounded_vars in
  res, env

let quantif_to_exp kf env p = 
  let loc = p.pred_loc in
  match p.pred_content with
  | Pforall(bounded_vars, { pred_content = Pimplies(hyps, goal) }) -> 
    convert kf env loc true p bounded_vars hyps goal
  | Pforall _ -> Error.not_yet "unguarded \\forall quantification"
  | Pexists(bounded_vars, { pred_content = Pand(hyps, goal) }) -> 
    convert kf env loc false p bounded_vars hyps goal
  | Pexists _ -> Error.not_yet "unguarded \\exists quantification"
  | _ -> assert false

(*
Local Variables:
compile-command: "make"
End:
*)
