(**************************************************************************)
(*                                                                        *)
(*  This file is part of the Frama-C's E-ACSL plug-in.                    *)
(*                                                                        *)
(*  Copyright (C) 2012-2018                                               *)
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
  (* universal quantification over integers (or a subtype of integer) *)
  let guards = compute_quantif_guards p bounded_vars hyps in
  (* transform [guards] into [lscope_var list],
     and update logic scope in the process *)
  let lvs_guards, env = List.fold_right
    (fun (t1, rel1, lv, rel2, t2) (lvs_guards, env) ->
      let lvs = Lscope.Lvs_quantif(t1, rel1, lv, rel2, t2) in
      let env = Env.Logic_scope.extend env lvs in
      lvs :: lvs_guards, env)
    guards
    ([], env)
  in
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
  (* innermost block *)
  let mk_innermost_block env =
    (* innermost loop body: store the result in [res] and go out according
       to evaluation of the goal *)
    let named_predicate_to_exp = !predicate_to_exp_ref in
    let test, env = named_predicate_to_exp kf (Env.push env) goal in
    let then_block = mkBlock [ mkEmptyStmt ~loc () ] in
    let else_block =
    (* use a 'goto', not a simple 'break' in order to handle 'forall' with
       multiple binders (leading to imbricated loops) *)
    mkBlock
      [ mkStmtOneInstr ~valid_sid:true (Set(var var_res, found_val, loc));
        mkStmt ~valid_sid:true (Goto(end_loop_ref, loc)) ]
    in
    let blk, env = Env.pop_and_get
      env
      (mkStmt ~valid_sid:true
        (If(mk_guard test, then_block, else_block, loc)))
      ~global_clear:false
      Env.After
    in
    let blk = Cil.flatten_transient_sub_blocks blk in
    [ mkStmt ~valid_sid:true (Block blk) ], env
  in
  let stmts, env =
    Loops.mk_nested_loops ~loc mk_innermost_block kf env lvs_guards
  in
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
