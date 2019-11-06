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

module E_acsl_label = Label
open Cil_types
open Cil_datatype

type localized_scope =
  | LGlobal
  | LFunction of kernel_function
  | LLocal_block of kernel_function

type mp_tbl = {
  new_exps: (varinfo * exp) Term.Map.t;
  (* generated mp variables as exp from terms *)
  clear_stmts: stmt list;
  (* stmts freeing the memory before exiting the block *)
}

type block_info = {
  new_block_vars: varinfo list;
  (* generated variables local to the block *)
  new_stmts: stmt list;
  (* generated stmts to put at the beginning of the block *)
  pre_stmts: stmt list;
  (* stmts already inserted into the current stmt,
     but which should be before [new_stmts]. *)
  post_stmts: stmt list;
}

type local_env = {
  block_info: block_info;
  mp_tbl: mp_tbl;
  rte: bool
}

type t = {
  visitor: Visitor.frama_c_visitor;
  lscope: Lscope.t;
  lscope_reset: bool;
  annotation_kind: Misc.annotation_kind;
  new_global_vars: (varinfo * localized_scope) list;
  (* generated variables. The scope indicates the level where the variable
     should be added. *)
  global_mp_tbl: mp_tbl;
  env_stack: local_env list;
  var_mapping: Varinfo.t Stack.t Logic_var.Map.t;
  (* records of C bindings for logic vars *)
  loop_invariants: predicate list list;
  (* list of loop invariants for each currently visited loops *)
  cpt: int;
  (* counter used when generating variables *)
}

let empty_block =
  { new_block_vars = [];
    new_stmts = [];
    pre_stmts = [];
    post_stmts = [] }

let empty_mp_tbl =
  { new_exps = Term.Map.empty;
    clear_stmts = [] }

let empty_local_env =
  { block_info = empty_block;
    mp_tbl = empty_mp_tbl;
    rte = true }

let dummy =
  { visitor = new Visitor.generic_frama_c_visitor (Visitor_behavior.inplace ());
    lscope = Lscope.empty;
    lscope_reset = true;
    annotation_kind = Misc.Assertion;
    new_global_vars = [];
    global_mp_tbl = empty_mp_tbl;
    env_stack = [];
    var_mapping = Logic_var.Map.empty;
    loop_invariants = [];
    cpt = 0; }

let empty v =
  { visitor = v;
    lscope = Lscope.empty;
    lscope_reset = true;
    annotation_kind = Misc.Assertion;
    new_global_vars = [];
    global_mp_tbl = empty_mp_tbl;
    env_stack = [];
    var_mapping = Logic_var.Map.empty;
    loop_invariants = [];
    cpt = 0 }

let top env = match env.env_stack with
  | [] -> Options.fatal "Empty environment. That is unexpected."
  | hd :: tl -> hd, tl

let has_no_new_stmt env =
  let local, _ = top env in
  local.block_info = empty_block

let current_kf env =
  let v = env.visitor in
  match v#current_kf with
  | None -> None
  | Some kf -> Some (Visitor_behavior.Get.kernel_function v#behavior kf)

let set_current_kf env kf =
  let v = env.visitor in
  v#set_current_kf kf

let get_visitor env = env.visitor
let get_behavior env = env.visitor#behavior

(* ************************************************************************** *)
(** {2 Loop invariants} *)
(* ************************************************************************** *)

let push_loop env =
  { env with loop_invariants = [] :: env.loop_invariants }

let add_loop_invariant env inv = match env.loop_invariants with
  | [] -> assert false
  | invs :: tl -> { env with loop_invariants = (inv :: invs) :: tl }

let pop_loop env = match env.loop_invariants with
  | [] -> assert false
  | invs :: tl -> invs, { env with loop_invariants = tl }

(* ************************************************************************** *)
(** {2 RTEs} *)
(* ************************************************************************** *)

let rte env b =
  let local_env, tl_env = top env in
  { env with env_stack = { local_env with rte = b } :: tl_env }

let generate_rte env =
  let local_env, _ = top env in
  local_env.rte

(* ************************************************************************** *)

(* eta-expansion required for typing generalisation *)
let acc_list_rev acc l = List.fold_left (fun acc x -> x :: acc) acc l

let do_new_var ~loc ?(scope=Varname.Block) ?(name="") env t ty mk_stmts =
  let local_env, tl_env = top env in
  let local_block = local_env.block_info in
  let is_z_t = Gmp_types.Z.is_t ty in
  if is_z_t then Gmp_types.Z.is_now_referenced ();
  let is_q_t = Gmp_types.Q.is_t ty in
  if is_q_t then Gmp_types.Q.is_now_referenced ();
  let n = succ env.cpt in
  let v =
    Cil.makeVarinfo
      ~source:true
      false (* is a global? *)
      false (* is a formal? *)
      ~referenced:true
      (Varname.get ~scope (Functions.RTL.mk_gen_name name))
      ty
  in
  v.vreferenced <- true;
  let lscope = match scope with
    | Varname.Global -> LGlobal
    | Varname.Function -> LFunction (Extlib.the (current_kf env))
    | Varname.Block -> LLocal_block (Extlib.the (current_kf env))
  in
(*  Options.feedback "new variable %a (global? %b)" Varinfo.pretty v global;*)
  let e = Cil.evar v in
  let stmts = mk_stmts v e in
  let new_stmts = acc_list_rev local_block.new_stmts stmts in
  let new_block_vars = match scope with
    | Varname.Global | Varname.Function -> local_block.new_block_vars
    | Varname.Block -> v :: local_block.new_block_vars
  in
  let new_block =
    { new_block_vars = new_block_vars;
      new_stmts = new_stmts;
      pre_stmts = local_block.pre_stmts;
      post_stmts = local_block.post_stmts
  }
  in
  v,
  e,
  if is_z_t || is_q_t then begin
    let extend_tbl tbl =
(*      Options.feedback "memoizing %a for term %a"
        Varinfo.pretty v (fun fmt t -> match t with None -> Format.fprintf fmt
        "NONE" | Some t -> Term.pretty fmt t) t;*)
      { clear_stmts = Gmp.clear ~loc e :: tbl.clear_stmts;
        new_exps = match t with
          | None -> tbl.new_exps
          | Some t -> Term.Map.add t (v, e) tbl.new_exps }
    in
    match scope with
    | Varname.Global | Varname.Function ->
      let local_env = { local_env with block_info = new_block } in
      (* also memoize the new variable, but must never be used *)
      { env with
        cpt = n;
        new_global_vars = (v, lscope) :: env.new_global_vars;
        global_mp_tbl = extend_tbl env.global_mp_tbl;
        env_stack = local_env :: tl_env }
    | Varname.Block ->
      let local_env =
        { block_info = new_block;
          mp_tbl = extend_tbl local_env.mp_tbl;
          rte = false (* must be already checked by mk_stmts *) }
      in
      { env with
        cpt = n;
        env_stack = local_env :: tl_env;
        new_global_vars = (v, lscope) :: env.new_global_vars }
  end else
    let new_global_vars = (v, lscope) :: env.new_global_vars in
    let local_env =
      { local_env with
        block_info = new_block;
        rte = false (* must be already checked by mk_stmts *) }
    in
    { env with
      new_global_vars = new_global_vars;
      cpt = n;
      env_stack = local_env :: tl_env }

exception No_term

let new_var ~loc ?(scope=Varname.Block) ?name env t ty mk_stmts =
  let local_env, _ = top env in
  let memo tbl =
    try
      match t with
      | None -> raise No_term
      | Some t ->
        let v, e = Term.Map.find t tbl.new_exps in
        if Typ.equal ty v.vtype then v, e, env else raise No_term
    with Not_found | No_term ->
      do_new_var ~loc ~scope ?name env t ty mk_stmts
  in
  match scope with
  | Varname.Global | Varname.Function -> memo env.global_mp_tbl
  | Varname.Block -> memo local_env.mp_tbl

let new_var_and_mpz_init ~loc ?scope ?name env t mk_stmts =
  new_var
    ~loc
    ?scope
    ?name
    env
    t
    (Gmp_types.Z.t ())
    (fun v e -> Gmp.init ~loc e :: mk_stmts v e)

module Logic_binding = struct

  let add_binding env logic_v vi =
    try
      let varinfos = Logic_var.Map.find logic_v env.var_mapping in
      Stack.push vi varinfos;
      env
    with Not_found | Stack.Empty ->
      let varinfos = Stack.create () in
      Stack.push vi varinfos;
      let var_mapping = Logic_var.Map.add logic_v varinfos env.var_mapping in
      { env with var_mapping = var_mapping }

  let add ?ty env logic_v =
    let ty = match ty with
      | Some ty -> ty
      | None -> match logic_v.lv_type with
        | Ctype ty -> ty
        | Linteger -> Gmp_types.Z.t ()
        | Ltype _ as ty when Logic_const.is_boolean_type ty -> Cil.charType
        | Ltype _ | Lvar _ | Lreal | Larrow _ as lty ->
          let msg =
            Format.asprintf
              "logic variable of type %a" Logic_type.pretty lty
          in
          Error.not_yet msg
    in
    let v, e, env =
      new_var
        ~loc:Location.unknown
        env
        ~name:logic_v.lv_name
        None
        ty
        (fun _ _ -> [])
    in
    v, e, add_binding env logic_v v

  let get env logic_v =
    try
      let varinfos = Logic_var.Map.find logic_v env.var_mapping in
      Stack.top varinfos
    with Not_found | Stack.Empty ->
      assert false

  let remove env logic_v =
    try
      let varinfos = Logic_var.Map.find logic_v env.var_mapping in
      ignore (Stack.pop varinfos)
    with Not_found | Stack.Empty ->
      assert false

end

module Logic_scope = struct
  let get env = env.lscope
  let extend env lvs = { env with lscope = Lscope.add lvs env.lscope }
  let set_reset env bool = { env with lscope_reset = bool }
  let get_reset env = env.lscope_reset
  let reset env =
    if env.lscope_reset then { env with lscope = Lscope.empty }
    else env
end

let emitter =
  Emitter.create
    "E_ACSL"
    [ Emitter.Code_annot ]
    ~correctness:[ Options.Gmp_only.parameter ]
    ~tuning:[]

let add_assert env stmt annot = match current_kf env with
  | None -> assert false
  | Some kf ->
    Queue.add
      (fun () -> Annotations.add_assert emitter ~kf stmt annot)
      env.visitor#get_filling_actions

let add_stmt ?(post=false) ?before env stmt =
  if not post then
    Extlib.may (fun old -> E_acsl_label.move env.visitor ~old stmt) before;
  let local_env, tl = top env in
  let block = local_env.block_info in
  let block =
    if post then
      { block with post_stmts = stmt :: block.post_stmts }
    else
      { block with new_stmts = stmt :: block.new_stmts }
  in
  let local_env = { local_env with block_info = block } in
  { env with env_stack = local_env :: tl }

let extend_stmt_in_place env stmt ~label block =
  let new_stmt = Cil.mkStmt ~valid_sid:true (Block block) in
  let sk = stmt.skind in
  stmt.skind <-
    Block (Cil.mkBlock [ new_stmt; Cil.mkStmt ~valid_sid:true sk ]);
    let pre = match label with
    | BuiltinLabel(Here | Post) -> true
    | BuiltinLabel(Old | Pre | LoopEntry | LoopCurrent | Init)
    | FormalLabel _ | StmtLabel _ -> false
    in
    if pre then
    let local_env, tl_env = top env in
    let b_info = local_env.block_info in
    let b_info = { b_info with pre_stmts = new_stmt :: b_info.pre_stmts } in
    { env with env_stack = { local_env with block_info = b_info } :: tl_env }
  else
    env

let push env =
(*  Options.feedback "push (was %d)" (List.length env.env_stack);*)
  { env with env_stack = empty_local_env :: env.env_stack }

let pop env =
(*  Options.feedback "pop";*)
  let _, tl = top env in
  { env with env_stack = tl }

let transfer ~from env = match from.env_stack, env.env_stack with
  | { block_info = from_blk } :: _, ({ block_info = env_blk } as local) :: tl
    ->
    let new_blk =
      { new_block_vars = from_blk.new_block_vars @ env_blk.new_block_vars;
        new_stmts = from_blk.new_stmts @ env_blk.new_stmts;
        pre_stmts = from_blk.pre_stmts @ env_blk.pre_stmts;
        post_stmts = from_blk.post_stmts @ env_blk.post_stmts }
    in
    { env with env_stack = { local with block_info = new_blk } :: tl }
  | _, _ ->
    assert false

type where = Before | Middle | After
let pop_and_get ?(split=false) env stmt ~global_clear where =
  let split = split && stmt.labels = [] in
(*  Options.feedback "pop_and_get from %a (%b)" Printer.pp_stmt stmt split;*)
  let local_env, tl = top env in
  let clear =
    if global_clear then begin
      Varname.clear_locals ();
      env.global_mp_tbl.clear_stmts @ local_env.mp_tbl.clear_stmts
    end else
      local_env.mp_tbl.clear_stmts
  in
(*  Options.feedback "clearing %d mpz (global_clear: %b)"
    (List.length clear) global_clear;*)
  let block = local_env.block_info in
  let b =
    let pre_stmts, stmt =
      let rec extract stmt acc = function
        | [] -> acc, stmt
        | _ :: tl ->
          match stmt.skind with
          | Block { bstmts = [ fst; snd ] } -> extract snd (fst :: acc) tl
          | _ ->
            Kernel.fatal
              "experting a block containing 2 statements instead of %a"
              Printer.pp_stmt stmt
      in
      extract stmt [] block.pre_stmts
    in
    let new_s = block.new_stmts in
    let cat stmt l = match stmt.skind with
      | Instr(Skip _) -> l
      | _ -> stmt :: l
    in
    let stmts =
      match where with
      | Before -> cat stmt (acc_list_rev (List.rev clear) new_s)
      | Middle -> acc_list_rev (cat stmt (List.rev clear)) new_s
      | After ->
        (* if [split], do not put the given [stmt] in the generated block *)
        let stmts = if split then [] else cat stmt [] in
        acc_list_rev (acc_list_rev stmts clear) new_s
    in
    Cil.mkBlock (acc_list_rev stmts pre_stmts)
  in
  b.blocals <- acc_list_rev b.blocals block.new_block_vars;
  let b =
    (* blocks with local cannot be transient (see doc in cil.ml),
       while transient blocks prevent the E-ACSL labeling strategy from working
       properly: no transient block in that cases. *)
    if b.blocals = [] && stmt.labels = [] then Cil.transient_block b
    else b
  in
  let final_blk =
    (* if [split], put the generated code in a distinct sub-block and
       add the given [stmt] afterwards. This way, we have the guarantee that
       the final block does not contain any local, so may be transient. *)
    if split then
      match stmt.skind with
      | Instr (Skip _) -> b
      | _ ->
        let sblock = Cil.mkStmt ~valid_sid:true (Block b) in
        Cil.transient_block (Cil.mkBlock [ sblock; stmt ])
    else
      b
  in
  (* remove superfluous brackets inside the generated block *)
  let final_blk = Cil.flatten_transient_sub_blocks final_blk in
  (* remove the non-scoping mark of the outermost block *)
  let final_blk = Cil.block_of_transient final_blk in
  (* add post-block statements *)
  final_blk.bstmts <- final_blk.bstmts @ block.post_stmts;
  final_blk, { env with env_stack = tl }

let get_generated_variables env = List.rev env.new_global_vars

let annotation_kind env = env.annotation_kind
let set_annotation_kind env k = { env with annotation_kind = k }

module Context = struct

  let ctx = ref []
  let save env = ctx := env.new_global_vars
  let restore env =
    if !ctx <> [] then begin
      let vars = env.new_global_vars in
      let env =
	{ env with new_global_vars =
	    List.filter
              (fun (v, scope) ->
                (match scope with
                | LGlobal | LFunction _ -> true
                | LLocal_block _ -> false)
                && List.for_all (fun (v', _) -> v != v') vars)
	      !ctx
	      @ vars }
      in
      ctx := [];
      env
    end else
      env

end

(* debugging purpose *)
let pretty fmt env =
  let local_env, _ = top env in
  Format.fprintf fmt "local new_stmts %t"
    (fun fmt ->
      List.iter
	(fun s -> Printer.pp_stmt fmt s)
	local_env.block_info.new_stmts)

(*
Local Variables:
compile-command: "make -C ../.."
End:
*)
