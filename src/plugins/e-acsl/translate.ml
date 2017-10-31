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

module E_acsl_label = Label
open Cil_types
open Cil_datatype

let dkey = Options.dkey_translation

let not_yet env s =
  Env.Context.save env;
  Error.not_yet s

let handle_error f env =
  let env = Error.handle f env in
  Env.Context.restore env

(* internal to [named_predicate_to_exp] but put it outside in order to not add
   extra tedious parameter.
   It is [true] iff we are currently visiting \valid. *)
let is_visiting_valid = ref false

(* ************************************************************************** *)
(* Transforming terms and predicates into C expressions (if any) *)
(* ************************************************************************** *)

let relation_to_binop = function
  | Rlt -> Lt
  | Rgt -> Gt
  | Rle -> Le
  | Rge -> Ge
  | Req -> Eq
  | Rneq -> Ne

let name_of_binop = function
  | Lt -> "lt"
  | Gt -> "gt"
  | Le -> "le"
  | Ge -> "ge"
  | Eq -> "eq"
  | Ne -> "ne"
  | LOr -> "or"
  | LAnd -> "and"
  | BOr -> "bor"
  | BXor -> "bxor"
  | BAnd -> "band"
  | Shiftrt -> "shiftr"
  | Shiftlt -> "shiftl"
  | Mod -> "mod"
  | Div -> "div"
  | Mult -> "mul"
  | PlusA -> "add"
  | MinusA -> "sub"
  | MinusPP | MinusPI | IndexPI | PlusPI -> assert false

let name_of_mpz_arith_bop = function
  | PlusA -> "__gmpz_add"
  | MinusA -> "__gmpz_sub"
  | Mult -> "__gmpz_mul"
  | Div -> "__gmpz_tdiv_q"
  | Mod -> "__gmpz_tdiv_r"
  | Lt | Gt | Le | Ge | Eq | Ne | BAnd | BXor | BOr | LAnd | LOr
  | Shiftlt | Shiftrt | PlusPI | IndexPI | MinusPI | MinusPP -> assert false

(* convert [e] in a way that it is compatible with the given typing context. *)
let add_cast ~loc ?name env ctx is_mpz_string t_opt e =
  let mk_mpz e =
    let _, e, env =
      Env.new_var
        ~loc
        ?name
        env
        t_opt
        (Gmpz.t ())
        (fun lv v -> [ Gmpz.init_set ~loc (Cil.var lv) v e ])
    in
    e, env
  in
  let e, env = if is_mpz_string then mk_mpz e else e, env in
  match ctx with
  | None -> e, env
  | Some ctx ->
    let ty = Cil.typeOf e in
    if Gmpz.is_t ctx then
      if Gmpz.is_t ty then
        e, env
      else
        (* Convert the C integer into a mpz.
           Remember: very long integer constants have been temporary converted
           into strings;
           also possible to get a non integralType (or Gmpz.t) with a non-one in
           the case of \null *)
        let e =
          if Cil.isIntegralType ty || is_mpz_string then e
          else Cil.mkCast e Cil.longType (* \null *)
        in
        mk_mpz e
    else
      (* handle a C-integer context *)
      if (Gmpz.is_t ty || is_mpz_string) then
        (* we get an mpz, but it fits into a C integer: convert it *)
        let fname, new_ty =
          if Cil.isSignedInteger ctx then
            "__gmpz_get_si", Cil.longType
          else
            "__gmpz_get_ui", Cil.ulongType
        in
        let _, e, env =
          Env.new_var
            ~loc
            ?name
            env
            None
            new_ty
            (fun v _ -> [ Misc.mk_call ~loc ~result:(Cil.var v) fname [ e ] ])
        in
        e, env
      else
        Cil.mkCastT ~force:false ~e ~oldt:ty ~newt:ctx, env

let constant_to_exp ~loc t = function
  | Integer(n, _repr) ->
    (try
       let ity = Typing.get_integer_ty t in
       match ity with
       | Typing.Other -> assert false
       | Typing.Gmp -> raise Cil.Not_representable
       | Typing.C_type kind ->
         let cast = Typing.get_cast t in
         match cast, kind with
         | Some ty, (ILongLong | IULongLong) when Gmpz.is_t ty ->
           raise Cil.Not_representable
         | (None | Some _), _ ->
           (* do not keep the initial string representation because the
              generated constant must reflect its type computed by the type
              system. For instance, when translating [INT_MAX+1], we must
              generate a [long long] addition and so [1LL]. If we keep the
              initial string representation, the kind would be ignored in the
              generated code and so [1] would be generated. *)
           Cil.kinteger64 ~loc ~kind n, false
     with Cil.Not_representable ->
       (* too big integer *)
       Cil.mkString ~loc (Integer.to_string n), true)
  | LStr s -> Cil.new_exp ~loc (Const (CStr s)), false
  | LWStr s -> Cil.new_exp ~loc (Const (CWStr s)), false
  | LChr c -> Cil.new_exp ~loc (Const (CChr c)), false
  | LReal {r_literal=s; r_nearest=f; r_lower=l; r_upper=u} ->
    if l <> u then
      Options.warning ~current:true ~once:true
	"approximating a real number by a float";
    Cil.new_exp ~loc (Const (CReal (f, FLongDouble, Some s))), false
  | LEnum e -> Cil.new_exp ~loc (Const (CEnum e)), false

let conditional_to_exp ?(name="if") loc t_opt e1 (e2, env2) (e3, env3) =
  let env = Env.pop (Env.pop env3) in
  match e1.enode with
  | Const(CInt64(n, _, _)) when Integer.is_zero n ->
    e3, Env.transfer ~from:env3 env
  | Const(CInt64(n, _, _)) when Integer.is_one n ->
    e2, Env.transfer ~from:env2 env
  | _ ->
    let ty = match t_opt with
      | None (* predicate *) -> Cil.intType
      | Some t -> Typing.get_typ t
    in
    let _, e, env =
      Env.new_var
        ~loc
        ~name
        env
        t_opt
        ty
        (fun v ev ->
          let lv = Cil.var v in
          let affect e = Gmpz.init_set ~loc lv ev e in
          let then_block, _ =
            let s = affect e2 in
            Env.pop_and_get env2 s ~global_clear:false Env.Middle
          in
          let else_block, _ =
            let s = affect e3 in
            Env.pop_and_get env3 s ~global_clear:false Env.Middle
          in
          [ Cil.mkStmt ~valid_sid:true (If(e1, then_block, else_block, loc)) ])
    in
    e, env

(* If [e] is inserted in a context of type [float] or equivalent, then an
   explicit cast must be introduced (an explicit coercion is required in C, but
   it is implicit in the logic world).
   [lty] is the type of the logic context, while [lty_t] is the logic type of
   the term which [e] comes from. *)
let cast_integer_to_float lty lty_t e =
  if Cil.isIntegralType (Cil.typeOf e) then
    let ty, correct = match lty, lty_t with
      | Ctype ty, _ -> ty, true
      | Lreal, Linteger -> Cil.longDoubleType, false
      | Lreal, _ -> Cil.longDoubleType, true
      | (Ltype _ | Lvar _ | Linteger | Larrow _), _ -> assert false
    in
    if not correct  then
      Options.warning
        "casting an integer to a long double without verification";
    Cil.mkCast ~force:false ~e ~newt:ty
  else
    e

let rec thost_to_host kf env = function
  | TVar { lv_origin = Some v } ->
    Var (Cil.get_varinfo (Env.get_behavior env) v), env, v.vname
  | TVar ({ lv_origin = None } as logic_v) ->
    Var (Env.Logic_binding.get env logic_v), env, logic_v.lv_name
  | TResult _typ ->
    let vis = Env.get_visitor env in
    let kf = Extlib.the vis#current_kf in
    let lhost = Misc.result_lhost kf in
    (match lhost with
    | Var v -> Var (Cil.get_varinfo (Env.get_behavior env) v), env, "result"
    | _ -> assert false)
  | TMem t ->
    let e, env = term_to_exp kf env t in
    Mem e, env, ""

and toffset_to_offset ?loc kf env = function
  | TNoOffset -> NoOffset, env
  | TField(f, offset) ->
    let offset, env = toffset_to_offset ?loc kf env offset in
    Field(f, offset), env
  | TIndex(t, offset) ->
    let e, env = term_to_exp kf env t in
    let offset, env = toffset_to_offset kf env offset in
    Index(e, offset), env
  | TModel _ -> not_yet env "model"

and tlval_to_lval kf env (host, offset) =
  let host, env, name = thost_to_host kf env host in
  let offset, env = toffset_to_offset kf env offset in
  let name = match offset with NoOffset -> name | Field _ | Index _ -> "" in
  (host, offset), env, name

(* the returned boolean says that the expression is an mpz_string;
   the returned string is the name of the generated variable corresponding to
   the term. *)
and context_insensitive_term_to_exp kf env t =
  let loc = t.term_loc in
  match t.term_node with
  | TConst c ->
    let c, is_mpz_string = constant_to_exp ~loc t c in
    c, env, is_mpz_string, ""
  | TLval lv ->
    let lv, env, name = tlval_to_lval kf env lv in
    Cil.new_exp ~loc (Lval lv), env, false, name
  | TSizeOf ty -> Cil.sizeOf ~loc ty, env, false, "sizeof"
  | TSizeOfE t ->
    let e, env = term_to_exp kf env t in
    Cil.sizeOf ~loc (Cil.typeOf e), env, false, "sizeof"
  | TSizeOfStr s -> Cil.new_exp ~loc (SizeOfStr s), env, false, "sizeofstr"
  | TAlignOf ty -> Cil.new_exp ~loc (AlignOf ty), env, false, "alignof"
  | TAlignOfE t ->
    let e, env = term_to_exp kf env t in
    Cil.new_exp ~loc (AlignOfE e), env, false, "alignof"
  | TUnOp(Neg | BNot as op, t') ->
    let ty = Typing.get_typ t in
    let e, env = term_to_exp kf env t' in
    if Gmpz.is_t ty then
      let name, vname = match op with
	| Neg -> "__gmpz_neg", "neg"
	| BNot -> "__gmpz_com", "bnot"
	| LNot -> assert false
      in
      let _, e, env =
	Env.new_var_and_mpz_init
	  ~loc
	  env
	  ~name:vname
	  (Some t)
	  (fun _ ev -> [ Misc.mk_call ~loc name [ ev; e ] ])
      in
      e, env, false, ""
    else
      Cil.new_exp ~loc (UnOp(op, e, ty)), env, false, ""
  | TUnOp(LNot, t) ->
    let ty = Typing.get_op t in
    if Gmpz.is_t ty then
      (* [!t] is converted into [t == 0] *)
      let zero = Logic_const.tinteger 0 in
      let ctx = Typing.get_integer_ty t in
      Typing.type_term ~use_gmp_opt:true ~ctx zero;
      let e, env =
        comparison_to_exp kf ~loc ~name:"not" env Typing.gmp Eq t zero (Some t)
      in
      e, env, false, ""
    else begin
      assert (Cil.isIntegralType ty);
      let e, env = term_to_exp kf env t in
      Cil.new_exp ~loc (UnOp(LNot, e, Cil.intType)), env, false, ""
    end
  | TBinOp(PlusA | MinusA | Mult as bop, t1, t2) ->
    let ty = Typing.get_typ t in
    let e1, env = term_to_exp kf env t1 in
    let e2, env = term_to_exp kf env t2 in
    if Gmpz.is_t ty then
      let name = name_of_mpz_arith_bop bop in
      let mk_stmts _ e = [ Misc.mk_call ~loc name [ e; e1; e2 ] ] in
      let name = name_of_binop bop in
      let _, e, env =
	Env.new_var_and_mpz_init ~loc ~name env (Some t) mk_stmts
      in
      e, env, false, ""
    else
      if Logic_typing.is_integral_type t.term_type then
        Cil.new_exp ~loc (BinOp(bop, e1, e2, ty)), env, false, ""
      else
        (* floating point context: casting the arguments potentially required *)
        let e1 = cast_integer_to_float t.term_type t1.term_type e1 in
        let e2 = cast_integer_to_float t.term_type t2.term_type e2 in
        Cil.new_exp ~loc (BinOp(bop, e1, e2, ty)),  env, false, ""
  | TBinOp(Div | Mod as bop, t1, t2) ->
    let ty = Typing.get_typ t in
    let e1, env = term_to_exp kf env t1 in
    let e2, env = term_to_exp kf env t2 in
    if Gmpz.is_t ty then
      (* TODO: preventing division by zero should not be required anymore.
         RTE should do this automatically. *)
      let ctx = Typing.get_integer_ty t in
      let t = Some t in
      let name = name_of_mpz_arith_bop bop in
      (* [TODO] can now do better since the type system got some info about
         possible values of [t2] *)
      (* guarding divisions and modulos *)
      let zero = Logic_const.tinteger 0 in
      Typing.type_term ~use_gmp_opt:true ~ctx zero;
      (* do not generate [e2] from [t2] twice *)
      let guard, env =
        let name = name_of_binop bop ^ "_guard" in
        comparison_to_exp ~loc kf env Typing.gmp ~e1:e2 ~name Eq t2 zero t
      in
      let mk_stmts _v e =
	assert (Gmpz.is_t ty);
	let vis = Env.get_visitor env in
	let kf = Extlib.the vis#current_kf in
	let cond =
	  Misc.mk_e_acsl_guard
	    (Env.annotation_kind env)
	    kf
	    guard
	    (Logic_const.prel ~loc (Req, t2, zero))
	in
	Env.add_assert env cond (Logic_const.prel (Rneq, t2, zero));
	let instr = Misc.mk_call ~loc name [ e; e1; e2 ] in
	[ cond; instr ]
      in
      let name = name_of_binop bop in
      let _, e, env = Env.new_var_and_mpz_init ~loc ~name env t mk_stmts in
      e, env, false, ""
    else
      (* no guard required since RTEs are generated separately *)
      if Logic_typing.is_integral_type t.term_type then
        Cil.new_exp ~loc (BinOp(bop, e1, e2, ty)), env, false, ""
      else
        (* floating point context: casting the arguments potentially required *)
        let e1 = cast_integer_to_float t.term_type t1.term_type e1 in
        let e2 = cast_integer_to_float t.term_type t2.term_type e2 in
        Cil.new_exp ~loc (BinOp(bop, e1, e2, ty)),  env, false, ""
  | TBinOp(Lt | Gt | Le | Ge | Eq | Ne as bop, t1, t2) ->
    (* comparison operators *)
    let ity = Typing.get_integer_op t in
    let e, env = comparison_to_exp ~loc kf env ity bop t1 t2 (Some t) in
    e, env, false, ""
  | TBinOp((Shiftlt | Shiftrt), _, _) ->
    (* left/right shift *)
    not_yet env "left/right shift"
  | TBinOp(LOr, t1, t2) ->
    (* t1 || t2 <==> if t1 then true else t2 *)
    let e1, env1 = term_to_exp kf (Env.rte env true) t1 in
    let env' = Env.push env1 in
    let res2 = term_to_exp kf (Env.push env') t2 in
    let e, env =
      conditional_to_exp ~name:"or" loc (Some t) e1 (Cil.one loc, env') res2
    in
    e, env, false, ""
  | TBinOp(LAnd, t1, t2) ->
    (* t1 && t2 <==> if t1 then t2 else false *)
    let e1, env1 = term_to_exp kf (Env.rte env true) t1 in
    let _, env2 as res2 = term_to_exp kf (Env.push env1) t2 in
    let env3 = Env.push env2 in
    let e, env =
      conditional_to_exp ~name:"and" loc (Some t) e1 res2 (Cil.zero loc, env3)
    in
    e, env, false, ""
  | TBinOp((BOr | BXor | BAnd), _, _) ->
    (* other logic/arith operators  *)
    not_yet env "missing binary bitwise operator"
  | TBinOp(PlusPI | IndexPI | MinusPI | MinusPP as bop, t1, t2) ->
    (* binary operation over pointers *)
    let ty = match t1.term_type with
      | Ctype ty -> ty
      | _ -> assert false
    in
    let e1, env = term_to_exp kf env t1 in
    let e2, env = term_to_exp kf env t2 in
    Cil.new_exp ~loc (BinOp(bop, e1, e2, ty)), env, false, ""
  | TCastE(ty, t') ->
    let e, env = term_to_exp kf env t' in
    let e, env = add_cast ~loc ~name:"cast" env (Some ty) false (Some t) e in
    e, env, false, ""
  | TLogic_coerce _ -> assert false (* handle in [term_to_exp] *)
  | TAddrOf lv ->
    let lv, env, _ = tlval_to_lval kf env lv in
    Cil.mkAddrOf ~loc lv, env, false, "addrof"
  | TStartOf lv ->
    let lv, env, _ = tlval_to_lval kf env lv in
    Cil.mkAddrOrStartOf ~loc lv, env, false, "startof"
  | Tapp(li, [], args) when Builtins.mem li.l_var_info.lv_name ->
    (* E-ACSL built-in function call *)
    let fname = li.l_var_info.lv_name in
    let args, env = (* args computed in the reverse order *)
      try
        List.fold_left
          (fun (l, env) a ->
            let e, env = term_to_exp kf env a in
            e :: l, env)
          ([], env)
          args
      with Invalid_argument _ ->
        Options.fatal "[Tapp] unexpected number of arguments when calling %s"
          fname
    in
    (* build the varinfo (as an expression) which stores the result of the
       function call. *)
    let _, e, env =
      Env.new_var
        ~loc
        ~name:(fname ^ "_app")
        env
        (Some t)
        (Misc.cty (Extlib.the li.l_type))
        (fun vi _ ->
          [ Misc.mk_call ~loc ~result:(Cil.var vi) fname (List.rev args) ])
    in
    e, env, false, "app"
  | Tapp _ ->
    not_yet env "applying logic function"
  | Tlambda _ -> not_yet env "functional"
  | TDataCons _ -> not_yet env "constructor"
  | Tif(t1, t2, t3) ->
    let e1, env1 = term_to_exp kf (Env.rte env true) t1 in
    let (_, env2 as res2) = term_to_exp kf (Env.push env1) t2 in
    let res3 = term_to_exp kf (Env.push env2) t3 in
    let e, env = conditional_to_exp loc (Some t) e1 res2 res3 in
    e, env, false, ""
  | Tat(t, BuiltinLabel Here) ->
    let e, env = term_to_exp kf env t in
    e, env, false, ""
  | Tat(t', label) ->
    (* convert [t'] to [e] in a separated local env *)
    let e, env = term_to_exp kf (Env.push env) t' in
    let e, env, is_mpz_string = at_to_exp env (Some t) label e in
    e, env, is_mpz_string, ""
  | Tbase_addr(BuiltinLabel Here, t) ->
    mmodel_call ~loc kf "base_addr" Cil.voidPtrType env t
  | Tbase_addr _ -> not_yet env "labeled \\base_addr"
  | Toffset(BuiltinLabel Here, t) ->
    mmodel_call ~loc kf "offset" Cil.intType env t
  | Toffset _ -> not_yet env "labeled \\offset"
  | Tblock_length(BuiltinLabel Here, t) ->
    mmodel_call ~loc kf "block_length" Cil.ulongType env t
  | Tblock_length _ -> not_yet env "labeled \\block_length"
  | Tnull -> Cil.mkCast (Cil.zero ~loc) (TPtr(TVoid [], [])), env, false, "null"
  | TCoerce _ -> Error.untypable "coercion" (* Jessie specific *)
  | TCoerceE _ -> Error.untypable "expression coercion" (* Jessie specific *)
  | TUpdate _ -> not_yet env "functional update"
  | Ttypeof _ -> not_yet env "typeof"
  | Ttype _ -> not_yet env "C type"
  | Tempty_set -> not_yet env "empty tset"
  | Tunion _ -> not_yet env "union of tsets"
  | Tinter _ -> not_yet env "intersection of tsets"
  | Tcomprehension _ -> not_yet env "tset comprehension"
  | Trange _ -> not_yet env "range"
  | Tlet _ -> not_yet env "let binding"

(* Convert an ACSL term into a corresponding C expression (if any) in the given
   environment. Also extend this environment in order to include the generating
   constructs. *)
and term_to_exp kf env t =
  let generate_rte = Env.generate_rte env in
  Options.feedback ~dkey ~level:4 "translating term %a (rte? %b)"
    Printer.pp_term t generate_rte;
  let env = Env.rte env false in
  let t = match t.term_node with TLogic_coerce(_, t) -> t | _ -> t in
  let e, env, is_mpz_string, name = context_insensitive_term_to_exp kf env t in
  let env = if generate_rte then translate_rte kf env e else env in
  let cast = Typing.get_cast t in
  let name = if name = "" then None else Some name in
  add_cast
    ~loc:t.term_loc
    ?name
    env
    cast
    is_mpz_string
    (Some t)
    e

(* generate the C code equivalent to [t1 bop t2]. *)
and comparison_to_exp
    ~loc ?e1 kf env ity bop ?(name=name_of_binop bop) t1 t2 t_opt =
  let e1, env = match e1 with
    | None ->
      let e1, env = term_to_exp kf env t1 in
      e1, env
    | Some e1 ->
      e1, env
  in
  let e2, env = term_to_exp kf env t2 in
  match ity with
  | Typing.Gmp ->
    let _, e, env =
      Env.new_var
        ~loc
        env
        t_opt
        ~name
        Cil.intType
        (fun v _ ->
          [ Misc.mk_call ~loc ~result:(Cil.var v) "__gmpz_cmp" [ e1; e2 ] ])
    in
    Cil.new_exp ~loc (BinOp(bop, e, Cil.zero ~loc, Cil.intType)), env
  | Typing.C_type _ | Typing.Other ->
    Cil.new_exp ~loc (BinOp(bop, e1, e2, Cil.intType)), env

(* \base_addr, \block_length and \freeable annotations *)
and mmodel_call ~loc kf name ctx env t =
  let e, env = term_to_exp kf (Env.rte env true) t in
  let _, res, env =
    Env.new_var
      ~loc
      ~name
      env
      None
      ctx
      (fun v _ ->
	[ Misc.mk_call ~loc ~result:(Cil.var v) (Misc.mk_api_name name) [ e ] ])
  in
  res, env, false, name

and get_c_term_type = function
  | Ctype ty -> ty
  | _ -> assert false

and mk_ptr_sizeof typ loc =
  match Cil.unrollType typ with
  | TPtr (t', _) -> Cil.new_exp ~loc (SizeOf t')
  | _ -> assert false

(* \valid, \offset and \initialized annotations *)
and mmodel_call_with_size ~loc kf name ctx env t =
  let e, env = term_to_exp kf (Env.rte env true) t in
  let _, res, env =
    Env.new_var
      ~loc
      ~name
      env
      None
      ctx
      (fun v _ ->
        let ty = get_c_term_type t.term_type in
        let sizeof = mk_ptr_sizeof ty loc in
        let fname = Misc.mk_api_name name in
        [ Misc.mk_call ~loc ~result:(Cil.var v) fname [ e; sizeof ] ])
  in
  res, env

and mmodel_call_valid ~loc kf name ctx env t =
  let e, env = term_to_exp kf (Env.rte env true) t in
  let base, _ = Misc.ptr_index ~loc e in
  let base_addr  = match base.enode with
    | AddrOf _ | Const _ -> Cil.zero ~loc
    | Lval(lv) | StartOf(lv) -> Cil.mkAddrOrStartOf ~loc lv
    | _ -> assert false
  in
  let _, res, env =
    Env.new_var
      ~loc
      ~name
      env
      None
      ctx
      (fun v _ ->
        let ty = get_c_term_type t.term_type in
        let sizeof = mk_ptr_sizeof ty loc in
        let fname = Misc.mk_api_name name in
        let args = [ e; sizeof; base; base_addr ] in
        [ Misc.mk_call ~loc ~result:(Cil.var v) fname args ])
  in
  res, env

and at_to_exp env t_opt label e =
  let stmt = E_acsl_label.get_stmt (Env.get_visitor env) label in
  (* generate a new variable denoting [\at(t',label)].
     That is this variable which is the resulting expression.
     ACSL typing rule ensures that the type of this variable is the same as
     the one of [e]. *)
  let loc = Stmt.loc stmt in
  let res_v, res, new_env =
    Env.new_var
      ~loc
      ~name:"at"
      ~scope:Env.Function
      env
      t_opt
      (Cil.typeOf e)
      (fun _ _ -> [])
  in
  let env_ref = ref new_env in
  (* visitor modifying in place the labeled statement in order to store [e]
     in the resulting variable at this location (which is the only correct
     one). *)
  let o = object
    inherit Visitor.frama_c_inplace
    method !vstmt_aux stmt =
      (* either a standard C affectation or an mpz one according to type of
	 [e] *)
      let new_stmt = Gmpz.init_set ~loc (Cil.var res_v) res e in
      assert (!env_ref == new_env);
      (* generate the new block of code for the labeled statement and the
	 corresponding environment *)
      let block, new_env =
	Env.pop_and_get new_env new_stmt ~global_clear:false Env.Middle
      in
      let pre = match label with
        | BuiltinLabel(Here | Post) -> true
        | BuiltinLabel(Old | Pre | LoopEntry | LoopCurrent | Init)
        | FormalLabel _ | StmtLabel _ -> false
      in
      env_ref := Env.extend_stmt_in_place new_env stmt ~pre block;
      Cil.ChangeTo stmt
  end
  in
  let bhv = Env.get_behavior new_env in
  let new_stmt = Visitor.visitFramacStmt o (Cil.get_stmt bhv stmt) in
  Cil.set_stmt bhv stmt new_stmt;
  res, !env_ref, false

(* Convert an ACSL named predicate into a corresponding C expression (if
   any) in the given environment. Also extend this environment which includes
   the generating constructs. *)
and named_predicate_content_to_exp ?name kf env p =
  let loc = p.pred_loc in
  match p.pred_content with
  | Pfalse -> Cil.zero ~loc, env
  | Ptrue -> Cil.one ~loc, env
  | Papp _ -> not_yet env "logic function application"
  | Pseparated _ -> not_yet env "\\separated"
  | Pdangling _ -> not_yet env "\\dangling"
  | Pvalid_function _ -> not_yet env "\\valid_function"
  | Prel(rel, t1, t2) ->
    let ity = Typing.get_integer_op_of_predicate p in
    comparison_to_exp ~loc kf env ity (relation_to_binop rel) t1 t2 None
  | Pand(p1, p2) ->
    (* p1 && p2 <==> if p1 then p2 else false *)
    let e1, env1 = named_predicate_to_exp kf (Env.rte env true) p1 in
    let _, env2 as res2 = named_predicate_to_exp kf (Env.push env1) p2 in
    let env3 = Env.push env2 in
    let name = match name with None -> "and" | Some n -> n in
    conditional_to_exp ~name loc None e1 res2 (Cil.zero loc, env3)
  | Por(p1, p2) ->
    (* p1 || p2 <==> if p1 then true else p2 *)
    let e1, env1 = named_predicate_to_exp kf (Env.rte env true) p1 in
    let env' = Env.push env1 in
    let res2 = named_predicate_to_exp kf (Env.push env') p2 in
    let name = match name with None -> "or" | Some n -> n in
    conditional_to_exp ~name loc None e1 (Cil.one loc, env') res2
  | Pxor _ -> not_yet env "xor"
  | Pimplies(p1, p2) ->
    (* (p1 ==> p2) <==> !p1 || p2 *)
    named_predicate_to_exp
      ~name:"implies"
      kf
      env
      (Logic_const.por ~loc ((Logic_const.pnot ~loc p1), p2))
  | Piff(p1, p2) ->
    (* (p1 <==> p2) <==> (p1 ==> p2 && p2 ==> p1) *)
    named_predicate_to_exp
      ~name:"equiv"
      kf
      env
      (Logic_const.pand ~loc
	 (Logic_const.pimplies ~loc (p1, p2),
	  Logic_const.pimplies ~loc (p2, p1)))
  | Pnot p ->
    let e, env = named_predicate_to_exp kf env p in
    Cil.new_exp ~loc (UnOp(LNot, e, Cil.intType)), env
  | Pif(t, p2, p3) ->
    let e1, env1 = term_to_exp kf (Env.rte env true) t in
    let (_, env2 as res2) = named_predicate_to_exp kf (Env.push env1) p2 in
    let res3 = named_predicate_to_exp kf (Env.push env2) p3 in
    conditional_to_exp loc None e1 res2 res3
  | Plet _ -> not_yet env "let _ = _ in _"
  | Pforall _ | Pexists _ -> Quantif.quantif_to_exp kf env p
  | Pat(p, BuiltinLabel Here) ->
    named_predicate_to_exp kf env p
  | Pat(p', label) ->
    (* convert [t'] to [e] in a separated local env *)
    let e, env = named_predicate_to_exp kf (Env.push env) p' in
    let e, env, is_string = at_to_exp env None label e in
    assert (not is_string);
    e, env
  | Pvalid_read(BuiltinLabel Here as llabel, t) as pc
  | (Pvalid(BuiltinLabel Here as llabel, t) as pc) ->
    let call_valid t =
      let name = match pc with
	| Pvalid _ -> "valid"
	| Pvalid_read _ -> "valid_read"
	| _ -> assert false
      in
      mmodel_call_valid ~loc kf name Cil.intType env t
    in
    if !is_visiting_valid then begin
      (* we already transformed \valid(t) into \initialized(&t) && \valid(t):
	 now convert this right-most valid. *)
      is_visiting_valid := false;
      call_valid t
    end else begin
      match t.term_node, t.term_type with
      | TLval tlv, Ctype ty ->
        let init =
          Logic_const.pinitialized ~loc (llabel, Misc.term_addr_of ~loc tlv ty)
        in
        Typing.type_named_predicate ~must_clear:false init;
        let p = Logic_const.pand ~loc (init, p) in
        is_visiting_valid := true;
        named_predicate_to_exp kf env p
      | _ -> call_valid t
    end
  | Pvalid _ -> not_yet env "labeled \\valid"
  | Pvalid_read _ -> not_yet env "labeled \\valid_read"
  | Pinitialized(BuiltinLabel Here, t) ->
    (match t.term_node with
    (* optimisation when we know that the initialisation is ok *)
    | TAddrOf (TResult _, TNoOffset) -> Cil.one ~loc, env
    | TAddrOf (TVar { lv_origin = Some vi }, TNoOffset)
	when vi.vformal || vi.vglob || Misc.is_generated_varinfo vi ->
      Cil.one ~loc, env
    | _ -> mmodel_call_with_size ~loc kf "initialized" Cil.intType env t)
  | Pinitialized _ -> not_yet env "labeled \\initialized"
  | Pallocable _ -> not_yet env "\\allocate"
  | Pfreeable(BuiltinLabel Here, t) ->
    let res, env, _, _ = mmodel_call ~loc kf "freeable" Cil.intType env t in
    res, env
  | Pfreeable _ -> not_yet env "labeled \\freeable"
  | Pfresh _ -> not_yet env "\\fresh"
  | Psubtype _ -> Error.untypable "subtyping relation" (* Jessie specific *)

and named_predicate_to_exp ?name kf ?rte env p =
  let rte = match rte with None -> Env.generate_rte env | Some b -> b in
  let env = Env.rte env false in
  let e, env = named_predicate_content_to_exp ?name kf env p in
  let env = if rte then translate_rte kf env e else env in
  let cast = Typing.get_cast_of_predicate p in
  add_cast
    ~loc:p.pred_loc
    ?name
    env
    cast
    false
    None
    e

and translate_rte_annots:
    'a. (Format.formatter -> 'a -> unit) -> 'a ->
  kernel_function -> Env.t -> code_annotation list -> Env.t =
  fun pp elt kf env l ->
    let old_valid = !is_visiting_valid in
    let old_kind = Env.annotation_kind env in
    let env = Env.set_annotation_kind env Misc.RTE in
    let env =
      List.fold_left
        (fun env a -> match a.annot_content with
        | AAssert(_, p) ->
	  handle_error
	    (fun env ->
	      Options.feedback ~dkey ~level:4 "prevent RTE from %a" pp elt;
	      translate_named_predicate kf (Env.rte env false) p)
	    env
        | _ -> assert false)
        env
        l
    in
    is_visiting_valid := old_valid;
    Env.set_annotation_kind env old_kind

and translate_rte kf env e =
  let stmt = Cil.mkStmtOneInstr ~valid_sid:true (Skip e.eloc) in
  let l = Rte.exp kf stmt e in
  translate_rte_annots Printer.pp_exp e kf env l

and translate_named_predicate kf env p =
  Options.feedback ~dkey ~level:3 "translating predicate %a"
    Printer.pp_predicate p;
  let rte = Env.generate_rte env in
  Typing.type_named_predicate ~must_clear:rte p;
  let e, env = named_predicate_to_exp kf ~rte env p in
  assert (Typ.equal (Cil.typeOf e) Cil.intType);
  Env.add_stmt
    env
    (Misc.mk_e_acsl_guard ~reverse:true (Env.annotation_kind env) kf e p)

let named_predicate_to_exp ?name kf env p =
  named_predicate_to_exp ?name kf env p (* forget optional argument ?rte *)

let () =
  Quantif.term_to_exp_ref := term_to_exp;
  Quantif.predicate_to_exp_ref := named_predicate_to_exp

(* This function is used by Guillaume.
   However, it is correct to use it only in specific contexts. *)
let predicate_to_exp kf p =
  Typing.type_named_predicate ~must_clear:true p;
  let empty_env = Env.empty (new Visitor.frama_c_copy Project_skeleton.dummy) in
  let e, _ = named_predicate_to_exp kf empty_env p in
  assert (Typ.equal (Cil.typeOf e) Cil.intType);
  e

exception No_simple_translation of term

(* This function is used by plug-in [Cfp]. *)
let term_to_exp typ t =
  (* infer a context from the given [typ] whenever possible *)
  let ctx_of_typ ty =
    if Gmpz.is_t ty then Typing.gmp
    else
      match ty with
      | TInt(ik, _) -> Typing.ikind ik
      | _ -> Typing.other
  in
  let ctx = Extlib.opt_map ctx_of_typ typ in
  Typing.type_term ~use_gmp_opt:true ?ctx t;
  let env = Env.empty (new Visitor.frama_c_copy Project_skeleton.dummy) in
  let env = Env.push env in
  let env = Env.rte env false in
  let e, env =
    try term_to_exp (Kernel_function.dummy ()) env t
    with Misc.Unregistered_library_function _ -> raise (No_simple_translation t)
  in
  if not (Env.has_no_new_stmt env) then raise (No_simple_translation t);
  e

(* ************************************************************************** *)
(* [translate_*] translates a given ACSL annotation into the corresponding C
   statement (if any) for runtime assertion checking *)
(* ************************************************************************** *)

let assumes_predicate bhv =
  List.fold_left
    (fun acc p ->
      let loc = p.ip_content.pred_loc in
      Logic_const.pand ~loc (acc,
                             Logic_const.unamed ~loc p.ip_content.pred_content))
    Logic_const.ptrue
    bhv.b_assumes

let original_project_ref = ref Project_skeleton.dummy
let set_original_project p = original_project_ref := p

let must_translate ppt =
  let selection =
    State_selection.of_list [ Property_status.self; Options.Valid.self ]
  in
  Project.on ~selection
    !original_project_ref
    (fun ppt ->
      match Property_status.get ppt with
      | Property_status.Best(Property_status.True, _) -> Options.Valid.get ()
      | _ -> true)
    ppt

let translate_preconditions kf kinstr env behaviors =
  let env = Env.set_annotation_kind env Misc.Precondition in
  let do_behavior env b =
    let assumes_pred = assumes_predicate b in
    List.fold_left
      (fun env p ->
         let do_it env =
           if must_translate (Property.ip_of_requires kf kinstr b p) then
             let loc = p.ip_content.pred_loc in
             let p =
               Logic_const.pimplies
                 ~loc
                 (assumes_pred,
                  Logic_const.unamed ~loc p.ip_content.pred_content)
	    in
	    translate_named_predicate kf env p
	  else
	    env
	in
	handle_error do_it env)
      env
      b.b_requires
  in
  List.fold_left do_behavior env behaviors

let translate_postconditions kf kinstr env behaviors =
  let env = Env.set_annotation_kind env Misc.Postcondition in
      (* generate one guard by postcondition of each behavior *)
  let do_behavior env b =
    let assumes_pred = assumes_predicate b in
    List.fold_left
      (fun env (t, p as post) ->
	if must_translate (Property.ip_of_ensures kf kinstr b post) then
	  let env =
	    handle_error
	      (fun env ->
		if b.b_assigns <> WritesAny then
		  not_yet env "assigns clause in behavior";
		if b.b_extended <> [] then
		  not_yet env "grammar extensions in behavior";
		env)
	      env
	  in
	  let do_it env =
	    match t with
	    | Normal ->
	      let loc = p.ip_content.pred_loc in
	      let p = p.ip_content in
	      let p =
		Logic_const.pimplies
		  ~loc
		  (Logic_const.pold ~loc assumes_pred,
		   Logic_const.unamed ~loc p.pred_content)
	      in
	      translate_named_predicate kf env p
	    | Exits | Breaks | Continues | Returns ->
	      not_yet env "@[abnormal termination case in behavior@]"
	  in
	  handle_error do_it env
	else env)
      env
      b.b_post_cond
  in
  List.fold_left do_behavior env behaviors

let translate_pre_spec kf kinstr env spec =
  let unsupported f x = ignore (handle_error (fun env -> f x; env) env) in
  let convert_unsupported_clauses env =
    unsupported
      (Extlib.may
	 (fun v ->
	   if must_translate (Property.ip_of_decreases kf kinstr v) then
	     not_yet env "variant clause"))
      spec.spec_variant;
    unsupported
      (Extlib.may
	 (fun t ->
	   if must_translate (Property.ip_of_terminates kf kinstr t) then
	     not_yet env "terminates clause"))
      spec.spec_terminates;
    (match spec.spec_complete_behaviors with
    | [] -> ()
    | l ->
      unsupported
        (List.iter
           (fun l ->
             if must_translate (Property.ip_of_complete kf kinstr ~active:[] l)
             then not_yet env "complete behaviors"))
        l);
    (match spec.spec_disjoint_behaviors with
    | [] -> ()
    | l ->
      unsupported
        (List.iter
           (fun l ->
             if must_translate (Property.ip_of_disjoint kf kinstr ~active:[] l)
             then not_yet env "disjoint behaviors"))
        l);
    env
  in
  let env = convert_unsupported_clauses env in
  handle_error
    (fun env -> translate_preconditions kf kinstr env spec.spec_behavior)
    env

let translate_post_spec kf kinstr env spec =
  handle_error
    (fun env -> translate_postconditions kf kinstr env spec.spec_behavior)
    env

let translate_pre_code_annotation kf stmt env annot =
  let convert env = match annot.annot_content with
    | AAssert(l, p) ->
      if must_translate (Property.ip_of_code_annot_single kf stmt annot) then
	let env = Env.set_annotation_kind env Misc.Assertion in
	if l <> [] then
	  not_yet env "@[assertion applied only on some behaviors@]";
	translate_named_predicate kf env p
      else
	env
    | AStmtSpec(l, spec) ->
      if l <> [] then
        not_yet env "@[statement contract applied only on some behaviors@]";
      translate_pre_spec kf (Kstmt stmt) env spec ;
    | AInvariant(l, loop_invariant, p) ->
      if must_translate (Property.ip_of_code_annot_single kf stmt annot) then
	let env = Env.set_annotation_kind env Misc.Invariant in
	if l <> [] then
	  not_yet env "@[invariant applied only on some behaviors@]";
	let env = translate_named_predicate kf env p in
	if loop_invariant then Env.add_loop_invariant env p else env
      else
	env
    | AVariant _ ->
      if must_translate (Property.ip_of_code_annot_single kf stmt annot)
      then not_yet env "variant"
      else env
    | AAssigns _ ->
      if must_translate (Property.ip_of_code_annot_single kf stmt annot)
      then not_yet env "assigns"
      else env
    | AAllocation _ ->
      if must_translate (Property.ip_of_code_annot_single kf stmt annot)
      then not_yet env "allocation"
      else env
    | APragma _ -> not_yet env "pragma"
    | AExtended _ -> env (* never translate extensions. *)
  in
  handle_error convert env

let translate_post_code_annotation kf stmt env annot =
  let convert env = match annot.annot_content with
    | AStmtSpec(_, spec) -> translate_post_spec kf (Kstmt stmt) env spec
    | AAssert _
    | AInvariant _
    | AVariant _
    | AAssigns _
    | AAllocation _
    | APragma _
    | AExtended _ -> env
  in
  handle_error convert env

(*
Local Variables:
compile-command: "make"
End:
*)
