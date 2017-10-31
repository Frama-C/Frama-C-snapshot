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

(* Detailed description of transformations implemented in this file is
   presented in Sections 2 and 3 of the RV'17 paper "Runtime Detection of
   Temporal Memory Errors" by K. Vorobyov, N. Kosmatov, J Signoles and
   A. Jakobsson. *)

open Cil_types
open Cil_datatype

(* ************************************************************************** *)
(* Configuration {{{ *)
(* ************************************************************************** *)

let generate = ref false
(* }}} *)

(* ************************************************************************** *)
(* Types {{{ *)
(* ************************************************************************** *)

(* Type of identifier tracked by a LHS referent number *)
type flow =
  | Direct (* take origin number of RHS *)
  | Indirect (* take referent number of RHS *)
  | Copy (* Copy shadow from RHS to LHS *)
(* }}} *)

(* ************************************************************************** *)
(* Miscellaneous {{{ *)
(* ************************************************************************** *)

(* Generate a function name in the temporal analysis name space, i.e., prefixed
   by [__e_acsl_temporal_ + function name].
   NOTE: Further on, all analysis function names are used without prefix *)
let mk_api_name name = Misc.mk_api_name ("temporal_" ^ name)

let is_alloc_name fn =
  fn = "malloc" || fn = "free" || fn = "realloc" || fn = "calloc"

let is_memcpy_name fn = fn = "memcpy"

let is_memset_name fn = fn = "memset"

let get_fname = function
  | { enode = Lval(Var(vi), _) } -> vi.vname
  | _ -> ""

let is_fn f fname = f (get_fname fname)

let is_alloc fvi = is_fn is_alloc_name fvi

let is_memcpy fvi = is_fn is_memcpy_name fvi

let is_memset fvi = is_fn is_memset_name fvi

(* True if a named function has a definition and false otherwise *)
let has_fundef fname =
  let recognize fn =
    try let _ = Globals.Functions.find_def_by_name fn in true
    with Not_found -> false
  in
  is_fn recognize fname

(* Shortcuts for SA in Mmodel_analysis *)
let must_model_exp exp env =
  let kf, bhv = Extlib.the (Env.current_kf env), Env.get_behavior env in
  Mmodel_analysis.must_model_exp ~bhv ~kf exp

let must_model_lval lv env =
  let kf, bhv = Extlib.the (Env.current_kf env), Env.get_behavior env in
  Mmodel_analysis.must_model_lval ~bhv ~kf lv

let must_model_vi vi env =
  let kf, bhv = Extlib.the (Env.current_kf env), Env.get_behavior env in
  Mmodel_analysis.must_model_vi ~bhv ~kf vi

(* }}} *)

(*  ************************************************************************* *)
(* Generate analysis function calls {{{ *)
(* ************************************************************************** *)

module Mk: sig
  (* Generate either
      - [store_nblock(lhs, rhs)], or
      - [store_nreferent(lhs, rhs)]
     function call based on the value of [flow] *)
  val store_reference: loc:location -> flow -> lval -> exp -> stmt

  (* Generate a [save_*_parameter] call *)
  val save_param: loc:location -> flow -> exp -> int -> stmt

  (* Generate [pull_parameter] call *)
  val pull_param: loc:location -> varinfo -> int -> stmt

  (* Generate [(save|pull)_return(lhs, param_no)] call *)
  val handle_return_referent: save:bool -> loc:location -> exp -> stmt

  (* Generate [reset_return()] call *)
  val reset_return_referent: loc:location -> stmt

  (* Generate [memcpy(lhs, rhs, size)] function call assuming that [lhs = rhs]
     represents an assignment of struct to a struct, that is, both sides are left
     values and we need to use addressof for both sides *)
  val temporal_memcpy_struct: loc:location -> lval -> exp -> stmt
end = struct

  let store_reference ~loc flow lhs rhs =
    let fname = match flow with
      | Direct -> "store_nblock"
      | Indirect -> "store_nreferent"
      | Copy -> Options.fatal "Copy flow type in store_reference"
    in
    Misc.mk_call ~loc (mk_api_name fname) [ Cil.mkAddrOf ~loc lhs; rhs ]

  let save_param ~loc flow lhs pos =
    let infix = match flow with
      | Direct -> "nblock"
      | Indirect -> "nreferent"
      | Copy -> "copy"
    in
    let fname = "save_" ^ infix ^ "_parameter" in
    Misc.mk_call ~loc (mk_api_name fname) [ lhs ; Cil.integer ~loc pos ]

  let pull_param ~loc vi pos =
    let exp = Cil.mkAddrOfVi vi in
    let fname = mk_api_name "pull_parameter" in
    let sz = Cil.kinteger ~loc IULong (Cil.bytesSizeOf vi.vtype) in
    Misc.mk_call ~loc fname [ exp ; Cil.integer ~loc pos ; sz ]

  let handle_return_referent ~save ~loc lhs =
    let fname = match save with
      | true -> "save_return"
      | false -> "pull_return"
    in
    (* TODO: Returning structs is unsupported so far *)
    (match (Cil.typeOf lhs) with
      | TPtr _ -> ()
      | _ -> Error.not_yet "Struct in return");
    Misc.mk_call ~loc (mk_api_name fname) [ lhs ]

  let reset_return_referent ~loc =
    Misc.mk_call ~loc (mk_api_name "reset_return") []

  let temporal_memcpy_struct ~loc lhs rhs =
    let fname  = mk_api_name "memcpy" in
    let size = Cil.sizeOf ~loc (Cil.typeOfLval lhs) in
    Misc.mk_call ~loc fname [ Cil.mkAddrOf ~loc lhs; rhs; size ]
end
(* }}} *)

(* ************************************************************************** *)
(* Handle assignments {{{ *)
(* ************************************************************************** *)

(* Given an lvalue [lhs] representing LHS of an assignment, and an expression
  [rhs] representing its RHS compute triple (l,r,f), such that:
   - lval [l] and exp [r] are addresses of a pointer and a memory block, and
   - flow [f] indicates how to update the meta-data of [l] using information
    stored by [r]. The values of [f] indicate the following
     + Direct - referent number of [l] is assigned the referent number of [r]
     + Indirect - referent number of [l] is assigned the origin number of [r]
     + Copy - metadata of [r] is copied to metadata of [l] *)
let assign ?(ltype) lhs rhs loc =
  (* Do not use [Extlib.opt_conv] here, application of the [None] part should
     not be evaluated at this point, as otherwise it will lead to an exception
     via [Cil.typeOfLval] later *)
  let ltype = match ltype with
    | Some l -> l
    | None -> (Cil.typeOfLval lhs)
  in
  match Cil.unrollType ltype with
  | TPtr _ ->
    let base, _ = Misc.ptr_index rhs in
    let rhs, flow =
      (match base.enode with
      | AddrOf _
      | StartOf _ -> rhs, Direct
      (* Unary operator describes !, ~ or -: treat it same as Const since
         it implies integer or logical operations. This case is rare but
         happens: for instance in Gap SPEC CPU benchmark the returned pointer
         is assigned -1 (for whatever bizarre reason) *)
      | Const _ | UnOp _ -> base, Direct
      (* Special case for literal strings which E-ACSL rewrites into
         global variables: take the origin number of a string *)
      | Lval(Var vi, _) when Misc.is_generated_varinfo vi -> base, Direct
      (* Lvalue of a pointer type can be a cast of an integral type, for
         instance for the case when address is taken by value (shown via the
         following example).
           uintptr_t addr = ...;
           char *p = (char* )addr;
         If this is the case then the analysis takes the value of a variable. *)
      | Lval lv ->
        if Cil.isPointerType (Cil.unrollType (Cil.typeOfLval lv)) then
          Cil.mkAddrOf ~loc lv, Indirect
        else
          rhs, Direct
      (* Binary operation which yields an integer (or FP) type.
         Since LHS is of pointer type we assume that the whole integer
         expression computes to an address for which there is no
         outer container, so the only thing to do is to take origin number *)
      | BinOp(op, _, _, _) ->
        (* At this point [ptr_index] should have split pointer arithmetic into
           base pointer and index so there should be no pointer arithmetic
           operations there. The following bit is to make sure of it. *)
        (match op with
          | MinusPI | PlusPI | IndexPI -> assert false
          | _ -> ());
        base, Direct
      | _ -> assert false)
    in Some (lhs, rhs, flow)
  | TNamed _ -> assert false
  | TInt _ | TFloat _ | TEnum _ -> None
  | TComp _ ->
    let rhs = match rhs.enode with
    | AddrOf _ -> rhs
    | Lval lv -> Cil.mkAddrOf ~loc lv
    | Const _ | SizeOf _ | SizeOfE _ | SizeOfStr _ | AlignOf _ | AlignOfE _
    | UnOp _ | BinOp _ | CastE _ | StartOf _ | Info _ ->
      Options.abort "unsupported RHS %a" Printer.pp_exp rhs
    in Some (lhs, rhs, Copy)
  (* va_list is a builtin type, we assume it has no pointers here and treat
     it as a "big" integer rather than a struct *)
  | TBuiltin_va_list _ -> None
  | TArray _ -> Some (lhs, rhs, Direct)
  (* void type should not happen as we are dealing with assignments *)
  | TVoid _ -> Options.fatal "Void type in assignment"
  | TFun _ -> Options.fatal "TFun type in assignment"

(* Generate a statement tracking temporal metadata associated with assignment
   [lhs] = [rhs], where lhs is a left value and [rhs] is an expression. *)
let mk_stmt_from_assign loc lhs rhs =
  let fn (lhs, rhs, flow) = match flow with
    | Direct | Indirect -> Mk.store_reference ~loc flow lhs rhs
    | Copy -> Mk.temporal_memcpy_struct ~loc lhs rhs
  in
  Extlib.opt_map fn (assign lhs rhs loc)
(* }}} *)

(* ************************************************************************** *)
(* Handle Set instructions {{{ *)
(* ************************************************************************** *)

(* Top-level handler for Set instructions *)
let set_instr ?(post=false) current_stmt loc lhs rhs env =
  if must_model_lval lhs env then
    Extlib.may_map
      (fun stmt -> Env.add_stmt ~before:current_stmt ~post env stmt)
      ~dft:env
      (mk_stmt_from_assign loc lhs rhs)
  else
    env
(* }}} *)

(* ************************************************************************** *)
(* Handle Call instructions {{{ *)
(* ************************************************************************** *)

module Function_call: sig
  (* Top-level handler for Call instructions *)
  val instr: stmt -> lval option -> exp -> exp list -> location -> Env.t -> Env.t
end = struct

  (* Track function arguments: export referents of arguments to a global
     structure so they can be retrieved once that function is called *)
  let save_params current_stmt loc args env =
    let (env, _) = List.fold_left
      (fun (env, index) param ->
        let lv = Mem(param), NoOffset in
        let ltype = Cil.typeOf param in
        let vals = assign ~ltype lv param loc in
        Extlib.may_map
          (fun (_, rhs, flow) ->
            let env =
              if must_model_exp param env then
                let stmt = Mk.save_param ~loc flow rhs index in
                Env.add_stmt ~before:current_stmt ~post:false env stmt
              else env
            in
            (env, index+1))
          ~dft:(env, index+1)
          vals)
      (env, 0)
      args
    in env

  (* Update local environment with a statement tracking temporal metadata
     associated with assignment [ret] = [func(args)]. *)
  let call_with_ret ?(alloc=false) current_stmt loc ret env =
    let rhs = Cil.new_exp ~loc (Lval ret) in
    let vals = assign ret rhs loc in
    (* Track referent numbers of assignments via function calls.
       Library functions (i.e., with no source code available) that return
       values are considered to be functions that allocate memory. They are
       considered so because they need to be handled exactly as memory
       allocating functions, that is, the referent of the returned pointer is
       assigned the origin number associated with the return value. For
       instance, for some [p = call();] [store_nblock( *p,..)] is appended.
       Note that for this we need [Direct] flow and also dereference the
       pointer to get its number. This is done in the following statement
       (where variable [alloc] indicates whether a function is a
       memory-allocating function or not).

       Alternatively, if a function does not allocate memory and its body has
       been instrumented, then information about referent numbers should be
       stored in the internal data structure and it is retrieved using
       [pull_return] added via a call to [Mk.handle_return_referent] *)
    Extlib.may_map
      (fun (lhs, rhs, flow) ->
        let flow, rhs = match flow with
          | Indirect when alloc -> Direct, (Misc.mk_deref ~loc rhs)
          | _ -> flow, rhs
        in
        let stmt =
          if alloc then
            Mk.store_reference ~loc flow lhs rhs
          else
            Mk.handle_return_referent ~save:false ~loc (Cil.mkAddrOf ~loc lhs)
        in
        Env.add_stmt ~before:current_stmt ~post:true env stmt)
      ~dft:env
      vals

  (* Update local environment with a statement tracking temporal metadata
     associated with memcpy/memset call *)
  let call_memxxx current_stmt loc args fname env =
    if is_memcpy fname || is_memset fname then
      let stmt = Misc.mk_call ~loc (mk_api_name (get_fname fname)) args in
      Env.add_stmt ~before:current_stmt ~post:false env stmt
    else
      env

  let instr current_stmt ret fname args loc env =
    (* Add function calls to reset_parameters and reset_return before each
       function call regardless. They are not really required, as if the
       instrumentation is correct then the right parameters will be saved
       and the right parameter will be pulled at runtime. In practice, however,
       it makes sense to make this somewhat-debug-level-call. In production mode
       the implementation of the function should be empty and compiler should
       be able to optimize that code out. *)
    let stmt = Misc.mk_call ~loc (mk_api_name "reset_parameters") [] in
    let env = Env.add_stmt ~before:current_stmt ~post:false env stmt in
    let stmt = Mk.reset_return_referent ~loc in
    let env = Env.add_stmt ~before:current_stmt ~post:false env stmt in
    (* Push parameters with either a call to a function pointer or a function
        definition otherwise there is no point. *)
    let env =
      if Cil.isFunctionType (Cil.typeOf fname) || has_fundef fname then
        save_params current_stmt loc args env
      else
        env
    in
    (* Handle special cases of memcpy/memset *)
    let env = call_memxxx current_stmt loc args fname env in
    let alloc = is_alloc fname || not (has_fundef fname) in
    Extlib.may_map
      (fun lhs ->
        if must_model_lval lhs env then
          call_with_ret ~alloc current_stmt loc lhs env
        else env)
      ~dft:env
      ret
end
(* }}} *)

(* ************************************************************************** *)
(* Handle Local_init instructions {{{ *)
(* ************************************************************************** *)
module Local_init: sig
  (* Top-level handler for Local_init instructions *)
  val instr: stmt -> varinfo -> local_init -> location -> Env.t -> Env.t
end = struct

  let rec handle_init current_stmt offset loc vi init env =
    match init with
    | SingleInit exp ->
      set_instr ~post:true current_stmt loc (Var vi, offset) exp env
    | CompoundInit(_, inits) ->
      List.fold_left
        (fun acc (off, init) ->
          handle_init current_stmt (Cil.addOffset off offset) loc vi init acc)
        env
        inits

  let instr current_stmt vi li loc env =
    if must_model_vi vi env then
      match li with
      | AssignInit init ->
        handle_init current_stmt NoOffset loc vi init env
      | ConsInit(fname, args, _) ->
        let ret = Some (Cil.var vi) in
        let fname = Cil.evar ~loc fname in
        Function_call.instr current_stmt ret fname args loc env
    else env
end
(* }}} *)

(* ************************************************************************** *)
(* Track function arguments {{{ *)
(* ************************************************************************** *)

(* Update local environment with a statement tracking temporal metadata
   associated with adding a function argument to a stack frame *)
let track_argument ?(typ) param index env =
  let typ = Extlib.opt_conv param.vtype typ in
  match Cil.unrollType typ with
  | TPtr _
  | TComp _ ->
    let stmt = Mk.pull_param ~loc:Location.unknown param index in
    Env.add_stmt ~post:false env stmt
  | TInt _ | TFloat _ | TEnum _ | TBuiltin_va_list _ -> env
  | TNamed _ -> assert false
  | TVoid _ |TArray _ | TFun _ ->
    Options.fatal "Failed to handle function parameter"
(* }}} *)

(* ************************************************************************** *)
(* Handle return statements {{{ *)
(* ************************************************************************** *)

(* Update local environment [env] with statements tracking return value
   of a function. *)
let handle_return_stmt loc ret env =
  match ret.enode with
  | Lval lv ->
    if Cil.isPointerType (Cil.typeOfLval lv) then
      let exp = Cil.mkAddrOf ~loc lv in
      let stmt = Mk.handle_return_referent ~loc ~save:true exp in
      Env.add_stmt ~post:false env stmt
    else
      env
  | _ -> Options.fatal "Something other than Lval in return"

let handle_return_stmt loc ret env =
  if must_model_exp ret env then
    handle_return_stmt loc ret env
  else
    env
(* }}} *)

(* ************************************************************************** *)
(* Handle instructions {{{ *)
(* ************************************************************************** *)

(* Update local environment [env] with statements tracking
   instruction [instr] *)
let handle_instruction current_stmt instr env =
  match instr with
  | Set(lv, exp, loc) -> set_instr current_stmt loc lv exp env
  | Call(ret, fname, args, loc) ->
    Function_call.instr current_stmt ret fname args loc env
  | Local_init(vi, li, loc) -> Local_init.instr current_stmt vi li loc env
  | Asm _ -> Options.warning ~once:true ~current:true "@[Analysis is\
potentially incorrect in presence of assembly code.@]"; env
  | Skip _ | Code_annot _ -> env
(* }}} *)

(* ************************************************************************** *)
(* Initialization of globals {{{ *)
(* ************************************************************************** *)

(* Provided that [vi] is a global variable initialized by the initializer [init]
   at offset [off] return [Some stmt], where [stmt] is a statement
   tracking that initialization. If [init] does not need to be tracked than
   the return value is [None] *)
let mk_global_init ~loc vi off init env =
  let exp = match init with
    | SingleInit e -> e
    (* Compound initializers should have been thrown away at this point *)
    | _ -> Options.fatal "Unexpected ComppoundInit in global initializer"
  in
  (* Initializer expression can be a literal string, so look up the
     corresponding variable which that literal string has been converted to *)
  let exp =
    try let rec get_string e = match e.enode with
      | Const(CStr str) -> str
      | CastE(_, exp) -> get_string exp
      | _ -> raise Not_found
    in
    let str = get_string exp in
    Cil.evar ~loc (Literal_strings.find str)
  with
    (* Not a literal string: just use the expression at hand *)
    Not_found -> exp
  in
  (* The input [vi] is from the old project, so get the corresponding variable
     from the new one, otherwise AST integrity is violated *)
  let vi = Cil.get_varinfo (Env.get_behavior env) vi in
  let lv = Var vi, off in
  mk_stmt_from_assign loc lv exp
(* }}} *)

(* ************************************************************************** *)
(* Public API {{{ *)
(* ************************************************************************** *)

let enable param = generate := param

let is_enabled () = !generate

let handle_function_parameters kf env =
  if is_enabled () then
    let env, _ = List.fold_left
      (fun (env, index) param ->
        let param = Cil.get_varinfo (Env.get_behavior env) param in
        let env =
          if Mmodel_analysis.must_model_vi ~kf param then
            track_argument param index env
          else env
        in env, index + 1)
      (env, 0)
      (Kernel_function.get_formals kf)
    in env
  else
    env

let handle_stmt stmt env =
  if is_enabled () then begin
    match stmt.skind with
    | Instr instr -> handle_instruction stmt instr env
    | Return(ret, loc) -> Extlib.may_map
      (fun ret -> handle_return_stmt loc ret env) ~dft:env ret
    | Goto _ | Break _ | Continue _ | If _ | Switch _ | Loop _ | Block _
    | UnspecifiedSequence _ | Throw _ | TryCatch _ | TryFinally _
    | TryExcept _ -> env
  end else
    env

let generate_global_init vi off init env =
  if is_enabled () then
    mk_global_init ~loc:vi.vdecl vi off init env
  else
    None
(* }}} *)
