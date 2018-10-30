(**************************************************************************)
(*                                                                        *)
(*  This file is part of WP plug-in of Frama-C.                           *)
(*                                                                        *)
(*  Copyright (C) 2007-2018                                               *)
(*    CEA (Commissariat a l'energie atomique et aux energies              *)
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

(* -------------------------------------------------------------------------- *)
(* --- C-Code Translation                                                 --- *)
(* -------------------------------------------------------------------------- *)

open Cil_datatype
open Cil_types
open Ctypes
open Qed
open Sigs
open Lang
open Lang.F

module Make(M : Sigs.Model) =
struct

  module M = M

  type loc = M.loc
  type value = M.loc Sigs.value
  type sigma = M.Sigma.t
  type result = loc Sigs.result

  let pp_value fmt = function
    | Val e -> Format.fprintf fmt "Val:%a" F.pp_term e
    | Loc l-> Format.fprintf fmt "Loc:%a" M.pretty l

  let cval = function
    | Val e -> e
    | Loc l -> M.pointer_val l

  let cloc = function
    | Loc l -> l
    | Val e -> M.pointer_loc e

  (* -------------------------------------------------------------------------- *)
  (* --- Initializers                                                       --- *)
  (* -------------------------------------------------------------------------- *)

  let is_zero_int = function
    | Val e -> p_equal e e_zero
    | Loc l -> M.is_null l

  let is_zero_float = function
    | Val e -> p_equal e e_zero_real
    | Loc l -> M.is_null l

  let is_zero_ptr v = M.is_null (cloc v)

  let rec is_zero sigma obj l =
    match obj with
    | C_int _ -> is_zero_int (M.load sigma obj l)
    | C_float _ -> is_zero_float (M.load sigma obj l)
    | C_pointer _ -> is_zero_ptr (M.load sigma obj l)
    | C_comp c ->
        p_all
          (fun f -> is_zero sigma (Ctypes.object_of f.ftype) (M.field l f))
          c.cfields
    | C_array a ->
        (*TODO[LC] make zero-initializers model-dependent.
                   For instance, a[N][M] becomes a[N*M] in MemTyped,
                   but not in MemVar *)
        let x = Lang.freshvar ~basename:"k" Logic.Int in
        let k = e_var x in
        let obj = Ctypes.object_of a.arr_element in
        let range = match a.arr_flat with
          | None -> []
          | Some f -> [ p_leq e_zero k ; p_lt k (e_int f.arr_size) ] in
        let init = is_zero sigma obj (M.shift l obj k) in
        p_forall [x] (p_hyps range init)

  let is_exp_range sigma l obj a b v =
    let x = Lang.freshvar ~basename:"k" Logic.Int in
    let k = e_var x in
    let range = [ p_leq a k ; p_lt k b ] in
    let init =
      match v with
      | None -> is_zero sigma obj (M.shift l obj k)
      | Some v ->
          let elt = (M.load sigma obj (M.shift l obj k)) in
          if Ctypes.is_pointer obj then
            M.loc_eq (cloc elt) (cloc v)
          else
            p_equal (cval elt) (cval v)
    in
    p_forall [x] (p_hyps range init)

  (* -------------------------------------------------------------------------- *)
  (* --- Recursion                                                          --- *)
  (* -------------------------------------------------------------------------- *)

  let s_exp : (sigma -> exp -> value) ref = ref (fun _ _ -> assert false)
  let s_cond : (sigma -> exp -> pred) ref = ref (fun _ _ -> assert false)

  let val_of_exp env e = cval (!s_exp env e)
  let loc_of_exp env e = cloc (!s_exp env e)

  (* -------------------------------------------------------------------------- *)
  (* --- L-Values                                                           --- *)
  (* -------------------------------------------------------------------------- *)

  let loc_of_lhost env = function
    | Var x -> M.cvar x
    | Mem e -> loc_of_exp env e

  let rec loc_of_offset env l typ = function
    | NoOffset -> l
    | Field(f,offset) -> loc_of_offset env (M.field l f) f.ftype offset
    | Index(e,offset) ->
        let k = val_of_exp env e in
        let te = Cil.typeOf_array_elem typ in
        let obj = Ctypes.object_of te in
        loc_of_offset env (M.shift l obj k) te offset

  let lval env (lhost,offset) =
    loc_of_offset env (loc_of_lhost env lhost) (Cil.typeOfLhost lhost) offset

  (* -------------------------------------------------------------------------- *)
  (* --- Unary Operator                                                     --- *)
  (* -------------------------------------------------------------------------- *)

  let exp_unop env typ unop e =
    let v =
      match Ctypes.object_of typ , unop with
      | C_int i , Neg -> Cint.iopp i (val_of_exp env e)
      | C_int i , BNot -> Cint.bnot i (val_of_exp env e)
      | C_float f , Neg -> Cfloat.fopp f (val_of_exp env e)
      | C_int _ , LNot -> Cvalues.bool_eq (val_of_exp env e) e_zero
      | C_float _ , LNot -> Cvalues.bool_eq (val_of_exp env e) e_zero_real
      | C_pointer _ , LNot -> Cvalues.is_true (M.is_null (loc_of_exp env e))
      | _ ->
          Warning.error "Undefined unary operator (%a)" Printer.pp_typ typ
    in Val v

  (* -------------------------------------------------------------------------- *)
  (* --- Binary Operator                                                    --- *)
  (* -------------------------------------------------------------------------- *)

  let arith env tr iop fop e1 e2 =
    match Ctypes.object_of tr with
    | C_int i -> Val (iop i (val_of_exp env e1) (val_of_exp env e2))
    | C_float f -> Val (fop f (val_of_exp env e1) (val_of_exp env e2))
    | _ -> assert false

  let arith_int env tr iop e1 e2 =
    match Ctypes.object_of tr with
    | C_int i -> Val (iop i (val_of_exp env e1) (val_of_exp env e2))
    | _ -> assert false

  let bool_of_comp env iop lop e1 e2 =
    let t1 = Cil.typeOf e1 in
    let t2 = Cil.typeOf e2 in
    if Cil.isPointerType t1 && Cil.isPointerType t2 then
      Cvalues.is_true (lop (loc_of_exp env e1) (loc_of_exp env e2))
    else
      iop (val_of_exp env e1) (val_of_exp env e2)

  let bool_of_exp env e =
    match Ctypes.object_of (Cil.typeOf e) with
    | C_int _ -> Cvalues.bool_neq (val_of_exp env e) e_zero
    | C_float _ -> Cvalues.bool_neq (val_of_exp env e) e_zero_real
    | C_pointer _ -> Cvalues.is_false (M.is_null (loc_of_exp env e))
    | _ -> assert false

  let exp_binop env tr binop e1 e2 = match binop with
    | PlusA   -> arith env tr Cint.iadd Cfloat.fadd e1 e2
    | MinusA  -> arith env tr Cint.isub Cfloat.fsub e1 e2
    | Mult    -> arith env tr Cint.imul Cfloat.fmul e1 e2
    | Div     -> arith env tr Cint.idiv Cfloat.fdiv e1 e2
    | Mod     -> arith_int env tr Cint.imod e1 e2
    | Shiftlt -> arith_int env tr Cint.blsl e1 e2
    | Shiftrt -> arith_int env tr Cint.blsr e1 e2
    | BAnd    -> arith_int env tr Cint.band e1 e2
    | BOr     -> arith_int env tr Cint.bor  e1 e2
    | BXor    -> arith_int env tr Cint.bxor e1 e2
    | Eq      -> Val (bool_of_comp env Cvalues.bool_eq  M.loc_eq  e1 e2)
    | Ne      -> Val (bool_of_comp env Cvalues.bool_neq M.loc_neq e1 e2)
    | Lt      -> Val (bool_of_comp env Cvalues.bool_lt  M.loc_lt  e1 e2)
    | Gt      -> Val (bool_of_comp env Cvalues.bool_lt  M.loc_lt  e2 e1)
    | Le      -> Val (bool_of_comp env Cvalues.bool_leq M.loc_leq e1 e2)
    | Ge      -> Val (bool_of_comp env Cvalues.bool_leq M.loc_leq e2 e1)
    | LAnd    -> Val (Cvalues.bool_and (bool_of_exp env e1) (bool_of_exp env e2))
    | LOr     -> Val (Cvalues.bool_or  (bool_of_exp env e1) (bool_of_exp env e2))
    | PlusPI | IndexPI ->
        let te = Cil.typeOf_pointed (Cil.typeOf e1) in
        let obj = Ctypes.object_of te in
        Loc(M.shift (loc_of_exp env e1) obj (val_of_exp env e2))
    | MinusPI ->
        let te = Cil.typeOf_pointed (Cil.typeOf e1) in
        let obj = Ctypes.object_of te in
        Loc(M.shift (loc_of_exp env e1) obj (e_opp (val_of_exp env e2)))
    | MinusPP ->
        let te = Cil.typeOf_pointed (Cil.typeOf e1) in
        let obj = Ctypes.object_of te in
        Val(M.loc_diff obj (loc_of_exp env e1) (loc_of_exp env e2))

  (* -------------------------------------------------------------------------- *)
  (* --- Cast                                                               --- *)
  (* -------------------------------------------------------------------------- *)

  let cast tr te ve =
    match Ctypes.object_of tr , Ctypes.object_of te with

    | C_int ir , C_int ie ->
        let v = cval ve in
        Val( if Ctypes.sub_c_int ie ir then v else Cint.downcast ir v )

    | C_float fr , C_float fe ->
        let v = cval ve in
        Val( if Ctypes.equal_float fe fr then v else
               Cfloat.float_of_real fr (Cfloat.real_of_float fe v) )

    | C_int ir , C_float fr ->
        Val(Cint.of_real ir (Cfloat.real_of_float fr (cval ve)))

    | C_float fr , C_int _ ->
        Val(Cfloat.float_of_real fr (Cmath.real_of_int (cval ve)))

    | C_pointer tr , C_pointer te ->
        let obj_r = Ctypes.object_of tr in
        let obj_e = Ctypes.object_of te in
        if Ctypes.compare obj_r obj_e = 0
        then ve
        else Loc (M.cast {pre=obj_e;post=obj_r} (cloc ve))

    | C_pointer te , C_int _ ->
        let e = cval ve in
        Loc(if F.equal e (F.e_zero) then M.null
            else M.loc_of_int (Ctypes.object_of te) e)

    | C_int ir , C_pointer _ ->
        Val (M.int_of_loc ir (cloc ve))

    | t1, t2 when Ctypes.equal t1 t2 -> ve

    | _ ->
        Warning.error "cast (%a) into (%a) not yet implemented"
          Printer.pp_typ te Printer.pp_typ tr

  (* -------------------------------------------------------------------------- *)
  (* --- Undefined Exp                                                      --- *)
  (* -------------------------------------------------------------------------- *)

  let exp_undefined e =
    let ty = Cil.typeOf e in
    let x = Lang.freshvar ~basename:"w" (Lang.tau_of_ctype ty) in
    Val (e_var x)

  (* -------------------------------------------------------------------------- *)
  (* --- Exp-Node                                                           --- *)
  (* -------------------------------------------------------------------------- *)

  let exp_node env e =
    match e.enode with

    | Const (CStr s)  -> Loc (M.literal ~eid:e.eid (Cstring.C_str s))
    | Const (CWStr s) -> Loc (M.literal ~eid:e.eid (Cstring.W_str s))
    | Const c -> Val (Cvalues.constant c)

    | Lval lv ->
        if Cil.isVolatileLval lv &&
           Cvalues.volatile ~warn:"unsafe read-access to volatile l-value" ()
        then exp_undefined e
        else
          let loc = lval env lv in
          let typ = Cil.typeOfLval lv in
          let obj = Ctypes.object_of typ in
          let data = M.load env obj loc in
          Lang.assume (Cvalues.is_object obj data) ;
          data

    | AddrOf lv | StartOf lv -> Loc (lval env lv)

    | UnOp(op,e,ty) -> exp_unop env ty op e
    | BinOp(op,e1,e2,tr) -> exp_binop env tr op e1 e2

    | Info(e,_) -> !s_exp env e

    | AlignOfE _ | AlignOf _
    | SizeOfE _ | SizeOf _ | SizeOfStr _ -> Val (Cvalues.constant_exp e)

    | CastE(tr,e) -> cast tr (Cil.typeOf e) (!s_exp env e)

  let rec call_node env e =
    match e.enode with
    | CastE(_,e) -> call_node env e
    | AddrOf lv | StartOf lv | Lval lv -> lval env lv
    | _ -> Warning.error ~source:"call" "Unsupported function pointer"

  (* -------------------------------------------------------------------------- *)
  (* --- Exp with Error                                                     --- *)
  (* -------------------------------------------------------------------------- *)

  let exp_protected env e =
    Warning.handle
      ~handler:exp_undefined
      ~severe:false
      ~effect:"Hide sub-term definition"
      (exp_node env) e

  (* -------------------------------------------------------------------------- *)
  (* --- Condition-Node                                                     --- *)
  (* -------------------------------------------------------------------------- *)

  let eq_t is_ptr t v1 v2 =
    match v1 , v2 with
    | Loc p , Loc q -> M.loc_eq p q
    | Val a , Val b -> p_equal a b
    | _ ->
        if is_ptr t
        then M.loc_eq (cloc v1) (cloc v2)
        else p_equal (cval v1) (cval v2)

  let neq_t is_ptr t v1 v2 =
    match v1 , v2 with
    | Loc p , Loc q -> M.loc_neq p q
    | Val a , Val b -> p_neq a b
    | _ ->
        if is_ptr t
        then M.loc_neq (cloc v1) (cloc v2)
        else p_neq (cval v1) (cval v2)

  let equal_typ t v1 v2 = eq_t Cil.isPointerType t v1 v2
  let equal_obj obj v1 v2 = eq_t Ctypes.is_pointer obj v1 v2
  let not_equal_typ t v1 v2 = neq_t Cil.isPointerType t v1 v2
  let not_equal_obj obj v1 v2 = neq_t Ctypes.is_pointer obj v1 v2

  let compare env vop lop fop e1 e2 =
    let t1 = Ctypes.object_of (Cil.typeOf e1) in
    let t2 = Ctypes.object_of (Cil.typeOf e2) in
    if not (Ctypes.equal t1 t2) then
      Warning.error "Comparison with different types (%a) and (%a)"
        Ctypes.pretty t1 Ctypes.pretty t2 ;
    match t1 with
    | C_pointer _ -> lop (loc_of_exp env e1) (loc_of_exp env e2)
    | C_float f -> (fop f) (val_of_exp env e1) (val_of_exp env e2)
    | _ -> vop (val_of_exp env e1) (val_of_exp env e2)

  let cond_node env e =
    match e.enode with

    | UnOp(  LNot, e,_)     -> p_not (!s_cond env e)
    | BinOp( LAnd, e1,e2,_) -> p_and (!s_cond env e1) (!s_cond env e2)
    | BinOp( LOr,  e1,e2,_) -> p_or (!s_cond env e1) (!s_cond env e2)
    | BinOp( Eq,   e1,e2,_) -> compare env p_equal M.loc_eq Cfloat.feq e1 e2
    | BinOp( Ne,   e1,e2,_) -> compare env p_neq M.loc_neq Cfloat.fneq e1 e2
    | BinOp( Lt,   e1,e2,_) -> compare env p_lt  M.loc_lt  Cfloat.flt e1 e2
    | BinOp( Gt,   e1,e2,_) -> compare env p_lt  M.loc_lt  Cfloat.flt e2 e1
    | BinOp( Le,   e1,e2,_) -> compare env p_leq M.loc_leq Cfloat.fle e1 e2
    | BinOp( Ge,   e1,e2,_) -> compare env p_leq M.loc_leq Cfloat.fle e2 e1

    | _ ->
        begin
          match Ctypes.object_of (Cil.typeOf e) with
          | C_int _ -> p_neq (val_of_exp env e) e_zero
          | C_float _ -> p_neq (val_of_exp env e) e_zero_real
          | C_pointer _ -> p_not (M.is_null (loc_of_exp env e))
          | obj -> Warning.error "Condition from (%a)" Ctypes.pretty obj
        end

  (* -------------------------------------------------------------------------- *)
  (* --- BootStrapping                                                      --- *)
  (* -------------------------------------------------------------------------- *)

  let exp env e = Context.with_current_loc e.eloc (exp_protected env) e
  let cond env e = Context.with_current_loc e.eloc (cond_node env) e
  let call env e = Context.with_current_loc e.eloc (call_node env) e
  let result env tr = function
    | R_var x -> F.e_var x
    | R_loc l -> cval (M.load env (Ctypes.object_of tr) l)
  let return env tr e = cval (cast tr (Cil.typeOf e) (exp env e))

  let () = s_exp := exp
  let () = s_cond := cond

  let instance_of floc kf =
    M.loc_eq floc (M.cvar (Kernel_function.get_vi kf))

  (* -------------------------------------------------------------------------- *)
  (* --- Initializers                                                       --- *)
  (* -------------------------------------------------------------------------- *)

  let unchanged sa sb v =
    let obj = Ctypes.object_of v.vtype in
    let loc = M.cvar v in
    let va = M.load sa obj loc in
    let vb = M.load sb obj loc in
    equal_obj obj va vb

  let init_value ~sigma lv typ init =
    let obj = Ctypes.object_of typ in
    let outcome = Warning.catch
        ~severe:false ~effect:"Skip initializer"
        (fun () ->
           let l = lval sigma lv in
           match init with
           | Some e ->
               let v = M.load sigma obj l in
               p_equal (val_of_exp sigma e) (cval v)
           | None -> is_zero sigma obj l
        ) () in
    match outcome with
    | Warning.Failed warn -> warn , F.p_true
    | Warning.Result(warn , hyp) -> warn , hyp

  let init_range ~sigma lv typ a b value =
    let obj = Ctypes.object_of typ in
    let outcome = Warning.catch
        ~severe:false ~effect:"Skip initializer"
        (fun () ->
           let l = lval sigma lv in
           let e = Extlib.opt_map (exp sigma) value in
           is_exp_range sigma l obj (e_bigint a) (e_bigint b) e
        ) () in
    match outcome with
    | Warning.Failed warn -> warn , F.p_true
    | Warning.Result(warn , hyp) -> warn , hyp


  type warned_hyp = Warning.Set.t * Lang.F.pred

  (* Hypothesis for initialization of one variable *)
  let rec init_variable ~sigma lv init acc =
    match init with

    | SingleInit exp ->
        init_value ~sigma lv (Cil.typeOfLval lv) (Some exp) :: acc

    | CompoundInit ( ct , initl ) ->

        let len = List.length initl in
        let acc =
          match ct with
          | TArray (ty,Some {enode = (Const CInt64 (size,_,_))},_,_)
            when Integer.lt (Integer.of_int len) size  ->
              init_range ~sigma lv ty (Integer.of_int len) size None :: acc

          | TComp (cp,_,_) when len < (List.length cp.cfields) ->

              List.fold_left
                (fun acc f ->
                   if List.exists
                       (function
                         | Field(g,_),_ -> Fieldinfo.equal f g
                         | _ -> false)
                       initl
                   then acc
                   else
                     let init =
                       init_value ~sigma
                         (Cil.addOffsetLval (Field(f, NoOffset)) lv)
                         f.ftype None in
                     init :: acc)
                acc (List.rev cp.cfields)

          | _ -> acc
        in
        match ct with
        | TArray (ty,_,_,_)
          when Wp_parameters.InitWithForall.get () ->
            (* delayed: the last consecutive index have the same value
               and are not yet initialized.
                (i0,pred,il) =def \forall x. x \in [il;i0] t[x] == pred
            *)
            let make_quant acc = function
              | None -> acc
              | Some (Index({enode=Const (CInt64 (i0,_,_))}, NoOffset),exp,il)
                when Integer.lt il i0 ->
                  let i2 = Integer.succ i0 in
                  init_range ~sigma lv ty il i2 (Some exp) :: acc
              | Some (off,exp,_) ->
                  let lv = Cil.addOffsetLval off lv in
                  init_value ~sigma lv ty (Some exp) :: acc
            in
            let acc, delayed =
              List.fold_left
                (fun (acc,delayed) (off,init) ->
                   match delayed, off, init with
                   | None, Index({enode=Const (CInt64 (i0,_,_))}, NoOffset),
                     SingleInit curr ->
                       (acc,Some(off,curr,i0))
                   | Some (i0,prev,ip), Index({enode=Const (CInt64 (i,_,_))}, NoOffset),
                     SingleInit curr
                     when ExpStructEq.equal prev curr
                       && Integer.equal (Integer.pred ip) i ->
                       (acc,Some(i0,prev,i))
                   | _, _,_ ->
                       let acc = make_quant acc delayed in
                       begin match off, init with
                         | Index({enode=Const (CInt64 (i0,_,_))}, NoOffset),
                           SingleInit curr ->
                             acc, Some (off,curr,i0)
                         | _ ->
                             let lv = Cil.addOffsetLval off lv in
                             init_variable ~sigma lv init acc, None
                       end)
                (acc,None)
                (List.rev initl) in
            (make_quant acc delayed)
        | _ ->
            List.fold_left
              (fun acc (off,init) ->
                 let lv = Cil.addOffsetLval off lv in
                 init_variable ~sigma lv init acc)
              acc (List.rev initl)

  let init ~sigma v = function
    | None -> [init_value ~sigma (Cil.var v) v.vtype None]
    | Some init -> List.rev (init_variable ~sigma (Cil.var v) init [])

end
