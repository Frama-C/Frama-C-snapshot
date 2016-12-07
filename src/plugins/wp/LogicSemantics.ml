(**************************************************************************)
(*                                                                        *)
(*  This file is part of WP plug-in of Frama-C.                           *)
(*                                                                        *)
(*  Copyright (C) 2007-2016                                               *)
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
(* --- ACSL Translation                                                   --- *)
(* -------------------------------------------------------------------------- *)

open Cil_types
open Cil_datatype
open LogicBuiltins
open Clabels
open Ctypes
open Lang
open Lang.F
open Definitions
open Memory

type polarity = [ `Positive | `Negative | `NoPolarity ]

module Make(M : Memory.Model) =
struct

  open M

  type loc = M.loc
  type value = loc Memory.value
  type logic = loc Memory.logic
  type region = loc sloc list
  type sigma = Sigma.t

  module L = Cvalues.Logic(M)
  module C = LogicCompiler.Make(M)

  (* -------------------------------------------------------------------------- *)
  (* --- Frames                                                             --- *)
  (* -------------------------------------------------------------------------- *)

  type call = C.call
  type frame = C.frame
  let pp_frame = C.pp_frame
  let get_frame = C.get_frame
  let in_frame = C.in_frame
  let mem_frame = C.mem_frame
  let mem_at_frame = C.mem_at_frame
  let mem_at = C.mem_at
  let frame = C.frame
  let call = C.call
  let call_pre = C.call_pre
  let call_post = C.call_post
  let return = C.return
  let result = C.result
  let status = C.status
  let guards = C.guards

  (* -------------------------------------------------------------------------- *)
  (* --- Debugging                                                          --- *)
  (* -------------------------------------------------------------------------- *)

  let pp_logic fmt = function
    | Vexp e -> F.pp_term fmt e
    | Vloc l -> M.pretty fmt l
    | Lset _ | Vset _ -> Format.pp_print_string fmt "<set>"

  let pp_bound fmt = function None -> () | Some p -> F.pp_term fmt p

  let pp_sloc fmt = function
    | Sloc l -> M.pretty fmt l
    | Sarray(l,_,n) -> Format.fprintf fmt "@[<hov2>%a@,.(..%d)@]"
                           M.pretty l (n-1)
    | Srange(l,_,a,b) -> Format.fprintf fmt "@[<hov2>%a@,.(%a@,..%a)@]"
                           M.pretty l pp_bound a pp_bound b
    | Sdescr(xs,l,p) -> Format.fprintf fmt "@[<hov2>{ %a | %a }@]"
                          M.pretty l F.pp_pred (F.p_forall xs p)

  let pp_region fmt sloc =
    List.iter (fun s -> Format.fprintf fmt "@ %a" pp_sloc s) sloc

  (* -------------------------------------------------------------------------- *)
  (* --- Translation Environment & Recursion                                --- *)
  (* -------------------------------------------------------------------------- *)

  type env = C.env
  let new_env = C.new_env
  let move = C.move
  let sigma = C.sigma
  let call_env s = C.move (C.new_env []) s

  let logic_of_value = function
    | Val e -> Vexp e
    | Loc l -> Vloc l

  let loc_of_term env t =
    match C.logic env t with
    | Vexp e -> M.pointer_loc e
    | Vloc l -> l
    | _ ->
        Warning.error "Non-expected set of locations (%a)" Printer.pp_term t

  let val_of_term env t =
    match C.logic env t with
    | Vexp e -> e
    | Vloc l -> M.pointer_val l
    | Vset s -> Vset.concretize s
    | Lset _ ->
        Warning.error "Non-expected set of values (%a)" Printer.pp_term t
        
  let set_of_term env t = L.vset (C.logic env t)

  let collection_of_term env t =
    let v = C.logic env t in
    match v with
    | Vexp s when Logic_typing.is_set_type t.term_type ->
        let te = Logic_typing.type_of_set_elem t.term_type in
        Vset [Vset.Set(tau_of_ltype te,s)]
    | w -> w

  let term env t =
    match C.logic env t with
    | Vexp e -> e
    | Vloc l -> M.pointer_val l
    | s -> Vset.concretize (L.vset s)

  (* -------------------------------------------------------------------------- *)
  (* --- Accessing an Offset (sub field-index in a compound)                --- *)
  (* -------------------------------------------------------------------------- *)

  let rec access_offset env (v:logic) = function
    | TNoOffset -> v
    | TModel _ -> Wp_parameters.not_yet_implemented "Model field"
    | TField(f,offset) ->
        let v_f = L.map (fun r -> e_getfield r (Cfield f)) v in
        access_offset env v_f offset
    | TIndex(k,offset) ->
        let rk = C.logic env k in
        let v_k = L.apply e_get v rk in
        access_offset env v_k offset

  (* -------------------------------------------------------------------------- *)
  (* --- Updating an Offset (sub field-index in a compound)                 --- *)
  (* -------------------------------------------------------------------------- *)

  let rec update_offset env (r:term) offset (v:term) = match offset with
    | TNoOffset -> v
    | TModel _ -> Wp_parameters.not_yet_implemented "Model field"
    | TField(f,offset) ->
        let r_f = e_getfield r (Cfield f) in
        let r_fv = update_offset env r_f offset v in
        e_setfield r (Cfield f) r_fv
    | TIndex(k,offset) ->
        let k = val_of_term env k in
        let r_kv = update_offset env (e_get r k) offset v in
        e_set r k r_kv

  (* -------------------------------------------------------------------------- *)
  (* --- Shifting Location of an Offset (pointer shift)                     --- *)
  (* -------------------------------------------------------------------------- *)

  (* typ is logic-type of (load v) *)
  let rec logic_offset env typ (v:logic) = function
    | TNoOffset -> typ , v
    | TModel _ -> Wp_parameters.not_yet_implemented "Model field"
    | TField(f,offset) ->
        logic_offset env f.ftype (L.field v f) offset
    | TIndex(k,offset) ->
        let te = Cil.typeOf_array_elem typ in
        let size = Ctypes.get_array_size (Ctypes.object_of typ) in
        let obj = Ctypes.object_of te in
        let vloc = L.shift v obj ?size (C.logic env k) in
        logic_offset env te vloc offset

  (* -------------------------------------------------------------------------- *)
  (* --- Logic Variable                                                     --- *)
  (* -------------------------------------------------------------------------- *)

  type lv_value =
    | VAL of logic
    | VAR of varinfo

  let logic_var env lv =
    match lv.lv_origin with
    | None -> VAL (C.logic_var env lv)
    | Some x ->
        if x.vformal then match C.formal x with
          | Some v -> VAL (logic_of_value v)
          | None -> VAR x
        else VAR x

  (* -------------------------------------------------------------------------- *)
  (* --- Term L-Values (this means 'loading' the l-value)                   --- *)
  (* -------------------------------------------------------------------------- *)

  let load_loc env typ loc loffset =
    let te,lp = logic_offset env typ (Vloc loc) loffset in
    L.load (C.sigma env) (Ctypes.object_of te) lp

  let term_lval env (lhost,loffset) =
    match lhost with
    | TResult _ ->
        let r = C.result () in
        access_offset env (Vexp (e_var r)) loffset
    | TMem e ->
        let te = Logic_typing.ctype_of_pointed e.term_type in
        let te , lp = logic_offset env te (C.logic env e) loffset in
        L.load (C.sigma env) (Ctypes.object_of te) lp
    | TVar{lv_name="\\exit_status"} ->
        assert (loffset = TNoOffset) ; (* int ! *)
        Vexp (e_var (C.status ()))
    | TVar lv ->
        begin
          match logic_var env lv with
          | VAL v -> access_offset env v loffset
          | VAR x -> load_loc env x.vtype (M.cvar x) loffset
        end

  (* -------------------------------------------------------------------------- *)
  (* --- Address of L-Values                                                --- *)
  (* -------------------------------------------------------------------------- *)

  let addr_lval env (lhost,loffset) =
    match lhost with
    | TResult _ -> Wp_parameters.abort ~current:true "Address of \\result"
    | TMem e ->
        let te = Logic_typing.ctype_of_pointed e.term_type in
        snd (logic_offset env te (C.logic env e) loffset)
    | TVar lv ->
        begin
          match logic_var env lv with
          | VAL v ->
              Wp_parameters.abort ~current:true
                "Address of logic value (%a)@." pp_logic v
          | VAR x ->
              snd (logic_offset env x.vtype (Vloc (M.cvar x)) loffset)
        end

  (* -------------------------------------------------------------------------- *)
  (* --- Unary Operators                                                    --- *)
  (* -------------------------------------------------------------------------- *)

  (* Only integral *)
  let term_unop = function
    | Neg -> L.map_opp
    | BNot -> L.map Cint.l_not
    | LNot -> L.map e_not

  (* -------------------------------------------------------------------------- *)
  (* --- Equality                                                           --- *)
  (* -------------------------------------------------------------------------- *)

  type eqsort =
    | EQ_set
    | EQ_loc
    | EQ_plain
    | EQ_array of Matrix.matrix
    | EQ_comp of compinfo
    | EQ_incomparable

  let eqsort_of_type t =
    match Logic_utils.unroll_type t with
    | Ltype({lt_name="set"},[_]) -> EQ_set
    | Linteger | Lreal | Lvar _ | Larrow _ | Ltype _ -> EQ_plain
    | Ctype t ->
        match Ctypes.object_of t with
        | C_pointer _ -> EQ_loc
        | C_int _ | C_float _ -> EQ_plain
        | C_comp c -> EQ_comp c
        | C_array a -> EQ_array (Matrix.of_array a)

  let eqsort_of_comparison a b =
    match eqsort_of_type a.term_type , eqsort_of_type b.term_type with
    | EQ_set , _ | _ , EQ_set -> EQ_set
    | EQ_loc , EQ_loc -> EQ_loc
    | EQ_comp c1 , EQ_comp c2 ->
        if Compinfo.equal c1 c2 then EQ_comp c1 else EQ_incomparable
    | EQ_array (t1,d1) , EQ_array (t2,d2) ->
        if Ctypes.equal t1 t2 then
          match Matrix.merge d1 d2 with
          | Some d -> EQ_array(t1,d)
          | None -> EQ_incomparable
        else EQ_incomparable
    | EQ_plain , EQ_plain -> EQ_plain
    | _ -> EQ_incomparable

  let use_equal = function
    | `Negative -> Wp_parameters.ExtEqual.get ()
    | `Positive | `NoPolarity -> false

  let term_equal polarity env a b =
    match eqsort_of_comparison a b with

    | EQ_set ->
        let sa = set_of_term env a in
        let sb = set_of_term env b in
        (* TODO: should be parametric in the equality of elements *) 
       Vset.equal sa sb

    | EQ_loc ->
        let la = loc_of_term env a in
        let lb = loc_of_term env b in
        M.loc_eq la lb

    | EQ_comp c ->
        let va = val_of_term env a in
        let vb = val_of_term env b in
        if use_equal polarity
        then p_equal va vb
        else Cvalues.equal_comp c va vb

    | EQ_array m ->
        let va = val_of_term env a in
        let vb = val_of_term env b in
        if use_equal polarity
        then p_equal va vb
        else Cvalues.equal_array m va vb

    | EQ_plain ->
        p_equal (val_of_term env a) (val_of_term env b)

    | EQ_incomparable ->
        (* incomparrable terms *)
        Wp_parameters.warning ~current:true
          "@[Incomparable terms (comparison is False):@ type %a with@ type %a@]"
          Printer.pp_logic_type a.term_type
          Printer.pp_logic_type b.term_type ;
        p_false

  let term_diff polarity env a b =
    p_not (term_equal (Cvalues.negate polarity) env a b)

  let compare_term env vrel lrel a b =
    if Logic_typing.is_pointer_type a.term_type then
      lrel (loc_of_term env a) (loc_of_term env b)
    else
      vrel (val_of_term env a) (val_of_term env b)

  (* -------------------------------------------------------------------------- *)
  (* --- Term Comparison                                                    --- *)
  (* -------------------------------------------------------------------------- *)

  let exp_equal env a b =
    Vexp(e_prop (term_equal `NoPolarity env a b))

  let exp_diff env a b =
    Vexp(e_prop (term_diff `NoPolarity env a b))

  let exp_compare env vrel lrel a b =
    Vexp(e_prop (compare_term env vrel lrel a b))

  (* -------------------------------------------------------------------------- *)
  (* --- Binary Operators                                                   --- *)
  (* -------------------------------------------------------------------------- *)

  let toreal t v =
    if t then L.map Cfloat.real_of_int v else v

  let arith env fint freal a b =
    let va = C.logic env a in
    let vb = C.logic env b in
    let ta = Logic_typing.is_integral_type a.term_type in
    let tb = Logic_typing.is_integral_type b.term_type in
    if ta && tb
    then fint va vb
    else freal (toreal ta va) (toreal tb vb)

  let rec fold_assoc bop acc ts = 
    match ts with
    | [] -> acc
    | t::others -> 
        match t.term_node with
        | TBinOp(binop,a,b) when bop == binop ->
                fold_assoc bop acc (a::b::others)
        | _  -> fold_assoc bop (t::acc) others
    
  let term_binop env binop a b =
    match binop with
    | PlusA -> arith env L.apply_add (L.apply F.e_add) a b
    | MinusA -> arith env L.apply_sub (L.apply F.e_sub) a b
    | Mult -> arith env (L.apply e_mul) (L.apply F.e_mul) a b
    | Div -> arith env (L.apply e_div) (L.apply F.e_div) a b
    | Mod -> L.apply e_mod (C.logic env a) (C.logic env b)
    | PlusPI | IndexPI ->
        let va = C.logic env a in
        let vb = C.logic env b in
        let te = Logic_typing.ctype_of_pointed a.term_type in
        L.shift va (Ctypes.object_of te) vb
    | MinusPI ->
        let va = C.logic env a in
        let vb = C.logic env b in
        let te = Logic_typing.ctype_of_pointed a.term_type in
        L.shift va (Ctypes.object_of te) (L.map_opp vb)
    | MinusPP ->
        let te = Logic_typing.ctype_of_pointed a.term_type in
        let la = loc_of_term env a in
        let lb = loc_of_term env b in
        Vexp(M.loc_diff (Ctypes.object_of te) la lb)
    | Shiftlt -> L.apply Cint.l_lsl (C.logic env a) (C.logic env b)
    | Shiftrt -> L.apply Cint.l_lsr (C.logic env a) (C.logic env b)
    | BAnd -> L.apply Cint.l_and (C.logic env a) (C.logic env b)
    | BXor -> L.apply Cint.l_xor (C.logic env a) (C.logic env b)
    | BOr -> L.apply Cint.l_or (C.logic env a) (C.logic env b)
    | LAnd -> Vexp(e_and (List.map (val_of_term env) (fold_assoc LAnd [] [a;b])))
    | LOr  -> Vexp(e_or  (List.map (val_of_term env) (fold_assoc LOr  [] [a;b])))
    | Lt -> exp_compare env p_lt M.loc_lt a b
    | Gt -> exp_compare env p_lt M.loc_lt b a
    | Le -> exp_compare env p_leq M.loc_leq a b
    | Ge -> exp_compare env p_leq M.loc_leq b a
    | Eq -> exp_equal env a b
    | Ne -> exp_diff env a b

  (* -------------------------------------------------------------------------- *)
  (* --- Term Cast                                                          --- *)
  (* -------------------------------------------------------------------------- *)

  type cvsort =
    | L_bool
    | L_real
    | L_integer
    | L_cint of c_int
    | L_cfloat of c_float
    | L_pointer of typ

  let rec cvsort_of_type t =
    match Logic_utils.unroll_type t with
    | Ltype({lt_name="set"},[t]) -> cvsort_of_type t
    | Ltype _ as b when Logic_const.is_boolean_type b -> L_bool
    | Linteger -> L_integer
    | Lreal -> L_real
    | Ctype c ->
        begin
          match Ctypes.object_of c with
          | C_int i -> L_cint i
          | C_float f -> L_cfloat f
          | C_pointer te -> L_pointer te
          | C_array a -> L_pointer a.arr_element
          | obj -> Warning.error "cast from (%a) not implemented yet"
                     Ctypes.pretty obj
        end
    | _ -> Warning.error "cast from (%a) not implemented yet"
             Printer.pp_logic_type t

  let term_cast env typ t =
    match Ctypes.object_of typ , cvsort_of_type t.term_type with
    | C_int i , L_cint i0 ->
        let v = C.logic env t in
        if (Ctypes.sub_c_int i0 i) then v
        else L.map (Cint.convert i) v
    | C_int i , L_integer ->
        L.map (Cint.convert i) (C.logic env t)
    | C_int i , L_pointer _ ->
        L.map_l2t (M.int_of_loc i) (C.logic env t)
    | C_int i , (L_cfloat _ | L_real) ->
        L.map (Cint.of_real i) (C.logic env t)
    | C_float f , (L_cfloat _ | L_real) ->
        L.map (Cfloat.convert f) (C.logic env t)
    | C_float f , (L_cint _ | L_integer) ->
        L.map (Cfloat.float_of_int f) (C.logic env t)
    | C_pointer ty , L_pointer t0 ->
        let value = C.logic env t in
        let o_src = Ctypes.object_of t0 in
        let o_dst = Ctypes.object_of ty in
        if Ctypes.compare o_src o_dst = 0
        then value
        else L.map_loc (M.cast { pre=o_src ; post=o_dst }) value
    | C_pointer ty , (L_integer | L_cint _) ->
        let obj = Ctypes.object_of ty in
        L.map_t2l (M.loc_of_int obj) (C.logic env t)
    | C_int _ , L_bool ->
        L.map Cvalues.bool_val (C.logic env t)
    | _ ->
        Warning.error "Cast from (%a) to (%a) not implemented yet"
          Printer.pp_logic_type t.term_type Printer.pp_typ typ

  (* -------------------------------------------------------------------------- *)
  (* --- Environment Binding                                                --- *)
  (* -------------------------------------------------------------------------- *)

  let bind_quantifiers (env:env) qs =
    let rec acc xs env hs = function
      | [] -> List.rev xs , env , hs
      | v::vs ->
          let t = Lang.tau_of_ltype v.lv_type in
          let x = Lang.freshvar ~basename:v.lv_name t in
          let h =
            if Wp_parameters.SimplifyForall.get ()
            then F.p_true
            else Cvalues.has_ltype v.lv_type (e_var x)
          in
          let e = C.env_let env v (Vexp (e_var x)) in
          acc (x::xs) e (h::hs) vs in
    acc [] env [] qs

  (* -------------------------------------------------------------------------- *)
  (* --- Term Nodes                                                         --- *)
  (* -------------------------------------------------------------------------- *)

  let rec term_node (env:env) t =
    match t.term_node with
    | TConst c -> Vexp (Cvalues.logic_constant c)
    | TSizeOf _ | TSizeOfE _ | TSizeOfStr _ | TAlignOf _ | TAlignOfE _ ->
        Vexp (Cvalues.constant_term t)

    | TLval lval -> term_lval env lval
    | TAddrOf lval | TStartOf lval -> addr_lval env lval

    | TUnOp(Neg,t) when not (Logic_typing.is_integral_type t.term_type) ->
        L.map F.e_opp (C.logic env t)
    | TUnOp(unop,t) -> term_unop unop (C.logic env t)
    | TBinOp(binop,a,b) -> term_binop env binop a b

    | TCastE(ty,t) -> term_cast env ty t

    | Tapp(f,ls,ts) ->
        begin
          match LogicBuiltins.logic f with
          | ACSLDEF ->
              let es = List.map (val_of_term env) ts in
              Vexp( C.call_fun env f ls es )
          | LFUN phi ->
              let vs = List.map (val_of_term env) ts in
              Vexp( e_fun phi vs )
        end

    | Tlambda _ ->
        Warning.error "Lambda-functions not yet implemented"

    | TDataCons({ctor_name="\\true"},_) -> Vexp(e_true)
    | TDataCons({ctor_name="\\false"},_) -> Vexp(e_false)

    | TDataCons(c,ts) ->
        let es = List.map (val_of_term env) ts in
        begin
          match LogicBuiltins.ctor c with
          | ACSLDEF -> Vexp( e_fun (CTOR c) es )
          | LFUN phi -> Vexp( e_fun phi es )
        end

    | Tif( cond , a , b ) ->
        let c = val_of_term env cond in
        let a = val_of_term env a in
        let b = val_of_term env b in
        Vexp (e_if c a b)

    | Tat( t , label ) ->
        let clabel = Clabels.c_label label in
        C.logic (C.env_at env clabel) t

    | Tbase_addr (label,t) ->
        ignore label ;
        L.map_loc M.base_addr (C.logic env t)

    | Toffset (label, _t) ->
        ignore label ;
        Warning.error "Offset construct not implemented yet"

    | Tblock_length (label,t) ->
        let obj = object_of (Logic_typing.ctype_of_pointed t.term_type) in
        let sigma = C.mem_at env (c_label label) in
        L.map_l2t (M.block_length sigma obj) (C.logic env t)

    | Tnull ->
        Vloc M.null

    | TCoerce (_,_)
    | TCoerceE (_,_) ->
        Wp_parameters.fatal "Jessie constructs"

    | TUpdate(a,offset,b) ->
        Vexp (update_offset env (val_of_term env a) offset (val_of_term env b))

    | Tempty_set -> Vset []
    | Tunion ts ->
        L.union t.term_type (List.map (collection_of_term env) ts)
    | Tinter ts ->
        L.inter t.term_type (List.map (collection_of_term env) ts)
    | Tcomprehension(t,qs,cond) ->
        begin
          let xs,env,domain = bind_quantifiers env qs in
          let condition = match cond with
            | None -> p_conj domain
            | Some p ->
                let cc = C.pred `NoPolarity env in
                let p = Lang.without_assume cc p in
                p_conj (p :: domain)
          in match C.logic env t with
          | Vexp e -> Vset[Vset.Descr(xs,e,condition)]
          | Vloc l -> Lset[Sdescr(xs,l,condition)]
          | _ -> Wp_parameters.fatal "comprehension set of sets"
        end

    | Tlet( { l_var_info=v ; l_body=LBterm a } , b ) ->
        let va = C.logic env a in
        C.logic (C.env_let env v va) b

    | Tlet _ ->
        Warning.error "Complex let-binding not implemented yet (%a)"
          Printer.pp_term t

    | Trange(a,b) ->
        let bound env = function
          | None -> None
          | Some x -> Some (val_of_term env x)
        in Vset(Vset.range (bound env a) (bound env b))

    | Ttypeof _ | Ttype _ ->
        Warning.error "Type tag not implemented yet"
    | TLogic_coerce(_,t) -> term_node env t

  (* -------------------------------------------------------------------------- *)
  (* --- Separated                                                          --- *)
  (* -------------------------------------------------------------------------- *)

  let separated_terms env ts =
    L.separated
      begin
        List.map
          (fun t ->
             let te = Logic_typing.ctype_of_pointed t.term_type in
             let obj = Ctypes.object_of te in
             obj , L.sloc (C.logic env t)
          ) ts
      end

  (* -------------------------------------------------------------------------- *)
  (* --- Relations                                                          --- *)
  (* -------------------------------------------------------------------------- *)

  let relation polarity env rel a b =
    match rel with
    | Rlt -> compare_term env p_lt M.loc_lt a b
    | Rgt -> compare_term env p_lt M.loc_lt b a
    | Rle -> compare_term env p_leq M.loc_leq a b
    | Rge -> compare_term env p_leq M.loc_leq b a
    | Req -> term_equal polarity env a b
    | Rneq -> term_diff polarity env a b

  (* -------------------------------------------------------------------------- *)
  (* --- Predicates                                                         --- *)
  (* -------------------------------------------------------------------------- *)

  let valid env acs label t =
    let te = Logic_typing.ctype_of_pointed t.term_type in
    let sigma = C.mem_at env (Clabels.c_label label) in
    let addrs = C.logic env t in
    L.valid sigma acs (Ctypes.object_of te) (L.sloc addrs)

  let predicate polarity env p =
    match p.pred_content with
    | Pfalse -> p_false
    | Ptrue -> p_true
    | Pseparated ts -> separated_terms env ts
    | Prel(rel,a,b) -> relation polarity env rel a b
    | Pand(a,b) -> p_and (C.pred polarity env a) (C.pred polarity env b)
    | Por(a,b)  -> p_or (C.pred polarity env a) (C.pred polarity env b)
    | Pxor(a,b) -> p_not (p_equiv (C.pred `NoPolarity env a) (C.pred `NoPolarity env b))
    | Pimplies(a,b) ->
        let negated = Cvalues.negate polarity in
        p_imply (C.pred negated env a) (C.pred polarity env b)
    | Piff(a,b) -> p_equiv (C.pred `NoPolarity  env a) (C.pred `NoPolarity  env b)
    | Pnot a -> p_not (C.pred (Cvalues.negate polarity) env a)
    | Pif(t,a,b) ->
        p_if (p_bool (val_of_term env t))
          (C.pred polarity env a)
          (C.pred polarity env b)
    | Papp({l_var_info = {lv_name = "\\subset"}},_,ts) ->
        begin match ts with
          | [a;b] -> L.subset
                       a.term_type (C.logic env a)
                       b.term_type (C.logic env b)
          | _ -> Warning.error "\\subset requires 2 arguments"
        end
    | Papp(f,ls,ts) ->
        begin
          match C.logic_info env f with
          | Some p ->
              if ls <> [] || ts <> [] then
                Warning.error "Unexpected parameters for named predicate '%a'"
                  Logic_info.pretty f ; p
          | None ->
              match LogicBuiltins.logic f with
              | ACSLDEF ->
                  let es = List.map (val_of_term env) ts in
                  C.call_pred env f ls es
              | LFUN phi ->
                  if ls <> [] then
                    Warning.error "Unexpected labels for purely logic '%a'"
                      Logic_info.pretty f ;
                  let vs = List.map (val_of_term env) ts in
                  p_call phi vs
        end

    | Plet( { l_var_info=v ; l_body=LBterm a } , p ) ->
        let va = C.logic env a in
        C.pred polarity (C.env_let env v va) p

    | Plet( { l_var_info=v ; l_body=LBpred q } , p ) ->
        let vq = C.pred `NoPolarity env q in
        C.pred polarity (C.env_letp env v vq) p

    | Plet _ ->
        Warning.error "Complex let-inding not implemented yet (%a)"
          Printer.pp_predicate p

    | Pforall(qs,p) ->
        let xs,env,hs = bind_quantifiers env qs in
        let p = Lang.without_assume (C.pred polarity env) p in
        p_forall xs (p_hyps hs p)

    | Pexists(qs,p) ->
        let xs,env,hs = bind_quantifiers env qs in
        let p = Lang.without_assume (C.pred polarity env) p in
        p_exists xs (p_conj (p :: hs))

    | Pat(p,label) ->
        let clabel = Clabels.c_label label in
        C.pred polarity (C.env_at env clabel) p

    | Pvalid(label,t) -> valid env RW label t
    | Pvalid_read(label,t) -> valid env RD label t

    | Pvalid_function _t ->
        Warning.error
          "\\valid_function not yet implemented@\n\
           @[<hov 0>(%a)@]" Printer.pp_predicate p

    | Pallocable _ | Pfreeable _ | Pfresh _ | Pinitialized _ | Pdangling _->
        Warning.error
          "Allocation, initialization and danglingness not yet implemented@\n\
           @[<hov 0>(%a)@]" Printer.pp_predicate p

    | Psubtype _ ->
        Warning.error "Type tags not implemented yet"

  (* -------------------------------------------------------------------------- *)
  (* --- Set of locations for a term representing a set of l-values         --- *)
  (* -------------------------------------------------------------------------- *)

  let assignable_lval env lv =
    match fst lv with
    | TResult _ -> [] (* special case ! *)
    | _ -> L.sloc (addr_lval env lv)

  let assignable env t =
    match t.term_node with
    | Tempty_set -> []
    | TLval lv -> assignable_lval env lv
    | Tunion ts -> List.concat (List.map (C.region env) ts)
    | Tinter _ -> Warning.error "Intersection in assigns not implemented yet"

    | Tcomprehension(t,qs,cond) ->
        begin
          let xs,env,domain = bind_quantifiers env qs in
          let conditions = match cond with
            | None -> domain
            | Some p -> C.pred `NoPolarity env p :: domain
          in
          List.map
            (function
              | Sloc l -> Sdescr(xs,l,p_conj conditions)
              | (Sarray _ | Srange _ | Sdescr _) as sloc ->
                  let ys,l,extend = L.rdescr sloc in
                  Sdescr(xs@ys,l,p_conj (extend :: conditions))
            ) (C.region env t)
        end

    | Tat(t,label) ->
        C.region (C.env_at env (Clabels.c_label label)) t

    | Tlet( { l_var_info=v ; l_body=LBterm a } , b ) ->
        let va = C.logic env a in
        C.region (C.env_let env v va) b

    | Tlet _ ->
        Warning.error "Complex let-binding not implemented yet (%a)"
          Printer.pp_term t

    | TLogic_coerce(_,t) -> C.region env t

    | TBinOp _ | TUnOp _ | Trange _ | TUpdate _ | Tapp _ | Tif _
    | TConst _ | Tnull | TDataCons _ | Tlambda _
    | Ttype _ | Ttypeof _ | TCastE _
    | TAlignOfE _ | TAlignOf _ | TSizeOfStr _ | TSizeOfE _ | TSizeOf _
    | Tblock_length _ | Tbase_addr _ | Toffset _ | TAddrOf _ | TStartOf _
      -> Wp_parameters.abort ~current:true
           "Non-assignable term (%a)" Printer.pp_term t

    | TCoerce (_,_)
    | TCoerceE (_,_) ->
        Wp_parameters.fatal "Jessie constructs"

  (* -------------------------------------------------------------------------- *)
  (* --- Protection                                                         --- *)
  (* -------------------------------------------------------------------------- *)

  let term_handler t =
    let x = Lang.freshvar ~basename:"w" (Lang.tau_of_ltype t.term_type) in
    Cvalues.plain t.term_type (e_var x)

  let term_protected env t =
    Warning.handle
      ~handler:term_handler
      ~severe:false
      ~effect:"Hide sub-term definition"
      (term_node env) t

  let pred_protected polarity env p =
    match polarity with
    | `Positive ->
        Warning.handle
          ~effect:"Target turned to False"
          ~severe:true ~handler:(fun _ -> p_false)
          (predicate `Positive env) p
    | `Negative ->
        Warning.handle
          ~effect:"Ignored Hypothesis"
          ~severe:false ~handler:(fun _ -> p_true)
          (predicate `Negative env) p
    | `NoPolarity ->
        predicate `NoPolarity env p

  (* -------------------------------------------------------------------------- *)
  (* --- Boot Strapping                                                     --- *)
  (* -------------------------------------------------------------------------- *)

  let term_trigger env t =
    let v = term_protected env t in
    if List.mem "TRIGGER" t.term_name then
      begin
        match v with
        | Vexp e -> C.trigger (Trigger.of_term e)
        | Vloc l -> C.trigger (Trigger.of_term (M.pointer_val l))
        | _ -> Wp_parameters.warning ~current:true
                 "Can not trigger on tset"
      end ; v

  let pred_trigger positive env np =
    let p = pred_protected positive env np in
    if List.mem "TRIGGER" np.Cil_types.pred_name then
      C.trigger (Trigger.of_pred p) ; p

  let pred polarity env p =
    Context.with_current_loc p.pred_loc (pred_trigger polarity env) p

  let logic env t =
    Context.with_current_loc t.term_loc (term_trigger env) t

  let region env t =
    Context.with_current_loc t.term_loc (assignable env) t

  let () = C.bootstrap_pred pred
  let () = C.bootstrap_term term
  let () = C.bootstrap_logic logic
  let () = C.bootstrap_region region

  let lemma = C.lemma

  (* -------------------------------------------------------------------------- *)
  (* --- Regions                                                            --- *)
  (* -------------------------------------------------------------------------- *)

  let assigns_from env froms =
    List.map
      (fun ({it_content=wr},_deps) ->
         object_of_logic_type wr.term_type ,
         region env wr)
      froms

  let assigns env = function
    | WritesAny -> None
    | Writes froms -> Some (assigns_from env froms)

  let valid = L.valid
  let included = L.included
  let separated = L.separated

  let occurs_opt x = function None -> false | Some t -> F.occurs x t

  let occurs_sloc x = function
    | Sloc l -> M.occurs x l
    | Sarray(l,_,_) -> M.occurs x l
    | Srange(l,_,a,b) -> M.occurs x l || occurs_opt x a || occurs_opt x b
    | Sdescr(xs,l,p) ->
        if List.exists (Var.equal x) xs then false
        else (M.occurs x l || F.occursp x p)

  let occurs x = List.exists (occurs_sloc x)

  let vars_opt = function None -> Vars.empty | Some t -> F.vars t

  let vars_sloc = function
    | Sloc l
    | Sarray(l,_,_) ->
        M.vars l
    | Srange(l,_,a,b) ->
        Vars.union (M.vars l) (Vars.union (vars_opt a) (vars_opt b))
    | Sdescr(xs,l,p) ->
        List.fold_left
          (fun xs x -> Vars.remove x xs)
          (Vars.union (M.vars l) (F.varsp p)) xs

  let vars sloc = List.fold_left
      (fun xs s -> Vars.union xs (vars_sloc s)) Vars.empty sloc

end
