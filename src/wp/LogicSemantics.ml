(**************************************************************************)
(*                                                                        *)
(*  This file is part of WP plug-in of Frama-C.                           *)
(*                                                                        *)
(*  Copyright (C) 2007-2014                                               *)
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

  type frame = C.frame
  let pp_frame = C.pp_frame
  let get_frame = C.get_frame
  let in_frame = C.in_frame
  let mem_frame = C.mem_frame
  let mem_at_frame = C.mem_at_frame
  let mem_at = C.mem_at
  let frame = C.frame
  let frame_copy = C.frame_copy
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
    | Sarray(l,_,s) -> Format.fprintf fmt "@[<hov2>%a@,+[%d]@]"
	M.pretty l s
    | Srange(l,_,a,b) -> Format.fprintf fmt "@[<hov2>%a@,+(%a@,..%a)@]"
	M.pretty l pp_bound a pp_bound b 
    | Sdescr _ -> Format.fprintf fmt "<descr>"

  let pp_region fmt sloc =
    List.iter (fun s -> Format.fprintf fmt "@ %a" pp_sloc s) sloc

  (* -------------------------------------------------------------------------- *)
  (* --- Translation Environment & Recursion                                --- *)
  (* -------------------------------------------------------------------------- *)

  type env = C.env
  let new_env = C.new_env
  let move = C.move
  let sigma = C.sigma
  let call s = C.move (C.new_env []) s

  let logic_of_value = function
    | Val e -> Vexp e
    | Loc l -> Vloc l

  let loc_of_term env t =
    match C.logic env t with
      | Vexp e -> M.pointer_loc e
      | Vloc l -> l
      | _ -> 
	  Wp_parameters.abort ~current:true "Unexpected set (%a)" 
	    Printer.pp_term t

  let val_of_term env t =
    match C.logic env t with
      | Vexp e -> e
      | Vloc l -> M.pointer_val l
      | _ -> 
	  Wp_parameters.abort ~current:true "Unexpected set (%a)" 
	    Printer.pp_term t

  let set_of_term env t =
    let v = C.logic env t in
    match v with
      | Vexp s when Logic_typing.is_set_type t.term_type -> 
	  let te = Logic_typing.type_of_set_elem t.term_type in
	  [Vset.Set(tau_of_ltype te,s)]
      | _ -> L.vset v

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
  let rec shift_offset env typ (v:logic) = function
    | TNoOffset -> typ , v
    | TModel _ -> Wp_parameters.not_yet_implemented "Model field"
    | TField(f,offset) ->
	shift_offset env f.ftype (L.field v f) offset
    | TIndex(k,offset) ->
	let te = Cil.typeOf_array_elem typ in
	let size = Ctypes.array_size typ in
	let obj = Ctypes.object_of te in
	let vloc = L.shift v obj ?size (C.logic env k) in
	shift_offset env te vloc offset

  (* -------------------------------------------------------------------------- *)
  (* --- --- *)
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
    let te,lp = shift_offset env typ (Vloc loc) loffset in
    L.load (C.sigma env) (Ctypes.object_of te) lp

  let term_lval env (lhost,loffset) =
    match lhost with
      | TResult _ -> 
	  let r = C.result () in
	  access_offset env (Vexp (e_var r)) loffset
      | TMem e ->
	  let te = Logic_typing.ctype_of_pointed e.term_type in
	  let te , lp = shift_offset env te (C.logic env e) loffset in
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
      | TResult _ -> Wp_parameters.fatal "Address of \\result"
      | TMem e ->
	  let te = Logic_typing.ctype_of_pointed e.term_type in
	  snd (shift_offset env te (C.logic env e) loffset)
      | TVar lv ->
	  begin
	    match logic_var env lv with
	      | VAL v -> Wp_parameters.fatal 
		  "Address of logic value (%a)@." pp_logic v
	      | VAR x ->
		  snd (shift_offset env x.vtype (Vloc (M.cvar x)) loffset)
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

  let use_equal positive =
    not positive && Wp_parameters.ExtEqual.get ()

  let term_equal positive env a b =
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
	  if use_equal positive
	  then p_equal va vb
	  else Cvalues.equal_comp c va vb
	    
      | EQ_array m ->
	  let va = val_of_term env a in
	  let vb = val_of_term env b in
	  if use_equal positive
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

  let term_diff positive env a b = p_not (term_equal (not positive) env a b)

  let compare_term env vrel lrel a b =
    if Logic_typing.is_pointer_type a.term_type then
      lrel (loc_of_term env a) (loc_of_term env b)
    else
      vrel (val_of_term env a) (val_of_term env b)

  (* -------------------------------------------------------------------------- *)
  (* --- Term Comparison                                                    --- *)
  (* -------------------------------------------------------------------------- *)

  let exp_equal env a b =
    Vexp(e_prop (term_equal true env a b))

  let exp_diff env a b =
    Vexp(e_prop (term_diff true env a b))
	
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
      | LAnd -> Vexp(e_and [val_of_term env a;val_of_term env b])
      | LOr -> Vexp(e_or [val_of_term env a;val_of_term env b])
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
    | L_real
    | L_integer
    | L_cint of c_int
    | L_cfloat of c_float
    | L_pointer of typ

  let rec cvsort_of_type t =
    match Logic_utils.unroll_type t with
      | Ltype({lt_name="set"},[t]) -> cvsort_of_type t
      | Linteger -> L_integer
      | Lreal -> L_real
      | Ctype c -> 
	  begin
	    match Ctypes.object_of c with
	      | C_int i -> L_cint i
	      | C_float f -> L_cfloat f
	      | C_pointer te -> L_pointer te
	      | C_array a -> L_pointer a.arr_element
	      | obj -> Warning.error "cast from (%a) not yet implemented" 
		  Ctypes.pretty obj
	  end
      | _ -> Warning.error "cast from (%a) not yet implemented"
	  Printer.pp_logic_type t

  let term_cast env typ t =
    match Ctypes.object_of typ , cvsort_of_type t.term_type with
      | C_int i , L_cint i0 -> 
	  let v = C.logic env t in
	  if (Ctypes.sub_c_int i0 i) then v 
	  else L.map (Cint.iconvert i) v
      | C_int i , L_integer ->
	  L.map (Cint.iconvert i) (C.logic env t)
      | C_int i , L_pointer _ ->
	  L.map_l2t (M.int_of_loc i) (C.logic env t)
      | C_int i , (L_cfloat _ | L_real) ->
	  L.map (Cint.of_real i) (C.logic env t)
      | C_float f , (L_cfloat _ | L_real) ->
	  L.map (Cfloat.fconvert f) (C.logic env t)
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
      | _ ->
	  Warning.error "Cast from (%a) to (%a) not yet implemented"
	    Printer.pp_typ typ Printer.pp_logic_type t.term_type

  (* -------------------------------------------------------------------------- *)
  (* --- Environment Binding                                                --- *)
  (* -------------------------------------------------------------------------- *)

  let bind_quantifiers (env:env) qs =
    let rec acc xs env hs = function
      | [] -> List.rev xs , env , hs
      | v::vs ->
	  let t = Lang.tau_of_ltype v.lv_type in
	  let x = Lang.freshvar ~basename:v.lv_name t in
	  let h = Cvalues.has_ltype v.lv_type (e_var x) in
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
	      | CONST e -> Vexp e
	      | LFUN phi ->
		  let vs = List.map (val_of_term env) ts in
		  Vexp( e_fun phi vs )
	  end

      | Tlambda _ ->
	  Warning.error "Lambda-functions not yet implemented"

      | TDataCons(c,ts) ->
	  let es = List.map (val_of_term env) ts in
	  begin
	    match LogicBuiltins.ctor c with
	      | ACSLDEF -> Vexp( e_fun (CTOR c) es )
	      | CONST e -> Vexp e
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
		  let p = Lang.without_assume (C.pred true env) p in
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

  let relation positive env rel a b =
    match rel with
      | Rlt -> compare_term env p_lt M.loc_lt a b
      | Rgt -> compare_term env p_lt M.loc_lt b a
      | Rle -> compare_term env p_leq M.loc_leq a b
      | Rge -> compare_term env p_leq M.loc_leq b a
      | Req -> term_equal positive env a b
      | Rneq -> term_diff positive env a b

  (* -------------------------------------------------------------------------- *)
  (* --- Predicates                                                         --- *)
  (* -------------------------------------------------------------------------- *)

  let valid env acs label t =
    let te = Logic_typing.ctype_of_pointed t.term_type in
    let sigma = C.mem_at env (Clabels.c_label label) in
    let addrs = C.logic env t in
    L.valid sigma acs (Ctypes.object_of te) (L.sloc addrs)
    
  let predicate positive env p = 
    match p.content with
      | Pfalse -> p_false
      | Ptrue -> p_true
      | Pseparated ts -> separated_terms env ts
      | Prel(rel,a,b) -> relation positive env rel a b
      | Pand(a,b) -> p_and (C.pred positive env a) (C.pred positive env b)
      | Por(a,b)  -> p_or (C.pred positive env a) (C.pred positive env b)
      | Pxor(a,b) -> p_not (p_equiv (C.pred positive env a) (C.pred positive env b))
      | Pimplies(a,b) -> p_imply (C.pred (not positive) env a) (C.pred positive env b)
      | Piff(a,b) -> p_equiv (C.pred positive env a) (C.pred positive env b)
      | Pnot a -> p_not (C.pred (not positive) env a)
      | Pif(t,a,b) -> 
	  p_if (p_bool (val_of_term env t)) 
	    (C.pred positive env a) 
	    (C.pred positive env b)

      | Papp(f,ls,ts) ->
	  begin
	    match LogicBuiltins.logic f with
	      | ACSLDEF ->
		  let es = List.map (val_of_term env) ts in
		  C.call_pred env f ls es
	      | CONST e -> p_bool e
	      | LFUN phi ->
		  let vs = List.map (val_of_term env) ts in
		  p_call phi vs
	  end
	    
      | Plet( { l_var_info=v ; l_body=LBterm a } , p ) ->
	  let va = C.logic env a in
	  C.pred positive (C.env_let env v va) p
	    
      | Plet _ ->
	  Warning.error "Complex let-inding not implemented yet (%a)"
	    Printer.pp_predicate_named p

      | Pforall(qs,p) ->
	  let xs,env,hs = bind_quantifiers env qs in
	  let p = Lang.without_assume (C.pred positive env) p in
	  p_forall xs (p_hyps hs p)

      | Pexists(qs,p) ->
	  let xs,env,hs = bind_quantifiers env qs in
	  let p = Lang.without_assume (C.pred positive env) p in
	  p_exists xs (p_conj (p :: hs))

      | Pat(p,label) ->
	  let clabel = Clabels.c_label label in
	  C.pred positive (C.env_at env clabel) p

      | Pvalid(label,t) -> valid env RW label t
      | Pvalid_read(label,t) -> valid env RD label t

      | Pallocable _ | Pfreeable _ | Pfresh _ | Pinitialized _ ->
	  Warning.error "Allocable, Freeable, Valid_read, Fresh and Initialized not yet implemented (%a)"
	    Printer.pp_predicate_named p

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
      | TLval lv | TStartOf lv -> assignable_lval env lv
      | Tunion ts -> List.concat (List.map (C.region env) ts)
      | Tinter _ -> Warning.error "Intersection in assigns not implemented yet"

      | Tcomprehension(t,qs,cond) ->
	  begin
	    let xs,env,domain = bind_quantifiers env qs in
	    let conditions = match cond with
	      | None -> domain
	      | Some p -> C.pred true env p :: domain
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

      | TCastE(_,t) -> C.region env t
      | TLogic_coerce(_,t) -> C.region env t

      | TBinOp _ | TUnOp _ | Trange _ | TUpdate _ | Tapp _ | Tif _
      | TConst _ | Tnull | TDataCons _ | Tlambda _
      | Ttype _ | Ttypeof _
      | TAlignOfE _ | TAlignOf _ | TSizeOfStr _ | TSizeOfE _ | TSizeOf _
      | Tblock_length _ | Tbase_addr _ | Toffset _ | TAddrOf _ 
	  -> Wp_parameters.fatal "Non-assignable term (%a)" Printer.pp_term t
	    
      | TCoerce (_,_)
      | TCoerceE (_,_) ->
          Wp_parameters.fatal "Jessie constructs"

  (* -------------------------------------------------------------------------- *)
  (* --- Boot Strapping                                                     --- *)
  (* -------------------------------------------------------------------------- *)

  let term_trigger env t =
    let v = term_node env t in
    if List.mem "TRIGGER" t.term_name then
      begin
	match v with
	  | Vexp e -> C.trigger (Trigger.of_term e)
	  | Vloc l -> C.trigger (Trigger.of_term (M.pointer_val l))
	  | _ -> Wp_parameters.warning ~current:true 
	      "Can not trigger on tset"
      end ; v

  let pred_trigger positive env np =
    let p = predicate positive env np in
    if List.mem "TRIGGER" np.Cil_types.name then
      C.trigger (Trigger.of_pred p) ; p

  let pred ~positive env p = Context.with_current_loc p.loc 
    (pred_trigger positive env) p
  let logic env t = Context.with_current_loc t.term_loc (term_trigger env) t
  let region env t = Context.with_current_loc t.term_loc (assignable env) t

  let () = C.bootstrap_pred (fun positive env p -> pred ~positive env p)
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
    | Sarray(l,_,_) -> M.vars l
    | Srange(l,_,a,b) -> 
	Vars.union (M.vars l) (Vars.union (vars_opt a) (vars_opt b))
    | Sdescr(xs,l,p) ->
	List.fold_left
	  (fun xs x -> Vars.remove x xs)
	  (Vars.union (M.vars l) (F.varsp p)) xs

  let vars sloc = List.fold_left 
    (fun xs s -> Vars.union xs (vars_sloc s)) Vars.empty sloc

end
