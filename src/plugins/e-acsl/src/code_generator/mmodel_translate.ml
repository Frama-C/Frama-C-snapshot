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

open Cil_types

(**************************************************************************)
(********************** Forward references ********************************)
(**************************************************************************)

let predicate_to_exp_ref
  : (kernel_function -> Env.t -> predicate -> exp * Env.t) ref
  = Extlib.mk_fun "named_predicate_to_exp_ref"

let term_to_exp_ref
  : (kernel_function -> Env.t -> term -> exp * Env.t) ref
  = Extlib.mk_fun "term_to_exp_ref"

(*****************************************************************************)
(****************************** Ranges Elimination ***************************)
(*****************************************************************************)

(* We call Range Elimination the operation through which ranges are
  substituted by universally quantified logic variables.
  Example:
    [\valid(&t[(n-1)..(n+2)][1][0..1])] can be soundly transformed into
    [\forall integer q1; n-1 <= q1 <= n+2 ==>
      \forall integer q2; 0 <= q2 <= 1 ==>
        \valid(&t[q1][1][q2])]
  However, the substitution can be unsound,
  in which case [Range_elimination_exception] must be raised.
  Example:
    [\valid(&t[(0..2)==(0..2) ? 0 : 1])] is equivalent to [\valid(&t[0])]
      since [==] refers to set equality when applied on ranges.
    But Range Elimination will give a predicate equivalent to [\valid(&t[1])]
      since [\forall 0 <= q1,q2 <= 2: q1==q2] is false.
    Hence [Range_elimination_exception] must be raised. *)
exception Range_elimination_exception

(* Takes a [toffset] and checks whether it contains an index that is a set *)
let rec has_set_as_index = function
  | TNoOffset ->
    false
  | TIndex(t, toffset) ->
    Logic_const.is_set_type t.term_type || has_set_as_index toffset
  | TModel(_, toffset) | TField(_, toffset) ->
    has_set_as_index toffset

(* Performs Range Elimination on index [TIndex(term, offset)]. Term part.
  Raises [Range_elimination_exception] if whether the operation is unsound or
  if we don't support the construction yet. *)
let eliminate_ranges_from_index_of_term ~loc t =
  match t.term_node with
  | Trange(Some n1, Some n2) ->
    let name = Varname.get ~scope:Varname.Block "range" in
    let lv = Cil_const.make_logic_var_kind name LVQuant Linteger in
    let tlv = Logic_const.tvar ~loc lv in
    tlv, (n1, lv, n2)
  | _ ->
    raise Range_elimination_exception

(* Performs Range Elimination on index [TIndex(term, offset)]. Offset part.
  Raises [Range_elimination_exception], through [eliminate_ranges_from_
  index_of_term], if whether the operation is unsound or
  if we don't support the construction yet. *)
let rec eliminate_ranges_from_index_of_toffset ~loc toffset quantifiers =
  match toffset with
  | TIndex(t, toffset') ->
    if Misc.is_range_free t then
      let toffset', quantifiers' =
        eliminate_ranges_from_index_of_toffset ~loc toffset' quantifiers
      in
      TIndex(t, toffset'), quantifiers'
    else
      (* Attempt Range Elimination on [t] *)
      let t1, quantifiers1 =
        eliminate_ranges_from_index_of_term ~loc t
      in
      let toffset2, quantifiers2 =
        eliminate_ranges_from_index_of_toffset ~loc toffset' quantifiers
      in
      let toffset3 = TIndex(t1, toffset2) in
      toffset3, quantifiers1 :: quantifiers2
  | TNoOffset ->
    toffset, quantifiers
  | TModel _ ->
    Error.not_yet "range elimination on TModel"
  | TField _ ->
    Error.not_yet "range elimination on TField"

(*****************************************************************************)
(********************** Calls without Range Elimination **********************)
(************** \base_addr, \block_length, \offset, \freeable ****************)
(*****************************************************************************)

(* \base_addr, \block_length, \offset and \freeable *)
let call ~loc kf name ctx env t =
  assert (name = "base_addr" || name = "block_length"
    || name = "offset" || name ="freeable");
  let e, env = !term_to_exp_ref kf (Env.rte env true) t in
  let _, res, env =
    Env.new_var
      ~loc
      ~name
      env
      None
      ctx
      (fun v _ ->
        let name = Functions.RTL.mk_api_name name in
        [ Misc.mk_call ~loc ~result:(Cil.var v) name [ e ] ])
  in
  res, env

(*****************************************************************************)
(************************* Calls with Range Elimination **********************)
(********************** \initialized, \valid, \valid_read ********************)
(*****************************************************************************)

(* Take the term [size] that has been typed into GMP
   and return an expression of type [size_t].
   The case where [!(0 <= size < SIZE_MAX)] is an UB ==> guard against it. *)
let gmp_to_sizet ~loc kf env size p =
  let sizet = Cil.(theMachine.typeOfSizeOf) in
  (* The guard *)
  let sizet_max = Logic_const.tint
    ~loc (Cil.max_unsigned_number (Cil.bitsSizeOf sizet))
  in
  let guard_upper = Logic_const.prel ~loc (Rlt, size, sizet_max) in
  let guard_lower = Logic_const.prel ~loc (Rle, Cil.lzero ~loc (), size) in
  let guard = Logic_const.pand ~loc (guard_lower, guard_upper) in
  Typing.type_named_predicate ~must_clear:false guard;
  let guard, env = !predicate_to_exp_ref kf env guard in
  (* Translate term [size] into an exp of type [size_t] *)
  let size, env = !term_to_exp_ref kf env size in
  let  _, e, env = Env.new_var
    ~loc
    ~name:"size"
    env
    None
    sizet
    (fun vi _ ->
      [ Misc.mk_e_acsl_guard ~reverse:true Misc.RTE kf guard p;
        Misc.mk_call ~loc ~result:(Cil.var vi) "__gmpz_get_ui" [ size ] ])
  in
  e, env

(* Call to [__e_acsl_<name>] for terms of the form [ptr + r]
  when [<name> = valid or initialized or valid_read] and
  where [ptr] is an address and [r] a range offset *)
let call_memory_block ~loc kf name ctx env ptr r p =
  let n1, n2 = match r.term_node with
    | Trange(Some n1, Some n2) ->
      n1, n2
    | Trange(None, _) | Trange(_, None) ->
      Options.abort "unbounded ranges are not part of E-ACSL"
    | _ ->
      assert false
  in
  (* s *)
  let ty = match Cil.unrollType (Misc.cty ptr.term_type) with
    | TPtr(ty, _) | TArray(ty, _, _, _) -> ty
    | _ -> assert false
  in
  let s = Logic_const.term ~loc (TSizeOf ty) Linteger in
  (* ptr *)
  let typ_charptr = Cil.charPtrType in
  let ptr = Logic_const.term
    ~loc
    (TBinOp(
      PlusPI,
      Logic_utils.mk_cast ~loc ~force:false typ_charptr ptr,
      Logic_const.term ~loc (TBinOp(Mult, s, n1)) Linteger))
    (Ctype typ_charptr)
  in
  Typing.type_term ~use_gmp_opt:false ~ctx:Typing.nan ptr;
  let term_to_exp = !term_to_exp_ref in
  let ptr, env = term_to_exp kf (Env.rte env true) ptr in
  (* size *)
  let size_term =
    (* Since [s] and [n1] have been typed through [ptr],
       we need to clone them in order to force retyping *)
    let s = { s with term_node = s.term_node } in
    let n1 = { n1 with term_node = n1.term_node } in
    Logic_const.term
      ~loc
      (TBinOp(
        Mult,
        s,
        Logic_const.term ~loc (TBinOp(MinusA, n2, n1)) Linteger))
      Linteger
  in
  Typing.type_term ~use_gmp_opt:false size_term;
  let size, env = match Typing.get_number_ty size_term with
    | Typing.Gmpz ->
      gmp_to_sizet ~loc kf env size_term p
    | Typing.(C_integer _ | C_float _) ->
      let size, env = term_to_exp kf env size_term in
      Cil.constFold false size, env
    | Typing.(Rational | Real | Nan) ->
      assert false
  in
  (* base and base_addr *)
  let base, _ = Misc.ptr_index ~loc ptr in
  let base_addr  = match base.enode with
    | AddrOf _ | Const _ -> Cil.zero ~loc
    | Lval lv | StartOf lv -> Cil.mkAddrOrStartOf ~loc lv
    | _ -> assert false
  in
  (* generating env *)
  let _, e, env =
    Env.new_var
      ~loc
      ~name
      env
      None
      ctx
      (fun v _ ->
        let fname = Functions.RTL.mk_api_name name in
        let args = match name with
        | "valid" | "valid_read" -> [ ptr; size; base; base_addr ]
        | "initialized" -> [ ptr; size ]
        | _ -> Error.not_yet ("builtin " ^ name)
        in
        [ Misc.mk_call ~loc ~result:(Cil.var v) fname args ])
  in
  e, env

(* [call_with_ranges] handles ranges in [t] when calling builtin [name].
  It only supports the following cases for the time being:
    A: [\builtin(ptr+r)] where [ptr] is an address and [r] a range or
       [\builtin(t[r])] or
       [\builtin(t[i_1]...[i_n])] where [t] is dynamically allocated
                                  and all the indexes are integers,
                                  except the last one which is a range
       The generated code is a SINGLE call to the corresponding E-ACSL builtin
    B: [\builtin(t[i_1]...[i_n])] where [t] is NOT dynamically allocated
                                  and the indexes are integers or ranges
       The generated code is a SET OF calls to the corresponding E-ACSL builtin
    C: Any other use of ranges/No range
       Call [call_default] which performs the translation for
       range free terms, and raises Not_yet if it ever encounters a range.
  Example for case:
    A: [\valid(&t[3..5])]
       Contiguous locations -> a single call to [__e_acsl_valid]
    B: [\valid(&t[4][3..5][2])]
       NON-contiguous locations -> multiple calls (3) to [__e_acsl_valid] *)
let call_with_ranges ~loc kf name ctx env t p call_default =
  if Misc.is_bitfield_pointers t.term_type then
    Error.not_yet "bitfield pointer";
  match t.term_node with
  | TBinOp((PlusPI | IndexPI), ptr, ({ term_node = Trange _ } as r)) ->
    if Misc.is_set_of_ptr_or_array ptr.term_type then
      Error.not_yet "arithmetic over set of pointers or arrays"
    else
      (* Case A *)
      call_memory_block ~loc kf name ctx env ptr r p
  | TAddrOf(TVar lv, TIndex({ term_node = Trange _ } as r, TNoOffset)) ->
    (* Case A *)
    assert (Logic_const.is_set_type t.term_type);
    let lty_noset = Logic_const.type_of_element t.term_type in
    let ptr = Logic_const.taddrof ~loc (TVar lv, TNoOffset) lty_noset in
    call_memory_block ~loc kf name ctx env ptr r p
  | TAddrOf(TVar ({ lv_type = Ctype (TArray _) } as lv), toffset) ->
    if has_set_as_index toffset then
      (* Case B *)
      try
        let toffset', quantifiers =
          eliminate_ranges_from_index_of_toffset ~loc toffset []
        in
        let lty_noset =
          if Logic_const.is_set_type t.term_type then
            Logic_const.type_of_element t.term_type
          else
            t.term_type
        in
        let t' = Logic_const.taddrof ~loc (TVar lv, toffset') lty_noset in
        let p_quantified =
          (* [loc] prevents a type error with eta-expansion and label *)
          let loc = Some loc in
          let call f = f ?loc (Logic_const.here_label, t') in
          match name with
          | "valid" -> call Logic_const.pvalid
          | "initialized" -> call Logic_const.pinitialized
          | "valid_read" -> call Logic_const.pvalid_read
          | _ -> Options.fatal "[call_with_ranges] unexpected builtin"
          in
          let p_quantified = List.fold_left
            (fun p (tmin, lv, tmax) ->
              (* \forall integer tlv; tmin <= tlv <= tmax ==> p *)
              let tlv = Logic_const.tvar ~loc lv in
              let lower_bound = Logic_const.prel ~loc (Rle, tmin, tlv) in
              let upper_bound = Logic_const.prel ~loc (Rle, tlv, tmax) in
              let bound = Logic_const.pand ~loc (lower_bound, upper_bound) in
              let bound_imp_p = Logic_const.pimplies ~loc (bound, p) in
              Logic_const.pforall ~loc ([lv], bound_imp_p))
            p_quantified
            quantifiers
        in
        Typing.type_named_predicate ~must_clear:true p_quantified;
        !predicate_to_exp_ref kf env p_quantified
      with Range_elimination_exception ->
        (* Case C *)
        call_default ~loc kf name ctx env t
    else
      (* Case C *)
      call_default ~loc kf name ctx env t
  | _ ->
    (* Case C *)
    call_default ~loc kf name ctx env t

(* \initialized *)
let call_with_size ~loc kf name ctx env t p =
  assert (name = "initialized");
  let call_for_unsupported_constructs ~loc kf name ctx env t =
    let term_to_exp = !term_to_exp_ref in
    let e, env = term_to_exp kf (Env.rte env true) t in
    let _, res, env =
      Env.new_var
        ~loc
        ~name
        env
        None
        ctx
        (fun v _ ->
          let ty = Misc.cty t.term_type in
          let sizeof = Misc.mk_ptr_sizeof ty loc in
          let fname = Functions.RTL.mk_api_name name in
          [ Misc.mk_call ~loc ~result:(Cil.var v) fname [ e; sizeof ] ])
    in
    res, env
  in
  call_with_ranges
    ~loc
    kf
    name
    ctx
    env
    t
    p
    call_for_unsupported_constructs

(* \valid and \valid_read *)
let call_valid ~loc kf name ctx env t p =
  assert (name = "valid" || name = "valid_read");
  let call_for_unsupported_constructs ~loc kf name ctx env t =
    let term_to_exp = !term_to_exp_ref in
    let e, env = term_to_exp kf (Env.rte env true) t in
    let base, _ = Misc.ptr_index ~loc e in
    let base_addr  = match base.enode with
      | AddrOf _ | Const _ -> Cil.zero ~loc
      | Lval lv | StartOf lv -> Cil.mkAddrOrStartOf ~loc lv
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
          let ty = Misc.cty t.term_type in
          let sizeof = Misc.mk_ptr_sizeof ty loc in
          let fname = Functions.RTL.mk_api_name name in
          let args = [ e; sizeof; base; base_addr ] in
          [ Misc.mk_call ~loc ~result:(Cil.var v) fname args ])
    in
    res, env
  in
  call_with_ranges
    ~loc
    kf
    name
    ctx
    env
    t
    p
    call_for_unsupported_constructs
