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
open Cil_datatype
open Locations
open Abstract_interp
open Cvalue
open Bit_utils


(* Truth values for a predicate analyzed by the value analysis *)

type predicate_status = True | False | Unknown

let string_of_predicate_status = function
  | Unknown -> "unknown"
  | True -> "valid"
  | False -> "invalid"

let pretty_predicate_status fmt v =
  Format.fprintf fmt "%s" (string_of_predicate_status v)

let join_predicate_status x y = match x, y with
  | True, True -> True
  | False, False -> False
  | True, False | False, True
  | Unknown, _ | _, Unknown -> Unknown

exception Stop

let _join_list_predicate_status l =
  try
    let r =
      List.fold_left
        (fun acc e ->
          match e, acc with
          | Unknown, _ -> raise Stop
          | e, None -> Some e
          | e, Some eacc -> Some (join_predicate_status eacc e)
        ) None l
    in
    match r with
    | None -> True
    | Some r -> r
  with Stop -> Unknown

(* Type of possible errors during evaluation. See pretty-printer for details *)
type logic_evaluation_error =
  | Unsupported of string
  | UnsupportedLogicVar of logic_var
  | AstError of string
  | NoEnv of logic_label
  | NoResult
  | CAlarm

let pretty_logic_evaluation_error fmt = function
  | Unsupported s -> Format.fprintf fmt "unsupported ACSL construct: %s" s
  | UnsupportedLogicVar tv ->
    Format.fprintf fmt "unsupported logic var %s" tv.lv_name
  | AstError s -> Format.fprintf fmt "error in AST: %s; please report" s
  | NoEnv (FormalLabel s) ->
    Format.fprintf fmt "no environment to evaluate \\at(_,%s)" s
  | NoEnv (BuiltinLabel l) ->
      Format.fprintf fmt "no environment to evaluate \\at(_,%a)"
        Printer.pp_logic_builtin_label l
  | NoEnv (StmtLabel _) ->
      Format.fprintf fmt "\\at() on a C label is unsupported"
  | NoResult -> Format.fprintf fmt "meaning of \\result not specified"
  | CAlarm -> Format.fprintf fmt "alarm during evaluation"

exception LogicEvalError of logic_evaluation_error

let unsupported s = raise (LogicEvalError (Unsupported s))
let unsupported_lvar v = raise (LogicEvalError (UnsupportedLogicVar v))
let ast_error s = raise (LogicEvalError (AstError s))
let no_env lbl = raise (LogicEvalError (NoEnv lbl))
let no_result () = raise (LogicEvalError NoResult)
let c_alarm () = raise (LogicEvalError CAlarm)

(** Three modes to handle the alarms when evaluating a logical term. *)
type alarm_mode =
  | Ignore            (* Ignores all alarms. *)
  | Fail              (* Raises a LogicEvalError when an alarm is encountered. *)
  | Track of bool ref (* Tracks the possibility of an alarm in the boolean. *)

let is_logic_defined alarm status =
  if status = Alarmset.True
  then true
  else
    match alarm with
    | Alarms.Valid_string _ | Alarms.Differing_blocks _
    | Alarms.Pointer_comparison _ -> true
    | _ -> false

(* Process an alarm map according to the alarm_mode. *)
let check_alarms alarms =
  let default status = status = Alarmset.True in
  function
  | Ignore -> ()
  | Fail ->
    if not (Alarmset.for_all is_logic_defined ~default alarms)
    then c_alarm ()
  | Track b ->
    if not (Alarmset.for_all is_logic_defined ~default alarms)
    then b := true

(* Process the possibility of an alarm according to the alarm_mode.
   The boolean [b] is true when an alarm is possible. *)
let track_alarms b = function
  | Ignore -> ()
  | Fail -> if b then c_alarm ()
  | Track bref -> if b then bref := true

(* A fake expression used for the evaluation functions that produce alarms.
   Such alarms must not be emitted! *)
let fake_expr =
  Cil.dummy_exp (Const (CStr "fake_expression_for_logic_evaluations"))

let unop_context = { Eval.operand = fake_expr }
let binop_context = Eval.{ left_operand = fake_expr;
                           right_operand = fake_expr;
                           binary_result = fake_expr }

let display_evaluation_error ~loc = function
  | CAlarm -> ()
  | pa ->
    Value_parameters.result ~source:(fst loc) ~once:true
      "cannot evaluate ACSL term, %a" pretty_logic_evaluation_error pa

(* Warning mode use when performing _reductions_ in the logic ( ** not **
   evaluation). "Logic alarms" are ignored, and the reduction proceeds as if
   they had not occurred. *)
let alarm_reduce_mode () =
  if Value_parameters.ReduceOnLogicAlarms.get () then Ignore else Fail

let find_or_alarm ~alarm_mode state loc =
  let is_invalid = not (Locations.is_valid ~for_writing:false loc) in
  track_alarms is_invalid alarm_mode;
  let v = Model.find_indeterminate ~conflate_bottom:true state loc in
  let is_indeterminate = Cvalue.V_Or_Uninitialized.is_indeterminate v in
  track_alarms is_indeterminate alarm_mode;
  V_Or_Uninitialized.get_v v

(* Evaluation environments. Used to evaluate predicate on \at nodes *)

(* Labels:
   pre: pre-state of the function. Equivalent to \old in the postcondition
     (and displayed as such)
   here: current location, always the intuitive meaning. Assertions are
     evaluated before the statement.
   post: forbidden in preconditions;
     In postconditions:
      in function contracts, state of in the post-state
      in statement contracts, state after the evaluation of the statement
   old: forbidden in assertions.
        In statement contracts post, means the state before the statement
        In functions contracts post, means the pre-state
*)

(* TODO: evaluating correctly Pat with the current Value domain is tricky,
   and only works reliably for the four labels below, that are either
   invariant during the course of the program, or fully local. The
   program below shows the problem:
   if (c) x = 1; else x = 3;
   L:
   x = 1;
   \assert \at(x == 1, L);
   A naive implementation of assertions involving C labels is likely to miss
   the fact that the assertion is false after the else branch. A good
   solution is to use a dummy edge that flows from L to the assertion,
   to force its re-evaluation.
*)


type labels_states = Cvalue.Model.t Logic_label.Map.t

let join_label_states m1 m2 =
  let aux _ s1 s2 = match s1, s2 with
    | None, None -> None
    | Some s, None | None, Some s -> Some s
    | Some s1, Some s2 -> Some (Cvalue.Model.join s1 s2)
  in
  if m1 == m2 then m1
  else Logic_label.Map.merge aux m1 m2

(* The logic can refer to the state at other points of the program
   using labels. [e_cur] indicates the current label (in changes when
   evaluating the term in a \at(label,term). [e_states] associates a
   memory state to each label. [result] contains the variable
   corresponding to \result; this works even with leaf functions
   without a body. [result] is None when \result is meaningless
   (e.g. the function returns void, logic outside of a function
   contract, etc.) *)
type eval_env = {
  e_cur: logic_label;
  e_states: labels_states;
  result: varinfo option;
}

let join_env e1 e2 = {
  e_cur = (assert (Logic_label.equal e1.e_cur e2.e_cur); e1.e_cur);
  e_states = join_label_states e1.e_states e2.e_states;
  result = (assert (e1.result == e2.result); e1.result);
}

let env_state env lbl =
  try Logic_label.Map.find lbl env.e_states
  with Not_found -> no_env lbl

let env_current_state e = env_state e e.e_cur

let overwrite_state env state lbl =
  { env with e_states = Logic_label.Map.add lbl state env.e_states }

let overwrite_current_state env state = overwrite_state env state env.e_cur

let lbl_here = Logic_const.here_label

let add_logic ll state (states: labels_states): labels_states =
  Logic_label.Map.add ll state states
let add_here = add_logic Logic_const.here_label
let add_pre = add_logic Logic_const.pre_label
let add_post = add_logic Logic_const.post_label
let add_old = add_logic Logic_const.old_label
(* Init is a bit special, it is constant and always added to the initial state*)
let add_init state =
  add_logic Logic_const.init_label (Db.Value.globals_state ()) state

let make_env logic_env state =
  let transfer label map =
    Logic_label.Map.add label (logic_env.Abstract_domain.states label) map
  in
  let map =
    Logic_label.Map.add lbl_here state
      (transfer Logic_const.pre_label
         (transfer Logic_const.old_label
            (transfer Logic_const.post_label
               (add_init Logic_label.Map.empty))))
  in
  { e_cur = lbl_here;
    e_states = map;
    result = logic_env.Abstract_domain.result }

let env_pre_f ~pre () = {
  e_cur = lbl_here;
  e_states = add_here pre (add_pre pre (add_init Logic_label.Map.empty));
  result = None (* Never useful in a pre *);
}

let env_post_f ?(c_labels=Logic_label.Map.empty) ~pre ~post ~result () = {
  e_cur = lbl_here;
  e_states = add_post post
      (add_here post (add_pre pre (add_old pre (add_init c_labels))));
  result = result;
}

let env_annot ?(c_labels=Logic_label.Map.empty) ~pre ~here () = {
  e_cur = lbl_here;
  e_states = add_here here (add_pre pre (add_init c_labels));
  result = None (* Never useful in a 'assert'. TODO: will be needed for stmt
                   contracts *);
}

let env_assigns ~pre = {
  e_cur = lbl_here;
  (* YYY: Post label is missing, but is too difficult in the current evaluation
          scheme, since we build it by evaluating the assigns... *)
  e_states = add_old pre
                (add_here pre (add_pre pre (add_init Logic_label.Map.empty)));
  result = None (* Treated in a special way in callers *)
}

let env_only_here state = {
  e_cur = lbl_here;
  e_states = add_here state (add_init Logic_label.Map.empty);
  result = None (* Never useful in a 'assert'. TODO: will be needed for stmt
                   contracts *);
}

(* Return the base and the type corresponding to the logic var if it is within
   the scope of the supported ones. Fail otherwise. *)
let supported_logic_var lvi =
  match Logic_utils.unroll_type lvi.lv_type with
    | Ctype ty when Cil.isIntegralType ty ->
      (Base.of_c_logic_var lvi), ty
    | _ -> unsupported_lvar lvi

let bind_logic_vars env lvs =
  let bind_one state lv =
    try
      let b, cty = supported_logic_var lv in
      let size = Int.of_int (Cil.bitsSizeOf cty) in
      let v = Cvalue.V_Or_Uninitialized.initialized V.top_int in
      Model.add_base_value b ~size v ~size_v:Int.one state
    with Cil.SizeOfError _ -> unsupported_lvar lv
  in
  let state = env_current_state env in
  let state = List.fold_left bind_one state lvs in
  overwrite_current_state env state

let unbind_logic_vars env lvs =
  let unbind_one state lv =
    let b, _ = supported_logic_var lv in
    Model.remove_base b state
  in
  let state = env_current_state env in
  let state = List.fold_left unbind_one state lvs in
  overwrite_current_state env state

let lop_to_cop op =
  match op with
  | Req -> Eq
  | Rneq -> Ne
  | Rle -> Le
  | Rge -> Ge
  | Rlt -> Lt
  | Rgt -> Gt

(* Types currently understood in the evaluation of the logic: no arrays,
   structs, logic arrays or subtle ACSL types. Sets of sets seem to
   be flattened, so the current treatment of them is correct. *)
let rec isLogicNonCompositeType t =
  match t with
    | Lvar _ | Larrow _ -> false
    | Ltype _ ->
        Logic_const.is_boolean_type t ||
          (try isLogicNonCompositeType (Logic_const.type_of_element t)
           with Failure _ -> false)
    | Linteger | Lreal -> true
    | Ctype t -> Cil.isArithmeticOrPointerType t

let rec infer_type = function
  | Ctype t ->
      (match t with
        | TInt _ -> Cil.intType
        | TFloat _ -> Cil.doubleType
        | _ -> t)
  | Lvar _ -> Cil.voidPtrType (* For polymorphic empty sets *)
  | Linteger -> Cil.intType
  | Lreal -> Cil.doubleType
  | Ltype _ | Larrow _ as t ->
      if Logic_const.is_plain_type t then
        unsupported (Pretty_utils.to_string Cil_datatype.Logic_type.pretty t)
      else Logic_const.plain_or_set infer_type t

(* Best effort for comparing the types currently understood by Value: ignore
   differences in integer and floating-point sizes, that are meaningless
   in the logic *)
let same_etype t1 t2 =
  match Cil.unrollType t1, Cil.unrollType t2 with
    | (TInt _ | TEnum _), (TInt _ | TEnum _) -> true
    | TFloat _, TFloat _ -> true
    | TPtr (p1, _), TPtr (p2, _) -> Cil_datatype.Typ.equal p1 p2
    | _, _ -> Cil_datatype.Typ.equal t1 t2

(* Rounding mode *)
let real_mode = Fval.Any

let infer_binop_res_type op targ =
  match op with
    | PlusA | MinusA | Mult | Div -> targ
    | PlusPI | MinusPI | IndexPI ->
        assert (Cil.isPointerType targ); targ
    | MinusPP -> Cil.intType
    | Mod | Shiftlt | Shiftrt | BAnd | BXor | BOr ->
        (* can only be applied on integral arguments *)
        assert (Cil.isIntegralType targ); Cil.intType
    | Lt | Gt | Le | Ge | Eq | Ne | LAnd | LOr ->
        Cil.intType (* those operators always return a boolean *)

(* Assumes that [lt] is a pointer type, or a set of pointer types. Returns
   the pointed type. *)
let logic_typ_pointed lt =
  match Logic_utils.unroll_type lt with
  | Ctype (TPtr _ as t)
  | Ltype ({lt_name = "set"},[Ctype t]) -> Cil.typeOf_pointed t
  | _ ->
    ast_error
      (Format.asprintf "not a pointer type %a" Printer.pp_logic_type lt)

(* This function could probably be in Logic_const. It computes [*tsets],
   assuming that [tsets] has a pointer type. (Note: it [tsets] is a tset,
   the result should probably have a set type too. This is currently
   not done. *)
let deref_tsets tsets =
  let star_tsets = Cil.mkTermMem ~addr:tsets ~off:TNoOffset in
  let typ = logic_typ_pointed tsets.term_type in
  Logic_const.term (TLval star_tsets) (Ctype typ)


type logic_deps = Zone.t Logic_label.Map.t

let deps_at lbl (ld:logic_deps) =
  try Logic_label.Map.find lbl ld
  with Not_found -> Zone.bottom

let add_deps lbl ldeps deps =
  let prev_deps = deps_at lbl ldeps in
  let deps = Zone.join prev_deps deps in
  let ldeps : logic_deps = Logic_label.Map.add lbl deps ldeps in
  ldeps

let join_logic_deps (ld1:logic_deps) (ld2: logic_deps) : logic_deps =
  let aux _ d1 d2 = match d1, d2 with
    | None as d, None | (Some _ as d), None | None, (Some _ as d) -> d
    | Some d1, Some d2 -> Some (Zone.join d1 d2)
  in
  Logic_label.Map.merge aux ld1 ld2

let empty_logic_deps =
  Logic_label.Map.add lbl_here Zone.bottom Logic_label.Map.empty


(* Type holding the result of an evaluation. Currently, 'a is either
   [Cvalue.V.t] for [eval_term], and [Location_Bits.t] for
   [eval_tlval_as_loc], and [Ival.t] for [eval_toffset].  [eover]
   contains an over-approximation of the evaluation. [eunder] contains an
   under-approximation, under the hypothesis that the state in which we
   evaluate is not Bottom. (Otherwise, all under-approximations would be
   Bottom themselves). The following two invariants should hold:
   (1) eunder \subset eover.
   (2) when evaluating something that is not a Tset, either eunder = Bottom,
      or eunder = eover, and cardinal(eover) <= 1. This is due to the fact
      that under-approximations are not propagated as an abstract domain, but
      only created from Trange or inferred from exact over-approximations. *)
type 'a eval_result = {
  etype: Cil_types.typ;
  eunder: 'a;
  eover: 'a;
  ldeps: logic_deps;
}

(* When computing an under-approximation, we make the hypothesis that the state
   is not Bottom. Hence, over-approximations of cardinal <= 1 are actually of
   cardinal 1, and are thus exact. *)
let under_from_over eover =
  if Cvalue.V.cardinal_zero_or_one eover
  then eover
  else Cvalue.V.bottom
;;

let under_loc_from_over eover =
  if Locations.Location_Bits.cardinal_zero_or_one eover
  then eover
  else Locations.Location_Bits.bottom
;;

let is_noop_cast ~src_typ ~dst_typ =
  let src_typ = Logic_const.plain_or_set
    (fun lt ->
      match Logic_utils.unroll_type lt with
      | Ctype typ -> Some (Eval_typ.classify_as_scalar typ)
      | _ -> None
    ) (Logic_utils.unroll_type src_typ)
  in
  let open Eval_typ in
  match src_typ, Eval_typ.classify_as_scalar dst_typ with
  | Some (TSInt rsrc), TSInt rdst ->
    let b1, b2 = Eval_typ.range_inclusion rsrc rdst in b1 && b2
  | Some (TSFloat srckind), TSFloat destkind ->
    Cil.frank srckind <= Cil.frank destkind
  | Some (TSPtr _), TSPtr _ -> true
  | _ -> false

(* Note: non-constant integers can happen e.g. for sizeof of structures of an unknown size. *)
let einteger v =
  { etype = Cil.intType;
    eunder = under_from_over v;
    eover = v;
    ldeps = empty_logic_deps }

(* Note: some reals cannot be exactly represented as floats; in which
   case we do not know their under-approximation. *)
let ereal v =
  let eunder = under_from_over v in
  { etype = Cil.doubleType; eunder; eover = v; ldeps = empty_logic_deps }


(* Check "logic alarms" when evaluating [v1 op v2]. All operators except the
   four below are defined unambiguously in ACSL. *)
let check_logic_alarms ~alarm_mode (_v1: V.t eval_result) op v2 =
  match op with
  | Div | Mod -> (* This captures floating-point division by 0, which is ok
                    because it is also a logic alarm for Value. *)
    track_alarms (V.contains_zero v2.eover) alarm_mode
  | Shiftlt | Shiftrt -> begin
      (* Check that [e2] is positive. [e1] can be arbitrary, we use
         the arithmetic vision of shifts *)
      try
        let i2 = Cvalue.V.project_ival_bottom v2.eover in
        let valid = Ival.is_included i2 Ival.positive_integers in
        track_alarms (not valid) alarm_mode
      with Cvalue.V.Not_based_on_null -> track_alarms true alarm_mode
    end
  | _ -> ()

(* Constrain the ACSL range [idx] when it is used to access an array of
   [size_arr] cells, and it is a Trange in which one size is not
   specified. (E.g. t[1..] is transformed into t[1..9] when t has size 10). *)
let constraint_trange idx size_arr =
  if Kernel.SafeArrays.get () then
    match idx.term_node with
    | Trange ((None as low), up) | Trange (low, (None as up)) -> begin
        let loc = idx.term_loc in
        match Extlib.opt_bind Cil.constFoldToInt size_arr with
        | None -> idx
        | Some size ->
          let low = match low with (* constrained l.h.s *)
            | Some _ -> low
            | None -> Some (Logic_const.tint ~loc Integer.zero)
          in
          let up = match up with (* constrained r.h.s *)
            | Some _ -> up
            | None -> Some (Logic_const.tint ~loc (Int.pred size))
          in
          Logic_const.trange ~loc (low, up)
      end
    | _ -> idx
  else idx

(* Note: "charlen" stands for either strlen or wcslen *)

(* Evaluates the logical predicates [strlen/wcslen] using str* builtins.
   Returns [res, alarms], where [res] is the return value of [strlen]
   ([None] the evaluation results in [bottom]). *)
let logic_charlen_builtin wrapper state arg v =
  let args = [ (Builtins_string.Term arg, v) ] in
  (* the call below could in theory return Builtins.Invalid_nb_of_args,
     but logic typing constraints prevent that. *)
  let res, alarms = wrapper state args in
  match res.Value_types.c_values with
  | [(opt_offsm, _state)] -> begin
      match opt_offsm with
      | None -> None
      | Some offsm -> Some (offsm, alarms)
    end
  | l -> Kernel.fatal "builtin should always return a singleton \
                       (got %d states)" (List.length l)

(* Never raises exceptions; instead, returns [-1,+oo] in case of alarms
   (most imprecise result possible for the logic strlen/wcslen predicates). *)
let eval_logic_charlen wrapper env arg v ldeps =
  let eover =
    match logic_charlen_builtin wrapper (env_current_state env) arg v with
    | None -> Cvalue.V.bottom
    | Some (offsm, alarms) ->
      if Builtins_string.String_alarms.Set.is_empty alarms
      then
        let v = Extlib.the (Cvalue.V_Offsetmap.single_interval_value offsm) in
        Cvalue.V_Or_Uninitialized.get_v v
      else Cvalue.V.inject_ival (Ival.inject_range (Some Int.minus_one) None)
  in
  let eunder = under_from_over eover in
  (* the C strlen function has type size_t, but the logic strlen predicate has
     type ℤ (signed) *)
  let etype = Cil.intType in
  { etype; ldeps; eover; eunder }

(* Evaluates the logical predicate is_allocable, according to the following
   logic:
   - if the size to allocate is always too large (> SIZE_MAX), allocation fails;
   - otherwise, if MallocReturnsNull is true or if the size may exceed SIZE_MAX,
     returns Unknown (to simulate non-determinism);
   - otherwise, allocation always succeeds. *)
let eval_is_allocable size =
  let size_ok = Builtins_malloc.alloc_size_ok size in
  match size_ok, Value_parameters.MallocReturnsNull.get () with
  | Alarmset.False, _ -> False
  | Alarmset.Unknown, _ | _, true -> Unknown
  | Alarmset.True, false -> True

(* returns true iff the logic variable is defined by the
   Frama-C standard library *)
let comes_from_fc_stdlib lvar =
  Cil.hasAttribute "fc_stdlib" lvar.lv_attr ||
  match lvar.lv_origin with
  | None -> false
  | Some vi ->
    Cil.hasAttribute "fc_stdlib" vi.vattr


(* This is a bit tricky: [do_promotion] needs to ignore the size of src_typ.
   In Cvalue_forward, this is the case for the casts between float and integers,
   but not between float or between integers. Thus, we call spefically
   [reinterpret_float] and [rewrap_integer] respectively.
   This should be resolved by an unified AST between C and logic. *)
let do_promotion ~rounding_mode ~src_typ ~dst_typ expr v =
  let open Eval_typ in
  match Eval_typ.classify_as_scalar dst_typ,
        Eval_typ.classify_as_scalar src_typ with
  | TSFloat _, (TSInt _ | TSPtr _)
  | (TSInt _ | TSPtr _), TSFloat _ ->
    Cvalue_forward.do_promotion ~rounding_mode ~src_typ ~dst_typ expr v
  | (TSInt dst | TSPtr dst), (TSInt _ | TSPtr _) ->
    (* TODO: this is [Cvalue_forward.rewrap_integer] without emission of the
       message about 2's complement for overflow. *)
    let size = Integer.of_int dst.Eval_typ.i_bits in
    fst (V.cast ~signed:dst.Eval_typ.i_signed ~size v), Alarmset.none
  | TSFloat fkind, TSFloat _ ->
    Cvalue_forward.cast_float expr fkind v
  | (TSNotScalar, _) | (_, TSNotScalar) ->
    v, Alarmset.none


(* -------------------------------------------------------------------------- *)
(* --- Evaluation of terms                                                --- *)
(* -------------------------------------------------------------------------- *)

let rec eval_term ~alarm_mode env t =
  match t.term_node with
    | Tat (t, lab) ->
        ignore (env_state env lab);
        eval_term ~alarm_mode { env with e_cur = lab } t

    | TConst (Integer (v, _)) -> einteger (Cvalue.V.inject_int v)
    | TConst (LEnum e) ->
        (match Cil.constFoldToInt e.eival with
           | Some v -> einteger (Cvalue.V.inject_int v)
           | _ -> ast_error "non-evaluable constant")
    | TConst (LChr c) ->
      einteger (Cvalue.V.inject_int (Cil.charConstToInt c))
    | TConst (LReal { r_lower ; r_upper }) -> begin
      try
        let r_lower = Fval.F.of_float r_lower in
        let r_upper = Fval.F.of_float r_upper in
 	 let inf, f = Fval.inject_r r_lower r_upper in
 	 if inf then c_alarm ();
 	 ereal (Cvalue.V.inject_ival (Ival.inject_float f))
      with Fval.Non_finite -> c_alarm ()
    end

    (*  | TConst ((CStr | CWstr) Missing cases *)

    | TAddrOf (thost, toffs) ->
        let r = eval_thost_toffset ~alarm_mode env thost toffs in
        { etype = TPtr (r.etype, []);
          ldeps = r.ldeps;
          eunder = loc_bits_to_loc_bytes_under r.eunder;
          eover = loc_bits_to_loc_bytes r.eover }

    | TStartOf (thost, toffs) ->
        let r = eval_thost_toffset ~alarm_mode env thost toffs in
        { etype = TPtr (Cil.typeOf_array_elem r.etype, []);
          ldeps = r.ldeps;
          eunder = loc_bits_to_loc_bytes_under r.eunder;
          eover = loc_bits_to_loc_bytes r.eover }

    | TLval _ ->
      let lval = eval_tlval ~alarm_mode env t in
      let typ = lval.etype in
      let size = Eval_typ.sizeof_lval_typ typ in
      let state = env_current_state env in
      let eover_loc = make_loc (lval.eover) size in
      let eover = find_or_alarm ~alarm_mode state eover_loc in
      let eover = Cvalue_forward.make_volatile ~typ eover in
      let eover, alarms = Cvalue_forward.reinterpret fake_expr typ eover in
      check_alarms alarms alarm_mode;
      (* Skip dependencies if state is dead *)
      let deps =
        if Cvalue.Model.is_reachable state then
          add_deps env.e_cur empty_logic_deps
            (enumerate_valid_bits ~for_writing:false eover_loc)
        else empty_logic_deps
      in
      (* TODO: This is a rough evaluation of the
         underapproximation. A better one can be obtained as
         follows: whenever a memory case in the under-approximation
         contains a singleton in [state] (which is an
         overapproximation), it can be added to the resulting
         [eunder]. This requires a new special "Eval_op.find"
         operation. *)
      let eunder = under_from_over eover in
      { etype = typ;
        ldeps = join_logic_deps deps (lval.ldeps);
        eunder; eover }

    (* TBinOp ((LOr | LAnd), _t1, _t2) -> TODO: a special case would be useful.
       But this requires reducing the state after having evaluated t1 by
       a term that is in fact a predicate *)
    | TBinOp (op,t1,t2) -> eval_binop ~alarm_mode env op t1 t2

    | TUnOp (op, t) ->
        let r = eval_term ~alarm_mode env t in
        let typ' = match op with
          | Neg -> r.etype
          | BNot -> r.etype (* can only be used on an integer type *)
          | LNot -> Cil.intType
        in
        let v, alarms =
          Cvalue_forward.forward_unop ~context:unop_context r.etype op r.eover
        in
        check_alarms alarms alarm_mode;
        let eover = v in
        { etype = typ';
          ldeps = r.ldeps;
          eover; eunder = under_from_over eover }

    | Trange(otlow, othigh) ->
      (* The overapproximation is the range [min(low.eover)..max(high.eover)].
	 The underapproximation is the range [max(low.eover)..min(high.eover)].
         Perhaps surprisingly, we do not use the under-approximations of
         otlow and othigh to compute the underapproximation. We could
         potentially compute [min(max(low.over),  min(low.under) ..
                              max(min(high.over), max(high.under)]
         However, tsets cannot be used as bounds of ranges. By invariant (2),
         eunder is either Bottom, or equal to eover, both being of cardinal
         one. In both cases, using eover is more precise. *)
      let deps = ref empty_logic_deps in
      let min v =
	try (match Ival.min_int (Cvalue.V.project_ival v) with
	| None -> `Approx
	| Some(x) -> `Finite(x))
	with Cvalue.V.Not_based_on_null -> `Approx
      in
      let max v =
	try (match Ival.max_int (Cvalue.V.project_ival v) with
	| None -> `Approx
	| Some(x) -> `Finite(x))
	with Cvalue.V.Not_based_on_null -> `Approx
      in
      (* Evaluate a bound:
	 - [sure_bound_under] is returned for the under-approximation when the
           bound is explicitly omitted in the ACSL term
	 - [min_max_*] is the function to retrieve the bound from the
           over_approximation, for both the underapproximation and the
           overapproximation. *)
      let eval_bound sure_bound_under min_max_under min_max_over = function
	| None -> sure_bound_under, `Approx
	| Some(result) ->
	  try
	    let result = eval_term ~alarm_mode env result in
	    deps := join_logic_deps !deps result.ldeps;
	    let under = min_max_under result.eover in
	    let over = min_max_over result.eover in
	    under, over
	  with LogicEvalError e ->
            if e <> CAlarm then
              Value_parameters.result ~source:(fst t.term_loc) ~once:true
                "Cannot evaluate@ range bound %a@ (%a). Approximating"
                Printer.pp_term result pretty_logic_evaluation_error e;
	    `Approx, `Approx
      in
      let min_under, min_over = eval_bound `MinusInf max min otlow in
      let max_under, max_over = eval_bound `PlusInf min max othigh in
      let to_bound = function
	| `Finite x -> Some x
	| `PlusInf | `MinusInf | `Approx -> None
      in
      let eunder = match (min_under, max_under) with
	| `Approx, _ | _, `Approx -> Cvalue.V.bottom
	| (`MinusInf | `Finite _), (`PlusInf | `Finite _) ->
          Cvalue.V.inject_ival
	    (Ival.inject_range (to_bound min_under) (to_bound max_under))
      in
      let eover =
	Cvalue.V.inject_ival
	  (Ival.inject_range (to_bound min_over) (to_bound max_over))
      in
      { ldeps = !deps;
        etype = Cil.intType;
        eunder; eover }

    | TCastE (typ, t) ->
      let r = eval_term ~alarm_mode env t in
      let eover, eunder =
        (* See if the cast does something. If not, we can keep eunder as is.*)
        if is_noop_cast ~src_typ:t.term_type ~dst_typ:typ
        then r.eover, r.eunder
        else
          let eover, alarms =
            do_promotion ~rounding_mode:real_mode
              ~src_typ:r.etype ~dst_typ:typ fake_expr r.eover
          in
          check_alarms alarms alarm_mode;
          eover, under_from_over eover
      in
      { etype = typ; ldeps = r.ldeps; eunder; eover }

    | Tif (tcond, ttrue, tfalse) ->
      eval_tif eval_term Cvalue.V.join Cvalue.V.meet ~alarm_mode env
        tcond ttrue tfalse

    | TSizeOf _ | TSizeOfE _ | TSizeOfStr _ | TAlignOf _ | TAlignOfE _ ->
        let e = Cil.constFoldTerm true t in
        let v = match e.term_node with
          | TConst (Integer (v, _)) -> Cvalue.V.inject_int v
          | _ -> V.top_int
        in
        einteger v

    | Tunion l ->
      let eunder, eover, deps = List.fold_left
          (fun (accunder, accover, accdeps) t ->
             let r = eval_term ~alarm_mode env t in
             (Cvalue.V.link accunder r.eunder,
              Cvalue.V.join accover r.eover,
              join_logic_deps accdeps r.ldeps))
          (Cvalue.V.bottom, Cvalue.V.bottom, empty_logic_deps) l
      in
      { etype = infer_type t.term_type;
        ldeps = deps; eunder; eover }

    | Tempty_set ->
      { etype = infer_type t.term_type;
        ldeps = empty_logic_deps;
        eunder = Cvalue.V.bottom;
        eover = Cvalue.V.bottom }

    | Tnull ->
        { etype = Cil.voidPtrType;
          ldeps = empty_logic_deps;
          eunder = Cvalue.V.singleton_zero;
          eover = Cvalue.V.singleton_zero }

    | TLogic_coerce(ltyp, t) ->
        let r = eval_term ~alarm_mode env t in
        (* we must handle coercion from singleton to set, for which there is
           nothing to do, AND coercion from an integer type to a floating-point
           type, that require a conversion. *)
        (match Logic_const.plain_or_set Extlib.id ltyp with
           | Linteger ->
             assert (Logic_typing.is_integral_type t.term_type);
             r
           | Ctype typ when Cil.isIntegralOrPointerType typ -> r
           | Lreal ->
               if Logic_typing.is_integral_type t.term_type
               then (* Needs to be converted to reals *)
                 let conv v =
                   let v, ok = V.cast_int_to_float real_mode v in
                   if not ok then c_alarm ();
                   v
                 in
                 { etype = Cil.doubleType;
                   ldeps = r.ldeps;
                   eunder = under_from_over r.eover;
                   eover = conv r.eover }
               else r (* already a floating-point number (hopefully) *)
           | _ -> unsupported
               (Format.asprintf "logic coercion %a -> %a@."
                  Printer.pp_logic_type t.term_type Printer.pp_logic_type ltyp)
        )

    (* TODO: the meaning of the label in \offset and \base_addr is not obvious
       at all *)
    | Toffset (_lbl, t) ->
        let r = eval_term ~alarm_mode env t in
        let add_offset _ offs acc = Ival.join offs acc in
        let offs = Location_Bytes.fold_topset_ok add_offset r.eover Ival.bottom in
	let eover = Cvalue.V.inject_ival offs in
        { etype = Cil.intType;
          ldeps = r.ldeps;
          eover;
          eunder = under_from_over eover }

    | Tbase_addr (_lbl, t) ->
        let r = eval_term ~alarm_mode env t in
        let add_base b acc = V.join acc (V.inject b Ival.zero) in
        let eover = Location_Bytes.fold_bases add_base r.eover V.bottom in
        { etype = Cil.charPtrType;
          ldeps = r.ldeps;
          eover;
          eunder = under_from_over eover }

    | Tblock_length (_lbl, t) -> (* TODO: take label into account for locals *)
        let r = eval_term ~alarm_mode env t in
        let add_block_length b acc =
          let bl =
            (* Convert the validity frontiers into a range of bytes. The
               frontiers are always 0 or 8*k-1 (because validity is in bits and
               starts on zero), so we add 1 everywhere, then divide by eight. *)
            let convert start_bits end_bits =
              let congr_succ i = Int.(equal zero (rem (succ i) eight)) in
              let congr_or_zero i = Int.(equal zero i || congr_succ i) in
              assert (congr_or_zero start_bits || congr_or_zero end_bits);
              let start_bytes = Int.(pos_div (Int.succ start_bits) eight) in
              let end_bytes =   Int.(pos_div (Int.succ end_bits)   eight) in
              Ival.inject_range (Some start_bytes) (Some end_bytes)
            in
            match Base.validity b with
            | Base.Empty -> Ival.zero
            | Base.Invalid -> Ival.top (* we may also emit an alarm *)
            | Base.Known (_, ma) -> convert ma ma
            | Base.Unknown (mi, None, ma) -> convert mi ma
            | Base.Unknown (_, Some mi, ma) -> convert mi ma
            | Base.Variable weak_v ->
              convert weak_v.Base.min_alloc weak_v.Base.max_alloc
          in
          Ival.join acc bl
        in
	let bl = Location_Bytes.fold_bases add_block_length r.eover Ival.bottom in
	let eover = V.inject_ival bl in
        { etype = Cil.charPtrType;
          ldeps = r.ldeps;
          eover;
          eunder = under_from_over eover }

    | Tapp (li, labels, args) when
        li.l_var_info.lv_name = "strlen" && comes_from_fc_stdlib li.l_var_info ->
      begin
        match labels, args with
        | [lbl], [arg] ->
          let r = eval_term ~alarm_mode env arg in
          eval_logic_charlen Builtins_string.frama_c_strlen_wrapper
            { env with e_cur = lbl } arg r.eover r.ldeps
        | _ -> assert false (* length previously checked *)
      end
    | Tapp _ | Tlambda _ -> unsupported "logic functions or predicates"
    | TDataCons _ -> unsupported "logic inductive types"
    | TUpdate _ -> unsupported "functional updates"
    | TCoerce _ | TCoerceE _ -> unsupported "logic coercions" (* jessie *)
    | Ttype _ -> unsupported "\\type operator"
    | Ttypeof _ -> unsupported "\\typeof operator"
    | Tcomprehension _ -> unsupported "sets defined by comprehension"
    | Tinter _ -> unsupported "set intersection"
    | Tlet _ -> unsupported "\\let bindings"
    | TConst (LStr _) -> unsupported "constant strings"
    | TConst (LWStr _) -> unsupported "wide constant strings"

and eval_binop ~alarm_mode env op t1 t2 =
  if not (isLogicNonCompositeType t1.term_type) then
    if Value_parameters.debug_atleast 1 then
      unsupported (Format.asprintf
                     "operation (%a) %a (%a) on non-supported type %a"
                     Printer.pp_term t1
                     Printer.pp_binop op
                     Printer.pp_term t2
                     Printer.pp_logic_type t1.term_type)
    else
      unsupported (Format.asprintf
                     "%a operation on non-supported type %a"
                     Printer.pp_binop op
                     Printer.pp_logic_type t1.term_type)
  else
    let r1 = eval_term ~alarm_mode env t1 in
    let r2 = eval_term ~alarm_mode env t2 in
    let te1 = Cil.unrollType r1.etype in
    (* We use the type of t1 to determine whether we are performing an int or
       float operation.*)
    let int_or_float_op int_op float_op =
      match te1 with
      | TInt _ | TPtr _ | TEnum _ -> int_op
      | TFloat (fkind, _) -> float_op fkind
      | _ -> ast_error (Format.asprintf
                          "binop on incorrect type %a" Printer.pp_typ te1)
    in
    check_logic_alarms ~alarm_mode r1 op r2;
    let typ_res = infer_binop_res_type op te1 in
    let forward_integer =
      Cvalue_forward.forward_binop_int
        ~context:binop_context ~logic:true ~typ:te1
    and forward_float =
      Cvalue_forward.forward_binop_float_alarm
        ~context:binop_context real_mode
    in
    let kop = int_or_float_op forward_integer forward_float in
    let eover, alarms = kop r1.eover op r2.eover in
    check_alarms alarms alarm_mode;
    let default _fk _r1 _r2 = under_from_over eover in
    let add_untyped_op factor =
      int_or_float_op (V.add_untyped_under ~factor) default
    in
    let eunder_op = match op with
      | PlusPI | IndexPI -> begin
        match Bit_utils.osizeof_pointed te1 with
        | Int_Base.Top -> fun _ _ -> V.bottom
        | Int_Base.Value _ as size -> add_untyped_op size
      end
      | PlusA -> add_untyped_op (Int_Base.one)
      | MinusA -> add_untyped_op (Int_Base.minus_one)
      | _ -> fun _ _ -> under_from_over eover
    in
    let eunder = eunder_op r1.eunder r2.eunder in
    { etype = typ_res;
      ldeps = join_logic_deps r1.ldeps r2.ldeps;
      eunder; eover }

and eval_tlhost ~alarm_mode env lv =
  match lv with
    | TVar { lv_origin = Some v } ->
        let loc = Location_Bits.inject (Base.of_varinfo v) Ival.zero in
        { etype = v.vtype;
          ldeps = empty_logic_deps;
          eover = loc;
          eunder = under_loc_from_over loc }
    | TResult typ ->
        (match env.result with
          | Some v ->
              let loc = Location_Bits.inject (Base.of_varinfo v) Ival.zero in
              { etype = typ;
                ldeps = empty_logic_deps;
                eunder = loc; eover = loc }
          | None -> no_result ())
    | TVar ({ lv_origin = None } as tlv) ->
        let b, ty = supported_logic_var tlv in
        let loc = Location_Bits.inject b Ival.zero in
        { etype = ty;
          ldeps = empty_logic_deps;
          eover = loc;
          eunder = under_loc_from_over loc }
    | TMem t ->
      let r = eval_term ~alarm_mode env t in
      let tres = match Cil.unrollType r.etype with
        | TPtr (t, _) -> t
        | _ -> ast_error "*p where p is not a pointer"
      in
      { etype = tres;
        ldeps = r.ldeps;
        eunder = loc_bytes_to_loc_bits r.eunder;
        eover = loc_bytes_to_loc_bits r.eover }

and eval_toffset ~alarm_mode env typ toffset =
  match toffset with
  | TNoOffset ->
      { etype = typ;
        ldeps = empty_logic_deps;
        eunder = Ival.zero;
        eover = Ival.zero }
  | TIndex (idx, remaining) ->
      let typ_e, size = match Cil.unrollType typ with
        | TArray (t, size, _, _) -> t, size
        | _ -> ast_error "index on a non-array"
      in
      let idx = constraint_trange idx size in
      let idxs = eval_term ~alarm_mode env idx in
      let offsrem = eval_toffset ~alarm_mode env typ_e remaining in
      let size_e = Bit_utils.sizeof typ_e in
      let eover =
        let offset =
          try Cvalue.V.project_ival_bottom idxs.eover
          with Cvalue.V.Not_based_on_null -> Ival.top
        in
	let offset = Ival.scale_int_base size_e offset in
	Ival.add_int offset offsrem.eover
      in
      let eunder =
        let offset =
          try Cvalue.V.project_ival idxs.eunder
          with Cvalue.V.Not_based_on_null -> Ival.bottom
        in
	let offset = match size_e with
	  | Int_Base.Top -> Ival.bottom
	  (* Note: scale_int_base would overapproximate when given a
	     Float.  Should never happen. *)
	  | Int_Base.Value f ->
	    (match offset with | Ival.Float _ -> assert false | _ -> ());
	    Ival.scale f offset
	in
	Ival.add_int_under offset offsrem.eunder
      in
      { etype = offsrem.etype;
        ldeps = join_logic_deps idxs.ldeps offsrem.ldeps;
        eunder; eover }

  | TField (fi, remaining) ->
      let size_current default =
        try Ival.of_int (fst (Cil.bitsOffset typ (Field(fi, NoOffset))))
        with Cil.SizeOfError _ -> default
      in
      let attrs = Cil.filter_qualifier_attributes (Cil.typeAttrs typ) in
      let typ_fi = Cil.typeAddAttributes attrs fi.ftype in
      let offsrem = eval_toffset ~alarm_mode env typ_fi remaining in
      { etype = offsrem.etype;
        ldeps = offsrem.ldeps;
        eover = Ival.add_int (size_current Ival.top) offsrem.eover;
        eunder = Ival.add_int_under (size_current Ival.bottom) offsrem.eunder }

  | TModel _ -> unsupported "model fields"

and eval_thost_toffset ~alarm_mode env thost toffs =
  let rhost = eval_tlhost ~alarm_mode env thost in
  let roffset = eval_toffset ~alarm_mode env rhost.etype toffs in
  { etype = roffset.etype;
    ldeps = join_logic_deps rhost.ldeps roffset.ldeps;
    eunder = Location_Bits.shift_under roffset.eunder rhost.eunder;
    eover = Location_Bits.shift roffset.eover rhost.eover;
  }

and eval_tlval ~alarm_mode env t =
  match t.term_node with
  | TLval (thost, toffs) ->
      eval_thost_toffset ~alarm_mode env thost toffs
  | Tunion l ->
    let eunder, eover, deps = List.fold_left
        (fun (accunder, accover, accdeps) t ->
           let r = eval_tlval ~alarm_mode env t in
           Location_Bits.link accunder r.eunder,
           Location_Bits.join accover r.eover,
           join_logic_deps accdeps r.ldeps
        ) (Location_Bits.top, Location_Bits.bottom, empty_logic_deps) l
    in
      { etype = infer_type t.term_type;
        ldeps = deps;
        eover; eunder }
  | Tempty_set ->
      { etype = infer_type t.term_type;
        ldeps = empty_logic_deps;
        eunder = Location_Bits.bottom;
        eover = Location_Bits.bottom }
  | Tat (t, lab) ->
      ignore (env_state env lab);
      eval_tlval ~alarm_mode { env with e_cur = lab } t
  | TLogic_coerce (_lt, t) ->
      (* Logic coerce on locations (that are pointers) can only introduce
         sets, that do not change the abstract value. *)
      eval_tlval ~alarm_mode env t
  | Tif (tcond, ttrue, tfalse) ->
    eval_tif eval_tlval Location_Bits.join Location_Bits.meet ~alarm_mode env
      tcond ttrue tfalse
  | _ -> ast_error (Format.asprintf "non-lval term %a" Printer.pp_term t)

and eval_tif : 'a. (alarm_mode:_ -> _ -> _ -> 'a eval_result) -> ('a -> 'a -> 'a) -> ('a -> 'a -> 'a) -> alarm_mode:_ -> _ -> _ -> _ -> _ -> 'a eval_result =
  fun eval join meet ~alarm_mode env tcond ttrue tfalse ->
    let r = eval_term ~alarm_mode env tcond in
    let ctrue =  Cvalue.V.contains_non_zero r.eover
    and cfalse =  Cvalue.V.contains_zero r.eover in
    match ctrue, cfalse with
    | true, true ->
      let vtrue = eval ~alarm_mode env ttrue in
      let vfalse = eval ~alarm_mode env tfalse in
      if not (same_etype vtrue.etype vfalse.etype) then
        Value_parameters.failure ~current:true
          "Incoherent types in conditional: %a vs. %a. \
           Please report"
          Printer.pp_typ vtrue.etype Printer.pp_typ vfalse.etype;
      let eover = join vtrue.eover vfalse.eover in
      let eunder = meet vtrue.eunder vfalse.eunder in
      { etype = vtrue.etype;
        ldeps = join_logic_deps vtrue.ldeps vfalse.ldeps;
        eunder; eover }
    | true, false  -> eval ~alarm_mode env ttrue
    | false, true  -> eval ~alarm_mode env tfalse
    | false, false -> assert false (* a logic alarm would have been raised*)

let eval_tlval_as_location ~alarm_mode env t =
  let r = eval_tlval ~alarm_mode env t in
  let s = Eval_typ.sizeof_lval_typ r.etype in
  make_loc r.eover s

let eval_tlval_as_location_with_deps ~alarm_mode env t =
  let r = eval_tlval ~alarm_mode env t in
  let s = Eval_typ.sizeof_lval_typ r.etype in
  (make_loc r.eover s, r.ldeps)


(* Return a pair of (under-approximating, over-approximating) zones. *)
let eval_tlval_as_zone_under_over ~alarm_mode ~for_writing env t =
  let r = eval_tlval ~alarm_mode env t in
  let s = Eval_typ.sizeof_lval_typ r.etype in
  let under = enumerate_valid_bits_under ~for_writing (make_loc r.eunder s) in
  let over = enumerate_valid_bits ~for_writing (make_loc r.eover s) in
  (under, over)

let eval_tlval_as_zone ~alarm_mode ~for_writing env t =
  let _under, over =
    eval_tlval_as_zone_under_over ~alarm_mode ~for_writing env t
  in
  over

(* If casting [trm] to [typ] has no effect in terms of the values contained
   in [trm], do nothing. Otherwise, raise [exn]. Adapted from [pass_cast] *)
let pass_logic_cast exn typ trm =
  match Logic_utils.unroll_type typ, Logic_utils.unroll_type trm.term_type with
    | Linteger, Ctype (TInt _ | TEnum _) -> () (* Always inclusion *)
    | Ctype (TInt _ | TEnum _ as typ), Ctype (TInt _ | TEnum _ as typeoftrm) ->
        let sztyp = sizeof typ in
        let szexpr = sizeof typeoftrm in
        let styp, sexpr =
          match sztyp, szexpr with
            | Int_Base.Value styp, Int_Base.Value sexpr -> styp, sexpr
            | _ -> raise exn
        in
        let sityp = is_signed_int_enum_pointer typ in
        let sisexpr = is_signed_int_enum_pointer typeoftrm in
        if (Int.ge styp sexpr && sityp = sisexpr) (* larger, same signedness *)
    || (Int.gt styp sexpr && sityp) (* strictly larger and signed *)
        then ()
        else raise exn

    | Lreal,  Ctype (TFloat _) -> () (* Always inclusion *)
    | Ctype (TFloat (f1,_)), Ctype (TFloat (f2, _)) ->
        if Cil.frank f1 < Cil.frank f2
        then raise exn

    | _ -> raise exn (* Not a scalar type *)


exception Not_an_exact_loc

(* Evaluate a term as a non-empty under-approximated location, or raise
   [Not_an_exact_loc]. *)
let rec eval_term_as_exact_locs ~alarm_mode env t =
  match t with
    | { term_node = TLval _ } ->
        let loc = eval_tlval ~alarm_mode env t in
        let typ = loc.etype in
        (* eval_term_as_exact_loc is only used for reducing values, and we must
           NOT reduce volatile locations. *)
        if Cil.typeHasQualifier "volatile" typ then raise Not_an_exact_loc;
        let loc = Locations.make_loc loc.eunder (Eval_typ.sizeof_lval_typ typ)in
        if Locations.is_bottom_loc loc then raise Not_an_exact_loc;
        typ, loc

    | { term_node = TLogic_coerce(_, t)} ->
        (* It is always ok to pass through a TLogic_coerce, as the destination
           type is always a supertype *)
        eval_term_as_exact_locs ~alarm_mode env t

    | { term_node = TCastE (ctype, t') } ->
        pass_logic_cast Not_an_exact_loc (Ctype ctype) t';
        eval_term_as_exact_locs ~alarm_mode env t'

    | _ -> raise Not_an_exact_loc


(* -------------------------------------------------------------------------- *)
(* --- Evaluation and reduction by predicates                             --- *)
(* -------------------------------------------------------------------------- *)

(** Auxiliary functions *)

let is_same_term_coerce t1 t2 =
  match t1.term_node, t2.term_node with
    | TLogic_coerce _, TLogic_coerce _ -> Logic_utils.is_same_term t1 t2
    | TLogic_coerce (_,t1), _ -> Logic_utils.is_same_term t1 t2
    | _, TLogic_coerce(_,t2) -> Logic_utils.is_same_term t1 t2
    | _ -> Logic_utils.is_same_term t1 t2


(* Evaluates a [valid_read_string] or [valid_read_wstring] predicate
   using str* builtins.
   - if [bottom] is obtained, return False;
   - otherwise, if no alarms are emitted, return True;
   - otherwise, return [Unknown]. *)
let eval_valid_read_str ~wide env arg v =
  let wrapper =
    if wide then (Builtins_string.frama_c_wcslen_wrapper ())
    else Builtins_string.frama_c_strlen_wrapper
  in
  match logic_charlen_builtin wrapper (env_current_state env) arg v with
  | None -> (* bottom state => string always invalid *) False
  | Some (_res, alarms) ->
    if Builtins_string.String_alarms.Set.is_empty alarms
    then (* no alarm => string always valid for reading *) True
    else (* alarm => string possibly invalid *) Unknown

(* Evaluates a [valid_string] or [valid_wstring] predicate.
   First, we check the constness of the arguments.
   Then, we evaluate [valid_read_string/valid_read_wstring] on non-const ones. *)
let eval_valid_str ~wide env arg v =
  assert (not (Cvalue.V.is_bottom v));
  (* filter const bases *)
  let v' = Cvalue.V.filter_base (fun b -> not (Base.is_read_only b)) v in
  if Cvalue.V.is_bottom v' then False (* all bases were const *)
  else
  if Cvalue.V.equal v v' then
    eval_valid_read_str ~wide env arg v (* all bases non-const *)
  else (* at least one base was const *)
    match eval_valid_read_str ~wide env arg v with
    | True -> Unknown (* weaken result *)
    | False | Unknown as r -> r


(* Do all the possible values of a location in [state] satisfy [test]?  [loc] is
   an over-approximation of the location, so the answer cannot be [False] even
   if some parts of [loc] do not satisfy [test]. Thus, this function does not
   fold the location, but instead applies [test] to the join of all values
   stored in [loc] in [state].  *)
let forall_in_over_location state loc test =
  let v = Model.find_indeterminate state loc in
  test v

exception EFalse

(* Do all the possible values of a location in [state] satisfy [test]?  [loc] is
   an under-approximation of the location, so the answer cannot be [True], as
   the values of some other parts of the location may not satisfy [test].
   However, it is [False] as soon as some part of [loc] contradicts [test]. *)
let forall_in_under_location state loc test =
  let inspect_value (_, _) (value, _, _) acc =
    match test value with
    | True | Unknown -> acc
    | False -> raise EFalse
  in
  let inspect_itv base itv acc =
    match Cvalue.Model.find_base_or_default base state with
    | `Top | `Bottom -> Unknown
    | `Value offsm ->
      Cvalue.V_Offsetmap.fold_between ~entire:true itv inspect_value offsm acc
  in
  let inspect_base base intervals acc =
    Int_Intervals.fold (inspect_itv base) intervals acc
  in
  let zone = Locations.enumerate_bits loc in
  try Zone.fold_i inspect_base zone Unknown
  with EFalse -> False
     | Abstract_interp.Error_Top -> Unknown

(* Evaluates an universal predicate about the values of a location evaluated to
   [r] in [state]. The predicates holds whenever all the possible values at the
   location satisfy [test]. *)
let eval_forall_predicate state r test =
  let size_bits = Eval_typ.sizeof_lval_typ r.etype in
  let make_loc loc = make_loc loc size_bits in
  let over_loc = make_loc r.eover in
  if not (Locations.is_valid ~for_writing:false over_loc) then c_alarm ();
  match forall_in_over_location state over_loc test with
  | Unknown ->
    let under_loc = make_loc r.eunder in
    forall_in_under_location state under_loc test
  | True -> True
  | False -> False

(* Evaluation of an \initialized predicate on a location evaluated to [r]
   in the state [state]. *)
let eval_initialized state r =
  let test = function
    | V_Or_Uninitialized.C_init_esc _
    | V_Or_Uninitialized.C_init_noesc _ -> True
    | V_Or_Uninitialized.C_uninit_esc _ -> Unknown
    | V_Or_Uninitialized.C_uninit_noesc v ->
      if Location_Bytes.is_bottom v then False else Unknown
  in
  eval_forall_predicate state r test

(* Evaluation of a \dangling predicate on a location evaluated to [r]
   in the state [state]. *)
let eval_dangling state r =
  let test = function
    | V_Or_Uninitialized.C_init_esc v ->
      if Location_Bytes.is_bottom v then True else Unknown
    | V_Or_Uninitialized.C_uninit_esc _ -> Unknown
    | V_Or_Uninitialized.C_init_noesc _
    | V_Or_Uninitialized.C_uninit_noesc _ -> False
  in
  eval_forall_predicate state r test

let is_rel_binop = function
  | Lt
  | Gt
  | Le
  | Ge
  | Eq
  | Ne -> true
  | _ -> false

let rel_of_binop = function
  | Lt -> Rlt
  | Gt -> Rgt
  | Le -> Rle
  | Ge -> Rge
  | Eq -> Req
  | Ne -> Rneq
            | _ -> assert false

exception DoNotReduce
exception Reduce_to_bottom

let reduce_by_papp env positive li _labels args =
  match positive, li.l_var_info.lv_name, args with
    | true, "\\is_finite", [arg] -> begin
      try
       let alarm_mode = alarm_reduce_mode () in
	let typ_loc, locs = eval_term_as_exact_locs ~alarm_mode
     env arg in
        let aux loc env =
	  let state = env_current_state env in
          let v = find_or_alarm ~alarm_mode state loc in
          let v =  Cvalue_forward.unsafe_reinterpret typ_loc v in
          let state' = Cvalue.Model.reduce_previous_binding state loc v in
          overwrite_current_state env state'
        in
        Eval_op.apply_on_all_locs aux locs env
      with LogicEvalError _ | Not_an_exact_loc -> env
    end

    | true, "\\subset", [argl;argr] -> begin
      try
        let alarm_mode = alarm_reduce_mode () in
        let vr = (eval_term ~alarm_mode env argr).eover in
        let _typ, locsl = eval_term_as_exact_locs ~alarm_mode env argl in
        let aux locl env =
          let state = env_current_state env in
          let vl = find_or_alarm ~alarm_mode state locl in
          let reduced = V.narrow vl vr in
          if V.equal V.bottom reduced then raise Reduce_to_bottom;
          let state' =
            Cvalue.Model.reduce_previous_binding state locl reduced
          in
          overwrite_current_state env state'
        in
        Eval_op.apply_on_all_locs aux locsl env
      with
      | LogicEvalError _ | Not_an_exact_loc -> env
      | Reduce_to_bottom -> overwrite_current_state env Model.bottom
    end

    | _ -> env

let reduce_by_valid env positive ~for_writing (tset: term) =
  (* Auxiliary function that reduces \valid(lv+offs), where lv is atomic
     (no more tsets), and offs is a bits-expressed constant offset.
     [offs_typ] is supposed to be the type of the pointed location after [offs]
     has been applied; it can be different from [typeOf_pointed lv], for
     example if offset is a field access. *)
  let aux lv env (offs_typ, offs) =
    try
      if not (Location_Bits.cardinal_zero_or_one lv.eover) ||
         not (Ival.cardinal_zero_or_one offs)
      then raise DoNotReduce;
      let state = env_current_state env in
      let lvloc = make_loc lv.eover (Eval_typ.sizeof_lval_typ lv.etype) in
      (* [p] is the range that we attempt to reduce *)
      let alarm_mode = alarm_reduce_mode () in
      let p_orig = find_or_alarm ~alarm_mode state lvloc in
      let pb = Locations.loc_bytes_to_loc_bits p_orig in
      let shifted_p = Location_Bits.shift offs pb in
      let lshifted_p = make_loc shifted_p (Eval_typ.sizeof_lval_typ offs_typ) in
      let valid = (* reduce the shifted pointer to the wanted part *)
        if positive
        then Locations.valid_part ~for_writing lshifted_p
        else Locations.invalid_part lshifted_p
      in
      let valid = valid.loc in
      if Location_Bits.equal shifted_p valid
      then env
      else
	(* Shift back *)
	let shift = Ival.neg_int offs in
	let pb = Location_Bits.shift shift valid in
	let p = Locations.loc_bits_to_loc_bytes pb in
	(* Store the result *)
	let state = Model.reduce_previous_binding state lvloc p in
	overwrite_current_state env state
    with
    | DoNotReduce | V.Not_based_on_null | Cil.SizeOfError _ | LogicEvalError _
      -> env
  in
  (*  Auxiliary function to reduce by the under-approximation of an offset.
      Since validities are contiguous, we simply reduce by the minimum and
      maximum of the under-approximation. *)
  let aux_min_max_offset f env off =
    try
      let env = match Ival.min_int off with
        | None -> env
        | Some min -> f env (Ival.inject_singleton min)
      in
      match Ival.max_int off with
      | None -> env
      | Some max -> f env (Ival.inject_singleton max)
    with Abstract_interp.Error_Bottom -> env
  in
  let rec do_one env t =
    match t.term_node with
      | Tunion l ->
          List.fold_left do_one env l

    | TLval _ ->
          let aux typ loc env =
            try
              let state =
                Eval_op.reduce_by_valid_loc ~positive ~for_writing
                  loc typ (env_current_state env)
              in
              overwrite_current_state env state
            with LogicEvalError _ -> env
          in
          (try
             let alarm_mode = alarm_reduce_mode () in
             let r = eval_tlval ~alarm_mode env t in
             let loc = make_loc r.eunder (Eval_typ.sizeof_lval_typ r.etype) in
             let r = Eval_op.apply_on_all_locs (aux r.etype) loc env in
             r
           with LogicEvalError _ -> env)

      | TAddrOf (TMem ({term_node = TLval _} as t), offs) ->
          (try
             let alarm_mode = alarm_reduce_mode () in
             let lt = eval_tlval ~alarm_mode env t in
             let typ = lt.etype in
             (* Compute the offsets, that depend on the type of the lval.
                The computed list is exactly what [aux] requires *)
             let roffs =
               eval_toffset ~alarm_mode env (Cil.typeOf_pointed typ) offs
             in
             let aux env offs = aux lt env (roffs.etype, offs) in
             aux_min_max_offset aux env roffs.eunder
           with LogicEvalError _ -> env)

      | TBinOp ((PlusPI | MinusPI) as op, ({term_node = TLval _} as tlv), i) ->
          (try
             let alarm_mode = alarm_reduce_mode () in
             let rtlv = eval_tlval ~alarm_mode env tlv in
             let ri = eval_term ~alarm_mode env i in
             (* Convert offsets to a simpler form if [op] is [MinusPI] *)
	     let li =
	       try V.project_ival ri.eunder
	       with V.Not_based_on_null -> raise Exit
	     in
	     let li = if op = PlusPI then li else Ival.neg_int li in
             let typ_p = Cil.typeOf_pointed rtlv.etype in
             let sbits = Int.of_int (Cil.bitsSizeOf typ_p) in
             (* Compute the offsets expected by [aux], which are [i *
                8 * sizeof( *tlv)] *)
             let li = Ival.scale sbits li in
             (* Now reduce [tlv] by values possible for [i] *)
             let aux env offs = aux rtlv env (typ_p, offs) in
             aux_min_max_offset aux env li
           with
           | LogicEvalError _ | Exit -> env
          )
      | _ -> env
  in
  do_one env tset

let rec reduce_by_relation ~alarm_mode env positive t1 rel t2 =
  (* special case: t1 is a term of the form "a rel' b",
     and is compared to "== 0" or "!= 0" => evaluate t1 directly;
     note: such terms may be created by other evaluation/reduction functions
     e.g. eval_predicate, reduce_by_predicate_content *)
  match t1.term_node, rel with
  | TBinOp (bop, t1', t2'), Rneq when is_rel_binop bop && Cil.isLogicZero t2 ->
    reduce_by_relation ~alarm_mode env positive t1' (rel_of_binop bop) t2'
  | TBinOp (bop, t1', t2'), Req when is_rel_binop bop && Cil.isLogicZero t2 ->
    reduce_by_relation ~alarm_mode env (not positive) t1' (rel_of_binop bop) t2'
  | _ ->
    let env = reduce_by_left_relation ~alarm_mode env positive t1 rel t2 in
    let sym_rel = match rel with
      | Rgt -> Rlt | Rlt -> Rgt | Rle -> Rge | Rge -> Rle
      | Req -> Req | Rneq -> Rneq
    in
    reduce_by_left_relation ~alarm_mode env positive t2 sym_rel t1

(* reduce [tl] so that [rl rel tr] holds *)
and reduce_by_left_relation ~alarm_mode env positive tl rel tr =
  try
    let debug = false in
    if debug then Format.printf "#Left term %a@." Printer.pp_term tl;
    let typ_loc, locs = eval_term_as_exact_locs ~alarm_mode env tl in
    let reduce = Eval_op.backward_comp_left_from_type typ_loc in
    let rtl = eval_term ~alarm_mode env tr in
    let cond_v = rtl.eover in
    if debug then Format.printf "#Val right term %a@." V.pretty cond_v;
    let aux loc env =
      let state = env_current_state env in
      if debug then Format.printf "#Left term as lv loc %a, typ %a@."
          Locations.pretty loc Printer.pp_typ typ_loc;
      let v = find_or_alarm ~alarm_mode state loc in
      if debug then Format.printf "#Val left lval %a@." V.pretty v;
      let v, alarms = Cvalue_forward.reinterpret fake_expr typ_loc v in
      let _ = check_alarms alarms alarm_mode in
      if debug then Format.printf "#Cast left lval %a@." V.pretty v;
      let comp = Value_util.conv_relation rel in
      let v' = reduce positive comp v cond_v in
      if debug then Format.printf "#Val reduced %a@." V.pretty v';
      (* TODOBY: if loc is an int that has been silently cast to real, we end
         up reducing an int according to a float. Instead, we should convert v
         to  real, then cast back v_asym to the good range *)
      if V.is_bottom v' then raise Reduce_to_bottom;
      if V.equal v' v then
        env
      else
        let state' =
          Cvalue.Model.reduce_previous_binding state loc v'
        in
        overwrite_current_state env state'
    in
    Eval_op.apply_on_all_locs aux locs env
  with Not_an_exact_loc | LogicEvalError _ -> env


(** Big recursive functions for predicates *)

let rec reduce_by_predicate ~alarm_mode env positive p =
  let loc = p.pred_loc in
  let rec reduce_by_predicate_content env positive p_content =
    match positive,p_content with
    | true,Ptrue | false,Pfalse -> env

    | true,Pfalse | false,Ptrue ->
      overwrite_current_state env Cvalue.Model.bottom

    (* desugared form of a <= b <= c <= d *)
    | true, Pand (
        {pred_content=Pand (
             {pred_content=Prel ((Rlt | Rgt | Rle | Rge | Req as op),_ta,tb) as p1},
             {pred_content=Prel (op', tb',tc) as p2})},
        {pred_content=Prel (op'',tc',_td) as p3})
      when
        op = op' && op' = op'' &&
        is_same_term_coerce tb tb' &&
        is_same_term_coerce tc tc'
      ->
      let red env p = reduce_by_predicate_content env positive p in
      let env = red env p1 in
      let env = red env p3 in
      let env = red env p2 in
      (*Not really useful in practice*)
      (*let env = red env (Prel (op, ta, tc)) in
        let env = red env (Prel (op, tb, td)) in *)
      env

    | true,Pand (p1,p2) | false,Por(p1,p2)->
      let r1 = reduce_by_predicate ~alarm_mode env positive p1 in
      reduce_by_predicate ~alarm_mode r1 positive p2

    | true,Por (p1,p2 ) | false,Pand (p1, p2) ->
      let env1 = reduce_by_predicate ~alarm_mode env positive p1 in
      let env2 = reduce_by_predicate ~alarm_mode env positive p2 in
      join_env env1 env2

    | true,Pimplies (p1,p2) ->
      let env1 = reduce_by_predicate ~alarm_mode env false p1 in
      let env2 = reduce_by_predicate ~alarm_mode env true p2 in
      join_env env1 env2

    | false,Pimplies (p1,p2) ->
      reduce_by_predicate ~alarm_mode
        (reduce_by_predicate ~alarm_mode env true p1)
        false
        p2

    | _,Pnot p -> reduce_by_predicate ~alarm_mode env (not positive) p

    | true,Piff (p1, p2) ->
      let red1 = reduce_by_predicate_content env true (Pand (p1, p2)) in
      let red2 = reduce_by_predicate_content env false (Por (p1, p2)) in
      join_env red1 red2

    | false,Piff (p1, p2) ->
      reduce_by_predicate ~alarm_mode env true
        (Logic_const.por ~loc
           (Logic_const.pand ~loc (p1, Logic_const.pnot ~loc p2),
            Logic_const.pand ~loc (Logic_const.pnot ~loc p1, p2)))

    | _,Pxor(p1,p2) ->
      reduce_by_predicate ~alarm_mode env
        (not positive) (Logic_const.piff ~loc (p1, p2))

    | _,Prel (op,t1,t2) ->
      begin
        try
          (* ugly, but eval_predicate_content does not exist yet *)
          let p = Logic_const.unamed ~loc p_content in
          let p' = if positive then p else Logic_const.pnot ~loc p in
          (* Evaluate the predicate before reducing. In some cases, although
             evaluation results in Bottom, reduction fails to reduce the
             resulting env to Bottom, and we lose precision. *)
          match eval_predicate env p' with
          | True -> env
          | False -> overwrite_current_state env Cvalue.Model.bottom
          | Unknown -> reduce_by_relation ~alarm_mode env positive t1 op t2
        with
        | DoNotReduce | LogicEvalError _ -> env
        | Reduce_to_bottom ->
          overwrite_current_state env Cvalue.Model.bottom
          (* if the exception was obtained without an alarm emitted,
             it is correct to return the bottom state *)
      end

    | _,Pvalid (_label,tsets) ->
      (* TODO: label should not be ignored. Instead, we should clear
         variables that are not in scope at the label. *)
      reduce_by_valid env positive ~for_writing:true  tsets
    | _,Pvalid_read (_label,tsets) ->
      reduce_by_valid env positive ~for_writing:false tsets

    | _,Pvalid_function _tsets -> env (* TODO *)

    | _,(Pinitialized (lbl_initialized,tsets)
        | Pdangling (lbl_initialized,tsets)) ->
      begin
        try
          let alarm_mode = alarm_reduce_mode () in
          (* See comments in the code for the evaluation of Pinitialized *)
          let star_tsets = deref_tsets tsets in
          let rlocb = eval_tlval ~alarm_mode env star_tsets in
          let size = Eval_typ.sizeof_lval_typ rlocb.etype in
          let state = env_state env lbl_initialized in
          let fred = match p_content with
            | Pinitialized _ -> V_Or_Uninitialized.reduce_by_initializedness
            | Pdangling _ -> V_Or_Uninitialized.reduce_by_danglingness
            | _ -> assert false
          in
          let fred = Eval_op.reduce_by_initialized_defined (fred positive) in
          let state_reduced =
            let loc = make_loc rlocb.eunder size in
            let loc = Eval_op.make_loc_contiguous loc in
            Eval_op.apply_on_all_locs fred loc state
          in
          overwrite_state env state_reduced lbl_initialized
        with LogicEvalError _ -> env
      end

    | _,Pat (p, lbl) ->
      (try
         let env_at = { env with e_cur = lbl } in
         let env' = reduce_by_predicate ~alarm_mode env_at positive p in
         { env' with e_cur = env.e_cur }
       with LogicEvalError _ -> env)

    | true, Pforall (varl, p) | false, Pexists (varl, p) ->
      begin
        try
          (* TODO: add case analysis on the variables of the quantification
             that are constrained *)
          let env = bind_logic_vars env varl in
          let env_result = reduce_by_predicate ~alarm_mode env true p in
          unbind_logic_vars env_result varl
        with LogicEvalError _ -> env
      end
    | _,Papp (li, labels, args) ->
      reduce_by_papp env positive li labels args
    | _,Pif (tcond, ptrue, pfalse) ->
      begin
        let reduce = reduce_by_predicate ~alarm_mode in
        let r = eval_term ~alarm_mode env tcond in
        let ctrue = Cvalue.V.contains_non_zero r.eover in
        let cfalse = Cvalue.V.contains_zero r.eover in
        match ctrue, cfalse with
        | true, true ->
          let reduce_by_rel =
            reduce_by_relation ~alarm_mode env positive tcond
          in
          let env_true = reduce_by_rel Cil_types.Rneq (Cil.lzero ()) in
          let env_false = reduce_by_rel Cil_types.Req (Cil.lzero ()) in
          let env_true =  reduce env_true positive ptrue in
          let env_false = reduce env_false positive pfalse in
          join_env env_true env_false
        | true, false -> reduce env positive ptrue
        | false, true -> reduce env positive pfalse
        | false, false -> assert false (* a logic alarm would have been raised*)
      end
    | true, Pexists (_, _) | false, Pforall (_, _)
    | _,Plet (_, _)
    | _,Pallocable (_,_) | _,Pfreeable (_,_) | _,Pfresh (_,_,_,_)
    | _,Psubtype _
    | _, Pseparated _
      -> env
  in
  reduce_by_predicate_content env positive p.pred_content

and eval_predicate env pred =
  let alarm_mode = Fail in
  let loc = pred.pred_loc in
  let rec do_eval env p =
    match p.pred_content with
    | Ptrue -> True
    | Pfalse -> False
    | Pand (p1,p2 ) ->
        begin match do_eval env p1 with
        | True -> do_eval env p2
        | False -> False
        | Unknown ->
          let reduced = reduce_by_predicate ~alarm_mode env true p1 in
          match do_eval reduced p2 with
            | False -> False
            | _ -> Unknown
        end
    | Por (p1,p2 ) ->
        let val_p1 = do_eval env p1 in
        (*Format.printf "Disjunction: state %a p1:%a@."
            Cvalue.Model.pretty (env_current_state env)
            Printer.pp_predicate p1; *)
        begin match val_p1 with
        | True -> True
        | False -> do_eval env p2
        | Unknown -> begin
          let reduced_state = reduce_by_predicate ~alarm_mode env false p1 in
          (* Format.printf "Disjunction: reduced to %a to eval %a@."
             Cvalue.Model.pretty (env_current_state reduced_state)
             Printer.pp_predicate p2; *)
          match do_eval reduced_state p2 with
            | True -> True
            | _ -> Unknown
          end
        end
    | Pxor (p1,p2) ->
        begin match do_eval env p1, do_eval env p2 with
          | True, True -> False
          | False, False -> False
          | True, False | False, True -> True
          | Unknown, _ | _, Unknown -> Unknown
        end
    | Piff (p1,p2 ) ->
        begin match do_eval env p1,do_eval env p2 with
        | True, True | False, False ->  True
        | Unknown, _ | _, Unknown -> Unknown
        | _ -> False
        end
    | Pat (p, lbl) -> begin
        ignore (env_state env lbl);
        try do_eval { env with e_cur = lbl } p
        with LogicEvalError ee -> display_evaluation_error ~loc:p.pred_loc ee; Unknown
      end

    | Pvalid (_label, tsets) | Pvalid_read (_label, tsets) -> begin
      (* TODO: see same constructor in reduce_by_predicate *)
        try
          let for_writing =
            (match p.pred_content with Pvalid_read _ -> false | _ -> true) in
          let state = env_current_state env in
          let typ_pointed = logic_typ_pointed tsets.term_type in
          (* Check if we are trying to write in a const l-value *)
          if for_writing && Value_util.is_const_write_invalid typ_pointed then
            raise Stop;
          let size = Eval_typ.sizeof_lval_typ typ_pointed in
          (* Check that the given location is valid *)
          let valid ~over:locbytes_over ~under:locbytes_under =
            let loc = loc_bytes_to_loc_bits locbytes_over in
            let loc = Locations.make_loc loc size in
            if not (Locations.is_valid ~for_writing loc) then (
              (* \valid does not hold if the over-approximation is invalid
                 everywhere, or if a part of the under-approximation is invalid
              *)
              let valid = valid_part ~for_writing loc in
              if Locations.is_bottom_loc valid then raise Stop;
              let loc_under = loc_bytes_to_loc_bits locbytes_under in
              let loc_under = Locations.make_loc loc_under size in
              let valid_loc_under =
                Locations.valid_part ~for_writing loc_under
              in
              if not (Location.equal loc_under valid_loc_under) then
                raise Stop;
              raise DoNotReduce (* In any case *))
          in
          (match tsets.term_node with
             | TLval _ ->
                 (* Evaluate the left-value, and check that it is initialized
                    and not an escaping pointer *)
                 let loc = eval_tlval_as_location ~alarm_mode env tsets in
                 if not (Locations.is_valid ~for_writing:false loc) then
                   c_alarm ();
                 let v = Model.find_indeterminate state loc in
                 let v, ok = match v with
                   | Cvalue.V_Or_Uninitialized.C_uninit_esc v
                   | Cvalue.V_Or_Uninitialized.C_uninit_noesc v
                   | Cvalue.V_Or_Uninitialized.C_init_esc v -> v, false
                   | Cvalue.V_Or_Uninitialized.C_init_noesc v -> v, true
                 in
                 if Cvalue.V.is_bottom v && not ok then raise Stop;
                 valid ~over:v ~under:V.bottom (*No precise under-approximation*);
                 if not ok then raise DoNotReduce
             | _ ->
               let v = eval_term ~alarm_mode env tsets in
               valid ~over:v.eover ~under:v.eunder
          );
          True
        with
          | DoNotReduce -> Unknown
          | LogicEvalError ee -> display_evaluation_error ~loc:p.pred_loc ee; Unknown
          | Stop -> False
      end

    | Pvalid_function tsets -> begin
        try
          let v = eval_term ~alarm_mode env tsets in
          let typ_pointer = Cil.typeOf_pointed v.etype in
          let funs, warn = Eval_typ.resolve_functions ~typ_pointer v.eover in
          if warn then
            match funs with
            | `Top -> Unknown
            | `Value funs ->
              (* No function possible -> signal hard error. Otherwise, follow
                 Value's convention, which is not to stop on semi-ok functions. *)
              if Kernel_function.Hptset.is_empty funs then False else Unknown
          else
            True
        with
        | LogicEvalError ee -> display_evaluation_error ~loc:p.pred_loc ee; Unknown
      end

    | Pinitialized (label,tsets) | Pdangling (label,tsets) -> begin
        try
          (* Create [*tsets] and compute its location. This is important in
             case [tsets] points to the address of a bitfield, which we
             cannot evaluate as a pointer (indexed on bytes) *)
          let star_tsets = deref_tsets tsets in
          let locb = eval_tlval ~alarm_mode env star_tsets in
          let state = env_state env label in
          match p.pred_content with
          | Pinitialized _ -> eval_initialized state locb
          | Pdangling _ -> eval_dangling state locb
          | _ -> assert false
        with
          | LogicEvalError ee -> display_evaluation_error ~loc:p.pred_loc ee; Unknown
      end
    | Prel (op,t1,t2) -> begin
        try
          let r = eval_binop ~alarm_mode env (lop_to_cop op) t1 t2 in
	  if V.equal V.singleton_zero r.eover
	  then False
	  else if V.equal V.singleton_one r.eover
	  then True
	  else Unknown
        with
          | LogicEvalError ee -> display_evaluation_error ~loc:p.pred_loc ee; Unknown
    end

    | Pforall (varl, p') | Pexists (varl, p') ->
      begin
        try
          let env = bind_logic_vars env varl in
          let r = do_eval env p' in
          match p.pred_content with
            | Pexists _ -> if r = False then False else Unknown
            | Pforall _ -> if r = True then True else Unknown
            | _ -> assert false
        with
          | LogicEvalError _ee -> (*display_evaluation_error ~loc ee;*) Unknown
      end

    | Pnot p ->  begin match do_eval env p with
      | True -> False
      | False -> True
      | Unknown -> Unknown
      end

    | Pimplies (p1,p2) ->
        do_eval env (Logic_const.por ~loc ((Logic_const.pnot ~loc p1), p2))

    | Pseparated ltsets ->
        (try
           let to_zones tset =
             (* Create [*tset] and compute its location. This is important in
                case [tset] points to the address of a bitfield, which we
                cannot evaluate as a pointer (indexed on bytes). *)
             let star_tset = deref_tsets tset in
             let rtset = eval_tlval ~alarm_mode env star_tset in
             let size = Eval_typ.sizeof_lval_typ rtset.etype in
             let loc_over = rtset.eover in
             let loc_under = rtset.eunder in
             Locations.enumerate_bits (Locations.make_loc loc_over size),
             Locations.enumerate_bits_under (Locations.make_loc loc_under size)
           in
           let lz = List.map to_zones ltsets in
           let unknown = ref false in
           (* Are those two lists of locations separated? *)
           let do_two (z1, zu1) l2 =
             let combine (z2, zu2) =
               if Zone.intersects z1 z2 then begin
                 unknown := true;
                 if Zone.intersects zu1 zu2 then raise Exit;
               end
             in
             List.iter combine l2
           in
           let rec aux = function
             | [] | [_] -> ()
             | loc :: qlocs ->
                 do_two loc qlocs;
                 aux qlocs
           in
           aux lz;
           if !unknown then Unknown else True
         with
           | Exit -> False
           | LogicEvalError ee -> display_evaluation_error ~loc:p.pred_loc ee; Unknown)

    | Papp (li, labels, args) -> eval_papp env li labels args
    | Pif (tcond, ptrue, pfalse) ->
      begin
        let r = eval_term ~alarm_mode env tcond in
        let ctrue =  Cvalue.V.contains_non_zero r.eover
        and cfalse =  Cvalue.V.contains_zero r.eover in
        match ctrue, cfalse with
        | true, true ->
          let reduce_by_rel = reduce_by_relation ~alarm_mode env true tcond in
          let env_true = reduce_by_rel Cil_types.Rneq (Cil.lzero ()) in
          let env_false = reduce_by_rel Cil_types.Req (Cil.lzero ()) in
          join_predicate_status (do_eval env_true ptrue) (do_eval env_false pfalse)
        | true, false -> do_eval env ptrue
        | false, true -> do_eval env pfalse
        | false, false -> assert false (* a logic alarm would have been raised*)
      end
    | Pfresh (_,_,_,_)
    | Pallocable _ | Pfreeable _
    | Plet (_,_)
    | Psubtype _
        -> Unknown

  (* Logic predicates. *)
  and eval_papp env li _labels args =
    match li.l_var_info.lv_name, args with
    | "\\warning", _ -> begin
      match args with
      | [{ term_node = TConst(LStr(str))}] ->
        Value_parameters.warning "reached \\warning(\"%s\")" str; Unknown
      | _ ->
        Value_parameters.abort
          "Wrong argument: \\warning expects a constant string"
    end
    | "\\is_finite", [arg] -> begin
      try
        let eval_result = eval_term ~alarm_mode env arg in
        (try
           let ival = V.project_ival eval_result.eover in
           ignore (Ival.project_float ival);
           True
         with
         | Ival.Nan_or_infinite | Cvalue.V.Not_based_on_null -> Unknown)
      with LogicEvalError ee -> display_evaluation_error ~loc ee; Unknown
    end
    | "\\subset", [argl;argr] -> begin
      try
	let l = eval_term ~alarm_mode env argl in
        let r = eval_term ~alarm_mode env argr in
        if V.is_included l.eover r.eunder then
          True (* all elements of [l] are included in the guaranteed elements
                  of [r] *)
        else if not (V.is_included l.eunder r.eover) ||
            not (V.intersects l.eover r.eover)
        then False (* one guaranteed element of [l] is not included in [r],
                      or [l] and [r] are disjoint, in which case there is
                      an element of [l] not in [r]. (Here, [l] is not bottom,
                      as [V.is_included bottom r.eunder] holds. *)
        else Unknown
      with
      | LogicEvalError ee -> display_evaluation_error ~loc ee; Unknown
    end
    | "valid_read_string", [arg] -> begin
        try
          let r = eval_term ~alarm_mode env arg in
          eval_valid_read_str ~wide:false env arg r.eover
        with LogicEvalError ee -> display_evaluation_error ~loc ee; Unknown
      end
    | "valid_string", [arg] -> begin
        try
          let r = eval_term ~alarm_mode env arg in
          eval_valid_str ~wide:false env arg r.eover
        with LogicEvalError ee -> display_evaluation_error ~loc ee; Unknown
      end
    | "valid_read_wstring", [arg] -> begin
        try
          let r = eval_term ~alarm_mode env arg in
          eval_valid_read_str ~wide:true env arg r.eover
        with LogicEvalError ee -> display_evaluation_error ~loc ee; Unknown
      end
    | "valid_wstring", [arg] -> begin
        try
          let r = eval_term ~alarm_mode env arg in
          eval_valid_str ~wide:true env arg r.eover
        with LogicEvalError ee -> display_evaluation_error ~loc ee; Unknown
      end
    | "is_allocable", [arg] when comes_from_fc_stdlib li.l_var_info -> begin
        try
          let r = eval_term ~alarm_mode env arg in
          eval_is_allocable r.eover
        with LogicEvalError ee -> display_evaluation_error ~loc ee; Unknown
      end
    | _, _ -> Unknown
  in
  try (* Each case of the matching above should handle evaluation errors.
         This is just an additional security. *)
    do_eval env pred
  with LogicEvalError ee -> display_evaluation_error ~loc ee; Unknown


(* -------------------------------------------------------------------------- *)
(* --- Dependencies of predicates                                         --- *)
(* -------------------------------------------------------------------------- *)

let predicate_deps env pred =
  let alarm_mode = Ignore in
  let rec do_eval env p =
    match p.pred_content with
    | Ptrue | Pfalse -> empty_logic_deps

    | Pand (p1, p2) | Por (p1, p2 ) | Pxor (p1, p2) | Piff (p1, p2 )
    | Pimplies (p1, p2) ->
        join_logic_deps (do_eval env p1) (do_eval env p2)

    | Prel (_, t1, t2) ->
        join_logic_deps (eval_term ~alarm_mode env t1).ldeps
                   (eval_term ~alarm_mode env t2).ldeps

    | Pif (c, p1, p2) ->
        join_logic_deps (eval_term ~alarm_mode env c).ldeps
          (join_logic_deps (do_eval env p1) (do_eval env p2))

    | Pat (p, lbl) ->
        do_eval { env with e_cur = lbl } p

    | Pvalid (_, tsets) | Pvalid_read (_, tsets) | Pvalid_function tsets->
        (eval_tlval ~alarm_mode env tsets).ldeps

    | Pinitialized (lbl, tsets) | Pdangling (lbl, tsets) ->
        let loc, deploc =
          eval_tlval_as_location_with_deps ~alarm_mode env tsets in
        let zone = enumerate_valid_bits ~for_writing:false loc in
        Logic_label.Map.add lbl zone deploc

    | Pnot p -> do_eval env p

    | Pseparated ltsets ->
        let evaled = List.map (eval_tlval ~alarm_mode env) ltsets in
        List.fold_left
          (fun acc e -> join_logic_deps acc e.ldeps)
          empty_logic_deps evaled

    | Pexists (l, p) | Pforall (l, p) ->
      let env = bind_logic_vars env l in
      (* TODO: unbind all references to l in the results? If so, clean up
         Logic_interp.do_term_lval. *)
      do_eval env p
    | Plet (_, p) -> do_eval env p

    | Pfresh (_,_,_,_)
    | Papp _
    | Pallocable _ | Pfreeable _
    | Psubtype _
        -> assert false
  in
  do_eval env pred


(* -------------------------------------------------------------------------- *)
(* --- Export                                                             --- *)
(* -------------------------------------------------------------------------- *)

(* Position default value for ~alarm_mode *)
let reduce_by_predicate env positive p =
  let alarm_mode = alarm_reduce_mode () in
  reduce_by_predicate ~alarm_mode env positive p

let () =
(* TODO: deprecate loc_to_loc, move loc_to_locs into Value *)
  Db.Properties.Interp.loc_to_loc :=
    (fun ~result state t ->
      let env = env_post_f ~pre:state ~post:state ~result () in
      try eval_tlval_as_location ~alarm_mode:Ignore env t
      with LogicEvalError _ -> raise Db.Properties.Interp.No_conversion
    );
(* TODO: specify better evaluation environment *)
  Db.Properties.Interp.loc_to_loc_under_over :=
    (fun ~result state t ->
      let env = env_post_f ~pre:state ~post:state ~result () in
      try
        let r= eval_tlval ~alarm_mode:Ignore env t in
	let s = Eval_typ.sizeof_lval_typ r.etype in
        make_loc r.eunder s, make_loc r.eover s, deps_at lbl_here r.ldeps
      with LogicEvalError _ -> raise Db.Properties.Interp.No_conversion
    );


(*
Local Variables:
compile-command: "make -C ../../../.."
End:
*)
