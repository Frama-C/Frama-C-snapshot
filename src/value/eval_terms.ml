(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2014                                               *)
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


(** Truth values for a predicate analyzed by the value analysis *)

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

let join_list_predicate_status l =
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
  | NoEnv (LogicLabel (_, s)) ->
      Format.fprintf fmt "no environment to evaluate \\at(%s,_)" s
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

let display_evaluation_error = function
  | CAlarm -> ()
  | pa ->
    Value_parameters.result ~once:true ~current:true
      "cannot evaluate ACSL term, %a" pretty_logic_evaluation_error pa


let warn_raise_mode =
  { CilE.imprecision_tracing = CilE.a_ignore ;
    defined_logic = CilE.a_ignore;

    unspecified = {CilE.a_ignore with CilE.a_call=c_alarm};
    others = {CilE.a_ignore with CilE.a_call=c_alarm};
  }


(** Evaluation environments. Used to evaluate predicate on \at nodes *)

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
   A na�ve implementation of assertions involving C labels is likely to miss
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

let lbl_here = LogicLabel (None, "Here")

let add_logic ll state states =
  Logic_label.Map.add (LogicLabel (None, ll)) state states
let add_here = add_logic "Here"
let add_pre = add_logic "Pre"
let add_post = add_logic "Post"
let add_old = add_logic "Old"

let env_pre_f ?(c_labels=Logic_label.Map.empty) ~init () = {
  e_cur = lbl_here;
  e_states = add_here init (add_pre init c_labels);
  result = None (* Never useful in a pre *);
}

let env_post_f ?(c_labels=Logic_label.Map.empty) ~pre ~post ~result () = {
  e_cur = lbl_here;
  e_states =
    add_post post (add_here post (add_pre pre (add_old pre c_labels))); 
  result = result;
}

let env_annot ?(c_labels=Logic_label.Map.empty) ~pre ~here () = {
  e_cur = lbl_here;
  e_states = add_here here (add_pre pre c_labels);
  result = None (* Never useful in a 'assert' *) (* TODO: will be needed for stmt contracts *);
}

let env_assigns ~init = {
  e_cur = lbl_here;
  (* YYY: is missing, but is too difficult in the current evaluation scheme *)
  e_states = add_old init (add_here init (add_pre init Logic_label.Map.empty));
  result = None (* Treated in a special way in callers *)
}

let env_here state = {
  e_cur = lbl_here;
  e_states = add_here state Logic_label.Map.empty;
  result = None (* Never useful in a 'assert' *) (* TODO: will be needed for stmt contracts *);
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
      Model.add_new_base b ~size V.top_int ~size_v:Int.one state
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

(* Best effort for compring the types currently understood by Value: ignore
   differences in integer and floating-point sizes, that are meaningless
   in the logic *)
let same_etype t1 t2 =
  match Cil.unrollType t1, Cil.unrollType t2 with
    | (TInt _ | TEnum _), (TInt _ | TEnum _) -> true
    | TFloat _, TFloat _ -> true
    | TPtr (p1, _), TPtr (p2, _) -> Cil_datatype.Typ.equal p1 p2
    | _, _ -> Cil_datatype.Typ.equal t1 t2

(* Rounding mode *)
let real_mode = Ival.Float_abstract.Any

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
   [eval_tlval_as_loc], and [Ival.t] for [eval_toffset].  [evalue] is
   a list because logic evaluate sets of terms (or tsets); so [evalue]
   represents the set of the results of evaluation of a tset. Note
   that this is an exact (not over-approximated) set of
   over-approximated values. *)
type 'a eval_result = {
  etype: Cil_types.typ;
  evalue: 'a list;
  ldeps: logic_deps;
}

let einteger v = { etype = Cil.intType; evalue = [v]; ldeps = empty_logic_deps}
let ereal v = { etype = Cil.doubleType; evalue = [v]; ldeps = empty_logic_deps}

let rec eval_term ~with_alarms env t =
  match t.term_node with
    | Tat (t, lab) ->
        eval_term ~with_alarms { env with e_cur = lab } t

    | TConst (Integer (v, _)) -> einteger (Cvalue.V.inject_int v)
    | TConst (LEnum e) ->
        (match (Cil.constFold true e.eival).enode with
           | Const (CInt64 (v, _, _)) -> einteger (Cvalue.V.inject_int v)
           | _ -> ast_error "non-evaluable constant")
    | TConst (LChr c) ->
        let i = match Cil.charConstToInt c with
          | CInt64 (i,_,_) -> i
          | _ -> assert false
        in
        einteger (Cvalue.V.inject_int i)
    | TConst (LReal { r_lower ; r_upper }) ->
        let f = Ival.inject_float_interval r_lower r_upper in
        ereal (Cvalue.V.inject_ival f)

    (*  | TConst ((CStr | CWstr) Missing cases *)

    | TAddrOf (thost, toffs) ->
        let r = eval_thost_toffset ~with_alarms env thost toffs in
        { etype = TPtr (r.etype, []);
          ldeps = r.ldeps;
          evalue = List.map loc_bits_to_loc_bytes r.evalue }

    | TStartOf (thost, toffs) ->
        let r = eval_thost_toffset ~with_alarms env thost toffs in
        { etype = TPtr (Cil.typeOf_array_elem r.etype, []);
          ldeps = r.ldeps;
          evalue = List.map loc_bits_to_loc_bytes r.evalue }

    | TLval _ ->
        let lvals = eval_tlval ~with_alarms env t in
        let typ = lvals.etype in
        let size = Bit_utils.sizeof typ in
        let eval_lval (l, deps) loc =
          let state = env_current_state env in
          let loc = make_loc loc size in
          let v =
            Cvalue.Model.find ~conflate_bottom:true ~with_alarms state loc
          in
          (* Skip dependencies if state is dead *)
          let deps =
            if Cvalue.Model.is_reachable state then
              add_deps env.e_cur deps
                (enumerate_valid_bits ~for_writing:false loc)
            else deps
          in
          Eval_op.reinterpret ~with_alarms typ v :: l, deps
        in
        let l, deps = List.fold_left eval_lval ([], lvals.ldeps) lvals.evalue in
        { etype = typ;
          ldeps = deps;
          evalue = l }

    (* TBinOp ((LOr | LAnd), _t1, _t2) -> TODO: a special case would be useful.
       But this requires reducing the state after having evaluated t1 by
       a term that is in fact a predicate *)       
    | TBinOp (op,t1,t2) -> eval_binop ~with_alarms env op t1 t2

    | TUnOp (op, t) ->
        let r = eval_term ~with_alarms env t in
        let typ' = match op with
          | Neg -> r.etype
          | BNot -> r.etype (* can only be used on an integer type *)
          | LNot -> Cil.intType
        in
        let eval v =
          Eval_op.eval_unop ~check_overflow:false ~with_alarms v r.etype op
        in
        { etype = typ';
          ldeps = r.ldeps;
          evalue = List.map eval r.evalue }

    | Trange (otlow, othigh) ->
        (* Eval one bound. `SureInf corresponds to an ACSL 'omitted bound',
           `MayInf to a value analysis approximation. There are subtle
           differences between, that are not completely exploited for now. *)
        let deps = ref empty_logic_deps in
        let eval = function
          | None -> `SureInf
          | Some t ->
            try
              let r = eval_term ~with_alarms env t in
              let v = match r.evalue with
                | [e] -> e
                | _ -> ast_error "found set in range bound"
              in
              if not (Cil.isIntegralType r.etype)
              then ast_error "non-integer range bound";
              deps := join_logic_deps !deps r.ldeps;
              try (match Ival.min_and_max (Cvalue.V.project_ival v) with
                | None, _ | _, None -> `MayInf
                | Some l, Some h -> `Finite (l, h)
              )
              with Cvalue.V.Not_based_on_null -> `MayInf
            with LogicEvalError e ->
              if e <> CAlarm then
                Value_parameters.result ~current:true ~once:true
                  "Cannot evaluate@ range bound %a@ (%a). Approximating"
                  Printer.pp_term t pretty_logic_evaluation_error e;
              `MayInf
        in
        let range low high =
          V.inject_ival (Ival.inject_range low high) in
        let r = match eval otlow, eval othigh with
          | `Finite (ilowlow, ilow), `Finite (ihigh, ihighhigh) ->
              if Int.gt ilowlow ihighhigh then []
              else
                if Int.equal ilowlow ihighhigh then
                  if Int.equal ilowlow ilow && Int.equal ihigh ihighhigh
                  then [V.inject_int ilow]
                  else (* complicated case. Due to the imprecisions, the range
                     might be empty, but the intersection is a single integer,
                     which is considered precise by all the other functions *)
                    c_alarm () (* TODO. (but what?) *)
                else
                  let middle = (* Compute elements that are guaranteed to
                                  be in the range, if possible one by one *)
                    if Int.ge ihigh ilow then
                      let plevel = Value_parameters.ArrayPrecisionLevel.get ()in
                      if Int.equal ilow ihigh then [V.inject_int ilow]
                      else
                        if Int.le (Int.sub ihigh ilow) (Int.of_int plevel) then
                          let rec enum i acc =
                            if Int.lt i ilow then acc
                            else enum (Int.sub i Int.one) (V.inject_int i ::acc)
                          in enum ihigh []
                        else [range (Some ilow) (Some ihigh)]
                    else []
                  in
                  if Int.equal ilowlow ilow && Int.equal ihigh ihighhigh
                  then middle
                  else range (Some ilowlow) (Some ihighhigh) :: middle
                    (* TODO: improve. Returning middle kills a lot of
                       possible reductions *)
          (* If an 'exact' flag is added to the evaluation of the logic, the
             code below must be rewritten as follows:
             `MayInf, `Finite (_h, hh) -> [(None, hh, inexact)]
             `SureInf, `Finite (h, hh) -> [(None, h, exact); (h, hh, inexact)]*)
          | (`MayInf | `SureInf), `Finite (_ihigh, ihighhigh) ->
               [range None (Some ihighhigh)]
          | `Finite (ilowlow, _ilow), (`MayInf | `SureInf) ->
                [range (Some ilowlow) None]
          | (`MayInf | `SureInf), (`MayInf | `SureInf) -> [range None None]
        in
        (*Value_parameters.debug "Range %a: %a@."
          d_term t (Pretty_utils.pp_list V.pretty) (List.map snd r);*)
        { etype = Cil.intType;
          ldeps = !deps;
          evalue = r }

    | TCastE (typ, t) ->
        let r = eval_term ~with_alarms env t in
        let conv v =
          let msg fmt =
            Format.fprintf fmt "%a (%a)" Printer.pp_term t V.pretty v
          in
          Eval_op.do_promotion ~with_alarms
            real_mode ~src_typ:r.etype ~dst_typ:typ v msg
        in
        { etype = typ;
          ldeps = r.ldeps;
          evalue = List.map conv r.evalue }

    | Tif (tcond, ttrue, tfalse) ->
        let r = eval_term ~with_alarms env tcond in
        let ctrue =  List.exists (Cvalue.V.contains_non_zero) r.evalue
        and cfalse = List.exists (Cvalue.V.contains_zero) r.evalue in
        (match ctrue, cfalse with
          | true, true ->
              let vtrue = eval_term ~with_alarms env ttrue in
              let vfalse = eval_term ~with_alarms env tfalse in
              if not (same_etype vtrue.etype vfalse.etype) then
                Value_parameters.failure ~current:true
                  "Incoherent types in conditional '%a': %a vs. %a. \
                  Please report"
                  Printer.pp_term t Printer.pp_typ vtrue.etype Printer.pp_typ vfalse.etype;
              let lr = vtrue.evalue @ vfalse.evalue in
              let r =
                if Logic_const.is_plain_type t.term_type
                then [List.fold_left V.join V.bottom lr]
                else lr
              in
              { etype = vtrue.etype;
                ldeps = join_logic_deps vtrue.ldeps vfalse.ldeps;
                evalue = r }
          | true, false -> eval_term ~with_alarms env ttrue
          | false, true -> eval_term ~with_alarms env tfalse
          | false, false ->
              assert false (* a logic alarm would have been raised*)
        )

    | TSizeOf _ | TSizeOfE _ | TSizeOfStr _ | TAlignOf _ | TAlignOfE _ ->
        let e = Cil.constFoldTerm true t in
        let v = match e.term_node with
          | TConst (Integer (v, _)) -> Cvalue.V.inject_int v
          | _ -> V.top_int
        in
        einteger v

    | Tunion l ->
        let tres = infer_type t.term_type in
        let l, deps = List.fold_left
          (fun (accv, accdeps) t ->
                  let r = eval_term ~with_alarms env t in
                  r.evalue @ accv, join_logic_deps accdeps r.ldeps)
             ([], empty_logic_deps) l
        in
        { etype = tres;
          ldeps = deps;
          evalue = l }

    | Tempty_set ->
        { etype = infer_type t.term_type; evalue = [];
          ldeps = empty_logic_deps }

    | Tnull ->
        { etype = Cil.voidPtrType;
          ldeps = empty_logic_deps;
          evalue = [Cvalue.V.singleton_zero] }

    | TLogic_coerce(typ, t) ->
        let r = eval_term ~with_alarms env t in
        (match typ with
           | Linteger ->
               assert (Logic_typing.is_integral_type t.term_type);
               r
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
                   evalue = List.map conv r.evalue }
               else r (* already a floating-point number (hopefully) *)
           | Ltype ({lt_name = "set"}, [typ])
               when Logic_utils.is_same_type typ t.term_type ->
             (* coercion from singleton to set. Since in the general case,
                the computed value is a set, there's nothing to do.
              *)
             r
           | _ -> unsupported
               (Pretty_utils.sfprintf "logic coercion %a -> %a@."
                  Printer.pp_logic_type t.term_type Printer.pp_logic_type typ)
        )

    (* TODO: the meaning of the label in \offset and \base_addr is not obvious
       at all *)
    | Toffset (_lbl, t) ->
        let r = eval_term ~with_alarms env t in
        let add_offset _ offs acc = Ival.join offs acc in
        let aux acc v = Location_Bytes.fold_topset_ok add_offset v acc in
        let offs = List.fold_left aux Ival.bottom r.evalue in
        { etype = Cil.intType;
          ldeps = r.ldeps;
          evalue = [Cvalue.V.inject_ival offs] }

    | Tbase_addr (_lbl, t) ->
        let r = eval_term ~with_alarms env t in
        let add_base b acc = V.join acc (V.inject b Ival.zero) in
        let aux acc v = Location_Bytes.fold_bases add_base v acc in
        { etype = Cil.charPtrType;
          ldeps = r.ldeps;
          evalue = [List.fold_left aux V.bottom r.evalue] }

    | Tblock_length (_lbl, t) -> (* TODO: take label into account for locals *)
        let r = eval_term ~with_alarms env t in
        let add_block_length b acc =
          let mo = Base.base_max_offset b in
          let bl =
            if Ival.is_bottom mo then
              Ival.top (* make logic total *)
            else (* Convert from bits to bytes *)
              let next_bit = Ival.add_int mo Ival.singleton_one in
              Ival.scale_div ~pos:true Int.eight next_bit
          in
          Ival.join acc bl
        in
        let aux acc v = Location_Bytes.fold_bases add_block_length v acc in
        let bl = List.fold_left aux Ival.bottom r.evalue in
        { etype = Cil.charPtrType;
          ldeps = r.ldeps;
          evalue = [V.inject_ival bl] }

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

and eval_binop ~with_alarms env op t1 t2 =
  if isLogicNonCompositeType t1.term_type then
    let r1 = eval_term ~with_alarms env t1 in
    let r2 = eval_term ~with_alarms env t2 in
    let te1 = Cil.unrollType r1.etype in
    (* We use the type of t1 to determine whether we are performing an int or
       float operation.*)
    let kop = match te1 with
      | TInt _ | TPtr _ | TEnum _ ->
          (* Do not pass ~typ here. We want the operations to be performed on
             unbounded integers mode *)
          Eval_op.eval_binop_int ~with_alarms ~te1 ?typ:None
      | TFloat _ -> Eval_op.eval_binop_float ~with_alarms real_mode None
      | _ -> ast_error (Pretty_utils.sfprintf
                          "binop on incorrect type %a" Printer.pp_typ te1)
    in
    let kop v1 v2 = kop v1 op v2 in
    let typ_res = infer_binop_res_type op te1 in
    let l1 = r1.evalue and l2 = r2.evalue in
    let r = match op, l1, l2 with
      | (PlusA | PlusPI | IndexPI | MinusA | MinusPI), _, _ ->
        List.fold_left (fun acc e1 ->
          List.fold_left (fun acc e2 -> kop e1 e2 :: acc) acc l2) [] l1

      (* Sets are compared by joining all their elements. This is correct,
         although imprecise *)
      | (Eq | Ne), _ , _ ->
        (match l1, l2 with
          | [], [] ->
              [if op = Eq then V.singleton_one else V.singleton_zero]
          | [], _ :: _ | _ :: _, [] ->
              [if op = Eq then V.singleton_zero else V.singleton_one]
          | h1 :: q1, h2 :: q2 ->
              let e1 = List.fold_left V.join h1 q1 in
              let e2 = List.fold_left V.join h2 q2 in
              let r = kop e1 e2 in
              let contains_zero = V.contains_zero r in
              let contains_non_zero = V.contains_non_zero r in
              [V.interp_boolean ~contains_zero ~contains_non_zero]
        )
      | _, [e1], [e2] -> [kop e1 e2]
      | _ -> ast_error "meaningless binop"
    in
    { etype = typ_res;
      ldeps = join_logic_deps r1.ldeps r2.ldeps;
      evalue = r }
  else
    unsupported (Pretty_utils.sfprintf
                   "%a operation on non-supported type %a" Printer.pp_binop op
                   Printer.pp_logic_type t1.term_type)

and eval_tlhost ~with_alarms env lv =
  match lv with
    | TVar { lv_origin = Some v } ->
        let loc = Location_Bits.inject (Base.of_varinfo v) Ival.zero in
        { etype = v.vtype;
          ldeps = empty_logic_deps;
          evalue = [loc] }
    | TResult typ ->
        (match env.result with
          | Some v ->
              let loc = Location_Bits.inject (Base.of_varinfo v) Ival.zero in
              { etype = typ;
                ldeps = empty_logic_deps;
                evalue = [loc] }
          | None -> no_result ())
    | TVar ({ lv_origin = None } as tlv) ->
        let b, ty = supported_logic_var tlv in
        let loc = Location_Bits.inject b Ival.zero in
        { etype = ty;
          ldeps = empty_logic_deps;
          evalue = [loc] }
    | TMem t ->
        let r = eval_term ~with_alarms env t in
        let tres = match Cil.unrollType r.etype with
          | TPtr (t, _) -> t
          | _ -> ast_error "*p where p is not a pointer"
        in
        { etype = tres;
          ldeps = r.ldeps;
          evalue = List.map loc_bytes_to_loc_bits r.evalue }

and eval_toffset ~with_alarms env typ toffset =
  match toffset with
  | TNoOffset ->
      { etype = typ;
        ldeps = empty_logic_deps;
        evalue = [Ival.singleton_zero] }
  | TIndex (idx, remaining) ->
      let typ_pointed = match Cil.unrollType typ with
        | TArray (t, _, _, _) -> t
        | TPtr(t,_) ->
            (match Cil.unrollType t with
              | TArray (t, _,_,_) -> t
              | _ -> ast_error "index on a non-array")
        | _ -> ast_error "index on a non-array"
      in
      let idxs = eval_term ~with_alarms env idx in
      let offsrem = eval_toffset ~with_alarms env typ_pointed remaining in
      let aux idx =
        let offset =
          try Cvalue.V.project_ival idx
          with Cvalue.V.Not_based_on_null -> Ival.top
        in
        let shift v =
          let offset = Ival.scale_int_base (sizeof typ_pointed) offset in
          Ival.add_int offset v
        in
        List.map shift offsrem.evalue
      in
      { etype = offsrem.etype;
        ldeps = join_logic_deps idxs.ldeps offsrem.ldeps;
        evalue = List.fold_left (fun r trm -> aux trm @ r) [] idxs.evalue; }

  | TField (fi, remaining) ->
      let current =
        try Ival.of_int (fst (Cil.bitsOffset typ (Field(fi, NoOffset))))
        with Cil.SizeOfError _ -> Ival.top
      in
      let offsrem = eval_toffset ~with_alarms env fi.ftype remaining in
      { etype = offsrem.etype;
        ldeps = offsrem.ldeps;
        evalue =  List.map (Ival.add_int current) offsrem.evalue }

  | TModel _ -> unsupported "model fields"

and eval_thost_toffset ~with_alarms env thost toffs =
  let rhost = eval_tlhost ~with_alarms env thost in
  let roffset = eval_toffset ~with_alarms env rhost.etype toffs in
  let shift l lochost =
    let shift offs = Location_Bits.shift offs lochost in
    List.map shift roffset.evalue @ l
  in
  { etype = roffset.etype;
    ldeps = join_logic_deps rhost.ldeps roffset.ldeps;
    evalue = List.fold_left shift [] rhost.evalue }

and eval_tlval ~with_alarms env t =
  match t.term_node with
  | TLval (thost, toffs) ->
      eval_thost_toffset ~with_alarms env thost toffs
  | Tunion l ->
      let aux (lr, deps) t =
        let r = eval_tlval ~with_alarms env t in
        r.evalue :: lr, join_logic_deps deps r.ldeps
      in
      let l, deps = List.fold_left aux ([], empty_logic_deps) l in
      { etype = infer_type t.term_type;
        ldeps = deps;
        evalue = List.concat l }
  | Tempty_set ->
      { etype = infer_type t.term_type; evalue = []; ldeps = empty_logic_deps }
  | Tat (t, lab) ->
      eval_tlval ~with_alarms { env with e_cur = lab } t
  | _ -> ast_error "non-lval term"

let eval_tlval_as_location ~with_alarms env t =
  let r = eval_tlval ~with_alarms env t in
  let s = Bit_utils.sizeof r.etype in
  let aux acc loc =
    assert (is_bottom_loc acc || Int_Base.equal s acc.size);
    make_loc (Location_Bits.join loc acc.loc) s
  in
  List.fold_left aux loc_bottom r.evalue

let eval_tlval_as_locations ~with_alarms env t =
  let r = eval_tlval ~with_alarms env t in
  let s = Bit_utils.sizeof r.etype in
  List.map (fun loc -> make_loc loc s) r.evalue, r.ldeps

let eval_tlval_as_zone ~with_alarms ~for_writing env t =
  let r = eval_tlval ~with_alarms env t in
  let s = Bit_utils.sizeof r.etype in
  let aux acc loc =
    let loc = make_loc loc s in
    let z = enumerate_valid_bits ~for_writing loc in
    Zone.join acc z
  in
  List.fold_left aux Zone.bottom r.evalue


(* If casting [trm] to [typ] has no effect in terms of the values contained
   in [trm], do nothing. Otherwise, raise [exn]. Adapted from [pass_cast] *)
let pass_logic_cast exn typ trm =
  (* TODOBY: add checks for volatile? *)
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

(* Must evaluate a term as a singleton location, or return [Not_an_exact_loc] *)
let rec eval_term_as_exact_loc ~with_alarms env t =
  match t with
    | { term_node = TLval _ } ->
        let locs = eval_tlval ~with_alarms env t in
        let typ = locs.etype in
        (match locs.evalue with
           | [] | _ :: _ :: _ -> raise Not_an_exact_loc
           | [loc] ->
               let loc = Locations.make_loc loc (Bit_utils.sizeof typ) in
               if not (cardinal_zero_or_one loc)
               then raise Not_an_exact_loc;
               typ, loc
        )
    | { term_node = TLogic_coerce(_, t)} ->
        (* It is always ok to pass through a TLogic_coerce, as the destination
           type is always a supertype *)
        eval_term_as_exact_loc ~with_alarms env t

    | { term_node = TCastE (ctype, t') } ->
        pass_logic_cast Not_an_exact_loc (Ctype ctype) t';
        eval_term_as_exact_loc ~with_alarms env t'

    | _ -> raise Not_an_exact_loc


exception DoNotReduce

let is_same_term_coerce t1 t2 =
  match t1.term_node, t2.term_node with
    | TLogic_coerce _, TLogic_coerce _ -> Logic_utils.is_same_term t1 t2
    | TLogic_coerce (_,t1), _ -> Logic_utils.is_same_term t1 t2
    | _, TLogic_coerce(_,t2) -> Logic_utils.is_same_term t1 t2
    | _ -> Logic_utils.is_same_term t1 t2

let rec reduce_by_predicate env positive p =
  reduce_by_predicate_content env positive p.content

and reduce_by_predicate_content env positive p_content =
    let with_alarms = warn_raise_mode in
    match positive,p_content with
    | true,Ptrue | false,Pfalse -> env

    | true,Pfalse | false,Ptrue ->
        overwrite_current_state env Cvalue.Model.bottom

    (* desugared form of a <= b <= c <= d *)
    | true, Pand (
        {content=Prel ((Rlt | Rgt | Rle | Rge | Req as op),_ta,tb) as p1},
        {content=Pand (
          {content=Prel (op', tb',tc) as p2},
          {content=Prel (op'',tc',_td) as p3})})
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
        let r1 = reduce_by_predicate env positive p1 in
        reduce_by_predicate r1 positive p2

    | true,Por (p1,p2 ) | false,Pand (p1, p2) ->
        join_env
          (reduce_by_predicate env positive p1)
          (reduce_by_predicate env positive p2)

    | true,Pimplies (p1,p2) ->
        join_env
          (reduce_by_predicate env false p1)
          (reduce_by_predicate env true p2)

    | false,Pimplies (p1,p2) ->
        reduce_by_predicate
          (reduce_by_predicate env true p1)
          false
          p2

    | _,Pnot p -> reduce_by_predicate env (not positive) p

    | true,Piff (p1, p2) ->
        let red1 =
          reduce_by_predicate_content env true (Pand (p1, p2)) in
        let red2 =
          reduce_by_predicate_content env false (Por (p1, p2)) in
        join_env red1 red2

    | false,Piff (p1, p2) ->
        reduce_by_predicate env true
          (Logic_const.por
             (Logic_const.pand (p1, Logic_const.pnot p2),
              Logic_const.pand (Logic_const.pnot p1, p2)))

    | _,Pxor(p1,p2) ->
        reduce_by_predicate env
          (not positive) (Logic_const.piff(p1, p2))

    | _,Prel (op,t1,t2) ->
      begin
        try
          let eval = match t1.term_type with
            | t when Cil.isLogicRealOrFloatType t ->
                Eval_op.reduce_rel_float
                  (Value_parameters.AllRoundingModes.get ())
            | t when Cil.isLogicIntegralType t -> Eval_op.reduce_rel_int
            | Ctype ct when Cil.isPointerType ct -> Eval_op.reduce_rel_int
            | _ -> raise DoNotReduce
          in
          reduce_by_relation eval env positive t1 op t2
        with
          | DoNotReduce -> env
          | LogicEvalError ee -> display_evaluation_error ee; env
          | Eval_exprs.Reduce_to_bottom ->
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

    | _,Pinitialized (lbl_initialized,tsets) ->
        begin try
          let rlocb = eval_term ~with_alarms env tsets in
          let size = Bit_utils.sizeof_pointed rlocb.etype in
          let size =
            try Int_Base.project size
            with _ -> c_alarm () (* Not really an alarm, an imprecision *)
          in
          let state = env_state env lbl_initialized in
          let state_reduced =
            List.fold_left
              (fun state loc ->
                let loc_bits = loc_bytes_to_loc_bits loc in
                Model.reduce_by_initialized_defined_loc
                   (Cvalue.V_Or_Uninitialized.change_initialized positive)
                   loc_bits size state
              ) state rlocb.evalue
          in
          overwrite_state env state_reduced lbl_initialized
          with
            | LogicEvalError ee -> display_evaluation_error ee; env
        end

    | _,Pat (p, lbl) ->
        (try
           let env_at = { env with e_cur = lbl } in
           let env' = reduce_by_predicate env_at positive p in
           { env' with e_cur = env.e_cur }
         with LogicEvalError ee -> display_evaluation_error ee; env)

    | _,Papp ({ l_var_info = { lv_name = "\\is_finite" }}, _, args) ->
      if not positive then env
      else
	let arg = (match args with [x] -> x | _ -> assert false) in
	(try
	   let state = env_current_state env in
	   let typ_loc, loc = eval_term_as_exact_loc ~with_alarms env arg in
	   let fkind = match (Cil.unrollType typ_loc) with
	     | TFloat( fkind, _) -> fkind
	     | _ -> assert false in
	   let v = Cvalue.Model.find ~conflate_bottom:true ~with_alarms state loc in
	   let v = Eval_op.reinterpret_float ~with_alarms:CilE.warn_none_mode fkind v in
	   let state' = Cvalue.Model.reduce_previous_binding state loc v in
	   let env = overwrite_current_state env state' in
	   env
	 with
	   | LogicEvalError ee -> display_evaluation_error ee; env
	   | Not_an_exact_loc -> env)
    | true, Pforall (varl, p) | false, Pexists (varl, p) ->
      begin
        try
          (* TODO: add case analysis on the variables of the quantification
             that are constrained *)
          let env = bind_logic_vars env varl in
          let env_result = reduce_by_predicate env true p in
          unbind_logic_vars env_result varl
        with LogicEvalError _ -> env
      end

    | _,Papp _
    | true, Pexists (_, _) | false, Pforall (_, _)
    | _,Plet (_, _) | _,Pif (_, _, _)
    | _,Pallocable (_,_) | _,Pfreeable (_,_) | _,Pfresh (_,_,_,_)  
    | _,Psubtype _
    | _, Pseparated _
        -> env

and reduce_by_valid env positive ~for_writing (tset: term) =
  let with_alarms = warn_raise_mode in
  (* Auxiliary function that reduces \valid( *lvloc+offs), where lvloc is atomic
     (no more tsets), and offs is a bits-expressed constant offset.
     [offs_typ] is supposed to be the type of the pointed location after [offs]
     has been applied; it can be different from [typeOf_pointed lv_typ], for
     example if offset is a field access. *)
  let aux (lv_typ, lvloc) env (offs_typ, offs) =
    try
      if not (Location_Bits.is_relationable lvloc) ||
         not (Ival.cardinal_zero_or_one offs)
      then raise DoNotReduce;
      let state = env_current_state env in
      let lvloc = make_loc lvloc (Bit_utils.sizeof lv_typ) in
      (* [p] is the range that we attempt to reduce *)
      let p_orig = Model.find ~with_alarms ~conflate_bottom:true state lvloc in
      let pb = Locations.loc_bytes_to_loc_bits p_orig in
      let shifted_p = Location_Bits.shift offs pb in
      let lshifted_p = make_loc shifted_p (Bit_utils.sizeof offs_typ) in
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
	let shift = Ival.neg offs in
	let pb = Location_Bits.shift shift valid in
	let p = Locations.loc_bits_to_loc_bytes pb in
	(* Store the result *)
	let state = Model.reduce_previous_binding state lvloc p in
	overwrite_current_state env state
    with
      | DoNotReduce | V.Not_based_on_null | Cil.SizeOfError _ -> env
      | LogicEvalError ee -> display_evaluation_error ee; env
  in
  let rec do_one env t =
    match t.term_node with
      | Tunion l ->
          List.fold_left do_one env l

      | TLval _ -> 
          let aux typ env lval =
            try
              let loc = make_loc lval (Bit_utils.sizeof typ) in
              if cardinal_zero_or_one loc then
                let state =
                  Eval_exprs.reduce_by_valid_loc ~positive ~for_writing
                    loc typ (env_current_state env)
                in
                overwrite_current_state env state
              else env
            with LogicEvalError ee -> display_evaluation_error ee; env
          in
          (try 
             let r = eval_tlval ~with_alarms env t in
             List.fold_left (aux r.etype) env r.evalue
           with LogicEvalError ee -> display_evaluation_error ee; env)

      | TAddrOf (TMem ({term_node = TLval _} as t), offs) ->
          (try
             let lt = eval_tlval ~with_alarms env t in
             let typ = lt.etype in
             List.fold_left
               (fun env lv ->
                  (* Compute the offsets, that depend on the type of the lval.
                     The computed list is exactly what [aux] requires *)
                  let roffs =
                    eval_toffset ~with_alarms env (Cil.typeOf_pointed typ) offs
                  in
                  List.fold_left
                    (fun env offs -> aux (typ, lv) env (roffs.etype, offs))
                    env roffs.evalue
               ) env lt.evalue
           with LogicEvalError ee -> display_evaluation_error ee; env)

      | TBinOp ((PlusPI | MinusPI) as op, ({term_node = TLval _} as tlv), i) ->
          (try
             let rtlv = eval_tlval ~with_alarms env tlv in
             let ri = eval_term ~with_alarms env i in
             (* Convert offsets to a simpler form if [op] is [MinusPI] *)
             let li =
               List.fold_left
                 (fun acc offs ->
                    try
                      let i = V.project_ival offs in
                      let i = if op = PlusPI then i else Ival.neg i in
                      (ri.etype, i) :: acc
                    with V.Not_based_on_null -> acc
                 ) [] ri.evalue
             in
             let typ_p = Cil.typeOf_pointed rtlv.etype in
             let sbits = Int.of_int (Cil.bitsSizeOf typ_p) in
             List.fold_left
               (fun env elv ->
                  (* Compute the offsets expected by [aux], which are
                     [i * 8 * sizeof( *tlv)] *)
                  let li = List.map
                    (fun (_, offs) -> typ_p, Ival.scale sbits offs) li
                  in
                  List.fold_left (aux (typ_p, elv)) env li
               ) env rtlv.evalue
           with LogicEvalError ee -> display_evaluation_error ee; env)
      | _ -> env
  in
  do_one env tset

and reduce_by_relation eval env positive t1 rel t2 =
  let env = reduce_by_left_relation eval env positive t1 rel t2 in
  let inv_binop = match rel with
    | Rgt -> Rlt | Rlt -> Rgt | Rle -> Rge | Rge -> Rle
    | Req -> Req | Rneq -> Rneq
  in
  reduce_by_left_relation eval env positive t2 inv_binop t1

and reduce_by_left_relation eval env positive tl rel tr =
  let with_alarms = warn_raise_mode in
  try
    let debug = false in
    let state = env_current_state env in
    if debug then Format.printf "#Left term %a@." Printer.pp_term tl;
    let typ_loc, loc = eval_term_as_exact_loc ~with_alarms env tl in
    if debug then Format.printf "#Left term as lv loc %a, typ %a@."
      Locations.pretty loc Printer.pp_typ typ_loc;
    let v = Cvalue.Model.find ~conflate_bottom:true ~with_alarms state loc in
    if debug then Format.printf "#Val left lval %a@." V.pretty v;
    let v = Eval_op.reinterpret ~with_alarms typ_loc v in
    if debug then Format.printf "#Cast left lval %a@." V.pretty v;
    let rtl = eval_term ~with_alarms env tr in
    let cond_v =
      List.fold_left Location_Bytes.join Location_Bytes.bottom rtl.evalue
    in
    if debug then Format.printf "#Val right term %a@." V.pretty cond_v;
    let op = lop_to_cop rel in
    let v_sym =
      eval.Eval_op.reduce_rel_symmetric positive op cond_v v in
    let v_asym =
      eval.Eval_op.reduce_rel_antisymmetric ~typ_loc positive op cond_v v_sym in
    if debug then Format.printf "#Val reduced %a@." V.pretty v_asym;
    (* TODOBY: if loc is an int that has been silently cast to real, we end up
       reducing an int according to a float. Instead, we should convert v to
       real, then cast back v_asym to the good range *)
    if V.is_bottom v_asym then raise Eval_exprs.Reduce_to_bottom;
    if V.equal v_asym v then
      env
    else 
      let state' = 
	Cvalue.Model.reduce_previous_binding state loc v_asym
      in
      overwrite_current_state env state'
  with
    | Not_an_exact_loc -> env
    | LogicEvalError ee -> display_evaluation_error ee; env


let eval_predicate env pred =
  let with_alarms = warn_raise_mode in
  let rec do_eval env p =
    match p.content with
    | Ptrue -> True
    | Pfalse -> False
    | Pand (p1,p2 ) ->
        begin match do_eval env p1 with
        | True -> do_eval env p2
        | False -> False
        | Unknown ->
          let reduced = reduce_by_predicate env true p1 in
          match do_eval reduced p2 with
            | False -> False
            | _ -> Unknown
        end
    | Por (p1,p2 ) ->
        let val_p1 = do_eval env p1 in
        (*Format.printf "Disjunction: state %a p1:%a@."
            Cvalue.Model.pretty (env_current_state env)
            Printer.pp_predicate_named p1; *)
        begin match val_p1 with
        | True -> True
        | False -> do_eval env p2
        | Unknown -> begin
          let reduced_state = reduce_by_predicate env false p1 in
          (* Format.printf "Disjunction: reduced to %a to eval %a@."
             Cvalue.Model.pretty (env_current_state reduced_state)
             Printer.pp_predicate_named p2; *)
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
        try do_eval { env with e_cur = lbl } p
        with LogicEvalError ee -> display_evaluation_error ee; Unknown
      end

    | Pvalid (_label, tsets) | Pvalid_read (_label, tsets) -> begin
      (* TODO: see same constructor in reduce_by_predicate *)
        try
          let for_writing =
            (match p.content with Pvalid_read _ -> false | _ -> true) in
          let state = env_current_state env in
          let size = match Logic_utils.unroll_type tsets.term_type with
            | Ctype (TPtr _ |  TArray _ as t)
            | Ltype ({lt_name = "set"},[Ctype t]) -> sizeof_pointed t
            | _ -> ast_error "valid on incorrect location %a"
          in
          (* Check that the given location is valid *)
          let valid locbytes =
            let loc = loc_bytes_to_loc_bits locbytes in
            let loc = Locations.make_loc loc size in
            if not (Locations.is_valid ~for_writing loc) then (
              (* Maybe the location is guaranteed to be invalid? *)
              let valid = valid_part ~for_writing loc in
              if Locations.is_bottom_loc valid
              then raise Stop
              else raise DoNotReduce)
          in
          (match tsets.term_node with
             | TLval _ ->
                 (* Evaluate the left-value, and check that it is initialized
                    and not an escaping pointer *)
                 List.iter
                   (fun loc ->
                      let v = Model.find_unspecified ~with_alarms
                        ~conflate_bottom:true state loc
                      in
                      let v, ok = match v with
                        | Cvalue.V_Or_Uninitialized.C_uninit_esc v
                        | Cvalue.V_Or_Uninitialized.C_uninit_noesc v
                        | Cvalue.V_Or_Uninitialized.C_init_esc v -> v, false
                        | Cvalue.V_Or_Uninitialized.C_init_noesc v -> v, true
                      in
                      if Cvalue.V.is_bottom v && not ok then raise Stop;
                      valid v;
                      if not ok then raise DoNotReduce
                   )
                   (fst (eval_tlval_as_locations ~with_alarms env tsets))
             | _ ->
               List.iter valid (eval_term ~with_alarms env tsets).evalue
          );
          True
        with
          | DoNotReduce -> Unknown
          | LogicEvalError ee -> display_evaluation_error ee; Unknown
          | Stop -> False
      end

    | Pinitialized (label,tsets) -> begin
        try
          let locb = eval_term ~with_alarms env tsets in
          let state = env_state env label in
          let typ = locb.etype in
          if not (Cil.isPointerType typ) then
            ast_error "initialized on incorrect location";
          let statuses =  List.map
            (fun loc ->
               let locbi = loc_bytes_to_loc_bits loc in
               let loc = make_loc locbi (sizeof_pointed typ) in
               let value = Model.find_unspecified ~with_alarms
                 ~conflate_bottom:true state loc
               in
               match value with
                 | V_Or_Uninitialized.C_uninit_esc v
                 | V_Or_Uninitialized.C_uninit_noesc v ->
                     if Location_Bytes.is_bottom v then False else Unknown
                 | V_Or_Uninitialized.C_init_esc _
                 | V_Or_Uninitialized.C_init_noesc _ -> True
            ) locb.evalue
          in
          join_list_predicate_status statuses
        with
          | Eval_exprs.Cannot_find_lv -> Unknown
          | LogicEvalError ee -> display_evaluation_error ee; Unknown
      end
    | Prel (op,t1,t2) -> begin
        try
          let r = eval_binop ~with_alarms env (lop_to_cop op) t1 t2 in
(*          if lop_to_cop op = Eq then
            Format.printf "## Logic deps for %a: @[%a@]@."
              Printer.pp_predicate_named p Zone.pretty r.ldeps; *)
          if List.for_all (V.equal V.singleton_zero) r.evalue
          then False
          else if List.for_all (V.equal V.singleton_one) r.evalue
          then True
          else Unknown
        with
          | LogicEvalError ee -> display_evaluation_error ee; Unknown
    end

    | Pforall (varl, p') | Pexists (varl, p') ->
      begin
        try
          let env = bind_logic_vars env varl in
          let r = do_eval env p' in
          match p.content with
            | Pexists _ -> if r = False then False else Unknown
            | Pforall _ -> if r = True then True else Unknown
            | _ -> assert false
        with
          | LogicEvalError _ee -> (*display_evaluation_error ee;*) Unknown
      end

    | Pnot p ->  begin match do_eval env p with
      | True -> False
      | False -> True
      | Unknown -> Unknown
      end

    | Pimplies (p1,p2) ->
        do_eval env (Logic_const.por ((Logic_const.pnot p1), p2))

    | Pseparated ltsets ->
        (try
           let to_locs tset =
             let rtset = eval_term ~with_alarms env tset in
             let typ = rtset.etype in
             if not (Cil.isPointerType typ)
             then ast_error "separated on non-pointers";
             let size = sizeof_pointed typ in
             List.map
               (fun loc ->
                 let loc = loc_bytes_to_loc_bits loc in
                 Locations.make_loc loc size
               ) rtset.evalue
           in
           let locs = List.map to_locs ltsets in
           let to_zone = Locations.enumerate_bits in
           let lz = List.map (List.map (fun l -> l, to_zone l)) locs in
           let unknown = ref false in
           (* Are those two lists of locations separated? *)
           let do_two l1 l2 =
             let combine (loc1, z1) (loc2, z2) =
               if Zone.intersects z1 z2 then
                 if Locations.cardinal_zero_or_one loc1 &&
                   Locations.cardinal_zero_or_one loc2
                 then raise Exit
                 else unknown := true
             in
             List.iter (fun e1 -> List.iter (combine e1) l2) l1
           in
           let rec aux = function
             | [] | [_] -> ()
             | locs :: qlocs ->
                 List.iter (do_two locs) qlocs;
                 aux qlocs
           in
           aux lz;
           if !unknown then Unknown else True
         with
           | Exit -> False
           | LogicEvalError ee -> display_evaluation_error ee; Unknown)

    (* Builtin predicates. *)
    | Papp ({ l_var_info = { lv_name = "\\warning" }}, _, args) ->
      (match args with
	| [{ term_node = TConst(LStr(str))}] ->
	  Value_parameters.warning "reached \\warning(\"%s\")" str; True
	| _ -> Value_parameters.abort "Wrong argument: \\warning expects a constant string")

    | Papp ({ l_var_info = { lv_name = "\\is_finite" }}, _, args) ->
      let arg = (match args with [x] -> x | _ -> assert false (* caught by typechecking. *)) in
      (try
    	 let eval_result = eval_term ~with_alarms env arg in
    	 let statuses = List.map
           (fun cvalue ->
    	     try
    	       let ival = V.project_ival cvalue in
    	       try
    		 let _ = Ival.project_float ival in True
    	       with Ival.Float_abstract.Nan_or_infinite -> Unknown
    	     with Cvalue.V.Not_based_on_null -> Unknown
           ) eval_result.evalue
         in
         join_list_predicate_status statuses
       with LogicEvalError ee -> display_evaluation_error ee; Unknown)

    | Papp _
    | Pfresh (_,_,_,_)
    | Pallocable _ | Pfreeable _
    | Plet (_,_) | Pif (_, _, _)
    | Psubtype _
        -> Unknown
  in
  do_eval env pred

let predicate_deps env pred =
  let with_alarms = CilE.warn_none_mode in
  let rec do_eval env p =
    match p.content with
    | Ptrue | Pfalse -> empty_logic_deps

    | Pand (p1, p2) | Por (p1, p2 ) | Pxor (p1, p2) | Piff (p1, p2 )
    | Pimplies (p1, p2) ->
        join_logic_deps (do_eval env p1) (do_eval env p2)

    | Prel (_, t1, t2) ->
        join_logic_deps (eval_term ~with_alarms env t1).ldeps
                   (eval_term ~with_alarms env t2).ldeps

    | Pif (c, p1, p2) ->
        join_logic_deps (eval_term ~with_alarms env c).ldeps
          (join_logic_deps (do_eval env p1) (do_eval env p2))

    | Pat (p, lbl) ->
        do_eval { env with e_cur = lbl } p

    | Pvalid (_, tsets)
    | Pvalid_read (_, tsets) ->
        (eval_tlval ~with_alarms env tsets).ldeps

    | Pinitialized (lbl, tsets) ->
        let loc, deploc = eval_tlval_as_locations ~with_alarms env tsets in
        let zones =
          List.fold_left
            (fun z loc ->
               Zone.join (enumerate_valid_bits ~for_writing:false loc) z)
            Zone.bottom loc
        in
        Logic_label.Map.add lbl zones deploc

    | Pnot p -> do_eval env p

    | Pseparated ltsets ->
        let evaled = List.map (eval_tlval ~with_alarms env) ltsets in
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

exception Does_not_improve

let rec fold_on_disjunction f p acc =
  match p.content with
  | Por (p1,p2 ) -> fold_on_disjunction f p2 (fold_on_disjunction f p1 acc)
  | _ -> f p acc

let count_disjunction p = fold_on_disjunction (fun _pred -> succ) p 0

let split_disjunction_and_reduce ~reduce ~env state_trace ~slevel p ip =
  let (state,trace) = state_trace in
  if not (Model.is_reachable state) then State_set.empty
  else
    let nb = count_disjunction p in
    if nb <= 1 && not reduce then
      State_set.singleton state_trace (* reduction not required, nothing to split *)
    else if nb <= slevel
    then begin (* Can split and maybe reduce *)
      let treat_subpred pred acc =
        let r = reduce_by_predicate env true pred in
        if Cvalue.Model.equal (env_current_state r) state then
          (* This part of the disjunction will contain the entire state.
             Reduction has failed, there is no point in propagating the
             smaller states in acc, that are contained in this one. *)
          raise Does_not_improve
        else
	  let trace =
	    if nb <= 1 then trace else Trace.add_disjunction ip pred trace
	  in
	  State_set.add (env_current_state r, trace) acc
      in
      try fold_on_disjunction treat_subpred p State_set.empty
      with Does_not_improve -> State_set.singleton state_trace
    end
    else if reduce then
      (* Not enough slevel to split, but we should reduce in a global way *)
      let reduced = reduce_by_predicate env true p in
      State_set.singleton (env_current_state reduced, trace)
    else (* Not enough slevel to split, and reduction not required *)
      State_set.singleton state_trace
;;

let () =
(* TODO: deprecate loc_to_loc, move loc_to_locs into Value *)
  Db.Properties.Interp.loc_to_loc :=
    (fun ~result state t ->
      let env = env_post_f ~pre:state ~post:state ~result () in
      try eval_tlval_as_location ~with_alarms:CilE.warn_none_mode env t
      with LogicEvalError _ -> raise (Invalid_argument "not an lvalue")
    );
(* TODO: specify better evaluation environment *)
  Db.Properties.Interp.loc_to_locs :=
    (fun ~result state t ->
      let env = env_post_f ~pre:state ~post:state ~result () in
      let with_alarms = CilE.warn_none_mode in
      try
        let r, deps = eval_tlval_as_locations ~with_alarms env t in
        r, deps_at lbl_here deps
      with LogicEvalError _ -> raise (Invalid_argument "not an lvalue")
    );


(*
Local Variables:
compile-command: "make -C ../.."
End:
*)
