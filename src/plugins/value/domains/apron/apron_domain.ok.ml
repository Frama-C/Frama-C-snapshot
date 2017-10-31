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

#24 "src/plugins/value/domains/apron/apron_domain.ok.ml"

open Cil_types
open Eval
open Apron

let dkey = Value_parameters.register_category "d-apron"

let ok = true

let debug = false

module type S = Abstract_domain.Internal
  with type value = Main_values.Interval.t
   and type location = Precise_locs.precise_location

let abort exclog =
  let open Manager in
  Value_parameters.fatal
    "Apron manager error : %a in function %a.@.%s"
    print_exc exclog.exn print_funid exclog.funid exclog.msg

let is_relevant_varinfo varinfo =
  not (Cil.typeHasQualifier "volatile" varinfo.vtype)
  && (true || not varinfo.vglob)

let is_relevant_lval = function
  | Var varinfo, NoOffset -> is_relevant_varinfo varinfo
  | _ -> false

let rec contains_relevant_lval expr = match expr.enode with
  | Lval lval -> is_relevant_lval lval
  | UnOp (_, e, _) | CastE (_, e) -> contains_relevant_lval e
  | BinOp (_, e1, e2, _) ->
    contains_relevant_lval e1 || contains_relevant_lval e2
  | _ -> false

let is_relevant expr = match expr.enode with
  | Lval _ -> true
  | UnOp (_, e, _) | CastE (_, e) -> contains_relevant_lval e
  | BinOp (_, e1, e2, _) ->
    contains_relevant_lval e1 && contains_relevant_lval e2
  | _ -> false


(** All conversion functions may fail with this exception. *)
exception Out_of_Scope of string

(* -------------------------------------------------------------------------- *)
(*                         Conversion of integers                             *)
(* -------------------------------------------------------------------------- *)

(* Apron Scalar to Apron integer. *)
let scalar_to_mpzf = function
  | Scalar.Mpqf rational ->
    let num, den = Mpqf.to_mpzf2 rational in
    if Mpzf.cmp_int den 1 = 0 then num
    else raise (Out_of_Scope "scalar_to_mpzf rational")
  | Scalar.Float _ | Scalar.Mpfrf _ ->
    raise (Out_of_Scope "scalar_to_mpzf non integer")

let scalar_to_int s =
  if Scalar.is_infty s <> 0
  then None
  else match s with
    | Scalar.Mpqf q ->
      (* TODO: extract to Integer directly, without intermediate smaller type *)
      let i = int_of_float (Mpqf.to_float q) in
      if Scalar.equal_int s i then Some i
      else raise (Out_of_Scope "scalar_to_int rational")
    | Scalar.Float _ | Scalar.Mpfrf _->
      raise (Out_of_Scope "scalar_to_int non_integer")


(* -------------------------------------------------------------------------- *)
(*                Translation of the AST from Cil to Apron                    *)
(* -------------------------------------------------------------------------- *)

(* Rounding mode used in the file. [Rnd] corresponds to rounding in all
   directions. Only useful for floating-point values. *)
let round = Texpr1.Rnd

type integer_range = Eval_typ.integer_range = { i_bits: int; i_signed: bool }


let bounds_of_typ range =
  let bitsize = range.i_bits in
  let size = Mpz.init () in
  Mpz.ui_pow_ui size 2 bitsize;
  let size = Mpzf.of_mpz size in
  if range.i_signed
  then
    let half = Mpz.init () in
    Mpz.ui_pow_ui half 2 (bitsize - 1);
    let half = Mpzf.of_mpz half in
    Mpzf.neg half, Mpzf.sub_int half 1, size
  else
    Mpzf.of_int 0, Mpzf.sub_int size 1, size

(* Mapping from the names of the Apron variables to their range *)
module VarRanges =
  State_builder.Hashtbl(Datatype.String.Hashtbl)(Eval_typ.DatatypeIntegerRange)
    (struct
      let name = "Value.Apron_domain.VarRanges"
      let dependencies = [Ast.self] (* through the varinfos names and ids *)
      let size = 32
    end)

(* is [expr] guaranteed to (statically) fit within [range] *)
let expr_fits_in_range (expr: Apron.Texpr1.expr) range =
  let open Apron.Texpr1 in
  match expr with
  | Var v ->
    let name = Apron.Var.to_string v in
    let range_v = VarRanges.find name in
    let ok_l, ok_r = Eval_typ.range_inclusion range_v range in
    ok_l && ok_r
  | Cst _ | Unop (_,_,_,_) | Binop (_,_,_,_,_) ->
    false (* TODO? Unclear whether those cases would add expressivity. *)

(* Auxiliary function for {!coerce} below. It normalizes [expr] in an expression
   that is guaranteed to fit within the integer type [range], or returns
   an interval covering the entire range.
   Algorithm from Verasco.
   See section 6.5 of the paper 'A Formally-Verified C Static Analyzer'. *)
let reduce eval expr range =
  if expr_fits_in_range expr range then expr
  else
    let interval = eval expr in
    if Interval.is_bottom interval then expr
    else
      let inf, sup, size = bounds_of_typ range in
      let top () =
        let coeff = Coeff.i_of_mpqf (Mpqf.of_mpz inf) (Mpqf.of_mpz sup) in
        Texpr1.Cst coeff
      in
      (* top intervals have bounds [-1/0; 1/0], standing for [-infty, +infty].
         Since the denominator is not 1, the translation will fail later in
         [scalar_to_mpzf]. Thus we should catch this case here. *)
      if Interval.is_top interval then begin
        if debug then Value_parameters.result ~current:true ~once:true
            "imprecise expr %a" Apron.Texpr1.print_expr expr;
        top ()
      end
      else
        try
          let min = scalar_to_mpzf interval.Interval.inf
          and max = scalar_to_mpzf interval.Interval.sup in
          if Mpzf.cmp (Mpzf.sub min max) size >= 0
          then top () (* [expr] covers more than [size]. Give up *)
          else
            (* [factor*size] is the amount by which we must shift [expr] *)
            let factor = Mpzf.fdiv_q (Mpzf.sub min inf) size in
            let shift = Mpzf.mul factor size in
            if Mpzf.cmp (Mpzf.sub max shift) sup > 0
            then top () (* [expr] overlaps on two ranges of [size] size.
                           Give up. *)
            else if Mpzf.cmp_int factor 0 = 0
            then expr (* Optimization *)
            else
              let coeff = Coeff.s_of_mpqf (Mpqf.of_mpz shift) in
              let e_coeff = Texpr1.Cst coeff in
              Texpr1.Binop (Texpr1.Sub, expr, e_coeff, Texpr1.Int, round)
        with Out_of_memory -> top ()

(* [coerce eval typ texpr] returns a normalized apron expression [e] such that
   its evaluation in the mathematical world and its evaluation with machine
   number are equivalent. [eval] is the mathematical evaluation function.
   If overflows are not allowed for the type [typ], then [texpr = e]. *)
let coerce ?(cast=false) eval typ texpr =
  match Cil.unrollType typ with
  | TInt (ikind, attrs) | TEnum ({ ekind = ikind}, attrs) ->
    let signed = Cil.isSigned ikind in
    if
      not cast
      && ((signed && Kernel.SignedOverflow.get ())
          || ((not signed) && Kernel.UnsignedOverflow.get ()))
    then
      texpr
    else
      let range = Eval_typ.ik_attrs_range ikind attrs in
      reduce eval texpr range
  | _ -> raise (Out_of_Scope "coerce not integer")


let translate_typ typ =
  match Cil.unrollType typ with
  | TInt _ | TEnum _ -> Texpr1.Int
  | _ -> raise (Out_of_Scope "translate_typ not int")

let translate_binop = function
  | PlusA  -> Texpr1.Add
  | MinusA -> Texpr1.Sub
  | Mult   -> Texpr1.Mul
  | Div    -> Texpr1.Div
  | Mod    -> Texpr1.Mod
  | _ -> raise (Out_of_Scope "translate_binop unhandled")

let translate_relation expr typ =
  let open Abstract_interp.Comp in
  function
  | Le ->
    let expr = Texpr1.unop Texpr1.Neg expr typ round in
    Tcons1.make expr Tcons1.SUPEQ
  | Lt ->
    let expr = Texpr1.unop Texpr1.Neg expr typ round in
    Tcons1.make expr Tcons1.SUP
  | Ge -> Tcons1.make expr Tcons1.SUPEQ
  | Gt -> Tcons1.make expr Tcons1.SUP
  | Eq -> Tcons1.make expr Tcons1.EQ
  | Ne -> Tcons1.make expr Tcons1.DISEQ

let translate_varinfo varinfo =
  if not (is_relevant_varinfo varinfo)
  then raise (Out_of_Scope "translate_varinfo irrelevant")
  else
    match Cil.unrollType varinfo.vtype with
    | TInt (ik, _) | TEnum ({ekind=ik}, _) ->
      let id = "_" ^ string_of_int varinfo.vid in
      let name = varinfo.vname ^ id in
      let var = Var.of_string name in
      if not (VarRanges.mem name) then
        VarRanges.replace name (Eval_typ.ik_range ik);
      var
    | _ -> raise (Out_of_Scope "translate_varinfo not integer")

let translate_lval = function
  | Var varinfo, NoOffset -> translate_varinfo varinfo
  | _ -> raise (Out_of_Scope "translate_lval not Var")

let translate_constant = function
  | CInt64 (i, _, _) -> begin
      try Coeff.s_of_int (Integer.to_int i) (* TODO: skip OCaml int type *)
      with Failure _ -> raise (Out_of_Scope "translate_constant big int")
    end
  | _ -> raise (Out_of_Scope "translate_constant not integer")

(* Translation of expressions from cil to apron. *)
let rec translate_expr eval oracle expr = match expr.enode with
  | Const cst -> Texpr1.Cst (translate_constant cst)
  | Lval lval -> Texpr1.Var (translate_lval lval)
  | UnOp (Neg, e1, typ) ->
    let e1' = translate_expr_linearize eval oracle e1 in
    Texpr1.(Unop (Neg, e1', translate_typ typ, round))
  | UnOp ((BNot | LNot), _, _) ->
    raise (Out_of_Scope "translate_expr bitwise unop")
  | BinOp (op, e1, e2, typ) ->
    let e1' = translate_expr_linearize eval oracle e1 in
    let e2' = translate_expr_linearize eval oracle e2 in
    let need_coercion = op = Mod || op = Div in
    let e1' = if need_coercion then coerce eval (Cil.typeOf e1) e1' else e1' in
    let e2' = if need_coercion then coerce eval (Cil.typeOf e2) e2' else e2' in
    let op' = translate_binop op in
    Texpr1.(Binop (op', e1', e2', translate_typ typ, round))
  | CastE (typ, e)->
    coerce ~cast:true eval typ (translate_expr_linearize eval oracle e)
  | Info (e, _) -> translate_expr eval oracle e
  | AddrOf _ | StartOf _  -> raise (Out_of_Scope "translate_expr addr")
  | SizeOf _ | SizeOfE _ | SizeOfStr _ | AlignOf _  | AlignOfE _ ->
    match Cil.constFoldToInt expr with
    | None -> raise (Out_of_Scope "translate_expr sizeof alignof")
    | Some i -> Texpr1.Cst (Coeff.s_of_int (Integer.to_int i))
(* Expressions that cannot be translated by [translate_expr] are replaced
   using an oracle. Of course, this oracle must be sound!. If the oracle
   cannot find a suitable replacement, it can re-raise the expresssion. *)
and translate_expr_linearize eval oracle expr =
  try translate_expr eval oracle expr
  with Out_of_Scope _ as e -> oracle expr e

(* Express a cil expression into an apron constraint. *)
let rec constraint_expr eval oracle env expr positive =
  match expr.enode with
  | UnOp (LNot, e, _) -> constraint_expr eval oracle env e (not positive)
  | BinOp ((Le|Ne|Eq|Gt|Lt|Ge as binop), e1, e2, typ) ->
    let e1' = translate_expr_linearize eval oracle e1 in
    let e2' = translate_expr_linearize eval oracle e2 in
    let e1'' = coerce eval (Cil.typeOf e1) e1' in
    let e2'' = coerce eval (Cil.typeOf e2) e2' in
    let typ = translate_typ (Cil.unrollType typ) in
    let e = Texpr1.Binop (Texpr1.Sub, e1'', e2'', typ, round) in
    let expr = Texpr1.of_expr env e in
    let binop = Value_util.conv_comp binop in
    let binop = if positive then binop else Abstract_interp.Comp.inv  binop in
    translate_relation expr typ binop
  | _ -> raise (Out_of_Scope "constraint_expr not handled")

(* Expresses the constraint [expr ∈ interval] as an Apron constraint. *)
let constraint_reduction env expr interval =
  let coeff = Texpr1.Cst (Coeff.Interval interval) in
  let expr = Texpr1.(Binop (Sub, expr, coeff, Int, round)) in
  let texpr = Texpr1.of_expr env expr in
  Tcons1.make texpr Tcons1.EQ


let truncate_interval typ interval =
  match Cil.unrollType typ with
  | TInt (ikind, attrs) | TEnum ({ ekind = ikind }, attrs) ->
    let signed = Cil.isSigned ikind in
    if
      (signed && not (Kernel.SignedOverflow.get ()))
      || ((not signed) && not (Kernel.UnsignedOverflow.get ()))
    then
      let range = Eval_typ.ik_attrs_range ikind attrs in
      let inf, sup, _size = bounds_of_typ range in
      let inf = Scalar.of_mpqf (Mpqf.of_mpz inf)
      and sup = Scalar.of_mpqf (Mpqf.of_mpz sup) in
      let min = interval.Interval.inf and max = interval.Interval.sup in
      let min = if Scalar.cmp min inf < 0 then inf else min
      and max = if Scalar.cmp max sup > 0 then sup else max in
      Interval.of_scalar min max
    else
      interval
  | _ -> interval

(* TODO: avoid OCaml int type during conversion *)
let interval_to_ival interval =
  let inf = scalar_to_int interval.Interval.inf
  and sup = scalar_to_int interval.Interval.sup in
  let inf = Extlib.opt_map Integer.of_int inf
  and sup = Extlib.opt_map Integer.of_int sup in
  Some (Ival.inject_range inf sup)

let int_to_scalar positive = function
  | None -> Scalar.of_infty positive
  | Some integer -> Scalar.of_mpqf (Mpqf.of_string (Integer.to_string integer))

let ival_to_interval = function
  | None -> Interval.top
  | Some ival ->
    let min, max = Ival.min_and_max ival in
    let min = int_to_scalar (-1) min and max = int_to_scalar 1 max in
    Interval.of_scalar min max


(* -------------------------------------------------------------------------- *)
(*                          Abstract Domain Functor                           *)
(* -------------------------------------------------------------------------- *)

module Make
    (Man: sig
       type t
       val manager: t Manager.t
       val name: string
       val key: t Abstract1.t Abstract_domain.key
     end)
= struct

  type state = Man.t Abstract1.t
  type value = Main_values.Interval.t
  type location = Precise_locs.precise_location

  let man = Man.manager

  let structure = Abstract_domain.Leaf Man.key
  let log_category = dkey

  let empty_env = Environment.make [||] [||]

  let top = Abstract1.top man empty_env
  let make_top env = Abstract1.top man env

  include Datatype.Make_with_collections (
    struct
      include Datatype.Undefined
      type t = state
      let name = Manager.get_library Man.manager
      let reprs = [top]
      let structural_descr = Structural_descr.t_unknown

      (* Abstract1.is_eq raises an error when the environments of the two
         states are incompatible. *)
      let equal a b =
        Environment.equal (Abstract1.env a) (Abstract1.env b)
        && Abstract1.is_eq man a b

      let hash = Abstract1.hash man

      (* BIGTODO: this function is not quite a total order, because [is_leq] is
         only a partial order. Using the hash as a first comparison is only
         a doubtful hack. *)
      let compare a b =
        if equal a b then 0
        else
          let cmp = compare (hash a) (hash b) in
          if cmp <> 0 then cmp
          else if Abstract1.is_leq man a b then 1 else -1

      let rehash = Datatype.identity
      let copy = Abstract1.copy man
      let pretty = Abstract1.print

      let mem_project = Datatype.never_any_project
    end )

  let name = Man.name

  let is_included = Abstract1.is_leq man

  let join s1 s2 =
    let env1 = Abstract1.env s1 and env2 = Abstract1.env s2 in
    if Environment.equal env1 env2
    then Abstract1.join man s1 s2
    else
      (* The two states may have different environments only in the joins
         at the end of a function call, for the recording of one state at
         each statement. *)
      let env1 = Abstract1.env s1
      and env2 = Abstract1.env s2 in
      let env = Environment.lce env1 env2 in
      let s1 = Abstract1.change_environment man s1 env false
      and s2 = Abstract1.change_environment man s2 env false in
      Abstract1.join man s1 s2

  let widen _kf _stmt s1 s2 = Abstract1.widening man s1 s2

  let narrow s1 s2 =
    let s = Abstract1.meet man s1 s2 in
    if Abstract1.is_bottom man s then `Bottom else `Value s

  type origin = unit

  let make_eval state =
    let env = Abstract1.env state in
    fun e ->
      let texp = Texpr1.of_expr env e in
      Abstract1.bound_texpr man state texp

  (* Meet the state with all the constraints. *)
  let meet_with_constraints env state constraints =
    let array = Tcons1.array_make env (List.length constraints) in
    List.iteri (fun i c -> Tcons1.array_set array i c) constraints;
    let st = Abstract1.meet_tcons_array man state array in
    if Abstract1.is_bottom man st then(
      Format.printf "Bottom with state %a and constraints %a@."
        Abstract1.print state (fun fmt a -> Tcons1.array_print fmt a) array;
      st)
    else st

  let _constraint_to_typ env state vars =
    let aux (var_apron, vi) =
      match Eval_typ.classify_as_scalar vi.vtype with
      | Eval_typ.TSInt range ->
        let inf, sup, _size = bounds_of_typ range in
        let inf = Scalar.of_mpqf (Mpqf.of_mpz inf)
        and sup = Scalar.of_mpqf (Mpqf.of_mpz sup) in
        let interval = Interval.of_scalar inf sup in
        let e = Texpr1.Var var_apron in
        constraint_reduction env e interval
      | _ -> assert false (* variable has been translated, and have int type *) 
    in
    let constraints = List.map aux vars in
    meet_with_constraints env state constraints

  (* Constraining a variable to the bounds of its type does not seem useful
     anymore. *)
  let constraint_to_typ _env state _vars = state

  let dummy_oracle _ exn = raise exn

  let compute state expr typ =
    let top = `Value (None, ()), Alarmset.all in
    if not (is_relevant expr)
    then top
    else
      try
        let eval = make_eval state in
        let oracle = dummy_oracle in
        let exp = coerce eval typ (translate_expr_linearize eval oracle expr) in
        let interval = eval exp in
        let interval = truncate_interval typ interval in
        let value =
          if Interval.is_bottom interval
          then `Bottom
          else `Value (interval_to_ival interval, ())
        in
        (* TODO: remove alarms if computation does not overflow *)
        value, Alarmset.all
      with
      | Out_of_Scope _ -> top
      (* May happen when evaluating an expression in the GUI, while the states
         of Apron have not been saved. In this case, we evaluate in the top
         apron state, whose environment raises the Failure exception. *)
      | Failure _ -> top

  let extract_expr _oracle state expr =
    compute state expr (Cil.typeOf expr)

  let extract_lval _oracle state lval typ _loc =
    let expr = Value_util.lval_to_exp lval in
    compute state expr typ

  let reduce_further _ _ _ = []

  let backward_location _state _lv _typ loc value = `Value (loc, value)

  let maybe_bottom state =
    if Abstract1.is_bottom man state
    then `Bottom
    else `Value state

  let forget_varinfo_list ~remove vars state =
    let env = state.Abstract1.env in
    let filter acc varinfo =
      try
        let apron_var = translate_varinfo varinfo in
        if Environment.mem_var env apron_var
        then (apron_var, varinfo) :: acc
        else acc
      with Out_of_Scope _ -> acc
    in
    let ok_vars = List.fold_left filter [] vars in
    let vars = Array.of_list (List.map fst ok_vars) in
    let state = Abstract1.forget_array man state vars false in
    if remove
    then
      let env = Environment.remove (Abstract1.env state) vars in
      Abstract1.change_environment man state env false
    else
      constraint_to_typ env state ok_vars

  let kill_bases loc state =
    let aux_ploc loc state =
      let bases = Locations.Location_Bits.get_bases loc.Locations.loc in
      match bases with
      | Base.SetLattice.Set set ->
        let var_of_base base acc =
          try (Base.to_varinfo base) :: acc
          with Base.Not_a_C_variable -> acc
        in
        let vars = Base.Hptset.fold var_of_base set [] in
        forget_varinfo_list ~remove:false vars state
      | Base.SetLattice.Top  -> make_top (Abstract1.env state)
    in
    Precise_locs.fold aux_ploc loc state

  let enter_scope vars state =
    let translate acc varinfo =
      try translate_varinfo varinfo :: acc
      with Out_of_Scope _ -> acc
    in
    let vars = List.fold_left translate [] vars in
    let env = Environment.add (Abstract1.env state) (Array.of_list vars) [||] in
    Abstract1.change_environment man state env false

  let leave_scope _kf vars state =
    forget_varinfo_list ~remove:true vars state

  let enter_loop _ state = state
  let incr_loop_counter _ state = state
  let leave_loop _ state = state

  module Transfer
      (Valuation: Abstract_domain.Valuation with type value = value
                                             and type loc = location)
  = struct

    (* make an oracle for the translation Cil->Apron, using the valuation.
       Translate integer expressions that have been evaluated (which should
       be all of them if the translation is called on a source expression!)
       into Apron intervals. *)
    let make_oracle valuation =
      fun exp exn ->
        if Cil.isIntegralType (Cil.typeOf exp) then
          match Valuation.find valuation exp with
          | `Value { value = { v = `Value itv } } ->
            let interval = ival_to_interval itv in
            Texpr1.Cst (Coeff.Interval interval)
          | _ -> raise exn
        else raise exn

    let update valuation state =
      let eval = make_eval state in
      let oracle = make_oracle valuation in
      let env = Abstract1.env state in
      (* Makes a list of apron constraints from a valuation:
         for each value marked as Reduced for an expression, creates the
         apron constraint [expression = value]. *)
      let gather_constraints exp record acc =
        if record.reductness = Reduced
        then
          try
            let expr = translate_expr_linearize eval oracle exp in
            let expr = coerce eval (Cil.typeOf exp) expr in
            (* When the value is top or bottom, no constraint is expressible. *)
            let cons = record.value.v >>- fun ival ->
              let interval = ival_to_interval ival in
              if Interval.is_top interval
              then `Bottom
              else `Value (constraint_reduction env expr interval)
            in
            Bottom.add_to_list cons acc
          with Out_of_Scope _ -> acc
        else acc
      in
      let constraints = Valuation.fold gather_constraints valuation [] in
      if constraints = []
      then state
      else meet_with_constraints env state constraints

    let assign _stmt lvalue expr _value valuation state =
      let state = update valuation state in
      try
        let state =
          try
            let eval = make_eval state in
            let oracle = make_oracle valuation in
            let var = translate_lval lvalue.lval in
            let expr = expr in
            let exp = translate_expr_linearize eval oracle expr in
            let exp = coerce eval lvalue.ltyp exp in
            let exp = Texpr1.of_expr (Abstract1.env state) exp in
            (* TODO: currently, all variables are present in the environment
               at all times. Change to a dynamic environment, in which new
               variables are added here, and removed when the scope changes. *)
            Abstract1.assign_texpr man state var exp None
          with
          | Out_of_Scope _ -> kill_bases lvalue.lloc state
        in
        maybe_bottom state
      with Manager.Error exclog -> abort exclog


    let assume _stmt exp bool valuation state =
      let state = update valuation state in
      try
        let env = Abstract1.env state in
        let eval = make_eval state in
        let oracle = make_oracle valuation in
        let cons = constraint_expr eval oracle env exp bool in
        let array = Tcons1.array_make env 1 in
        Tcons1.array_set array 0 cons;
        let state = Abstract1.meet_tcons_array man state array in
        maybe_bottom state
      with
      | Out_of_Scope _ -> `Value state

    let start_call _stmt call valuation state =
      let state = update valuation state in
      let eval = make_eval state in
      let oracle = make_oracle valuation in
      let process_argument (vars, acc) arg =
        try
          let env = Abstract1.env state in
          let var = translate_varinfo arg.formal in
          let vars = var :: vars in
          let acc =
            try
              let exp = translate_expr_linearize eval oracle arg.concrete in
              let texp = Texpr1.of_expr env exp in
              (var, texp) :: acc
            with Out_of_Scope _ -> acc
          in
          vars, acc
        with
        | Out_of_Scope _ -> (vars, acc)
      in
      let vars, list = List.fold_left process_argument ([], []) call.arguments in
      let env = Abstract1.env state in
      let vars_array = Array.of_list vars in
      let env = Environment.add env vars_array [||] in
      let vars, texprs = List.split list in
      let vars_array = Array.of_list vars
      and texprs_array = Array.of_list texprs in
      let state = Abstract1.change_environment man state env false in
      let state =
        Abstract1.assign_texpr_array man state vars_array texprs_array None
      in
      if Abstract1.is_bottom man state
      then Result (`Bottom, Value_types.Cacheable)
      else Compute state

    let finalize_call _stmt _call ~pre:_ ~post = `Value post

    let approximate_call _stmt call state =
      let name = Kernel_function.get_name call.kf in
      let state =
        if Ast_info.is_frama_c_builtin name ||
           (name <> "free" && Eval_typ.kf_assigns_only_result_or_volatile call.kf)
        then state
        else make_top (Abstract1.env state)
      in
      (* We need to introduce the variable used to model the return code
         (even though we do not constrain it), because it will be remove later
         by the generic part of the evaluator. *)
      let state = match call.return with
        | Some vi_ret -> enter_scope [vi_ret] state
        | None -> state
      in
      `Value [state]

    let show_expr _valuation _state _fmt _expr = ()
  end

  let logic_assign _assigns location ~pre:_ state = kill_bases location state
  let evaluate_predicate _ _ _ = Alarmset.Unknown
  let reduce_by_predicate _ state _ _ = `Value state

  let empty () = top

  let introduce_globals vars state = enter_scope vars state

  let enter_scope _kf vars state = enter_scope vars state

  let initialize_variable _lval _loc ~initialized:_ _init_value state = state

  let initialize_variable_using_type _kind varinfo state =
    try
      let var = translate_varinfo varinfo in
      let env = Abstract1.env state in
      if Environment.mem_var env var
      then state
      else
        let env = Environment.add env [|var|] [||] in
        let state = Abstract1.change_environment man state env false in
        constraint_to_typ env state [(var, varinfo)]
    with
    | Out_of_Scope _ -> state

  let filter_by_bases _ state = state
  let reuse ~current_input:_ ~previous_output = previous_output

  let storage = Value_parameters.ApronStorage.get

end


let octagon_key = Structure.Key_Domain.create_key "apron-octagon"
let box_key = Structure.Key_Domain.create_key "apron-box"
let polka_loose_key = Structure.Key_Domain.create_key "polka-loose"
let polka_strict_key = Structure.Key_Domain.create_key "polka-strict"
let polka_equalities_key = Structure.Key_Domain.create_key "polka-equalities"


module Apron_Octagon = struct
  type t = Oct.t
  let manager = Oct.manager_alloc ()
  let name = "Apron octagon domain"
  let key = octagon_key
end

module Apron_Box = struct
  type t = Box.t
  let manager = Box.manager_alloc ()
  let name = "Apron box domain"
  let key = box_key
end

module Apron_Polka_Loose = struct
  type t = Polka.loose Polka.t
  let manager = Polka.manager_alloc_loose ()
  let name = "Polka loose polyhedra domain"
  let key = polka_loose_key
end
module Apron_Polka_Strict = struct
  type t = Polka.strict Polka.t
  let manager = Polka.manager_alloc_strict ()
  let name = "Polka strict polyhedra domain"
  let key = polka_strict_key
end
module Apron_Polka_Equalities = struct
  type t = Polka.equalities Polka.t
  let manager = Polka.manager_alloc_equalities ()
  let name = "Polka linear equalities domain"
  let key = polka_equalities_key
end

(** Apron manager allocation changes the rounding mode. *)
let () = Floating_point.set_round_nearest_even ()

module Octagon = Domain_builder.Complete (Make (Apron_Octagon))
module Box = Domain_builder.Complete (Make (Apron_Box))
module Polka_Loose = Domain_builder.Complete (Make (Apron_Polka_Loose))
module Polka_Strict = Domain_builder.Complete (Make (Apron_Polka_Strict))
module Polka_Equalities = Domain_builder.Complete (Make (Apron_Polka_Equalities))


(*
Local Variables:
compile-command: "make -C ../../../../.. -j"
End:
*)
