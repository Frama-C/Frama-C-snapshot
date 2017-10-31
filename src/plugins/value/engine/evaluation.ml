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

(* Evaluation of expressions to values. *)

open Cil_types
open Eval


(* The forward evaluation of an expression [e] gives a value to each subterm
   of [e], from its variables to the root expression [e]. It also computes the
   set of alarms which may occur in the evaluation of each subterm.
   All these intermediate results of an evaluation are stored in a cache, whose
   type is described in eval.mli. The cache is the complete result of the
   evaluation. *)

(* The forward evaluation of an expression relies on queries of the abstract
   domain, which must be able to assign a value to some expression (see
   abstract_domain.mli for more details).
   An oracle for the value of expressions is also given to the domain,
   which may use it to build its answer. This oracle is the main forward
   evaluation function itself, so the domain may initiate the evaluation
   of some new expressions.
   To avoid loops in the use of the oracle:
   - before any computation or an expression [e], Value.top is stored
     in the cache; this dummy value will be erased at the end of the
     computation, but in the meantime, any evaluation of [e] (by the oracle)
     returns top immediately.
   - fuel is used to limit the depth of the use of the oracle. The fuel level is
     decremented at each use of the oracle up to zero, where the oracle returns
     top.
     The fuel level with which an expression has been evaluated is stored in the
     cache. The recomputation of an expression may be performed with a higher
     fuel than before, if needed. *)

(* Reductions may happen in the forward evaluation when:
   - a domain returns a value more precise than the one internally computed;
   - alarms are emitted by an operation on values. In particular, locations
     are reduced to their valid part for a read or write operation.
   These reductions are propagated to the sub-expressions by a backward
   evaluation, after the forward evaluation has finished.
   The backward evaluation also propagates the reduction stemming from an if
   statement, where the condition may be reduced to zero or non-zero. *)

(* An expression is deemed volatile if it contains an access to a volatile
   location. The forward evaluation computes this syntactically, by checking
   for volatile qualifiers on sub-lvalues and intermediate types. A 'volatile'
   flag is propagated through the expression. This flag prevents the update of
   the value computed by the initial forward evaluation. *)

(* When a backward reduction has been successfully performed, the domain may
   initiate new reductions, via the reduce_further function.
   A fuel level is used in the same way as for the forward evaluation to
   avoid reduction loops. *)


(* The fuel level with which an expression has been evaluated. *)
type fuel =
  | Loop
  (* No evaluation at all: the value in the cache is a dummy value, set
     to avoid a loop in the use of the oracle. *)
  | Finite of int
  (* An evaluation with a finite level of fuel, which has been consumed. *)
  | Infty
  (* The evaluation never used all its fuel. *)

let less_fuel_than n = function
  | Loop     -> true
  | Finite f -> f >= n
  | Infty    -> true

type reduction_kind = | Neither | Forward | Backward

let update_reduction reduction b = match reduction with
  | Neither -> if b then Neither else Forward
  | x -> x

(* Some information about a forward evaluation. *)
type forward_report = {
  fuel: fuel;                (* The fuel used for the evaluation. *)
  reduction: reduction_kind; (* Whether a reduction has occur, which may be
                                propagated to the sub-terms. *)
  volatile: bool;         (* If true, the expression may contain an access
                             to a volatile location, and thus cannot be safely
                             reduced. *)
}

(* Parameters of the evaluation of the location of a left value. *)
type loc_report = {
  for_writing: bool;
  with_reduction: bool;
}

(* If a value is cached by an external source, we assume that it was
   computed with infty fuel, that possible reduction have been
   propagated backward, and that the expression cannot be volatile. *)
let extern_report = { fuel = Infty; reduction = Neither; volatile = false }

(* Report used when the cache is filled with a dummy value to avoid evaluation
   loops. *)
let dummy_report = { fuel = Loop; reduction = Neither; volatile = false }

let no_fuel = -1
let root_fuel () = Value_parameters.OracleDepth.get ()
let backward_fuel () = Value_parameters.ReductionDepth.get ()

let already_precise_loc_report ~for_writing ~reduction loc_report =
  (not for_writing || loc_report.for_writing)
  && (not reduction || loc_report.with_reduction)


let rec may_be_reduced_offset = function
  | NoOffset -> false
  | Field (_, offset) -> may_be_reduced_offset offset
  | Index _ -> true

let may_be_reduced_lval (host, offset) = match host with
  | Var _ -> may_be_reduced_offset offset
  | Mem _ -> true


module type S = sig
  type state
  type value
  type origin
  type loc
  module Valuation : Valuation with type value = value
                                and type origin = origin
                                and type loc = loc
  val evaluate :
    ?valuation:Valuation.t -> ?reduction:bool ->
    state -> exp -> (Valuation.t * value) evaluated
  val copy_lvalue :
    ?valuation:Valuation.t ->
    state -> lval -> (Valuation.t * value flagged_value) evaluated
  val lvaluate :
    ?valuation:Valuation.t -> for_writing:bool ->
    state -> lval -> (Valuation.t * loc * typ) evaluated
  val reduce:
    ?valuation:Valuation.t -> state -> exp -> bool -> Valuation.t evaluated
  val assume:
    ?valuation:Valuation.t -> state -> exp -> value -> Valuation.t or_bottom
  val split_by_evaluation:
    exp -> Integer.t list -> state list ->
    (Integer.t * state list * bool) list * state list
  val check_non_overlapping:
    state -> lval list -> lval list -> unit evaluated
  val eval_function_exp:
    exp -> state -> (Kernel_function.t * Valuation.t) list evaluated
end

let return t = `Value t, Alarmset.none

(* Intersects [alarms] with the only possible alarms from the dereference of
   the left-value [lval] of type [typ].
   Useful if the abstract domain returns a non-closed AllBut alarmset for
   some lvalues. *)
let close_dereference_alarms lval typ alarms =
  let init_alarm = Alarms.Uninitialized lval
  and escap_alarm = Alarms.Dangling lval in
  let init_status = Alarmset.find init_alarm alarms
  and escap_status = Alarmset.find escap_alarm alarms in
  let reduced = init_status <> Alarmset.True || escap_status <> Alarmset.True in
  let closed_alarms = Alarmset.set init_alarm init_status Alarmset.none in
  let closed_alarms = Alarmset.set escap_alarm escap_status closed_alarms in
  match typ with
  | TFloat (fkind, _) ->
    let expr = Value_util.lval_to_exp lval in
    let nan_inf_alarm = Alarms.Is_nan_or_infinite (expr, fkind) in
    let nan_inf_status = Alarmset.find nan_inf_alarm alarms in
    Alarmset.set nan_inf_alarm nan_inf_status closed_alarms, reduced
  | _ -> closed_alarms, reduced

let define_value value =
  { v = `Value value; initialized = true; escaping = false }

(* [record] and [alarms] must be the value and the alarms resulting from the
   evaluation of the lvalue [lval].
   This function removes the alarms about the initialization and the
   escaping of [lval], and sets accordingly the initialized and escaping flags
   of the computed value. *)
let indeterminate_copy lval result alarms =
  let init_alarm = Alarms.Uninitialized lval
  and escap_alarm = Alarms.Dangling lval in
  let initialized = Alarmset.find init_alarm alarms = Alarmset.True
  and escaping = not (Alarmset.find escap_alarm alarms = Alarmset.True) in
  let alarms =
    if not (initialized)
    then Alarmset.set init_alarm Alarmset.True alarms
    else alarms
  in
  let alarms =
    if escaping
    then Alarmset.set escap_alarm Alarmset.True alarms
    else alarms
  in
  let reductness = Unreduced in
  let v, origin = match result with
    | `Bottom -> `Bottom, None
    | `Value (v, origin) -> `Value v, Some origin
  in
  let value = { v; initialized; escaping } in
  let record = { value; origin; reductness; val_alarms = alarms} in
  record, alarms


module type Value = sig
  include Abstract_value.External
  val reduce : t -> t
end

module type Queries = sig
  include Abstract_domain.Queries
  include Datatype.S with type t = state
end

module Make
    (Value : Value)
    (Loc : Abstract_location.S with type value = Value.t)
    (Domain : Queries with type value = Value.t
                       and type location = Loc.location)
= struct

  type state = Domain.state
  type value = Value.t
  type origin = Domain.origin
  type loc = Loc.location

  module ECache = Cil_datatype.ExpStructEq.Map
  module LCache = Cil_datatype.LvalStructEq.Map

  (* Imperative cache for the evaluation:
     all intermediate results of an evaluation are cached here.
     See [eval.mli] for more details. *)
  module Cache = struct
    type value = Value.t
    type origin = Domain.origin
    type loc = Loc.location

    (* For expression, the forward_report about the evaluation is also stored. *)
    type t =
      ((value, origin) record_val * forward_report) ECache.t
      * (loc record_loc * (forward_report * loc_report)) LCache.t

    (* Interface of Context.Valuation *)
    let empty : t = ECache.empty, LCache.empty
    let find (cache:t) exp =
      try `Value (fst (ECache.find exp (fst cache)))
      with Not_found -> `Top
    let add (cache:t) exp record =
      let s, t = cache in ECache.add exp (record, extern_report) s, t
    let fold f (cache:t) acc =
      ECache.fold (fun e (r, _) acc -> f e r acc) (fst cache) acc

    (* Functions used by the evaluator, with the boolean for backward
       reduction. *)
    let find' (cache:t) exp = ECache.find exp (fst cache)
    let add' (cache:t) exp record =
      let s, t = cache in ECache.add exp record s, t

    (* Locations of lvalue. *)
    let find_loc (cache:t) lval =
      try `Value (fst (LCache.find lval (snd cache)))
      with Not_found -> `Top

    (* Locations of lvalue. *)
    let find_loc' (cache:t) lval =
      try `Value (LCache.find lval (snd cache))
      with Not_found -> `Top
    let add_loc' (cache:t) lval record =
      let s, t = cache in s, LCache.add lval record t

    let remove (s, t) expr = ECache.remove expr s, t
    let remove_loc (s, t) lval = s, LCache.remove lval t
  end

  (* Imperative cache for the evaluator. A reference is mandatory here, because
     the cache must be also filled by the evaluations initiated by a domain
     through the oracle, but should not leak in the domain queries signature. *)
  let cache = ref Cache.empty

  (* Was the fuel entirely consumed? *)
  let fuel_consumed = ref false

  let top_record =
    let flagged_top =
      { v = `Value Value.top; initialized = false; escaping = true }
    in
    { value = flagged_top; origin = None;
      reductness = Dull; val_alarms = Alarmset.all }

  (* Updates the abstractions stored in the cache for the expression [expr]
     with the given record, report and value. [kind] is the type of the
     reduction (forward or backward). *)
  let reduce_expr_recording kind expr (record, report) value =
    (* Avoids reduction of volatile expressions. *)
    if report.volatile then ()
    else
      let red = record.reductness in
      let reductness =
        if red = Unreduced && kind <> Neither then Reduced else red
      in
      (* TODO: allow to reduce initialized and escaping flags? *)
      let record = { record with value = define_value value; reductness } in
      let report = { report with reduction = kind } in
      cache := Cache.add' !cache expr (record, report)

  (* Updates the abstractions stored in the cache for the expression [expr]
     with the value [value]. [kind] is the type of the reduction.*)
  let reduce_expr_value kind expr value =
    let record, report = Cache.find' !cache expr in
    reduce_expr_recording kind expr (record, report) value

  let reduce_value record =
    let v = record.value.v >>-: Value.reduce in
    { record with value = {record.value with v = v} }


  let may_overflow = function
    | Shiftlt | Mult | MinusPP | MinusPI | IndexPI | PlusPI
    | PlusA | Div | Mod | MinusA -> true
    | _ -> false

  let handle_integer_overflow expr range eval =
    let signed = range.Eval_typ.i_signed in
    if (signed && Kernel.SignedOverflow.get ()) ||
       (not signed && Kernel.UnsignedOverflow.get ())
    then eval >>= Value.truncate_integer expr range
    else eval >>=: Value.rewrap_integer range

  let handle_overflow ~may_overflow expr typ eval =
    match Eval_typ.classify_as_scalar typ with
    | Eval_typ.TSInt range ->
      (* If the operation cannot overflow, truncates the abstract value to the
         range of the type (without emitting alarms). This can regain some
         precision when the abstract operator was too imprecise.
         Otherwise, truncates or rewraps the abstract value according to
         the parameters of the analysis. *)
      if not may_overflow
      then eval >>=. fun v -> fst (Value.truncate_integer expr range v)
      else handle_integer_overflow expr range eval
    | Eval_typ.TSFloat fk ->
      eval >>= Value.cast_float expr fk
    | Eval_typ.TSPtr _
    | Eval_typ.TSNotScalar -> eval

  (* Makes the oracle for the domain queries, called by the forward evaluation.
     Defined below, after applying the subdivided_evaluation to the forward
     evaluation function.  *)
  let make_oracle = ref (fun _ _ _ -> `Value Value.top, Alarmset.all)

  (* Returns the cached value and alarms for the evaluation if it exists;
     call [coop_forward_eval] and caches its result otherwise.
     Also returns a boolean indicating whether the expression is volatile.  *)
  let rec root_forward_eval fuel state expr =
    (* Search in the cache for the result of a previous computation. *)
    try
      let record, report = Cache.find' !cache expr in
      (* If the record was computed with more fuel than [fuel], return it. *)
      if report.fuel = Loop then fuel_consumed := true;
      if less_fuel_than fuel report.fuel
      then (record.value.v >>-: fun v -> v, report.volatile), record.val_alarms
      else raise Not_found
    (* If no result found, evaluate the expression. *)
    with Not_found ->
      let previous_fuel_consumed = !fuel_consumed in
      (* Fuel not consumed for this new evaluation. *)
      fuel_consumed := false;
      (* Fill the cache to avoid loops in the use of the oracle. *)
      cache := Cache.add' !cache expr (top_record, dummy_report);
      (* Evaluation of [expr]. *)
      let result, alarms = coop_forward_eval fuel state expr in
      let value =
        result >>- fun (record, reduction, volatile) ->
        (* Put the alarms in the record. *)
        let record = { record with val_alarms = alarms } in
        (* Inter-reduction of the value (in case of a reduced product). *)
        let record = reduce_value record in
        (* Cache the computed result with an appropriate report. *)
        let fuel = if !fuel_consumed then Finite fuel else Infty in
        let report = {fuel; reduction; volatile} in
        cache := Cache.add' !cache expr (record, report);
        record.value.v >>-: fun v -> v, volatile
      in
      (* Reset the flag fuel_consumed. *)
      fuel_consumed := previous_fuel_consumed || !fuel_consumed;
      value, alarms

  and forward_eval fuel state expr = root_forward_eval fuel state expr >>=: fst

  (* The functions below returns, along with the computed value (when it is not
     bottom):
     - the state of reduction of the current expression: Neither if it has
       not been reduced, Forward otherwise.
     - a boolean indicating whether the expression is volatile. *)

  (* Asks the abstract domain for abstractions (value and alarms) of [expr],
     and performs the narrowing with the abstractions computed by
     [internal_forward_eval].  *)
  and coop_forward_eval fuel state expr =
    match expr.enode with
    | Lval lval -> eval_lval fuel state lval
    | BinOp _ | UnOp _ | CastE _ -> begin
        let intern_value, alarms = internal_forward_eval fuel state expr in
        let oracle = !make_oracle fuel state in
        let domain_value, alarms' = Domain.extract_expr oracle state expr in
        (* Intersection of alarms, as each sets of alarms are correct
           and "complete" for the evaluation of [expr]. *)
        match Alarmset.inter alarms alarms' with
        | `Inconsistent ->
          Value_parameters.abort ~current:true ~once:true
            "Inconsistent status of alarms: unsound states."
        | `Value alarms ->
          let v =
            intern_value >>- fun (intern_value, reduction, volatile) ->
            domain_value >>- fun (domain_value, origin) ->
            Value.narrow intern_value domain_value >>-: fun result ->
            let reductness =
              if Value.equal domain_value result then Unreduced
              else if Value.(equal domain_value top) then Created else Reduced
            in
            let reduction =
              update_reduction reduction (Value.equal intern_value result)
            and origin = Some origin
            and value = define_value result in
            (* The proper alarms will be set in the record by forward_eval. *)
            {value; origin; reductness; val_alarms = Alarmset.all},
            reduction, volatile
          in
          v, alarms
      end
    | _ ->
      internal_forward_eval fuel state expr
      >>=: fun (value, reduction, volatile) ->
      let value = define_value value
      and origin = None
      and reductness = Dull in
      {value; origin; reductness; val_alarms = Alarmset.all},
      reduction, volatile

  (* Recursive descent in the sub-expressions. *)
  and internal_forward_eval fuel state expr =
    let compute_reduction (v, a) volatile =
      (v, a) >>=: fun v ->
      let reduction = if Alarmset.is_empty a then Neither else Forward in
      v, reduction, volatile
    in
    match expr.enode with
    | Info (e, _) -> internal_forward_eval fuel state e

    | Const c ->
      begin match c with
        | CEnum {eival = e} ->
          forward_eval fuel state e >>=: fun value -> value, Neither, false
        | _ -> Value.constant expr c >>=: fun value -> value, Neither, false
      end

    | Lval _lval -> assert false

    | AddrOf v | StartOf v ->
      lval_to_loc fuel ~for_writing:false ~reduction:false state v
      >>=: fun (loc, _, _) ->
      Loc.to_value loc, Neither, false

    | UnOp (op, e, typ) ->
      root_forward_eval fuel state e >>= fun (v, volatile) ->
      let context = { operand = e }
      and e_typ = Cil.unrollType (Cil.typeOf e) in
      let v = Value.forward_unop ~context e_typ op v in
      let may_overflow = op = Neg in
      let v = handle_overflow ~may_overflow expr typ v in
      compute_reduction v volatile

    | BinOp (op, e1, e2, typ) ->
      let context =
        { left_operand = e1; right_operand = e2; binary_result = expr }
      in
      root_forward_eval fuel state e1 >>= fun (v1, volatile1) ->
      root_forward_eval fuel state e2 >>= fun (v2, volatile2) ->
      let typ_e1 = Cil.unrollType (Cil.typeOf e1) in
      let v = Value.forward_binop ~context typ_e1 op v1 v2 in
      let may_overflow = may_overflow op in
      let v = handle_overflow ~may_overflow expr typ v in
      compute_reduction v (volatile1 || volatile2)

    | CastE (dst_typ, e) ->
      root_forward_eval fuel state e >>= fun (value, volatile) ->
      let src_typ = Cil.typeOf e in
      let v = Value.do_promotion ~src_typ ~dst_typ e value in
      compute_reduction v volatile

    | SizeOf _ | SizeOfE _ | SizeOfStr _ | AlignOf _ | AlignOfE _ ->
      match Cil.constFoldToInt expr with
      | Some v -> return (Value.inject_int (Cil.typeOf expr) v, Neither, false)
      | _      -> return (Value.top_int, Neither, false)


  (* ------------------------------------------------------------------------
                                Lvalue evaluation
     ------------------------------------------------------------------------ *)

  (* Calls the internal evaluation of an lvalue to a location, and stores the
     result in the cache. If the result is already in the cache, the computation
     is avoided, unless if it may reduce the cache.
     If [reduction] is false, don't reduce the location and the offset by their
     valid parts, and don't emit alarms about their validity.
     If the location is not bottom, the function also returns the typ of the
     lvalue, and a boolean indicating that the lvalue contains a sub-expression
     with volatile qualifier (in its host or offset). *)
  and lval_to_loc fuel ~for_writing ~reduction state lval =
    let compute () =
      let res, alarms =
        reduced_lval_to_loc fuel ~for_writing ~reduction state lval
      in
      let res =
        res >>-: fun (loc, typ_offs, red, volatile) ->
        let record = { loc; typ = typ_offs; loc_alarms = alarms }
        and report = { fuel = Finite fuel; reduction = red; volatile }
        and loc_report = { for_writing; with_reduction = reduction } in
        cache := Cache.add_loc' !cache lval (record, (report, loc_report));
        (loc, typ_offs, volatile)
      in
      res, alarms
    in
    match Cache.find_loc' !cache lval with
    | `Value (record, (report, loc_report)) ->
      if
        already_precise_loc_report ~for_writing ~reduction loc_report
        && less_fuel_than fuel report.fuel
      then `Value (record.loc, record.typ, report.volatile), record.loc_alarms
      else compute ()
    | `Top -> compute ()

  (* If [reduction] is false, don't reduce the location and the offset by their
     valid parts, and don't emit alarms about their validity. *)
  and reduced_lval_to_loc fuel ~for_writing ~reduction state lval =
    internal_lval_to_loc fuel ~for_writing ~reduction state lval
    >>= fun (loc, typ, volatile) ->
    if not reduction
    then `Value (loc, typ, Neither, volatile), Alarmset.none
    else
      let bitfield = Cil.isBitfield lval in
      Loc.reduce_loc_by_validity ~for_writing ~bitfield lval loc
      >>=: fun valid_loc ->
      let reduction = if Loc.equal_loc valid_loc loc then Neither else Forward in
      valid_loc, typ, reduction, volatile

  (* Internal evaluation of a lvalue to an abstract location.
     Combination of the evaluation of the right part of an lval (an host) with
     an offset, to obtain a location *)
  and internal_lval_to_loc fuel ~for_writing ~reduction state lval =
    let host, offset = lval in
    let typ = match host with
      | Var host -> host.vtype
      | Mem x -> Cil.typeOf_pointed (Cil.typeOf x)
    in
    eval_offset fuel ~reduce_valid_index:reduction typ state offset
    >>= fun (offs, typ_offs, offset_volatile) ->
    if for_writing && Value_util.is_const_write_invalid typ_offs
    then
      `Bottom,
      Alarmset.singleton (Alarms.Memory_access (lval, Alarms.For_writing))
    else
      eval_host fuel state typ_offs offs host >>=: fun (loc, host_volatile) ->
      loc, typ_offs, offset_volatile || host_volatile

  (* Host evaluation. Also returns a boolean which is true if the host
     contains a volatile sub-expression. *)
  and eval_host fuel state typ_offset offs = function
    | Var host ->
      (Loc.forward_variable typ_offset host offs >>-: fun loc -> loc, false),
      Alarmset.none
    | Mem x ->
      root_forward_eval fuel state x >>=. fun (loc_lv, volatile) ->
      Loc.forward_pointer typ_offset loc_lv offs >>-: fun loc ->
      loc, volatile

  (* Offset evaluation. Also returns a boolean which is true if the offset
     contains a volatile sub-expression. *)
  and eval_offset fuel ~reduce_valid_index typ state = function
    | NoOffset               -> return (Loc.no_offset, typ, false)
    | Index (index_expr, remaining) ->
      let typ_pointed, array_size = match Cil.unrollType typ with
        | TArray (t, size, _, _) -> t, size
        | t -> Value_parameters.fatal ~current:true
                 "Got type '%a'" Printer.pp_typ t
      in
      eval_offset fuel ~reduce_valid_index typ_pointed state remaining >>=
      fun (roffset, typ_offs, remaining_volatile) ->
      root_forward_eval fuel state index_expr >>= fun (index, volatile) ->
      let valid_index =
        if not (Kernel.SafeArrays.get ()) || not reduce_valid_index
        then `Value index, Alarmset.none
        else
          try
            (* If possible, reduce the index value by the array size. *)
            let size = Cil.lenOfArray64 array_size in
            (* Handle the special GCCism of zero-sized arrays: Frama-C
               pretends their size is unknown, exactly like GCC. *)
            if Integer.is_zero size
            then `Value index, Alarmset.none
            else
              let size_expr = Extlib.the array_size in (* array_size exists *)
              Loc.reduce_index_by_array_size ~size_expr ~index_expr size index
              >>=: fun new_index ->
              (* Update the cache with the new value of the index *)
              if not (Value.equal index new_index)
              then reduce_expr_value Forward index_expr new_index;
              new_index
          with
          | Cil.LenOfArray -> `Value index, Alarmset.none (* unknown array size *)
      in
      valid_index >>=: fun index ->
      Loc.forward_index typ_pointed index roffset, typ_offs,
      remaining_volatile || volatile
    | Field (fi, remaining) ->
      let attrs = Cil.filter_qualifier_attributes (Cil.typeAttrs typ) in
      let typ_fi = Cil.typeAddAttributes attrs fi.ftype in
      eval_offset fuel ~reduce_valid_index typ_fi state remaining
      >>=: fun (r, typ_res, volatile) ->
      let off = Loc.forward_field typ fi r in
      off, typ_res, volatile

  and eval_lval ?(indeterminate=false) fuel state lval =
    (* Computes the location of [lval]. *)
    lval_to_loc fuel ~for_writing:false ~reduction:true state lval
    >>= fun (loc, typ_lv, volatile_expr) ->
    (* the lvalue is volatile:
       - if it has qualifier volatile (lval_to_loc propagates qualifiers
         in the proper way through offsets)
       - if it contains a sub-expression which is volatile (volatile_expr)
    *)
    let volatile = volatile_expr || Cil.typeHasQualifier "volatile" typ_lv in
    (* Find the value of the location, if not bottom. *)
    let oracle = !make_oracle fuel state in
    let v, alarms = Domain.extract_lval oracle state lval typ_lv loc in
    let alarms, reduced = close_dereference_alarms lval typ_lv alarms in
    if indeterminate
    then
      let record, alarms = indeterminate_copy lval v alarms in
      `Value (record, Neither, volatile), alarms
    else
      let reduction = if Alarmset.is_empty alarms then Neither else Forward in
      (v, alarms) >>=: fun (value, origin) ->
      let value = define_value value
      and origin = Some origin
      and reductness = if reduced then Reduced else Unreduced in
      (* The proper alarms will be set in the record by forward_eval. *)
      {value; origin; reductness; val_alarms = Alarmset.all},
      reduction, volatile

  (* ------------------------------------------------------------------------
                       Subdivided Forward Evaluation
     ------------------------------------------------------------------------ *)

  (* These two modules could be implemented as mutually recursive, to avoid
     the reference for the oracle given to the domains. *)
  module Forward_Evaluation = struct
    type state = Domain.t
    let evaluate ?(valuation=Cache.empty) ~fuel state expr =
      cache := valuation;
      root_forward_eval fuel state expr >>=: fun (value, _) ->
      !cache, value
  end

  module Subdivided_Evaluation =
    Subdivided_evaluation.Make (Value) (Loc) (Cache) (Forward_Evaluation)

  let () =
    make_oracle :=
      fun fuel state ->
        let fuel = pred fuel in
        if fuel >  0
        then
          fun expr ->
            let valuation = !cache in
            Subdivided_Evaluation.evaluate ~valuation ~fuel state expr
            >>=: fun (valuation, value) ->
            cache := valuation;
            value
        else
          fun _ -> fuel_consumed := true; `Value Value.top, Alarmset.all

  let subdivided_forward_eval valuation state expr =
    let fuel = root_fuel () in
    Subdivided_Evaluation.evaluate ~valuation ~fuel state expr


  (* ------------------------------------------------------------------------
                           Backward Evaluation
     ------------------------------------------------------------------------ *)

  (* Find the value of a previously evaluated expression. *)
  let find_val expr =
    match Cache.find !cache expr with
    | `Value record -> record.value.v
    | `Top -> assert false (* [expr] must have been evaluated already. *)

  (* Find the record computed for an lvalue.
     Return None if no reduction can be performed. *)
  let find_loc_for_reduction lval =
    if not (may_be_reduced_lval lval)
    then None
    else
      let record, report = match Cache.find_loc' !cache lval with
        | `Value all -> all
        | `Top -> assert false
      in
      if (snd report).with_reduction
      then Some (record, report)
      else None

  (* Evaluate an expression before any reduction, if needed. Also return the
     report indicating if a forward reduction during the forward evaluation may
     be propagated backward. *)
  let evaluate_for_reduction state expr =
    try `Value (Cache.find' !cache expr)
    with Not_found ->
      fst (forward_eval no_fuel state expr) >>-: fun _ ->
      try Cache.find' !cache expr
      with Not_found -> assert false

  (* The backward propagation at a step is relevant only if:
     - the new value (if any) is more precise than the old one.
       Then the latter is reduced by the former, and the reduction kind is set
       to [Backward].
     - or the old value has been reduced during the forward evaluation.
       Then, [report.reduced] is [Forward], and must be set to [Neither] as
       the reduction is propagated but the value of the current expression is
       unchanged. *)
  let backward_reduction old_value latter_reduction value =
    let propagate_forward_reduction () =
      if latter_reduction = Forward then Some (old_value, Neither) else None
    in
    match value with
    | None -> `Value (propagate_forward_reduction ())
    | Some new_value ->
      Value.narrow old_value new_value >>-: fun value ->
      if Value.is_included old_value value
      then propagate_forward_reduction ()
      else Some (value, Backward)

  (* [backward_eval state expr value] reduces the the expression [expr] and
     its subterms in the cache, according to the state [state]:
     - the reductions performed during the forward evaluation (due to alarms or
       abstract domains) are propagated backward to the subexpressions;
     - if [value = Some v], then [expr] is assumed to evaluate to [v] (and is
       reduced accordingly). *)
  let rec backward_eval fuel state expr value =
    (* Evaluate the expression if needed. *)
    evaluate_for_reduction state expr >>- fun (record, report) ->
    (* Reduction of [expr] by [value]. *)
    let reduce kind value =
      (* Avoids reduction of volatile expressions. *)
      if report.volatile then ()
      else
        let value = Value.reduce value in
        reduce_expr_recording kind expr (record, report) value;
        (* If enough fuel, asks the domain for more reductions. *)
        if fuel > 0
        then
          let reduce (expr, v) =
            ignore (backward_eval (pred fuel) state expr (Some v))
          in
          List.iter reduce (Domain.reduce_further state expr value)
    in
    record.value.v >>- fun old_value ->
    (* Determines the need of a backward reduction. *)
    backward_reduction old_value report.reduction value >>- function
    | None ->
      (* If no reduction to be propagated, just visit the subterms. *)
      recursive_descent fuel state expr
    | Some (value, kind) ->
      (* Otherwise, backward propagation to the subterms. *)
      match expr.enode with
      | Lval lval ->
        begin
          (* For a lvalue, we try to reduce its location according to the value;
             this operation may lead to a more precise value for this lvalue,
             which is then reduced accordingly. *)
          backward_loc state lval value >>- function
          | None ->
            reduce kind value;
            recursive_descent_lval fuel state lval
          | Some (loc, new_value) ->
            let kind =
              if Value.is_included old_value new_value then Neither else Backward
            in
            reduce kind new_value;
            internal_backward_lval fuel state loc lval
        end
      | _ ->
        reduce kind value;
        internal_backward fuel state expr value

  (* Backward propagate the reduction [expr] = [value] to the subterms of the
     compound expression [expr]. *)
  and internal_backward fuel state expr value =
    match expr.enode with
    | Lval _lv -> assert false
    | UnOp (LNot, e, _) ->
      let cond = Value_util.normalize_as_cond e false in
      (* TODO: should we compute the meet with the result of the call to
         Value.backward_unop? *)
      backward_eval fuel state cond (Some value)
    | UnOp (op, e, _typ) ->
      let typ_e = Cil.unrollType (Cil.typeOf e) in
      find_val e >>- fun v ->
      Value.backward_unop ~typ_arg:typ_e op ~arg:v ~res:value
      >>- fun v ->
      backward_eval fuel state e v
    | BinOp (binop, e1, e2, typ) ->
      let typ_res = Cil.unrollType typ
      and typ_e1 = Cil.typeOf e1 in
      find_val e1 >>- fun v1 ->
      find_val e2 >>- fun v2 ->
      Value.backward_binop
        ~input_type:typ_e1
        ~resulting_type:typ_res
        binop ~left:v1 ~right:v2 ~result:value
      >>- fun (v1, v2) ->
      backward_eval fuel state e1 v1 >>- fun () ->
      backward_eval fuel state e2 v2
    | CastE (typ, e) ->
      begin
        let dst_typ = Cil.unrollType typ in
        let src_typ = Cil.unrollType (Cil.typeOf e) in
        find_val e >>- fun src_val ->
        Value.backward_cast ~src_typ ~dst_typ ~src_val ~dst_val:value
        >>- function v -> backward_eval fuel state e v
      end
    | Info (e, _) -> backward_eval fuel state e None
    | _ -> `Value ()

  and recursive_descent fuel state expr =
    match expr.enode with
    | Lval lval -> backward_lval fuel state lval
    | UnOp (_, e, _)
    | CastE (_, e)
    | Info (e, _) -> backward_eval fuel state e None
    | BinOp (_binop, e1, e2, _typ) ->
      backward_eval fuel state e1 None >>- fun () ->
      backward_eval fuel state e2 None
    | _ -> `Value ()

  and recursive_descent_lval fuel state (host, offset) =
    recursive_descent_host fuel state host >>- fun () ->
    recursive_descent_offset fuel state offset

  and recursive_descent_host fuel state = function
    | Var _ -> `Value ()
    | Mem expr -> backward_eval fuel state expr None >>-: fun _ -> ()

  and recursive_descent_offset fuel state = function
    | NoOffset               -> `Value ()
    | Field (_, remaining)   -> recursive_descent_offset fuel state remaining
    | Index (exp, remaining) ->
      backward_eval fuel state exp None >>- fun __ ->
      recursive_descent_offset fuel state remaining

  (* Even if the value of an lvalue has not been reduced, its memory location
     could have been, and this can be propagated backward. Otherwise, continue
     the recursive descent. *)
  and backward_lval fuel state lval =
    match find_loc_for_reduction lval with
    | None -> recursive_descent_lval fuel state lval
    | Some (record, report) ->
      if (fst report).reduction = Forward
      then internal_backward_lval fuel state record.loc lval
      else recursive_descent_lval fuel state lval

  (* [backward_loc state lval value] tries to reduce the memory location of the
     lvalue [lval] according to its value [value] in the state [state]. *)
  and backward_loc state lval value =
    match find_loc_for_reduction lval with
    | None -> `Value None
    | Some (record, report) ->
      Domain.backward_location state lval record.typ record.loc value
      >>- fun (loc, new_value) ->
      Value.narrow new_value value >>-: fun value ->
      let b = not (Loc.equal_loc record.loc loc) in
      (* Avoids useless reductions and reductions of volatile expressions. *)
      if b && not (fst report).volatile
      then
        let record = { record with loc } in
        let report = { (fst report) with reduction = Backward }, snd report in
        cache := Cache.add_loc' !cache lval (record, report);
      else ();
      if b || (fst report).reduction = Forward
      then Some (loc, value)
      else None

  and internal_backward_lval fuel state location = function
    | Var host, offset ->
      Loc.backward_variable host location >>- fun loc_offset ->
      backward_offset fuel state host.vtype offset loc_offset
    | Mem expr, offset ->
      match offset with
      | NoOffset ->
        let loc_value = Loc.to_value location in
        backward_eval fuel state expr (Some loc_value) >>-: fun _ -> ()
      | _ ->
        let reduce_valid_index = true in
        let typ_lval = Cil.typeOf_pointed (Cil.typeOf expr) in
        fst (eval_offset no_fuel ~reduce_valid_index typ_lval state offset)
        >>- fun (loc_offset, _, _) ->
        find_val expr >>- fun value ->
        Loc.backward_pointer value loc_offset location
        >>- fun (pointer_value, loc_offset) ->
        backward_eval fuel state expr (Some pointer_value) >>- fun _ ->
        backward_offset fuel state typ_lval offset loc_offset

  and backward_offset fuel state typ offset loc_offset = match offset with
    | NoOffset               -> `Value ()
    | Field (field, remaining)  ->
      Loc.backward_field typ field loc_offset >>- fun rem ->
      backward_offset fuel state field.ftype remaining rem
    | Index (exp, remaining) ->
      find_val exp >>- fun v ->
      let typ_pointed = Cil.typeOf_array_elem typ in
      fst (eval_offset no_fuel ~reduce_valid_index:true typ_pointed state remaining)
      >>- fun (rem, _, _) ->
      Loc.backward_index typ_pointed v rem loc_offset >>- fun (v', rem') ->
      let reduced_v = if Value.is_included v v' then None else Some v' in
      backward_eval fuel state exp reduced_v >>- fun _ ->
      backward_offset fuel state typ_pointed remaining rem'


  (* ------------------------------------------------------------------------
                       Second Pass of Forward Evaluation
     ------------------------------------------------------------------------ *)

  exception Not_Exact_Reduction

  (** Second forward evaluation after a backward propagation for the condition
      of an if statement.
      Allows to forward propagate the backward reductions. Uses the internal
      forward functions to actually perform the computation instead of relying
      on the cache. However, the internal evaluation uses the cache for the sub-
      expressions, so this evaluation is still bottom-up, and update the cache
      progressively.
      Stops the descent as soon as there is no backward propagation to recover
      for an expression. However, more backward reduction could have been done
      below for other reasons (due to alarms or domains).
      Raises Not_Exact_Reduction if at any point, the forward evaluation leads
      to a less precise value than the one stored after the backward evaluation.
      This means that the backward propagation has not been precise enough. *)
  let rec second_forward_eval state expr =
    let record, report =
      try Cache.find' !cache expr
      with Not_found -> assert false
    in
    if report.reduction <> Backward then `Value ()
    else
      record.value.v >>- fun value ->
      recursive_descent state expr >>- fun () ->
      let new_value =
        match expr.enode with
        | Lval lval -> second_eval_lval state lval value
        | _ ->
          fst (internal_forward_eval no_fuel state expr)
          >>-: fun (v, _, _) -> v
      in
      new_value >>- fun evaled ->
      let evaled = Value.reduce evaled in
      Value.narrow value evaled >>-: fun new_value ->
      if not (Value.is_included evaled value)
      then raise Not_Exact_Reduction
      else
        let kind = if Value.equal value new_value then Neither else Forward in
        reduce_expr_value kind expr new_value

  and second_eval_lval state lval value =
    if not (may_be_reduced_lval lval)
    then `Value value
    else
      let record, report = match Cache.find_loc' !cache lval with
        | `Value all -> all
        | `Top -> assert false
      in
      let evaloc =
        if (fst report).reduction = Backward
        then
          let for_writing = false
          and reduction = true in
          fst (reduced_lval_to_loc no_fuel ~for_writing ~reduction state lval)
          >>-: fun (loc, _, _, _) ->
          (* TODO: Loc.narrow *)
          let record = { record with loc } in
          let reduction =
            if Loc.equal_loc record.loc loc then Neither else Forward
          in
          let report = { (fst report) with reduction }, snd report in
          cache := Cache.add_loc' !cache lval (record, report);
        else `Value ()
      in
      evaloc >>- fun () ->
      fst (eval_lval no_fuel state lval) >>- fun (record, _, _) ->
      record.value.v

  and recursive_descent state expr =
    match expr.enode with
    | Lval lval -> recursive_descent_lval state lval
    | UnOp (_, e, _)
    | CastE (_, e)
    | Info (e, _) -> second_forward_eval state e
    | BinOp (_binop, e1, e2, _typ) ->
      second_forward_eval state e1 >>- fun () ->
      second_forward_eval state e2
    | _ -> `Value ()

  and recursive_descent_lval state (host, offset) =
    recursive_descent_host state host >>- fun () ->
    recursive_descent_offset state offset

  and recursive_descent_host state = function
    | Var _ -> `Value ()
    | Mem expr -> second_forward_eval state expr

  and recursive_descent_offset state = function
    | NoOffset               -> `Value ()
    | Field (_, remaining)   -> recursive_descent_offset state remaining
    | Index (exp, remaining) ->
      second_forward_eval state exp >>- fun () ->
      recursive_descent_offset state remaining

  (* ------------------------------------------------------------------------
                              Generic Interface
     ------------------------------------------------------------------------ *)

  module Valuation = Cache

  let evaluate ?(valuation=Cache.empty) ?(reduction=true) state expr =
    let eval, alarms = subdivided_forward_eval valuation state expr in
    let result =
      if not reduction || Alarmset.is_empty alarms
      then eval
      else
        eval >>- fun (valuation, value) ->
        cache := valuation;
        backward_eval (backward_fuel ()) state expr None >>-: fun _ ->
        !cache, value
    in
    result, alarms

  let copy_lvalue ?(valuation=Cache.empty) state lval =
    let expr = Value_util.lval_to_exp lval
    and fuel = root_fuel () in
    try
      let record, report = Cache.find' valuation expr in
      if less_fuel_than fuel report.fuel
      then `Value (valuation, record.value), record.val_alarms
      else raise Not_found
    with Not_found ->
      cache := valuation;
      eval_lval ~indeterminate:true fuel state lval
      >>=: fun (record, _, volatile) ->
      let record = reduce_value record in
      (* Cache the computed result with an appropriate report. *)
      let report =
        { fuel = Finite (root_fuel ()); reduction = Neither; volatile }
      in
      let valuation = Cache.add' !cache expr (record, report) in
      valuation, record.value

  (* When evaluating an lvalue, we use the subdivided evaluation for the
     expressions included in the lvalue. *)
  let rec evaluate_offsets valuation state = function
    | NoOffset             -> `Value valuation, Alarmset.none
    | Field (_, offset)    -> evaluate_offsets valuation state offset
    | Index (expr, offset) ->
      subdivided_forward_eval valuation state expr
      >>= fun (valuation, _value) ->
      evaluate_offsets valuation state offset

  let evaluate_host valuation state = function
    | Var _    -> `Value valuation, Alarmset.none
    | Mem expr ->
      subdivided_forward_eval valuation state expr >>=: fst

  let lvaluate ?(valuation=Cache.empty) ~for_writing state lval =
    let host, offset = lval in
    evaluate_host valuation state host >>= fun valuation ->
    evaluate_offsets valuation state offset >>= fun valuation ->
    cache := valuation;
    lval_to_loc (root_fuel ()) ~for_writing ~reduction:true state lval
    >>=. fun (_, typ, _) ->
    backward_lval (backward_fuel ()) state lval >>-: fun _ ->
    match Cache.find_loc !cache lval with
    | `Value record -> !cache, record.loc, typ
    | `Top -> assert false

  let reduce ?valuation:(valuation=Cache.empty) state expr positive =
    (* Generate [e == 0] *)
    let expr = Value_util.normalize_as_cond expr (not positive) in
    cache := valuation;
    root_forward_eval (root_fuel ()) state expr >>=. fun (_v, volatile) ->
    (* Reduce by [(e == 0) == 0] *)
    backward_eval (backward_fuel ()) state expr (Some Value.zero)
    >>- fun () ->
    try second_forward_eval state expr >>-: fun () -> !cache
    with Not_Exact_Reduction ->
      (* Avoids reduce_by_cond_enumerate on volatile expressions. *)
      if volatile then `Value !cache
      else Subdivided_Evaluation.reduce_by_enumeration !cache state expr false

  let assume ?valuation:(valuation=Cache.empty) state expr value =
    cache := valuation;
    backward_eval (backward_fuel ()) state expr (Some value) >>-: fun _ ->
    !cache


  (* ------------------------------------------------------------------------
                                      Misc
     ------------------------------------------------------------------------ *)

  let eval_function_exp funcexp state =
    match funcexp.enode with
    | Lval (Var vinfo, NoOffset) ->
      `Value [Globals.Functions.get vinfo, Valuation.empty],
      Alarmset.none
    | Lval (Mem v, NoOffset) ->
      evaluate state v >>= fun (valuation, value) ->
      let typ_pointer = Cil.typeOf funcexp in
      let kfs, alarm = Value.resolve_functions ~typ_pointer value in
      let alarm =
        Alarmset.(if alarm then singleton (Alarms.Function_pointer v) else none)
      in
      (* For pointer calls, we retro-propagate which function is being called
         in the abstract state. This may be useful:
         - inside the call for languages with OO (think 'self')
         - everywhere, because we may remove invalid values for the pointer
         - after if enough slevel is available, as states obtained in
           different functions are not merged by default. *)
      let reduce kf =
        cache := valuation;
        let vi_f = Kernel_function.get_vi kf in
        let value = Value.inject_address vi_f in
        backward_eval 0 state v (Some value) >>- fun () ->
        try second_forward_eval state v >>-: fun () -> !cache
        with Not_Exact_Reduction ->
          (* Build the expression [exp_f == &f] and reduce accordingly *)
          let addr = Cil.mkAddrOfVi vi_f in
          let expr = Cil.mkBinOp ~loc:v.eloc Eq v addr in
          let valuation = !cache in
          Subdivided_Evaluation.reduce_by_enumeration valuation state expr true
      in
      let process kf acc =
        let res = reduce kf >>-: fun valuation -> kf, valuation in
        Bottom.add_to_list res acc
      in
      begin match kfs with
        | `Value kfs ->
          Bottom.bot_of_list (Kernel_function.Hptset.fold process kfs []), alarm
        | `Top ->
          if Mark_noresults.no_memoization_enabled () then
            Value_parameters.abort ~current:true
              "Function pointer evaluates to anything. Try deactivating \
               option(s) -no-results, -no-results-function and \
               -obviously-terminates@."
          else
            Value_parameters.fatal ~current:true
              "Function pointer evaluates to anything. function %a"
              Printer.pp_exp funcexp
      end
    | _ -> assert false

  let split_by_evaluation = match Value.get Main_values.cvalue_key with
    | None -> fun _ _ states -> [], states
    | Some get -> fun expr expected_values states ->
      let typ = Cil.typeOf expr in
      let eval acc state =
        match fst (evaluate state expr) with
        | `Bottom -> acc
        | `Value (_cache, value) ->
          let zero_or_one = Cvalue.V.cardinal_zero_or_one (get value) in
          (state, value, zero_or_one) :: acc
      in
      let eval_states = List.fold_left eval [] states in
      let match_expected_value expected_value states =
        let process_one_state (eq, mess, neq) (s, v, zero_or_one as current) =
          if Value.is_included expected_value v then
            (* The integer on which we split is part of the result *)
            if zero_or_one then
              (s :: eq, mess, neq) (* Clean split *)
            else
              (eq, true, current :: neq) (* v is not exact: mess, i.e. no split *)
          else
            (eq, mess, current :: neq) (* Integer not in the result at all *)
        in
        List.fold_left process_one_state ([], false, []) states
      in
      let process_one_value (acc, states) i =
        let value = Value.reduce (Value.inject_int typ i) in
        let eq, mess, neq = match_expected_value value states in
        (i, eq, mess) :: acc, neq
      in
      let matched, tail =
        List.fold_left process_one_value ([], eval_states) expected_values
      in
      matched, List.map (fun (s, _, _) -> s) tail

  let check_non_overlapping state lvs1 lvs2 =
    let eval_loc (acc_list, valuation) lval =
      let for_writing = false in
      match fst (lvaluate ~valuation ~for_writing state lval) with
      | `Bottom -> acc_list, valuation
      | `Value (valuation, loc, _) -> (lval, loc) :: acc_list, valuation
    in
    let eval_list valuation lvs =
      List.fold_left eval_loc ([], valuation) lvs
    in
    let list1, valuation = eval_list Valuation.empty lvs1 in
    let list2, _ = eval_list valuation lvs2 in
    Loc.check_non_overlapping list1 list2

end


(*
Local Variables:
compile-command: "make -C ../../../.."
End:
*)
