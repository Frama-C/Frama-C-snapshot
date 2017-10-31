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
open Cvalue
open Eval

let dkey = Value_parameters.register_category "nonlin"

module LvalMap = Cil_datatype.LvalStructEq.Map

(* Join two maps; keys bound in both maps become bound to (expr, depth). *)
let union expr depth map1 map2 =
  let merge _ a b = match a, b with
    | None, None -> None
    | Some x, None
    | None, Some x -> Some x
    | Some _, Some _ -> Some (expr, depth)
  in
  LvalMap.merge merge map1 map2

(* [gather_non_linear expr] computes a map from the lvalues [lval] in [expr] to
   the smallest subexpressions of [expr] that contains all occurrence of [lval].
   The depth of each subexpressions (w.r.t. lvalues and binop operations) is
   also stored in the map.
   If a lvalue is bound to itself, then it appears only once in [expr].
   Otherwise, we say that the expression is non linear on this lvalue. *)
let gather_non_linear expr =
  let rec compute depth expr =
    match expr.enode with
    | Lval (host, offset as lv) ->
      let d = succ depth in
      let map = compute_from_offset d expr offset in
      let map = union expr d (compute_from_host d host) map in
      LvalMap.add lv (expr, d) map
    | UnOp (_, e, _) | CastE (_, e) | Info (e, _) -> compute depth e
    | BinOp (_, e1, e2, _) ->
      (* Lvalues that appear in [e1] and [e2] are bound to [expr]. *)
      let d = succ depth in union expr d (compute d e1) (compute d e2)
    | _ -> LvalMap.empty
  and compute_from_host depth = function
    | Mem e -> compute depth e
    | Var _ -> LvalMap.empty
  and compute_from_offset depth lval = function
    | NoOffset -> LvalMap.empty
    | Field (_, offset) -> compute_from_offset depth lval offset
    | Index (e, offset) ->
      let map1 = compute_from_offset depth lval offset in
      let map2 = compute depth e in
      union lval depth map1 map2
  in
  compute 0 expr

(* Map from subexpressions to the list of their non-linear lvalues. *)
module ExpMap = struct
  include Cil_datatype.ExpStructEq.Map
  let add expr lv map =
    try
      let list = find expr map in
      add expr (lv :: list) map
    with Not_found -> add expr [lv] map
end

(* Map from the depth of subexpression to ExpMap. *)
module DepthMap = struct
  include Datatype.Int.Map
  let add depth expr lv map =
    let expmap =
      try
        let expmap = find depth map in
        ExpMap.add expr lv expmap
      with Not_found -> ExpMap.singleton expr [lv]
    in
    add depth expmap map
end

let same lval expr = match expr.enode with
  | Lval lv -> Cil_datatype.LvalStructEq.equal lv lval
  | _ -> false

(* Converts a map from lvalues to expressions and depth into an association
   list from expressions to list of lvalues, sorted by decreasing depth of
   expressions.
   The lvalues bound to themselves are ignored. *)
let reverse_map map =
  let fill lval (expr, depth) acc =
    if same lval expr then acc else DepthMap.add depth expr lval acc
  in
  let depthmap = LvalMap.fold fill map DepthMap.empty in
  let concat _depth map acc = ExpMap.bindings map @ acc in
  DepthMap.fold concat depthmap []


module LvalList = Datatype.List (Cil_datatype.LvalStructEq)
module NonLinear = Datatype.Pair (Cil_datatype.Exp) (LvalList)
module NonLinears = Datatype.List (NonLinear)

module Non_linear_expressions =
  State_builder.Hashtbl (Cil_datatype.ExpStructEq.Hashtbl) (NonLinears)
    (struct
      let name = "Value.Subdivided_evaluation.Non_linear_expressions"
      let size = 16
      let dependencies = [ Ast.self ]
    end)

(* Computes the non-linear subexpressions of [expr], and the lvalues on which
   they are non-linear.
   Returns an association list from such subexpressions to lists of involved
   lvalues, sorted by decreasing depth of the subexpressions. *)
let compute_non_linear expr =
  try Non_linear_expressions.find expr
  with Not_found ->
    let map = gather_non_linear expr in
    let list = reverse_map map in
    List.iter
      (fun (e, lval) ->
         Value_parameters.result ~current:true ~once:true ~dkey
           "non-linear '%a', lv '%a'" Printer.pp_exp e
           (Pretty_utils.pp_list ~sep:", " Printer.pp_lval) lval)
      list;
    Non_linear_expressions.replace expr list;
    list


exception Too_linear

let min_and_max_float v =
  try
    let i = V.project_ival v in
    let f1, f2 = Ival.min_and_max_float i in
    V.inject_float f1, V.inject_float f2
  with V.Not_based_on_null -> assert false

let subdiv_float_interval ~size v =
  try
    let v_ival = V.project_ival v in
    let ival1, ival2 = Ival.subdiv_float_interval ~size v_ival in
    V.inject_ival ival1, V.inject_ival ival2
  with V.Not_based_on_null -> assert false

let min_and_max_int v =
  try
    let i = V.project_ival v in
    match Ival.min_and_max i with
    | None, _ | _, None -> raise Too_linear
    | Some i1, Some i2 -> V.inject_int i1, V.inject_int i2
  with V.Not_based_on_null -> assert false

let subdiv_int v =
  try
    let i = V.project_ival v in
    let l, h = Ival.subdiv_int i in
    V.inject_ival l, V.inject_ival h
  with V.Not_based_on_null -> assert false

let subdiv_float size v =
  let size =
    match Integer.to_int size with
    | 32 -> Some Fval.Float32
    | 64 -> Some Fval.Float64
    | s ->
      if s = Cil.(theMachine.theMachine.sizeof_longdouble) then
        (* for long doubles, we currently consider as "all rounding modes" *)
        None
      else
        Kernel.fatal ~current:true "invalid float size: %a"
          Abstract_interp.Int.pretty size
  in
  subdiv_float_interval ~size v

(* [make_split lval value] makes a split function for the lval [lval]
   containing the abstract value [value].
   Raise Too_linear if the value contains pointer. *)
let make_split size value =
  try
    let b, i = Cvalue.V.find_lonely_key value in
    if Base.is_null b then
      match i with
      | Ival.Set _ | Ival.Top _ -> subdiv_int
      | Ival.Float _ -> subdiv_float size
    else
      raise Too_linear (* pointers *)
  with Not_found -> raise Too_linear (* multiple bases -> pointers *)


module type Forward_Evaluation = sig
  type value
  type valuation
  type state
  val evaluate:
    ?valuation:valuation -> fuel:int ->
    state -> exp -> (valuation * value) evaluated
end

module Make
    (Value : Abstract_value.External)
    (Loc : Abstract_location.S with type value = Value.t)
    (Valuation: Valuation with type value = Value.t
                           and type loc = Loc.location)
    (Eva: Forward_Evaluation with type value := Value.t
                              and type valuation := Valuation.t)
= struct

  (* Values are converted to {!Cvalue.V.t}, because those are
     currently the only values on which we can split. *)

  let get_cval = match Value.get Main_values.cvalue_key with
    | Some get -> get
    | None -> fun _ -> Cvalue.V.top

  let set_cval =
    let set = Value.set Main_values.cvalue_key in
    fun cval v -> set cval v

  let activated = Value.mem Main_values.cvalue_key

  (* A subdivision of the evaluation of an expression according to an lvalue
     [lval] is stored by a working list (implemented as a heap), where each
     disjunct of the abstract value for [lval] is associated to the result of
     the evaluation for this disjunct.
     The set of abstract values for [lval] are a partition of the initial
     abstract value computed for [lval].
     The results of the evaluations are pairs of a value and the emitted alarms.
  *)
  type eval_result = Value.t evaluated
  module Cmp = struct
    type t = Cvalue.V.t * eval_result
    (* We use this reference because we need to change the "direction" of
       the comparison function, but it is convenient to have the same
       module in both cases. *)
    let cmp_subdiv = ref (fun _ _ -> 0)
    let compare (x1, (y1, _): t) (x2, (y2, _): t) =
      let n = !cmp_subdiv y1 y2 in
      (* the relative order between x1 and x2 is unimportant. We just need
         an ordering function that does not make equal subdivisions with equal
         images but different interval. The current order has been chosen
         to mimic the results of the previous implementations. *)
      if n = 0 then Cvalue.V.compare x2 x1 else n
  end
  module Subdiv = Leftistheap.Make(Cmp)
  type subdiv = Subdiv.t

  (* Flatten a subdivision by joining the values and the alarms.
     For the subdivided lvalue, the initial values that lead to Bottom are
     removed (but the alarms of the evaluation are kept). *)
  let flatten (list: subdiv) =
    let join_or_acc join a b = match a with
      | `Bottom  -> `Value b
      | `Value a -> `Value (join a b)
    in
    Subdiv.fold
      (fun (var', (value', alarms')) (var, value, alarms) ->
         match value' with
         | `Bottom -> var, value, Alarmset.union alarms alarms'
         | `Value v -> join_or_acc Cvalue.V.join var var',
                       join_or_acc Value.join value v,
                       Alarmset.union alarms alarms')
      list
      (`Bottom, `Bottom, Alarmset.none)

  (* Computes the image of the bounds of [lv_cval]. Since those bounds
     are singletons, their image must be in the result of the subdivision *)
  let compute_sure_bounds compute lv_cval =
    let boundmin, boundmax =
      if V.is_included lv_cval V.top_float then min_and_max_float lv_cval
      else if V.is_included lv_cval V.top_int then min_and_max_int lv_cval
      else raise Too_linear (* pointers *)
    in
    let res1, _alarms1 = compute boundmin in
    let res2, _alarms2 = compute boundmax in
    Bottom.join Value.join res1 res2

  (* [list] must be a list representing a subdivision.
     [has_better_bound] is an order over the elements of [list].
     [subdiv_lval subdivnb list has_better_bound split compute] takes the
     smallest element of [list] according to [has_better_bound], [split] its
     value in two smaller values, and [compute] the result for each.
     The process is repeated [subdivnb] times, or until we detect no more
     improvement is possible. *)
  let subdiv subdivnb list has_better_bound split compute =
    Cmp.cmp_subdiv := has_better_bound;
    let working_list = ref (Subdiv.of_list list) in
    let min = Subdiv.min !working_list in
    (* We will never be able to improve further than these bounds. *)
    let bound = ref (compute_sure_bounds compute (fst min)) in
    (* compute the image of [subvalue], and insert it in [list] for further
       refinement. If [subvalue] is a singleton, also refine our criterion
       for stopping. *)
    let compute_subvalue subvalue (list: subdiv) =
      let res = compute subvalue in
      if V.cardinal_zero_or_one subvalue then
        bound := Bottom.join Value.join (fst res) !bound;
      Subdiv.insert (subvalue, res) list;
    in
    try
      for _i = 1 to subdivnb do
        let (value, (v, _)), s = Subdiv.extract_min !working_list in
        if has_better_bound v !bound >= 0 then
          (* [v] over-approximates the images of all the values in [value].
             They are all worse than [!bound], which must be in the final
             result. Thus, there is no point in subdividing [value].
             Furthermore, since [working_list] is sorted, all the
             other subdivisions also have a worse bound. Thus, we stop. *)
          raise Abstract_interp.Can_not_subdiv;
        let subvalue1, subvalue2 = split value in
        let s1 = compute_subvalue subvalue1 s in
        let s2 = compute_subvalue subvalue2 s1 in
        working_list := s2
      done;
      !working_list
    with Abstract_interp.Can_not_subdiv ->
      !working_list

  (* This function makes orders over elements of a subdiv list.
     We try to reduce the infimum and the supremum of the result value of the
     evaluation. The initial [result_value] is needed to know the type of the
     comparison (float or int). *)
  let better_bound result_value =
    let result_value = get_cval result_value in
    let compare_min, compare_max =
      if V.is_included result_value V.top_float
      then V.compare_min_float, V.compare_max_float
      else if V.is_included result_value V.top_int
      then V.compare_min_int, V.compare_max_int
      else raise Too_linear
    in
    let better_bound compare_bound e1 e2 =
      match e1, e2 with
      | `Bottom, `Bottom -> 0
      | `Bottom, _ -> 1
      | _, `Bottom -> -1
      | `Value v1, `Value v2 -> compare_bound (get_cval v1) (get_cval v2)
    in
    better_bound compare_min, better_bound compare_max

  module Clear = Clear_Valuation (Valuation)

  (* These two functions assume that the given expression or lvalue have been
     evaluated in the valuation. *)
  let find_val valuation expr = match Valuation.find valuation expr with
    | `Value record -> record
    | `Top -> assert false

  let find_loc valuation lval = match Valuation.find_loc valuation lval with
    | `Value record -> record
    | `Top -> assert false

  (* Subdivision of the evaluation of the expression [expr], according to the
     lvalue [lval], in the state [state].
     [subexpr] is the smallest subexpression of [expr] containing all occurrences
     of the lvalue [lval]. It is thus its value that needs to be reduced.
     However, we perform the complete evaluation of [expr] to be able to remove
     the values of [lval] that lead to Bottom.
     Note that [expr] may be equal to [subexpr].
     [result] and [alarms] are the result of the evaluation of [subexpr].
     This function returns the alarms and the valuation resulting from the
     subdivision. *)
  let subdiv_lval ~fuel subdivnb state expr subexpr lval result alarms =
    let valuation, value = result in
    (* Abstract value of [lval]. *)
    let lv_exp = Value_util.lval_to_exp lval in
    let lv_record = find_val valuation lv_exp in
    match lv_record.value.v with
    | `Bottom -> raise Too_linear
    | `Value lv_value ->
      let lv_cval = get_cval lv_value in
      let record = find_loc valuation lval in
      (* The size is defined, as [lv] is a scalar *)
      let size = Int_Base.project (Eval_typ.sizeof_lval_typ record.typ) in
      (* Split function for this abstract value. *)
      let split = make_split size lv_cval in
      (* Clear the valuation to force the evaluation on top of [lval]. *)
      let cleared_valuation =
        Clear.clear_englobing_exprs valuation ~expr ~subexpr:lv_exp
      in
      let eq_equal_subexpr = Cil_datatype.ExpStructEq.equal expr subexpr in
      (* Computes the value of [subexpr] when [lval] has the value [subvalue].
         Returns [Bottom] if the complete evaluation of [expr] is bottom for
         this subvalue.  *)
      let compute sub_cval =
        let subvalue = set_cval sub_cval lv_value in
        let value  = { lv_record.value with v = `Value subvalue } in
        let record = { lv_record with value = value } in
        let valuation = Valuation.(add cleared_valuation lv_exp record) in
        let eval = Eva.evaluate ~fuel ~valuation state expr in
        (* Retrieve the value of [subexpr] from [eval]. Also returns the alarms
           for the evaluation of [expr], which ensure the soundness of the
           bottom case. *)
        if eq_equal_subexpr then
          eval >>=: snd (* optimize *)
        else
          eval >>=. fun (valuation, _) ->
          let record = find_val valuation subexpr in record.value.v
      in
      (* The initial subdivision, with one disjunct. *)
      let subdiv_list = [ lv_cval, (`Value value, alarms) ] in
      let better_min, better_max = better_bound value in
      (* Subdivision to reduce the infimum of the result value. *)
      let wl_min = subdiv subdivnb subdiv_list better_min split compute in
      let subdiv_list = Subdiv.elements wl_min in
      (* Subdivision to reduce the supremum of the result value. *)
      let wl_max = subdiv subdivnb subdiv_list better_max split compute in
      (* Results of the subdivision: new values for [lval] and for [subexpr]. *)
      let reduced_lv, result_subexpr, val_alarms = flatten wl_max in
      (* If [reduced_lv] and [result_subexpr] are not bottom,
         then reduce the valuation by their new value records. *)
      let valuation =
        reduced_lv >>- fun reduced_lv ->
        let valuation = (* Update for lv *)
          let v = `Value (set_cval reduced_lv lv_value) in
          let value = { lv_record.value with v } in
          Valuation.add valuation lv_exp
            {lv_record with value; reductness = Reduced}
        in
        result_subexpr >>-: fun result_subexpr ->
        let valuation = (* Update for subexp *)
          let record = find_val valuation subexpr in
          let value = { record.value with v = `Value result_subexpr } in
          Valuation.add valuation subexpr
            { record with value; val_alarms }
        in
        valuation
      in
      valuation, val_alarms

  (* Before any subdivision, a first evaluation is needed. If it leads to bottom,
     then returns bottom and the alarms. Otherwise, do the subdivision and
     only return its result, which is sound and may be more precise than the
     previous evaluation. *)
  let (>>>) (t, a) f = match t with
    | `Bottom  -> `Bottom, a
    | `Value t -> f t

  (* Evaluation of [expr] in state [state],
     with at most (2 * [subdivnb]) subdivisions for each lvalue. *)
  let subdivides_evaluation ~fuel subdivnb valuation state expr =
    (* Evaluation of [expr] without subdivision. *)
    let default = Eva.evaluate ~fuel ~valuation state expr in
    default >>> fun (valuation, value) ->
    if not (Value.is_included value Value.top_int) then begin
      Value_parameters.debug ~level:2
        "subdivfloatvar: expression evaluates to an address";
      default
    end
    else
      let no_alarm = Alarmset.is_empty (snd default) in
      (* List of non-linear subexpressions [subexpr], with the lvalues that
         appear multiple times in [subexpr], candidates for the subdivision. *)
      let vars = compute_non_linear expr in
      let rec try_sub vars valuation =
        match vars with
        | [] ->
          (* No more subdivision: finally evaluate the complete expression. *)
          Eva.evaluate ~fuel ~valuation state expr
        | (_subexpr, []) :: tail ->
          (* No more lvalue on which to subdivide for [subexpr]. *)
          try_sub tail valuation
        | (subexpr, lval :: lvs) :: tail ->
          let tail = (subexpr, lvs) :: tail in
          try
            Value_parameters.result ~current:true ~once:true ~dkey
              "subdividing on %a" Printer.pp_lval lval;
            (* Result for the evaluation of the subexpression [subexpr]. *)
            let _, alarms as res =
              Eva.evaluate ~fuel ~valuation state subexpr
            in
            res >>> fun result ->
            (* Do not try to subdivide if [subexpr] contains some pointers:
               the {!better_bound} heuristic only works on numerical values. *)
            if not Cvalue.V.(is_included (get_cval (snd result)) top_int)
            then raise Too_linear;
            (* If the evaluation of the complete expression [expr] raises some
               alarms, then force the evaluation of [expr] for the subdivision:
               some subvalues of [lval] could lead to bottom and be removed.
               Otherwise, only evaluate [subexpr] in the subdivision. *)
            let e = if no_alarm then subexpr else expr in
            (* Subdivide on the value of [lval]. *)
            subdiv_lval ~fuel subdivnb state e subexpr lval result alarms
            >>> fun valuation ->
            (* Clear the valuation on top of [subexpr], to force the future
               reevaluation of the complete expression [expr] with the updated
               result for [subexpr] in the valuation. *)
            let valuation = Clear.clear_englobing_exprs valuation ~expr ~subexpr in
            try_sub tail valuation
          with Too_linear -> try_sub tail valuation
      in
      try_sub vars valuation

  let evaluate ?(valuation=Valuation.empty) ~fuel state expr =
    let subdivnb = Value_parameters.LinearLevel.get () in
    if subdivnb = 0 || not activated
    then
      Eva.evaluate ~valuation ~fuel state expr
    else
      subdivides_evaluation ~fuel subdivnb valuation state expr


  (* ---------------------- Reduction by enumeration ------------------------ *)

  (* Reduce by cond enumerate : when a backward evaluation is not precise
     enough, tries to reduce further by enumerating the value of some
     "influential" lvalues. As we can enumerate only on cvalues, extracts
     the cvalue component of the value module. *)

  (* Find the value of a previously evaluated expression. *)
  let find_val valuation expr =
    match Valuation.find valuation expr with
    | `Value record -> record.value.v
    | `Top -> assert false (* [expr] must have been evaluated already. *)

  let find_loc valuation lval =
    match Valuation.find_loc valuation lval with
    | `Value record -> record.loc
    | `Top -> assert false

  (* We want to enumerate on imprecise but non-completely imprecise Cvalues. *)
  let is_enumerable value =
    not (Cvalue.V.cardinal_zero_or_one value || Cvalue.V.is_imprecise value)

  (* split on a value if it has less than [upto] abstract values, or
     enumerate only on its bounds. *)
  let fold_enumerate upto op v acc =
    try
      ignore (Cvalue.V.cardinal_less_than v upto);
      Cvalue.V.fold_enum op v acc
    with Abstract_interp.Not_less_than ->
      (* Enumerate on the possible bases, then on the min and max of the
         offsets *)
      Cvalue.V.fold_i
        (fun b i acc ->
           Ival.fold_int_bounds
             (fun i acc -> op (Cvalue.V.inject b i) acc)
             i acc)
        v acc

  (* Find locations on which it is interesting to proceed by case disjunction
     to evaluate the expression: locations which are singletons (on which the
     cvalue domain can reduce) and has an enumerable value. *)
  let rec get_influential_vars valuation exp acc =
    match exp.enode with
    | Lval (host, off as lval) ->
      if Cil.typeHasQualifier "volatile" (Cil.typeOfLval lval) then `Value acc
      else
        let loc = find_loc valuation lval in
        if Cvalue.V.cardinal_zero_or_one (get_cval (Loc.to_value loc))
        then
          (* no variable in the host or in the offset can be influential. Check
             the contents of the location, on which we might want to enumerate*)
          find_val valuation exp >>-: fun contents ->
          if is_enumerable (get_cval contents)
          then exp :: acc
          else acc
        else
          (* A variable in the host or in the offset may be influential. The
             contents themselves are not influential, because we would need to
             split both on the location and by content in sync. *)
          get_vars_host valuation host acc >>- fun acc ->
          get_vars_offset valuation off acc
    | BinOp (_, e1, e2, _) ->
      get_influential_vars valuation e1 acc >>- fun acc ->
      get_influential_vars valuation e2 acc
    | UnOp (_, e, _) -> get_influential_vars valuation e acc
    | CastE (_, exp) -> get_influential_vars valuation exp acc
    | _ -> `Value acc

  and get_vars_host valuation host acc = match host with
    | Var _v -> `Value acc
    | Mem e -> get_influential_vars valuation e acc

  and get_vars_offset valuation offset acc = match offset with
    | NoOffset         -> `Value acc
    | Field (_, off)   -> get_vars_offset valuation off acc
    | Index (ind, off) ->
      get_influential_vars valuation ind acc >>- fun acc ->
      get_vars_offset valuation off acc

  let get_influential_exprs valuation expr =
    get_influential_vars valuation expr []

  let reduce_by_cond_enumerate valuation state cond positive influentials =
    (* Test whether the condition [expr] may still be true when the
       sub-expression [e] has the value [v]. *)
    let condition_may_still_be_true valuation expr record value =
      let value = { record.value with v = `Value value } in
      let valuation = Valuation.add valuation expr { record with value } in
      let eval, _alarms = Eva.evaluate ~valuation ~fuel:0 state cond in
      match eval with
      | `Bottom -> false
      | `Value (_valuation, value) ->
        let v = get_cval value in
        if positive
        then Cvalue.V.contains_non_zero v
        else if Value_parameters.UndefinedPointerComparisonPropagateAll.get ()
        then Cvalue.V.contains_zero v
        else Cvalue.V.is_included Cvalue.V.singleton_zero v
    in
    let enumerate subexpr =
      match Valuation.find valuation subexpr with
      | `Top -> `Value valuation
      | `Value record ->
        record.value.v >>- fun v ->
        let cleared_valuation =
          Clear.clear_englobing_exprs valuation ~expr:cond ~subexpr
        in
        let process sub_cvalue acc =
          let subvalue = set_cval sub_cvalue v in
          if condition_may_still_be_true cleared_valuation subexpr record subvalue
          then Bottom.join Value.join (`Value subvalue) acc else acc
        in
        let cvalue = get_cval v in
        let upto = succ (Ival.get_small_cardinal ()) in
        fold_enumerate upto process cvalue `Bottom >>-: fun value ->
        if Value.equal v value
        then valuation
        else
          let reductness =
            if record.reductness = Created then Created else Reduced
          in
          let value = { record.value with v = `Value value } in
          let record = { record with value; reductness } in
          Valuation.add valuation subexpr record
    in
    match influentials with
    | [] -> `Value valuation
    | expr :: _ -> enumerate expr

  (* If the value module contains no cvalue component, this function is
     inoperative. Otherwise, it calls reduce_by_cond_enumerate with the
     value accessor for the cvalue component. *)
  let reduce_by_enumeration valuation state expr positive =
    if activated && Value_parameters.EnumerateCond.get ()
    then
      get_influential_exprs valuation expr >>- fun split_on ->
      reduce_by_cond_enumerate valuation state expr positive split_on
    else `Value valuation
end


(*
Local Variables:
compile-command: "make -C ../../../.."
End:
*)
