(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2018                                               *)
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
open Eval

let dkey = Value_parameters.register_category "nonlin"

(* ----------------- Occurrences of lvalues in expressions ------------------ *)

module LvalMap = Cil_datatype.LvalStructEq.Map
module LvalSet = Cil_datatype.LvalStructEq.Set

(* An expression [e] is non-linear on [x] if [x] appears multiple times in [e].
   When evaluating such an expression, a disjunction over the possible values of
   [x] may gain some precision with respect to the interval semantics.

   An expression can be non-linear on several variables. On expressions such as
   [x*x+y*y] or [(x+x)*y + y], we want to subdivide on x and on y separately
   (and for the second expression, we want to subdivide on x before y).
   For expressions such as [x*x - 2*x*y + y*y], we want to subdivide on x and y
   simultaneously.

   When evaluating an expression [e] by subdividing the values of a list of
   lvalues [lvals]:
   - we evaluate the entire expression [e], and removes the values that lead
     to bottom from the possible values for [lvals];
   - we join the result of all subdivisions, and we reduce accordingly the
     possible values for [e] and for the smallest subexpression that contains
     all occurrences of [lvals]. Indeed, the more precise value computed for [e]
     could be erased by a subdivision on others variables: when [e = x*x + y*y],
     the subdivision on [y] overwrites the value of [e] computed by subdivision
     on [x], but not the value of [x*x]. *)

(* To detect non-linearity in an expression [expr], we browse [expr] by
   computing a maps that link any lvalue [lval] in [expr] to:
   - a subexpression of [expr] that contains all occurrences of [lval];
   - the depth of the subexpression in [expr]; we want to subdivide first on
     lvalues whose depth is higher.
   - a set of all lvalues [x] such that [lval] appears in the subexpression that
     contains all occurrences of [x]. If the inverse is also true, we want to
     subdivide on [x] and [lval] simultaneously. *)

(* Union of two maps (see above) for the expression [expr] of depth [depth].
   Both maps have been computed for the direct sub-expressions of [expr]. *)
let union expr depth map1 map2 =
  (* Lvalues for which [expr] is the new subexpression (see above). *)
  let top = ref LvalSet.empty in
  (* Lvalues such that a lvalue from [!top] appears in their subexpression. *)
  let deps = ref LvalSet.empty in
  let merge lval a b = match a, b with
    | None, None -> None
    | Some x, None
    | None, Some x -> Some x
    | Some (_, _, deps1), Some (_, _, deps2) ->
      top := LvalSet.add lval !top;
      deps := LvalSet.union (LvalSet.union deps1 deps2) !deps;
      Some (expr, depth, LvalSet.union deps1 deps2)
  in
  let map = LvalMap.merge merge map1 map2 in
  LvalMap.mapi
    (fun lval (e, d, lvs) ->
       (* Alls lvalues in [expr] now appear in the subexpression of [!top]. *)
       let lvs = LvalSet.union lvs !top in
       (* Lvalues in [!deps] should be subdivided with the lvalues in [!top]. *)
       if LvalSet.mem lval !deps then (expr, depth, lvs) else (e, d, lvs))
    map

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
      let map1 = compute_from_offset d expr offset in
      let map2 = compute_from_host d host in
      let map = union expr depth map1 map2 in
      if LvalMap.is_empty map && Cil.isArithmeticType (Cil.typeOfLval lv)
      then LvalMap.singleton lv (expr, d, LvalSet.empty)
      else map
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
  let fill lval (expr, depth, _) acc =
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

(* ------------------------ Operations on cvalues --------------------------- *)

let can_be_subdivided cvalue =
  Cvalue.V.is_arithmetic cvalue && not (Cvalue.V.cardinal_zero_or_one cvalue)

(* All the functions below assume that their arguments are arithmetic
   (no pointer values). *)

(* The [bounds_] function are used to stop the subdivision as an optimization.
   They must return singleton values, but not necessarily the bounds of the
   argument, in case this is not really possible. *)

let bounds_float v =
  try
    let i = Cvalue.V.project_ival v in
    match Ival.min_and_max_float i with
    | None, _ -> let v = Cvalue.V.inject_float Fval.nan in v, v
    | Some (f1, f2), _ ->
      Cvalue.V.inject_float (Fval.inject_singleton f1),
      Cvalue.V.inject_float (Fval.inject_singleton f2)
  with Cvalue.V.Not_based_on_null -> assert false

let bounds_int v =
  try
    let i = Cvalue.V.project_ival v in
    match Ival.min_and_max i with
    | None, None -> let v = Cvalue.V.inject_int Integer.zero in v, v
    | Some i, None | None, Some i -> let v = Cvalue.V.inject_int i in v, v
    | Some i1, Some i2 -> Cvalue.V.inject_int i1, Cvalue.V.inject_int i2
  with Cvalue.V.Not_based_on_null -> assert false

let bounds_cvalue cvalue =
  if Cvalue.V.(is_included cvalue top_float) then bounds_float cvalue
  else if Cvalue.V.(is_included cvalue top_int) then bounds_int cvalue
  else assert false (* pointers *)

let compare_bound ival_compare_bound v1 v2 =
  if v1 == v2 then 0
  else if Cvalue.V.is_bottom v2 then -1
  else if Cvalue.V.is_bottom v1 then 1
  else
    try
      let f1 = Cvalue.V.project_ival v1 in
      let f2 = Cvalue.V.project_ival v2 in
      ival_compare_bound f1 f2
    with Cvalue.V.Not_based_on_null -> assert false

let has_greater_min_bound = compare_bound Ival.has_greater_min_bound
let has_smaller_max_bound = compare_bound Ival.has_smaller_max_bound

let subdivide size cvalue =
  try
    let ival = Cvalue.V.project_ival cvalue in
    let ival1, ival2 = Ival.subdivide size ival in
    Cvalue.V.inject_ival ival1, Cvalue.V.inject_ival ival2
  with Cvalue.V.Not_based_on_null -> assert false

(* -------------------------- Length indexed lists -------------------------- *)

(* We subdivide the evaluation of an expression by splitting the possible values
   of a list of lvalues. Each disjunct of a subdivision contains its hypothesis,
   that is the list of values asssumed for these lvalues.
   For a given subdivison, all these hypothesis lists should have the same
   length (the number of lvalues on which the subdivision is done). We ensure
   this invariant through length indexed lists, by using GADT. *)
module Hypotheses = struct
  type zero
  type 'a succ = S
  let _ = S
  type ('a, 'length) llist =
    | Nil: ('a, zero) llist
    | Cons: 'a * ('a, 'b) llist -> ('a, 'b succ) llist

  let rec fold : type l. ('a -> 'b -> 'b) -> ('a, l) llist -> 'b -> 'b =
    fun f list acc ->
      match list with
      | Nil -> acc
      | Cons (x, tl) -> fold f tl (f x acc)

  let rec fold2:
    type l. ('a -> 'b -> 'c -> 'c) -> ('a, l) llist -> ('b, l) llist -> 'c -> 'c
    = fun f l1 l2 acc ->
      match l1, l2 with
      | Nil, Nil -> acc
      | Cons (x1, tl1), Cons (x2, tl2) -> fold2 f tl1 tl2 (f x1 x2 acc)

  let rec map: type l. ('a -> 'b) -> ('a, l) llist -> ('b, l) llist =
    fun f -> function
      | Nil -> Nil
      | Cons (x, tl) -> Cons (f x, map f tl)

  type 'a l = L: ('a, 'l) llist -> 'a l

  let from_list list =
    List.fold_left (fun (L acc) elt -> L (Cons (elt, acc))) (L Nil) list

  (* The hypothesis of a disjunct is a list of cvalues for the subdivided
     lvalues.*)
  type 'length subvalues = (Cvalue.V.t, 'length) llist

  (* Makes a list of bottom cvalues. *)
  let rec bottom: type l. l subvalues -> l subvalues = function
    | Nil -> Nil
    | Cons (_, tl) -> Cons (Cvalue.V.bottom, bottom tl)

  (* Pointwise comparison of two lists of subvalues. *)
  let rec compare_subvalues: type l. l subvalues -> l subvalues -> int =
    fun hyp1 hyp2 ->
      match hyp1, hyp2 with
      | Nil, Nil -> 0
      | Cons (v1, tail1), Cons (v2, tail2) ->
        let n = Cvalue.V.compare v1 v2 in
        if n = 0 then compare_subvalues tail1 tail2 else n

  (* Pointwise join of two lists of subvalues. *)
  let rec join_subvalues: type l. l subvalues -> l subvalues -> l subvalues =
    fun l1 l2 ->
      match l1, l2 with
      | Nil, Nil -> Nil
      | Cons (x1, tl1), Cons (x2, tl2) ->
        Cons (Cvalue.V.join x1 x2, join_subvalues tl1 tl2)

  (* Extract the extremum of each subvalue of a list. Returns a list of lists
     of values (one for each combination of extremum of the initial values). *)
  let rec bound_subvalues: type l. l subvalues -> l subvalues list =
    function
    | Nil -> [ Nil ]
    | Cons (cvalue, tl) ->
      let min, max = bounds_cvalue cvalue in
      let list = bound_subvalues tl in
      let prepend acc tl = Cons (min, tl) :: Cons (max, tl) :: acc in
      List.fold_left prepend [] list

  (* Are all cvalues in the list a singleton? *)
  let rec cardinal_zero_or_one: type l. l subvalues -> bool = function
    | Nil -> true
    | Cons (cvalue, tl) ->
      Cvalue.V.cardinal_zero_or_one cvalue && cardinal_zero_or_one tl

  (* Split each subvalue of a list. Returns a list of subvalues lists.
     If n subvalues of the initial list have been split, the returned list
     contains 2^n lists of subvalues.
     Raises Can_not_subdiv if no subvalue of the initial list can be split. *)
  let split sizes subvalues =
    let rec split:
      type l. (Integer.t, l) llist -> l subvalues -> l subvalues list =
      fun sizes subvalues ->
        match sizes, subvalues with
        | Nil, Nil -> [ Nil ]
        | Cons (size, tl_size), Cons (v, tl) ->
          let append =
            try
              let v1, v2 = subdivide size v in
              fun acc tl -> Cons (v1, tl) :: Cons (v2, tl) :: acc
            (* Other subvalues could be split; keep this initial value. *)
            with Abstract_interp.Can_not_subdiv ->
            fun acc tl -> Cons (v, tl) :: acc
          in
          let list = split tl_size tl in
          List.fold_left append [] list
    in
    let list = split sizes subvalues in
    (* If the final list contains only one element, no value has been split. *)
    if List.length list <= 1
    then raise Abstract_interp.Can_not_subdiv
    else list
end

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

  module Clear = Clear_Valuation (Valuation)

  (* These two functions assume that the given expression or lvalue have been
     evaluated in the valuation. *)
  let find_val valuation expr = match Valuation.find valuation expr with
    | `Value record -> record
    | `Top -> assert false

  let find_loc valuation lval = match Valuation.find_loc valuation lval with
    | `Value record -> record
    | `Top -> assert false

  (* --------------------- Types for the subdivision ------------------------ *)

  (* Information about a subdivided lvalue: the lvalue expression, its record
     and initial value from a first evaluation. These are needed to update the
     lvalue after the subdivision. *)
  type lval_info =
    { lval: lval;
      lv_expr: exp;
      lv_record: (Value.t, Valuation.origin) record_val;
      lv_value: Value.t }

  (* A length indexed list of lvalues on which the subdivision is done. *)
  type 'l sub_lvals = (lval_info, 'l) Hypotheses.llist

  (* A length indexed list of sub-values for the lvalues on which the
     subdivision is done. *)
  type 'l subvalues = 'l Hypotheses.subvalues

  (* A disjunct of a subdivision:
     - the hypotheses of the disjunct are the subvalues used for the considered
       lvalues to perform the evaluation;
     - the result and alarms resulting from the evaluation with these subvalues;
     - the subresult is the value of the subexpression containing all
       occurrences of the considered lvalues. *)
  type 'l disjunct =
    { hypotheses: 'l subvalues;
      result: Value.t or_bottom;
      alarms: Alarmset.t;
      subresult: Value.t or_bottom; }

  (* Type of the function that splits subvalues into a partition of smaller
     subvalues. *)
  type 'l split = 'l subvalues -> 'l subvalues list

  (* Type of the function that computes a disjunct from subvalues. *)
  type 'l compute = 'l subvalues -> 'l disjunct

  (* A subdivision of the evaluation is stored by a working list of disjuncts,
     implemented as a heap.
     The order of the heap is chosen so that the head is always the most
     relevant disjunct to subdivide next, i.e. the disjunct with the most
     imprecise result.
     The set of subvalues for the considered lvalues must be a partition of the
     initial abstract value initially computed for these lvalues. *)
  module Subdivision = struct
    (* We use this reference because we need to change the "direction" of
       the comparison function, but it is convenient to have the same
       module in both cases. *)
    let cmp_result = ref (fun _ _ -> 0)

    (* If the results are equal, the relative order between disjuncts is
       unimportant. We just need an ordering function that does not make equal
       disjuncts with equal images but different subvalues. The current order
       has been chosen to mimic the results of the previous implementations. *)
    let compare disjunct1 disjunct2 =
      let n = !cmp_result disjunct1.result disjunct2.result in
      if n <> 0 then n else
        let n = !cmp_result disjunct1.subresult disjunct2.subresult in
        if n <> 0 then n else
          Hypotheses.compare_subvalues disjunct2.hypotheses disjunct1.hypotheses

    type 'l t = E | T of int * 'l disjunct * 'l t * 'l t

    let rank = function E -> 0 | T (r, _, _, _) -> r

    let make x a b =
      let ra = rank a and rb = rank b in
      if ra >= rb then T (rb + 1, x, a, b) else T (ra + 1, x, b, a)

    let rec merge h1 h2 = match h1, h2 with
      | E, h | h, E -> h
      | T (_, x, a1, b1), T (_, y, a2, b2) ->
        if compare x y <= 0
        then make x a1 (merge b1 h2)
        else make y a2 (merge h1 b2)

    let insert x h = merge (T (1, x, E, E)) h
    let singleton x = insert x E

    exception Empty

    let min = function E -> raise Empty | T (_,x,_,_) -> x

    let extract_min = function
      | E -> raise Empty
      | T (_, x, a, b) -> x, merge a b

    let rec fold f h acc = match h with
      | E -> acc
      | T (_, x, h1, h2) -> fold f h2 (f x (fold f h1 acc))

    (* Used to reorder the elements of the heap when the comparison function has
       changed. *)
    let reorder t = fold (fun elt t -> insert elt t) t E
  end

  (* ------------------------------ Subdivision ----------------------------- *)

  (* Makes the split function for a list of lvalues. The split function depends
     on the size of each lvalue, computed from their type.  *)
  let make_split valuation (lvals: 'l sub_lvals) : 'l split =
    let compute_size info =
      (* The size is defined, as [lv] is a scalar *)
      let record = find_loc valuation info.lval in
      Int_Base.project (Eval_typ.sizeof_lval_typ record.typ)
    in
    let sizes = Hypotheses.map compute_size lvals in
    Hypotheses.split sizes

  (* Joins all the disjuncts of a subdivision into one disjunct. Used to
     consolidate the results of a subdivision, before updating the valuation
     with the new values.
     For the subdivided lvalues, do not include the subvalues that led to
     bottom. *)
  let flatten (subdivision: 'l Subdivision.t) : 'l disjunct =
    let join disjunct acc =
      let hypotheses =
        if disjunct.result = `Bottom
        then acc.hypotheses
        else Hypotheses.join_subvalues acc.hypotheses disjunct.hypotheses
      in
      { hypotheses;
        result = Bottom.join Value.join acc.result disjunct.result;
        alarms = Alarmset.union acc.alarms disjunct.alarms;
        subresult = Bottom.join Value.join acc.subresult disjunct.subresult }
    in
    let disjunct, subdivision = Subdivision.extract_min subdivision in
    let disjunct =
      if disjunct.result = `Bottom
      then { disjunct with hypotheses = Hypotheses.bottom disjunct.hypotheses }
      else disjunct
    in
    Subdivision.fold join subdivision disjunct

  (* Updates the valuation with the new subvalues for the lvalues on which
     the subdivision is performed. *)
  let update_variables valuation (lvals: 'l sub_lvals) (subvalues: 'l subvalues) =
    let update lv_info subvalue valuation =
      let value = set_cval subvalue lv_info.lv_value in
      let value = { lv_info.lv_record.value with v = `Value value } in
      let record = { lv_info.lv_record with value; reductness = Reduced } in
      Valuation.add valuation lv_info.lv_expr record
    in
    Hypotheses.fold2 update lvals subvalues valuation

  (* Reduces the values of the lvalues on which the subdivision has been done. *)
  let reduce_variables valuation (lvals: 'l sub_lvals) (subvalues: 'l subvalues) =
    let update lv_info subvalue valuation =
      let value =
        if Cvalue.V.is_bottom subvalue
        then `Bottom
        else `Value (set_cval subvalue Value.top)
      in
      (* Narrow the new value with the old value, that could have been reduced
         during the first forward evaluation. *)
      let record = find_val valuation lv_info.lv_expr in
      let value = Bottom.narrow Value.narrow value record.value.v in
      let value = { record.value with v = value } in
      let record = { record with value; reductness = Reduced } in
      Valuation.add valuation lv_info.lv_expr record
    in
    Hypotheses.fold2 update lvals subvalues valuation

  (* Updates the valuation with the final value (and alarms if provided)
     computed for an expression during the subdivision. *)
  let update_expr valuation expr ?alarms value =
    let record = find_val valuation expr in
    let value = { record.value with v = value } in
    let record = { record with value; reductness = Reduced } in
    let record = match alarms with
      | None -> record
      | Some val_alarms -> { record with val_alarms }
    in
    Valuation.add valuation expr record

  (* Computes the image of the bounds of [subvalues]. Since those bounds
     are singletons, their image must be in the result of the subdivision. *)
  let compute_sure_bounds (compute: 'l compute) (subvalues: 'l subvalues) =
    let singleton_subvalues = Hypotheses.bound_subvalues subvalues in
    let compute_result subvalues = (compute subvalues).result in
    let results = List.map compute_result singleton_subvalues in
    List.fold_left (Bottom.join Value.join) `Bottom results

  (* Performs a subdivision by applying successively [split] and [compute] on
     the initial [subdivision].
     [has_better_bound] defines the order of the subdivision. At each step, this
     function takes the smallest disjunct of the current subdivision according
     to [has_better_bound], then [split] its subvalues into smaller subvalues,
     and [compute] the disjuncts for each.
     The process is repeated [subdivnb] times, or until we detect no more
     improvement is possible. Note that [compute] is applied to each subvalues
     produced by [split]. If split produces [n] subvalues each time, then
     [compute] is applied [subdivnb * n] times. *)
  let do_subdiv subdivnb subdivision has_better_bound split compute =
    Subdivision.cmp_result := has_better_bound;
    let working_list = ref (Subdivision.reorder subdivision) in
    let min = Subdivision.min !working_list in
    (* We will never be able to improve further than these bounds. *)
    let bound = ref (compute_sure_bounds compute min.hypotheses) in
    (* Computes the image of [subvalues], and insert it in [subdivision] for
       further refinement. If [subvalues] is a singleton, also refines our
       criterion for stopping. *)
    let compute_disjunct subdivision subvalues =
      let disjunct = compute subvalues in
      if Hypotheses.cardinal_zero_or_one subvalues then
        bound := Bottom.join Value.join disjunct.result !bound;
      Subdivision.insert disjunct subdivision;
    in
    try
      for _i = 1 to subdivnb do
        let disjunct, subdiv = Subdivision.extract_min !working_list in
        if has_better_bound disjunct.result !bound >= 0 then
          (* The bound of this disjunct result is already better than [!bound],
             which must be in the final result. Thus, there is no point in
             subdividing [disjunct]. And since [subdivision] is sorted, all the
             other subdivisions also have a better bound. Thus, we stop. *)
          raise Abstract_interp.Can_not_subdiv;
        let subvalues_list = split disjunct.hypotheses in
        let subdiv = List.fold_left compute_disjunct subdiv subvalues_list in
        working_list := subdiv;
      done;
      !working_list
    with Abstract_interp.Can_not_subdiv -> !working_list

  let better_bound compare_bound e1 e2 =
    match e1, e2 with
    | `Bottom, `Bottom -> 0
    | `Bottom, _ -> 1
    | _, `Bottom -> -1
    | `Value v1, `Value v2 -> compare_bound (get_cval v1) (get_cval v2)

  (* These function make orders over the disjuncts of a subdivision.
     They respectively try to reduce the infimum and the supremum of the
     resulting value of the evaluation. *)
  let better_min_bound = better_bound has_greater_min_bound
  let better_max_bound = better_bound has_smaller_max_bound

  (* Subdivision of the evaluation of the expression [expr], according to the
     values of a list of lvalues [lvals], in the state [state].
     [subexpr] is the smallest subexpression of [expr] containing all
     occurrences of the lvalues in [lvals]. At the end of the subdivision, we
     reduce the final value of [expr], [subexpr], and of the lvalues in [lvals].
     [valuation] is the result of the evaluation of [expr] without subdivision.
     This function returns the alarms and the valuation resulting from the
     subdivided evaluation. *)
  let subdivide_lvals ~fuel subdivnb valuation state expr subexpr lvals =
    let Hypotheses.L variables = Hypotheses.from_list lvals in
    (* Split function for the subvalues of [lvals]. *)
    let split = make_split valuation variables in
    (* Clear the valuation to force the evaluation on top of [lvals]. *)
    let clear lv_info valuation =
      Clear.clear_englobing_exprs valuation ~expr ~subexpr:lv_info.lv_expr
    in
    let cleared_valuation = Hypotheses.fold clear variables valuation in
    let eq_equal_subexpr = Cil_datatype.ExpStructEq.equal expr subexpr in
    (* Computes a disjunct from subvalues for [lvals]. *)
    let compute subvalues =
      (* Updates [variables] with their new [subvalues]. *)
      let valuation = update_variables cleared_valuation variables subvalues in
      (* Evaluates [expr] with this new valuation. *)
      let eval, alarms = Eva.evaluate ~fuel ~valuation state expr in
      let result = eval >>-: snd in
      (* Optimization if [subexpr] = [expr]. *)
      if eq_equal_subexpr
      then { hypotheses = subvalues; result; alarms; subresult = result }
      else
        (* Retrieve the value of [subexpr] from the valuation. *)
        let subresult =
          eval >>- fun (valuation, _) ->
          let record = find_val valuation subexpr in
          record.value.v
        in
        { hypotheses = subvalues; result; alarms; subresult }
    in
    let initial_cvalues =
      Hypotheses.map (fun info -> get_cval info.lv_value) variables
    in
    let initial_disjunct = compute initial_cvalues in
    (* The initial subdivision, with only one disjunct. *)
    let subdiv = Subdivision.singleton initial_disjunct in
    (* Subdivision to reduce the infimum of the result value. *)
    let subdiv = do_subdiv subdivnb subdiv better_min_bound split compute in
    (* Subdivision to reduce the supremum of the result value. *)
    let subdiv = do_subdiv subdivnb subdiv better_max_bound split compute in
    (* Join the resulting subdivision. *)
    let disjunct = flatten subdiv in
    let alarms = disjunct.alarms in
    (* Updates the initial valuation with the new values for [lvals], [expr]
       and [subexpr]. *)
    let eval_result =
      disjunct.result >>-: fun value ->
      let valuation = reduce_variables valuation variables disjunct.hypotheses in
      let valuation = update_expr valuation expr ~alarms disjunct.result in
      let valuation =
        if eq_equal_subexpr
        then valuation
        else update_expr valuation subexpr disjunct.subresult
      in
      valuation, value
    in
    eval_result, alarms

  (* Builds the information for an lvalue. *)
  let get_info ~fuel valuation state lval =
    let lv_expr = Value_util.lval_to_exp lval in
    (* Reevaluates the lvalue in the initial state, as its value could have
       been reduced in the evaluation of the complete expression, and we cannot
       omit the alarms for the removed values. *)
    fst (Eva.evaluate ~fuel ~valuation state lv_expr) >>- fun (valuation, _) ->
    let lv_record = find_val valuation lv_expr in
    lv_record.value.v >>-: fun lv_value ->
    { lval; lv_expr; lv_record; lv_value }

  (* Makes a list of lvalue information from a list of lvalues. Removes lvalues
     whose cvalue is singleton or contains addresses, as we cannot subdivide on
     such values. *)
  let make_info_list ~fuel valuation state lvals =
    let get_info = get_info ~fuel valuation state in
    let get_info acc lval = Bottom.add_to_list (get_info lval) acc in
    let list = List.fold_left get_info [] lvals in
    List.filter (fun info -> can_be_subdivided (get_cval info.lv_value)) list

  (* Before any subdivision, a first evaluation is needed. If it leads to
     bottom, then returns bottom and the alarms. Otherwise, do the subdivision
     and only return its result, which is sound and may be more precise than the
     previous evaluation. *)
  let (>>>) (t, alarms) f = match t with
    | `Bottom  -> `Bottom, alarms
    | `Value (valuation, result) -> f valuation result alarms

  (* Subdivided evaluation of [expr] in state [state]. *)
  let subdivide_evaluation ~fuel subdivnb initial_valuation state expr =
    (* Evaluation of [expr] without subdivision. *)
    let default = Eva.evaluate ~fuel ~valuation:initial_valuation state expr in
    default >>> fun valuation result alarms ->
    (* Do not try to subdivide if the result is singleton or contains some
       pointers: the better_bound heuristic only works on numerical values. *)
    if not (can_be_subdivided (get_cval result))
    then default
    else
      (* List of non-linear subexpressions [subexpr], with the lvalues that
         appear multiple times in [subexpr], candidates for the subdivision. *)
      let vars = compute_non_linear expr in
      let rec subdivide_subexpr vars valuation result alarms =
        match vars with
        | [] -> `Value (valuation, result), alarms
        | (subexpr, lvals) :: tail ->
          (* Retrieve necessary information about the lvalues.
             Also remove lvalues with pointer or singleton values. *)
          let lvals_info = make_info_list ~fuel initial_valuation state lvals in
          match lvals_info with
          | [] -> subdivide_subexpr tail valuation result alarms
          | _ ->
            let lvals = List.map (fun info -> info.lval) lvals_info in
            let nb = List.length lvals in
            (* When subdividing on [nb] variables, each split produces 2^n
               subvalues that are all evaluated. Limits the number of splits to
               keep the number of evaluations linear on [nb]. *)
            let subdivnb =
              if nb > 3
              then
                let pow = Integer.power_int_positive_int in
                (subdivnb * nb) / (Integer.to_int (pow 2 (nb - 1)))
              else subdivnb
            in
            Value_parameters.result ~current:true ~once:true ~dkey
              "subdividing on %a"
              (Pretty_utils.pp_list ~sep:", " Printer.pp_lval) lvals;
            subdivide_lvals ~fuel subdivnb valuation state expr subexpr lvals_info
            >>> subdivide_subexpr tail
      in
      subdivide_subexpr vars valuation result alarms

  let evaluate ?(valuation=Valuation.empty) ~fuel state expr =
    let subdivnb = Value_parameters.LinearLevel.get () in
    if subdivnb = 0 || not activated
    then Eva.evaluate ~valuation ~fuel state expr
    else subdivide_evaluation ~fuel subdivnb valuation state expr


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

  (* We want to enumerate on imprecise but non-completely imprecise cvalues:
     reject singleton values, garbled mixes, and values pointing to too many
     bases. *)
  let is_enumerable value =
    not (Cvalue.V.cardinal_zero_or_one value || Cvalue.V.is_imprecise value) &&
    let bases_number = Cvalue.V.fold_bases (fun _ acc -> acc + 1) value 0 in
    bases_number < 5 (* arbitrary limit *)

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
