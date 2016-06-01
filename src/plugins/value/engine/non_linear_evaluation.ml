(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2016                                               *)
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
module Ki = Cil_datatype.Kinstr
open Cvalue
open Eval

let dkey = Value_parameters.register_category "nonlin"

let (>>=) (t, a) f = match t with
  | `Bottom  -> `Bottom, a
  | `Value t -> f t a

let (>>=:) (t, a) f = match t with
  | `Bottom  -> `Bottom, a
  | `Value t -> f t, a

let (>>-) t f = match t with
  | `Bottom  -> `Bottom
  | `Value t -> f t

let (>>-:) t f = match t with
  | `Bottom  -> `Bottom
  | `Value t -> `Value (f t)

module LvalList = Datatype.List (Cil_datatype.LvalStructEq)
module LvalHTbl = Cil_datatype.LvalStructEq.Hashtbl

module Non_linear_expressions =
  State_builder.Hashtbl (Cil_datatype.Exp.Hashtbl) (LvalList)
    (struct
      let name = "Non_linear_evaluation"
      let size = 16
      let dependencies = [ Ast.self ]
    end)

class do_non_linear_assignments = object(self)
  inherit Visitor.frama_c_inplace

  val found = LvalHTbl.create 16

  method private store_non_linear e =
    let flatten lval count acc = if count > 1 then lval :: acc else acc in
    let list = LvalHTbl.fold flatten found [] in
    List.iter
      (fun lval ->
         Value_parameters.result ~current:true ~once:true ~dkey
           "non-linear '%a', lv '%a'" Printer.pp_exp e Printer.pp_lval lval)
      list;
    LvalHTbl.clear found;
    Non_linear_expressions.replace e list;
    list

  method! vlval lv =
    let cur =
      try LvalHTbl.find found lv
      with Not_found -> 0
    in
    LvalHTbl.replace found lv (cur+1);
    Cil.DoChildren (* visit the l-values inside e.g. t[i] *)

  method! vexpr exp =
    match exp.enode with
    | Lval _ | UnOp _ | BinOp _ | CastE _ | Info _ -> Cil.DoChildren
    | _ -> (* None other expr contain a dereferenced l-value *)
      Cil.SkipChildren

  method! vtype _ = Cil.SkipChildren

  method v_full_exp exp =
    LvalHTbl.clear found;
    ignore (Visitor.visitFramacExpr (self:>Visitor.frama_c_inplace) exp);
    self#store_non_linear exp
end

let compute_non_linear exp =
  try Non_linear_expressions.find exp
  with Not_found ->
    let c = new do_non_linear_assignments in
    c#v_full_exp exp

exception Too_linear

let min_and_max_float v =
  try
    let i = V.project_ival v in
    let f1, f2 = Ival.min_and_max_float i in
    V.inject_float f1, V.inject_float f2
  with V.Not_based_on_null -> assert false

let subdiv_float ~size v =
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

let subdiv_int ~size:_ v =
  try
    let i = V.project_ival v in
    let l, h = Ival.subdiv_int i in
    V.inject_ival l, V.inject_ival h
  with V.Not_based_on_null -> assert false


(* [make_split lval loc value] makes a split function for the lval [lval]
   with location [loc] and abstract value [value].
   Raise Too_linear if the value contains pointer. *)
let make_split _lval size value =
  let size =
    if Value_parameters.AllRoundingModes.get () then 0
    else Integer.to_int size
  in
  if V.is_included value V.top_float then subdiv_float ~size
  else if V.is_included value V.top_int then subdiv_int ~size
  else raise Too_linear (* pointers *)

module Make
    (Value : Abstract_value.External)
    (Eva: Evaluation.S with type value = Value.t)
= struct

  (* Values are converted to {!Cvalue.V.t}, because those are
     currently the only values on which we can split. *)

  let get_cval = match Value.get Main_values.cvalue_key with
    | Some get -> get
    | None -> fun _ -> Cvalue.V.top

  let set_cval =
    let set = Value.set Main_values.cvalue_key in
    fun cval v -> set cval v

  (* A subdivision of the evaluation of an expression according to an lvalue [lval]
     is stored by an association list, where each disjunct of the abstract value
     for [lval] is associated to the result of the evaluation for this disjunct.
     The set of abstract values for [lval] are a partition of the initial
     abstract value computed for [lval].
     The results of the evaluations are pairs of a value and the emitted alarms.
  *)
  type result = Value.t evaluated
  type subdiv = (Cvalue.V.t * result) list

  let join_or_bottom join a b = match a with
    | `Bottom  -> `Value b
    | `Value a -> `Value (join a b)

  (* Flatten a subdivision by joining the values and the alarms.
     For the subdivided lvalue, the initial values that lead to Bottom are removed
     (but the alarms of the evaluation are kept). *)
  let flatten (list: subdiv) =
    List.fold_left
      (fun (var, value, alarms) (var', (value', alarms')) ->
         match value' with
         | `Bottom -> var, value, Alarmset.union alarms alarms'
         | `Value v -> join_or_bottom Cvalue.V.join var var',
                       join_or_bottom Value.join value v,
                       Alarmset.union alarms alarms')
      (`Bottom, `Bottom, Alarmset.none)
      list

  let rec insert_in_sorted_list cmp elt list =
    match list with
    | [] -> [elt]
    | hd :: tail ->
      if cmp elt hd > 0
      then hd :: (insert_in_sorted_list cmp elt tail)
      else elt :: list

  (* [list] must be a list representing a subdivision (an association list between
     values and the results of evaluations for them, as explained above).
     [has_better_bound] is an order over the parts of a subdivision (the elements
     of the [list].
     [subdiv_lval subdivnb list has_better_bound bound split compute] takes the
     smallest element of [list] according to [has_better_bound], [split] its
     value in two smaller values, and [compute] the result for each.
     The process is repeated [subdivnb] times, or until the smallest element of the
     [list] is greater or equal to [bound].
  *)
  let subdiv subdivnb (list: subdiv) has_better_bound bound split compute =
    let working_list = ref (List.sort has_better_bound list) in
    let subdiv_for_bound has_better_bound bound =
      let compute_subvalue subvalue list =
        let res = compute subvalue in
        insert_in_sorted_list has_better_bound (subvalue, res) list
      in
      let subdiv = function
        | [] -> assert false
        | (value, _ as v) :: tail ->
          let subvalue1, subvalue2 = split value in
          if has_better_bound v bound >= 0
          then raise Abstract_interp.Can_not_subdiv;
          let s = compute_subvalue subvalue1 tail in
          compute_subvalue subvalue2 s
      in
      try
        for _i = 1 to subdivnb do
          working_list := subdiv !working_list;
        done
      with Abstract_interp.Can_not_subdiv -> ()
    in
    subdiv_for_bound has_better_bound bound;
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
    else V.compare_min_int, V.compare_max_int
  in
  let better_bound compare_bound (_, (e1, _)) (_, (e2, _)) =
    match e1, e2 with
    | `Bottom, `Bottom -> 0
    | `Bottom, _ -> 1
    | _, `Bottom -> -1
    | `Value v1, `Value v2 -> compare_bound (get_cval v1) (get_cval v2)
  in
  better_bound compare_min, better_bound compare_max

  module Clear = Clear_Valuation (Eva.Valuation)

  (* These two functions assume that the given expression or lvalue have been
     evaluated in the valuation. *)
  let find_val valuation expr = match Eva.Valuation.find valuation expr with
    | `Value record -> record
    | `Top -> assert false

  let find_loc valuation lval = match Eva.Valuation.find_loc valuation lval with
    | `Value record -> record
    | `Top -> assert false

  (* Subdivision of the evaluation of the expression [expr], according to the
     lvalue [lval], in the state [state].
     [valuation] is the cache resulting from a previous evaluation of [expr]
     without subdivision,
     [value] and [alarms] are the result of the evaluation of [expr]. *)
  let subdiv_lval ~indeterminate subdivnb state valuation expr value alarms lval =
    (* Abstract value of [lval]. *)
    let lv_exp = Cil.new_exp ~loc:expr.Cil_types.eloc (Cil_types.Lval lval) in
    let lv_record = find_val valuation lv_exp in
    match lv_record.value.v with
    | `Bottom -> raise Too_linear
    | `Value lv_value ->
      let lv_cval = get_cval lv_value in
      (* Split function for this abstract value. *)
      let record = find_loc valuation lval in
      (* The size is defined, as [lv] is a scalar *)
      let size = Int_Base.project (Eval_typ.sizeof_lval_typ record.typ) in
      let split = make_split lval size lv_cval in
      let cleared_valuation = Clear.clear_expr valuation lv_exp in
      (* Computes the value of [expr] when [lval] has the value [subvalue]. *)
      let compute sub_cval =
        let subvalue = set_cval sub_cval lv_value in
        let value  = { lv_record.value with v = `Value subvalue } in
        let record = { lv_record with value = value } in
        let valuation = Eva.Valuation.(add cleared_valuation lv_exp record) in
        Eva.evaluate ~valuation ~indeterminate state expr
        >>=: fun (_valuation, value) ->
        `Value value
      in
      (* Evaluation for the bounds of [lv_value]. *)
      let bound1, bound2 =
        if V.is_included lv_cval V.top_float then min_and_max_float lv_cval
        else if V.is_included lv_cval V.top_int then min_and_max_int lv_cval
        else raise Too_linear (* pointers *)
      in
      (* As [bound1] and [bound2] are singleton included in [lv_value],
         we cannot obtain smaller parts of the subdivision of the evaluation
         according to [lval]. Thus, [r1] and [r2] will be our limits for the
         subdivision. *)
      let r1 = let res1, alarms1 = compute bound1 in bound1, (res1, alarms1)
      and r2 = let res2, alarms2 = compute bound2 in bound2, (res2, alarms2) in

      (* The initial subdivision, with one disjunct. *)
      let subdiv_list = [ lv_cval, (`Value value, alarms) ] in
      let has_better_min_bound, has_better_max_bound = better_bound value in
      (* Subdivision to reduce the infimum of the result value. *)
      let min_bound = if has_better_min_bound r2 r1 > 0 then r1 else r2 in
      let subdiv_list =
        subdiv subdivnb subdiv_list has_better_min_bound min_bound split compute
      in
      (* Subdivision to reduce the supremum of the result value. *)
      let max_bound = if has_better_max_bound r2 r1 > 0 then r1 else r2 in
      let subdiv_list =
        subdiv subdivnb subdiv_list has_better_max_bound max_bound split compute
      in
      (* Results of the subdivision *)
      let reduced_var, result_value, alarms = flatten subdiv_list in
      (* If [reduced_var] and [result_value] are not bottom,
         then reduce the valuation by their new value records. *)
      let eval =
        reduced_var >>- fun reduced_var ->
        result_value >>-: fun result_value ->
        let v = set_cval reduced_var lv_value in
        let valuation =
          Eva.Valuation.add valuation lv_exp
            {lv_record with value = { lv_record.value with v = `Value v };
                            reductness = Reduced}
        in
        let record = find_val valuation expr in
        let record = { record with value = { record.value with v = `Value result_value };
                                   val_alarms = alarms } in
        let valuation = Eva.Valuation.add valuation expr record in
        valuation, result_value
      in
      eval, alarms

  (* Evaluation of [expr] in state [state],
     with at most (2 * [subdivnb]) subdivisions.*)
  let subdivides_evaluation ~indeterminate subdivnb valuation state expr =
    (* Evaluation of [expr] without subdivision. *)
    let default = Eva.evaluate ~valuation ~indeterminate state expr in
    default >>= fun (valuation, value) alarms ->
    if not (Value.is_included value Value.top_int) then begin
      Value_parameters.debug ~level:2
        "subdivfloatvar: expression evaluates to an address";
      default
    end
    else
      (* List of lvalues that appear multiple times in [expr],
         candidates for the subdivision. *)
      let vars = List.rev (compute_non_linear expr) in
      (* Comparison function between disjuncts. *)
      let rec try_sub vars =
        match vars with
        | [] -> default
        | lval :: tail ->
          try subdiv_lval ~indeterminate subdivnb state valuation expr value alarms lval
          with Too_linear -> try_sub tail
      in
      try_sub vars

  let evaluate
      ?(valuation=Eva.Valuation.empty) ?(indeterminate=false) ?(reduction=true)
      state expr =
    let subdivnb = Value_parameters.LinearLevel.get () in
    if subdivnb = 0 || not reduction || not (Value.mem Main_values.cvalue_key)
    then
      Eva.evaluate ~valuation ~indeterminate ~reduction state expr
    else
      subdivides_evaluation ~indeterminate subdivnb valuation state expr

end


(*
Local Variables:
compile-command: "make -C ../../../.."
End:
*)
