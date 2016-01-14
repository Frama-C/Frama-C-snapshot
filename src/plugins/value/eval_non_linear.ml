(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2015                                               *)
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
open Cil
open Locations
module Ki = Cil_datatype.Kinstr
open Abstract_interp
open Cvalue

let dkey = Value_parameters.register_category "nonlin"

(* (lval * location option) list *)
module LvalLocOpt_list =
  Datatype.List (Datatype.Pair(Cil_datatype.LvalStructEq)
                              (Datatype.Option(Location)))

module Non_linear_expressions =
  State_builder.Hashtbl(Cil_datatype.Exp.Hashtbl)(LvalLocOpt_list)
    (struct
      let name = "Eval_non_linear"
      let size = 16
      let dependencies = [ Ast.self ]
    end)

class do_non_linear_assignments = object(self)
  inherit Visitor.frama_c_inplace

  val found = Cil_datatype.LvalStructEq.Hashtbl.create 16

  method private store_non_linear e =
    let res = ref [] in
    let with_alarms = CilE.warn_none_mode in
    let aux lv count =
      (* Format.printf "LV %a %d@." Printer.pp_lval lv count; *)
      if count > 1 then
        let loc = Eval_exprs.lval_to_loc ~with_alarms Model.top lv in
        let loc =
          if Location_Bits.(equal top loc.loc) then None else Some loc
        in
        Value_parameters.result ~current:true ~once:true ~dkey
          "non-linear '%a', lv '%a'" Printer.pp_exp e Printer.pp_lval lv;
        res := (lv, loc) :: !res in
    Cil_datatype.LvalStructEq.Hashtbl.iter aux found;
    Cil_datatype.LvalStructEq.Hashtbl.clear found;
    Non_linear_expressions.replace e !res

  method! vstmt s =
    match s.skind with
    | Instr (Set (_lv,exp,_)) ->
      (* Currently, we do a special treatment for non-linear expressions
         only in this case *)
      self#v_full_exp exp;
      SkipChildren
    | _ -> SkipChildren

  method! vlval lv =
    let cur =
      try Cil_datatype.LvalStructEq.Hashtbl.find found lv 
      with Not_found -> 0
    in
    Cil_datatype.LvalStructEq.Hashtbl.replace found lv (cur+1);
    DoChildren (* visit the l-values inside e.g. t[i] *)

  method! vexpr exp =
    match exp.enode with
    | Lval _ | UnOp _ | BinOp _ | CastE _ | Info _ -> Cil.DoChildren
    | _ -> (* None other expr contain a dereferenced l-value *)
      Cil.SkipChildren

  method v_full_exp exp =
    Cil_datatype.LvalStructEq.Hashtbl.clear found;
    ignore (Visitor.visitFramacExpr (self:>Visitor.frama_c_inplace) exp);
    self#store_non_linear exp

  method! vtype _ = SkipChildren

end

let compute_non_linear exp =
  try Non_linear_expressions.find exp
  with Not_found ->
    let c = new do_non_linear_assignments in
    c#v_full_exp exp;
    Non_linear_expressions.find exp

exception Too_linear

(* Functions used to split an abstract value of a given 'type' (float or
   integer.) *)
type split = {
  min_and_max: V.t -> V.t * V.t (* extrema of an abstract value *);
  subdiv: size:int -> V.t -> V.t * V.t (* split an abstract value in two *);
}

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

(* Splitting over floating-point values *)
let split_float = { min_and_max = min_and_max_float; subdiv = subdiv_float; }

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

(* Splitting over integer values *)
let split_int = { min_and_max = min_and_max_int; subdiv = subdiv_int }

let eval_expr_with_deps_state_subdiv subdivnb ~with_alarms deps state e =
  (* We are going to proceed by disjunction. Avoid using the supplied
     ~with_alarms whenever possible, as splitting more might allow
     avoiding the alarm *)
  let (state_without_subdiv, deps_without_subdiv, result_without_subdiv) =
    Eval_exprs.eval_expr_with_deps_state
      ~with_alarms:CilE.warn_none_mode deps state e
  in
  if not (V.is_included result_without_subdiv V.top_int) then begin
    Value_parameters.debug ~level:2
      "subdivfloatvar: expression evaluates to an address";
    Eval_exprs.eval_expr_with_deps_state ~with_alarms deps state e
  end
  else
    let compare_min, compare_max =
      if V.is_included result_without_subdiv V.top_float
      then V.compare_min_float, V.compare_max_float
      else V.compare_min_int, V.compare_max_int
    in
    let vars = List.rev (compute_non_linear e) in
    let rec try_sub vars =
      match vars with
      | [] ->
         Eval_exprs.eval_expr_with_deps_state ~with_alarms deps state e
      | (lv, locopt) :: tail ->
        let loc = match locopt with
          | Some loc -> loc
          | None ->
            Eval_exprs.lval_to_loc ~with_alarms:CilE.warn_none_mode state lv
        in
        if not (Locations.cardinal_zero_or_one loc) then
          try_sub tail
        else
          try
            let typ_lv = Cil.typeOfLval lv in
            let _, v_value = Cvalue.Model.find state loc in
            let v_value = Eval_op.reinterpret ~with_alarms typ_lv v_value in
            let split =
              if V.is_included v_value V.top_float then split_float
              else if V.is_included v_value V.top_int then split_int
              else raise Too_linear (* pointers *)
            in
            let working_list = ref [ (v_value, result_without_subdiv) ] in
            let compute ?(with_alarms=CilE.warn_none_mode) subvalue =
              let substate =
                Cvalue.Model.reduce_previous_binding state loc subvalue
              in
              Eval_exprs.eval_expr ~with_alarms substate e
            in
            let bound1, bound2 = split.min_and_max v_value in
            let r1 = compute bound1 in
            let r2 = compute bound2 in
            (* This function assumes the result of the computation will be
               stored as an interval. (This is not the case for integers,
               but the default cardinality of small sets is so small that
               we would gain little anyway.) Thus, we know the result will
               be at least the interval [r1 .. r2]. Any subrange of
               [bound1..bound2] whose image is already in [r1 .. f2] will not
               be refined further. *)
            let wont_find_better =
              ref (if compare_min r2 r1 >= 0 then r1 else r2)
            in
            let had_bottom = ref false in
            let size =
              if Value_parameters.AllRoundingModes.get ()
              then 0
              else Int.to_int (Int_Base.project loc.Locations.size)
            in
            let subdiv_for_bound better_bound =
              let insert_subvalue_in_list (_, exp_value as p) l =
                let wont = !wont_find_better in
                let bound_to_test =
                  if better_bound exp_value wont <= 0
                  then exp_value
                  else wont
                in
                let rec aux l =
                  match l with
                  | [] -> [p]
                  | (_, exp_value1 as p1) :: tail ->
                      if better_bound exp_value1 bound_to_test >= 0
                      then p :: l
                      else p1 :: (aux tail)
                in
                aux l
              in
              let exp_subvalue subvalue l =
                let subexpr = compute subvalue in
                if V.is_bottom subexpr then had_bottom := true;
                insert_subvalue_in_list (subvalue, subexpr) l
              in
              let subdiv l =
                match l with
                | [] -> raise Can_not_subdiv
                | (value, exp_value) :: tail ->
                   let subvalue1, subvalue2 = split.subdiv ~size value in
                   if better_bound !wont_find_better exp_value = 0
                   then raise Can_not_subdiv;
                   let s = exp_subvalue subvalue1 tail in
                   exp_subvalue subvalue2 s
              in
              try
                for _i = 1 to subdivnb do
                  working_list := subdiv !working_list;
                done
              with Can_not_subdiv -> ()
            in
            subdiv_for_bound compare_min ;
            (* Now sort working_list in decreasing order
               on the upper bounds of exp_value *)
            let comp_exp_value (_value1,exp_value1) (_value2,exp_value2) =
              compare_max exp_value1 exp_value2
            in
            working_list := List.sort comp_exp_value !working_list ;
            wont_find_better := if compare_max r2 r1 >= 0 then r1 else r2;
            subdiv_for_bound compare_max ;
            let working_list = !working_list in
            (* Replay evaluation to emit alarms. This is a hack... *)
            let emit_alarms value = ignore (compute ~with_alarms value) in
            let reduced_state, optimized_exp_value =
              if !had_bottom
              then
                let reduced_var, optimized_exp_value =
                  List.fold_left
                    (fun (accv, acce) (value, exp_value) ->
                       emit_alarms value;
                       if V.is_bottom exp_value
                       then accv, acce
                       else V.join value accv, V.join exp_value acce)
                    (V.bottom, V.bottom) working_list
                in
                Cvalue.Model.reduce_previous_binding state loc reduced_var,
                optimized_exp_value
              else
                state_without_subdiv,
                List.fold_left
                  (fun acc (value, exp_value) ->
                     emit_alarms value;
                     V.join exp_value acc)
                  V.bottom working_list
            in
            reduced_state, deps_without_subdiv, optimized_exp_value
          with Too_linear -> try_sub tail
    in
    try_sub vars

let eval_expr_with_deps_state ~with_alarms deps state e =
  let subdivnb = Value_parameters.LinearLevel.get () in
  if subdivnb = 0 then
    Eval_exprs.eval_expr_with_deps_state ~with_alarms deps state e
  else
    eval_expr_with_deps_state_subdiv subdivnb ~with_alarms deps state e




(*
Local Variables:
compile-command: "make -C ../../.."
End:
*)
