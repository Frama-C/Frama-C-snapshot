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
open Abstract_interp
open Cvalue

(* Auxiliary module for inference of split criterion. We collect all the
   usages of a function call, and all places where they are compared against
   an integral constant *)
module ReturnUsage = struct
  let debug = false

  module MapLval = Cil_datatype.Lval.Map

  (* Uses of a given lvalue *)
  type return_usage_by_lv = {
    ret_callees: Kernel_function.Hptset.t (* all the functions that put their
                                             results in this lvalue *);
    ret_compared: Datatype.Integer.Set.t (* all the constant values this
                                            lvalue is compared against *);
  }
  (* Per-function usage: all interesting lvalues are mapped to the way
     they are used *)
  and return_usage_per_fun = return_usage_by_lv MapLval.t

  module RUDatatype = Kernel_function.Map.Make(Datatype.Integer.Set)

  let find_or_default uf lv =
    try MapLval.find lv uf
    with Not_found -> {
      ret_callees = Kernel_function.Hptset.empty;
      ret_compared = Datatype.Integer.Set.empty;
    }

  (* Treat a [Call] instruction. Immediate calls (no functions pointers)
     are added to the current usage store *)
  let add_call (uf: return_usage_per_fun) lv_opt e_fun =
    match e_fun.enode, lv_opt with
      | Lval (Var vi, NoOffset), Some lv
        when Cil.isIntegralOrPointerType (Cil.typeOfLval lv) ->
          let kf = Globals.Functions.get vi in
          let u = find_or_default uf lv in
          let funs = Kernel_function.Hptset.add kf u.ret_callees in
          let u = { u with ret_callees = funs } in
          if debug then Format.printf
            "[Usage] %a returns %a@." Kernel_function.pretty kf Printer.pp_lval lv;
          MapLval.add lv u uf
      | _ -> uf

  (* Treat a [Set] instruction [lv = (cast) lv']. Useful for return codes
     that are stored inside values of a slightly different type *)
  let add_alias (uf: return_usage_per_fun) lv_dest e =
    match e.enode with
      | CastE (typ, { enode = Lval lve })
          when Cil.isIntegralOrPointerType typ &&
            Cil.isIntegralOrPointerType (Cil.typeOfLval lve)
        ->
          let u = find_or_default uf lve in
          MapLval.add lv_dest u uf
      | _ -> uf

  (* add a comparison with the integer [i] to the lvalue [lv] *)
  let add_compare_ct uf i lv =
    if Cil.isIntegralOrPointerType (Cil.typeOfLval lv) then
      let u = find_or_default uf lv in
      let v = Datatype.Integer.Set.add i u.ret_compared in
      let u = { u with ret_compared = v } in
      if debug then Format.printf
        "[Usage] Comparing %a to %a@." Printer.pp_lval lv Int.pretty i;
      MapLval.add lv u uf
    else
      uf


  (* Treat an expression [lv == ct], [lv != ct] or [!lv], possibly with some
     cast. [ct] is added to the store of usages. *)
  let add_compare (uf: return_usage_per_fun) cond =
    (* if [ct] is an integer constant, memoize it is compared to [lv] *)
    let add ct lv =
      (match Cil.constFoldToInt ct with
        | Some i -> add_compare_ct uf i lv
        | _ -> uf)
    in
    match cond.enode with
      | BinOp ((Eq | Ne), {enode = Lval lv}, ct, _)
      | BinOp ((Eq | Ne), ct, {enode = Lval lv}, _) -> add ct lv

      | BinOp ((Eq | Ne), {enode = CastE (typ, {enode = Lval lv})}, ct, _)
      | BinOp ((Eq | Ne), ct, {enode = CastE (typ, {enode = Lval lv})}, _)
        when Cil.isIntegralOrPointerType typ &&
          Cil.isIntegralOrPointerType (Cil.typeOfLval lv) ->
        add ct lv

      | UnOp (LNot, {enode = Lval lv}, _) ->
          add_compare_ct uf Int.zero lv

      | UnOp (LNot, {enode = CastE (typ, {enode = Lval lv})}, _)
        when Cil.isIntegralOrPointerType typ &&
          Cil.isIntegralOrPointerType (Cil.typeOfLval lv) ->
        add_compare_ct uf Int.zero lv

      | _ -> uf

  (* Treat an expression [v] or [e1 && e2] or [e1 || e2]. This expression is
     supposed to be just inside an [if(...)], so that we may recognize patterns
     such as [if (f() && g())]. Patterns such as [if (f() == 1 && !g())] are
     handled in another way: the visitor recognizes comparison operators
     and [!], and calls {!add_compare}. *)
  let rec add_direct_comparison uf e =
    match e.enode with
    | Lval lv ->
      add_compare_ct uf Int.zero lv

    | CastE (typ, {enode = Lval lv})
        when Cil.isIntegralOrPointerType typ &&
          Cil.isIntegralOrPointerType (Cil.typeOfLval lv) ->
      add_compare_ct uf Int.zero lv

    | BinOp ((LAnd | LOr), e1, e2, _) ->
      add_direct_comparison (add_direct_comparison uf e1) e2

    | _ -> uf


  (* Per-program split strategy. Functions are mapped
     to the values their return code should be split against. *)
  type return_split = Datatype.Integer.Set.t Kernel_function.Map.t


  (** add to [kf] hints to split on all integers in [s]. *)
  let add_split kf s (ru:return_split) : return_split =
    let cur =
      try Kernel_function.Map.find kf ru
      with Not_found -> Datatype.Integer.Set.empty
    in
    let s = Datatype.Integer.Set.union cur s in
    Kernel_function.Map.add kf s ru


  (* Extract global usage: map functions to integers their return values
     are tested against *)
  let summarize_by_lv (uf: return_usage_per_fun): return_split =
    let aux _lv u acc =
      if Datatype.Integer.Set.is_empty u.ret_compared then acc
      else
        let aux_kf kf ru = add_split kf u.ret_compared ru in
        Kernel_function.Hptset.fold aux_kf u.ret_callees acc
    in
    MapLval.fold aux uf Kernel_function.Map.empty


  class visitorVarUsage = object
    inherit Visitor.frama_c_inplace

    val mutable usage = MapLval.empty

    method! vinst i =
      (match i with
        | Set (lv, e, _) ->
            usage <- add_alias usage lv e
        | Call (lv_opt, e, _, _) ->
            usage <- add_call usage lv_opt e
        | _ -> ()
      );
      Cil.DoChildren

    method! vstmt_aux s =
      (match s.skind with
      | If (e, _, _, _)
      | Switch (e, _, _, _) ->
          usage <- add_direct_comparison usage e
      | _ -> ()
      );
      Cil.DoChildren

    method! vexpr e =
      usage <- add_compare usage e;
      Cil.DoChildren

    method result () =
      summarize_by_lv usage
  end

  (* For functions returning pointers, add a split on NULL/non-NULL *)
  let add_null_pointers_split (ru: return_split): return_split =
    let null_set = Datatype.Integer.Set.singleton Integer.zero in
    let aux kf acc =
      if Cil.isPointerType (Kernel_function.get_return_type kf) then
        add_split kf null_set acc
      else acc
    in
    Globals.Functions.fold aux ru


  let compute file =
    let vis = new visitorVarUsage in
    Visitor.visitFramacFileSameGlobals (vis:> Visitor.frama_c_visitor) file;
    let split_compared = vis#result () in
    let split_null_pointers = add_null_pointers_split split_compared in
    split_null_pointers

end

module AutoStrategy = State_builder.Option_ref
  (ReturnUsage.RUDatatype)
  (struct
    let name = "Value.Split_return.Autostrategy"
    let dependencies = [Ast.self]
   end)

let compute_auto () =
  if AutoStrategy.is_computed () then
    AutoStrategy.get ()
  else begin
    let s = ReturnUsage.compute (Ast.get ()) in
    AutoStrategy.set s;
    AutoStrategy.mark_as_computed ();
    s
  end

(* Auto-strategy for one given function *)
let find_auto_strategy kf =
  try
    let s = Kernel_function.Map.find kf (compute_auto ()) in
    Split_strategy.SplitEqList (Datatype.Integer.Set.elements s)
  with Not_found -> Split_strategy.NoSplit

module KfStrategy = Kernel_function.Make_Table(Split_strategy)
  (struct
    let size = 17
    let dependencies = [Value_parameters.SplitReturnFunction.self;
                        Value_parameters.SplitGlobalStrategy.self;
                        AutoStrategy.self]
    let name = "Value.Split_return.Kfstrategy"
   end)

(* Invariant: this function never returns Split_strategy.SplitAuto *)
let kf_strategy =
  KfStrategy.memo
    (fun kf ->
      try (* User strategies take precedence *)
        match Value_parameters.SplitReturnFunction.find kf with
        | Split_strategy.SplitAuto -> find_auto_strategy kf
        | s -> s
      with Not_found ->
        match Value_parameters.SplitGlobalStrategy.get () with
        | Split_strategy.SplitAuto -> find_auto_strategy kf
        | s -> s
    )

let default states =
  let (joined,_) = State_set.join states in
  if Model.is_reachable joined then [joined] else []

let split_eq_aux kf return_lv i states =
  let with_alarms = CilE.warn_none_mode in
  let loc = Eval_exprs.lval_to_loc ~with_alarms Model.top return_lv in
  let v_i = V.inject_int i in
  let (eq, neq, mess) = List.fold_left
    (fun (eq, neq, mess) state ->
      if Model.is_reachable state then
        let _, v' = Model.find state loc in
        (*Format.printf "## vi %a, v %a@." V.pretty v_i V.pretty v'; *)
        if V.equal v_i v' then
          (Model.join state eq, neq, mess)
        else
          if V.is_included v_i v' then
            (eq, state :: neq, true)
          else
            (eq, state :: neq, mess)
      else
        (eq, neq, mess)
    ) (Model.bottom, [], false) states
  in
  if mess then
    Value_parameters.result ~once:true ~current:true
      "%a: cannot properly split on \\result == %a"
      Kernel_function.pretty kf Abstract_interp.Int.pretty i;
  (eq, neq)

let split_eq_multiple kf_name return_lv li states =
  let rec aux states li = match li with
    | [] ->
      (match states with
        | [] -> []
        | e :: q -> [List.fold_left Model.join e q])
    | i :: qli ->
        let eq, neq = split_eq_aux kf_name return_lv i states in
        let rq = aux neq qli in
        if Model.is_reachable eq then eq :: rq else rq
  in
  aux (State_set.to_list states) li


let join_final_states kf ~return_lv states =
  let split i =
    match return_lv with
      | None -> default states
      | Some (Var v, NoOffset as lv) ->
          if Cil.isIntegralOrPointerType v.vtype then
            split_eq_multiple kf lv i states
          else
            default states
      | Some _ -> assert false (* Cil invariant *)
  in
  match kf_strategy kf with
    | Split_strategy.SplitEqList i -> split i
    | Split_strategy.NoSplit -> default states
    | Split_strategy.FullSplit -> State_set.to_list states
    | Split_strategy.SplitAuto -> assert false (* transformed into SplitEqList*)


let pretty_strategies fmt =
  Format.fprintf fmt "@[<v>";
  let open Split_strategy in
  let pp_list = Pretty_utils.pp_list ~sep:",@ " Int.pretty in
  let pp_one user_auto pp = function
    | NoSplit -> ()
    | FullSplit ->
      Format.fprintf fmt "@[\\full_split(%t)@]@ " pp
    | SplitEqList l ->
      Format.fprintf fmt "@[\\return(%t) == %a (%s)@]@ " pp pp_list l user_auto
    | SplitAuto -> assert false (* should have been replaced by SplitEqList *)
  in
  let pp_kf kf fmt = Kernel_function.pretty fmt kf in
  let pp_user (kf, strategy) =
    match strategy with
    | None -> ()
    | Some SplitAuto -> pp_one "auto" (pp_kf kf) (kf_strategy kf)
    | Some s -> pp_one "user" (pp_kf kf) s
  in
  Value_parameters.SplitReturnFunction.iter pp_user;
  if not (Value_parameters.SplitReturnFunction.is_empty ()) &&
     match Value_parameters.SplitGlobalStrategy.get () with
     | Split_strategy.NoSplit | Split_strategy.SplitAuto -> false
     | _ -> true
  then Format.fprintf fmt "@[other functions:@]@ ";
  begin match Value_parameters.SplitGlobalStrategy.get () with
    | SplitAuto ->
      let pp_auto kf s =
        if not (Value_parameters.SplitReturnFunction.mem kf) then
          let s = SplitEqList (Datatype.Integer.Set.elements s) in
          pp_one "auto" (pp_kf kf) s
      in
      let auto = compute_auto () in
      Kernel_function.Map.iter pp_auto auto;
    | s -> pp_one "auto" (fun fmt -> Format.pp_print_string fmt "@all") s
  end;
  Format.fprintf fmt "@]"

let pretty_strategies () =
  if not (Value_parameters.SplitReturnFunction.is_empty ()) ||
    (Value_parameters.SplitGlobalStrategy.get () != Split_strategy.NoSplit)
  then
    Value_parameters.result "Splitting return states on:@.%t" pretty_strategies



(*
Local Variables:
compile-command: "make -C ../../.."
End:
*)
