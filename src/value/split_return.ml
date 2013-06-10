(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2013                                               *)
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

type split_strategy =
  | NoSplit
  | SplitEqList of Datatype.Big_int.t list
(* To be completed with more involved strategies *)

module SplitStrategy =
  Datatype.Make_with_collections(struct
    type t = split_strategy
    let name = "Value.Split_return.split_strategy"
    let rehash = Datatype.identity
    let structural_descr = Structural_descr.Abstract
    let reprs = [NoSplit]
    let compare s1 s2 = match s1, s2 with
      | NoSplit, NoSplit -> 0
      | SplitEqList l1, SplitEqList l2 ->
          Extlib.list_compare Int.compare l1 l2
      | NoSplit, SplitEqList _ -> -1
      | SplitEqList _, NoSplit -> 1
    let equal = Datatype.from_compare
    let hash = function
      | NoSplit -> 17
      | SplitEqList l ->
          List.fold_left (fun acc i -> acc * 13 + 57 * Int.hash i) 1 l
    let copy = Datatype.identity
    let internal_pretty_code = Datatype.undefined
    let pretty fmt = function
      | NoSplit -> Format.pp_print_string fmt "no split"
      | SplitEqList l ->
        Format.fprintf fmt "Split on \\result == %a"
          (Pretty_utils.pp_list ~sep:",@ " Datatype.Big_int.pretty) l
    let varname _ = "v"
    let mem_project _ _ = false
  end)


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
    ret_compared: Datatype.Big_int.Set.t (* all the constant values this
                                            lvalue is compared against *);
  }
  (* Per-function usage: all interesting lvalues are mapped to the way
     they are used *)
  and return_usage_per_fun = return_usage_by_lv MapLval.t
  (* Per-program usage. Lvalues are no longer used, functions are mapped
     to the values their return code is compared against *)
  and return_usage = Datatype.Big_int.Set.t Kernel_function.Map.t

  module RUDatatype = Kernel_function.Map.Make(Datatype.Big_int.Set)

  let find_or_default uf lv =
    try MapLval.find lv uf
    with Not_found -> {
      ret_callees = Kernel_function.Hptset.empty;
      ret_compared = Datatype.Big_int.Set.empty;
    }

  (* Treat a [Call] instruction. Immediate calls (no functions pointers)
     are added to the current usage store *)
  let add_call (uf: return_usage_per_fun) lv_opt e_fun =
    match e_fun.enode, lv_opt with
      | Lval (Var vi, NoOffset), Some lv
        when Cil.isIntegralType (Cil.typeOfLval lv) ->
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
          when Cil.isIntegralType typ && Cil.isIntegralType (Cil.typeOfLval lve)
        ->
          let u = find_or_default uf lve in
          MapLval.add lv_dest u uf
      | _ -> uf

  (* Treat an expression [lv == ct], [lv != ct] or [!lv], possibly with some
     cast. [ct] is added to the store of usages. *)
  let add_compare (uf: return_usage_per_fun) cond =
    (* add a comparison with the integer [i] to the lvalue [lv] *)
    let add_ct i lv =
      if Cil.isIntegralType (Cil.typeOfLval lv) then
        let u = find_or_default uf lv in
        let v = Datatype.Big_int.Set.add i u.ret_compared in
        let u = { u with ret_compared = v } in
        if debug then Format.printf
          "[Usage] Comparing %a to %a@." Printer.pp_lval lv Int.pretty i;
        MapLval.add lv u uf
      else
        uf
    in
    (* if [ct] is an integer constant, memoize it is compared to [lv] *)
    let add ct lv =
      (match (Cil.constFold true ct).enode with
        | Const (CInt64 (i, _, _)) -> add_ct i lv
        | _ -> uf)
    in
    match cond.enode with
      | BinOp ((Eq | Ne), {enode = Lval lv}, ct, _)
      | BinOp ((Eq | Ne), ct, {enode = Lval lv}, _) -> add ct lv

      | BinOp ((Eq | Ne), {enode = CastE (typ, {enode = Lval lv})}, ct, _)
      | BinOp ((Eq | Ne), ct, {enode = CastE (typ, {enode = Lval lv})}, _)
        when Cil.isIntegralType typ && Cil.isIntegralType (Cil.typeOfLval lv) ->
          add ct lv

      | UnOp (LNot, {enode = Lval lv}, _) ->
          add_ct Int.zero lv

      | UnOp (LNot, {enode = CastE (typ, {enode = Lval lv})}, _)
        when Cil.isIntegralType typ && Cil.isIntegralType (Cil.typeOfLval lv) ->
          add_ct Int.zero lv

      | _ -> uf

  (* Extract global usage: map functions to integers their return values
     are tested against *)
  let summarize (uf: return_usage_per_fun) =
    let aux _lv u acc =
      if Datatype.Big_int.Set.is_empty u.ret_compared then acc
      else
        let aux' kf (acc:return_usage) : return_usage =
          let cur =
            try Kernel_function.Map.find kf acc
            with Not_found -> Datatype.Big_int.Set.empty
          in
          let s = Datatype.Big_int.Set.union cur u.ret_compared in
          Kernel_function.Map.add kf s acc
      in
      Kernel_function.Hptset.fold aux' u.ret_callees acc
    in
    MapLval.fold aux uf Kernel_function.Map.empty


  class visitorVarUsage = object
    inherit Visitor.frama_c_inplace

    val mutable usage = MapLval.empty

    method vinst i =
      (match i with
        | Set (lv, e, _) ->
            usage <- add_alias usage lv e
        | Call (lv_opt, e, _, _) ->
            usage <- add_call usage lv_opt e
        | _ -> ()
      );
      Cil.DoChildren

    method vexpr e =
      usage <- add_compare usage e;
      Cil.DoChildren

    method result () =
      summarize usage
  end

  let compute file =
    let vis = new visitorVarUsage in
    Visitor.visitFramacFileSameGlobals (vis:> Visitor.frama_c_visitor) file;
    vis#result ()

  let pretty_usage fmt u =
    let pp_set =
      Pretty_utils.pp_iter ~sep:",@ " Datatype.Big_int.Set.iter Int.pretty in
    let pp kf s =
      Format.fprintf fmt "@[\\return(%a) == %a@]@ "
        Kernel_function.pretty kf pp_set s
    in
    Format.fprintf fmt "@[<v>";
    Kernel_function.Map.iter pp u;
    Format.fprintf fmt "@]"

end


module AutoStrategy = State_builder.Option_ref
  (ReturnUsage.RUDatatype)
  (struct
    let name = "Value.Split_return.Autostrategy"
    let dependencies = [Ast.self; Value_parameters.SplitReturnAuto.self]
   end)

module KfStrategy = Kernel_function.Make_Table(SplitStrategy)
  (struct
    let size = 17
    let dependencies = [Value_parameters.SplitReturnFunction.self;
                        AutoStrategy.self]
    let name = "Value.Split_return.Kfstrategy"
   end)


let strategy =
  KfStrategy.memo
    (fun kf ->
      let name = Kernel_function.get_name kf in
      try
        let l = Value_parameters.SplitReturnFunction.find name in
        if l = [] then raise Not_found (* use automatic detection *);
        SplitEqList (List.map Abstract_interp.Int.of_int l)
      with Not_found ->
        let auto =
          match AutoStrategy.get_option () with
            | None ->
                let v =
                  if Value_parameters.SplitReturnAuto.get () then
                    let ast = Ast.get () in
                    let v = ReturnUsage.compute ast in
                    Value_parameters.result "Splitting return states on:@.%a"
                      ReturnUsage.pretty_usage v;
                    v
                  else Kernel_function.Map.empty
                in
                AutoStrategy.set v;
                v
            | Some v -> v
        in
        try
          let set = Kernel_function.Map.find kf auto in
          let li = Datatype.Big_int.Set.fold (fun i acc -> i :: acc) set [] in
          SplitEqList li
        with Not_found -> NoSplit)

let default states =
  let joined = State_set.join states in
  if Model.is_reachable joined then [joined] else []

let split_eq_aux kf return_lv i states =
  let with_alarms = CilE.warn_none_mode in
  let loc = Eval_exprs.lval_to_loc ~with_alarms Model.top return_lv in
  let v_i = V.inject_int i in
  let (eq, neq, mess) = List.fold_left
    (fun (eq, neq, mess) state ->
      if Model.is_reachable state then
        let v = Model.find_unspecified ~with_alarms state loc in
        let v' = V_Or_Uninitialized.get_v v in
        (*Format.printf "## vi %a, v %a@." V.pretty v_i V.pretty v'; *)
        if V.equal v_i v' then
          (Model.join state eq, neq, mess)
        else
          let v'' = V.diff_if_one v' v_i in
          if V.equal v'' v' then
            (eq, state :: neq, mess)
          else
            (eq, state :: neq, true)
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
          if Cil.isIntegralType v.vtype then
            split_eq_multiple kf lv i states
          else
            default states
      | Some _ -> assert false (* Cil invariant *)
  in
  match strategy kf with
    | NoSplit -> default states
    | SplitEqList i -> split i



(*
Local Variables:
compile-command: "make -C ../.."
End:
*)
