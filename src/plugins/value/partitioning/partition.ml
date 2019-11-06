(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2019                                               *)
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

open Bottom.Type

(* --- Split monitors --- *)

type split_monitor = {
  split_limit : int;
  mutable split_values : Datatype.Integer.Set.t;
}

let new_monitor ~split_limit = {
  split_limit;
  split_values = Datatype.Integer.Set.empty;
}

(* --- Stamp rationing --- *)

(* Stamps used to label states according to slevel.
   The second integer is used to keep separate the different states resulting
   from a transfer function producing a state list before a new stamping.  *)
type stamp = (int * int) option (* store stamp / transfer stamp *)

(* Stamp rationing according to the slevel. *)
type rationing =
  { current: int ref; (* last used stamp. *)
    limit: int;       (* limit of available stamps; after, stamps are [None]. *)
    merge: bool       (* on merge slevel annotations or -eva-merge-after-loop,
                         merge the incoming states with one unique stamp. *)
  }

let new_rationing ~limit ~merge = { current = ref 0; limit; merge }

(* --- Keys --- *)

module ExpMap = Cil_datatype.ExpStructEq.Map
module IntPair = Datatype.Pair (Datatype.Int) (Datatype.Int)
module LoopList = Datatype.List (IntPair)
module BranchList = Datatype.List (Datatype.Int)

type branch = int

(* The key have several fields, one for each kind of partitioning:
   - Ration stamps: These modelize the legacy slevel. Each state is given
     a ration stamp (represented by two integers) until there is no slevel
     left. The first number is attributed by the store it comes from, the
     second one is attributed by the last transfer.
     It is an option type, when there is no more ration stamp, this field is
     set to None; each new state will not be distinguished by this field.
   - Branches: This field enumerate the last junctions points passed through.
     The partitioning may chose how the branches are identified, but it
     is a First-In-First-Out set.
   - Loops: This field stores the loop iterations needed to reach this state
     for each loop we are currently in. It is stored in reverse order
     (innermost loop first) It also stores the maximum number of unrolling ;
     this number varies from a state to another, as it is computed from
     an expression evaluated when we enter the loop.
   - Static/Dynamic splits: track the splits applied to the state as a map
     from the expression of the split to the value of this expression. Since
     the split creates states in which the expression evalutates to a
     singleton, the values of the map are integers.
     Static splits are only evaluated when the annotation is encountered
     whereas dynamic splits are reevaluated regularly. *)
type key = {
  ration_stamp : stamp;
  branches : branch list;
  loops : (int * int) list; (* current iteration / max unrolling *)
  static_split : (Integer.t*split_monitor) ExpMap.t; (* exp->value*monitor *)
  dynamic_split : (Integer.t*split_monitor) ExpMap.t; (* exp->value*monitor *)
}

module Key =
struct
  type t = key

  (* Initial key, before any partitioning *)
  let zero = {
    ration_stamp = None;
    branches = [];
    loops = [];
    static_split = ExpMap.empty;
    dynamic_split = ExpMap.empty;
  }

  let compare k1 k2 =
    let (<?>) c (cmp,x,y) =
      if c = 0 then cmp x y else c
    in
    let compare_split (i1,_m1) (i2,_m2) =
      Integer.compare i1 i2
    in
    Extlib.opt_compare IntPair.compare k1.ration_stamp k2.ration_stamp
    <?> (LoopList.compare, k1.loops, k2.loops)
    <?> (ExpMap.compare compare_split, k1.static_split, k2.static_split)
    <?> (ExpMap.compare compare_split, k1.dynamic_split, k2.dynamic_split)
    <?> (BranchList.compare, k1.branches, k2.branches)

  let pretty fmt key =
    begin match key.ration_stamp with
      | Some (n,_) -> Format.fprintf fmt "#%d" n
      | None -> ()
    end;
    Pretty_utils.pp_list ~pre:"[@[" ~sep:" ;@ " ~suf:"@]]"
      Format.pp_print_int
      fmt
      key.branches;
    Pretty_utils.pp_list ~pre:"(@[" ~sep:" ;@ " ~suf:"@])"
      (fun fmt (i,_j) -> Format.pp_print_int fmt i)
      fmt
      key.loops;
    Pretty_utils.pp_list ~pre:"{@[" ~sep:" ;@ " ~suf:"@]}"
      (fun fmt (e,(i,_m)) -> Format.fprintf fmt "%a:%a"
          Cil_printer.pp_exp e
          (Integer.pretty ~hexa:false) i)
      fmt
      (ExpMap.bindings key.static_split @ ExpMap.bindings key.dynamic_split)

  let exceed_rationing key = key.ration_stamp = None
end


(* --- Partitions --- *)

module KMap = Map.Make (Key)

type 'a partition = 'a KMap.t

let empty = KMap.empty
let find = KMap.find
let replace = KMap.add
let is_empty = KMap.is_empty
let size = KMap.cardinal
let iter = KMap.iter
let map = KMap.map
let filter = KMap.filter
let merge = KMap.merge

let to_list (p : 'a partition) : 'a list =
  KMap.fold (fun _k x l -> x :: l) p []


(* --- Partitioning actions --- *)

type unroll_limit =
  | ExpLimit of Cil_types.exp
  | IntLimit of int
  | AutoUnroll of Cil_types.stmt * int * int

type split_kind = Static | Dynamic

type action =
  | Enter_loop of unroll_limit
  | Leave_loop
  | Incr_loop
  | Branch of branch * int
  | Ration of rationing
  | Restrict of Cil_types.exp * Integer.t list
  | Split of Cil_types.exp * split_kind * split_monitor
  | Merge of Cil_types.exp * split_kind
  | Update_dynamic_splits

exception InvalidAction

(* --- Flows --- *)

module MakeFlow (Abstract: Abstractions.Eva) =
struct
  type state = Abstract.Dom.t
  type t =  (key * state) list

  let empty = []

  let initial (p : 'a list) : t =
    List.map (fun state -> Key.zero, state) p

  let to_list (f : t) : state list =
    List.map snd f

  let of_partition (p : state partition) : t =
    KMap.fold (fun k x l -> (k,x) :: l) p []

  let to_partition (p : t) : state partition =
    let add p (k,x) =
      (* Join states with the same key *)
      let x' =
        try
          Abstract.Dom.join (KMap.find k p) x
        with Not_found -> x
      in
      KMap.add k x' p
    in
    List.fold_left add KMap.empty p

  let is_empty (p : t) =
    p = []

  let size (p : t) =
    List.length p

  let union (p1 : t) (p2 : t) : t =
    p1 @ p2

  (* --- Automatic loop unrolling ------------------------------------------- *)

  module AutoLoopUnroll = Auto_loop_unroll.Make (Abstract)

  (* --- Evaluation and split functions ------------------------------------- *)

  (* Domains transfer functions. *)
  module TF = Abstract.Dom.Transfer (Abstract.Eval.Valuation)

  exception Operation_failed

  let fail ~exp message =
    let source = fst exp.Cil_types.eloc in
    let warn_and_raise message =
      Value_parameters.warning ~source ~once:true "%s" message;
      raise Operation_failed
    in
    Pretty_utils.ksfprintf warn_and_raise message

  let evaluate_exp_to_ival ?valuation state exp =
    (* Evaluate the expression *)
    let valuation, value =
      match Abstract.Eval.evaluate ?valuation ~reduction:false state exp with
      | `Value (valuation, value), alarms when Alarmset.is_empty alarms ->
        valuation, value
      | _ ->
        fail ~exp "this partitioning parameter cannot be evaluated safely on \
                   all states"
    in
    (* Get the cvalue *)
    let cvalue = match Abstract.Val.get Main_values.CVal.key with
      | Some get_cvalue -> get_cvalue value
      | None -> fail ~exp "partitioning is disabled when the CValue domain is \
                           not active"
    in
    (* Extract the ival *)
    let ival =
      try
        Cvalue.V.project_ival cvalue
      with Cvalue.V.Not_based_on_null ->
        fail ~exp "this partitioning parameter must evaluate to an integer"
    in
    valuation, ival

  exception Split_limit of Integer.t option

  let split_by_value ~monitor state exp =
    let module SplitValues = Datatype.Integer.Set in
    let valuation, ival = evaluate_exp_to_ival state exp in
    (* Build a state with the lvalue set to a singleton *)
    let build i acc =
      let value = Abstract.Val.inject_int (Cil.typeOf exp) i in
      let state =
        Abstract.Eval.assume ~valuation state exp value >>- fun valuation ->
        (* Check the reduction *)
        TF.update valuation state
      in
      match state with
      | `Value state ->
        let _,new_ival = evaluate_exp_to_ival state exp in
        if not (Ival.is_singleton_int new_ival) then
          fail ~exp "failing to learn perfectly from split" ;
        monitor.split_values <-
          SplitValues.add i monitor.split_values;
        (i, state) :: acc
      | `Bottom -> (* This value cannot be set in the state ; the evaluation of
                      expr was unprecise *)
        acc
    in
    try
      (* Check the size of the ival *)
      begin match Ival.cardinal ival with
        | None -> raise (Split_limit None)
        | Some c as count ->
          if Integer.(gt c (of_int monitor.split_limit)) then
            raise (Split_limit count)
      end;
      (* For each integer of the ival, build a new state *)
      try
        let result = Ival.fold_int build ival [] in
        let c = SplitValues.cardinal monitor.split_values in
        if c > monitor.split_limit then
          raise (Split_limit (Some (Integer.of_int c)));
        result
      with Abstract_interp.Error_Top -> (* The ival is float *)
        raise (Split_limit None)
    with
    | Split_limit count ->
      let pp_count fmt =
        match count with
        | None -> ()
        | Some c -> Format.fprintf fmt " (%a)" (Integer.pretty ~hexa:false) c
      in
      fail ~exp "split on more than %d values%t prevented ; try to improve \
                 the analysis precision or look at the option -eva-split-limit \
                 to increase this limit."
        monitor.split_limit pp_count

  let eval_exp_to_int state exp =
    let _valuation, ival = evaluate_exp_to_ival state exp in
    try
      Integer.to_int (Ival.project_int ival)
    with
    | Ival.Not_Singleton_Int ->
      fail ~exp "this partitioning parameter must evaluate to a singleton"
    | Failure _ ->
      fail ~exp "this partitioning parameter is too big"

  (* --- Applying partitioning actions onto flows --------------------------- *)

  let stamp_by_value = match Abstract.Val.get Main_values.CVal.key with
    | None -> fun _ _ _ -> None
    | Some get -> fun expr expected_values state ->
      let typ = Cil.typeOf expr in
      let make stamp i = stamp, i, Abstract.Val.inject_int typ i in
      let expected_values = List.mapi make expected_values in
      match fst (Abstract.Eval.evaluate state expr) with
      | `Bottom -> None
      | `Value (_cache, value) ->
        let is_included (_, _, v) = Abstract.Val.is_included v value in
        match List.find_opt is_included expected_values with
        | None -> None
        | Some (stamp, i, _) ->
          if Cvalue.V.cardinal_zero_or_one (get value)
          then Some (stamp, 0)
          else begin
            Value_parameters.result ~once:true ~current:true
              "cannot properly split on \\result == %a"
              Abstract_interp.Int.pretty i;
            None
          end

  let split_state ~monitor (kind : split_kind) (exp : Cil_types.exp)
      (key : key) (state : state) : (key * state) list =
    try
      let add value map = ExpMap.add exp (value, monitor) map in
      let update_key (v,x) =
        let k =
          match kind with
          | Static -> { key with static_split = add v key.static_split }
          | Dynamic -> { key with dynamic_split = add v key.dynamic_split }
        in
        (k,x)
      in
      List.map update_key (split_by_value ~monitor state exp)
    with Operation_failed ->
      [(key,state)]

  let split ~monitor (kind : split_kind) (exp : Cil_types.exp) (p : t) =
    let add_split acc (key,state) =
      split_state ~monitor kind exp key state @ acc
    in
    List.fold_left add_split [] p

  let update_dynamic_splits p =
    (* Update one state *)
    let update_state acc (key,state) =
      (* Split the states in the list l for the given exp *)
      let update_exp exp (_i,monitor) l =
        let resplit acc (k,x) =
          split_state ~monitor Dynamic exp k x @ acc
        in
        List.fold_left resplit [] l
      in
      (* Foreach exp in original state: split *)
      ExpMap.fold update_exp key.dynamic_split [(key,state)] @ acc
    in
    List.fold_left update_state [] p

  let map_keys (f : key -> state -> key) (p : t) : t =
    List.map (fun (k,x) -> f k x, x) p

  let transfer_keys p = function
    | Split (expr, kind, monitor) ->
      split ~monitor kind expr p

    | Update_dynamic_splits ->
      update_dynamic_splits p

    | action -> (* Simple map transfer functions *)
      let transfer = match action with
        | Split _ | Update_dynamic_splits ->
          assert false (* Handled above *)

        | Enter_loop limit_kind -> fun k x ->
          let limit = try match limit_kind with
            | ExpLimit exp -> eval_exp_to_int x exp
            | IntLimit i -> i
            | AutoUnroll (stmt, min_unroll, max_unroll) ->
              match AutoLoopUnroll.compute ~max_unroll x stmt with
              | None -> min_unroll
              | Some i ->
                Value_parameters.warning ~once:true ~current:true
                  ~wkey:Value_parameters.wkey_loop_unroll
                  "Automatic loop unrolling.";
                i
            with
            | Operation_failed -> 0
          in
          { k with loops = (0,limit) :: k.loops }

        | Leave_loop -> fun k _x ->
          begin match k.loops with
            | [] -> raise InvalidAction
            | _ :: tl -> { k with loops = tl }
          end

        | Incr_loop -> fun k _x ->
          begin match k.loops with
            | [] -> raise InvalidAction
            | (h, limit) :: tl ->
              if h >= limit then begin
                if limit > 0 then
                  Value_parameters.warning ~once:true ~current:true
                    ~wkey:Value_parameters.wkey_loop_unroll
                    "loop not completely unrolled";
                k
              end
              else
                { k with loops = (h + 1, limit) :: tl }
          end

        | Branch (b,max) -> fun k _x ->
          if max > 0 then
            { k with branches = b :: Extlib.list_first_n (max - 1) k.branches  }
          else if k.branches <> [] then
            { k with branches = [] }
          else
            k

        | Ration { current; limit; merge } ->
          let length = List.length p in
          (* The incoming states exceed the rationing limit: no more stamps. *)
          if !current + length > limit then begin
            current := limit;
            fun k _ -> { k with ration_stamp = None }
          end
          (* If merge, a unique ration stamp for all incoming states. *)
          else if merge then begin
            current := !current + length;
            fun k _ -> { k with ration_stamp = Some (!current, 0) }
          end
          (* Default case: a different stamp for each incoming state. *)
          else
            let stamp () = incr current; Some (!current, 0) in
            fun k _ -> { k with ration_stamp = stamp () }

        | Restrict (expr, expected_values) -> fun k s ->
          { k with ration_stamp = stamp_by_value expr expected_values s}

        | Merge (exp, Static) -> fun k _x ->
          { k with static_split = ExpMap.remove exp k.static_split }

        | Merge (exp, Dynamic) -> fun k _x ->
          { k with dynamic_split = ExpMap.remove exp k.dynamic_split }
      in
      map_keys transfer p

  let transfer_states (f : state -> state list) (p : t) : t =
    let n = ref 0 in
    let transfer acc (k,x) =
      let add =
        match k.ration_stamp with
        (* No ration stamp, just add the state to the list *)
        | None -> fun l y -> (k,y) :: l
        (* There is a ration stamp, set the second part of the stamp to a unique transfer number *)
        | Some (s,_) -> fun l y ->
          let k' = { k with ration_stamp = Some (s, !n) } in
          incr n;
          (k',y) :: l
      in
      List.fold_left add acc (f x)
    in
    List.fold_left transfer [] p

  let iter (f : state -> unit) (p : t) : unit =
    List.iter (fun (_k,x) -> f x) p

  let join_duplicate_keys (p : t) : t =
    let cmp (k, _) (k', _) = Key.compare k k' in
    let p = List.fast_sort cmp p in
    let rec aux acc (key, state) = function
      | [] -> (key, state) :: acc
      | (key', state') :: tl ->
        if Key.compare key key' = 0
        then aux acc (key, Abstract.Dom.join state state') tl
        else aux ((key, state) :: acc) (key', state') tl
    in
    match p with
    | [] | [_] -> p
    | e :: tl -> aux [] e tl

  let filter_map (f: key -> state -> state option) (p : t) : t =
    let rec aux = function
      | [] -> []
      | (key, x) :: tl -> match f key x with
        | Some y -> (key, y) :: (aux tl)
        | None -> aux tl
    in
    aux p
end
