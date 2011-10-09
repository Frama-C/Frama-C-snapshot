(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2011                                               *)
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
open Cil_datatype
open Eval_exprs
open Locations
open Abstract_interp
open Cvalue
open Bit_utils
open Value_util


(** Truth values for a predicate analyzed by the value analysis *)

type predicate_value = True | False | Unknown

let string_of_predicate_value = function
  | Unknown -> "unknown"
  | True -> "valid"
  | False -> "invalid"

let pretty_predicate_value fmt v =
  Format.fprintf fmt "%s" (string_of_predicate_value v)

let join_predicate x y = match x, y with
  | True, True -> True
  | False, False -> False
  | True, False | False, True
  | Unknown, _ | _, Unknown -> Unknown

exception Stop

let fold_join_predicate fold f s =
  try
    match
     fold 
       (fun acc e ->
         match f e with
         | Unknown -> raise Stop
         | v -> match acc with
           | None -> Some v
           | Some acc -> Some (join_predicate acc v)
       )
       None
       s
    with
      | None -> True
      | Some v -> v
  with Stop -> Unknown

(* Exception raised to end the computation for a predicate *)
exception Predicate_alarm
let predicate_alarm () = raise Predicate_alarm

let warn_raise_mode =
  { CilE.imprecision_tracing = CilE.Aignore ;
    others = CilE.Acall predicate_alarm ;
    unspecified = CilE.Acall predicate_alarm }


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

type label = Pre | Here | Old | Post

type eval_env = {
  e_cur: label;
  e_pre:  Cvalue.Model.t;
  e_here: Cvalue.Model.t;
  e_old:  Cvalue.Model.t option;
  e_post: Cvalue.Model.t option;
}

let join_env e1 e2 =
  let join_opt v1 v2 = match v1, v2 with
    | None, None -> None
    | Some v, None | None, Some v -> Some v
    | Some v1, Some v2 -> Some (Cvalue.Model.join v1 v2)
  in {
    e_cur = (assert (e1.e_cur = e2.e_cur); e1.e_cur);
    e_pre =  Cvalue.Model.join e1.e_pre  e2.e_pre;
    e_here = Cvalue.Model.join e1.e_here e2.e_here;
    e_old = join_opt e1.e_old e2.e_old;
    e_post = join_opt e1.e_post e2.e_post;
  }

let extract_opt_env = function
  | Some v -> v
  | None -> predicate_alarm ()

let convert_label = function
  | StmtLabel _ -> predicate_alarm ()
  | LogicLabel (_, "Pre")  -> Pre
  | LogicLabel (_, "Here") -> Here
  | LogicLabel (_, "Old")  -> Old
  | LogicLabel (_, "Post") -> Post
  | LogicLabel _ -> predicate_alarm ()

let env_state env = function
  | Pre  -> env.e_pre
  | Here -> env.e_here
  | Old  -> extract_opt_env env.e_old
  | Post -> extract_opt_env env.e_post

let env_current_state e = env_state e e.e_cur

let overwrite_state env state = function
  | Pre -> { env with e_pre = state }
  | Here -> { env with e_here = state }
  | Old -> { env with e_old = Some state }
  | Post -> { env with e_post = Some state }

let overwrite_current_state env state = overwrite_state env state env.e_cur

let env_pre_f ~init = {
  e_cur = Here;
  e_pre = init; e_here = init;
  e_post = None; e_old = None;
}

let env_post_f ~pre ~post = {
  e_cur = Here;
  e_pre =  pre; e_old = Some pre;
  e_post = Some post; e_here = post;
}

let env_annot ~pre ~here = {
  e_cur = Here;
  e_pre = pre; e_here = here;
  e_post = None; e_old = None;
}


let (!!) = Lazy.force

let lop_to_cop op =
  match op with
  | Req -> Eq
  | Rneq -> Ne
  | Rle -> Le
  | Rge -> Ge
  | Rlt -> Lt
  | Rgt -> Gt

let rec eval_term env result t =
  let with_alarms = warn_raise_mode in
  match t.term_node with
    | Tat (t, lab) -> begin
        let lab = convert_label lab in
        eval_term { env with e_cur = lab } result t
      end

    | TConst (CInt64 (v, _, _)) -> [intType, Cvalue.V.inject_int v]
    | TConst (CEnum e) ->
        (match (constFold true e.eival).enode with
           | Const (CInt64 (v, _, _)) -> [intType, Cvalue.V.inject_int v]
           | _ -> raise Predicate_alarm)
    | TConst (CChr c) -> [intType, Cvalue.V.inject_int
                                       (Int.of_int (int_of_char c))]
    | TConst (CReal (f, _, _)) ->
        (* TODO: there might be a problem with float constant present in the
           code that have been rounded *)
        Value_parameters.result ~once:true "float support is experimental";
        let f = Ival.F.of_float f in
        let _, f = Ival.Float_abstract.inject_r f f in
        [floatType, Cvalue.V.inject_ival (Ival.Float f)]
(*  | TConst ((CStr | CWstr) Missing cases *)

    | TAddrOf _
    | TStartOf _ ->
        let conv (typ, loc) = (typ, loc_bits_to_loc_bytes loc) in
        List.map conv (eval_tlval env result t)

    | TLval _ ->
        let lvals = eval_tlval env result t in
        let eval_lval (typ, loc) =
          let v = Cvalue.Model.find ~conflate_bottom:false
            ~with_alarms (env_current_state env)
            (make_loc loc (Bit_utils.sizeof typ))
          in
          let v = do_cast ~with_alarms typ v in
          (typ, v)
        in
        List.map eval_lval lvals

    | TBinOp (op,t1,t2) -> begin
        let l1 = eval_term env result t1 in
        let l2 = eval_term env result t2 in
        let aux (te1, v1)  (_te2, v2) =
          (* Format.printf "T1 %a %a:%a, T2 %a %a:%a@."
               d_term t1 Cvalue.V.pretty  v1 d_type te1
               d_term t2 Cvalue.V.pretty v2 d_type _te2; *)
          let te1 = unrollType te1 in
          (* We use the type of t1 to determine whether we are performing
             an int or float operation. Hopefully this is correct *)
          let v = match te1 with
            | TInt _ | TPtr _ | TEnum _ ->
              (* Do not pass ~typ here. We want the operations to be
                 performed on unbounded integers mode *)
                eval_binop_int ~with_alarms ~te1 v1 op v2
            | TFloat _ ->
                eval_binop_float ~with_alarms v1 op v2
            | _ -> raise Predicate_alarm
          in
          (te1, v)
        in
        match op, l1, l2 with
          | (PlusA | PlusPI | IndexPI | MinusA | MinusPI), _, _
          | (Eq | Ne), _ , _ -> (* TODO: use set semantics *)
              List.fold_left (fun acc e1 ->
                List.fold_left (fun acc e2 -> aux e1 e2 :: acc) acc l2) [] l1
          | _, [e1], [e2] -> [aux e1 e2]
          | _ -> (*Format.printf "Bla %a %a@." d_term t1 d_term t2;*)
              raise Predicate_alarm
      end

    | TUnOp (op, t) ->
        let l = eval_term env result t in
        let typ' t = match op with
          | Neg -> t
          | BNot -> t (* can only be used on an integer type *)
          | LNot -> intType
        in
        let eval typ v = eval_unop ~with_alarms v typ op in
        List.map (fun (typ, v) -> typ' typ, eval typ v) l

    | Trange (otlow, othigh) ->
        let eval proj join = function
          | None -> None
          | Some t ->
              let lbound = eval_term env result t in
              let aux (typ, v) =
                if not (isIntegralType typ) then raise Predicate_alarm;
                try proj (Cvalue.V.project_ival v)
                with Cvalue.V.Not_based_on_null -> None
              in
              match List.map aux lbound with
                | [] -> raise Predicate_alarm
                | h :: q ->
                    let join v1 v2 = match v1, v2 with
                      | None, _ | _, None -> None
                      | Some v1, Some v2 -> Some (join v1 v2)
                    in
                    List.fold_left join h q
        in
        let min = eval Ival.min_int Int.min otlow
        and max = eval Ival.max_int Int.max othigh in
        [intType, Cvalue.V.inject_ival (Ival.inject_range min max)]

    | TCastE (typ, t) ->
        let l = eval_term env result t in
        List.map (fun (_, v) -> typ, do_cast ~with_alarms typ v) l

    | Tif (tcond, ttrue, tfalse) ->
        let l = eval_term env result tcond in
        let vtrue =  List.exists (fun (_, v) -> Cvalue.V.contains_non_zero v) l
        and vfalse = List.exists (fun (_, v) -> Cvalue.V.contains_zero v) l  in
        (if vtrue then eval_term env result ttrue else [])
        @ (if vfalse then eval_term env result tfalse else [])

    | TSizeOf _ | TSizeOfE _ | TSizeOfStr _ | TAlignOf _ | TAlignOfE _ ->
        let e = Cil.constFoldTerm true t in
        let r = match e.term_node with
          | TConst (CInt64 (v, _, _)) -> Cvalue.V.inject_int v
          | _ -> V.top_int
        in
        [intType, r]

    | _ -> raise Predicate_alarm

and eval_tlhost env result lv =
  match lv with
    | TVar { lv_origin = Some v } ->
        let loc = Location_Bits.inject (Base.find v) Ival.zero in
        [v.vtype, loc]
    | TResult typ ->
        (match result with
          | Some v ->
              let loc = Location_Bits.inject (Base.find v) Ival.zero in
              [typ, loc]
          | None -> raise Predicate_alarm)
    | TVar { lv_origin = None } -> (* TODO: add an env for logic vars *)
        raise Predicate_alarm
    | TMem t ->
        let l = eval_term env result t in
        List.map (fun (t, loc) ->
          match t with
            | TPtr (t, _) -> t, loc_bytes_to_loc_bits loc
            | _ -> raise Predicate_alarm
        ) l

and eval_toffset env result typ toffset =
  match toffset with
  | TNoOffset ->
      [typ, Ival.singleton_zero]
  | TIndex (trm, remaining) ->
      let typ_pointed = match (unrollType typ) with
        | TArray (t, _, _, _) -> t
        | TPtr(t,_) ->
            (match unrollType t with
              | TArray (t, _,_,_) -> t
              | _ -> raise Predicate_alarm)
        | _ -> raise Predicate_alarm
      in
      let lloctrm = eval_term env result trm in
      let aux (_typ, current) =
        let offset =
          try Cvalue.V.project_ival current
          with Cvalue.V.Not_based_on_null -> raise Predicate_alarm
        in
        let loffsrem = eval_toffset env result typ_pointed remaining in
        let aux (typ, r) =
          let offset = Ival.scale_int64base (sizeof typ_pointed) offset in
          typ, Ival.add offset r
        in
        List.map aux loffsrem
      in
      List.fold_left (fun acc trm -> aux trm @ acc) [] lloctrm

  | TField (fi,remaining) ->
      let current,_ = bitsOffset typ (Field(fi,NoOffset)) in
      let loffs = eval_toffset env result fi.ftype remaining in
      List.map (fun (typ, r) -> typ, Ival.add (Ival.of_int current) r) loffs

and eval_tlval env result t =
  let process ftyp tlval toffs =
    let lvals = eval_tlhost env result tlval in
    let aux acc (typ, loc) =
      let loffset = eval_toffset env result typ toffs in
      let aux acc (typ_offs, offs) =
        let loc = Location_Bits.location_shift offs loc in
        (ftyp typ_offs, loc) :: acc
      in
      List.fold_left aux acc loffset
    in
    List.fold_left aux [] lvals
  in
  match t.term_node with
  | TAddrOf (tlval, toffs)
  | TStartOf (tlval, toffs) ->
      process (fun typ -> TPtr (typ, [])) tlval toffs
  | TLval (tlval, toffs) ->
      process (fun typ -> typ) tlval toffs
  | Tunion l -> List.concat (List.map (eval_tlval env result) l)
  | Tempty_set -> []
  (* TODO: add support for TcastE, by adapting what is done for pass_cast
     in eval_exprs.ml *)
  | _ -> raise Predicate_alarm

let eval_tlval_as_location env result t =
  let l = eval_tlval env result t in
  let aux acc (typ, loc) =
    let s = Bit_utils.sizeof typ in
    assert (loc_equal acc loc_bottom || Int_Base.equal s acc.size);
    make_loc (Location_Bits.join loc acc.loc) s
  in
  List.fold_left aux loc_bottom l

exception Not_an_exact_loc

let eval_term_as_exact_loc env result t =
  match t.term_node with
    | TLval _ ->
        (match eval_tlval env result t with
           | [] | _ :: _ :: _ -> raise Not_an_exact_loc
           | [typ, loc] ->
               let loc = Locations.make_loc loc (Bit_utils.sizeof typ) in
               if not (valid_cardinal_zero_or_one ~for_writing:false loc)
               then raise Not_an_exact_loc;
               typ, loc
        )
    | _ -> raise Not_an_exact_loc

let rec reduce_by_predicate ~result env positive p =
  let result =
    match positive,p.content with
    | true,Ptrue | false,Pfalse -> env

    | true,Pfalse | false,Ptrue ->
        overwrite_current_state env Cvalue.Model.bottom

    | true,Pand (p1,p2 ) | false,Por(p1,p2)->
        let r1 = reduce_by_predicate ~result env positive p1 in
        reduce_by_predicate ~result r1 positive p2

    | true,Por (p1,p2 ) | false,Pand (p1, p2) ->
        join_env
          (reduce_by_predicate ~result env positive p1)
          (reduce_by_predicate ~result env positive p2)

    | true,Pimplies (p1,p2) ->
        join_env
          (reduce_by_predicate ~result env false p1)
          (reduce_by_predicate ~result env true p2)

    | false,Pimplies (p1,p2) ->
        reduce_by_predicate ~result
          (reduce_by_predicate ~result env true p1)
          false
          p2

    | _,Pnot p -> reduce_by_predicate ~result env (not positive) p

    | true,Piff (p1, p2) ->
        let red1 =
          reduce_by_predicate ~result env true (Logic_const.pand (p1, p2)) in
        let red2 =
          reduce_by_predicate ~result env false (Logic_const.por (p1, p2)) in
        join_env red1 red2

    | false,Piff (p1, p2) ->
        reduce_by_predicate ~result env true
          (Logic_const.por
             (Logic_const.pand (p1, Logic_const.pnot p2),
              Logic_const.pand (Logic_const.pnot p1, p2)))

    | _,Pxor(p1,p2) ->
        reduce_by_predicate ~result env
          (not positive) (Logic_const.piff(p1, p2))

    | _,Prel (op,t1,t2) ->
      begin
        try
          let eval = match t1.term_type with
            | t when isLogicRealOrFloatType t ->
                eval_float (Value_parameters.AllRoundingModes.get ())
            | t when isLogicIntegralType t -> eval_int
            | Ctype (TPtr _) -> eval_int
            | _ -> raise Predicate_alarm
          in
          reduce_by_relation eval ~result env positive t1 op t2
        with
          | Predicate_alarm -> env
          | Reduce_to_bottom ->
              overwrite_current_state env Cvalue.Model.bottom
              (* if the exception was obtained without an alarm emitted,
                 it is correct to return the bottom state *)
      end

    | _,Pvalid ({ term_node = TLval _} as t) ->
      begin
        try
          let l = eval_tlval env result t in
          let aux env (typ, lval) =
            let loc = make_loc lval (Bit_utils.sizeof typ) in
            if valid_cardinal_zero_or_one ~for_writing:false loc then
              let state =
                reduce_by_valid_loc ~positive ~for_writing:false
                  loc typ (env_current_state env)
              in
              overwrite_current_state env state
            else env
          in
          List.fold_left aux env l
        with Predicate_alarm -> env
      end
    | _, Pvalid _ -> env (* no way to reduce for now. *)

    | _,Pinitialized tsets ->
        begin try
          let locb = eval_term env result tsets in
          List.fold_left
            (fun env (e, loc) ->
              let state = reduce_by_initialized_loc ~with_alarms:warn_raise_mode
                ~positive (e, loc) (env_current_state env)
              in
              overwrite_current_state env state
            ) env locb
          with
            | Predicate_alarm -> env
        end
    | _,Pat _ -> env
    | _,Papp _
    | _,Pexists (_, _) | _,Pforall (_, _)
    | _,Pvalid_range (_, _, _)| _,Pvalid_index (_, _)
    | _,Plet (_, _) | _,Pif (_, _, _)
    | _,Pfresh _  | _,Psubtype _
    | _, Pseparated _
        -> env
  in
  result

and reduce_by_relation eval ~result env positive t1 rel t2 =
  let env = reduce_by_left_relation eval ~result env positive t1 rel t2 in
  let inv_binop = match rel with
    | Rgt -> Rlt | Rlt -> Rgt | Rle -> Rge | Rge -> Rle
    | _ -> rel
  in
  reduce_by_left_relation eval ~result env positive t2 inv_binop t1

and reduce_by_left_relation eval ~result env positive tl rel tr =
  let with_alarms = warn_raise_mode in
  try
    let state = env_current_state env in
    let typ_loc, loc = eval_term_as_exact_loc env result tl in
    let value_for_loc =
      Cvalue.Model.find ~conflate_bottom:true ~with_alarms state loc in
    let value_for_loc = do_cast ~with_alarms typ_loc value_for_loc in
    let cond_v =
      List.fold_left  (fun v (_, v') -> Location_Bytes.join v v')
        Location_Bytes.bottom (eval_term env result tr)
    in
    let op = lop_to_cop rel in
    let v_sym = eval.eval_symetric positive op cond_v value_for_loc in
    let v_asym = eval.eval_antisymetric ~typ_loc positive op cond_v v_sym in
    if V.equal v_asym V.bottom then raise Reduce_to_bottom;
    if V.equal v_asym value_for_loc
    then env
    else 
      let state' = Cvalue.Model.reduce_binding ~with_alarms state loc v_asym in
      overwrite_current_state env state'
  with Predicate_alarm | Not_an_exact_loc -> env


let rec eval_predicate ~result env pred =
  let rec do_eval env p =
    match p.content with
    | Ptrue -> True
    | Pfalse -> False
    | Pand (p1,p2 ) ->
        begin match do_eval env p1 with
        | True -> do_eval env p2
        | False -> False
        | Unknown ->
          let reduced = reduce_by_predicate ~result env true p1 in
          match do_eval reduced p2 with
            | False -> False
            | _ -> Unknown
        end
    | Por (p1,p2 ) ->
        let val_p1 = do_eval env p1 in
        (*Format.printf "Disjunction: state %a p1:%a@."
            Cvalue.Model.pretty (env_current_state env)
            Cil.d_predicate_named p1; *)
        begin match val_p1 with
        | True -> True
        | False -> do_eval env p2
        | Unknown -> begin
          let reduced_state = reduce_by_predicate ~result env false p1 in
          (* Format.printf "Disjunction: reduced to %a to eval %a@."
             Cvalue.Model.pretty (env_current_state reduced_state)
             Cil.d_predicate_named p2; *)
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
        let _env = { env with e_cur = convert_label lbl } in
        do_eval env p
      end
    | Papp _ -> Unknown
    | Pvalid tsets -> begin
        try
          List.iter
            (fun (typ, loc) ->
              if not (isPointerType typ)
              then raise Predicate_alarm (* TODO: global arrays *);
              let size = sizeof_pointed typ in
              let loc = loc_bytes_to_loc_bits loc in
              let loc = Locations.make_loc loc size in
              if not (Locations.is_valid ~for_writing:false loc) then (
                (* Maybe the location is guaranteed to be invalid? *)
                (if Locations.cardinal_zero_or_one loc then
                    let valid = valid_part ~for_writing:false loc in
                    if Location_Bits.equal Location_Bits.bottom valid.loc
                    then raise Stop;
                );
                raise Predicate_alarm
              ))
            (eval_term env result tsets);
          True
        with
          | Predicate_alarm -> Unknown
          | Stop -> False
      end
    | Pinitialized tsets -> begin
        try
          let locb = eval_term env result tsets in
          fold_join_predicate List.fold_left
            (fun (typ, loc) ->
               let locbi = loc_bytes_to_loc_bits loc in
               let size = match unrollType typ with
                 | TPtr (t, _) -> bitsSizeOf t
                 | _ -> assert false
               in
               let loc = make_loc locbi (Int_Base.inject (Int.of_int size)) in
               let value = Cvalue.Model.find_unspecified
                 ~with_alarms:CilE.warn_none_mode (env_current_state env) loc
               in
               match value with
                 | Cvalue.V_Or_Uninitialized.C_uninit_esc v
                 | Cvalue.V_Or_Uninitialized.C_uninit_noesc v ->
                     if Location_Bytes.is_bottom v then False else Unknown
                 | Cvalue.V_Or_Uninitialized.C_init_esc _
                 | Cvalue.V_Or_Uninitialized.C_init_noesc _ -> True
            ) locb
        with
          | Cannot_find_lv
          | Predicate_alarm -> Unknown
      end
    | Prel (op,t1,t2) -> begin
        try
          let t = t1.term_type in (* TODO: t1.term_type and t2.term_type are
            sometimes different (Z vs. int, double vs. R, attribute const added,
            etc). I think it is always correct to use any of the two types,
            but I am not 100% sure *)
          let trm = Logic_const.term (TBinOp (lop_to_cop op, t1, t2)) t in
          let l = List.map snd (eval_term env result trm) in
          if List.for_all
            (Location_Bytes.equal Location_Bytes.singleton_zero) l
          then False
          else if List.for_all
            (Location_Bytes.equal Location_Bytes.singleton_one) l
          then True
          else Unknown
        with
          | Predicate_alarm -> Unknown
        end
    | Pexists (varl, p1) | Pforall (varl, p1) ->
        let result =
          begin try
          let env = List.fold_left
            (fun acc var ->
               match var.lv_origin with
               | None -> raise Exit
               | Some vi ->
                   let loc = loc_of_varinfo vi in
                   let state =
                     Cvalue.Model.add_binding
                       ~with_alarms:warn_raise_mode ~exact:true
                       (env_current_state acc) loc Location_Bytes.top
                   in
                   overwrite_current_state env state
            ) env varl
          in
          do_eval env p1
        with
          Exit -> Unknown
        | Predicate_alarm -> Unknown
        end
        in
        begin match p.content with
        | Pexists _ -> if result = False then False else Unknown
        | Pforall _ -> if result = True then True else Unknown
        | _ -> assert false
        end

    | Pnot p ->  begin match do_eval env p with
      | True -> False
      | False -> True
      | Unknown -> Unknown
      end
    | Pimplies (p1,p2) ->
        do_eval env (Logic_const.por ((Logic_const.pnot p1), p2))
    | Pseparated (_tset_l) -> Unknown
    | Pfresh _
    | Pvalid_range (_, _, _)| Pvalid_index (_, _)
    | Plet (_, _) | Pif (_, _, _)
    | Psubtype _
        -> Unknown
  in
  do_eval env pred

exception Does_not_improve

let rec fold_on_disjunction f p acc =
  match p.content with
  | Por (p1,p2 ) -> fold_on_disjunction f p2 (fold_on_disjunction f p1 acc)
  | _ -> f p acc

let count_disjunction p = fold_on_disjunction (fun _pred -> succ) p 0

let reduce_by_disjunction ~result ~env states n p =
  if State_set.is_empty states
  then states
  else if (State_set.length states) * (count_disjunction p) <= n
  then begin
      let treat_state acc state =
        let env = overwrite_current_state env state in
        let treat_pred pred acc =
          let result = reduce_by_predicate ~result env true pred in
          if Cvalue.Model.equal (env_current_state result) state
          then raise Does_not_improve
          else State_set.add (env_current_state result) acc
        in
        try
          fold_on_disjunction treat_pred p acc
        with
          Does_not_improve -> State_set.add state acc
      in
      State_set.fold treat_state State_set.empty states
    end
  else
    State_set.fold
      (fun acc state ->
        let env = overwrite_current_state env state in
        let reduced = reduce_by_predicate ~result env true p in
        State_set.add (env_current_state reduced) acc)
      State_set.empty
      states

module ActiveBehaviors = struct

  let header b =
    if Cil.is_default_behavior b then ""
    else ", behavior " ^ b.b_name

  let is_active_aux init_states b =
    let assumes =
      (Logic_const.pands
         (List.map Logic_const.pred_of_id_pred b.b_assumes))
    in
    fold_join_predicate State_set.fold
      (fun init -> eval_predicate ~result:None (env_pre_f init) assumes)
      init_states

  type t = {
    init_states: State_set.t;
    funspec: funspec;
    is_active: funbehavior -> predicate_value
  }

  module HashBehaviors = Hashtbl.Make(
    struct
      type t = funbehavior
      let equal b1 b2 = b1.b_name = b2.b_name
      let hash b = Hashtbl.hash b.b_name
    end)

  let create init_states kf =
    let funspec = Kernel_function.get_spec kf in
    let h = HashBehaviors.create 3 in
    { is_active =
        (fun b ->
           try HashBehaviors.find h b
           with Not_found ->
             let active = is_active_aux init_states b in
             HashBehaviors.add h b active;
             active
        );
      init_states = init_states;
      funspec = funspec;
    }

  let active ba = ba.is_active

  let is_active ba b = active ba b != False

  exception No_such_behavior

  let behavior_from_name ab b =
    try List.find (fun b' -> b'.b_name = b) ab.funspec.spec_behavior
    with Not_found -> raise No_such_behavior

  let active_behaviors ab =
    List.filter (is_active ab) ab.funspec.spec_behavior

  (* Is the behavior b the only one currently active. Check if it is in a
     group of complete behaviors, and the only one active in its group.
     TODO: we should also check that we can prove the 'complete' clause *)
  let only_active ab b =
    assert (is_active ab b);
    let none_other_active group =
      let other_not_active b'_name =
        b'_name = b.b_name ||
        (let b' = behavior_from_name ab b'_name in not (is_active ab b'))
      in
      List.for_all other_not_active group
    in
    try
      let complete =
        List.find (List.mem b.b_name) ab.funspec.spec_complete_behaviors
      in
      none_other_active complete
    with
        Not_found | No_such_behavior -> false

end


let check_postconditions kf kinstr ~result ~slevel header ~init_state ~active_behaviors ~post_state kind behaviors =
  let fused_init = State_set.join init_state in
  (* TODO BY: not optimal in reduce_by_disjunction below *)
  let e_post =
    lazy (env_post_f ~post:(State_set.join post_state) ~pre:fused_init)
  in
  let incorporate_behavior state b =
    if b.b_post_cond = [] then state
    else
      let header = header ^ ActiveBehaviors.header b in
      let posts = List.filter (fun (x,_) -> x = kind) b.b_post_cond in
      let update_status st post =
        let ip = Property.ip_of_ensures kf kinstr b post in
        emit_status ip st
      in
      match ActiveBehaviors.active active_behaviors b with
      | True ->
        List.fold_left
          (fun acc (_,{ip_content=pred;ip_loc=loc} as post) ->
	    let source = fst loc in
            if State_set.is_empty acc then
	      (Value_parameters.result ~once:true ~source
                 "%s: no state left to evaluate postcondition, status not computed.%t"
	         header pp_callstack;
               acc)
            else
              let pred = Ast_info.predicate loc pred in
              let res = fold_join_predicate State_set.fold
                (fun state ->
                  let env = env_post_f ~post:state ~pre:fused_init in
                  eval_predicate ~result env pred) acc
              in
	      Value_parameters.result ~once:true ~source
                "%s: postcondition got status %a.%t"
	        header pretty_predicate_value res pp_callstack;
              match res with
                | False ->
                    update_status Property_status.False_if_reachable post;
	          State_set.empty
                | True ->
                    update_status Property_status.True post;
                    (* The reduction is needed in the True case,
                       because the function is "reduce_by_disjunction".
                       Example: //@ assert x<0 || x>=0; *)
                    reduce_by_disjunction ~result ~env:!!e_post acc slevel pred
                | Unknown ->
                    update_status Property_status.Dont_know post;
                    reduce_by_disjunction ~result ~env:!!e_post acc slevel pred
          ) state posts
      | Unknown ->
        List.fold_left
          (fun acc (_,{ip_content=pred;ip_loc=loc} as post) ->
	    let source = fst loc in
            if State_set.is_empty acc then
	      (Value_parameters.result ~once:true ~source
                 "%s: no state left to evaluate postcondition, status not computed.%t"
	         header pp_callstack;
               acc)
            else
              let pred = Ast_info.predicate loc pred in
              let res = fold_join_predicate State_set.fold
                (fun state ->
                  let env = env_post_f ~post:state ~pre:fused_init in
                  eval_predicate ~result env pred)
                acc
              in
              Value_parameters.result ~once:true ~source
                "%s: postcondition got status %a.%t"
                header pretty_predicate_value res pp_callstack;
              match res with
                | Unknown | False ->
                    update_status Property_status.Dont_know post;
                    Value_parameters.result ~once:true ~source
                      "%s: postcondition got status %a, \
but it is unknown if the behavior is active.%t"
                      header pretty_predicate_value res pp_callstack;
                    state
                | True ->
                    update_status Property_status.True post;
                    Value_parameters.result ~once:true ~source
                      "%s: postcondition got status %a, \
but it is unknown if the behavior is active.%t"
                      header pretty_predicate_value res pp_callstack;
                    state
          ) state posts
      | False ->
        (* if assumes is false, post-condition status is not updated *)
        (match posts with
        | [] -> ()
        | (_,{ip_loc=(source,_)})::_ ->
          Value_parameters.result ~once:true ~source
            "%s: assumes got status invalid; post-condition not evaluated.%t"
            header pp_callstack);
        state
  in
  List.fold_left incorporate_behavior post_state behaviors

let check_fct_postconditions ~result kf ~init_state ~active_behaviors 
    ~post_state kind =
  try
    let spec = (Kernel_function.get_spec kf).spec_behavior in
    let slevel = get_slevel kf in
    check_postconditions kf Kglobal ~result ~slevel
      (Pretty_utils.sfprintf "Function %a@?" Kernel_function.pretty kf)
      ~init_state ~active_behaviors ~post_state kind spec
  with Not_found -> post_state

let check_preconditions kf kinstr ~slevel header active_behaviors 
    init_state spec =
  let env = env_pre_f (State_set.join init_state) in
  let incorporate_behavior states b =
    if b.b_requires = [] then states
    else
      let header = header ^ ActiveBehaviors.header b in
      let update_status st vc =
        let ip = Property.ip_of_requires kf kinstr b vc in
        emit_status ip st
      in
      match ActiveBehaviors.active active_behaviors b with
      | True ->
        List.fold_left (fun state ({ip_content=pr;ip_loc=loc} as pre) ->
	  let source = fst loc in
          if State_set.is_empty state then
	    (Value_parameters.result ~once:true ~source
               "%s: no state in which to evaluate precondition, status not computed.%t"
	       header pp_callstack;
             state)
          else
            let pr = Ast_info.predicate loc pr in
            let res = fold_join_predicate State_set.fold
              (fun state ->
                eval_predicate ~result:None (env_pre_f state) pr)
              state
            in
	    Value_parameters.result ~once:true ~source
              "%s: precondition got status %a.%t"
              header pretty_predicate_value res pp_callstack;
            match res with
              | False ->
                  update_status Property_status.False_if_reachable pre;
                  State_set.empty
              | True ->
                  update_status Property_status.True pre;
                  (* The reduction is needed in the True case,
                     because the function is "reduce_by_disjunction".
                     Example: //@ assert x<0 || x>=0; *)
                  reduce_by_disjunction ~result:None ~env state slevel pr
              | Unknown ->
                  update_status Property_status.Dont_know pre;
                  reduce_by_disjunction ~result:None ~env state slevel pr
        ) states b.b_requires
      | Unknown ->
        List.fold_left
          (fun state ({ip_content=pr;ip_loc=loc} as pre) ->
	    let source = fst loc in
            if State_set.is_empty state then
	      (Value_parameters.result ~once:true ~source
                 "%s: no state in which to evaluate precondition, status not computed.%t"
	         header pp_callstack;
               state)
            else
              let pr = Ast_info.predicate loc pr in
              let res = fold_join_predicate State_set.fold
                (fun state ->
                  eval_predicate ~result:None (env_pre_f state) pr)
                state
              in
              Value_parameters.result ~once:true ~source:(fst loc)
                "%s: precondition got status %a.%t"
                header pretty_predicate_value res pp_callstack;
              match res with
                | Unknown | False ->
                    update_status Property_status.Dont_know pre;
                    Value_parameters.result ~once:true ~source
                      "%s: precondition got status %a, \
but it is unknown if the behavior is active.%t"
                      header pretty_predicate_value res pp_callstack;
                    state
                | True ->
                    update_status Property_status.True pre;
                    Value_parameters.result ~once:true ~source
		      "%s: precondition got status %a.%t"
		      header pretty_predicate_value res pp_callstack;
                    state
          ) states b.b_requires
      | False ->
        (* if assumes is false, post-condition status is not updated *)
        (match b.b_requires with
        | [] -> ()
        | {ip_loc=(source,_)}::_ ->
          Value_parameters.result ~once:true ~source
            "%s: assumption got status invalid; precondition not evaluated.%t"
            header pp_callstack);
        states
  in
  List.fold_left
    incorporate_behavior
    init_state spec.spec_behavior

(** Check the precondition of [kf]. This may result in splitting [init_state]
    into multiple states if the precondition contains disjunctions. The active
    behaviors are computed wrt [init_state], but further computations on [kf]
    will use active behaviors computed wrt the result of this function. *)
let check_fct_preconditions kf init_state =
  let init_states = State_set.singleton init_state in
  try
    let spec = Kernel_function.get_spec kf in
    let slevel = get_slevel kf in
    let active_behaviors = ActiveBehaviors.create init_states kf in
    check_preconditions kf Kglobal ~slevel
      (Pretty_utils.sfprintf "Function %a@?" Kernel_function.pretty kf)
      active_behaviors init_states spec
  with Not_found -> init_states


let () =
  Db.Value.valid_behaviors :=
    (fun kf state ->
      let ab = ActiveBehaviors.create (State_set.singleton state) kf in
      ActiveBehaviors.active_behaviors ab
    );
  Db.Properties.Interp.loc_to_loc :=
    (fun ~result state t ->
      try eval_tlval_as_location (env_pre_f state) result t
      with Predicate_alarm -> raise (Invalid_argument "not an lvalue")
    )


(*
Local Variables:
compile-command: "make -C ../.."
End:
*)
