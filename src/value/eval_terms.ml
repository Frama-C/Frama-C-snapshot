(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2012                                               *)
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
open Eval_exprs
open Locations
open Abstract_interp
open Cvalue
open Bit_utils


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

(* Type of possible errors during evaluation. See pretty-printer for details *)
type logic_evaluation_error =
    | Unsupported of string
    | AstError of string
    | NoEnv of string
    | NoResult
    | CAlarm

let pretty_logic_evaluation_error fmt = function
  | Unsupported s -> Format.fprintf fmt "unsupported ACSL construct: %s" s
  | AstError s -> Format.fprintf fmt "error in AST: %s; please report" s
  | NoEnv s -> Format.fprintf fmt "no environment to evaluate \\at(%s,_)" s
  | NoResult -> Format.fprintf fmt "meaning of \\result not specified"
  | CAlarm -> Format.fprintf fmt "alarm during evaluation"

exception LogicEvalError of logic_evaluation_error

let unsupported s = raise (LogicEvalError (Unsupported s))
let ast_error s = raise (LogicEvalError (AstError s))
let no_env env = raise (LogicEvalError (NoEnv env))
let no_result () = raise (LogicEvalError NoResult)
let c_alarm () = raise (LogicEvalError CAlarm)

let display_evaluation_error = function
  | CAlarm -> ()
  | pa ->
    Value_parameters.result ~once:true ~current:true
      "cannot evaluate ACSL term, %a" pretty_logic_evaluation_error pa


let singleton = function
  | [e] -> e
  | [] | _ :: _ :: _ -> Value_parameters.fatal "Found set instead of singleton"

let warn_raise_mode =
  { CilE.imprecision_tracing = CilE.Aignore ;
    defined_logic = CilE.Aignore;

    unspecified = CilE.Acall c_alarm;
    others = CilE.Acall c_alarm ;
 }


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

(* TODO: evaluating correctly Pat with the current Value domain is tricky,
   and only works reliably for the four labels below, that are either
   invariant during the course of the program, or fully local. The
   program below shows the problem:
   if (c) x = 1; else x = 3;
   L:
   x = 1;
   \assert \at(x == 1, L);
   A naïve implementation of assertions involving C labels is likely to miss
   the fact that the assertion is false after the else branch. A good
   solution is to use a dummy edge that flows from L to the assertion,
   to force its re-evaluation.
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

let extract_opt_env env = function
  | Some v -> v
  | None -> no_env env

let convert_label = function
  | StmtLabel _ -> unsupported "\\at() on a C label"
  | LogicLabel (_, "Pre")  -> Pre
  | LogicLabel (_, "Here") -> Here
  | LogicLabel (_, "Old")  -> Old
  | LogicLabel (_, "Post") -> Post
  | LogicLabel _ -> unsupported "\\at() on an unsupported logic label"

let env_state env = function
  | Pre  -> env.e_pre
  | Here -> env.e_here
  | Old  -> extract_opt_env "Pre" env.e_old
  | Post -> extract_opt_env "Post" env.e_post

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

(* Types currently understood in the evaluation of the logic: no arrays,
   structs, logic arrays or subtle ACSL types. Sets of sets seem to
   be flattened, so the current treatment of them is correct. *)
let rec isLogicNonCompositeType t =
  match t with
    | Lvar _ | Larrow _ -> false
    | Ltype _ ->
        Logic_const.is_boolean_type t ||
          (try isLogicNonCompositeType (Logic_const.type_of_element t)
           with Failure _ -> false)
    | Linteger | Lreal -> true
    | Ctype t ->
      match Cil.unrollType t with
        | TInt _ | TEnum _ | TFloat _ | TPtr _ -> true
        | _ -> false

let rec eval_term ~with_alarms env result t =
  match t.term_node with
    | Tat (t, lab) -> begin
        let lab = convert_label lab in
        eval_term ~with_alarms { env with e_cur = lab } result t
      end

    | TConst (Integer (v, _)) -> [intType, Cvalue.V.inject_int v]
    | TConst (LEnum e) ->
        (match (constFold true e.eival).enode with
           | Const (CInt64 (v, _, _)) -> [intType, Cvalue.V.inject_int v]
           | _ -> ast_error "non-evaluable constant")
    | TConst (LChr c) -> 
      [intType, Cvalue.V.inject_int (Int.of_int (int_of_char c))]
	(* TODO : not compatible with ISO C interpretation of chars 
	   along Cil.interpret_character_constant *)
    | TConst (LReal (f, _)) ->
        let f = Ival.F.of_float f in
        let _, f = Ival.Float_abstract.inject_r f f in
	(* TODO : check that the float injection is compatible 
	   with the real number represente din the string. *)
        [floatType, Cvalue.V.inject_ival (Ival.inject_float f)]
(*  | TConst ((CStr | CWstr) Missing cases *)

    | TAddrOf _
    | TStartOf _ ->
        let conv (typ, loc) = (typ, loc_bits_to_loc_bytes loc) in
        List.map conv (eval_tlval ~with_alarms env result t)

    | TLval _ ->
        let lvals = eval_tlval ~with_alarms env result t in
        let eval_lval (typ, loc) =
          let v = Cvalue.Model.find ~conflate_bottom:true
            ~with_alarms (env_current_state env)
            (make_loc loc (Bit_utils.sizeof typ))
          in
          let v = do_cast ~with_alarms typ v in
          (typ, v)
        in
        List.map eval_lval lvals

    (* TBinOp ((LOr | LAnd), _t1, _t2) -> TODO: a special case would be useful.
       But this requires reducing the state after having evaluated t1 by
       a term that is in fact a predicate *)       
    | TBinOp (op,t1,t2) -> begin
      if isLogicNonCompositeType t1.term_type then
        let typ_bool = Cil.intType (* No special representation in Value *) in
        let l1 = eval_term ~with_alarms env result t1 in
        let l2 = eval_term ~with_alarms env result t2 in
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
            | _ -> ast_error (Pretty_utils.sfprintf
                                "binop on incorrect type %a" Cil.d_type te1)
          in
          let check_types = false in
          let typ_res = match op with
            | PlusA | MinusA | Mult | Div -> te1
            | PlusPI | MinusPI | IndexPI ->
                if check_types then assert (Cil.isPointerType te1);
                te1
            | MinusPP -> Cil.intType
            | Mod | Shiftlt | Shiftrt | BAnd | BXor | BOr ->
                (* can only be applied on integral arguments *)
                if check_types then
                  assert (Cil.isLogicIntegralType t1.term_type &&
                          Cil.isLogicIntegralType t2.term_type);
                Cil.intType
            | Lt | Gt | Le | Ge | Eq | Ne | LAnd | LOr ->
                typ_bool (* those operators always return a boolean *)
          in
          (typ_res, v)
        in
        match op, l1, l2 with
          | (PlusA | PlusPI | IndexPI | MinusA | MinusPI), _, _ ->
              List.fold_left (fun acc e1 ->
                List.fold_left (fun acc e2 -> aux e1 e2 :: acc) acc l2) [] l1

          | (Eq | Ne), _ , _ -> begin(*TODO: could be improved, but worth it?*)
              match l1, l2 with
              | [], [] ->
                  [typ_bool, Cvalue.V.singleton_one]
              | [], _ :: _ | _ :: _, [] ->
                  [typ_bool, Cvalue.V.singleton_zero]
              | h1 :: q1, h2 :: q2 ->
                let join (t1, v1) (t2, v2) =
                  if Cil_datatype.Typ.equal t1 t2 then
                    (t1, V.join v1 v2)
                  else ast_error (Pretty_utils.sfprintf
                       "non-homogeneous set %a-%a" Cil.d_type t1 Cil.d_type t2)
                in
                let e1 = List.fold_left join h1 q1 in
                let e2 = List.fold_left join h2 q2 in
                let _, r = aux e1 e2 in
                let contains_zero, contains_non_zero =
                  V.contains_zero r, V.contains_non_zero r
                in
                [typ_bool, V.interp_boolean ~contains_zero ~contains_non_zero]
            end
  
          | _, [e1], [e2] -> [aux e1 e2]
          | _ -> ast_error "meaningless binop"
      else
        unsupported (Pretty_utils.sfprintf
                       "%a operation on non-supported type %a" Cil.d_binop op
                       Cil.d_logic_type t1.term_type)
      end

    | TUnOp (op, t) ->
        let l = eval_term ~with_alarms env result t in
        let typ' t = match op with
          | Neg -> t
          | BNot -> t (* can only be used on an integer type *)
          | LNot -> intType
        in
        let eval typ v =
          eval_unop ~check_overflow:false ~with_alarms v typ op
        in
        List.map (fun (typ, v) -> typ' typ, eval typ v) l

    | Trange (otlow, othigh) ->
        (* Eval one bound. `SureInf corresponds to an ACSL 'omitted bound',
           `MayInf to a value analysis approximation. There are subtle
           differences between, that are not completely exploited for now. *)
        let eval = function
          | None -> `SureInf
          | Some t ->
              let (typ, v) = singleton (eval_term ~with_alarms env result t) in
              if not (isIntegralType typ)
              then ast_error "non-integer range bound";
              try (match Ival.min_and_max (Cvalue.V.project_ival v) with
                | None, _ | _, None -> `MayInf
                | Some l, Some h -> `Finite (l, h)
              )
              with Cvalue.V.Not_based_on_null -> `MayInf
        in
        let range low high =
          (intType, Cvalue.V.inject_ival (Ival.inject_range low high)) in
        let inj_int i = (intType, Cvalue.V.inject_int i) in
        let r = match eval otlow, eval othigh with
          | `Finite (ilowlow, ilow), `Finite (ihigh, ihighhigh) ->
              if Int.gt ilowlow ihighhigh then []
              else
                if Int.equal ilowlow ihighhigh then
                  if Int.equal ilowlow ilow && Int.equal ihigh ihighhigh
                  then [inj_int ilow]
                  else (* complicated case. Due to the imprecisions, the range
                     might be empty, but the intersection is a single integer,
                     which is considered precise by all the other functions *)
                    c_alarm () (* TODO. (but what?) *)
                else
                  let middle = (* Compute elements that are guaranteed to
                                  be in the range, if possible one by one *)
                    if Int.ge ihigh ilow then
                      let plevel = Value_parameters.ArrayPrecisionLevel.get ()in
                      if Int.equal ilow ihigh then [inj_int ilow]
                      else
                        if Int.le (Int.sub ihigh ilow) (Int.of_int plevel) then
                          let rec enum i acc =
                            if Int.lt i ilow then acc
                            else enum (Int.sub i Int.one) (inj_int i :: acc)
                          in enum ihigh []
                        else [range (Some ilow) (Some ihigh)]
                    else []
                  in
                  if Int.equal ilowlow ilow && Int.equal ihigh ihighhigh
                  then middle
                  else range (Some ilowlow) (Some ihighhigh) :: middle
                    (* TODO: improve. Returning middle kills a lot of
                       possible reductions *)
          (* If an 'exact' flag is added to the evaluation of the logic, the
             code below must be rewritten as follows:
             `MayInf, `Finite (_h, hh) -> [(None, hh, inexact)]
             `SureInf, `Finite (h, hh) -> [(None, h, exact); (h, hh, inexact)]*)
          | (`MayInf | `SureInf), `Finite (_ihigh, ihighhigh) ->
               [range None (Some ihighhigh)]
          | `Finite (ilowlow, _ilow), (`MayInf | `SureInf) ->
                [range (Some ilowlow) None]
          | (`MayInf | `SureInf), (`MayInf | `SureInf) -> [range None None]
        in
        (*Value_parameters.debug "Range %a: %a@."
          d_term t (Pretty_utils.pp_list Cvalue.V.pretty) (List.map snd r);*)
        r

    | TCastE (typ, t) ->
        let l = eval_term ~with_alarms env result t in
        List.map (fun (_, v) -> typ, do_cast ~with_alarms typ v) l

    | Tif (tcond, ttrue, tfalse) ->
        let l = eval_term ~with_alarms env result tcond in
        let vtrue =  List.exists (fun (_, v) -> Cvalue.V.contains_non_zero v) l
        and vfalse = List.exists (fun (_, v) -> Cvalue.V.contains_zero v) l  in
        (if vtrue then eval_term ~with_alarms env result ttrue else [])
        @ (if vfalse then eval_term ~with_alarms env result tfalse else [])

    | TSizeOf _ | TSizeOfE _ | TSizeOfStr _ | TAlignOf _ | TAlignOfE _ ->
        let e = Cil.constFoldTerm true t in
        let r = match e.term_node with
          | TConst (Integer (v, _)) -> Cvalue.V.inject_int v
          | _ -> V.top_int
        in
        [intType, r]

    | Tunion l ->
        List.fold_left (fun r t -> eval_term ~with_alarms env result t @ r) [] l

    | Tempty_set -> []

    | Tnull -> [Cil.voidPtrType, Cvalue.V.singleton_zero]

    (* TODO *)
    | Toffset _ -> unsupported "\\offset function"
    | Tbase_addr _ -> unsupported "\\base_addr function"
    | Tblock_length _ -> unsupported "\\block_length function"

    | Tinter _ -> unsupported "set intersection" (* TODO *)

    | Tapp _ | Tlambda _ -> unsupported "logic functions or predicates"
    | TDataCons _ -> unsupported "logic inductive types"
    | TUpdate _ -> unsupported "functional updates"
    | TCoerce _ | TCoerceE _ -> unsupported "logic coercions" (* jessie *)
    | Ttype _ -> unsupported "\\type operator"
    | Ttypeof _ -> unsupported "\\typeof operator"
    | Tcomprehension _ -> unsupported "sets defined by comprehension"
    | Tlet _ -> unsupported "\\let bindings"
    | TConst (LStr _) -> unsupported "constant strings"
    | TConst (LWStr _) -> unsupported "wide constant strings"


and eval_tlhost ~with_alarms env result lv =
  match lv with
    | TVar { lv_origin = Some v } ->
        let loc = Location_Bits.inject (Base.find v) Ival.zero in
        [v.vtype, loc]
    | TResult typ ->
        (match result with
          | Some v ->
              let loc = Location_Bits.inject (Base.find v) Ival.zero in
              [typ, loc]
          | None -> no_result ())
    | TVar { lv_origin = None } -> (* TODO: add an env for logic vars *)
        unsupported "evaluation of logic vars"
    | TMem t ->
        let l = eval_term ~with_alarms env result t in
        List.map (fun (t, loc) ->
          match Cil.unrollType t with
            | TPtr (t, _) -> t, loc_bytes_to_loc_bits loc
            | _ -> ast_error "*p where p is not a pointer"
        ) l

and eval_toffset ~with_alarms env result typ toffset =
  match toffset with
  | TNoOffset ->
      [typ, Ival.singleton_zero]
  | TIndex (trm, remaining) ->
      let typ_pointed = match unrollType typ with
        | TArray (t, _, _, _) -> t
        | TPtr(t,_) ->
            (match unrollType t with
              | TArray (t, _,_,_) -> t
              | _ -> ast_error "index on a non-array")
        | _ -> ast_error "index on a non-array"
      in
      let lloctrm = eval_term ~with_alarms env result trm in
      let aux (_typ, current) =
        let offset =
          try Cvalue.V.project_ival current
          with Cvalue.V.Not_based_on_null -> Ival.top
        in
        let loffsrem =
          eval_toffset ~with_alarms env result typ_pointed remaining in
        let aux (typ, r) =
          let offset = Ival.scale_int64base (sizeof typ_pointed) offset in
          typ, Ival.add_int offset r
        in
        List.map aux loffsrem
      in
      List.fold_left (fun acc trm -> aux trm @ acc) [] lloctrm

  | TField (fi,remaining) ->
      let current,_ = bitsOffset typ (Field(fi,NoOffset)) in
      let loffs = eval_toffset ~with_alarms env result fi.ftype remaining in
      List.map (fun (typ, r) -> typ, Ival.add_int (Ival.of_int current) r) loffs

  | TModel _ -> unsupported "model fields"

and eval_tlval ~with_alarms env result t =
  let process ftyp tlval toffs =
    let lvals = eval_tlhost ~with_alarms env result tlval in
    let aux acc (typ, loc) =
      let loffset = eval_toffset ~with_alarms env result typ toffs in
      let aux acc (typ_offs, offs) =
        let loc = Location_Bits.location_shift offs loc in
        (ftyp typ_offs, loc) :: acc
      in
      List.fold_left aux acc loffset
    in
    List.fold_left aux [] lvals
  in
  match t.term_node with
  | TAddrOf (tlval, toffs) ->
      process (fun typ -> TPtr (typ, [])) tlval toffs
  | TStartOf (tlval, toffs) ->
      process (fun typ -> TPtr (Cil.typeOf_array_elem typ, [])) tlval toffs
  | TLval (tlval, toffs) ->
      process (fun typ -> typ) tlval toffs
  | Tunion l -> List.concat (List.map (eval_tlval ~with_alarms env result) l)
  | Tempty_set -> []
  (* TODO: add support for TcastE, by adapting what is done for pass_cast
     in eval_exprs.ml *)
  | _ -> ast_error "non-lval term"

let eval_tlval_as_location ~with_alarms env result t =
  let l = eval_tlval ~with_alarms env result t in
  let aux acc (typ, loc) =
    let s = Bit_utils.sizeof typ in
    assert (loc_equal acc loc_bottom || Int_Base.equal s acc.size);
    make_loc (Location_Bits.join loc acc.loc) s
  in
  List.fold_left aux loc_bottom l

let eval_tlval_as_locations ~with_alarms env result t =
  let l = eval_tlval ~with_alarms env result t in
  let aux acc (typ, loc) =
    let s = Bit_utils.sizeof typ in
    let loc = make_loc loc s in
    loc :: acc
  in
  List.fold_left aux [] l

let eval_tlval_as_zone ~with_alarms ~for_writing env result t =
  let l = eval_tlval ~with_alarms env result t in
  let aux acc (typ, loc) =
    let s = Bit_utils.sizeof typ in
    let loc = make_loc loc s in
    let z = valid_enumerate_bits ~for_writing loc in
    Zone.join acc z
  in
  List.fold_left aux Zone.bottom l

exception Not_an_exact_loc

let eval_term_as_exact_loc ~with_alarms env result t =
  match t.term_node with
    | TLval _ ->
        (match eval_tlval ~with_alarms env result t with
           | [] | _ :: _ :: _ -> raise Not_an_exact_loc
           | [typ, loc] ->
               let loc = Locations.make_loc loc (Bit_utils.sizeof typ) in
               if not (valid_cardinal_zero_or_one ~for_writing:false loc)
               then raise Not_an_exact_loc;
               typ, loc
        )
    | _ -> raise Not_an_exact_loc


exception DoNotReduce

let rec reduce_by_predicate ~result env positive p =
  reduce_by_predicate_content ~result env positive p.content

and reduce_by_predicate_content ~result env positive p_content =
    let with_alarms = warn_raise_mode in
    match positive,p_content with
    | true,Ptrue | false,Pfalse -> env

    | true,Pfalse | false,Ptrue ->
        overwrite_current_state env Cvalue.Model.bottom

    (* desugared form of a <= b <= c <= d *)
    | true, Pand (
        {content=Prel ((Rlt | Rgt | Rle | Rge | Req as op),_ta,tb) as p1},
        {content=Pand (
          {content=Prel (op', tb',tc) as p2},
          {content=Prel (op'',tc',_td) as p3})})
        when
	  op = op' && op' = op'' &&
	  Logic_utils.is_same_term tb tb' &&
	  Logic_utils.is_same_term tc tc'
        ->
        let red env p = reduce_by_predicate_content ~result env positive p in
        let env = red env p1 in
        let env = red env p3 in
        let env = red env p2 in
        (*Not really useful in practice*)
      (*let env = red env (Prel (op, ta, tc)) in
        let env = red env (Prel (op, tb, td)) in *)
        env

    | true,Pand (p1,p2) | false,Por(p1,p2)->
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
          reduce_by_predicate_content ~result env true (Pand (p1, p2)) in
        let red2 =
          reduce_by_predicate_content ~result env false (Por (p1, p2)) in
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
                reduce_rel_float (Value_parameters.AllRoundingModes.get ())
            | t when isLogicIntegralType t -> reduce_rel_int
            | Ctype ct when isPtrType ct -> reduce_rel_int
            | _ -> raise DoNotReduce
          in
          reduce_by_relation eval ~result env positive t1 op t2
        with
          | DoNotReduce -> env
          | LogicEvalError ee -> display_evaluation_error ee; env
          | Reduce_to_bottom ->
              overwrite_current_state env Cvalue.Model.bottom
              (* if the exception was obtained without an alarm emitted,
                 it is correct to return the bottom state *)
      end

    | _,Pvalid (_label,tsets) ->
        (* Value's validity does not depend on the program point. the implicit
           label is intentionally ignored. This is however problematic for
           dynamic allocation. TODO *)
	reduce_by_valid ~result env positive ~for_writing:true  tsets
    | _,Pvalid_read (_label,tsets) ->
	reduce_by_valid ~result env positive ~for_writing:false tsets

    | _,Pinitialized (lbl_initialized,tsets) ->
        begin try
          let locb = eval_term ~with_alarms env result tsets in
          let lbl = convert_label lbl_initialized in
          let state = env_state env lbl in
          let state_reduced =
            List.fold_left
              (fun state (e, loc) ->
                 reduce_by_initialized_defined
                   (Cvalue.V_Or_Uninitialized.change_initialized positive)
                   (e, loc) state
              ) state locb
          in
          overwrite_state env state_reduced lbl 
          with
            | LogicEvalError ee -> display_evaluation_error ee; env
        end

    | _,Pat (p, lbl) ->
        (try
           let env_at = { env with e_cur = convert_label lbl } in
           let env' = reduce_by_predicate ~result env_at positive p in
           { env' with e_cur = env.e_cur }
         with LogicEvalError ee -> display_evaluation_error ee; env)

    | _,Papp _
    | _,Pexists (_, _) | _,Pforall (_, _)
    | _,Plet (_, _) | _,Pif (_, _, _)
    | _,Pallocable (_,_) | _,Pfreeable (_,_) | _,Pfresh (_,_,_,_)  
    | _,Psubtype _
    | _, Pseparated _
        -> env

and reduce_by_valid ~result env positive ~for_writing (tset: term) =
  let with_alarms = warn_raise_mode in
  (* Auxiliary function that reduces \valid(lvloc+offs), where lvloc is atomic
     (no more tsets), and offs is a bits-expressed constant offset.
     [offs_typ] is supposed to be the type of the pointed location after [offs]
     has been applied; it can be different from [typeOf_pointed lv_typ], for
     example if offset is a field access. *)
  let aux (lv_typ, lvloc) env (offs_typ, offs) =
    try
      if not (Location_Bits.is_relationable lvloc) ||
         not (Ival.cardinal_zero_or_one offs)
      then raise DoNotReduce;
      let state = env_current_state env in
      let lvloc = make_loc lvloc (Bit_utils.sizeof lv_typ) in
      (* [p] is the range that we attempt to reduce *)
      let p_orig = Model.find ~with_alarms ~conflate_bottom:true state lvloc in
      let pb = Locations.loc_bytes_to_loc_bits p_orig in
      let shifted_p = Location_Bits.location_shift offs pb in
      let lshifted_p = make_loc shifted_p (Bit_utils.sizeof offs_typ) in
      let valid = (* reduce the shifted pointer to the wanted part *)
        if positive
        then Locations.valid_part ~for_writing lshifted_p
        else Locations.invalid_part lshifted_p
      in
      let valid = valid.loc in
      if Location_Bits.equal shifted_p valid
      then env
      else
	(* Shift back *)
	let shift = Ival.neg offs in
	let pb = Location_Bits.location_shift shift valid in
	let p = Locations.loc_bits_to_loc_bytes pb in
	(* Store the result *)
	let state = Model.reduce_previous_binding ~with_alarms state lvloc p in
	overwrite_current_state env state
    with
      | DoNotReduce | V.Not_based_on_null -> env
      | LogicEvalError ee -> display_evaluation_error ee; env
  in
  let rec do_one env t =
    match t.term_node with
      | Tunion l ->
          List.fold_left do_one env l

      | TLval _ -> 
          let aux env (typ, lval) =
            try
              let loc = make_loc lval (Bit_utils.sizeof typ) in
              if valid_cardinal_zero_or_one ~for_writing loc then
                let state =
                  reduce_by_valid_loc ~positive ~for_writing
                    loc typ (env_current_state env)
                in
                overwrite_current_state env state
              else env
            with LogicEvalError ee -> display_evaluation_error ee; env
          in
          (try 
             let l = eval_tlval ~with_alarms env result t in
             List.fold_left aux env l
           with LogicEvalError ee -> display_evaluation_error ee; env)

      | TAddrOf (TMem ({term_node = TLval _} as t), offs) ->
          (try
             let lt = eval_tlval ~with_alarms env result t in
             List.fold_left
               (fun env (typ, _ as lv) ->
                  (* Compute the offsets, that depend on the type of the lval.
                     The computed list is exactly what [aux] requires *)
                  let loffs = eval_toffset ~with_alarms env result typ offs in
                  List.fold_left (aux lv) env loffs) env lt
           with LogicEvalError ee -> display_evaluation_error ee; env)

      | TBinOp ((PlusPI | MinusPI) as op, ({term_node = TLval _} as tlv), i) ->
          (try
             let ltlv = eval_tlval ~with_alarms env result tlv in
             let li = eval_term ~with_alarms env result i in
             (* Convert offsets to a simpler form if [op] is [MinusPI] *)
             let li =
               List.fold_left
                 (fun acc (typ, offs) ->
                    try
                      let i = V.project_ival offs in
                      let i = if op = PlusPI then i else Ival.neg i in
                      (typ, i) :: acc
                    with V.Not_based_on_null -> acc
                 ) [] li
             in
             List.fold_left
               (fun env (typ, _ as elv) ->
                  (* Compute the offsets expected by [aux], which are
                     [i * 8 * sizeof( *tlv)] *)
                  let typ_p = Cil.typeOf_pointed typ in
                  let sbits = Int.of_int (Cil.bitsSizeOf typ_p) in
                  let li = List.map
                    (fun (_, offs) -> typ_p, Ival.scale sbits offs) li
                  in
                  List.fold_left (aux elv) env li) env ltlv
           with LogicEvalError ee -> display_evaluation_error ee; env)
      | _ -> env
  in
  do_one env tset

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
    let typ_loc, loc = eval_term_as_exact_loc ~with_alarms env result tl in
    let value_for_loc =
      Cvalue.Model.find ~conflate_bottom:true ~with_alarms state loc in
    let value_for_loc = do_cast ~with_alarms typ_loc value_for_loc in
    let cond_v =
      List.fold_left  (fun v (_, v') -> Location_Bytes.join v v')
        Location_Bytes.bottom (eval_term ~with_alarms env result tr)
    in
    let op = lop_to_cop rel in
    let warn_comp, _, _ = check_comparable op value_for_loc cond_v in
    if warn_comp then CilE.warn_pointer_comparison with_alarms;
    let v_sym = eval.reduce_rel_symetric positive op cond_v value_for_loc in
    let v_asym = eval.reduce_rel_antisymetric ~typ_loc positive op cond_v v_sym in
    if V.is_bottom v_asym then raise Reduce_to_bottom;
    if V.equal v_asym value_for_loc
    then env
    else 
      let state' = 
	Cvalue.Model.reduce_previous_binding ~with_alarms state loc v_asym
      in
      overwrite_current_state env state'
  with
    | Not_an_exact_loc -> env
    | LogicEvalError ee -> display_evaluation_error ee; env


let eval_predicate ~result env pred =
  let with_alarms = warn_raise_mode in
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
        try do_eval { env with e_cur = convert_label lbl } p
        with LogicEvalError ee -> display_evaluation_error ee; Unknown
      end

    | Pvalid (label, tsets)
    | Pvalid_read (label, tsets) -> begin
        try
          let for_writing =
            (match p.content with Pvalid_read _ -> false | _ -> true) in
          let state = env_state env (convert_label label) in
          let size = match Logic_utils.unroll_type tsets.term_type with
            | Ctype (TPtr _ |  TArray _ as t)
            | Ltype ({lt_name = "set"},[Ctype t]) -> sizeof_pointed t
            | _ -> ast_error "valid on incorrect location %a"
          in
          (* Check that the given location is valid *)
          let valid locbytes =
            let loc = loc_bytes_to_loc_bits locbytes in
            let loc = Locations.make_loc loc size in
            if not (Locations.is_valid ~for_writing loc) then (
              (* Maybe the location is guaranteed to be invalid? *)
              let valid = valid_part ~for_writing loc in
              if Location_Bits.equal Location_Bits.bottom valid.loc
              then raise Stop
              else raise DoNotReduce)
          in
          (match tsets.term_node with
             | TLval _ ->
                 (* Evaluate the left-value, and check that it is initialized
                    and not an escaping pointer *)
                 List.iter
                   (fun loc ->
                      let v = Model.find_unspecified ~with_alarms state loc in
                      let v, ok = match v with
                        | Cvalue.V_Or_Uninitialized.C_uninit_esc v
                        | Cvalue.V_Or_Uninitialized.C_uninit_noesc v
                        | Cvalue.V_Or_Uninitialized.C_init_esc v -> v, false
                        | Cvalue.V_Or_Uninitialized.C_init_noesc v -> v, true
                      in
                      if Cvalue.V.is_bottom v && not ok then raise Stop;
                      valid v;
                      if not ok then raise DoNotReduce
                   )
                   (eval_tlval_as_locations ~with_alarms env result tsets)
             | _ ->
                 List.iter (fun (_, loc) -> valid loc)
                   (eval_term ~with_alarms env result tsets)
          );
          True
        with
          | DoNotReduce -> Unknown
          | LogicEvalError ee -> display_evaluation_error ee; Unknown
          | Stop -> False
      end

    | Pinitialized (label,tsets) -> begin
        try
          let locb = eval_term ~with_alarms env result tsets in
          let state = env_state env (convert_label label) in
          fold_join_predicate List.fold_left
            (fun (typ, loc) ->
               let locbi = loc_bytes_to_loc_bits loc in
               if not (isPointerType typ) then
                 ast_error "initialized on incorrect location";
               let loc = make_loc locbi (sizeof_pointed typ) in
               let value = Model.find_unspecified ~with_alarms state loc in
               match value with
                 | Cvalue.V_Or_Uninitialized.C_uninit_esc v
                 | Cvalue.V_Or_Uninitialized.C_uninit_noesc v ->
                     if Location_Bytes.is_bottom v then False else Unknown
                 | Cvalue.V_Or_Uninitialized.C_init_esc _
                 | Cvalue.V_Or_Uninitialized.C_init_noesc _ -> True
            ) locb
        with
          | Cannot_find_lv -> Unknown
          | LogicEvalError ee -> display_evaluation_error ee; Unknown
      end
    | Prel (op,t1,t2) -> begin
        try
          let t = t1.term_type in (* TODO: t1.term_type and t2.term_type are
            sometimes different (Z vs. int, double vs. R, attribute const added,
            etc). I think it is always correct to use any of the two types,
            but I am not 100% sure *)
          let trm = Logic_const.term (TBinOp (lop_to_cop op, t1, t2)) t in
          let l = List.map snd (eval_term ~with_alarms env result trm) in
          if List.for_all
            (Location_Bytes.equal Location_Bytes.singleton_zero) l
          then False
          else if List.for_all
            (Location_Bytes.equal Location_Bytes.singleton_one) l
          then True
          else Unknown
        with
          | LogicEvalError ee -> display_evaluation_error ee; Unknown
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
                     Cvalue.Model.add_binding ~with_alarms ~exact:true
                       (env_current_state acc) loc Location_Bytes.top
                   in
                   overwrite_current_state env state
            ) env varl
          in
          do_eval env p1
        with
          | Exit -> Unknown
          | LogicEvalError ee -> display_evaluation_error ee; Unknown
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

    | Pseparated ltsets ->
        (try
           let to_locs tset =
             List.map
               (fun (typ, loc) ->
                 if not (isPointerType typ)
                 then ast_error "separated on non-pointers";
                 let size = sizeof_pointed typ in
                 let loc = loc_bytes_to_loc_bits loc in
                 Locations.make_loc loc size
               ) (eval_term ~with_alarms env result tset)
           in
           let locs = List.map to_locs ltsets in
           let to_zone = Locations.enumerate_bits in
           let lz = List.map (List.map (fun l -> l, to_zone l)) locs in
           let unknown = ref false in
           (* Are those two lists of locations separated? *)
           let do_two l1 l2 =
             let combine (loc1, z1) (loc2, z2) =
               if Zone.intersects z1 z2 then
                 if Locations.cardinal_zero_or_one loc1 &&
                   Locations.cardinal_zero_or_one loc2
                 then raise Exit
                 else unknown := true
             in
             List.iter (fun e1 -> List.iter (combine e1) l2) l1
           in
           let rec aux = function
             | [] | [_] -> ()
             | locs :: qlocs ->
                 List.iter (do_two locs) qlocs;
                 aux qlocs
           in
           aux lz;
           if !unknown then Unknown else True
         with
           | Exit -> False
           | LogicEvalError ee -> display_evaluation_error ee; Unknown)

    | Pfresh (_,_,_,_)
    | Papp _
    | Pallocable _ | Pfreeable _
    | Plet (_,_) | Pif (_, _, _)
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

(* If [always] is true, reduce in all cases. Otherwise, reduce only
   when [p] is a disjunction *)
(* TODO: if would be great to have split [states] into those Valid (no reduction
   if not a disjunction) and the others (always reduce). This must be done in
   the callers, though *)
let reduce_by_disjunction ~always ~result ~env states n p =
  if State_set.is_empty states then states
  else
    let nb = count_disjunction p in
    if nb <= 1 && not always then states (* nothing to reduce *)
    else if (State_set.length states) * nb <= n
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
  else (* Not enough slevel to have a noticeable effect. Just reduce the
          various states globally *)
    State_set.fold
      (fun acc state ->
        let env = overwrite_current_state env state in
        let reduced = reduce_by_predicate ~result env true p in
        State_set.add (env_current_state reduced) acc)
      State_set.empty
      states

let () =
(* TODO: deprecate loc_to_loc, move loc_to_locs into Value *)
  Db.Properties.Interp.loc_to_loc :=
    (fun ~result state t ->
      try eval_tlval_as_location
            ~with_alarms:warn_raise_mode (env_pre_f state) result t
      with LogicEvalError _ -> raise (Invalid_argument "not an lvalue")
    );
  Db.Properties.Interp.loc_to_locs :=
    (fun ~result state t ->
      try eval_tlval_as_locations
            ~with_alarms:CilE.warn_none_mode (env_pre_f state) result t
      with LogicEvalError _ -> raise (Invalid_argument "not an lvalue")
    );


(*
Local Variables:
compile-command: "make -C ../.."
End:
*)
