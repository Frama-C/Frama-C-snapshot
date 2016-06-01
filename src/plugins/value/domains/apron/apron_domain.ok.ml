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

#24 "src/plugins/value/domains/apron/apron_domain.ok.ml"

open Cil_types
open Eval
open Apron

let ok = true

module type S = Abstract_domain.Internal
  with type value = Main_values.Interval.t
   and type location = Precise_locs.precise_location

let abort exclog =
  let open Manager in
  Value_parameters.fatal
    "Apron manager error : %a in function %a.@.%s"
    print_exc exclog.exn print_funid exclog.funid exclog.msg

let is_relevant_varinfo varinfo =
  not (Cil.typeHasQualifier "volatile" varinfo.vtype)
  && (true || not varinfo.vglob)

let is_relevant_lval = function
  | Var varinfo, NoOffset -> is_relevant_varinfo varinfo
  | _ -> false

let rec contains_relevant_lval expr = match expr.enode with
  | Lval lval -> is_relevant_lval lval
  | UnOp (_, e, _) | CastE (_, e) -> contains_relevant_lval e
  | BinOp (_, e1, e2, _) ->
    contains_relevant_lval e1 || contains_relevant_lval e2
  | _ -> false

let is_relevant expr = match expr.enode with
  | Lval _ -> true
  | UnOp (_, e, _) | CastE (_, e) -> contains_relevant_lval e
  | BinOp (_, e1, e2, _) ->
    contains_relevant_lval e1 && contains_relevant_lval e2
  | _ -> false


(** All conversion functions may fail with this exception. *)
exception Out_of_Scope

(* -------------------------------------------------------------------------- *)
(*                         Conversion of integers                             *)
(* -------------------------------------------------------------------------- *)

(* Apron Scalar to Apron integer. *)
let scalar_to_mpzf = function
  | Scalar.Mpqf rational ->
    let num, den = Mpqf.to_mpzf2 rational in
    if Mpzf.cmp_int den 1 = 0 then num else raise Out_of_Scope
  | Scalar.Float _ | Scalar.Mpfrf _ -> raise Out_of_Scope

let scalar_to_int s =
  if Scalar.is_infty s <> 0
  then None
  else match s with
    | Scalar.Mpqf q ->
      (* TODO: extract to Integer directly, without intermediate smaller type *)
      let i = int_of_float (Mpqf.to_float q) in
      if Scalar.equal_int s i then Some i else raise Out_of_Scope
    | Scalar.Float _
    | Scalar.Mpfrf _-> raise Out_of_Scope


(* -------------------------------------------------------------------------- *)
(*                Translation of the AST from Cil to Apron                    *)
(* -------------------------------------------------------------------------- *)

(* Rounding mode used in the file. [Rnd] corresponds to rounding in all
   directions. Only useful for floating-point values. *)
let round = Texpr1.Rnd


let bounds_of_typ ikind =
  let bitsize = Cil.bitsSizeOfInt ikind in
  let size = Mpz.init () in
  Mpz.ui_pow_ui size 2 bitsize;
  let size = Mpzf.of_mpz size in
  if Cil.isSigned ikind
  then
    let half = Mpz.init () in
    Mpz.ui_pow_ui half 2 (bitsize - 1);
    let half = Mpzf.of_mpz half in
    Mpzf.neg half, Mpzf.sub_int half 1, size
  else
    Mpzf.of_int 0, Mpzf.sub_int size 1, size


(* Auxiliary function for {!coerce} below.
   Algorithm from Verasco.
   See section 6.5 of the paper 'A Formally-Verified C Static Analyzer'. *)
let reduce eval expr inf sup size =
  let interval = eval expr in
  if Interval.is_bottom interval then expr
  else
    let top () =
      let coeff = Coeff.i_of_mpqf (Mpqf.of_mpz inf) (Mpqf.of_mpz sup) in
      Texpr1.Cst coeff
    in
    try
      let min = scalar_to_mpzf interval.Interval.inf
      and max = scalar_to_mpzf interval.Interval.sup in
      if Mpzf.cmp (Mpzf.sub min max) size >= 0
      then top ()
      else
        let factor = Mpzf.fdiv_q (Mpzf.sub min inf) size in
        let shift = Mpzf.mul factor size in
        if Mpzf.cmp (Mpzf.sub max shift) sup > 0
        then top ()
        else if Mpzf.cmp_int factor 0 = 0
        then expr
        else
          let coeff = Coeff.s_of_mpqf (Mpqf.of_mpz shift) in
          let e_coeff = Texpr1.Cst coeff in
          Texpr1.Binop (Texpr1.Sub, expr, e_coeff, Texpr1.Int, round)
    with
      Out_of_memory -> top ()

(* [coerce eval typ texpr] returns a normalized apron expression [e] such that
   its evaluation in the mathematical world and its evaluation with machine
   number are equivalent. [eval] is the mathematical evaluation function.
   If overflows are not allowed for the type [typ], then [texpr = e]. *)
let coerce ?(cast=false) eval typ texpr =
  match Cil.unrollType typ with
  | TInt (ikind, _) | TEnum ({ ekind = ikind}, _) ->
    let signed = Cil.isSigned ikind in
    if
      not cast
      && ((signed && Kernel.SignedOverflow.get ())
          || ((not signed) && Kernel.UnsignedOverflow.get ()))
    then
      texpr
    else
      let inf, sup, size = bounds_of_typ ikind in
      reduce eval texpr inf sup size
  | _ -> raise Out_of_Scope


let translate_typ = function
  | TInt _ | TEnum _ -> Texpr1.Int
  | _ -> raise Out_of_Scope

let translate_binop = function
  | PlusA  -> Texpr1.Add
  | MinusA -> Texpr1.Sub
  | Mult   -> Texpr1.Mul
  | Div    -> Texpr1.Div
  | Mod    -> Texpr1.Mod
  | _ -> raise Out_of_Scope

let translate_relation expr typ =
  let open Abstract_interp.Comp in
  function
  | Le ->
    let expr = Texpr1.unop Texpr1.Neg expr typ round in
    Tcons1.make expr Tcons1.SUPEQ
  | Lt ->
    let expr = Texpr1.unop Texpr1.Neg expr typ round in
    Tcons1.make expr Tcons1.SUP
  | Ge -> Tcons1.make expr Tcons1.SUPEQ
  | Gt -> Tcons1.make expr Tcons1.SUP
  | Eq -> Tcons1.make expr Tcons1.EQ
  | Ne -> Tcons1.make expr Tcons1.DISEQ

let translate_varinfo varinfo =
  if not (is_relevant_varinfo varinfo)
  then raise Out_of_Scope
  else
    let id = "_" ^ string_of_int varinfo.vid in
    let var = Var.of_string (varinfo.vname ^ id) in
    match varinfo.vtype with
    | TInt _ | TEnum _ -> var
    | _ -> raise Out_of_Scope

let translate_lval = function
  | Var varinfo, NoOffset -> translate_varinfo varinfo
  | _ -> raise Out_of_Scope

let translate_constant = function
  | CInt64 (i, _, _) -> begin
      try Coeff.s_of_int (Integer.to_int i) (* TODO: skip OCaml int type *)
      with Failure _ -> raise Out_of_Scope
    end
  | _ -> raise Out_of_Scope

(* Translation of expressions from cil to apron. *)
let rec translate_expr eval expr = match expr.enode with
  | Const cst -> Texpr1.Cst (translate_constant cst)
  | Lval lval -> Texpr1.Var (translate_lval lval)
  | UnOp (Neg, e1, typ) ->
    let e1' = translate_expr eval e1 in
    Texpr1.(Unop (Neg, e1', translate_typ typ, round))
  | BinOp (op, e1, e2, typ) ->
    let e1' = translate_expr eval e1 in
    let e2' = translate_expr eval e2 in
    let need_coercion = op = Mod || op = Div in
    let e1' = if need_coercion then coerce eval (Cil.typeOf e1) e1' else e1' in
    let e2' = if need_coercion then coerce eval (Cil.typeOf e2) e2' else e2' in
    let op' = translate_binop op in
    Texpr1.(Binop (op', e1', e2', translate_typ typ, round))
  | CastE (typ, e)-> coerce ~cast:true eval typ (translate_expr eval e)
  | Info (e, _) -> translate_expr eval e
  | _ -> raise Out_of_Scope

(* Express a cil expression into an apron constraint. *)
let rec constraint_expr eval env expr positive =
  match expr.enode with
  | UnOp (LNot, e, _) -> constraint_expr eval env e (not positive)
  | BinOp ((Le|Ne|Eq|Gt|Lt|Ge as binop), e1, e2, typ) ->
    let e1' = coerce eval (Cil.typeOf e1) (translate_expr eval e1) in
    let e2' = coerce eval (Cil.typeOf e2) (translate_expr eval e2) in
    let typ = translate_typ (Cil.unrollType typ) in
    let e = Texpr1.Binop (Texpr1.Sub, e1', e2', typ, round) in
    let expr = Texpr1.of_expr env e in
    let binop = Value_util.conv_comp binop in
    let binop = if positive then binop else Abstract_interp.Comp.inv  binop in
    translate_relation expr typ binop
  | _ -> raise Out_of_Scope

(* Expresses the constraint [expr ∈ interval] as an Apron constraint. *)
let constraint_reduction env expr interval =
  let coeff = Texpr1.Cst (Coeff.Interval interval) in
  let expr = Texpr1.(Binop (Sub, expr, coeff, Int, round)) in
  let texpr = Texpr1.of_expr env expr in
  Tcons1.make texpr Tcons1.EQ


let truncate_interval typ interval =
  match Cil.unrollType typ with
  | TInt (ikind, _) | TEnum ({ ekind = ikind }, _) ->
    let signed = Cil.isSigned ikind in
    if
      (signed && not (Kernel.SignedOverflow.get ()))
      || ((not signed) && not (Kernel.UnsignedOverflow.get ()))
    then
      let inf, sup, _size = bounds_of_typ ikind in
      let inf = Scalar.of_mpqf (Mpqf.of_mpz inf)
      and sup = Scalar.of_mpqf (Mpqf.of_mpz sup) in
      let min = interval.Interval.inf and max = interval.Interval.sup in
      let min = if Scalar.cmp min inf < 0 then inf else min
      and max = if Scalar.cmp max sup > 0 then sup else max in
      Interval.of_scalar min max
    else
      interval
  | _ -> interval

(* TODO: avoid OCaml int type during conversion *)
let interval_to_ival interval =
  let inf = scalar_to_int interval.Interval.inf
  and sup = scalar_to_int interval.Interval.sup in
  let inf = Extlib.opt_map Integer.of_int inf
  and sup = Extlib.opt_map Integer.of_int sup in
  Some (Ival.inject_range inf sup)

let int_to_scalar positive = function
  | None -> Scalar.of_infty positive
  | Some integer -> Scalar.of_mpqf (Mpqf.of_string (Integer.to_string integer))

let ival_to_interval = function
  | None -> Interval.top
  | Some ival ->
    try
      let min, max = Ival.min_and_max ival in
      let min = int_to_scalar (-1) min and max = int_to_scalar 1 max in
      Interval.of_scalar min max
    with
      Cvalue.V.Not_based_on_null -> Interval.top


(* -------------------------------------------------------------------------- *)
(*                          Abstract Domain Functor                           *)
(* -------------------------------------------------------------------------- *)

module Make
    (Man: sig
       type t
       val manager: t Manager.t
       val key: t Abstract1.t Abstract_domain.key
     end)
= struct

  type state = Man.t Abstract1.t
  type value = Main_values.Interval.t
  type location = Precise_locs.precise_location

  let man = Man.manager

  let structure = Abstract_domain.Leaf Man.key

  let empty_env = Environment.make [||] [||]

  let top = Abstract1.top man empty_env
  let make_top env = Abstract1.top man env

  include Datatype.Make_with_collections (
    struct
      include Datatype.Undefined
      type t = state
      let name = Manager.get_library man
      let reprs = [top]
      let structural_descr = Structural_descr.t_unknown

      let equal = Abstract1.is_eq man

      let hash = Abstract1.hash man

      (* BIGTODO: this function is not quite a total order, because [is_leq] is
         only a partial order. Using the hash as a first comparison is only
         a doubtful hack. *)
      let compare a b =
        if equal a b then 0
        else
          let cmp = compare (hash a) (hash b) in
          if cmp <> 0 then cmp
          else if Abstract1.is_leq man a b then 1 else -1

      let rehash = Datatype.identity
      let copy = Abstract1.copy man
      let pretty = Abstract1.print

      let mem_project = Datatype.never_any_project
    end )

  let is_included = Abstract1.is_leq man
  let join s1 s2 =
    try Abstract1.join man s1 s2
    with Manager.Error exclog -> abort exclog
  let join_and_is_included a b = let j = join a b in j, equal j b
  let widen _kf _stmt s1 s2 = Abstract1.widening man s1 s2

  type origin = unit

  let make_eval state =
    let env = Abstract1.env state in
    fun e ->
      let texp = Texpr1.of_expr env e in
      Abstract1.bound_texpr man state texp

  let compute state expr typ =
    let top = `Value (None, ()), Alarmset.all in
    if not (is_relevant expr)
    then top
    else
      try
        let eval = make_eval state in
        let exp = coerce eval typ (translate_expr eval expr) in
        let interval = eval exp in
        let interval = truncate_interval typ interval in
        let value =
          if Interval.is_bottom interval
          then `Bottom
          else `Value (interval_to_ival interval, ())
        in
        (* TODO: remove alarms if computation does not overflow *)
        value, Alarmset.all
      with
      | Out_of_Scope -> top

  let extract_expr _oracle state expr =
    compute state expr (Cil.typeOf expr)

  let extract_lval _oracle state lval typ _loc =
    let expr = Cil.dummy_exp (Cil_types.Lval lval) in
    compute state expr typ

  let reduce_further _ _ _ = []

  let backward_location _state _lv _typ loc value = `Value (loc, value)

  let return state =
    if Abstract1.is_bottom man state
    then `Bottom
    else `Value state

  let forget_varinfo_list ~remove vars state =
    let filter acc varinfo =
      try
        let apron_var = translate_varinfo varinfo in
        if Environment.mem_var state.Abstract1.env apron_var
        then apron_var :: acc
        else acc
      with Out_of_Scope -> acc
    in
    let vars = Array.of_list (List.fold_left filter [] vars) in
    let state = Abstract1.forget_array man state vars false in
    if remove
    then
      let env = Environment.remove (Abstract1.env state) vars in
      Abstract1.change_environment man state env false
    else state

  let kill_bases loc state =
    let aux_ploc loc state =
      let bases = Locations.Location_Bits.get_bases loc.Locations.loc in
      match bases with
      | Base.SetLattice.Set set ->
        let var_of_base base acc =
          try (Base.to_varinfo base) :: acc
          with Base.Not_a_C_variable -> acc
        in
        let vars = Base.Hptset.fold var_of_base set [] in
        forget_varinfo_list ~remove:false vars state
      | Base.SetLattice.Top  -> make_top (Abstract1.env state)
    in
    Precise_locs.fold aux_ploc loc state

  type summary = unit
  module Summary = Datatype.Unit

  module Transfer
      (Valuation: Abstract_domain.Valuation with type value = value
                                             and type loc = location)
  = struct

    type state = t
    type summary = unit
    type value = Main_values.Interval.t
    type location = Precise_locs.precise_location
    type valuation = Valuation.t

    let update valuation state =
      let eval = make_eval state in
      let env = Abstract1.env state in
      (* Makes a list of apron constraints from a valuation:
         for each value marked as Reduced for an expression, creates the
         apron constraint [expression = value]. *)
      let gather_constraints exp record acc =
        if record.reductness = Reduced
        then
          try
            let expr = translate_expr eval exp in
            let expr = coerce eval (Cil.typeOf exp) expr in
            (* When the value is top or bottom, no constraint is expressible. *)
            let cons = record.value.v >>- fun ival ->
              let interval = ival_to_interval ival in
              if Interval.is_top interval
              then `Bottom
              else `Value (constraint_reduction env expr interval)
            in
            Bottom.add_to_list cons acc
          with Out_of_Scope -> acc
        else acc
      in
      let constraints = Valuation.fold gather_constraints valuation [] in
      if constraints = []
      then state
      else
        (* Meet the state with all the constraints. *)
        let array = Tcons1.array_make env (List.length constraints) in
        List.iteri (fun i c -> Tcons1.array_set array i c) constraints;
        let st = Abstract1.meet_tcons_array man state array in
        if Abstract1.is_bottom man st then(
          Format.printf "Bottom with state %a and constraints %a@."
            Abstract1.print state (fun fmt a -> Tcons1.array_print fmt a) array;
          st)
        else st

    let assign _stmt lvalue expr _value valuation state =
      let state = update valuation state in
      try
        let state =
          try
            let eval = make_eval state in
            let var = translate_lval lvalue.lval in
            let expr = expr in
            let exp = translate_expr eval expr in
            let exp = coerce eval lvalue.ltyp exp in
            let exp = Texpr1.of_expr (Abstract1.env state) exp in
            (* TODO: currently, all variables are present in the environment
               at all times. Change to a dynamic environment, in which new
               variables are added here, and removed when the scope changes. *)
            Abstract1.assign_texpr man state var exp None
          with
          | Out_of_Scope -> kill_bases lvalue.lloc state
        in
        return state
      with Manager.Error exclog -> abort exclog


    let assume _stmt exp bool valuation state =
      let state = update valuation state in
      try
        let env = Abstract1.env state in
        let eval = make_eval state in
        let cons = constraint_expr eval env exp bool in
        let array = Tcons1.array_make env 1 in
        Tcons1.array_set array 0 cons;
        let state = Abstract1.meet_tcons_array man state array in
        return state
      with
      | Out_of_Scope -> `Value state


    let call_action _stmt call valuation state =
      let state = update valuation state in
      let eval = make_eval state in
      let process_argument (vars, acc) arg =
        try
          let env = Abstract1.env state in
          let var = translate_varinfo arg.formal in
          let vars = var :: vars in
          let acc =
            try
              let exp = translate_expr eval arg.concrete in
              let texp = Texpr1.of_expr env exp in
              (var, texp) :: acc
            with Out_of_Scope -> acc
          in
          vars, acc
        with
        | Out_of_Scope -> (vars, acc)
      in
      let vars, list = List.fold_left process_argument ([], []) call.arguments in
      let env = Abstract1.env state in
      let vars_array = Array.of_list vars in
      let env = Environment.add env vars_array [||] in
      let vars, texprs = List.split list in
      let vars_array = Array.of_list vars
      and texprs_array = Array.of_list texprs in
      let state = Abstract1.change_environment man state env false in
      let state =
        Abstract1.assign_texpr_array man state vars_array texprs_array None
      in
      if Abstract1.is_bottom man state
      then Result (`Bottom, Value_types.Cacheable)
      else Compute (Continue state, true)

    let summarize _kf _stmt ~returned:_ state = `Value ((), state)

    let resolve_call _stmt _call ~assigned _valuation ~pre ~post =
      let env = Abstract1.env pre in
      let (), state = post in
      let state = Abstract1.change_environment man state env false in
      match assigned with
      | None -> return state
      | Some (lv, _) -> return (kill_bases lv.lloc state)

    let default_call _stmt _call state =
      let top = make_top (Abstract1.env state) in
      `Value [{ post_state = top; summary = (); returned_value = None }]

  end


  let compute_using_specification _ (kf, _) state =
    let name = Kernel_function.get_name kf in
    if
      (name >= "Frama_C_show" && name < "Frama_C_shox")
      || (name >= "Frama_C_dump" && name < "Frama_C_dumq")
    then `Value [{ post_state = state; summary = (); returned_value = None }]
    else
      let top = make_top (Abstract1.env state) in
      `Value [{ post_state = top; summary = (); returned_value = None }]

  type eval_env = state
  let env_current_state state = `Value state
  let env_annot ~pre:_ ~here () = here
  let env_pre_f ~pre () = pre
  let env_post_f ~pre:_ ~post ~result:_ () = post
  let eval_predicate _ _ = Alarmset.Unknown
  let reduce_by_predicate state _ _ = state

  let open_block _fundec block ~body:_ state =
    let translate acc varinfo =
      try translate_varinfo varinfo :: acc
      with Out_of_Scope -> acc
    in
    let vars = List.fold_left translate [] block.blocals in
    let env = Environment.add (Abstract1.env state) (Array.of_list vars) [||] in
    Abstract1.change_environment man state env false

  let close_block fundec block ~body state =
    let state = forget_varinfo_list ~remove:true block.blocals state in
    if body
    then forget_varinfo_list ~remove:true fundec.sformals state
    else state

  let empty () = top

  let initialize_var state lval _ _ =
    try
      let env = Abstract1.env state in
      let var = translate_lval lval in
      let env = Environment.add env [|var|] [||] in
      Abstract1.change_environment man state env false
    with
    | Out_of_Scope -> state

  let initialize_var_using_type state varinfo =
    try
      let var = translate_varinfo varinfo in
      let env = Abstract1.env state in
      if Environment.mem_var env var
      then state
      else
        let env = Environment.add env [|var|] [||] in
        Abstract1.change_environment man state env false
    with
    | Out_of_Scope -> state

  let global_state () = None

  let filter_by_bases _ state = state
  let reuse ~current_input:_ ~previous_output = previous_output

end


let octagon_key = Structure.Key_Domain.create_key "apron-octagon"
let box_key = Structure.Key_Domain.create_key "apron-box"
let polka_loose_key = Structure.Key_Domain.create_key "polka-loose"
let polka_strict_key = Structure.Key_Domain.create_key "polka-strict"
let polka_equalities_key = Structure.Key_Domain.create_key "polka-equalities"


module Apron_Octagon = struct
  type t = Oct.t
  let manager = Oct.manager_alloc ()
  let key = octagon_key
end

module Apron_Box = struct
  type t = Box.t
  let manager = Box.manager_alloc ()
  let key = box_key
end

module Apron_Polka_Loose = struct
  type t = Polka.loose Polka.t
  let manager = Polka.manager_alloc_loose ()
  let key = polka_loose_key
end
module Apron_Polka_Strict = struct
  type t = Polka.strict Polka.t
  let manager = Polka.manager_alloc_strict ()
  let key = polka_strict_key
end
module Apron_Polka_Equalities = struct
  type t = Polka.equalities Polka.t
  let manager = Polka.manager_alloc_equalities ()
  let key = polka_equalities_key
end

(** Apron manager allocation changes the rounding mode. *)
let () = Floating_point.set_round_nearest_even ()

module Octagon = Make (Apron_Octagon)
module Box = Make (Apron_Box)
module Polka_Loose = Make (Apron_Polka_Loose)
module Polka_Strict = Make (Apron_Polka_Strict)
module Polka_Equalities = Make (Apron_Polka_Equalities)


(*
Local Variables:
compile-command: "make -C ../../../.."
End:
*)
