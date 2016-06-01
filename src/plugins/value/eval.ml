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

(** *)

(* -------------------------------------------------------------------------- *)
(**                       {2 Lattice structure }                              *)
(* -------------------------------------------------------------------------- *)

include Bottom.Type

type 'a or_top    = [ `Value of 'a | `Top ]

(* -------------------------------------------------------------------------- *)
(**                     {2 Types for the evaluations }                        *)
(* -------------------------------------------------------------------------- *)

(* Forward evaluation. *)
type 't with_alarms = 't * Alarmset.t
type 't evaluated = 't or_bottom with_alarms

(* This monad propagates the `Bottom value if needed and join the potential
   alarms returned during the evaluation. *)
let (>>=) (t, a) f = match t with
  | `Bottom  -> `Bottom, a
  | `Value t -> let t', a' = f t in t', Alarmset.union a a'

(* Use this monad if the following function returns a simple value. *)
let (>>=:) (t, a) f = match t with
  | `Bottom  -> `Bottom, a
  | `Value t -> let t' = f t in `Value t', a

(* Use this monad if the following function returns no alarms. *)
let (>>=.) (t, a) f = match t with
  | `Bottom  -> `Bottom, a
  | `Value t -> let t' = f t in t', a

(* Backward evaluation. *)
type 'a reduced = [ `Bottom | `Unreduced | `Value of 'a ]


(* Context for the evaluation of abstract value operators. *)

(* unop e1 = e2. *)
type unop_context = exp * exp

(* e1 binop e2 = e3. *)
type binop_context = exp * exp * exp * typ

(* index, remaining, typ pointed, array size expression *)
type index_context =  exp * offset * typ * exp option


(* -------------------------------------------------------------------------- *)
(**                     {2 Cache for the evaluations }                        *)
(* -------------------------------------------------------------------------- *)

(* State of the reduction of an abstract value. *)
type reductness =
  | Unreduced  (* No reduction. *)
  | Reduced    (* A reduction has been performed for this expression. *)
  | Created    (* The abstract value has been created. *)
  | Dull       (* Reduction is pointless for this expression. *)

(* Right values with 'undefined' and 'escaping addresses' flags. *)
type 'a flagged_value = {
  v: 'a;
  initialized: bool;
  escaping: bool;
}

(* Data record associated to each evaluated expression. *)
type ('a, 'origin) record_val = {
  value : 'a or_bottom flagged_value;  (* The resulting abstract value *)
  origin: 'origin option;   (* The origin of the abstract value *)
  reductness : reductness;  (* The state of reduction. *)
  val_alarms : Alarmset.t   (* The emitted alarms during the evaluation. *)
}

(* Data record associated to each evaluated left-value. *)
type 'a record_loc = {
  loc: 'a;                  (* The location of the left-value. *)
  typ: typ;                 (* *)
  loc_alarms: Alarmset.t    (* The emitted alarms during the evaluation. *)
}

(* Results of an evaluation: the results of all intermediate calculation (the
    value of each expression and the location of each lvalue) are cached in a
    map. *)
module type Valuation = sig
  type t
  type value  (* Abstract value. *)
  type origin (* Origin of values. *)
  type loc    (* Abstract memory location. *)

  val empty : t
  val find : t -> exp -> (value, origin) record_val or_top
  val add : t -> exp -> (value, origin) record_val -> t
  val fold : (exp -> (value, origin) record_val -> 'a -> 'a) -> t -> 'a -> 'a
  val find_loc : t -> lval -> loc record_loc or_top
  val filter :
    (exp -> (value, origin) record_val -> bool) ->
    (lval -> loc record_loc -> bool) ->
    t -> t
end


module Clear_Valuation
    (Valuation : Valuation)
= struct

  exception Found

  let is_subexpr sub expr =
    if Cil_datatype.Exp.equal expr sub
    then true
    else
      let vis = object
        inherit Visitor.frama_c_inplace
        method! vexpr e =
          if Cil_datatype.ExpStructEq.equal e sub then raise Found;
          Cil.DoChildren
      end
      in
      try ignore (Visitor.visitFramacExpr vis expr); false
      with Found -> true

  let is_sublval sub lval =
    let vis = object
      inherit Visitor.frama_c_inplace
      method! vexpr e =
        if Cil_datatype.ExpStructEq.equal e sub then raise Found;
        Cil.DoChildren
    end
    in
    try ignore (Visitor.visitFramacLval vis lval); false
    with Found -> true

  let clear_expr valuation expr =
    Valuation.filter
      (fun e _ -> not (is_subexpr expr e))
      (fun lv _ -> not (is_sublval expr lv))
      valuation
end


(* -------------------------------------------------------------------------- *)
(**                        {2 Types of assignments }                          *)
(* -------------------------------------------------------------------------- *)

type 'loc left_value = {
  lval: lval;
  lloc: 'loc;
  ltyp: typ;
}

(* Copy of values. *)
type 'value copied =
  | Determinate of 'value flagged_value
  | Exact of ('value or_bottom) flagged_value

(* Assigned values. *)
type 'value assigned =
  | Assign of 'value
  | Copy of lval * 'value copied

let value_assigned = function
  | Assign v -> `Value v
  | Copy (_, copied) ->
    match copied with
    | Determinate v -> `Value v.v
    | Exact v -> v.v


(* -------------------------------------------------------------------------- *)
(**                       {2 Interprocedural Analysis }                       *)
(* -------------------------------------------------------------------------- *)

type 'value argument = {
  formal: varinfo;
  concrete: exp;
  avalue: 'value assigned;
}

type 'value call = {
  kf: kernel_function;
  arguments: 'value argument list;
  rest: (exp * 'value assigned) list
}


type ('state, 'summary, 'value) return = {
  post_state: 'state;
  returned_value: 'value or_bottom flagged_value option;
  summary: 'summary;
}

type ('state, 'summary, 'value) call_result =
  ('state, 'summary, 'value) return list or_bottom

(* Initialization of a dataflow analysis, by definig the initial value of
    each statement. *)
type 't init =
  | Default
  | Continue of 't
  | Custom of (stmt * 't) list

(* Action to perform on a call site. *)
type ('state, 'summary, 'value) action =
  | Compute of 'state init * bool
  | Recall  of 'state init
  | Result  of ('state, 'summary, 'value) call_result * Value_types.cacheable

exception InvalidCall


(*
Local Variables:
compile-command: "make -C ../../.."
End:
*)
