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

open Cil_types
open Eval
open Numerors_utils

module I = Numerors_interval
module P = Precisions

module Arith = Numerors_arithmetics


(*-----------------------------------------------------------------------------
 *            Abstract value for numerical errors estimation
 *-----------------------------------------------------------------------------
 *  The abstract value is a record with four fields :
 *    - exact   : interval abstraction of the real value
 *    - approx  : interval abstraction of the float value
 *    - abs_err : interval abstraction of the absolute error value
 *    - rel_err : interval abstraction of the relative error value
 *  A zonotope abstraction for each of those fields may be added
 *---------------------------------------------------------------------------*)
type err = Top | Zero | Elt of Arith.t


(*-----------------------------------------------------------------------------
 *                           Pretty printer
 *---------------------------------------------------------------------------*)
let pp_print fmt = function
  | Elt t -> Arith.pretty fmt t
  | Zero  -> Format.fprintf fmt "{ZERO}"
  | Top   -> Format.fprintf fmt "{TOP}"


(*-----------------------------------------------------------------------------
 *                          Set errors to top
 *---------------------------------------------------------------------------*)
let set_absolute_to_top = function
  | Elt e -> Elt { e with Arith.abs_err = I.top ~prec:P.Real }
  | err -> err

let set_relative_to_top = function
  | Elt e -> Elt { e with Arith.rel_err = I.top ~prec:P.Real }
  | err -> err


(*-----------------------------------------------------------------------------
 *                        Lattice structure
 *---------------------------------------------------------------------------*)
let top = Top

let is_included x y =
  match x, y with
  | Zero, Zero | _, Top -> true
  | Zero, Elt t -> Arith.is_included (Arith.zero t) t
  | Elt a, Elt b -> Arith.is_included a b
  | Elt _, Zero | Top, _ -> false

let join x y =
  match x, y with
  | Zero, Zero -> Zero
  | Top, _ | _, Top -> Top
  | Elt a, Zero | Zero, Elt a -> Elt (Arith.join (Arith.zero a) a)
  | Elt a, Elt b -> Elt (Arith.join a b)

let narrow x y =
  match x, y with
  | Zero, Zero -> `Value Zero
  | Top, t | t, Top -> `Value t
  | Elt a, Zero | Zero, Elt a ->
    Arith.narrow (Arith.zero a) a >>- fun t -> `Value (Elt t)
  | Elt a, Elt b -> Arith.narrow a b >>- fun t -> `Value (Elt t)

let reduce _ t = `Value t


(*-----------------------------------------------------------------------------
 *                    Elements needed for Eva core
 *---------------------------------------------------------------------------*)
module T = struct
  type t = err
  include Datatype.Undefined
  let structural_descr = Structural_descr.t_unknown
  let compare x y =
    match x, y with
    | Elt a, Elt b -> Arith.compare a b
    | Top, Top | Zero, Zero -> 0
    | Top, _ | _, Zero ->  1
    | _, Top | Zero, _ -> -1
  let equal = Datatype.from_compare
  let hash = Hashtbl.hash
  let reprs = [top]
  let name = "Value.Numerors_values.Numerors"
  let pretty = pp_print
end
include Datatype.Make(T)
let pretty_debug = pretty
let pretty_typ _ = pretty
let key = Structure.Key_Value.create_key "numerors_values"


(*-----------------------------------------------------------------------------
 *                            Constructors
 *---------------------------------------------------------------------------*)
let zero = Zero
let one = top
let top_int = top
let inject_int _ _ = top

let create exact approx abs_err rel_err =
  Elt (Arith.create exact approx abs_err rel_err)

let of_ints ~prec min max =
  let exact   = I.of_ints ~prec:P.Real (min, max) in
  let approx  = I.of_ints ~prec:prec   (min, max) in
  let abs_err = I.zero ~prec:P.Real in
  let rel_err = I.zero ~prec:P.Real in
  create exact approx abs_err rel_err


(*-----------------------------------------------------------------------------
 *                           Miscellaneous
 *---------------------------------------------------------------------------*)

(* Handle the computation mode for the forward operations *)
let mode_on_errors exact approx abs_err rel_err =
  Elt (Arith.forward_interaction (Arith.create exact approx abs_err rel_err))

(*-----------------------------------------------------------------------------
 *                        Arithmetic import
 *---------------------------------------------------------------------------*)
module Exact    = Arith.Exact
module Approx   = Arith.Approx
module Abs_Err  = Arith.Abs_Err
module Rel_Err  = Arith.Rel_Err


(*-----------------------------------------------------------------------------
 *                      Numerors value of a constant
 *---------------------------------------------------------------------------*)
let constant _ = function
  | CReal (r, fkind, opt) ->
    let prec = Precisions.of_fkind fkind in
    let exact =
      match opt with
      | Some s -> I.of_strings Precisions.Real (s, s)
      | None   -> I.of_floats  Precisions.Real (r, r)
    in
    let approx = I.of_floats_without_rounding prec (r, r) in
    let abs_err = I.sub approx exact in
    let rel_err =
      if I.is_zero exact then I.of_floats ~prec:P.Real (0.0, 0.0)
      else I.div abs_err exact
    in
    mode_on_errors exact approx abs_err rel_err
  | _ -> top


(*-----------------------------------------------------------------------------
 *              Forward unary operations on Numerors value
 *---------------------------------------------------------------------------*)
let forward_unop _typ op v =
  match v, op with
  | Elt v, Neg ->
    let exact  , approx  = Exact.Forward.neg   v, Approx.Forward.neg  v in
    let abs_err = Abs_Err.Forward.neg v ~exact ~approx in
    let rel_err = Rel_Err.Forward.neg v ~exact ~abs_err in
    `Value (mode_on_errors exact approx abs_err rel_err)
  | Zero, Neg -> `Value zero
  | _, LNot | _, BNot | Top, _ -> `Value top


(*-----------------------------------------------------------------------------
 *                 Forward cast on Numerors value
 *-----------------------------------------------------------------------------
 * The cast of integers into floats is actually handled by the Numerors
 * domain in the module <MakeNumerorsCValuesProduct>.
 *---------------------------------------------------------------------------*)
let forward_cast ~src_type ~dst_type = function
  | Top   -> `Value Top
  | Zero  -> `Value Zero
  | Elt t ->
    match src_type, dst_type with
    | Eval_typ.TSFloat _, Eval_typ.TSFloat fk ->
      `Value (Elt (Arith.change_prec (Precisions.of_fkind fk) t))
    | _, _ -> `Value top


(*-----------------------------------------------------------------------------
 *             Forward binary operations on Numerors values
 *---------------------------------------------------------------------------*)
let forward_binop _typ op x y =
  match x, y, op with
  | Elt x, Elt y, PlusA  ->
    let exact  , approx  = Exact.Forward.add   x y, Approx.Forward.add  x y in
    let abs_err = Abs_Err.Forward.add x y ~exact ~approx in
    let rel_err = Rel_Err.Forward.add x y ~exact ~abs_err in
    `Value (mode_on_errors exact approx abs_err rel_err)
  | Elt x, Elt y, MinusA ->
    let exact  , approx  = Exact.Forward.sub   x y, Approx.Forward.sub  x y in
    let abs_err = Abs_Err.Forward.sub x y ~exact ~approx in
    let rel_err = Rel_Err.Forward.sub x y ~exact ~abs_err in
    `Value (mode_on_errors exact approx abs_err rel_err)
  | Elt x, Elt y, Mult ->
    let exact  , approx  = Exact.Forward.mul   x y, Approx.Forward.mul  x y in
    let abs_err = Abs_Err.Forward.mul x y ~exact ~approx in
    let rel_err = Rel_Err.Forward.mul x y ~exact ~abs_err in
    `Value (mode_on_errors exact approx abs_err rel_err)
  | Elt x, Elt y, Div ->
    let exact  , approx  = Exact.Forward.div   x y, Approx.Forward.div  x y in
    let abs_err = Abs_Err.Forward.div x y ~exact ~approx in
    let rel_err = Rel_Err.Forward.div x y ~exact ~abs_err in
    `Value (mode_on_errors exact approx abs_err rel_err)
  | _, _, _ -> `Value top

(*-----------------------------------------------------------------------------
 *            Backward unary operations on Numerors values
 *---------------------------------------------------------------------------*)
let backward_unop ~typ_arg:_ op ~arg ~res =
  match arg, res, op with
  | Elt x, Elt r, Neg ->
    Exact.Backward.neg   x r >>- fun exact   ->
    Approx.Backward.neg  x r >>- fun approx  ->
    Abs_Err.Backward.neg x r >>- fun abs_err ->
    Rel_Err.Backward.neg x r >>- fun rel_err ->
    `Value (Some (create exact approx abs_err rel_err))
  | _, _, _ -> `Value None


(*-----------------------------------------------------------------------------
 *            Backward binary operations on Numerors values
 *---------------------------------------------------------------------------*)
let backward_binop ~input_type:_ ~resulting_type:_ op ~left ~right ~result =
  match left, right, result, op with
  | Elt x, Elt y, Elt r, PlusA ->
    Exact.Backward.add   x y r >>- fun (x_exact   , y_exact   ) ->
    Approx.Backward.add  x y r >>- fun (x_approx  , y_approx  ) ->
    Abs_Err.Backward.add x y r >>- fun (x_abs_err , y_abs_err ) ->
    Rel_Err.Backward.add x y r >>- fun (x_rel_err , y_rel_err ) ->
    let x = create x_exact x_approx x_abs_err x_rel_err in
    let y = create y_exact y_approx y_abs_err y_rel_err in
    `Value (Some x, Some y)
  | Elt x, Elt y, Elt r, MinusA ->
    Exact.Backward.sub   x y r >>- fun (x_exact   , y_exact   ) ->
    Approx.Backward.sub  x y r >>- fun (x_approx  , y_approx  ) ->
    Abs_Err.Backward.sub x y r >>- fun (x_abs_err , y_abs_err ) ->
    Rel_Err.Backward.sub x y r >>- fun (x_rel_err , y_rel_err ) ->
    let x = create x_exact x_approx x_abs_err x_rel_err in
    let y = create y_exact y_approx y_abs_err y_rel_err in
    `Value (Some x, Some y)
  | Elt x, Elt y, Elt r, Mult ->
    Exact.Backward.mul   x y r >>- fun (x_exact   , y_exact   ) ->
    Approx.Backward.mul  x y r >>- fun (x_approx  , y_approx  ) ->
    Abs_Err.Backward.mul x y r >>- fun (x_abs_err , y_abs_err ) ->
    Rel_Err.Backward.mul x y r >>- fun (x_rel_err , y_rel_err ) ->
    let x = create x_exact x_approx x_abs_err x_rel_err in
    let y = create y_exact y_approx y_abs_err y_rel_err in
    `Value (Some x, Some y)
  | Elt x, Elt y, Elt r, Div ->
    Exact.Backward.div   x y r >>- fun (x_exact   , y_exact   ) ->
    Approx.Backward.div  x y r >>- fun (x_approx  , y_approx  ) ->
    Abs_Err.Backward.div x y r >>- fun (x_abs_err , y_abs_err ) ->
    Rel_Err.Backward.div x y r >>- fun (x_rel_err , y_rel_err ) ->
    let x = create x_exact x_approx x_abs_err x_rel_err in
    let y = create y_exact y_approx y_abs_err y_rel_err in
    `Value (Some x, Some y)
  (* x == y *)
  | _, _, Zero, Ne -> narrow left right >>-: fun t -> Some t, Some t
  (* x < y *)
  | Elt x, Elt y, Zero, Ge ->
    Arith.Backward_Comparisons.lt x y >>-: fun (x, y) ->
    Some (Elt x), Some (Elt y)
  (* x <= y *)
  | Elt x, Elt y, Zero, Gt ->  (* x >= y *)
    Arith.Backward_Comparisons.le x y >>-: fun (x, y) ->
    Some (Elt x), Some (Elt y)
  (* x >= y *)
  | Elt x, Elt y, Zero, Lt ->
    Arith.Backward_Comparisons.ge x y >>-: fun (x, y) ->
    Some (Elt x), Some (Elt y)
  (* x > y *)
  | Elt x, Elt y, Zero, Le ->
    Arith.Backward_Comparisons.gt x y >>-: fun (x, y) ->
    Some (Elt x), Some (Elt y)
  | _ -> `Value (None, None)


(*-----------------------------------------------------------------------------
 *               Operations not handled on Numerors values
 *---------------------------------------------------------------------------*)

let assume_non_zero v = `Unknown v
let assume_bounded _kind _bound v = `Unknown v
let assume_not_nan ~assume_finite:_ _fkind v = `Unknown v
let assume_comparable _cmp v1 v2 = `Unknown (v1, v2)

let rewrap_integer _ _ = top
let backward_cast ~src_typ:_ ~dst_typ:_ ~src_val:_ ~dst_val:_ = `Value None
let resolve_functions _ = `Top, true


(*-----------------------------------------------------------------------------
 *            Built-in to create values in a interval
 *---------------------------------------------------------------------------*)
let dbetween min max =
  match min, max with
  | Elt min, Elt max ->
    let z = I.zero ~prec:P.Real in
    let f = I.join min.Arith.approx max.Arith.approx in
    let r = I.mul f (I.of_ints ~prec:P.Real (1, 1)) in
    let r = { Arith.exact = r
            ; Arith.approx = f
            ; Arith.abs_err = z
            ; Arith.rel_err = z
            }
    in `Value (Elt r)
  | _, _ -> `Value top

let rbetween min max =
  match min, max with
  | Elt min, Elt max ->
    let approx  = I.join min.Arith.approx max.Arith.approx in
    let exact   = I.mul approx (I.of_ints ~prec:P.Real (1, 1)) in
    let rel_err = I.epsilon (I.prec min.Arith.approx) in
    let abs_err = I.mul rel_err exact in
    let res = { Arith.exact   = exact
              ; Arith.approx  = approx
              ; Arith.abs_err = abs_err
              ; Arith.rel_err = rel_err
              }
    in `Value (Elt res)
  | _, _ -> `Value top


(*-----------------------------------------------------------------------------
 *                      Built-in for square root
 *---------------------------------------------------------------------------*)
let sqrt = function
  | Elt x ->
    let exact   = Exact.Forward.sqrt x in
    let approx  = Approx.Forward.sqrt x in
    let abs_err = Abs_Err.Forward.sqrt x ~exact ~approx in
    let rel_err = Rel_Err.Forward.sqrt x ~exact ~abs_err in
    `Value (create exact approx abs_err rel_err)
  | _ -> `Value top


(*-----------------------------------------------------------------------------
 *              Built-in for transcendental functions
 *---------------------------------------------------------------------------*)
let log = function
  | Elt x ->
    let exact   = Exact.Forward.log x in
    let approx  = Approx.Forward.log x in
    let abs_err = Abs_Err.Forward.log x ~exact ~approx in
    let rel_err = Rel_Err.Forward.log x ~exact ~abs_err in
    `Value (create exact approx abs_err rel_err)
  | _ -> `Value top

let exp = function
  | Elt x ->
    let exact   = Exact.Forward.exp x in
    let approx  = Approx.Forward.exp x in
    let abs_err = Abs_Err.Forward.exp x ~exact ~approx in
    let rel_err = Rel_Err.Forward.exp x ~exact ~abs_err in
    `Value (create exact approx abs_err rel_err)
  | _ -> `Value top

let get_max_absolute_error = function
  | Elt x -> Some (snd (I.get_bounds (I.abs (x.Arith.abs_err))))
  | _ -> None

let get_max_relative_error = function
  | Elt x -> Some (snd (I.get_bounds (I.abs (x.Arith.rel_err))))
  | _ -> None
