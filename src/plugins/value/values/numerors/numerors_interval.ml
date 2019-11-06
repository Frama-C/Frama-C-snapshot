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

open Numerors_utils
module F = Numerors_float


(*-----------------------------------------------------------------------------
 *                        Interval representation
 *-----------------------------------------------------------------------------
 * Structure invariants :
 *  - the two bounds in the constructor I must have the same precision
 *  - the two bounds can not be a NaN
 *---------------------------------------------------------------------------*)
type t  = NaN of Precisions.t | I of F.t * F.t * bool


(*-----------------------------------------------------------------------------
 *                            Pretty printer
 *---------------------------------------------------------------------------*)
let pretty fmt itv =
  let pp_nan f n = if n then Format.fprintf f "{NaN}" in
  match itv with
  | NaN _ -> pp_nan fmt true
  | I (x, y, n) ->
    Format.fprintf fmt "%a[%a ; %a]" pp_nan n F.pretty x F.pretty y


(*-----------------------------------------------------------------------------
 *                Methods to get informations on intervals
 *---------------------------------------------------------------------------*)
let prec = function NaN p -> p | I (x, _, _) -> F.prec x

let get_max_exponent = function
  | NaN _ -> Value_parameters.fatal "Numerors: can't return the exponent of a NaN"
  | I (x, y, _) -> Transitioning.Stdlib.max (F.exponent x) (F.exponent y)

let get_exponents = function
  | NaN _ -> Value_parameters.fatal "Numerors: can't return the exponent of a NaN"
  | I (x, y, _) -> F.exponent x, F.exponent y

let get_bounds = function
  | NaN _ -> Value_parameters.fatal "Numerors: can't return the bounds of a NaN"
  | I (x, y, _) -> x, y


(*-----------------------------------------------------------------------------
 *                          Internal functions
 *---------------------------------------------------------------------------*)
(* Exception raised when operands have different precisions *)
exception Different_Precisions

(* Monad to handle NaN intervals *)
let ( >>- ) a f = match a with I (x, y, n) -> f (x, y, n) | n -> n

(* Monad to handle the precisions *)
let ( >>+ ) (a, b) f =
  let pa = prec a and pb = prec b in
  if Precisions.eq pa pb then f pa
  else raise Different_Precisions

(* Create a new I element *)
let make ?(nan = false) x y =
  let there_a_nan = F.is_nan x || F.is_nan y in
  let bad_order   = F.gt x y in
  let bad_precs   = not @@ Precisions.eq (F.prec x) (F.prec y) in
  if there_a_nan || bad_order || bad_precs then
    Value_parameters.fatal
      "Numerors: impossible to create an interval with bounds %a and %a"
      F.pretty x F.pretty y
  else if F.is_pos_zero x && F.is_neg_zero y then
    I (y, x, nan)
  else
    I (x, y, nan)

(* Add a NaN to an interval *)
let add_nan itv = itv >>- fun (x, y, _) -> I (x, y, true)

(* Change the infinite bounds into max float *)
let make_finite ~prec = function
  | NaN _ -> `Bottom
  | I (x, y, _) when F.is_inf x && F.eq x y -> `Bottom
  | I (x, y, n) ->
    let make_finite x =
      if F.is_inf x then
        let rec pow = function
          | 0 -> 1 | 1 -> 2
          | n -> let b = pow (n / 2) in b * b * (if n mod 2 = 0 then 1 else 2)
        in
        let exp = pow ((Precisions.exponent prec) - 1) in
        let exp = match F.sign x with
            Sign.Positive -> exp | Sign.Negative -> 2 - exp
        in F.pow_int (F.of_int ~prec 2) exp
      else x
    in `Value (I (make_finite x, make_finite y, n))

(* Change the precision *)
let change_prec prec = function
  | NaN _ -> NaN prec
  | I (x, y, n) ->
    let x = F.change_prec ~rnd:Rounding.Down ~prec x in
    let y = F.change_prec ~rnd:Rounding.Up   ~prec y in
    I (x, y, n)

(*-----------------------------------------------------------------------------
 *                            Constructors
 *---------------------------------------------------------------------------*)
let top  ~prec = make ~nan:true (F.neg_inf prec) (F.pos_inf prec)
let zero ~prec = make (F.neg_zero prec) (F.pos_zero prec)
let nan  ~prec = NaN prec

let pos_inf ~prec = make (F.pos_inf prec) (F.pos_inf prec)
let neg_inf ~prec = make (F.neg_inf prec) (F.neg_inf prec)

let of_ints    ~prec (x, y) =
  let fx = F.of_int    ~rnd:Rounding.Down ~prec x in
  let fy = F.of_int    ~rnd:Rounding.Up   ~prec y in
  make fx fy

let of_floats  ~prec (x, y) =
  let fx = F.of_float  ~rnd:Rounding.Down ~prec x in
  let fy = F.of_float  ~rnd:Rounding.Up   ~prec y in
  make fx fy

let of_floats_without_rounding ~prec (x, y) =
  let fx = F.of_float  ~rnd:Rounding.Near ~prec x in
  let fy = F.of_float  ~rnd:Rounding.Near ~prec y in
  make fx fy

let of_strings ~prec (x, y) =
  let fx = F.of_string ~rnd:Rounding.Down ~prec x in
  let fy = F.of_string ~rnd:Rounding.Up   ~prec y in
  make fx fy

let of_numerors_floats (x, y) = make x y

let pos_zero ~prec = of_floats ~prec (0.0, 0.0)

let epsilon p =
  let e = F.machine_epsilon p in
  of_numerors_floats (F.neg e, e)

let enlarge itv = itv >>- fun (x, y, n) ->
  I (F.prev_float x, F.next_float y, n)



(*-----------------------------------------------------------------------------
 *                        Comparison methods
 *---------------------------------------------------------------------------*)
let compare a b = (a, b) >>+ fun _ ->
  match a, b with
  | NaN _, NaN _ -> 0 | NaN _, _ ->  1 | _, NaN _ -> -1
  | I (x, y, n), I (x', y', n') ->
    let c = Transitioning.Stdlib.compare n n' in
    if c = 0 then
      let c = F.compare x x' in
      if c = 0 then F.compare y y'
      else c
    else c
let eq a b = compare a b =  0
let le a b = compare a b <= 0
let lt a b = compare a b <  0
let ge a b = compare a b >= 0
let gt a b = compare a b >  0


(*-----------------------------------------------------------------------------
 *                        Lattice structure
 *---------------------------------------------------------------------------*)
(* Two intervals with different precisions are not comparable. *)
let is_included a b =
  try (a, b) >>+ fun _ ->
    match a, b with
    | I (x, y, n), I (x', y', n') -> F.ge x x' && F.le y y' && (not n || n')
    | NaN _, I (_, _, true) | NaN _, NaN _ -> true
    | _ -> false
  with Different_Precisions -> false

(* The join of two intervals with different precisions
 * generates an exception *)
let join a b = (a, b) >>+ fun _ ->
  match a, b with
  | I (x, y, n), I (x', y', n') ->
    make ~nan:(n || n') (F.min x x') (F.max y y')
  | NaN _, itv | itv, NaN _ -> add_nan itv

(* The narrow of two intervals with different precisions
 * generates an exception *)
let narrow a b = (a, b) >>+ fun prec ->
  match a, b with
  | I (x, y, n), I (x', y', n') ->
    let is_finite = F.le x' y && F.le x y' in
    let is_nan = n && n' in
    if is_finite then
      `Value (make ~nan:is_nan (F.max x x') (F.min y y'))
    else if is_nan then `Value (NaN prec)
    else `Bottom
  | (I (_, _, true) | NaN _), (NaN _ | I (_, _, true)) -> `Value (NaN prec)
  | _ -> `Bottom


(*-----------------------------------------------------------------------------
 *        Methods to check what is contained by an interval
 *---------------------------------------------------------------------------*)
let ( >>: ) (itv, b) f = match itv with NaN _ -> b | I (x, y, n) -> f (x, y, n)

let is_nan itv = (itv, true) >>: fun _ -> false

let is_finite itv = (itv, false) >>: fun (x, y, _) ->
  not (F.is_inf x) && not (F.is_inf y)

let is_pos_zero itv = (itv, false) >>: fun (x, y, _) ->
  F.is_pos_zero x && F.is_pos_zero y

let is_neg_zero itv = (itv, false) >>: fun (x, y, _) ->
  F.is_neg_zero x && F.is_neg_zero y

let is_zero itv = (itv, false) >>: fun (x, y, _) ->
  F.is_a_zero x && F.is_a_zero y

let is_pos_inf itv = (itv, false) >>: fun (x, y, _) ->
  F.is_inf x && F.is_inf y && F.is_pos x

let is_neg_inf itv = (itv, false) >>: fun (x, y, _) ->
  F.is_inf x && F.is_inf y && F.is_neg y

let contains_infinity itv = (itv, false) >>: fun (x, y, _) ->
  F.is_inf x || F.is_inf y

let contains_pos_infinity itv = (itv, false) >>: fun (x, y, _) ->
  let is_pos_inf x = F.is_inf x && F.is_pos x in
  is_pos_inf x || is_pos_inf y

let contains_neg_infinity itv = (itv, false) >>: fun (x, y, _) ->
  let is_neg_inf x = F.is_inf x && F.is_neg x in
  is_neg_inf x || is_neg_inf y

let contains_a_zero itv = (itv, false) >>: fun (x, y, _) ->
  let z = F.pos_zero (F.prec x) in F.le x z && F.le z y

let contains_pos_zero itv = (itv, false) >>: fun (x, y, _) ->
  contains_a_zero itv && (F.is_pos x || F.is_pos y)

let contains_neg_zero itv = (itv, false) >>: fun (x, y, _) ->
  contains_a_zero itv && (F.is_neg x || F.is_neg y)

let contains_nan itv = (itv, true) >>: fun (_, _, n) -> n

let contains_strictly_pos itv = (itv, false) >>: fun (_, y, _) ->
  F.is_strictly_pos y

let contains_strictly_neg itv = (itv, false) >>: fun (x, _, _) ->
  F.is_strictly_neg x

let is_strictly_pos itv = (itv, false) >>: fun (x, _, _) ->
  F.is_strictly_pos x

let is_strictly_neg itv = (itv, false) >>: fun (_, y, _) ->
  F.is_strictly_neg y


(*-----------------------------------------------------------------------------
 *                 Arithmetics. See Fval for further details.
 *---------------------------------------------------------------------------*)
(* Normal calculation with NaN and precision handling *)
let calc f ~prec a b = match a, b with
  | I (xa, ya, na), I (xb, yb, nb) -> f (xa, ya, na) (xb, yb, nb) prec
  | NaN _, _ | _, NaN _ -> NaN prec

(* Monotonic calculation *)
type operator = ?rnd:Rounding.t -> ?prec:Precisions.t -> F.t -> F.t -> F.t
let monotonic ~prec (op : operator) a b =
  let exact = not @@ Precisions.eq prec Precisions.Real in
  let f (b1, e1, n1) (b2, e2, n2) prec =
    let nan = ref (n1 || n2) in
    let results = ref [] in
    let add r = results := r :: !results in
    let treat_nan x y =
      nan := true ;
      if F.is_inf x && not (F.eq b1 e1) then
        add @@ op ~prec (F.apply_sign x (F.of_int ~prec 1)) y ;
      if F.is_inf y && not (F.eq b2 e2) then
        add @@ op ~prec x (F.apply_sign y (F.of_int ~prec 1)) ;
    in
    let op rnd x y =
      let r = op ~rnd ~prec x y in
      if F.is_nan r then treat_nan x y else add r
    in
    let s = [b1 ; b1 ; e1 ; e1] and s' = [b2 ; e2 ; b2 ; e2] in
    if not exact then begin
      List.iter2 (op Rounding.Down) s s' ;
      List.iter2 (op Rounding.Up  ) s s' ;
    end else List.iter2 (op Rounding.Near) s s' ;
    let min = List.fold_left F.min (F.pos_inf prec) !results in
    let max = List.fold_left F.max (F.neg_inf prec) !results in
    if F.compare min max > 0 then (assert !nan ; NaN prec)
    else make ~nan:!nan min max
  in calc f ~prec:prec a b

let neg itv = (itv, itv) >>: fun (x, y, n) -> make ~nan:n (F.neg y) (F.neg x)

let sqrt ?(prec = Precisions.Real) itv = (itv, NaN prec) >>: fun (x, y, n) ->
  let is_correct t = F.is_pos t || F.is_neg_zero t in
  if is_correct y then
    let y = F.sqrt ~rnd:Rounding.Up ~prec y in
    if is_correct x then
      let x = F.sqrt ~rnd:Rounding.Down ~prec x in
      make ~nan:n x y
    else make ~nan:true (F.neg_zero prec) y
  else NaN prec

let square ?(prec = Precisions.Real) itv = (itv, NaN prec) >>: fun (x, y, n) ->
  let abs_x, abs_y = F.abs x, F.abs y in
  let max = F.square ~prec (F.max abs_x abs_y) in
  let min =
    if F.is_neg x && F.is_pos y then F.pos_zero prec
    else F.square ~prec (F.min abs_x abs_y)
  in make ~nan:n min max

let add ?(prec = Precisions.Real) =
  monotonic ~prec F.add
let sub ?(prec = Precisions.Real) =
  monotonic ~prec F.sub

let mul ?(prec = Precisions.Real) a b =
  let r = monotonic ~prec F.mul a b in
  let nan_occurs x y =
    contains_infinity x && contains_a_zero y
  in if nan_occurs a b || nan_occurs b a then add_nan r else r

let div ?(prec = Precisions.Real) a b =
  let r = monotonic ~prec F.div a b in
  let nan = contains_a_zero a && contains_a_zero b in
  let has_pinf =
    contains_pos_zero b && contains_strictly_pos a ||
    contains_neg_zero b && contains_strictly_neg a
  and has_ninf =
    contains_pos_zero b && contains_strictly_neg a ||
    contains_neg_zero b && contains_strictly_pos a
  in
  let r = if has_pinf then join (pos_inf ~prec) r else r in
  let r = if has_ninf then join (neg_inf ~prec) r else r in
  if nan then add_nan r else r

let abs itv = (itv, itv) >>: fun (x, y, n) ->
  let prec = F.prec x in
  let z = F.pos_zero prec in
  if contains_a_zero itv then make ~nan:n z (F.max (F.abs x) (F.abs y))
  else if F.is_strictly_neg y then make ~nan:n (F.neg y) (F.neg x)
  else itv

let log ?(prec = Precisions.Real) itv = (itv, NaN prec) >>: fun (x, y, n) ->
  if F.is_pos y || F.is_neg_zero y then
    let y = F.log ~rnd:Rounding.Up ~prec y in
    if F.is_pos x || F.is_neg_zero x then
      let x = F.log ~rnd:Rounding.Down ~prec x in
      make ~nan:n x y
    else make ~nan:true (F.neg_inf prec) y
  else NaN prec

let exp ?(prec = Precisions.Real) itv = (itv, NaN prec) >>: fun (x, y, n) ->
  let x = F.exp ~rnd:Rounding.Down ~prec x in
  let y = F.exp ~rnd:Rounding.Up   ~prec y in
  make ~nan:n x y


(*-----------------------------------------------------------------------------
 *  Backward comparisons (be carefull that all those functions consider that y
 *  is in the upper part of the comparison, i.e we handle x <= y or y >= x,
 *  which change the order of the parameters of the functions).
 *---------------------------------------------------------------------------*)
let backward_le ?prec:_ x y =
  match x, y with
  | I (ax, bx, nx), I (_, by, ny) ->
    let b = if F.eq bx by then bx else F.min bx by in
    if F.le ax b then `Value (I (ax, b, nx || ny))
    else `Bottom
  | _, _ -> `Value x

let backward_lt ?(prec = Precisions.Real) x y =
  let e = F.machine_epsilon prec in
  match x, y with
  | I (ax, bx, nx), I (_, by, ny) ->
    let b =
      let by = F.sub ~prec by (F.mul ~prec e by) in
      if F.eq bx by then bx else F.min bx by
    in
    if F.le ax b then `Value (I (ax, b, nx || ny))
    else `Bottom
  | _, _ -> `Value x

let backward_ge ?prec:_ y x =
  match x, y with
  | I (ax, _, nx), I (ay, by, ny) ->
    let a = if F.eq ax ay then ay else F.max ax ay in
    if F.le a by then `Value (I (a, by, nx || ny))
    else `Bottom
  | _, _ -> `Value y

let backward_gt ?(prec = Precisions.Real) y x =
  let e = F.machine_epsilon prec in
  match x, y with
  | I (ax, _, nx), I (ay, by, ny) ->
    let a =
      let ax = F.add ~prec ax (F.mul ~prec e ax) in
      if F.eq ax ay then ay else F.max ax ay
    in
    if F.le a by then `Value (I (a, by, nx || ny))
    else `Bottom
  | _, _ -> `Value y



(*-----------------------------------------------------------------------------
 *
 *---------------------------------------------------------------------------*)
let finite_values ~prec = function
  | NaN _ -> None
  | I (x, y, _) ->
    let min = F.max (F.maximal_neg_float prec) x in
    let max = F.min (F.maximal_pos_float prec) y in
    if max < min then None else Some (min, max)

let backward_op (op : operator) fnan ?(prec = Precisions.Real) value result =
  if contains_infinity result || (contains_nan value && contains_nan result)
  then `Value (top prec)
  else
    let reduced_for_nan =
      if contains_nan result
      then `Value (fnan value)
      else `Bottom
    and reduced_for_finite =
      match finite_values prec result, finite_values prec value with
      | None, _ | _, None -> `Bottom
      | Some (xres, yres), Some (xval, yval) ->
        if Precisions.eq prec Precisions.Real then
          let x = op ~rnd:Rounding.Up    ~prec xres yval in
          let y = op ~rnd:Rounding.Down  ~prec yres xval in
          `Value (make ~nan:false x y)
        else
          let xres = F.prev_float xres and yres = F.next_float yres in
          let x = op ~rnd:Rounding.Near  ~prec xres yval in
          let y = op ~rnd:Rounding.Near  ~prec yres xval in
          `Value (make ~nan:false x y)
    in Bottom.join join reduced_for_nan reduced_for_finite
[@@inline]

let synthetize left right =
  match left, right with
  | `Bottom, _ | _, `Bottom -> `Bottom
  | `Value left, `Value right -> `Value (left, right)
[@@inline]

let backward_add ?(prec = Precisions.Real) ~left ~right ~result =
  let reduce_for_nan value =
    match contains_pos_infinity value, contains_neg_infinity value with
    | true , true   -> I (F.neg_inf prec, F.pos_inf prec, true)
    | true , false  -> I (F.neg_inf prec, F.neg_inf prec, true)
    | false, true   -> I (F.pos_inf prec, F.pos_inf prec, true)
    | false, false  -> NaN prec
  in
  let right' = backward_op F.sub reduce_for_nan ~prec left  result in
  let left'  = backward_op F.sub reduce_for_nan ~prec right result in
  synthetize left' right'

let backward_sub ?(prec = Precisions.Real) ~left ~right ~result =
  match backward_add ~prec ~left ~right:(neg right) ~result with
  | `Bottom -> `Bottom
  | `Value (left, right) -> `Value (left, neg right)

let backward_mul ?(prec = Precisions.Real) ~left ~right ~result =
  let reduce_for_nan value =
    match contains_infinity value, contains_a_zero value with
    | true, _ | _, true -> I (F.neg_inf prec, F.pos_inf prec, true)
    | false, false -> NaN prec
  in
  let right' = backward_op F.div reduce_for_nan ~prec left  result in
  let left'  = backward_op F.div reduce_for_nan ~prec right result in
  synthetize left' right'

let backward_div ?(prec = Precisions.Real) ~left ~right ~result =
  let reduce_for_nan value =
    match contains_infinity value, contains_a_zero value with
    | true, _ | _, true -> I (F.neg_inf prec, F.pos_inf prec, true)
    | false, false -> NaN prec
  in
  let right' =
    match backward_op F.div reduce_for_nan ~prec left  result with
    | `Value right' ->
      let one = F.of_int ~prec 1 in
      let one = I (one, one, false) in
      `Value (div ~prec one right')
    | `Bottom -> `Bottom
  in
  let left'  = backward_op F.div reduce_for_nan ~prec right result in
  synthetize left' right'
