(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2018                                               *)
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

open Eval
open Numerors_utils
module P = Precisions
module F = Numerors_float
module I = Numerors_interval


(* Type declaration *)
type t = { exact : I.t ; approx : I.t ; abs_err : I.t ; rel_err : I.t }

(* Printer *)
let pretty fmt t =
  Format.fprintf fmt
    "@[Exact   : %a@ Approx  : %a :: %a@ Abs Err : %a@ Rel Err : %a@]"
    I.pretty t.exact   I.pretty t.approx P.pretty (I.prec t.approx)
    I.pretty t.abs_err I.pretty t.rel_err

(* Zero *)
let zero t =
  let z = I.zero ~prec:P.Real in
  let prec = I.prec t.approx in
  { exact = z ; approx = I.zero ~prec ; abs_err = z ; rel_err = z }

(* Precision *)
let prec t = I.prec t.approx

(* Creation *)
let create exact approx abs_err rel_err =
  { exact ; approx ; abs_err ; rel_err }


(*-----------------------------------------------------------------------------
 *                            Iterator
 *---------------------------------------------------------------------------*)
let iterate (facc, init) felt t1 t2 =
  let to_l t = [ t.exact ; t.approx ; t.abs_err ; t.rel_err ] in
  List.fold_left2 (fun acc x y -> facc (felt x y) acc) init (to_l t1) (to_l t2)

let apply f x y =
  { exact   = f x.exact y.exact
  ; approx  = f x.approx y.approx
  ; abs_err = f x.abs_err y.abs_err
  ; rel_err = f x.rel_err y.rel_err
  }

(*-----------------------------------------------------------------------------
 *                         Lattice methods
 *---------------------------------------------------------------------------*)
let is_included = iterate ((&&), true) (I.is_included)

let join = apply I.join

let narrow x y =
  I.narrow x.exact   y.exact   >>- fun exact   ->
  I.narrow x.approx  y.approx  >>- fun approx  ->
  I.narrow x.abs_err y.abs_err >>- fun abs_err ->
  I.narrow x.rel_err y.rel_err >>- fun rel_err ->
  `Value { exact ; approx ; abs_err ; rel_err }


(*-----------------------------------------------------------------------------
 *                           Comparison
 *---------------------------------------------------------------------------*)
let compare x y =
  let f c acc = if acc = 0 then c else acc in
  iterate (f, 0) I.compare x y


(*-----------------------------------------------------------------------------
 *                         Miscellaneous
 *---------------------------------------------------------------------------*)

(* Returns the precision of all the elements of <l> if they all share the
 * same. Stop the program if not. *)
let ( >>+ ) l f =
  assert (l != []) ; let p = I.prec (List.hd l).approx in
  let iterator x = assert (P.eq p (I.prec x.approx)) in
  List.iter iterator (List.tl l) ; f p

(* Use to return the results of the backward operators *)
let generic_backward (a, a') (b, b') =
  let x = I.narrow a a' in
  let y = I.narrow b b' in
  x >>- fun x -> y >>- fun y -> `Value (x, y)

(* Returns the narrow of the operands at the precision of reals.
 * Asserts that it does not result in a bottom *)
let non_bottom_narrow a b =
  match I.narrow a b with
  | `Value v -> v
  | `Bottom ->
    Value_parameters.fatal
      "Numerors: a narrowing leads incorrectly to bottom.@ \
       Narrow between %a@ and %a."
      I.pretty a I.pretty b

(* NaN have no meaning for absolute or relative errors: if the computation of
   an error leads to NaN, use top instead to ensure soundness. *)
let handle_nan_errors r = if I.contains_nan r then I.top ~prec:P.Real else r

(* Narrow errors [x] and [y]. *)
let narrow_errors x y =
  non_bottom_narrow (handle_nan_errors x) (handle_nan_errors y)

(* Interval containing the singleton 1.0 *)
let one = I.of_ints ~prec:P.Real (1, 1)

(* Internal : constant 2 *)
let two = F.of_int 2

(* Change the precisions *)
let change_prec prec t =
  let approx = I.change_prec prec t.approx in
  if I.is_nan t.approx then
    { t with approx ; abs_err = I.top P.Real ; rel_err = I.top P.Real }
  else
    let epsilon = F.machine_epsilon prec in
    let abs_err =
      let diff =
        let calc = I.sub approx t.approx in
        let default =
          let ufp = F.pow_int two (I.get_max_exponent t.approx) in
          let t = F.mul ufp epsilon in
          I.of_numerors_floats (F.neg t, t)
        in narrow_errors calc default
      in I.add diff t.abs_err
    and rel_err = I.of_numerors_floats (F.neg epsilon, epsilon)
    in { exact = t.exact ; approx ; abs_err ; rel_err }

(* Rounding error of transcendental functions. We suppose that this rounding
   error can be seen using the rounding model of the article amplified by a
   coefficient. Actually, this coefficient is set to one, but it will become a
   parameter of Numerors in the future. *)
let rnd_err prec exact =
  let lambda = I.of_floats ~prec:P.Real (1., 1.) in
  let epsilon =
    let e = F.machine_epsilon prec in
    I.mul lambda @@ I.of_numerors_floats (F.neg e, e)
  and delta =
    let d = F.machine_delta prec in
    I.mul lambda @@ I.of_numerors_floats (F.neg d, d)
  in I.add (I.mul epsilon exact) delta

(* Elementary Rounding Errors *)
module Elementary : sig

  val abs : prec:P.t -> I.t -> I.t
  val rel : prec:P.t -> I.t -> I.t

end = struct

  (* Get machine constants depending of the input interval *)
  let machine_constants p itv =
    if not (I.is_zero itv) then
      (* Computation of the smallest normalized number in the format *)
      let norm = F.pow two (F.sub two (F.pow_int two ((P.exponent p) - 1))) in
      (* This value will be set to the machine delta of the precision if the
         interval contains at least a subnormal number *)
      let machine_delta =
        let del_itv = I.of_numerors_floats (F.neg norm, norm) in
        match I.narrow del_itv itv with
        | `Value _  -> Some (F.machine_delta p)
        | `Bottom   -> None
      in
      (* This value will be set to the machine epsilon of the precision if the
         interval contains at least a normalized number *)
      let machine_epsilon =
        let neg_part = I.of_numerors_floats (F.neg_inf P.Real, F.neg norm) in
        let pos_part = I.of_numerors_floats (norm, F.pos_inf P.Real) in
        match I.narrow neg_part itv, I.narrow itv pos_part with
        | `Value _, _ | _, `Value _ -> Some (F.machine_epsilon p)
        | `Bottom, `Bottom -> None
      in
      machine_epsilon, machine_delta
    else None, None

  (* Elementary absolute rounding error *)
  let abs ~prec old_itv =
    match I.make_finite ~prec old_itv with
    | `Bottom -> I.nan ~prec:P.Real
    | `Value itv ->
      let epsilon_opt, delta_opt = machine_constants prec itv in
      let epsilon_part = match epsilon_opt with
        | Some epsilon ->
          (* As developped in the corresponding article, the normalized part of
             the elementary rounding error is computed as
             ufp([x])*[machine_epsilon] *)
          let ufp = F.pow_int two (I.get_max_exponent itv) in
          let t = F.mul ufp epsilon in
          I.of_numerors_floats (F.neg t, t)
        | None -> I.zero P.Real
      in
      let delta_part = match delta_opt with
        | Some delta -> I.of_numerors_floats (F.neg delta, delta)
        | None -> I.zero P.Real
      in
      let res = I.join epsilon_part delta_part in
      if not (I.eq old_itv itv) then I.add_nan res else res

  (* Elementary relative rounding error *)
  let rel ~prec old_itv =
    match I.make_finite ~prec old_itv with
    | `Bottom -> I.nan ~prec:P.Real
    | `Value itv ->
      let epsilon_opt, delta_opt = machine_constants prec itv in
      let epsilon_part = match epsilon_opt with
        | Some epsilon ->
          (* The maximum of (ufp([x])/[x]) can be optimized for intervals of
             values of the same exponent. *)
          let max_ufp =
            let one = F.of_int 1 and x, y = I.get_bounds (I.abs itv) in
            let is_framed = F.exponent x = F.exponent y in
            if is_framed && not (I.contains_a_zero itv) then
              F.div one @@ F.significand x
            else one
          in
          let t = F.mul max_ufp epsilon in
          I.of_numerors_floats (F.neg t, t)
        | None -> I.zero P.Real
      in
      let delta_part = match delta_opt with
        | Some delta ->
          let one = I.of_ints ~prec:P.Real (-1, 1) in
          let r = I.div (I.of_numerors_floats (F.neg delta, delta)) itv in
          non_bottom_narrow r one
        | None -> I.zero P.Real
      in
      let res = I.join epsilon_part delta_part in
      if not (I.eq old_itv itv) then I.add_nan res else res

end


(*-----------------------------------------------------------------------------
 *                      Interaction mode handling
 *---------------------------------------------------------------------------*)
let forward_interaction t =
  let fzero a b c =
    if I.is_zero a && I.is_zero b then a
    else if (I.is_pos_inf b || I.is_neg_inf b) && I.is_zero a then a
    else c
  in
  let abs = fzero t.rel_err t.exact (I.mul t.rel_err t.exact) in
  let rel = fzero t.abs_err t.exact (I.div t.abs_err t.exact) in
  let abs_err, rel_err =
    match Mode.get () with
    | Mode.No_Interaction -> t.abs_err, t.rel_err
    | Mode.Abs_From_Rel -> abs, t.rel_err
    | Mode.Rel_From_Abs -> t.abs_err, rel
    | Mode.With_Interactions ->
      non_bottom_narrow t.abs_err abs, non_bottom_narrow t.rel_err rel
  in create t.exact t.approx abs_err rel_err


(*-----------------------------------------------------------------------------
 *                        Arithmetic definition
 *---------------------------------------------------------------------------*)
module type Arithmetic = sig

  (* Type returned by the forward operations *)
  type forward

  module Forward  : sig
    val neg   : t -> forward
    val log   : t -> forward
    val exp   : t -> forward
    val sqrt  : t -> forward
    val add   : t -> t -> forward
    val sub   : t -> t -> forward
    val mul   : t -> t -> forward
    val div   : t -> t -> forward
  end

  module Backward : sig
    val neg   : t -> t -> I.t or_bottom
    val add   : t -> t -> t -> (I.t * I.t) or_bottom
    val sub   : t -> t -> t -> (I.t * I.t) or_bottom
    val mul   : t -> t -> t -> (I.t * I.t) or_bottom
    val div   : t -> t -> t -> (I.t * I.t) or_bottom
  end

end


(*-----------------------------------------------------------------------------
 *               Arithmetic for the calculations on reals
 *---------------------------------------------------------------------------*)
module Exact : Arithmetic with type forward = I.t = struct

  type forward = I.t

  module Forward  = struct

    let neg   v = I.neg   v.exact
    let log   v = I.log   v.exact
    let exp   v = I.exp   v.exact
    let sqrt  v = I.sqrt  v.exact

    let add x y = [x ; y] >>+ fun _ -> I.add x.exact y.exact
    let sub x y = [x ; y] >>+ fun _ -> I.sub x.exact y.exact
    let mul x y = [x ; y] >>+ fun _ -> I.mul x.exact y.exact
    let div x y = [x ; y] >>+ fun _ -> I.div x.exact y.exact

  end

  module Backward = struct

    let neg x r   = [x ; r] >>+ fun _ ->
      I.narrow x.exact (I.neg r.exact)

    let add x y r = [x ; y ; r] >>+ fun _ ->
      I.backward_add ~prec:P.Real ~left:x.exact ~right:y.exact ~result:r.exact

    let sub x y r = [x ; y ; r] >>+ fun _ ->
      I.backward_sub ~prec:P.Real ~left:x.exact ~right:y.exact ~result:r.exact

    let mul x y r = [x ; y ; r] >>+ fun _ ->
      I.backward_mul ~prec:P.Real ~left:x.exact ~right:y.exact ~result:r.exact

    let div x y r = [x ; y ; r] >>+ fun _ ->
      I.backward_div ~prec:P.Real ~left:x.exact ~right:y.exact ~result:r.exact

  end

end


(*-----------------------------------------------------------------------------
 *               Arithmetic for the calculations on reals
 *---------------------------------------------------------------------------*)
module Approx : Arithmetic with type forward = I.t = struct

  type forward = I.t

  module Forward  = struct

    let neg   v = I.neg v.approx
    let log   v = I.log  ~prec:(I.prec v.approx) v.approx
    let exp   v = I.exp  ~prec:(I.prec v.approx) v.approx
    let sqrt  v = I.sqrt ~prec:(I.prec v.approx) v.approx

    let add x y = [x ; y] >>+ fun prec ->
      I.add ~prec x.approx y.approx
    let sub x y = [x ; y] >>+ fun prec ->
      I.sub ~prec x.approx y.approx
    let mul x y = [x ; y] >>+ fun prec ->
      I.mul ~prec x.approx y.approx
    let div x y = [x ; y] >>+ fun prec ->
      I.div ~prec x.approx y.approx

  end

  module Backward = struct

    let neg x r   = [x ; r] >>+ fun _ ->
      I.narrow x.approx (I.neg r.approx)

    let add x y r = [x ; y ; r] >>+ fun prec ->
      I.backward_add ~prec ~left:x.approx ~right:y.approx ~result:r.approx

    let sub x y r = [x ; y ; r] >>+ fun prec ->
      I.backward_sub ~prec ~left:x.approx ~right:y.approx ~result:r.approx

    let mul x y r = [x ; y ; r] >>+ fun prec ->
      I.backward_mul ~prec ~left:x.approx ~right:y.approx ~result:r.approx

    let div x y r = [x ; y ; r] >>+ fun prec ->
      I.backward_div ~prec ~left:x.approx ~right:y.approx ~result:r.approx

  end

end


(*-----------------------------------------------------------------------------
 *            Arithmetic for the calculations on absolute errors
 *---------------------------------------------------------------------------*)
module Abs_Err : Arithmetic with
  type forward = exact:I.t -> approx:I.t -> I.t
= struct

  type forward = exact:I.t -> approx:I.t -> I.t

  module Forward  = struct

    let neg   v ~exact:_ ~approx:_ = I.neg v.abs_err

    let log   v ~exact ~approx =
      let naive = I.sub approx exact in
      let rnd   = rnd_err (I.prec v.approx) exact in
      let err   = I.add (I.log (I.add one v.rel_err)) rnd in
      narrow_errors naive err

    let exp   v ~exact ~approx =
      let naive = I.sub approx exact in
      let rnd   = rnd_err (I.prec v.approx) exact in
      let err   = I.add (I.mul (I.sub (I.exp v.abs_err) one) v.exact) rnd in
      narrow_errors naive err

    let sqrt  v ~exact ~approx =
      let naive = I.sub approx exact in
      let t = match Mode.get () with
        | Mode.With_Interactions -> I.sqrt (I.add v.rel_err one)
        | _ -> I.sqrt (I.add (I.div v.abs_err v.exact) one)
      in
      let g = Elementary.abs ~prec:(I.prec v.approx) (I.sqrt v.approx) in
      let err = I.add (I.mul (I.sqrt v.exact) (I.sub t one)) g in
      narrow_errors naive err

    let add x y ~exact ~approx = [x ; y] >>+ fun p ->
      let naive = I.sub approx exact in
      let ulp   = Elementary.abs ~prec:p (I.add x.approx y.approx) in
      let err   = I.add (I.add x.abs_err y.abs_err) ulp in
      narrow_errors naive err

    let sub x y ~exact ~approx = [x ; y] >>+ fun p ->
      let naive = I.sub approx exact in
      let ulp   = Elementary.abs ~prec:p (I.sub x.approx y.approx) in
      let err   = I.add (I.sub x.abs_err y.abs_err) ulp in
      narrow_errors naive err

    let mul x y ~exact ~approx = [x ; y] >>+ fun p ->
      let naive = I.sub approx exact in
      let ulp = Elementary.abs ~prec:p (I.mul x.approx y.approx) in
      let res = I.add (I.mul x.exact y.abs_err) (I.mul y.exact x.abs_err) in
      let err = I.add (I.add res (I.mul x.abs_err y.abs_err)) ulp in
      (*
      let err_2 =
        let f x y =
          let fxey = I.mul x.approx y.abs_err in
          let ryex = I.mul y.exact  x.abs_err in
          I.add (I.add fxey ryex) ulp
        in narrow_errors (f x y) (f y x)
      in
      narrow_errors (narrow_errors err_1 err_2) naive
      *)
      narrow_errors naive err

    let div x y ~exact ~approx = [x ; y] >>+ fun p ->
      let naive = I.sub approx exact in
      let ulp   = Elementary.abs ~prec:p (I.div x.approx y.approx) in
      let err   =
        if Mode.get () = Mode.With_Interactions then
          let num   = I.sub x.abs_err (I.mul x.exact y.rel_err) in
          I.add (I.div num y.approx) ulp
        else
          let h =
            let a   = I.div x.abs_err y.approx in
            let num = I.mul x.exact y.abs_err  in
            let ry2 = I.square y.exact in
            let den = I.mul ry2 (I.add one (I.div y.abs_err y.exact)) in
            I.sub a (I.div num den)
          in
          let g =
            let t = I.mul (I.div x.exact y.exact) y.abs_err in
            I.div (I.sub x.abs_err t) y.approx
          in
          I.add (narrow_errors h g) ulp
      in
      narrow_errors naive err

  end

  module Backward = struct

    let neg x r = [x ; r] >>+ fun _ ->
      I.narrow x.abs_err (I.neg r.abs_err)

    let add x y r = [x ; y ; r] >>+ fun p ->
      let ulp = Elementary.abs ~prec:p (I.add x.approx y.approx) in
      let r  = I.sub r.abs_err ulp in
      let x' = x.abs_err, I.sub r y.abs_err in
      let y' = y.abs_err, I.sub r x.abs_err in
      generic_backward x' y'

    let sub x y r = [x ; y ; r] >>+ fun p ->
      let ulp = Elementary.abs ~prec:p (I.sub x.approx y.approx) in
      let r  = I.sub r.abs_err ulp in
      let x' = x.abs_err, I.add r y.abs_err in
      let y' = y.abs_err, I.sub x.abs_err r in
      generic_backward x' y'

    let mul x y r = [x ; y ; r] >>+ fun p ->
      let ulp = Elementary.abs ~prec:p (I.mul x.approx y.approx) in
      let r = I.sub r.abs_err ulp in
      let err_1 x y = I.div (I.sub r (I.mul x.approx y.abs_err)) y.exact in
      let err_2 x y = I.div (I.sub r (I.mul x.exact y.abs_err)) y.approx in
      let calc x y = x.abs_err, non_bottom_narrow (err_1 x y) (err_2 x y) in
      generic_backward (calc x y) (calc y x)

    let div x y r = [x ; y ; r] >>+ fun p ->
      let ulp = Elementary.abs ~prec:p (I.div x.approx y.approx) in
      let r = I.sub r.abs_err ulp in
      let t = I.mul (I.div x.exact y.exact) y.abs_err in
      let err_1_x = I.add (I.mul y.approx r) t in
      let t = I.add one (I.div y.abs_err y.exact) in
      let t = I.mul (I.square y.exact) t in
      let t = I.div (I.mul x.exact y.abs_err) t in
      let err_2_x = I.mul y.approx (I.add r t) in
      let x' = x.abs_err, non_bottom_narrow err_1_x err_2_x in
      let t = I.div y.exact x.exact in
      let err_1_y = I.mul t (I.sub x.abs_err (I.mul y.approx r)) in
      let t1 = I.mul y.exact (I.sub (I.div x.abs_err y.approx) r) in
      let e1 = I.div y.exact (I.sub (I.div x.exact t1) one) in
      let t2 = I.square y.exact in
      let t2 = I.mul t2 (I.sub (I.div x.abs_err y.approx) r) in
      let e2 = I.div t2 (I.sub x.exact t1) in
      let err_2_y = non_bottom_narrow e1 e2 in
      let y' = y.abs_err, non_bottom_narrow err_1_y err_2_y in
      generic_backward x' y'

  end

end


(*-----------------------------------------------------------------------------
 *            Arithmetic for the calculations on relative errors
 *---------------------------------------------------------------------------*)
module Rel_Err : Arithmetic with
  type forward = exact:I.t -> abs_err:I.t -> I.t
= struct

  type forward = exact:I.t -> abs_err:I.t -> I.t

  (* Verify if the input are exactly zero *)
  let is_perfect_zero x =
    I.is_zero x.exact   && I.is_zero x.approx  &&
    I.is_zero x.abs_err && I.is_zero x.rel_err

  (* Type of an operator *)
  type operator = ?prec:P.t -> I.t -> I.t -> I.t

  module Forward  = struct

    (* Generic function for forward addition and substraction *)
    let add_or_sub_forward (op : operator) x y p =
      let g = Elementary.rel ~prec:p (op x.approx y.approx) in
      let den = op one (I.div y.exact x.exact) in
      let num = I.sub x.rel_err y.rel_err in
      let t = I.add g one in
      I.add (I.mul (I.add (I.div num den) y.rel_err) t) g

    let neg v ~exact:_ ~abs_err:_  = v.rel_err

    let log _v ~exact ~abs_err =
      let naive = I.div abs_err exact in
      handle_nan_errors naive

    let exp _v ~exact ~abs_err =
      let naive = I.div abs_err exact in
      handle_nan_errors naive

    let sqrt v ~exact ~abs_err =
      let p = I.prec v.approx in
      let naive = I.div abs_err exact in
      let g = I.add (Elementary.rel p (I.sqrt v.approx)) one in
      let err = I.sub (I.mul (I.sqrt (I.add v.rel_err one)) g) one in
      narrow_errors naive err

    let add x y ~exact ~abs_err = [x ; y] >>+ fun p ->
      if not (is_perfect_zero x && is_perfect_zero y) then
        let naive = I.div abs_err exact in
        let err_1 = add_or_sub_forward I.add x y p in
        let err_2 = add_or_sub_forward I.add y x p in
        List.fold_left narrow_errors naive [err_1 ; err_2]
      else I.zero ~prec:P.Real

    let sub x y ~exact ~abs_err = [x ; y] >>+ fun p ->
      if not (is_perfect_zero x && is_perfect_zero y) then
        let naive = I.div abs_err exact in
        let err_1 = add_or_sub_forward I.sub x y p in
        let err_2 = add_or_sub_forward I.sub y x p in
        List.fold_left narrow_errors naive [err_1 ; err_2]
      else I.zero ~prec:P.Real

    let mul x y ~exact ~abs_err = [x ; y] >>+ fun p ->
      if not ((I.is_zero exact) && (I.is_zero abs_err)) then
        let g = I.add (Elementary.rel ~prec:p (I.mul x.approx y.approx)) one in
        let ex, ey = I.add x.rel_err one, I.add y.rel_err one in
        let err = I.sub (I.mul (I.mul ex ey) g) one in
        let naive = I.div abs_err exact in
        narrow_errors err naive
      else exact

    let div x y ~exact ~abs_err = [x ; y] >>+ fun p ->
      if not (I.is_zero abs_err) || not (I.is_zero exact) then
        let g = I.add (Elementary.rel ~prec:p (I.div x.approx y.approx)) one in
        let ex, ey = I.add x.rel_err one, I.add y.rel_err one in
        let err = I.sub (I.mul (I.div ex ey) g) one in
        let naive = I.div abs_err exact in
        narrow_errors err naive
      else I.zero ~prec:P.Real
  end

  module Backward = struct

    (* Generic function for backward addition and substraction *)
    let add_or_sub_backward (op : operator) x y r p =
      let g = Elementary.rel ~prec:p (op x.approx y.approx) in
      let t = I.div (I.sub r.rel_err g) (I.add one g) in
      let d = I.div y.exact x.exact in
      let e = match Mode.get () with
        | Mode.With_Interactions -> I.div y.abs_err x.exact
        | _ -> I.mul y.rel_err d
      in I.sub (I.mul t (op one d)) e

    let neg x r = [x ; r] >>+ fun _ ->
      I.narrow x.rel_err r.rel_err

    let add x y r = [x ; y ; r] >>+ fun p ->
      let x' = x.rel_err, add_or_sub_backward I.add x y r p in
      let y' = y.rel_err, add_or_sub_backward I.add y x r p in
      generic_backward x' y'

    let sub x y r = [x ; y ; r] >>+ fun p ->
      let x' = x.rel_err, add_or_sub_backward I.sub x y r p in
      let y' = y.rel_err, add_or_sub_backward I.sub x y r p in
      generic_backward x' y'

    let mul x y r = [x ; y ; r] >>+ fun p ->
      let g = I.add (Elementary.rel ~prec:p (I.mul x.approx y.approx)) one in
      let d x = I.mul (I.add x.rel_err one) g in
      let n = I.add r.rel_err one in
      let ex = I.sub (I.div n (d y)) one in
      let ey = I.sub (I.div n (d x)) one in
      generic_backward (x.rel_err, ex) (y.rel_err, ey)

    let div x y r = [x ; y ; r] >>+ fun p ->
      let g = I.add (Elementary.rel ~prec:p (I.mul x.approx y.approx)) one in
      let dr = I.add r.rel_err one in
      let dx = I.add x.rel_err one in
      let dy = I.add y.rel_err one in
      let ex = I.sub (I.div (I.mul dr dy) g) one in
      let ey = I.sub (I.div (I.mul g dx) dr) one in
      generic_backward (x.rel_err, ex) (y.rel_err, ey)

  end

end


(*-----------------------------------------------------------------------------
 *                  Backward comparison operators
 *---------------------------------------------------------------------------*)
module Backward_Comparisons = struct

  let backward_interaction x y =
    let f x = non_bottom_narrow x.abs_err (I.mul x.exact x.rel_err) in
    let x_abs_err, y_abs_err = match Mode.get () with
      | Mode.With_Interactions | Mode.Abs_From_Rel -> f x, f y
      | Mode.Rel_From_Abs | Mode.No_Interaction -> x.abs_err, y.abs_err
    in { x with abs_err = x_abs_err }, { y with abs_err = y_abs_err }

  let lt x y = [x ; y] >>+ fun p ->
    I.backward_le x.exact y.exact >>- fun x_exact ->
    I.backward_ge y.exact x.exact >>- fun y_exact ->
    I.backward_lt ~prec:p x.approx y.approx >>- fun x_approx ->
    I.backward_ge ~prec:p y.approx x.approx >>- fun y_approx ->
    let bx = { x with exact = x_exact ; approx = x_approx } in
    let by = { y with exact = y_exact ; approx = y_approx } in
    `Value (backward_interaction bx by)

  let le x y = [x ; y] >>+ fun p ->
    I.backward_le x.exact y.exact >>- fun x_exact ->
    I.backward_ge y.exact x.exact >>- fun y_exact ->
    I.backward_le ~prec:p x.approx y.approx >>- fun x_approx ->
    I.backward_ge ~prec:p y.approx x.approx >>- fun y_approx ->
    let bx = { x with exact = x_exact ; approx = x_approx } in
    let by = { y with exact = y_exact ; approx = y_approx } in
    `Value (backward_interaction bx by)

  let ge x y = [x ; y] >>+ fun p ->
    I.backward_ge x.exact y.exact >>- fun x_exact ->
    I.backward_le y.exact x.exact >>- fun y_exact ->
    I.backward_ge ~prec:p x.approx y.approx >>- fun x_approx ->
    I.backward_le ~prec:p y.approx x.approx >>- fun y_approx ->
    let bx = { x with exact = x_exact ; approx = x_approx } in
    let by = { y with exact = y_exact ; approx = y_approx } in
    `Value (backward_interaction bx by)

  let gt x y = [x ; y] >>+ fun p ->
    I.backward_ge x.exact y.exact >>- fun x_exact ->
    I.backward_le y.exact x.exact >>- fun y_exact ->
    I.backward_gt ~prec:p x.approx y.approx >>- fun x_approx ->
    I.backward_le ~prec:p y.approx x.approx >>- fun y_approx ->
    let bx = { x with exact = x_exact ; approx = x_approx } in
    let by = { y with exact = y_exact ; approx = y_approx } in
    `Value (backward_interaction bx by)

end
