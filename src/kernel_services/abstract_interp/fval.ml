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

[@@@ ocaml.warning "-32"]
(* Those OCaml functions do not have the proper semantics w.r.t -0/+0. We
   make them inoperative for this entire file. *)

let min = ()
let max = ()
let compare = ()

[@@@ ocaml.warning "+32"]

type kind = Float_sig.prec = Single | Double | Long_Double | Real

let kind = function
  | Cil_types.FFloat -> Single
  | Cil_types.FDouble -> Double
  | Cil_types.FLongDouble -> Long_Double

let pretty_kind fmt kind =
  Format.pp_print_string fmt
    (match kind with
     | Single -> "Single"
     | Double -> "Double"
     | Long_Double -> "Long Double"
     | Real -> "Real")

module F = struct
  (** no NaN should be produced by function of this module
      An exception, that should not be caught is returned instead. *)

  type t = float

  let packed_descr = Structural_descr.p_float

  (** NOTE: all floating-point comparisons using OCaml's standard operators
      do NOT distinguish between -0.0 and 0.0.
      Whenever floats are compared using them, it implies that negative zeroes
      are also considered, e.g. "if x < 0.0" is equivalent to "if x < -0.0",
      which is also equivalent to "F.compare x (-0.0) < 0".
      This 'compare' operator distinguishes -0. and 0. *)
  (* replace "noalloc" with [@@noalloc] for OCaml version >= 4.03.0 *)
  [@@@ warning "-3"]
  external compare : float -> float -> int = "float_compare_total" "noalloc"
  [@@@ warning "+3"]
  let equal f1 f2 = compare f1 f2 = 0

  (* The Caml version of compare below is fine but the C version above is
     faster and does not allocate—it would be possible for the Caml version
     to avoid allocation, but OCaml 4.00.1 allocates 80 bytes, for instance *)
  (*  let compare f1 f2 =
        let i1 = Int64.bits_of_float f1 in
        let i2 = Int64.bits_of_float f2 in
        let m1 = (Int64.logand i1 Int64.min_int) in
        let m2 = (Int64.logand i2 Int64.min_int) in
        if m1 = m2
        then compare f1 f2
        else compare m1 m2 *)

  let pretty_normal = Floating_point.pretty_normal
  let pretty = Floating_point.pretty

  let plus_zero = 0.0

  let is_finite f = match classify_float f with
    | FP_nan | FP_infinite -> false
    | FP_normal | FP_subnormal | FP_zero -> true

  (* Must *not* be exported. All functions of this module should check the
     arguments with which they call the functions labelled "may raise Nan
     exception" *)
  exception Invalid_NaN

  (* May raise NaN exception *)
  let ensure_not_nan r =
    match classify_float r with
    | FP_nan -> raise Invalid_NaN
    | FP_normal | FP_subnormal | FP_infinite | FP_zero -> r

  let ensure_not_nan_unary f x = ensure_not_nan (f x)

  let id = fun x -> x
  let of_float = ensure_not_nan_unary id
  let to_float = id

  let next_previous_normal int64fup int64fdown float =
    let r = Int64.bits_of_float float in
    let f =
      if r >= 0L then
        int64fup
      else
        int64fdown
    in
    Int64.float_of_bits (f r)

  let next_previous int64fup int64fdown float =
    match classify_float float with
    | FP_nan -> raise Invalid_NaN
    | FP_infinite -> float
    | FP_normal | FP_subnormal -> begin
        let f = next_previous_normal int64fup int64fdown float in
        match classify_float f with
        | FP_nan -> assert false (* can only be produced from an infinity *)
        | FP_infinite | FP_normal | FP_subnormal | FP_zero -> f
      end
    | FP_zero ->
      (next_previous_normal int64fup int64fdown (float +. min_float)) -. min_float

  let next_float fkind f' =
    let f = next_previous Int64.succ Int64.pred f' in
    match fkind with
    | Real | Long_Double -> f'
    | Double -> f
    | Single ->
      Floating_point.set_round_upward ();
      let f = Floating_point.round_to_single_precision_float f in
      Floating_point.set_round_nearest_even ();
      f

  let prev_float fkind f' =
    let f = next_previous Int64.pred Int64.succ f' in
    match fkind with
    | Real | Long_Double -> f'
    | Double -> f
    | Single ->
      Floating_point.set_round_downward ();
      let f = Floating_point.round_to_single_precision_float f in
      Floating_point.set_round_nearest_even ();
      f
end

include Float_interval.Make (Fc_float)

let top = top Real

let inject_singleton f = inject ~nan:false f f

let minus_zero = inject_singleton (-0.)
let plus_zero = inject_singleton 0.
let zeros = inject ~nan:false (-0.) 0.

(* If [fkind] is [Float32], we check that [b] and [e] are valid
   32-bit representations: lower bits are 0, and the value fits inside
   a 32-bit float. *)
let check_representability prec b e =
  if prec = Single &&
     (Floating_point.round_to_single_precision_float b <> b ||
      Floating_point.round_to_single_precision_float e <> e)
  then
    let this_one fmt x =
      if Floating_point.round_to_single_precision_float x <> x
      then Format.pp_print_string fmt "->"
    in
    Kernel.fatal "Ival: invalid float32, %ab=%g (%a) %ae=%g (%a)"
      this_one b
      b (Floating_point.pretty_normal ~use_hex:true) b
      this_one e
      e (Floating_point.pretty_normal ~use_hex:true) e

let inject ?(nan = false) prec b e =
  check_representability prec b e;
  inject ~nan b e

let round_to_single_precision_float = forward_cast ~dst:Single

let meet = narrow

let pi =
  (* [pi] is the nearest double to \pi, and is smaller than \pi. *)
  let pi = 3.14159265358979323846 in
  inject Real ~nan:false pi (F.next_float Double pi)

let e =
  (* [e] is the nearest double to \e, and is smaller than \e. *)
  let e = 2.7182818284590452354 in
  inject Real ~nan:false e (F.next_float Double e)

let contains_plus_zero = contains_pos_zero

exception Not_Singleton_Float

let floor _ = floor
let ceil _ = ceil
let trunc _ = trunc
let fround _ = fround

let project_float t = match min_and_max t with
  | Some (b, e), false when F.equal b e -> b
  | _ -> raise Not_Singleton_Float

let backward_cast_float_to_double = backward_cast ~src:Single
let backward_cast_double_to_real t = t

let subdiv_float_interval = subdivide

(*
Local Variables:
compile-command: "make -C ../../.. byte"
End:
*)
