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

open Cil_types


(*-----------------------------------------------------------------------------
 *     Module describing the different precisions that will be manipulated
 *---------------------------------------------------------------------------*)
module Precisions = struct

  type t = Simple | Double | Long_Double | Real

  let rp () = Value_parameters.Numerors_Real_Size.get ()

  let pretty fmt = function
    | Simple -> Format.fprintf fmt "Simple"
    | Double -> Format.fprintf fmt "Double"
    | Long_Double -> Format.fprintf fmt "Long Double"
    | Real -> Format.fprintf fmt "Real"

  let of_fkind = function
    | FFloat      -> Simple
    | FDouble     -> Double
    | FLongDouble -> Long_Double

  (* Defined by the IEEE-754 standard *)
  let get = function
    | Simple      -> 24     | Double      -> 53
    | Long_Double -> 113    | Real        -> rp ()

  (* Defined by the IEEE-754 standard *)
  let exponent = function
    | Simple      -> 8      | Double      -> 11
    | Long_Double -> 15     | Real        -> Pervasives.max_int

  (* Computed as - ((2 - 2^(e-1)) - (m - 1)) where e is the number of bits of
     the exponent and m is the number of bits of the significand *)
  let denormalized = function
    | Simple      -> -149    | Double  -> -1074
    | Long_Double -> -16494  | Real    -> Pervasives.min_int

  let compare a b = Pervasives.compare (get a) (get b)
  let eq a b = compare a b =  0

  let max a b = if compare a b <= 0 then b else a
  let min a b = if compare a b <= 0 then a else b

end


(*-----------------------------------------------------------------------------
 *                         Sign type for infinites
 *---------------------------------------------------------------------------*)
module Sign = struct

  type t = Positive | Negative

  let pretty fmt = function
    | Positive -> Format.fprintf fmt "+"
    | Negative -> Format.fprintf fmt "-"

  let of_int i = if i < 0 then Negative else Positive

  let compare a b =
    match a, b with
    | Positive, Positive | Negative, Negative -> 0
    | Positive, Negative ->  1
    | Negative, Positive -> -1
  let eq a b = compare a b = 0

  let neg s = if s = Positive then Negative else Positive
  let mul a b = if eq a b then Positive else Negative

  let is_pos = eq Positive
  let is_neg = eq Negative

end


(*-----------------------------------------------------------------------------
 *                              Rounding mode
 *---------------------------------------------------------------------------*)
module Rounding = struct

  type t = Up | Down | Near

  let pretty fmt = function
    | Up    -> Format.fprintf fmt "Up"
    | Down  -> Format.fprintf fmt "Down"
    | Near  -> Format.fprintf fmt "Near"

  let eq a b =
    match a, b with
    | Up, Up | Down, Down | Near, Near -> true
    | _, _ -> false

end


(*-----------------------------------------------------------------------------
 *                          Interaction mode
 *---------------------------------------------------------------------------*)
module Mode = struct

  type t = Abs_From_Rel | Rel_From_Abs | No_Interaction | With_Interactions

  let get () =
    match Value_parameters.Numerors_Mode.get () with
    | "relative" -> Rel_From_Abs
    | "absolute" -> Abs_From_Rel
    | "none" -> No_Interaction
    | "both" -> With_Interactions
    | _ -> assert false

end
