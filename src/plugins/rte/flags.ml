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

(* -------------------------------------------------------------------------- *)
(* --- Fine Tuning Visitors                                               --- *)
(* -------------------------------------------------------------------------- *)

type t = {
  remove_trivial: bool;
  initialized: bool;
  mem_access: bool;
  div_mod: bool;
  shift: bool;
  left_shift_negative: bool;
  right_shift_negative: bool;
  signed_overflow: bool;
  unsigned_overflow: bool;
  signed_downcast: bool;
  unsigned_downcast: bool;
  float_to_int: bool;
  finite_float: bool;
  pointer_call: bool;
  bool_value: bool;
}

let all = {
  remove_trivial = true;
  initialized = true;
  mem_access = true;
  div_mod = true;
  shift = true;
  left_shift_negative = true;
  right_shift_negative = true;
  signed_overflow = true;
  unsigned_overflow = true;
  signed_downcast = true;
  unsigned_downcast = true;
  float_to_int = true;
  finite_float = true;
  pointer_call = true;
  bool_value = true;
}

let none = {
  remove_trivial = false;
  initialized = false;
  mem_access = false;
  div_mod = false;
  shift = false;
  left_shift_negative = false;
  right_shift_negative = false;
  signed_overflow = false;
  unsigned_overflow = false;
  signed_downcast = false;
  unsigned_downcast = false;
  float_to_int = false;
  finite_float = false;
  pointer_call = false;
  bool_value = false;
}

(* Which annotations should be added,
   from local options, or deduced from the options of RTE and the kernel *)

let option (get : unit -> bool) = function None -> get () | Some flag -> flag

let default
    ?remove_trivial
    ?initialized
    ?mem_access
    ?div_mod
    ?shift
    ?left_shift_negative
    ?right_shift_negative
    ?signed_overflow
    ?unsigned_overflow
    ?signed_downcast
    ?unsigned_downcast
    ?float_to_int
    ?finite_float
    ?pointer_call
    ?bool_value
    () =
  {
    remove_trivial = option (fun () -> not (Options.Trivial.get ())) remove_trivial ;
    initialized = option Options.DoInitialized.get initialized ;
    mem_access = option Options.DoMemAccess.get mem_access ;
    div_mod = option Options.DoDivMod.get div_mod ;
    shift = option Options.DoShift.get shift;
    left_shift_negative = option Kernel.LeftShiftNegative.get left_shift_negative ;
    right_shift_negative = option Kernel.RightShiftNegative.get right_shift_negative ;
    signed_overflow = option Kernel.SignedOverflow.get signed_overflow ;
    unsigned_overflow = option Kernel.UnsignedOverflow.get unsigned_overflow ;
    signed_downcast = option Kernel.SignedDowncast.get signed_downcast ;
    unsigned_downcast = option Kernel.UnsignedDowncast.get unsigned_downcast ;
    float_to_int = option Options.DoFloatToInt.get float_to_int ;
    finite_float = option (fun () -> Kernel.SpecialFloat.get () <> "none") finite_float ;
    pointer_call = option Options.DoPointerCall.get pointer_call ;
    bool_value = option Kernel.InvalidBool.get bool_value ;
  }

(* -------------------------------------------------------------------------- *)
