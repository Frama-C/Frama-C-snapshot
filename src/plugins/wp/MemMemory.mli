(**************************************************************************)
(*                                                                        *)
(*  This file is part of WP plug-in of Frama-C.                           *)
(*                                                                        *)
(*  Copyright (C) 2007-2019                                               *)
(*    CEA (Commissariat a l'energie atomique et aux energies              *)
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
(* --- Memory Theory                                                      --- *)
(* -------------------------------------------------------------------------- *)

open Lang
open Lang.F

(** {2 Theory} *)

val t_addr : tau
val t_malloc : tau (** allocation tables *)
val t_mem : tau -> tau (** t_addr indexed array *)

val a_null : term (** Null address. Same as [a_addr 0 0] *)
val a_global : term -> term (** Zero-offset base. Same as [a_addr base 0] *)
val a_addr : term -> term -> term (** Constructor for [{ base ; offset }] *)
val a_shift : term -> term -> term (** Shift: [a_shift a k] adds [k] to [a.offset] *)
val a_base : term -> term (** Returns the base *)
val a_offset : term -> term (** Returns the offset *)
val a_base_offset : term -> term
(** Returns the offset in {i bytes} from the {i logic} offset
    (which is a memory cell index, actually) *)

val f_null : lfun
val f_base : lfun
val f_global : lfun
val f_shift : lfun
val f_offset : lfun
val f_havoc : lfun
val f_region : lfun
val f_addr_of_int : lfun (** Physical address *)
val f_int_of_addr : lfun (** Physical address *)

val p_addr_lt : lfun
val p_addr_le : lfun
val p_linked : lfun
val p_framed : lfun
val p_sconst : lfun
val p_separated : lfun
val p_included : lfun
val p_valid_rd : lfun
val p_valid_rw : lfun
val p_invalid : lfun
val p_eqmem : lfun

(* -------------------------------------------------------------------------- *)

(** {2 Addr Producer Registration} *)

(** Register simplifiers for functions producing [addr] terms:
    - [~base es] is the simplifier for [(f es).base]
    - [~offset es] is the simplifier for [(f es).offset]
    - [~linear:true] register simplifier [f(f(p,i),k)=f(p,i+j)] on [f]
    - [~equal a b] is the [set_eq_builtin] for [f]

    The equality builtin is wrapped inside a default builtin that
    compares [f es] by computing [base] and [offset].
*)

val register :
  ?base:(term list -> term) ->
  ?offset:(term list -> term) ->
  ?equal:(term -> term -> pred) ->
  ?linear:bool ->
  lfun -> unit


(** {2 Frame Conditions}

    [frames ~addr] are frame conditions for reading a value
    at address [addr] from a chunk of memory.
    The value read at [addr] have length [offset],
    while individual element in memory chunk have type [tau] and
    offset length [sizeof].

    Memory variables use [~basename] or ["mem"] by default.
*)

val frames : addr:term -> offset:term -> sizeof:term ->
  ?basename:string -> tau -> Sigs.frame list

(** {2 Range of Address} *)

val separated :
  shift:('a -> Ctypes.c_object -> term -> 'a) ->
  addrof:('a -> term) ->
  sizeof:(Ctypes.c_object -> int) ->
  'a Sigs.rloc -> 'a Sigs.rloc -> pred

val included :
  shift:('a -> Ctypes.c_object -> term -> 'a) ->
  addrof:('a -> term) ->
  sizeof:(Ctypes.c_object -> int) ->
  'a Sigs.rloc -> 'a Sigs.rloc -> pred

(* -------------------------------------------------------------------------- *)
