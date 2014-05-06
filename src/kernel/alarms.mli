(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2014                                               *)
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

(** Alarms Database.
    @modify Fluorine-20130401 fully re-implemented.
    @plugin development guide *)

open Cil_types

(** Only signed overflows int are really RTEs. The other kinds may be
    meaningful nevertheless. *)
type overflow_kind = Signed | Unsigned | Signed_downcast | Unsigned_downcast

type access_kind = For_reading | For_writing
type bound_kind = Lower_bound | Upper_bound

(** @modify Fluorine-20130401 full re-implementation *)
type alarm =
  | Division_by_zero of exp
  | Memory_access of lval * access_kind
  | Logic_memory_access (* temporary? *) of 
      term * access_kind
  | Index_out_of_bound of
      exp (** index *) 
    * exp option (** None = lower bound is zero; Some up = upper bound *) 
  | Invalid_shift of exp * int option (** strict upper bound, if any *)
  | Pointer_comparison of 
      exp option (** [None] when implicit comparison to NULL pointer *) 
    * exp
  | Differing_blocks of exp * exp (** The two expressions (which evaluate to
                     pointers) must point to the same allocated block *)
  | Overflow of
      overflow_kind
    * exp
    * Integer.t (** the bound *) 
    * bound_kind
  | Float_to_int of
      exp
    * Integer.t (** the bound for the integer type. The actual alarm
                    is [exp < bound+1] or [bound-1 < exp]. *) 
    * bound_kind
  | Not_separated of lval * lval  (** the two lvalues must be separated *)
  | Overlap of lval * lval (** overlapping read/write: the two lvalues must be
                               separated or equal *)
  | Uninitialized of lval
  | Is_nan_or_infinite of exp * fkind
  | Valid_string of exp

include Datatype.S_with_collections with type t = alarm

val self: State.t

val register: 
  Emitter.t -> ?kf:kernel_function -> kinstr -> ?loc:location -> 
  ?status:Property_status.emitted_status -> ?save:bool -> alarm -> 
  code_annotation * bool
(** Register the given alarm on the given statement. By default, no status is
    generated. If [save] is [false] (default is [true]), the annotation
    corresponding to the alarm is built, but neither it nor the alarm is
    registered. [kf] must be given only if the [kinstr] is a statement, and
    must be the function enclosing this statement.
    @return true if the given alarm has never been emitted before on the
    same kinstr (without taking into consideration the status or
    the emitter). 
    @modify Oxygen-20120901 remove labeled argument ~deps
    @modify Fluorine-20130401 add the optional arguments [kf], [loc] and
    [save]; also returns the corresponding code_annotation *)

val iter: 
  (Emitter.t -> kernel_function -> stmt -> rank:int -> alarm -> code_annotation
   -> unit) 
  -> unit
(** Iterator over all alarms and the associated annotations at some program
    point.
    @since Fluorine-20130401 *)

val fold: 
  (Emitter.t -> kernel_function -> stmt -> rank:int -> alarm -> code_annotation
   -> 'a
   -> 'a) 
  -> 'a
  -> 'a
(** Folder over all alarms and the associated annotations at some program
    point.
    @since Fluorine-20130401 *)

val find: code_annotation -> alarm option
(** @return the alarm corresponding to the given assertion, if any.
    @since Fluorine-20130401 *)

val remove: ?filter:(alarm -> bool) -> ?kinstr:kinstr -> Emitter.t -> unit
(** Remove the alarms and the associated annotations emitted by the given
    emitter. If [kinstr] is specified, remove only the ones associated with this
    kinstr. If [filter] is specified, remove only the alarms [a] such that
    [filter a] is [true]. 
    @since Fluorine-20130401 *)

val create_predicate: ?loc:location -> t -> predicate named
(** Generate the predicate corresponding to a given alarm. 
    @since Fluorine-20130401 *)

(*
Local Variables:
compile-command: "make -C ../.."
End:
*)
