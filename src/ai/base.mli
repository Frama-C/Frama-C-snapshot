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

(** Abstraction of the base of an addressable memory zone, together with
    the validity of the zone.*)

open Abstract_interp

type cstring = CSString of string | CSWstring of Escape.wstring
(** This type abstracts over the two kinds of constant strings present
    in strings. It is used in a few modules below Base. *)

type base = private
  | Var of Cil_types.varinfo * validity
      (** Base for a standard C variable. *)
  | Initialized_Var of Cil_types.varinfo * validity
      (** Base for a variable with a non-standard initial value. This exact
          value is defined in module {!Cvalue.Default_offsetmap}. *)
  | CLogic_Var of Cil_types.logic_var * Cil_types.typ * validity
      (** Base for a logic variable that has a C type. *)
  | Null (** Base for an addresse like [(int* )0x123] *)
  | String of int (** unique id of the constant string (one per code location)*)
    * cstring (** contents of the constant string *)

and validity =
  | Known of Int.t * Int.t (** Valid between those two bits *)
  | Unknown of Int.t * Int.t option * Int.t 
      (** Unknown(b,k,e) indicates:
          If k is [None], potentially valid between b and e
          If k is [Some k], then b <= k <= e, and the base is
           - valid between b and k;
  	   - potentially valid between k+1 and e:
          Accesses on potentially valid parts will succeed, but will also
          raise an alarm. *)
  | Periodic of Int.t * Int.t (** min-max bounds*) * Int.t (** Period *)
      (** Valid between the two bounds, and considered as a repetition
          of the given period. Only one period is stored; consequently,
          strong updates are impossible. *)
  | Invalid (** Valid nowhere. Typically used for the NULL base, or for
                function pointers. *)

module Base: sig
  include Datatype.S_with_collections with type t = base
  val id: t -> int
end

include Datatype.S_with_collections with type t = base

module Hptset: Hptset.S
  with type elt = t
  and type 'a shape = 'a Hptmap.Shape(Base).t

module SetLattice: Lattice_type.Lattice_Hashconsed_Set with module O = Hptset


(** [pretty_addr fmt base] pretty-prints the name of [base] on [fmt], with 
    a leading ampersand if it is a variable *)
val pretty_addr : Format.formatter -> t -> unit

val typeof : t -> Cil_types.typ option
(** Type of the memory block that starts from the given base. Useful to give to
    the function {!Bit_utils.pretty_bits}, typically. *)


(** {2 Validity} *)

val pretty_validity : Format.formatter -> validity -> unit
val validity : t -> validity
val validity_from_type : Cil_types.varinfo -> validity
val valid_range: validity -> Lattice_Interval_Set.Int_Intervals.t


(** {2 Finding bases} *)

val of_varinfo: Cil_types.varinfo -> t
val of_string_exp: Cil_types.exp -> t
val of_c_logic_var: Cil_types.logic_var -> t
(** Must only be called on logic variables that have a C type *)


(** {2 Origin of the variable underlying a base} *)

exception Not_a_C_variable
val to_varinfo: t -> Cil_types.varinfo
(** @return the variable's varinfo if the base corresponds to a C variable
      (in particular, not a logic variable).
    @raise Not_a_C_variable otherwise. *)

val is_formal_or_local : t -> Cil_types.fundec -> bool
val is_any_formal_or_local : t -> bool
val is_any_local : t -> bool
val is_global : t -> bool
val is_formal_of_prototype : t -> Cil_types.varinfo -> bool
val is_local: t -> Cil_types.fundec -> bool
val is_formal: t -> Cil_types.fundec -> bool
val is_block_local: t -> Cil_types.block -> bool
val is_function : t -> bool


(** {2 NULL base} *)

val null : t
val is_null : t -> bool

val min_valid_absolute_address: unit -> Int.t
val max_valid_absolute_address: unit -> Int.t
(** Bounds for option absolute-valid-range *)


(** {2 Size of a base} *)

val bits_sizeof : t -> Int_Base.t

exception Not_valid_offset
val is_valid_offset : for_writing:bool -> Int.t -> t -> Ival.t -> unit
(** Is the given bits-expressed offset guaranteed to be valid? Does nothing in
    this case, and raises [Not_valid_offset] if the offset may be invalid. *)

val base_max_offset: t -> Ival.t
(** Maximal valid offset (in bits) of the given base. Returns [Ival.bottom]
    for invalid bases. Returns an interval for bases with an unknown validity.*)


(** {2 Misc} *)

val is_read_only : t -> bool
(** Is the base valid as a read/write location, or only for reading.
    The [const] attribute is not currently taken into account. *)
val id : t -> int
val is_aligned_by : t -> Int.t -> bool


(** {2 Registering bases}
    This is only useful to create an initial memory state for analysis,
    and is never needed for normal users. *)

val register_initialized_var: Cil_types.varinfo -> validity -> t
val register_memory_var : Cil_types.varinfo -> validity -> t
  (** Memory variables are variables not present in the source of the program.
      They are created only to fill the contents of another variable, or
      through dynamic allocation. Their field [vlogic] is set to true. *)
  (* TODO: change name of [vlogic] field. *)

(*
Local Variables:
compile-command: "make -C ../.."
End:
*)
