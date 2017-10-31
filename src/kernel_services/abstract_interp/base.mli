(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2017                                               *)
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

(** Validity for variables that might change size. *)
type variable_validity = private {
  mutable weak : bool (** Indicate that the variable is weak, i.e. that
                          it may represent multiple memory locations *);
  mutable min_alloc : Int.t (** First bit guaranteed to be valid; can be -1 *);
  mutable max_alloc : Int.t (** Last possibly valid bit *);
  max_allocable: Int.t (** Maximum valid bit after size increase *);
}

(** Whether the allocated base has been obtained via calls to
    malloc/calloc/realloc ([Malloc]), alloca ([Alloca]), or is related to a
    variable-length array ([VLA]). *)
type deallocation = Malloc | VLA | Alloca

type base = private
  | Var of Cil_types.varinfo * validity
      (** Base for a standard C variable. *)
  | CLogic_Var of Cil_types.logic_var * Cil_types.typ * validity
      (** Base for a logic variable that has a C type. *)
  | Null (** Base for an address like [(int* )0x123] *)
  | String of int (** unique id of the constant string (one per code location)*)
    * cstring (** contents of the constant string *)
  | Allocated of Cil_types.varinfo * deallocation  * validity
      (** Base for a variable dynamically allocated via malloc/calloc/realloc/alloca *)

and validity =
  | Empty (** For 0-sized bases *)
  | Known of Int.t * Int.t (** Valid between those two bits *)
  | Unknown of Int.t * Int.t option * Int.t 
      (** Unknown(b,k,e) indicates:
          If k is [None], potentially valid between b and e
          If k is [Some k], then b <= k <= e, and the base is
           - valid between b and k;
  	   - potentially valid between k+1 and e:
          Accesses on potentially valid parts will succeed, but will also
          raise an alarm. *)
  | Variable of variable_validity
        (** Variable(min_alloc, max_alloc) means:
            - all offsets between [0] and [min_alloc] are valid; min_alloc can
              be -1, in which case no offsets are guaranteed to be valid.
            - offsets between [min_alloc+1] and [max_alloc] are potentially valid;
            - offsets above [max_alloc+1] are invalid.
        *)
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

module SetLattice: Lattice_type.Lattice_Set with module O = Hptset

module Validity: Datatype.S with type t = validity

(** [pretty_addr fmt base] pretty-prints the name of [base] on [fmt], with 
    a leading ampersand if it is a variable *)
val pretty_addr : Format.formatter -> t -> unit

val typeof : t -> Cil_types.typ option
(** Type of the memory block that starts from the given base. Useful to give to
    the function {!Bit_utils.pretty_bits}, typically. *)


(** {2 Validity} *)

val pretty_validity : Format.formatter -> validity -> unit
val validity : t -> validity

(** [validity_from_size size] returns [Empty] if [size] is zero,
    or [Known (0, size-1)] if [size > 0].
    [size] must not be negative.
   @since Aluminium-20160501 *)
val validity_from_size : Int.t -> validity
val validity_from_type : Cil_types.varinfo -> validity

type range_validity =
  | Invalid_range
  | Valid_range of Int_Intervals_sig.itv option

(** [valid_range v] returns [Invalid_range] if [v] is [Invalid],
    [Valid_range None] if [v] is [Empty], or [Valid_range (Some (mn, mx))]
    otherwise, where [mn] and [mx] are the minimum and maximum (possibly)
    valid bounds of [v]. *)
val valid_range: validity -> range_validity

(** [is_weak_validity v] returns true iff [v] is a [Weak] validity. *)
val is_weak_validity: validity -> bool

val create_variable_validity:
  weak:bool -> min_alloc:Int.t -> max_alloc:Int.t -> variable_validity

val update_variable_validity:
  variable_validity -> weak:bool -> min_alloc:Int.t -> max_alloc:Int.t -> unit
(** Update the corresponding fields of the variable validity. Bases
    already weak cannot be made 'strong' through this function, and the
    validity bounds can only grow. *)


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
val null_set: Hptset.t
(** Set containing only the base {!null}. *)

val min_valid_absolute_address: unit -> Int.t
val max_valid_absolute_address: unit -> Int.t
(** Bounds for option absolute-valid-range *)


(** {2 Size of a base} *)

val bits_sizeof : t -> Int_Base.t

val is_valid_offset : for_writing:bool -> Int.t -> t -> Ival.t -> bool
(** [is_valid_offset ~for_writing size b offset] checks that [offset]
    (expressed in bits) plus [size] bits is valid in [b]. *)


(** {2 Misc} *)

val is_read_only : t -> bool
(** Is the base valid as a read/write location, or only for reading.
    The [const] attribute is not currently taken into account. *)

val is_weak : t -> bool
(** Is the given base a weak one (in the sens that its validity is {!Weak}).
    Only possible for {!Allocated} bases. *)

val id : t -> int
val is_aligned_by : t -> Int.t -> bool


(** {2 Registering bases}
    This is only useful to create an initial memory state for analysis,
    and is never needed for normal users. *)

val register_allocated_var: Cil_types.varinfo -> deallocation -> validity -> t
  (** Allocated variables are variables not present in the source of the
      program, but instead created through dynamic allocation. Their field
      [vsource] is set to false. *)

val register_memory_var : Cil_types.varinfo -> validity -> t
  (** Memory variables are variables not present in the source of the program.
      They are created only to fill the contents of another variable.
      Their field [vsource] is set to false. *)

(*
Local Variables:
compile-command: "make -C ../../.."
End:
*)
