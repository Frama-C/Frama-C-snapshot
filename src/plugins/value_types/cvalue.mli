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

(** Representation of Value's abstract memory. *)

open Abstract_interp
open Locations

(** Estimation of the cardinal of the concretization of an abstract state
  or value. *)
module CardinalEstimate: sig
  type t
  val one: t
  val pretty: Format.formatter -> t -> unit
  val pretty_long_log10: Format.formatter -> t -> unit
end

(** Values. *)
module V : sig

  (** Values are essentially bytes-indexed locations, the NULL base
      representing basic integers or float. Operations that are not related to
      locations (ie that are not present in [Location_Bytes]) are  defined
      below. *)
  include module type of Location_Bytes
    (* Too many aliases, and OCaml module system is not able to keep track
       of all of them. Use some shortcuts *)
    with type M.t = Location_Bytes.M.t
    and type t = Location_Bytes.t
    and type generic_widen_hint = Location_Bytes.generic_widen_hint
    and type size_widen_hint = Location_Bytes.size_widen_hint

  include module type of Offsetmap_lattice_with_isotropy
      with type t := t
      and type generic_widen_hint := generic_widen_hint
      and type size_widen_hint := size_widen_hint
      and type widen_hint := widen_hint

  val pretty_typ: Cil_types.typ option -> t Pretty_utils.formatter

  (** Returns true if the value may not be a pointer. *)
  val is_arithmetic: t -> bool

  exception Not_based_on_null
  val project_ival : t -> Ival.t
  (** Raises [Not_based_on_null] if the value may be a pointer. *)

  val project_float : t -> Fval.t
  (** Raises [Not_based_on_null] if the value may be a pointer. *)

  val project_ival_bottom: t -> Ival.t
  (* Temporary API, will be merged with project_ival later *)
    
  val is_imprecise : t -> bool
  val is_topint : t -> bool
  val is_bottom : t -> bool
  val is_isotropic : t -> bool

  val contains_zero : t -> bool
  val contains_non_zero : t -> bool

  val of_char : char -> t
  val of_int64: int64 -> t

  val backward_mult_int_left: right:t -> result:t -> t option Bottom.or_bottom

  val backward_comp_int_left: Comp.t -> t -> t -> t
  val backward_comp_float_left_true: Comp.t -> Fval.kind -> t -> t -> t
  val backward_comp_float_left_false: Comp.t -> Fval.kind -> t -> t -> t

  val forward_comp_int: signed:bool -> Comp.t -> t -> t -> Comp.result

  val inject_comp_result: Comp.result -> t

  val inject_int : Int.t -> t
  val inject_float : Fval.t -> t
  val interp_boolean : contains_zero:bool -> contains_non_zero:bool -> t

(** [cast_int_to_int ~size ~signed v] applies to the abstract value [v] the
    conversion to the integer type described by [size] and [signed]. The results
    are [new_value, ok]. The boolean [ok], when true, indicates that the cast
    was the identity.
    Offsets of bases other than NULL are not clipped. If they were clipped,
    they should be clipped at the validity of the base. The C standard does
    not say that [p+(1ULL<<32+1)] is the same as [p+1], it says that 
    [p+(1ULL<<32+1)] is invalid. *)
  val cast_int_to_int: size:Int.t -> signed:bool -> t -> t * bool

  val reinterpret_as_float: Cil_types.fkind -> t -> t
  val reinterpret_as_int: signed:bool -> size:Integer.t -> t -> t
  val cast_float_to_float: Fval.kind -> t -> t
  val cast_float_to_int :
    signed:bool -> size:int -> t ->
    alarm (** non-finite *) *
    (alarm * alarm) (** overflow, in both directions *) *
    t
  val cast_float_to_int_inverse :
    single_precision:bool -> t -> t option
  val cast_int_to_float :
    Fval.kind -> t -> t
  val cast_int_to_float_inverse :
    single_precision:bool -> t -> t option

  val add_untyped : factor:Int_Base.t -> t -> t -> t
  (** [add_untyped ~factor e1 e2] computes [e1+factor*e2] using C semantic
      for +, i.e. [ptr+v] is [add_untyped ~factor:sizeof( *ptr ) ptr v]. (Thus,
      [factor] is in bytes.) This function handles simultaneously PlusA, MinusA,
      PlusPI, MinusPI and sometimes MinusPP, by setting [factor] accordingly.
      This is more precise than having multiple functions, as computations such
      as [(int)&t[1] - (int)&t[2]] would not be treated precisely otherwise. *)

  val add_untyped_under : factor:Int_Base.t -> t -> t -> t
  (** Under-approximating variant of {!add_untyped}. Takes two
      under-approximation, and returns an under-approximation.*)
 
  val sub_untyped_pointwise: ?factor:Int_Base.t -> t -> t -> Ival.t * bool
  (** See {!Locations.sub_pointwise}. In this module, [factor] is expressed in
      bytes. The two pointers are supposed to be pointing to the same base;
      the returned boolean indicates that this assumption might be incorrect. *)

  val mul: t -> t -> t
  val div: t -> t -> t
  val c_rem: t -> t -> t
  val shift_right: t -> t -> t
  val shift_left: t -> t -> t
  val bitwise_and: signed:bool -> size:int -> t -> t -> t
  val bitwise_xor: t -> t -> t
  val bitwise_or : t -> t -> t
  val bitwise_not: t -> t
  val bitwise_not_size: signed:bool -> size:int -> t -> t

  (** [all_values ~size v] returns true iff v contains all integer values
      representable in [size] bits. *)
  val all_values : size:Int.t -> t -> bool
  val create_all_values : signed:bool -> size:int -> t

  (** [cardinal_estimate v ~size] returns an estimation of the cardinal
      of [v], knowing that [v] fits in [size] bits. *)
  val cardinal_estimate: t -> size:Int.t -> Int.t
end

(** Values with 'undefined' and 'escaping addresses' flags. *)
module V_Or_Uninitialized : sig

  (** Semantics of the constructors:
      - [C_init_*]: definitely initialized
      - [C_uninit_*]: possibly uninitialized
      - [C_*_noesc]: never contains escaping addresses
      - [C_*_esc]: may contain escaping addresses

      - [C_uninit_noesc V.bottom]: guaranteed to be uninitialized
      - [C_init_esc V.bottom]: guaranteed to be an escaping address
      - [C_uninit_esc V.bottom]: either uninitialized or an escaping address

      - [C_init_noesc V.bottom]: "real" bottom, with an empty concretization.
         Corresponds to an unreachable state.  *)
  type t =
    | C_uninit_esc of V.t
    | C_uninit_noesc of V.t
    | C_init_esc of V.t
    | C_init_noesc of V.t

  include module type of Offsetmap_lattice_with_isotropy
    with type t := t
    and  type size_widen_hint = Location_Bytes.size_widen_hint
    and  type generic_widen_hint = Location_Bytes.generic_widen_hint
    and  type widen_hint = Locations.Location_Bytes.widen_hint
  include Lattice_type.With_Under_Approximation with type t:= t
  include Lattice_type.With_Narrow with type t := t
  include Lattice_type.With_Top with type t := t
  include Lattice_type.With_Top_Opt with type t := t

  val get_v : t -> V.t
  val make : initialized: bool -> escaping: bool -> V.t -> t

  val is_bottom: t -> bool

  (** [is_initialized v = true] implies [v] is definitely initialized.
      [is_initialized v = false] implies [v] is possibly uninitialized.
      [is_initialized v = false && is_bottom v] implies [v] is definitely
      uninitialized. *)
  val is_initialized : t -> bool

  (** [is_noesc v = true] implies [v] has no escaping addresses.
      [is_noesc v = false] implies [v] may have escaping addresses. *)
  val is_noesc : t -> bool

  (** [is_indeterminate v = false] implies [v] only has definitely initialized
                                   values and non-escaping addresses.
      [is_indeterminate v = true] implies [v] may have uninitialized values
                                  and/or escaping addresses. *)
  val is_indeterminate: t -> bool

  (** Returns the canonical representant of a definitely uninitialized value. *)
  val uninitialized: t

  (** [initialized v] returns the definitely initialized, non-escaping
      representant of [v]. *)
  val initialized : V.t -> t

  val reduce_by_initializedness : bool -> t -> t
  (** [reduce_by_initializedness initialized v] reduces [v] so that its result
     [r] verifies [\initialized(r)] if [initialized] is [true], and
     [!\initialized(r)] otherwise. *)

  val reduce_by_danglingness : bool -> t -> t
  (** [reduce_by_danglingness dangling v] reduces [v] so that its result [r]
     verifies [\dangling(r)] if [dangling] is [true], and
     [!\dangling(r)] otherwise. *)

  val remove_indeterminateness: t -> t
  (** Remove 'uninitialized' and 'escaping addresses' flags from the argument *)

  val unspecify_escaping_locals : 
    exact:bool -> (V.M.key -> bool) -> t -> bool * t

  val map: (V.t -> V.t) -> t -> t
  val map2: (V.t -> V.t -> V.t) -> t -> t -> t
  (** initialized/escaping information is the join of the information
      on each argument. *)
 end

(** Memory slices. They are maps from intervals to values with
    flags. All sizes and intervals are in bits. *)
module V_Offsetmap: sig
  include module type of Offsetmap_sig
  with type v = V_Or_Uninitialized.t
  and type widen_hint = V_Or_Uninitialized.generic_widen_hint

  val narrow: t -> t -> t Bottom.Type.or_bottom
  val narrow_reinterpret: t -> t -> t Bottom.Type.or_bottom
  (** See the corresponding functions in {!Offsetmap_sig}. *)
end


(** Values bound by default to a variable. *)
module Default_offsetmap: sig
  val default_offsetmap : Base.t -> V_Offsetmap.t Bottom.or_bottom
end

(** Memories. They are maps from bases to memory slices *)
module Model: sig

  (** Functions inherited from [Lmap_sig] interface *)
  include module type of Lmap_sig
    with type v = V_Or_Uninitialized.t
    and type offsetmap = V_Offsetmap.t
    and type widen_hint_base = V_Or_Uninitialized.generic_widen_hint

  include Lattice_type.With_Narrow with type t := t

  (** {2 Finding values *} *)

  (** [find_indeterminate ~conflate_bottom state loc] returns the value
      and flags associated to [loc] in [state]. The flags are the union
      of the flags at all the locations and offsets corresponding to [loc].
      The value is the join of all the values pointed by [l..l+loc.size-1]
      for all [l] among the locations in [loc]. For an individual [l],
      the value pointed to is determined as such:
      - if no part of [l..l+loc.size-1] is [V.bottom], the value is the most
        precise value of [V] approximating the sequence of bits present at
        [l..l+loc.size-1]
      - if [l..l+loc.size-1] points to [V.bottom] everywhere, the value
        is [bottom].
      - if [conflate_bottom] is [true] and at least one bit pointed to by
        [l..l+loc.size-1] is [V.bottom], the value is [V.bottom]
      - if [conflate_bottom] is [false] and at least one bit pointed to by
        [l..l+loc.size-1] is not [V.bottom], the value is an approximation
        of the join of all the bits at [l..l+loc.size-1].
      As a rule of thumb, you must set [conflate_bottom=true] when the
      operation you abstract really accesses [loc.size] bits, and when
      undeterminate values are an error. This is typically the case when
      reading a scalar value. Conversely, if you are reading many bits at
      once (for example, to approximate the entire contents of a struct),
      set [conflate_bottom] to [false] -- to account for the possibility
      of padding bits. The default value is [true].
  *)
  val find_indeterminate :
    ?conflate_bottom:bool -> t -> location -> V_Or_Uninitialized.t

  (** [find ?conflate_bottom state loc] returns the same value as
      [find_indeterminate], but removes the indeterminate flags from the
      result. *)
  val find : ?conflate_bottom:bool -> t -> location -> V.t

  (** {2 Writing values into the state} *)

  (** [add_binding state loc v] simulates the effect of writing [v] at location
      [loc] in [state]. If [loc] is not writable, {!bottom} is returned.
      For this function, [v] is an initialized value; the function
      {!add_indeterminate_binding} allows to write a possibly indeterminate
      value to [state]. *)
  val add_binding :
    exact:bool -> t -> location -> V.t -> t
  val add_indeterminate_binding :
    exact:bool -> t -> location -> V_Or_Uninitialized.t -> t


  (** {2 Reducing the state} *)

  (** The functions below can be used to refine the value bound to a given
      location. In both cases, the location must be exact. *)

  (** [reduce_previous_binding state loc v] reduces the value associated to loc
      in state; use with caution, as the inclusion between the new and the
      old value is not checked.  *)
  val reduce_previous_binding : t -> location -> V.t -> t

  (** Same behavior as [reduce_previous_binding], but takes a value
      with 'undefined' and 'escaping addresses' flags. *)
  val reduce_indeterminate_binding: t -> location -> V_Or_Uninitialized.t -> t


  (** {2 Misc} *)

  val uninitialize_blocks_locals : Cil_types.block list -> t -> t
  val remove_variables : Cil_types.varinfo list -> t -> t
  (** For variables that are coming from the AST, this is equivalent to
      uninitializing them. *)

  val cardinal_estimate: t -> CardinalEstimate.t

end

(*
Local Variables:
compile-command: "make -C ../../.."
End:
*)
