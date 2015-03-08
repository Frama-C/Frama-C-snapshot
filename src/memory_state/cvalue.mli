(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2015                                               *)
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
    and type z = Location_Bytes.z

  include module type of Offsetmap_lattice_with_isotropy
      with type t := t
      and type widen_hint := widen_hint

  val pretty_typ: Cil_types.typ option -> t Pretty_utils.formatter

  exception Not_based_on_null
  val project_ival : t -> Ival.t
  (** Raises [Not_based_on_null] if the value may be a pointer. *)

  val project_ival_bottom: t -> Ival.t
  (* Temporary API, will be merged with project_ival later *)

  val min_and_max_float : t -> Ival.F.t * Ival.F.t
    
  val is_imprecise : t -> bool
  val is_topint : t -> bool
  val is_bottom : t -> bool
  val is_isotropic : t -> bool

  val contains_zero : t -> bool
  val contains_non_zero : t -> bool

  val of_char : char -> t
  val of_int64: int64 -> t
  val subdiv_float_interval : size:int -> t -> t * t

  val compare_min_float : t -> t -> int
  val compare_max_float : t -> t -> int
  val compare_min_int : t -> t -> int
  val compare_max_int : t -> t -> int

  val filter_le_ge_lt_gt_int: Cil_types.binop -> t -> cond_expr:t -> t
  val filter_le_ge_lt_gt_float :
    Cil_types.binop ->
    bool -> Ival.Float_abstract.float_kind -> t -> cond_expr:t -> t

  val eval_comp: signed:bool -> Cil_types.binop -> t -> t -> t
  (** Can only be called on the 6 comparison operators *)

  val inject_int : Int.t -> t
  val interp_boolean : contains_zero:bool -> contains_non_zero:bool -> t

(** [cast ~size ~signed v] applies to the abstract value [v] the conversion 
    to the integer type described by [size] and [signed]. The results
    are [new_value, ok]. The boolean [ok], when true, indicates that no
    imprecision was introduced.
    Offsets of bases other than Null are not clipped. If they were clipped,
    they should be clipped at the validity of the base. The C standard does
    not say that [p+(1ULL<<32+1)] is the same as [p+1], it says that 
    [p+(1ULL<<32+1)] is invalid. *)
  val cast: size:Int.t -> signed:bool -> t -> t * bool

  val cast_float:
    rounding_mode:Ival.Float_abstract.rounding_mode -> t -> bool * bool * t
  val cast_double: t -> bool * bool * t
  val cast_float_to_int :
    signed:bool -> size:int -> t ->
    bool (** addresses *) *
    bool (** non-finite *) *
    (bool * bool) (** overflow, in both directions *) *
    t
  val cast_float_to_int_inverse :
    single_precision:bool -> t -> t
  val cast_int_to_float :
    Ival.Float_abstract.rounding_mode -> t -> t * bool

  val add_untyped : Int_Base.t -> t -> t -> t
  val add_untyped_under : Int_Base.t -> t -> t -> t

  val sub_untyped_pointwise: t -> t -> Ival.t * bool
  (** Substracts two pointers (assumed to have type [char*]) and returns the
      difference of their offsets. The two pointers are supposed to be pointing
      to the same base; the returned boolean indicates that this assumption
      might be incorrect. *)

  val mul: t -> t -> t
  val div: t -> t -> t
  val c_rem: t -> t -> t
  val shift_right: t -> t -> t
  val shift_left: t -> t -> t
  val bitwise_and: signed:bool -> size:int -> t -> t -> t
  val bitwise_xor: t -> t -> t
  val bitwise_or : t -> t -> t

  val all_values : size:Int.t -> t -> bool
  val create_all_values :
    modu:Int.t -> signed:bool -> size:int -> t
end

(** Values with 'undefined' and 'escaping addresses' flags. *)
module V_Or_Uninitialized : sig
  type un_t =
    | C_uninit_esc of V.t
    | C_uninit_noesc of V.t
    | C_init_esc of V.t
    | C_init_noesc of V.t

  include module type of Offsetmap_lattice_with_isotropy
    with type t = un_t
    and  type widen_hint = Locations.Location_Bytes.widen_hint
  include Lattice_type.With_Under_Approximation with type t:= t
  include Lattice_type.With_Narrow with type t := t

  val get_v : t -> V.t

  val is_bottom: t -> bool
  val is_initialized : t -> bool
  val is_noesc : t -> bool
  val is_indeterminate: t -> bool

  val uninitialized: t
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
  (** Remove 'unitialized' and 'escaping addresses' flags from the argument *)

  val unspecify_escaping_locals : 
    exact:bool -> (V.M.key -> bool) -> t -> Base.SetLattice.t * t

  val map: (V.t -> V.t) -> t -> t
 end

(** Memory slices. They are maps from intervals to values with
    flags. All sizes and intervals are in bits. *)
module V_Offsetmap:
  module type of Offsetmap_sig
  with type v = V_Or_Uninitialized.t
  and type widen_hint = V_Or_Uninitialized.widen_hint


(** Values bound by default to a variable. *)
module Default_offsetmap: sig
  val create_initialized_var :
    Cil_types.varinfo -> Base.validity -> V_Offsetmap.t -> Base.t
  val default_offsetmap : Base.t -> [ `Bottom | `Map of V_Offsetmap.t ]
end

(** Memories. They are maps from bases to memory slices *)
module Model: sig

  (** Functions inherited from [Lmap_sig] interface *)
  include module type of Lmap_sig
    with type v = V_Or_Uninitialized.t
    and type offsetmap = V_Offsetmap.t
    and type widen_hint_base = V_Or_Uninitialized.widen_hint

  (** {2 Finding values *} *)

  (** [find_unspecified ~conflate_bottom state loc] returns the value
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
      of padding bits. The default value is [true]. The function
      also returns [true] when the read location may be invalid.
  *)
  val find_unspecified :
    ?conflate_bottom:bool -> t -> location -> bool * V_Or_Uninitialized.t

  (** [find ?conflate_bottom state loc] returns the same value as
      [find_indeterminate], but removes the indeterminate flags from the
      result. The returned boolean indicates only a possibly invalid
      location, not indeterminateness. *)
  val find : ?conflate_bottom:bool -> t -> location -> bool * V.t

  (** {2 Writing values into the state} *)

  (** [add_binding state loc v] simulates the effect of writing [v] at location
      [loc] in [state]. If [loc] is not writable, {!bottom} is returned.
      The returned boolean indicates that the location may be invalid.
      For this function, [v] is an initialized value; the function
      {!add_binding_unspecified} allows to write a possibly unspecified
      value to [state]. *)
  val add_binding :
    exact:bool -> t -> location -> V.t -> bool * t

  val add_binding_unspecified :
    exact:bool -> t -> location -> V_Or_Uninitialized.t -> bool * t


  (** {2 Reducing the state} *)

  (** The functions below can be used to refine the value bound to a given
      location. In both cases, the location must be exact. *)

  (** [reduce_previous_binding state loc v] reduces the value associated to loc
      in state; use with caution, as the inclusion between the new and the
      old value is not checked.  *)
  val reduce_previous_binding : t -> location -> V.t -> t

  (** [reduce_binding state loc v] refines the value associated to
      [loc] in [state] according to [v], by keeping the values common
      to the existing value and [v]. *)
  val reduce_binding : t -> location -> V.t -> t


  (** {2 Creating an initial state} *)

  (** The functions below can be used to create an initial state to perform
      an analysis. In particular, they can write to read-only locations. *)

  val add_initial_binding: t -> location -> V_Or_Uninitialized.t -> t

  val add_new_base :
    Base.t -> size:Int.t -> V.t -> size_v:Int.t -> t -> t


  (** {2 Misc} *)

  val uninitialize_blocks_locals : Cil_types.block list -> t -> t
  val remove_variables : Cil_types.varinfo list -> t -> t
  (** For variables that are coming from the AST, this is equivalent to
      uninitializing them. *)

end

(*
Local Variables:
compile-command: "make -C ../.."
End:
*)
