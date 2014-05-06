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

  exception Not_based_on_null
  val project_ival : t -> Ival.t
  (** Raises [Not_based_on_null] if the value may be a pointer. *)

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

  val filter_le : t -> cond_expr:t -> t
  val filter_ge : t -> cond_expr:t -> t
  val filter_lt : t -> cond_expr:t -> t
  val filter_gt : t -> cond_expr:t -> t
  val filter_le_float :
    bool -> typ_loc:Cil_types.typ -> t -> cond_expr:t -> t
  val filter_ge_float :
    bool -> typ_loc:Cil_types.typ -> t -> cond_expr:t -> t
  val filter_lt_float :
    bool -> typ_loc:Cil_types.typ -> t -> cond_expr:t -> t
  val filter_gt_float :
    bool -> typ_loc:Cil_types.typ -> t -> cond_expr:t -> t

  val eval_comp: signed:bool -> Cil_types.binop -> t -> t -> t
  (** Can only be called on the 6 comparison operators *)

  val inject_int : Int.t -> t
  val interp_boolean : contains_zero:bool -> contains_non_zero:bool -> t

  val cast: size:Int.t -> signed:bool -> t -> t * bool
  val cast_float:
    rounding_mode:Ival.Float_abstract.rounding_mode -> t -> bool * bool * t
  val cast_double: t -> bool * bool * t
  val cast_float_to_int :
    signed:bool -> size:int -> t ->
    bool (** addresses *) * bool (** top *) * bool (** overflow *) * t
  val cast_float_to_int_inverse :
    single_precision:bool -> t -> t
  val cast_int_to_float :
    Ival.Float_abstract.rounding_mode -> t -> t * bool

  val add_untyped : Int_Base.t -> t -> t -> t
  val sub_untyped_pointwise: t -> t -> Ival.t * bool
  (** Substracts two pointers (assumed to have type [char*]) and returns the
      difference of their offsets. The two pointers are supposed to be pointing
      to the same base; the returned boolean indicates that this assumption
      might be incorrect. *)

  val mul: with_alarms:CilE.warn_mode -> t -> t -> t
  val div : with_alarms:CilE.warn_mode -> t -> t -> t
  val c_rem : with_alarms:CilE.warn_mode -> t -> t -> t
  val shift_right :
    with_alarms:CilE.warn_mode -> size:(bool*int) option -> t -> t -> t
  val shift_left :
    with_alarms:CilE.warn_mode -> size:(bool*int) option -> t -> t -> t
  val bitwise_and : signed:bool -> size:int -> t -> t -> t
  val bitwise_xor: with_alarms:CilE.warn_mode -> t -> t -> t
  val bitwise_or : with_alarms:CilE.warn_mode -> t -> t -> t

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

  val get_v : un_t -> V.t
  external get_flags : un_t -> int = "caml_obj_tag" "noalloc"

  val uninitialized: un_t

  val initialized : V.t -> un_t
  val change_initialized : bool -> un_t -> un_t

  val is_initialized : int -> bool
  val is_noesc : int -> bool

  val unspecify_escaping_locals : 
    exact:bool -> (V.M.key -> bool) -> un_t -> Base.SetLattice.t * un_t
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
  val default_offsetmap : Base.t -> V_Offsetmap.t
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
      You usually want to use [conflate_bottom=false], unless your goal
      is to test for the the fact that [loc] points to something undeterminate.
  *)
  val find_unspecified :
    with_alarms:CilE.warn_mode ->
    conflate_bottom:bool -> t -> location -> V_Or_Uninitialized.t

  (** [find ~with_alarms state loc] returns the same value as
      [find_indeterminate], but removes the flags from the result. If either
      the "unitialized" or "escaping" address flag was present, the
      corresponding alarm is raised by the function. *)
  val find :
    with_alarms:CilE.warn_mode ->
    conflate_bottom:bool -> t -> location -> V.t

  (** Similar to [find], but we expect a non-indeterminate result; if
      the value returned had escaping or uninitialized flags, they are
      removed in the state that is returned along with the cvalue. *)
  val find_and_reduce_indeterminate :
    with_alarms:CilE.warn_mode -> t -> location -> t * V.t

  (** {2 Writing values into the state} *)

  (** [add_binding state loc v] simulates the effect of writing [v] at location
      [loc] in [state]. If [loc] is not writable, {!bottom} is returned.

      For this function, [v] is an initialized value; the function
      {!add_binding_unspecified} allows to write a possibly unspecified
      value to [state]. *)
  val add_binding :
    with_alarms:CilE.warn_mode -> exact:bool -> t -> location -> V.t -> t

  val add_binding_unspecified :
    exact:bool -> t -> location -> V_Or_Uninitialized.t -> t


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

  val reduce_by_initialized_defined_loc :
    (V_Or_Uninitialized.t -> V_Or_Uninitialized.t) ->
    Locations.Location_Bits.t -> Int.t -> t -> t

  val uninitialize_blocks_locals : Cil_types.block list -> t -> t
  val uninitialize_formals_locals : Cil_types.fundec -> t -> t

end

(*
Local Variables:
compile-command: "make -C ../.."
End:
*)
