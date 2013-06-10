(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2013                                               *)
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
    with type z = Location_Bytes.z
    and type M.t = Location_Bytes.M.t

  include Lattice_With_Isotropy.S
      with type t := t
      and type widen_hint := widen_hint
      and module Top_Param := Base.SetLattice

  exception Not_based_on_null
  val project_ival : t -> Ival.t
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
  val mul: with_alarms:CilE.warn_mode -> t -> t -> t
  val div : with_alarms:CilE.warn_mode -> t -> t -> t
  val c_rem : with_alarms:CilE.warn_mode -> t -> t -> t
  val shift_right :
    with_alarms:CilE.warn_mode -> size:(bool*int) option -> t -> t -> t
  val shift_left :
    with_alarms:CilE.warn_mode -> size:(bool*int) option -> t -> t -> t
  val bitwise_and : signed:bool -> size:int -> t -> t -> t
  val bitwise_xor: with_alarms:CilE.warn_mode -> t -> t -> t
  val bitwise_or : size:int -> t -> t -> t

  val all_values : size:Int.t -> t -> bool
  val create_all_values :
    modu:Int.t -> signed:bool -> size:int -> t

  val has_sign_problems : t -> bool
end

(** Values with 'undefined' and 'escaping addresses' flags. *)
module V_Or_Uninitialized : sig
  type un_t =
    | C_uninit_esc of V.t
    | C_uninit_noesc of V.t
    | C_init_esc of V.t
    | C_init_noesc of V.t

  include Lattice_With_Isotropy.S
  with type t = un_t
  and  type widen_hint = Locations.Location_Bytes.widen_hint
	 
  val uninitialized : un_t
  val initialized : V.t -> un_t
  val change_initialized : bool -> un_t -> un_t
  val get_v : un_t -> V.t
  val get_flags : un_t -> int
  val unspecify_escaping_locals : 
    exact:bool -> (V.M.key -> bool) -> un_t -> Base.SetLattice.t * un_t
  val is_initialized : int -> bool
  val is_noesc : int -> bool
  val cardinal_zero_or_one_or_isotropic: t -> bool
 end

(** Memory slices. Tey are. maps from intervals to values with flags. All sizes
    and intervals are in bits. *)
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
    and type widen_hint_y = V_Or_Uninitialized.widen_hint

  val find_unspecified :
    with_alarms:CilE.warn_mode -> t -> location -> V_Or_Uninitialized.t
  val find :
    conflate_bottom:bool ->
    with_alarms:CilE.warn_mode -> t -> location -> V.t
  val find_and_reduce_indeterminate :
    with_alarms:CilE.warn_mode -> t -> location -> t * V.t

  val add_binding :
    with_alarms:CilE.warn_mode ->
    exact:bool -> t -> location -> V.t -> t
  val add_binding_unspecified :
    t -> location -> V_Or_Uninitialized.t -> t
  val add_binding_not_initialized : t -> location -> t

  val add_new_base :
    Base.t -> size:Int.t -> V.t -> size_v:Int.t -> t -> t

  val reduce_by_initialized_defined_loc :
    (V_Or_Uninitialized.t -> V_Or_Uninitialized.t) ->
    Locations.Location_Bits.t -> Int.t -> t -> t
  val reduce_previous_binding :
    with_alarms:CilE.warn_mode -> t -> location -> V.t -> t
  val reduce_binding :
    with_alarms:CilE.warn_mode -> t -> location -> V.t -> t

  val uninitialize_blocks_locals : Cil_types.block list -> t -> t
  val uninitialize_formals_locals : Cil_types.fundec -> t -> t
end

(*
Local Variables:
compile-command: "make -C ../.."
End:
*)
