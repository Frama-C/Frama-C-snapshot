(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2011                                               *)
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

(** Internal state representation of the value analysis. *)

open Locations
open Abstract_interp
open Abstract_value

exception Use_Main_Memory
  (** To be raised whenever we need to fall back to value analysis *)
type tt

module type Model_S = sig

  (** {3 Datatypes} *)

  include Datatype.S
  type widen_hint = Cvalue_type.Model.widen_hint
  type cluster

  (** {3 ...} *)

  val is_reachable : t -> bool
  val pretty_c_assert : Format.formatter -> t -> unit
  val pretty_without_null : Format.formatter -> t -> unit
  val pretty_filter :
    Format.formatter -> t -> Zone.t -> (Base.t -> bool) -> unit
  val join : t -> t -> t

  val find :
    conflate_bottom:bool ->
    with_alarms:CilE.warn_mode -> t -> location -> Location_Bytes.t

  val find_unspecified : with_alarms:CilE.warn_mode -> t -> location ->
    Cvalue_type.V_Or_Uninitialized.t

  val add_binding :
    with_alarms:CilE.warn_mode -> exact:bool -> t -> location ->
    Location_Bytes.t -> t
  val add_binding_unspecified : t -> location -> t

  val reduce_binding : t -> location -> Location_Bytes.t -> t
  val is_included : t -> t -> bool
  val is_included_actual_generic :
    Zone.t -> t -> t -> Location_Bytes.t Base.Map.t
  val widen : widen_hint -> t -> t -> (bool * t)
  val bottom : t
  val inject : Cvalue_type.Model.t -> t
  val empty_map : t
  val top : t
  val is_top: t -> bool
  val value_state : t -> Cvalue_type.Model.t

  val drop_relations : t -> t
  val filter_base : (Base.t -> bool) -> t -> t
  val remove_base : Base.t -> t -> t
  val clear_state_from_locals : Cil_types.fundec -> t -> t
  val uninitialize_locals: Cil_types.block list -> t -> t

  val compute_actual_final_from_generic :
    t -> t -> Zone.t -> Cvalue_type.Model.instanciation ->
    t * Location_Bits.Top_Param.t

  val is_included_by_location_enum :  t -> t -> Zone.t -> bool

  val find_mem : location -> Int_Base.t -> Ival.t -> t -> Cvalue_type.V.t
    (** computes the value of [*location] *)

  val add_mem :
    location -> Int_Base.t -> Ival.t -> t -> Cvalue_type.V.t ->
    cluster list * t
      (** add an information about the value of [*location] *)

  val propagate_change_from_real_to_virt :
    protected_clusters:cluster list -> location -> t -> Cvalue_type.V.t -> t
    (** clean up relation about [location] (call this function each time
	location has changed) *)

  val add_equality : ?offset:Ival.t -> t -> location -> location -> t
  val reduce_equality : t -> location -> location -> t
  val compute_diff : t -> location -> location -> Cvalue_type.V.t

  val shift_location : t -> location -> Ival.t -> Cvalue_type.V.t -> t

  val find_base : Base.t -> t -> Cvalue_type.V_Offsetmap.t
    (** @raise Not_found when the vid is not in the map *)

  val create_initial : base:Base.t ->
    v:Cvalue_type.V.t ->
    modu:Int.t ->
    state:t -> t
    (** Overwrites [base] in [state] with an initialized offsetmap filled
        with repetitions of the value [v] of size [modu]. *)

  (** {3 Copy / paste} *)

  val paste_offsetmap :
    Cvalue_type.V_Offsetmap.t -> Location_Bits.t -> Int.t -> Int.t -> t -> t
    (** @raise Lmap.Cannot_copy when copying is not possible. *)

  val copy_paste : location  -> location -> t -> t
    (** @raise Lmap.Cannot_copy when copying is not possible. *)

  val copy_from_virtual :
    location ->
    Ival.t ->
    Int.t -> t -> Cvalue_type.V_Offsetmap.t
    (** @raise Lmap.Cannot_copy when copying is not possible. *)

  val copy_offsetmap : with_alarms:CilE.warn_mode ->
    Locations.location -> t -> Cvalue_type.V_Offsetmap.t option

  val comp_prefixes: t -> t -> unit
  val find_prefix : t -> Hptmap.prefix -> Cvalue_type.Model.subtree option

end

module Model : Model_S with type t = Cvalue_type.Model.t * tt

(*
Local Variables:
compile-command: "make -C ../.."
End:
*)
