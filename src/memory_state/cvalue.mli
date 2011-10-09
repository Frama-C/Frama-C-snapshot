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

(** Undocumented. 
    Do not use this module if you don't know what you are doing. *)

(* [JS 2011/10/03] To the authors/users of this module: please document it. *)

open Abstract_interp
open Locations

module V :
  sig
    module M :
      sig
        type key = Base.t
        type leaf_annot = Location_Bytes.M.leaf_annot
        type branch_annot = Location_Bytes.M.branch_annot
        type tt = Location_Bytes.M.tt = private
          | Empty
          | Leaf of key * Ival.t * leaf_annot
          | Branch of int * int * tt * tt * branch_annot
        type t = tt
        val iter : (Base.t -> Ival.t -> unit) -> t -> unit
        val find : key -> t -> Ival.t
        val fold : (Base.t -> Ival.t -> 'a -> 'a) -> t -> 'a -> 'a
      end
    module Top_Param : Lattice_Set
      with type O.elt = Base.t
      and type O.t = Location_Bytes.Top_Param.O.t
    type z =
      Location_Bytes.z =
      | Top of Location_Bytes.Top_Param.t * Origin.t
      | Map of M.t
    exception Error_Top
    exception Error_Bottom

    include Lattice
      with type t = z
      and type widen_hint =
             Location_Bytes.Top_Param.widen_hint * (Base.t -> Ival.widen_hint)
    val top_float : t
    val top_single_precision_float : t
    val is_zero : t -> bool
    val hash : t -> int
    val zero_or_one : t
    val singleton_zero : t
    val singleton_one : t
    val topify_arith_origin : t -> t
    val topify_misaligned_read_origin : t -> t
    val topify_merge_origin : t -> t
    val under_topify : t -> t
    val top_int : t
    val find_or_bottom : Base.t -> M.t -> Ival.t
    val add_or_bottom : Base.t -> Ival.t -> M.t -> M.t
    val inject : Base.t -> Ival.t -> t
    val inject_ival : Ival.t -> t
    val inject_top_origin : Origin.t -> Location_Bytes.Top_Param.O.t -> t
    val fold_enum : split_non_enumerable:int -> (t -> 'a -> 'a) -> t -> 'a -> 'a
    val splitting_cardinal_less_than :
      split_non_enumerable:int -> t -> int -> int
    val cardinal_zero_or_one : t -> bool
    val cardinal_less_than : t -> int -> int
    val find_exclusive : Base.t -> t -> Ival.t
    val split : Base.t -> t -> Ival.t * t
    exception Not_all_keys
    val get_keys_exclusive : Ival.t -> t -> Base.t list
    val find_lonely_binding : t -> Base.t * Ival.t
    val find_lonely_key : t -> Base.t * Ival.t
    val diff : t -> t -> t
    val diff_if_one : t -> t -> t
    val location_shift : Ival.t -> t -> t
    val fold_i : (Base.t -> Ival.t -> 'a -> 'a) -> t -> 'a -> 'a
    val fold_bases : (Base.t -> 'a -> 'a) -> t -> 'a -> 'a
    val top_leaf_origin : unit -> t
    val topify_with_origin : Origin.t -> t -> t
    val may_reach : Base.t -> t -> bool
    val cached_fold :
      cache:string * int ->
      temporary:bool ->
      f:(Base.t -> Ival.t -> 'a) ->
      projection:(Base.t -> Ival.t) ->
      joiner:('a -> 'a -> 'a) -> empty:'a -> t -> 'a
    val contains_addresses_of_locals : (M.key -> bool) -> t -> bool
    val remove_escaping_locals :
      (M.key -> bool) -> t -> Location_Bytes.Top_Param.t * t
    val contains_addresses_of_any_locals : t -> bool
    val iter_on_strings :
      skip:Base.t option ->
      (Base.t -> string -> int -> int -> unit) -> t -> unit
    exception Not_based_on_null
    val project_ival : t -> Ival.t
    val types :
      (int,
       (t * string *
        (Format.formatter -> (unit -> unit) -> string -> string -> t -> unit))
       list)
      Hashtbl.t
    val pretty_int_range :
      Format.formatter -> (unit -> 'a) -> string -> string -> t -> unit
    val pretty_float_range :
      Format.formatter -> (unit -> 'a) -> string -> string -> t -> unit
    val pretty_c_assert :
      (unit -> unit) -> string -> int -> Format.formatter -> t -> unit
    val force_float : Cil_types.fkind -> t -> bool * t
    val is_imprecise : z -> bool
    val is_topint : t -> bool
    val is_bottom : t -> bool
    val is_isotropic : z -> bool
    val contains_zero : Location_Bytes.z -> bool
    val contains_non_zero : t -> bool
    val of_char : char -> t
    val subdiv_float_interval : size:int -> t -> t * t
    val compare_bound : (Ival.t -> Ival.t -> 'a) -> t -> t -> 'a
    val compare_min_float : t -> t -> int
    val compare_max_float : t -> t -> int
    val compare_min_int : t -> t -> int
    val compare_max_int : t -> t -> int
    val filter_comparison :
      (Ival.t -> Ival.t -> Ival.t) -> z -> cond_expr:t -> z
    val filter_comparison_float :
      (Ival.t -> Ival.t -> Ival.t) -> t -> cond_expr:t -> t
    val filter_le : z -> cond_expr:t -> z
    val filter_ge : z -> cond_expr:t -> z
    val filter_lt : z -> cond_expr:t -> z
    val filter_gt : z -> cond_expr:t -> z
    val filter_le_float :
      bool -> typ_loc:Cil_types.typ -> t -> cond_expr:t -> t
    val filter_ge_float :
      bool -> typ_loc:Cil_types.typ -> t -> cond_expr:t -> t
    val filter_lt_float :
      bool -> typ_loc:Cil_types.typ -> t -> cond_expr:t -> t
    val filter_gt_float :
      bool -> typ_loc:Cil_types.typ -> t -> cond_expr:t -> t
    val pretty : Format.formatter -> z -> unit
    val find_exact_base_without_offset : z -> Base.t list * bool
    val inject_int : Int.t -> t
    val interp_boolean : contains_zero:bool -> contains_non_zero:bool -> t
    val add : t -> Location_Bytes.t -> Location_Bytes.t
    val add_untyped :
      Int_Base.t ->
      Location_Bytes.t -> Location_Bytes.t -> t
    val check_equal : bool -> t -> t -> t
    val compare_min_max :
      Int.t option -> Int.t option -> int
    val compare_max_min :
      Int.t option -> Int.t option -> int
    val do_le: Int.t option -> Int.t option -> Int.t option -> Int.t option -> t
    val do_ge: Int.t option -> Int.t option -> Int.t option -> Int.t option -> t
    val do_lt: Int.t option -> Int.t option -> Int.t option -> Int.t option -> t
    val do_gt: Int.t option -> Int.t option -> Int.t option -> Int.t option -> t
    val comparisons:
      string ->
      signed:bool ->
      (Int.t option -> Int.t option -> Int.t option -> Int.t option -> t) ->
      t -> t -> t
    val cast_float : t -> bool * bool * t
    val cast :
      with_alarms:CilE.warn_mode ->
      size:Int.t -> signed:bool -> t -> t
    val import_function :
      topify_arith_origin:(t -> t) ->
      with_alarms:CilE.warn_mode ->
      string -> (Ival.t -> Ival.t -> Ival.t) -> t -> t -> t
    val arithmetic_function :
      with_alarms:CilE.warn_mode ->
      string -> (Ival.t -> Ival.t -> Ival.t) -> t -> t -> t
    val unary_arithmetic_function :
      with_alarms:CilE.warn_mode -> string -> (Ival.t -> Ival.t) -> t -> t
    val cast_float_to_int : signed:bool -> size:int -> t -> bool * bool * t
    val cast_int_to_float :
      with_alarms:CilE.warn_mode ->
      Ival.Float_abstract.rounding_mode -> t -> t
    val div : with_alarms:CilE.warn_mode -> t -> t -> t
    val c_rem : with_alarms:CilE.warn_mode -> t -> t -> t
    val oper_on_values :
      with_alarms:CilE.warn_mode ->
      string ->
      (Int.t ->
       Int.t -> Int.t ) ->
      t -> t -> t 
    val shift_right :
      with_alarms:CilE.warn_mode -> size:int option -> t -> z -> t
    val bitwise_and : signed:bool -> size:int -> t -> t -> t
    val extract_bits :
      start:Int.t ->
      stop:Int.t -> t -> bool * t
    val big_endian_merge_bits :
      conflate_bottom:bool ->
      total_length:int ->
      length:My_bigint.t -> value:t -> offset:My_bigint.t -> t -> t
    val little_endian_merge_bits :
      conflate_bottom:bool ->
      total_length:int -> value:t -> offset:Int.t -> t -> t
    val all_values : size:Int.t -> t -> bool
    val anisotropic_cast : size:Int.t -> t -> t
    val create_all_values :
      modu:Int.t -> signed:bool -> size:int -> t
    val bitwise_or : size:int -> t -> t -> t
    val shift_left :
      with_alarms:CilE.warn_mode -> size:int option -> z -> z -> t
    val has_sign_problems : t -> bool
  end


module V_Or_Uninitialized :
  sig
    type un_t =
        C_uninit_esc of V.t
      | C_uninit_noesc of V.t
      | C_init_esc of V.t
      | C_init_noesc of V.t
    include Lattice_With_Isotropy.S with type t = un_t
				    and
    type widen_hint = Locations.Location_Bytes.widen_hint

    val initialized : V.t -> un_t
    val change_initialized : bool -> un_t -> un_t
    val get_v : un_t -> V.t
    val get_flags : un_t -> int
    val unspecify_escaping_locals : 
      (V.M.key -> bool) -> un_t -> Location_Bytes.Top_Param.t * un_t
    val is_initialized : int -> bool
    val is_noesc : int -> bool
 end

module V_Offsetmap:
  Offsetmap.S with type y = V_Or_Uninitialized.t
              and type widen_hint = V_Or_Uninitialized.widen_hint
              and type t = Offsetmap.Make(V_Or_Uninitialized).t

module V_Offsetmap_ext:
  Offsetmap.S with type y = V_Or_Uninitialized.t
              and type widen_hint = V_Or_Uninitialized.widen_hint
              and type t = Offsetmap.Make(V_Or_Uninitialized).t

(*
module Partial_lmap : Lmap.Location_map
  with type y = V_Or_Uninitialized.t
  and type widen_hint_offsetmap = V_Or_Uninitialized.widen_hint
  and type loffset = V_Offsetmap_ext.t
  and module Make = Lmap.Make_LOffset(V_Or_Uninitialized)(V_Offsetmap_ext).Make
*)

module Default_offsetmap :
  sig
    val initialized_var_table : V_Offsetmap.t Cil_datatype.Varinfo.Hashtbl.t
    val create_initialized_var :
      Cil_datatype.Varinfo.Hashtbl.key ->
      Base.validity -> V_Offsetmap.t -> Base.t
    val default_offsetmap : Base.t -> V_Offsetmap.t
  end

module Model :
  sig
    module LBase :
      sig
        type t (* = Lmap.Make_LOffset(V_Or_Uninitialized)(V_Offsetmap_ext).Make(Default_offsetmap).LBase.t *)
        val iter : (Base.base -> V_Offsetmap_ext.t -> unit) -> t -> unit
      end
    type tt = (* Partial_lmap.Make(Default_offsetmap).tt = *) private
      | Bottom
      | Top
      | Map of LBase.t
    include Datatype.S with type t = tt
    type widen_hint =
        bool * Base.Set.t * (Base.t -> V_Or_Uninitialized.widen_hint)
    val inject : Base.t -> V_Offsetmap_ext.t -> t
    val add_offsetmap : Base.t -> V_Offsetmap_ext.t -> t -> t
    val pretty_without_null : Format.formatter -> t -> unit
    val pretty_filter :
      Format.formatter -> t -> Zone.t -> (Base.t -> bool) -> unit
    val is_included : t -> t -> bool
    val top : t
    val is_top : t -> bool
    val empty_map : t
    val is_empty_map : t -> bool
    val bottom : t
    val is_reachable : t -> bool
    val widen : widen_hint -> t -> t -> bool * t
    val filter_base : (Base.t -> bool) -> t -> t
    val find_base : Base.t -> t -> V_Offsetmap_ext.t
    val remove_base : Base.t -> t -> t
    val copy_paste :
      with_alarms:CilE.warn_mode -> location -> location -> t -> t
    val paste_offsetmap :
      with_alarms:CilE.warn_mode ->      
      from:V_Offsetmap_ext.t ->
      dst_loc:Location_Bits.t ->
      start:Int.t ->
      size:Int.t -> exact:bool -> t -> t
    val copy_offsetmap :
      with_alarms:CilE.warn_mode ->
      location -> t -> V_Offsetmap_ext.t option
    val is_included_by_location_enum : t -> t -> Zone.t -> bool
    val fold :
      size:Int.t ->
      (location -> V_Or_Uninitialized.t -> 'a -> 'a) -> t -> 'a -> 'a
    val fold_single_bindings :
      size:Int.t ->
      (location -> V_Or_Uninitialized.t -> 'a -> 'a) -> t -> 'a -> 'a
    val fold_base : (Base.t -> 'a -> 'a) -> t -> 'a -> 'a
    val fold_base_offsetmap :
      (Base.t -> V_Offsetmap_ext.t -> 'a -> 'a) -> t -> 'a -> 'a
    val find_offsetmap_for_location :
      Location_Bits.t -> t -> V_Offsetmap_ext.t
    val add_whole : location -> V_Or_Uninitialized.t -> t -> t
    val remove_whole : location -> t -> t
    val comp_prefixes : t -> t -> unit
    type subtree (* =
        Lmap.Make_LOffset(V_Or_Uninitialized)(V_Offsetmap_ext).Make(Default_offsetmap).subtree *)
    val find_prefix : t -> Hptmap.prefix -> subtree option
    val hash_subtree : subtree -> int
    val equal_subtree : subtree -> subtree -> bool
    val reciprocal_image :
      Base.t -> t -> Zone.t * Location_Bits.t
    exception Error_Bottom
    val cached_fold :
      f:(Base.t -> V_Offsetmap_ext.t -> 'a) ->
      cache:string * int ->
      temporary:bool -> joiner:('a -> 'a -> 'a) -> empty:'a -> t -> 'a
    val cached_map :
      f:(Base.t -> V_Offsetmap_ext.t -> V_Offsetmap_ext.t) ->
      cache:string * int -> temporary:bool -> t -> t
    exception Found_prefix of Hptmap.prefix * subtree * subtree
    type y = V.t
    val join : t -> t -> t
    val reduce_equality : t -> location -> location -> t
    val pretty_c_assert : Format.formatter -> t -> unit
    val find_unspecified :
      with_alarms:CilE.warn_mode -> t -> location -> V_Or_Uninitialized.t
    val find :
      conflate_bottom:bool ->
      with_alarms:CilE.warn_mode -> t -> location -> V.t
    val has_been_initialized : Base.t -> t -> bool
    val add_binding_not_initialized : t -> location -> t
    val add_binding_unspecified :
      t -> location -> V_Or_Uninitialized.t -> t
    val add_binding :
      with_alarms:CilE.warn_mode ->
      exact:bool -> t -> location -> V.t -> t
    val reduce_binding :
      with_alarms:CilE.warn_mode -> t -> location -> V.t -> t
    val create_initial :
      base:Base.t -> v:V.t -> modu:Int.t -> state:t -> t
    val uninitialize_locals : Cil_types.block list -> t -> t
    val clear_state_from_locals : Cil_types.fundec -> t -> t
  end

(*
Local Variables:
compile-command: "make -C ../.."
End:
*)
