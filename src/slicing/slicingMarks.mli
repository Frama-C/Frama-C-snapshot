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

open SlicingTypes

val bottom_mark : sl_mark
val mk_user_mark : data:bool -> addr:bool -> ctrl:bool -> sl_mark

(** generated [spare] = the smallest visible mark *)
val mk_gen_spare : sl_mark
val mk_user_spare : sl_mark

val is_bottom_mark : sl_mark -> bool
val is_top_mark : sl_mark -> bool
val is_spare_mark : sl_mark -> bool
val is_ctrl_mark : sl_mark -> bool
val is_addr_mark : sl_mark -> bool
val is_data_mark : sl_mark -> bool

val merge_marks : sl_mark list -> sl_mark
val inter_marks : sl_mark list -> sl_mark

(** [combine_marks] add a new information to the old value.
* @return (new_mark, is_new)
           where [is_new=true] if the new mark is not included in the old one.
*)
val combine_marks : sl_mark -> sl_mark -> (sl_mark * sl_mark)
val minus_marks : sl_mark -> sl_mark -> sl_mark

val compare_marks : sl_mark -> sl_mark -> int
val mark_to_string : sl_mark -> string
val pretty_mark : Format.formatter -> sl_mark -> unit

val missing_input_mark : call:sl_mark -> called:sl_mark -> sl_mark option
val missing_output_mark : call:sl_mark -> called:sl_mark -> sl_mark option


type sig_marks  = sl_mark PdgIndex.Signature.t

val empty_sig : sig_marks
val get_input_mark : sig_marks -> int -> sl_mark
val get_all_input_marks :
  sig_marks -> (PdgIndex.Signature.in_key * sl_mark) list
val get_matching_input_marks :
  sig_marks -> Locations.Zone.t -> (PdgIndex.Signature.in_key * sl_mark) list
val merge_inputs_m1_mark : sig_marks -> sl_mark
val get_input_loc_under_mark : sig_marks -> Locations.Zone.t -> sl_mark
val get_in_ctrl_mark : sig_marks -> sl_mark
val something_visible : sig_marks -> bool
val some_visible_out : sig_marks -> bool
val is_topin_visible : sig_marks -> bool
val get_marked_out_zone : sig_marks -> bool * Locations.Zone.t
val pretty_sig : Format.formatter -> sig_marks -> unit
