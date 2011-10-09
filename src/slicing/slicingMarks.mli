(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2011                                               *)
(*    CEA   (Commissariat à l'énergie atomique et aux énergies            *)
(*           alternatives)                                                *)
(*    INRIA (Institut National de Recherche en Informatique et en         *)
(*           Automatique)                                                 *)
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
(*  See the GNU Lesser General Public License version v2.1                *)
(*  for more details (enclosed in the file licenses/LGPLv2.1).            *)
(*                                                                        *)
(**************************************************************************)

type t_mark = SlicingTypes.sl_mark

val bottom_mark : t_mark
val mk_user_mark : data:bool -> addr:bool -> ctrl:bool -> t_mark

(** generated [spare] = the smallest visible mark *)
val mk_gen_spare : t_mark
val mk_user_spare : t_mark

val is_bottom_mark : t_mark -> bool
val is_top_mark : t_mark -> bool
val is_spare_mark : t_mark -> bool
val is_ctrl_mark : t_mark -> bool
val is_addr_mark : t_mark -> bool
val is_data_mark : t_mark -> bool

val merge_marks : t_mark list -> t_mark
val inter_marks : t_mark list -> t_mark

(** [combine_marks] add a new information to the old value.
* @return (new_mark, is_new)
           where [is_new=true] if the new mark is not included in the old one.
*)
val combine_marks : t_mark -> t_mark -> (t_mark * t_mark)
val minus_marks : t_mark -> t_mark -> t_mark

val compare_marks : t_mark -> t_mark -> int
val mark_to_string : t_mark -> string
val pretty_mark : Format.formatter -> t_mark -> unit

val missing_input_mark : call:t_mark -> called:t_mark -> t_mark option
val missing_output_mark : call:t_mark -> called:t_mark -> t_mark option

type t_sig_marks  = t_mark PdgIndex.Signature.t

val empty_sig : t_sig_marks
val get_input_mark : t_sig_marks -> int -> t_mark
val get_all_input_marks : t_sig_marks ->
                          (PdgIndex.Signature.t_in_key * t_mark) list
val merge_inputs_m1_mark : t_sig_marks -> t_mark
val get_input_loc_under_mark : t_sig_marks -> Locations.Zone.t -> t_mark
(*val same_output_visibility : t_sig_marks -> t_sig_marks -> bool*)
val get_in_ctrl_mark : t_sig_marks -> t_mark
val something_visible : t_sig_marks -> bool
val some_visible_out : t_sig_marks -> bool
val is_topin_visible : t_sig_marks -> bool
                                        (*
val check_output_marks : (int * t_mark) list -> t_sig_marks option ->
                                (int * t_mark) list * bool
val check_called_output_marks : t_sig_marks -> t_sig_marks option ->
                                (int * t_mark) list * bool
val check_input_marks : t_sig_marks ->
                        (PdgIndex.Signature.t_in_key * t_mark) list ->
                        (PdgIndex.Signature.t_in_key * t_mark) list * bool
val check_called_input_marks : t_sig_marks -> t_sig_marks option ->
                                (PdgIndex.Signature.t_in_key * t_mark) list * bool
                                *)
val get_marked_out_zone : t_sig_marks -> bool * Locations.Zone.t
val pretty_sig : Format.formatter -> t_sig_marks -> unit
