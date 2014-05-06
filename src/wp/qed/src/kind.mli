(**************************************************************************)
(*                                                                        *)
(*  This file is part of WP plug-in of Frama-C.                           *)
(*                                                                        *)
(*  Copyright (C) 2007-2014                                               *)
(*    CEA (Commissariat a l'energie atomique et aux energies              *)
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

(* -------------------------------------------------------------------------- *)
(* --- Sort and Types Tools                                               --- *)
(* -------------------------------------------------------------------------- *)

(** Logic Types Utilities *)

open Logic

val of_tau : ('f,'a) datatype -> sort
val of_poly : (int -> sort) -> ('f,'a) datatype -> sort
val image : sort -> sort

val degree_of_tau  : ('f,'a) datatype -> int
val degree_of_list : ('f,'a) datatype list -> int
val degree_of_sig  : ('f,'a) funtype -> int

val type_params : int -> ('f,'a) datatype list

val merge : sort -> sort -> sort
val merge_list : ('a -> sort) -> sort -> 'a list -> sort

val tmap : ('a,'f) datatype array -> ('a,'f) datatype -> ('a,'f) datatype

val basename : sort -> string
val pretty : Format.formatter -> sort -> unit

val pp_tau : 
  (Format.formatter -> int -> unit) ->
  (Format.formatter -> 'f -> unit) ->
  (Format.formatter -> 'a -> unit) ->
  Format.formatter -> ('f,'a) datatype -> unit

val pp_data :
  (Format.formatter -> 'a -> unit) -> 
  (Format.formatter -> 'b -> unit) -> 
  Format.formatter -> 'a -> 'b list -> unit

val pp_record:
  (Format.formatter -> 'f -> unit) -> 
  (Format.formatter -> 'b -> unit) -> 
  Format.formatter -> ?opened:bool -> ('f * 'b) list -> unit

val eq_tau :
  ('f -> 'f -> bool) ->
  ('a -> 'a -> bool) ->
  ('f,'a) datatype -> ('f,'a) datatype -> bool

val compare_tau:
  ('f -> 'f -> int) ->
  ('a -> 'a -> int) ->
  ('f,'a) datatype -> ('f,'a) datatype -> int

