(**************************************************************************)
(*                                                                        *)
(*  This file is part of WP plug-in of Frama-C.                           *)
(*                                                                        *)
(*  Copyright (C) 2007-2016                                               *)
(*    CEA (Commissariat a l'energie atomique et aux energies              *)
(*         alternatives)                                                  *)
(*                                                                        *)
(*  All rights reserved.                                                  *)
(*  Contact CEA LIST for licensing.                                       *)
(**************************************************************************)

val name : string list -> string

(* ------------------------------------------------------------------------ *)
(* ---  Exception Handling in WP                                        --- *)
(* ------------------------------------------------------------------------ *)

open Cil_types

exception Error of string * string
(** To be raised a feature of C/ACSL cannot be supported by a memory model
    or is not implemented, or ... *)

val set_model : string -> unit

val unsupported : ?model:string -> ('a,Format.formatter,unit,'b) format4 -> 'a
val not_yet_implemented : ?model:string -> ('a,Format.formatter,unit,'b) format4 -> 'a

val pp_logic_label : Format.formatter -> logic_label -> unit

val pp_assigns :
  Format.formatter -> Cil_types.identified_term Cil_types.assigns -> unit

val pp_string_list : ?sep:Pretty_utils.sformat -> empty:string ->
  Format.formatter -> string list -> unit

type 'a cc =
  | Result of 'a
  | Warning of string * string (* source , reason *)

val protect : exn -> string * string (* source , reason *)

val protect_function : ('a -> 'b) -> 'a -> 'b cc
val protect_translation : ('a -> 'b -> 'r) -> 'a -> 'b -> 'r cc
val protect_translation3 : ('a -> 'b -> 'c -> 'r) -> 'a -> 'b -> 'c -> 'r cc
val protect_translation4 :
  ('a -> 'b -> 'c -> 'd -> 'r) -> 'a -> 'b -> 'c -> 'd -> 'r cc
val protect_translation5 :
  ('a -> 'b -> 'c -> 'd -> 'e -> 'r) -> 'a -> 'b -> 'c -> 'd -> 'e -> 'r cc

val protect_map : ('a -> 'b cc) -> 'a list -> 'b list cc
