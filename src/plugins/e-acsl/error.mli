(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2017                                               *)
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

(** Handling errors. *)

exception Typing_error of string
exception Not_yet of string

val untypable: string -> 'a
(** Type error built from the given argument. *)
  
val not_yet: string -> 'a
(** Not_yet_implemented error built from the given argument. *)
  
val handle: ('a -> 'a) -> 'a -> 'a
(** Run the closure with the given argument and handle potential errors. 
    Return the provide argument in case of errors. *)

val generic_handle: ('a -> 'b) -> 'b -> 'a -> 'b
(** Run the closure with the given argument and handle potential errors. 
    Return the additional argument in case of errors. *)

val nb_untypable: unit -> int
(** Number of untypable annotations. *)

val nb_not_yet: unit -> int
(** Number of not-yet-supported annotations. *)

(*
Local Variables:
compile-command: "make"
End:
*)
