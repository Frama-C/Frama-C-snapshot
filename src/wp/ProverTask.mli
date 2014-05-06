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
(* --- Library for Running Provers                                        --- *)
(* -------------------------------------------------------------------------- *)

class printer : Format.formatter -> string ->
object
  method paragraph : unit
  method lines : unit
  method section : string -> unit
  method hline : unit
  method printf : 'a. ('a,Format.formatter,unit) format -> 'a
end

val pp_file : message:string -> file:string -> unit

(** never fails *)
class type pattern = 
object
  method get_after : ?offset:int -> int -> string
    (** [get_after ~offset:p k] returns the end of the message
	starting [p] characters after the end of group [k]. *)
  method get_string : int -> string
  method get_int : int -> int
  method get_float : int -> float
end

val p_group : string -> string (** Put pattern in group [\(p\)] *)
val p_int : string (** Int group pattern [\([0-9]+\)] *)
val p_float : string (** Float group pattern [\([0-9.]+\)] *)
val p_string : string (** String group pattern ["\(...\)"] *)
val p_until_space : string (** No space group pattern "\\([^ \t\n]*\\)" *)

val location : string -> int -> Lexing.position

type logs = [ `OUT | `ERR | `BOTH ]

class virtual command : string ->
object

  method set_command : string -> unit
  method add : string list -> unit
  method add_int : name:string -> value:int -> unit
  method add_positive : name:string -> value:int -> unit
  method add_float : name:string -> value:float -> unit
  method add_parameter : name:string -> (unit -> bool) -> unit
  method add_list : name:string -> string list -> unit
  method timeout : int -> unit
  method validate_time : (float -> unit) -> unit
  method validate_pattern : ?logs:logs -> ?repeat:bool -> 
    Str.regexp -> (pattern -> unit) -> unit
  method run : ?echo:bool -> ?logout:string -> ?logerr:string -> 
    unit -> int Task.task

end

val server : unit -> Task.server

val spawn : bool Task.task list -> unit
  (** Spawn all the tasks over the server and retain the first 'validated' one *)
