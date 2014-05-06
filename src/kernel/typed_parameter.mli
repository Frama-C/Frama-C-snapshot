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

(** Parameter settable through a command line option. 
    This is a low level API, internaly used by the kernel. As a plug-in
    developer, you certainly prefer to use the API of {!Plugin} instead.
    @since Nitrogen-20111001 
    @plugin development guide *)

type ('a, 'b) gen_accessor = 
    { get: unit -> 'a; 
      set: 'a -> unit; 
      add_set_hook: ('b -> 'b -> unit) -> unit;
      add_update_hook: ('b -> 'b -> unit) -> unit }

type 'a accessor = ('a, 'a) gen_accessor

type typed_accessor =
  | Bool of bool accessor * string option (** the negative option, if any *)
  | Int of int accessor * (unit -> int * int) (** getting range *)
  | String of string accessor * (unit -> string list) (** possible values *)
  | String_set of (string, Datatype.String.Set.t) gen_accessor
  | String_list of (string, string list) gen_accessor

type parameter = private
    { name: string; (** Name of the option corresponding to the parameter. 
			It is exactly the state name of the option (see
			{!State.get_name}). *)
      help: string; (** Help message *)
      accessor: typed_accessor; (** How to get and set the value of the
				    parameter *)
      is_set: unit -> bool (** Is this option really set? *) }

include Datatype.S_with_collections with type t = parameter

val get: string -> t
(** Get the parameter from the option name. *)

val get_value: t -> string
(** Get the current value of the parameter, as a string. *)

(**/**)
(** Not for casual users. Use API of {!Plugin} instead. *)
val create: 
  name:string -> 
  help:string -> 
  accessor:typed_accessor ->
  is_set: (unit -> bool) ->
  t
(**/**)

(*
Local Variables:
compile-command: "make -C ../.."
End:
*)
