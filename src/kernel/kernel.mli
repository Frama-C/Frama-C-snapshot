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

(** Provided services for kernel developers. *)

include Plugin.S

(** Each parameter of functors used to registered a new kernel parameter must
    have a module name. *)

module type Parameter_input = sig
  include Plugin.Parameter_input
  val module_name: string
end

module type Parameter_input_with_arg = sig
  include Plugin.Parameter_input_with_arg
  val module_name: string
end

module type COMPLEX_VALUE = sig
  include Plugin.COMPLEX_VALUE
  val module_name: string
end

module Bool
  (X:sig 
     include Parameter_input 
     val default: bool
       (** The default value of the parameter. So giving the option
	   [option_name] to Frama-C, change the value of the parameter to
	   [not default]. *)
   end) : Plugin.BOOL

(** Build a boolean option initialized to [false].
    @plugin development guide *)
module False(X: Parameter_input) : Plugin.BOOL

(** Build a boolean option initialized to [true].
    @plugin development guide *)
module True(X: Parameter_input) : Plugin.BOOL

(** Build an integer option.
    @plugin development guide *)
module Int
  (X: sig val default: int include Parameter_input_with_arg end) : Plugin.INT

(** Build an integer option initialized to [0].
    @plugin development guide *)
module Zero(X:Parameter_input_with_arg) : Plugin.INT

(** Build a string option.
    @plugin development guide *)
module String
  (X: sig include Parameter_input_with_arg val default: string end) : 
  Plugin.STRING

(** Build a string option initialized to [""].
    @plugin development guide *)
module EmptyString(X: Parameter_input_with_arg) : Plugin.STRING

(** Build an option as a set of strings, initialized to the empty set. *)
module StringSet(X: Parameter_input_with_arg) : Plugin.STRING_SET

(** Should not be used by casual users *)
module StringList(X: Parameter_input_with_arg) : Plugin.STRING_LIST

(** @plugin development guide *)
module IndexedVal (V:COMPLEX_VALUE) : Plugin.INDEXED_VAL with type value = V.t

(*
Local Variables:
compile-command: "LC_ALL=C make -C ../.."
End:
*)
