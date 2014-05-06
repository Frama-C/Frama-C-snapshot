(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2014                                               *)
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
(*  See the GNU Lesser General Public License version 2.1                 *)
(*  for more details (enclosed in the file licenses/LGPLv2.1).            *)
(*                                                                        *)
(**************************************************************************)

(** {1 Global Logic Environment} *)

open Cil_types

(** {2 Global Tables} *)
module Logic_info: State_builder.Hashtbl
  with type key = string and type data = Cil_types.logic_info

module Logic_type_info: State_builder.Hashtbl
  with type key = string and type data = Cil_types.logic_type_info

module Logic_ctor_info: State_builder.Hashtbl
  with type key = string and type data = Cil_types.logic_ctor_info

(** @since Oxygen-20120901 *)
module Model_info: State_builder.Hashtbl
  with type key = string and type data = Cil_types.model_info

(** @since Oxygen-20120901 *)
module Lemmas: State_builder.Hashtbl 
  with type key = string and type data = Cil_types.global_annotation

val builtin_states: State.t list

(** {2 Shortcuts to the functions of the modules above} *)

(** Prepare all internal tables before their uses:
    clear all tables except builtins. *)
val prepare_tables : unit -> unit

(** {3 Add an user-defined object} *)

(** add_logic_function_gen takes as argument a function eq_logic_info
    which decides whether two logic_info are identical. It is intended
    to be Logic_utils.is_same_logic_profile, but this one can not be
    called from here since it will cause a circular dependency
    Logic_env <- Logic_utils <- Cil <- Logic_env.
    {b Do not use this function directly} unless you're really sure about
    what you're doing. Use {!Logic_utils.add_logic_function} instead.
*)
val add_logic_function_gen:
  (logic_info -> logic_info -> bool) -> logic_info -> unit
val add_logic_type: string -> logic_type_info -> unit
val add_logic_ctor: string -> logic_ctor_info -> unit

(**
   @since Oxygen-20120901 
*)
val add_model_field: model_info -> unit

(** {3 Add a builtin object} *)

module Builtins: sig
  val apply: unit -> unit
    (** adds all requested objects in the environment. *)
  val extend: (unit -> unit) -> unit
    (** request an addition in the environment. Use one of the functions below
        in the body of the argument.
     *)
end

(** logic function/predicates that are effectively used in current project. *)
module Logic_builtin_used: sig
  val add: logic_info -> unit
  val mem: logic_info -> bool
  val iter: (logic_info -> unit) -> unit
  val self: State.t
end

(** see add_logic_function_gen above *)
val add_builtin_logic_function_gen:
  (builtin_logic_info -> builtin_logic_info -> bool) ->
  builtin_logic_info -> unit
val add_builtin_logic_type: string -> logic_type_info -> unit
val add_builtin_logic_ctor: string -> logic_ctor_info -> unit

val is_builtin_logic_function: string -> bool
val is_builtin_logic_type: string -> bool
val is_builtin_logic_ctor: string -> bool

val iter_builtin_logic_function: (builtin_logic_info -> unit) -> unit
val iter_builtin_logic_type: (logic_type_info -> unit) -> unit
val iter_builtin_logic_ctor: (logic_ctor_info -> unit) -> unit

(** {3 searching the environment} *)

val find_all_logic_functions : string -> logic_info list

(** returns all model fields of the same name.
   @since Oxygen-20120901 
*)
val find_all_model_fields: string -> model_info list

(** [find_model_info field typ] returns the model field associated to [field]
    in type [typ].
    @raise Not_found if no such type exists.
    @since Oxygen-20120901 
*)
val find_model_field: string -> typ -> model_info

(** cons is a logic function with no argument. It is used as a variable,
    but may occasionally need to find associated logic_info.
    @raise Not_found if the given varinfo is not associated to a global logic
    constant.
 *)
val find_logic_cons: logic_var -> logic_info
val find_logic_type: string -> logic_type_info
val find_logic_ctor: string -> logic_ctor_info

(** {3 tests of existence} *)
val is_logic_function: string -> bool
val is_logic_type: string -> bool
val is_logic_ctor: string -> bool

(** @since Oxygen-20120901 *)
val is_model_field: string -> bool

(** {3 removing} *)
val remove_logic_function: string -> unit
val remove_logic_type: string -> unit
val remove_logic_ctor: string -> unit

(** @since Oxygen-20120901 *)
val remove_model_field: string -> unit

(** {2 Typename table} *)

(** marks an identifier as being a typename in the logic *)
val add_typename: string -> unit

(** marks temporarily a typename as being a normal identifier in the logic *)
val hide_typename: string -> unit

(** removes latest typename status associated to a given identifier *)
val remove_typename: string -> unit

(** erases all the typename status *)
val reset_typenames: unit -> unit

(** returns the typename status of the given identifier. *)
val typename_status: string -> bool

(** marks builtin logical types as logical typenames for the logic lexer. *)
val builtin_types_as_typenames: unit -> unit

(** {2 Internal use} *)

val init_dependencies: State.t -> unit
  (** Used to postpone dependency of Lenv global tables wrt Cil_state, which
      is initialized afterwards. *)

(*
  Local Variables:
  compile-command: "make -C ../../.."
  End:
*)
