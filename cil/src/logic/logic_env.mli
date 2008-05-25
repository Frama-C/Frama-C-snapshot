(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2008                                               *)
(*    CEA   (Commissariat à l'Énergie Atomique)                           *)
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

(** {1 Global Logic Environment} *)

open Cil_types

(** {2 Global Tables} *)
  module LogicInfo: Computation.HASHTBL_OUTPUT
    with type key = string and type data = Cil_types.logic_info

  module PredicateInfo: Computation.HASHTBL_OUTPUT
    with type key = string and type data = Cil_types.predicate_info

  module LogicTypeInfo: Computation.HASHTBL_OUTPUT
    with type key = string and type data = Cil_types.logic_type_info

  module LogicCtorInfo: Computation.HASHTBL_OUTPUT
    with type key = string and type data = Cil_types.logic_ctor_info

(** {2 Shortcuts to the functions of the modules above} *)

  val add_logic_function: logic_info -> unit
  val add_predicate: predicate_info -> unit
  val add_logic_type: string -> logic_type_info -> unit
  val add_logic_ctor: string -> logic_ctor_info -> unit

  val find_logic_function: string -> logic_info
  val find_predicate: string -> predicate_info
  val find_logic_type: string -> logic_type_info
  val find_logic_ctor: string -> logic_ctor_info

  val is_logic_function: string -> bool
  val is_predicate: string -> bool
  val is_logic_type: string -> bool
  val is_logic_ctor: string -> bool

  val remove_logic_function: string -> unit

(** {2 Internal use} *)
  (** Used to postpone dependency of Lenv global tables wrt Cil_state, which
      is initialized afterwards.
   *)
  val init_dependencies: Project.Computation.t -> unit

  module Builtins: sig 
    val apply: unit -> unit 
    val extend: (unit -> unit) -> unit
  end

