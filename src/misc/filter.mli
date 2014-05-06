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

open Cil_types

(** [Filter] helps to build a new [cilfile] from an old one by removing some of
 * its elements. One can even build several functions from a source function
 * by specifying different names for each of them.
 * *)

(** Signature of a module that decides which element of a function
 * have to be visible or not *)
module type RemoveInfo = sig

  (** exception that fun_assign_visible should raise to indicate that
      the corresponding assigns clause should be erased entirely
   *)
  exception EraseAssigns

  (** exception that fun_frees_visible or fun_allocates_visible should 
      raise to indicate that the corresponding allocation clause should 
      be erased entirely
   *)
  exception EraseAllocation

  (** some type for the whole project information *)
  type proj

  (** some type for a function information *)
  type fct

  (** This function will be called for each function of the source program.
  * A new function will be created for each element of the returned list.
  *)
  val fct_info : proj -> kernel_function -> fct list

  (** useful when we want to have several functions in the result for one
  * source function. If if is not the case, you can return [varinfo.vname].
  * It is the responsibility of the user to given different names to different
  * function. *)
  val fct_name : varinfo -> fct -> string

  (** tells if the n-th formal parameter is visible. *)
  val param_visible : fct -> int -> bool

  (** tells if the body of a function definition is visible.
  * True is most cases, but can be defined to be false when we want to export
  * only the declaration of a function instead of its definition *)
  val body_visible : fct -> bool

  (** tells if the local variable is visible. *)
  val loc_var_visible : fct -> varinfo -> bool

  (** tells if the statement is visible. *)
  val inst_visible : fct -> stmt -> bool

  (** tells if the label is visible. *)
  val label_visible : fct -> stmt -> label -> bool

  (** tells if the annotation, attached to the given statement is visible. *)
  val annotation_visible: fct -> stmt -> code_annotation -> bool

  val fun_precond_visible : fct -> predicate -> bool
  val fun_postcond_visible : fct -> predicate -> bool
  val fun_variant_visible : fct -> term -> bool

  val fun_frees_visible : fct -> identified_term -> bool
  val fun_allocates_visible : fct -> identified_term -> bool

  val fun_assign_visible : fct -> identified_term from -> bool
    (** true if the assigned value (first component of the from) is visible
        @raise EraseAssigns to indicate that the corresponding assigns clause
        should be erased entirely (i.e. assigns everything. If it were to
        just return false to all elements, this would result in assigns \nothing
     *)
  val fun_deps_visible : fct -> identified_term -> bool
    (** true if the corresponding functional dependency is visible. *)

  (** [called_info] will be called only if the call statement is visible.
  * If it returns [None], the source call will be visible,
  * else it will use the returned [fct] to know if the return value and the
  * arguments are visible.
  * The input [fct] parameter is the one of the caller function.
  * *)
  val called_info : proj * fct -> stmt ->
                    (kernel_function * fct) option

  (** tells if the lvalue of the call has to be visible *)
  val res_call_visible : fct -> stmt -> bool

  (** tells if the function returns something or if the result is [void].
  * Notice that if this function returns [true] the function will have the same
  * return type than the original function. So, if it was already [void], it
  * makes no difference if this function returns true or false.
  *
  * - For a defined function, this should give the same result than
  * [inst_visible fct_info (Kernel_function.find_return kf)].
  * - [res_call_visible] must return [false]
  *   if [result_visible] returns false on the called function.
  *)
  val result_visible : kernel_function -> fct -> bool

  (** [cond_edge_visible f s] emplies that [s] is an 'if' in [f]. The
      first returned boolean indicates that the 'then' edge is useful,
      the second one the 'else' is. Setting one or both to true will
      lead to the simplification in the 'if'. *)
  val cond_edge_visible: fct -> stmt -> bool * bool

end

(** Given a module that match the module type described above,
* [F.build_cil_file] initializes a new project containing the slices
*)
module F (Info : RemoveInfo) : sig

  val build_cil_file : string ->  Info.proj -> Project.t

end

(*
Local Variables:
compile-command: "make -C ../.."
End:
*)
