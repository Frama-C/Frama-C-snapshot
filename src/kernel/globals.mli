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

(** Operations on globals.
    @plugin development guide *)

open Cil_types

(* Forward reference to functions defined in Kernel_function. Do not
   use outside of this module.
 *)
val find_first_stmt: (kernel_function -> stmt) ref
val find_enclosing_block: (stmt -> block) ref

(** Globals variables.
    The AST should be computed before using this module
    (cf. {! Ast.compute}). *)
module Vars: sig

  (** {2 Getters} *)

  val find: varinfo -> initinfo

  val find_from_astinfo: string -> localisation -> varinfo

  val get_astinfo: varinfo -> string * localisation

  (** {2 Iterators} *)

  val iter: (varinfo -> initinfo -> unit) -> unit
  val fold: (varinfo -> initinfo -> 'a -> 'a) -> 'a -> 'a

  val iter_in_file_order: (varinfo -> initinfo -> unit) -> unit

  (** {2 Setters}

      Functions of this section should not be called by casual users. *)

  exception AlreadyExists of varinfo * initinfo
  val add: varinfo -> initinfo -> unit
    (** @raise AlreadyExists if the given varinfo is already registered. *)

  val add_decl: varinfo -> unit
    (** @raise AlreadyExists if the given varinfo is already registered. *)

  val self: State.t

end

(* ************************************************************************* *)
(** Functions.
    The AST should be computed before using this module
    (cf. {! Ast.compute}). *)
module Functions: sig

  val self: State.t

  (** {2 Getters} *)

  val get: varinfo -> kernel_function
    (** @raise Not_found if the given varinfo has not a function type. *)
  val get_params: kernel_function -> varinfo list
  val get_vi: kernel_function -> varinfo

  (** {2 Searching} *)

  val find_by_name : string -> kernel_function
    (** @raise Not_found if there is no function of this name. *)

  val find_def_by_name : string -> kernel_function
    (** @raise Not_found if there is no function definition of this name. *)

  val find_englobing_kf: kinstr -> kernel_function option
    (** @deprecated since Carbon-20101201
        Use [Kernel_function.find_englobing_kf] instead *)

  (** {2 Iterators} *)

  val iter: (kernel_function -> unit) -> unit
  val fold: (kernel_function -> 'a -> 'a) -> 'a -> 'a
  val iter_on_fundecs: (fundec -> unit) -> unit

  (** {2 Setters}

      Functions of this section should not be called by casual users. *)

  val add: cil_function -> unit
    (**TODO: remove this function and replace all calls by: *)

  val replace_by_declaration: funspec -> varinfo -> location -> unit

  val replace_by_definition: funspec -> fundec -> location -> unit
    (**TODO: do not take a funspec as argument *)

  val set_spec: (kernel_function -> (funspec -> funspec) -> unit) ref

  val register: kernel_function -> unit
end

(* ************************************************************************* *)
(** Globals annotations.
    The AST should be computed before using this module
    (cf. {! Ast.compute}). *)
module Annotations: sig

  val self: State.t
    (** The state kind corresponding to the table of global annotations. *)

  (** {2 Getters} *)

  val get_all: unit -> (global_annotation * bool) list

  (** {2 Iterators} *)

  val iter: (global_annotation -> bool -> unit) -> unit
    (** The boolean parameter of the given function is [true] iff the
        annotation was generated. *)

  (** {2 Setters} *)

  val add_user: global_annotation -> unit
  val add_generated: global_annotation -> unit

end

(* ************************************************************************* *)
(** Globals associated to filename. *)
module FileIndex : sig

  val self: State.t
    (** The state kind corresponding to the table of global C symbols.
        @since Boron-20100401 *)

  (** {2 Getters} *)

  val get_symbols : filename:string -> global list
    (** All global C symbols of the given module.
        @since Boron-20100401 *)

  val find : filename:string -> string * (global list)
    (** All global C symbols for valviewer.
        The file name to display is returned, and the [global] list reversed. *)

  val get_files: unit -> string list
    (** Get the files list containing all [global] C symbols. *)

  (** {2 Searching among all [global] C symbols} *)

  val get_globals : filename:string -> (varinfo * initinfo) list
  (** Global variables of the given module for the kernel user interface *)

  val get_global_annotations: filename:string -> global_annotation list
  (** Global annotations of the given module for the kernel user interface
      @since Nitrogen-20111001 *)

  val get_functions :
    ?declarations:bool -> filename:string -> kernel_function list
    (** Global functions of the given module for the kernel user interface.
        If [declarations] is true, functions declared in a module but defined
        in another module are only reported in the latter (default is false).
    *)

  val kernel_function_of_local_var_or_param_varinfo :
    varinfo -> (kernel_function * bool)
    (** kernel_function where the local variable or formal parameter is
        declared. The boolean result is true for a formal parameter.
        @raise Not_found if the varinfo is a global one. *)

end

(* ************************************************************************* *)
(** {2 Entry point} *)
(* ************************************************************************* *)

exception No_such_entry_point of string
  (** May be raised by [entry_point] below. *)

val entry_point : unit -> kernel_function * bool
  (** @return the current function entry point and a boolean indicating if it
      is a library entry point.
      @raise No_such_entry_point if the current entrypoint name does not
      exist. This exception is automatically handled by the Frama-C kernel. Thus
      you don't have to catch it yourself, except if you do a specific work. *)

val set_entry_point : string -> bool -> unit
(** [set_entry_point name lib] sets [Kernel.MainFunction] to [name] if
    [lib] is [false] and [Kernel.LibEntry] to [name] if [lib] is [true].
    Moreover, clear the results of all the analysis which depend on
    [Kernel.MainFunction] or [Kernel.LibEntry].
    @plugin development guide *)

(* ************************************************************************* *)
(** {2 Comments} *)
(* ************************************************************************* *)

val get_comments_global: global -> string list
(** Gets a list of comments associated to the given global. This function
    is useful only when -keep-comments is on.

    A comment is associated to a global if it occurs after 
    the declaration/definition of the preceding one in the file, before the end
    of the current declaration/definition and does not occur in the 
    definition of a function. Note that this function is experimental and
    may fail to associate comments properly. Use directly 
    {! Cabshelper.Comments.get} to retrieve comments in a given region.
    (see {!Globals.get_comments_stmt} for retrieving comments associated to
    a statement).

    @since Nitrogen-20111001
*)

val get_comments_stmt: stmt -> string list
(** Gets a list of comments associated to the given global. This function
    is useful only when -keep-comments is on.

    A comment is associated to a global if it occurs after 
    the preceding statement and before the current statement ends (except for
    the last statement in a block, to which statements occuring before the end
    of the block are associated). Note that this function is experimental and
    may fail to associate comments properly. Use directly 
    {! Cabshelper.Comments.get} to retrieve comments in a given region.

    @since Nitrogen-20111001
*)

(*
Local Variables:
compile-command: "make -C ../.."
End:
*)
