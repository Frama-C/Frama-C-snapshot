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

(** Logic typing and logic environment.
    @plugin development guide *)

open Cil_types

(** Relation operators conversion 
    @since Nitrogen-20111001
*)
val type_rel: Logic_ptree.relation -> Cil_types.relation

(** Arithmetic binop conversion. Addition and Substraction are always 
    considered as being used on integers. It is the responsibility of the
    user to introduce PlusPI/IndexPI, MinusPI and MinusPP where needed. 
    @since Nitrogen-20111001
*)
val type_binop: Logic_ptree.binop -> Cil_types.binop

val unescape: string -> string
val wcharlist_of_string: string -> int64 list

val is_arithmetic_type: Cil_types.logic_type -> bool
val is_integral_type: Cil_types.logic_type -> bool
val is_set_type: Cil_types.logic_type -> bool
val is_array_type: Cil_types.logic_type -> bool
val is_pointer_type: Cil_types.logic_type -> bool

val type_of_pointed: logic_type -> logic_type
val type_of_array_elem: logic_type -> logic_type
val type_of_set_elem: logic_type -> logic_type
  
val ctype_of_pointed: logic_type -> typ
val ctype_of_array_elem: logic_type -> typ

(**
   @deprecated Neon-20130301 use Logic_const.addTermOffsetLval instead
*)
val add_offset_lval: term_offset -> term_lval -> term_lval

val arithmetic_conversion:
  Cil_types.logic_type -> Cil_types.logic_type -> Cil_types.logic_type

(** Local logic environment *)
module Lenv : sig
  type t
  val empty : unit -> t
end

(** Functions that can be called when type-checking an extension of ACSL. *)
type typing_context = {
  is_loop: unit -> bool;
  anonCompFieldName : string;
  conditionalConversion : typ -> typ -> typ;
  find_macro : string -> Logic_ptree.lexpr;
  find_var : string -> logic_var;
  find_enum_tag : string -> exp * typ;
  find_comp_type : kind:string -> string -> typ;
  find_comp_field: compinfo -> string -> offset;
  find_type : string -> typ;
  find_label : string -> stmt ref;
  remove_logic_function : string -> unit;
  remove_logic_type: string -> unit;
  remove_logic_ctor: string -> unit;
  add_logic_function: logic_info -> unit;
  add_logic_type: string -> logic_type_info -> unit;
  add_logic_ctor: string -> logic_ctor_info -> unit;
  find_all_logic_functions: string -> logic_info list;
  find_logic_type: string -> logic_type_info;
  find_logic_ctor: string -> logic_ctor_info;
  pre_state:Lenv.t;
  post_state:termination_kind list -> Lenv.t;
  assigns_env: Lenv.t;
  type_predicate:Lenv.t -> Logic_ptree.lexpr -> predicate named;
  type_term:Lenv.t -> Logic_ptree.lexpr -> term;
  type_assigns:
    accept_formal:bool -> 
    Lenv.t -> Logic_ptree.lexpr assigns -> identified_term assigns;
 
 error: 'a. location -> ('a,Format.formatter,unit) format -> 'a;
}

(** [register_behavior_extension name f] registers a typing function [f] to 
    be used to type clause with name [name]. 
    This function may change the funbehavior in place. 
    Here is a basic example:
    let foo_typer ~typing_context ~loc bhv ps =
    match ps with p::[] ->
      bhv.b_extended <- ("FOO",42,
			 [Logic_const.new_predicate 
                                (typing_context.type_predicate 
				 (typing_context.post_state Normal) 
				 p)])
      ::bhv.b_extended
      | _ -> typing_context.error loc "expecting a predicate after keyword FOO"
    let () = register_behavior_extension "FOO" foo_typer

    @since Carbon-20101201
*)
val register_behavior_extension:  
  string ->
  (typing_context:typing_context -> loc:location -> funbehavior -> 
    Logic_ptree.lexpr list -> unit)
  -> unit

module Make
  (C :
    sig
      val is_loop: unit -> bool 
      (** whether the annotation we want to type is contained in a loop. *)
      val anonCompFieldName : string
      val conditionalConversion : typ -> typ -> typ
      val find_macro : string -> Logic_ptree.lexpr
      val find_var : string -> logic_var
      val find_enum_tag : string -> exp * typ
      val find_comp_type : kind:string -> string -> typ
      val find_comp_field: compinfo -> string -> offset
      val find_type : string -> typ
      val find_label : string -> stmt ref

      val remove_logic_function : string -> unit
      val remove_logic_type: string -> unit
      val remove_logic_ctor: string -> unit

      val add_logic_function: logic_info -> unit
      val add_logic_type: string -> logic_type_info -> unit
      val add_logic_ctor: string -> logic_ctor_info -> unit

      val find_all_logic_functions : string -> Cil_types.logic_info list
      val find_logic_type: string -> logic_type_info
      val find_logic_ctor: string -> logic_ctor_info

      (** What to do when we have a term of type Integer in a context
          expecting a C integral type. 
          @raise Failure to reject such conversion
          @since Nitrogen-20111001
       *)
      val integral_cast: Cil_types.typ -> Cil_types.term -> Cil_types.term

    end) :
sig

  (** @since Nitrogen-20111001 *)
  val type_of_field: 
    location -> string -> logic_type -> (term_offset * logic_type)

  (** @since Nitrogen-20111001 *)
  val mk_cast: Cil_types.term -> Cil_types.logic_type -> Cil_types.term

  (** type-checks a term. *)
  val term : Lenv.t -> Logic_ptree.lexpr -> term

  val predicate : Lenv.t -> Logic_ptree.lexpr -> predicate named

  (** [code_annot loc behaviors rt annot] type-checks an in-code annotation.
    @param loc current location
    @param behaviors list of existing behaviors
    @param rt return type of current function
    @param annot the annotation
   *)
  val code_annot :
    Cil_types.location -> string list ->
    Cil_types.logic_type -> Logic_ptree.code_annot -> code_annotation

  val type_annot :
    location -> Logic_ptree.type_annot -> logic_info

  val model_annot :
    location -> Logic_ptree.model_annot -> model_info

  val annot : Logic_ptree.decl -> global_annotation

  val custom : Logic_ptree.custom_tree -> Cil_types.custom_tree

  (** [funspec behaviors f prms typ spec] type-checks a function contract.
      @param behaviors list of existing behaviors (outside of the current
      spec, e.g. in the spec of the corresponding declaration when type-checking
      the spec of a definition)
      @param f the function
      @param prms its parameters
      @param its type
      @param spec the spec to typecheck
   *)
  val funspec :
    string list ->
    varinfo -> (varinfo list) option -> typ -> Logic_ptree.spec -> funspec

end

(** append the Old and Post labels in the environment *)
val append_old_and_post_labels: Lenv.t -> Lenv.t

(** appends the Here label in the environment *)
val append_here_label: Lenv.t -> Lenv.t

(** appends the "Pre" label in the environment *)
val append_pre_label: Lenv.t -> Lenv.t


(** adds a given variable in local environment. *)
val add_var: string -> logic_var -> Lenv.t -> Lenv.t

(** add [\result] in the environment. *)
val add_result: Lenv.t -> logic_type -> Lenv.t

(** enter a given post-state. *)
val enter_post_state: Lenv.t -> termination_kind -> Lenv.t

(** enter a given post-state and put [\result] in the env.
NB: if the kind of the post-state is neither [Normal] nor [Returns],
this is not a normal ACSL environment. Use with caution.
*)
val post_state_env: termination_kind -> logic_type -> Lenv.t


(*
Local Variables:
compile-command: "LC_ALL=C make -C ../../.."
End:
*)
