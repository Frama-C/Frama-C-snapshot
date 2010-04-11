(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2010                                               *)
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
(*  See the GNU Lesser General Public License version v2.1                *)
(*  for more details (enclosed in the file licenses/LGPLv2.1).            *)
(*                                                                        *)
(**************************************************************************)

(** Logic typing and logic environment.
    @plugin development guide *)

open Cil_types

(** Local logic environment *)
module Lenv : sig
  type t
  val empty : unit -> t
end


module Make
  (C :
    sig
      val annonCompFieldName : string
      val conditionalConversion : typ -> typ -> typ
      val find_var : string -> logic_var
      val find_enum_tag : string -> exp * typ
      val find_comp_type : kind:string -> string -> typ
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

    end) :
sig

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

  val annot : Logic_ptree.decl -> global_annotation

  val funspec :
    varinfo -> (varinfo list) option -> typ -> Logic_ptree.spec -> funspec

end

(** append the Old label in the environment *)
val append_old_and_post_labels: Lenv.t -> Lenv.t

(** appends the Here label in the environment *)
val append_here_label: Lenv.t -> Lenv.t

(** appends the "Pre" label in the environment 
    when [pre_is_old] is true, it adds it has a synonym for "Old".
    (the latter should be set when typing function contracts)
*)
val append_pre_label: pre_is_old:bool -> Lenv.t -> Lenv.t

(*
Local Variables:
compile-command: "LC_ALL=C make -C ../../.."
End:
*)
