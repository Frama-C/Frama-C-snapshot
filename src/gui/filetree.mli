(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2010                                               *)
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

(** The tree containing the list of modules and functions together with dynamic columns *)

class type t =  object 
  method model : GTree.model

  method set_file_attribute: 
    ?strikethrough:bool -> ?text:string -> string -> unit
    (** Manually set some attributes of the given filename. *)

  method set_global_attribute: 
    ?strikethrough:bool -> ?text:string -> Cil_types.varinfo -> unit
    (** Manually set some attributes of the given variable. *)

  method add_select_function : 
    (was_activated:bool -> activating:bool -> Cil_types.global list -> unit) -> unit
    (** Register a callback that is called whenever an element of the file tree 
        is selected or unselected. *)
    
  method append_pixbuf_column: 
    title:string -> (Cil_types.global list -> GTree.cell_properties_pixbuf list) -> unit
    (** [append_pixbuf_column title f] appends a new column with name [title] to the 
        file tree and register [f] as a callback computing the list of properties 
        for this column. Do not forget that properties need to be set and unset.
        Selects the given variable in the tree view and run the associated callbacks. *)

  method select_global : Cil_types.varinfo -> unit
    (** Selects the given variable in the tree view and run the associated callbacks. *)

  method view : GTree.view
    (** The tree view associated in which the file tree is packed. *)

  method reset : unit -> unit
    (** Resynchronize the tree view with the current project state. 
        This is called by the generic reset extension of {!Design} and shall
        not be called by other plugins.
    *)

  (**/**)
  method reset_dynamic_columns : 
    (GTree.view -> Cil_types.global list GTree.column -> unit) list -> unit
    (** Internal use only for legacy filetree mode *)
  (**/**)

end

val make : GTree.view -> t
(** Create a file tree packed in the given tree_view. *)

(*
Local Variables:
compile-command: "LC_ALL=C make -C ../.. -j"
End:
*)
