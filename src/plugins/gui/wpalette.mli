(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2017                                               *)
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

open Widget

(** A side-bar palette of tools. *)

(**
   Configurable palette-tool.
   Each tool is a widget that consists of three components:
    - a selectable label with optional status icon
    - an optional action button (icon only)
    - an optional configuration panel

   The action button is only displayed when associated with a callback.
   Clicking the label toggles the configuration panel, if the tool is [active].
*)
class tool :
  ?label:string ->
  ?tooltip:string ->
  ?content:widget ->
  unit ->
  object
    inherit widget
    method tool : tool (** Self cast *)

    method is_active : bool
    method on_active : (bool -> unit) -> unit
    method set_active : bool -> unit
    method has_action : bool
    
    method set_label : string -> unit
    method set_status : icon -> unit
    method set_tooltip : string -> unit

    method set_action :
      ?icon:icon ->
      ?tooltip:string ->
      ?callback:(unit -> unit) ->
      unit -> unit
    (** Makes the {i action} button visible.
         - If no icon is provided, the previous one is kept.
         - If no tooltip is provided, the previous one is kept.
         - If no callback is given, the button is deactivated. 
         - The callback replaces any previous one and makes 
           the action button clickable. *)

    method clear_action : unit
    (** Deactivate and hide the {i action} button. *)
    
    method set_content : widget -> unit
    (** Shall be used at most once, and before [#coerce] or [#widget]. *)
    
  end

(** A Palette. Implemented with a vertical box with a scrollbar. *)
class panel : unit ->
  object
    inherit widget
      
    method add_tool : tool -> unit
    (** Append a palette-tool. 
        The panel ensures that only one tool is selected and toggled. *)
      
    method add_widget : GObj.widget -> unit
    (** Append an arbitrary widget among other widget tools. *)
  end
