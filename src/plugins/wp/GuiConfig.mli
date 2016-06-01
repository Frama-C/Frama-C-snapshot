(**************************************************************************)
(*                                                                        *)
(*  This file is part of WP plug-in of Frama-C.                           *)
(*                                                                        *)
(*  Copyright (C) 2007-2016                                               *)
(*    CEA (Commissariat a l'energie atomique et aux energies              *)
(*         alternatives)                                                  *)
(*                                                                        *)
(*  All rights reserved.                                                  *)
(*  Contact CEA LIST for licensing.                                       *)
(**************************************************************************)

(* ------------------------------------------------------------------------ *)
(* ---  WP Provers Configuration Panel                                  --- *)
(* ------------------------------------------------------------------------ *)

open ProverWhy3

class provers : string -> [dp list] Widget.selector

class dp_chooser :
  main:Design.main_window_extension_points ->
  available:provers ->
  enabled:provers ->
  object
    method run : unit -> unit (** Edit enabled provers *)
  end

class dp_button :
  available:provers ->
  enabled:provers ->
  object
    inherit Widget.widget
    method update : unit -> unit
  end
