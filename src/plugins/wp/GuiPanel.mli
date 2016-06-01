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

val update : unit -> unit
val on_update : (unit -> unit) -> unit

val reload : unit -> unit
val on_reload : (unit -> unit) -> unit

val run_and_prove :
  Design.main_window_extension_points -> GuiSource.selection -> unit

val register :
  main:Design.main_window_extension_points ->
  available_provers:GuiConfig.provers ->
  enabled_provers:GuiConfig.provers ->
  configure_provers:(unit -> unit) -> unit
