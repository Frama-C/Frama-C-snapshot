(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2016                                               *)
(*    CEA (Commissariat à l'énergie atomique et aux énergies              *)
(*         alternatives)                                                  *)
(*                                                                        *)
(*  All rights reserved.                                                  *)
(*  Contact CEA LIST for licensing.                                       *)
(*                                                                        *)
(**************************************************************************)

(** {1 GUI utilities for Metrics} *)

(** Initialize the main Metrics panel into an upper and lower part.
    @returns a box containing the lower part of the panel where metrics can
    display their results.
*)
val init_panel : Design.main_window_extension_points -> GPack.box ;;

(** @returns a value allowing to register the panel into the main GUI *)
val coerce_panel_to_ui : < coerce : 'a; .. > -> 'b -> string * 'a * 'c option  ;;

(** Diplay the list of list of strings in a LablgGTK table object *)
val display_as_table : string list list -> GPack.box  -> unit ;;

(** Reset metrics panel to pristine conditions by removeing children from
    bottom container
*)
val reset_panel : 'a -> unit ;;

(** register_metrics [metrics_name] [display_function] () adds a selectable
    choice for the metrics [metrics_name] and add a hook calling
    [display_function] whenever this metrics is selected and launched.
*)
val register_metrics : string -> (GPack.box -> unit) -> unit ;;
