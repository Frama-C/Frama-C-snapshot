(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2009                                               *)
(*    CEA (Commissariat à l'Énergie Atomique)                             *)
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

(** Information about version of Frama-C.
    
    The body of this module is generated from Makefile.in.
    @plugin development guide *)

(** Version identifier. *)
val version : string

(** Compilation date. *)
val date : string

(** Is the Frama-C GUI running? 
    @since Beryllium-20090601-beta1 *)
val is_gui: bool ref

(** Directory where architecture independent files are. *)
val datadir : string

(** Directory where the Frama-C kernel library is.
    @since Beryllium-20090601-beta1 *)
val libdir : string

(** Directory where the Frama-C dynamic plug-ins are.
    @since Beryllium-20090601-beta1 *)
val plugin_dir : string

(** Plug-ins statically linked within Frama-C. *)
val static_plugins : string list

(** GUI of plug-ins statically linked within Frama-C. *)
val static_gui_plugins : string list

(*
  Local Variables:
  compile-command: "LC_ALL=C make -C ../.."
  End:
*)
